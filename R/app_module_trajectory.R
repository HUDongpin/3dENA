# Server module for centroid trajectory analysis.
#
# Analytical functions are supplied by trajectory_analysis.R and plotting
# functions by trajectory_plot.R. This module deliberately passes raw ENA point
# coordinates to those functions; display scaling is never applied to analysis.

.trajectory_or <- function(value, default) {
  if (is.null(value) || !length(value)) default else value
}


.trajectory_resolve_value <- function(value) {
  if (shiny::is.reactive(value)) {
    return(value())
  }
  value
}


.trajectory_flatten_axes <- function(value) {
  value <- .trajectory_resolve_value(value)

  if (is.null(value)) {
    return(character(0))
  }

  if (is.list(value) && !is.data.frame(value)) {
    value <- unlist(lapply(value, .trajectory_flatten_axes), use.names = FALSE)
  }

  if (inherits(value, "formula")) {
    value <- all.vars(value)
  }

  value <- as.character(value)
  value <- sub("^~\\s*", "", value)
  unique(value[nzchar(value) & !is.na(value)])
}


.trajectory_bootstrap_max_reps <- function() 500L

.trajectory_bootstrap_max_seconds <- function() {
  raw <- suppressWarnings(as.numeric(Sys.getenv(
    "ENA3D_MAX_BOOTSTRAP_SECONDS", unset = "60"
  )))
  if (length(raw) != 1L || is.na(raw) || !is.finite(raw) || raw < 5 || raw > 300) {
    stop("ENA3D_MAX_BOOTSTRAP_SECONDS must be between 5 and 300.")
  }
  raw
}


.trajectory_internal_metadata <- function(columns) {
  columns <- as.character(columns)
  grepl(
    "^(ENA_UNIT|KEYCOL|X|ROW|ROW_ID|ROWID)$|^[._]trajectory_",
    columns,
    ignore.case = TRUE
  )
}


.trajectory_user_metadata <- function(columns) {
  columns <- unique(as.character(columns))
  columns[
    !is.na(columns) & nzchar(columns) & !.trajectory_internal_metadata(columns)
  ]
}


.trajectory_id_coverage <- function(points, time_var, id_var,
                                    group_var = NULL) {
  empty <- list(
    n_ids = 0L,
    n_repeated_ids = 0L,
    repeated_fraction = 0,
    n_valid_rows = 0L,
    n_repeated_rows = 0L,
    repeated_row_fraction = 0,
    n_periods = 0L,
    max_periods_per_id = 0L,
    n_duplicate_id_time_rows = 0L,
    grouped = FALSE
  )
  if (!is.data.frame(points) || !is.character(time_var) ||
      length(time_var) != 1L || !time_var %in% names(points) ||
      !is.character(id_var) || length(id_var) != 1L ||
      !id_var %in% names(points)) {
    return(empty)
  }

  grouped <- is.character(group_var) && length(group_var) == 1L &&
    !is.na(group_var) && nzchar(group_var) && group_var %in% names(points)
  time_values <- points[[time_var]]
  id_values <- points[[id_var]]
  valid <- !is.na(time_values) & !is.na(id_values) &
    nzchar(trimws(as.character(id_values)))
  if (grouped) {
    group_values <- points[[group_var]]
    valid <- valid & !is.na(group_values) &
      nzchar(trimws(as.character(group_values)))
  } else {
    group_values <- rep("__all__", nrow(points))
  }
  if (!any(valid)) {
    empty$grouped <- grouped
    return(empty)
  }

  frame <- data.frame(
    .group = as.character(group_values[valid]),
    .id = as.character(id_values[valid]),
    .time = .trajectory_order_labels(time_values[valid]),
    stringsAsFactors = FALSE
  )
  frame$.profile <- paste(
    nchar(frame$.group, type = "bytes"), frame$.group,
    nchar(frame$.id, type = "bytes"), frame$.id,
    sep = ":"
  )
  profile_periods <- tapply(
    frame$.time,
    frame$.profile,
    function(value) length(unique(value))
  )
  repeated_profiles <- names(profile_periods)[profile_periods >= 2L]
  repeated_rows <- frame$.profile %in% repeated_profiles
  pair_key <- paste(frame$.profile, frame$.time, sep = "\r")

  list(
    n_ids = length(profile_periods),
    n_repeated_ids = length(repeated_profiles),
    repeated_fraction = length(repeated_profiles) / length(profile_periods),
    n_valid_rows = nrow(frame),
    n_repeated_rows = sum(repeated_rows),
    repeated_row_fraction = mean(repeated_rows),
    n_periods = length(unique(frame$.time)),
    max_periods_per_id = max(profile_periods),
    n_duplicate_id_time_rows = length(pair_key) - length(unique(pair_key)),
    grouped = grouped
  )
}


.trajectory_id_coverage_message <- function(coverage, id_var = NULL,
                                            group_var = NULL) {
  id_label <- if (is.null(id_var) || !length(id_var) || !nzchar(id_var)) {
    "The selected ID"
  } else {
    paste0("ID `", id_var, "`")
  }
  context <- if (isTRUE(coverage$grouped) && !is.null(group_var) &&
                 length(group_var) && nzchar(group_var)) {
    paste0(" within `", group_var, "` groups")
  } else {
    ""
  }
  if (!coverage$n_ids) {
    return(paste0(
      id_label, " has no valid ID/time rows", context,
      ". Choose a stable repeated-unit field."
    ))
  }
  if (!coverage$n_repeated_ids) {
    return(paste0(
      "0 of ", coverage$n_ids, " ID profiles repeat across time", context,
      ". This selection is cross-sectional only; choose a stable repeated-unit ",
      "ID before running a longitudinal trajectory."
    ))
  }
  paste0(
    coverage$n_repeated_ids, " of ", coverage$n_ids,
    " ID profiles repeat across time", context, " (",
    round(100 * coverage$repeated_row_fraction, 1),
    "% of valid rows; up to ", coverage$max_periods_per_id,
    " periods per profile; ", coverage$n_duplicate_id_time_rows,
    " duplicate ID/time rows)."
  )
}


.trajectory_id_choices <- function(points, candidates, time_var) {
  candidates <- intersect(.trajectory_user_metadata(candidates), names(points))
  if (!length(candidates) || is.null(time_var) || !length(time_var) ||
      !time_var %in% names(points)) {
    return(stats::setNames(candidates, candidates))
  }
  labels <- vapply(candidates, function(candidate) {
    coverage <- .trajectory_id_coverage(points, time_var, candidate)
    paste0(
      candidate, " — ", coverage$n_repeated_ids, "/", coverage$n_ids,
      " repeated ID profiles"
    )
  }, character(1))
  stats::setNames(candidates, labels)
}


.trajectory_bootstrap_cost <- function(points, dimensions, n_boot,
                                       uncertainty = FALSE,
                                       comparison = FALSE) {
  enabled_jobs <- as.integer(isTRUE(uncertainty)) + as.integer(isTRUE(comparison))
  n_boot <- suppressWarnings(as.integer(n_boot))
  if (!enabled_jobs || is.na(n_boot) || n_boot < 2L) {
    return(list(enabled = FALSE, seconds = 0, tier = "none", work_units = 0))
  }
  row_count <- if (is.data.frame(points)) nrow(points) else 0L
  dimension_count <- max(1L, length(unique(dimensions)))
  # This is deliberately conservative and explicitly approximate. It reflects
  # both per-replicate setup and participant-coordinate work, and is used only
  # to help users choose a responsible hosted-server job size.
  work_units <- enabled_jobs * n_boot * row_count * dimension_count
  seconds <- enabled_jobs * n_boot * 0.05 + work_units / 50000
  tier <- if (seconds < 5) "light" else if (seconds < 30) "moderate" else "heavy"
  list(
    enabled = TRUE,
    seconds = unname(seconds),
    tier = tier,
    work_units = unname(work_units)
  )
}


.trajectory_bootstrap_cost_message <- function(cost) {
  if (!isTRUE(cost$enabled)) {
    return("Bootstrap is off; centroid paths are computed without resampling.")
  }
  duration <- if (cost$seconds < 1) {
    "under one second"
  } else if (cost$seconds < 90) {
    paste0("about ", ceiling(cost$seconds), " seconds")
  } else {
    paste0("about ", round(cost$seconds / 60, 1), " minutes")
  }
  paste0(
    "Estimated hosted-server cost: ", cost$tier, " (", duration,
    "). This is a rough estimate; load and data structure can change runtime. ",
    "Jobs above ", .trajectory_bootstrap_max_seconds(),
    " estimated seconds are rejected, and every accepted job is also ",
    "terminated if its isolated worker reaches that executable deadline."
  )
}


.trajectory_validate_bootstrap_cost <- function(
    cost, max_seconds = .trajectory_bootstrap_max_seconds()) {
  if (isTRUE(cost$enabled) && is.finite(cost$seconds) &&
      cost$seconds > max_seconds) {
    stop(sprintf(
      paste(
        "This bootstrap job is estimated at %.1f seconds, above the hosted",
        "limit of %.0f seconds. Reduce repetitions, use selected axes, disable",
        "one bootstrap job, or run the analysis offline."
      ),
      cost$seconds, max_seconds
    ))
  }
  invisible(cost)
}


.trajectory_bootstrap_max_processes <- function() {
  raw <- suppressWarnings(as.integer(Sys.getenv(
    "ENA3D_MAX_BOOTSTRAP_PROCESSES", unset = "2"
  )))
  if (length(raw) != 1L || is.na(raw) || raw < 1L || raw > 16L) {
    stop("ENA3D_MAX_BOOTSTRAP_PROCESSES must be between 1 and 16.")
  }
  raw
}


.trajectory_bootstrap_process_registry <- new.env(parent = emptyenv())


.trajectory_bootstrap_condition <- function(message, subclass) {
  condition <- simpleError(message, call = NULL)
  class(condition) <- c(subclass, class(condition))
  condition
}


.trajectory_analysis_file <- function() {
  configured <- getOption("ena3d.trajectory_analysis_file", NULL)
  app_dir <- if (exists(".ena3d_app_dir", envir = .GlobalEnv,
                        inherits = FALSE)) {
    get(".ena3d_app_dir", envir = .GlobalEnv, inherits = FALSE)
  } else {
    character(0)
  }
  project_root <- Sys.getenv("ENA3D_PROJECT_ROOT", unset = "")
  ancestors <- character(0)
  cursor <- normalizePath(getwd(), mustWork = FALSE)
  repeat {
    ancestors <- c(ancestors, cursor)
    parent <- dirname(cursor)
    if (identical(parent, cursor)) break
    cursor <- parent
  }
  candidates <- unique(c(
    configured,
    file.path(app_dir, "trajectory_analysis.R"),
    file.path(project_root, "R", "trajectory_analysis.R"),
    file.path(getwd(), "trajectory_analysis.R"),
    file.path(getwd(), "R", "trajectory_analysis.R"),
    file.path(ancestors, "trajectory_analysis.R"),
    file.path(ancestors, "R", "trajectory_analysis.R")
  ))
  candidates <- candidates[
    !is.na(candidates) & nzchar(candidates) & file.exists(candidates)
  ]
  if (!length(candidates)) {
    stop(
      paste0(
        "Could not locate trajectory_analysis.R for the isolated bootstrap ",
        "worker. Set option `ena3d.trajectory_analysis_file` explicitly."
      ),
      call. = FALSE
    )
  }
  normalizePath(candidates[[1L]], mustWork = TRUE)
}


.trajectory_prune_bootstrap_processes <- function() {
  keys <- ls(.trajectory_bootstrap_process_registry, all.names = TRUE)
  for (key in keys) {
    process <- get(
      key, envir = .trajectory_bootstrap_process_registry, inherits = FALSE
    )
    alive <- tryCatch(isTRUE(process$is_alive()), error = function(error) FALSE)
    if (!alive) {
      rm(list = key, envir = .trajectory_bootstrap_process_registry)
    }
  }
  invisible(NULL)
}


# Start the expensive bootstrap work outside the Shiny R process.  A callr
# child is deliberately used instead of an in-process timer: R cannot service
# other Shiny sessions while CPU-bound R code is merely wrapped in a timeout.
# The returned promise is settled by short `later` polls, and the entire child
# process tree is killed when the executable deadline is reached.
.trajectory_start_bootstrap_job <- function(
    uncertainty_arguments = NULL,
    comparison_arguments = NULL,
    timeout_seconds = .trajectory_bootstrap_max_seconds(),
    analysis_file = .trajectory_analysis_file(),
    poll_interval = 0.05) {
  if (is.null(uncertainty_arguments) && is.null(comparison_arguments)) {
    stop("At least one isolated bootstrap operation is required.", call. = FALSE)
  }
  if (!is.numeric(timeout_seconds) || length(timeout_seconds) != 1L ||
      is.na(timeout_seconds) || !is.finite(timeout_seconds) ||
      timeout_seconds <= 0) {
    stop("`timeout_seconds` must be one positive finite number.", call. = FALSE)
  }
  if (!is.numeric(poll_interval) || length(poll_interval) != 1L ||
      is.na(poll_interval) || !is.finite(poll_interval) ||
      poll_interval <= 0 || poll_interval > 1) {
    stop("`poll_interval` must be between 0 and 1 second.", call. = FALSE)
  }
  for (package in c("callr", "later", "promises")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop(
        "The `", package,
        "` package is required for isolated hosted bootstrap jobs.",
        call. = FALSE
      )
    }
  }

  .trajectory_prune_bootstrap_processes()
  active_count <- length(ls(
    .trajectory_bootstrap_process_registry, all.names = TRUE
  ))
  if (active_count >= .trajectory_bootstrap_max_processes()) {
    stop(
      paste0(
        "The hosted server is already running the maximum number of isolated ",
        "bootstrap jobs. Wait for a running job to finish and try again."
      ),
      call. = FALSE
    )
  }

  started_at <- unname(proc.time()[["elapsed"]])
  process <- callr::r_bg(
    func = function(analysis_file, uncertainty_arguments,
                    comparison_arguments) {
      analysis <- new.env(parent = globalenv())
      sys.source(analysis_file, envir = analysis)
      result <- list(uncertainty = NULL, comparison = NULL)
      if (!is.null(uncertainty_arguments)) {
        result$uncertainty <- do.call(
          get("bootstrap_centroid_path", envir = analysis, inherits = FALSE),
          uncertainty_arguments
        )
      }
      if (!is.null(comparison_arguments)) {
        result$comparison <- do.call(
          get("compare_centroid_paths", envir = analysis, inherits = FALSE),
          comparison_arguments
        )
      }
      result
    },
    args = list(
      analysis_file = normalizePath(analysis_file, mustWork = TRUE),
      uncertainty_arguments = uncertainty_arguments,
      comparison_arguments = comparison_arguments
    ),
    stdout = "|",
    stderr = "|",
    supervise = TRUE
  )
  registry_key <- as.character(process$get_pid())
  assign(
    registry_key, process,
    envir = .trajectory_bootstrap_process_registry
  )

  settled <- FALSE
  timed_out <- FALSE
  reject_callback <- NULL
  release <- function() {
    if (exists(
      registry_key, envir = .trajectory_bootstrap_process_registry,
      inherits = FALSE
    )) {
      rm(list = registry_key, envir = .trajectory_bootstrap_process_registry)
    }
    invisible(NULL)
  }
  settle <- function(callback, value) {
    if (settled) return(invisible(FALSE))
    settled <<- TRUE
    release()
    callback(value)
    invisible(TRUE)
  }
  terminate <- function() {
    if (tryCatch(process$is_alive(), error = function(error) FALSE)) {
      try(process$kill_tree(), silent = TRUE)
    }
    invisible(NULL)
  }

  promise <- promises::promise(function(resolve, reject) {
    reject_callback <<- reject
    poll <- NULL
    poll <- function() {
      if (settled) return(invisible(NULL))
      elapsed <- unname(proc.time()[["elapsed"]]) - started_at
      if (!timed_out && elapsed >= timeout_seconds) {
        timed_out <<- TRUE
        terminate()
      }
      alive <- tryCatch(process$is_alive(), error = function(error) FALSE)
      if (timed_out) {
        if (alive) {
          # Signal again in case the worker forked between the deadline check
          # and the first process-tree traversal.  Keep polling rather than
          # blocking the event loop while the operating system reaps it.
          terminate()
          later::later(poll, delay = poll_interval)
          return(invisible(NULL))
        }
        settle(reject, .trajectory_bootstrap_condition(
          sprintf(
            paste0(
              "The hosted bootstrap exceeded the executable %.1f-second ",
              "limit and its isolated worker was terminated. Reduce ",
              "repetitions, use selected axes, disable one bootstrap job, ",
              "or run the analysis offline."
            ),
            timeout_seconds
          ),
          "trajectory_bootstrap_timeout"
        ))
        return(invisible(NULL))
      }
      if (!alive) {
        value <- tryCatch(process$get_result(), error = function(error) error)
        if (inherits(value, "error")) {
          settle(reject, .trajectory_bootstrap_condition(
            paste0(
              "The isolated bootstrap worker failed: ", conditionMessage(value)
            ),
            "trajectory_bootstrap_worker_error"
          ))
        } else {
          settle(resolve, value)
        }
        return(invisible(NULL))
      }
      remaining <- max(0, timeout_seconds - elapsed)
      later::later(poll, delay = min(poll_interval, remaining))
      invisible(NULL)
    }
    later::later(poll, delay = 0)
  })

  cancel <- function(reason = "The isolated bootstrap job was cancelled.") {
    if (settled) return(invisible(FALSE))
    terminate()
    condition <- .trajectory_bootstrap_condition(
      reason, "trajectory_bootstrap_cancelled"
    )
    if (is.function(reject_callback)) {
      settle(reject_callback, condition)
    } else {
      settled <<- TRUE
      release()
    }
    invisible(TRUE)
  }

  structure(
    list(
      promise = promise,
      cancel = cancel,
      process = process,
      timeout_seconds = timeout_seconds,
      started_at = started_at
    ),
    class = "trajectory_bootstrap_job"
  )
}


.trajectory_points <- function(ena_obj) {
  if (is.null(ena_obj)) {
    stop("No ENA object is available.")
  }

  points <- if (is.data.frame(ena_obj)) ena_obj else ena_obj$points
  if (is.null(points) || !is.data.frame(points)) {
    stop("The ENA object must contain a data-frame-like $points component.")
  }
  if (nrow(points) == 0L) {
    stop("The ENA object contains no points.")
  }

  as.data.frame(points)
}


.trajectory_metadata_columns <- function(ena_obj, points, dimensions) {
  if (!is.data.frame(ena_obj) && !is.null(ena_obj$meta.data)) {
    metadata <- intersect(names(as.data.frame(ena_obj$meta.data)), names(points))
    if (length(metadata)) {
      return(.trajectory_user_metadata(metadata))
    }
  }

  .trajectory_user_metadata(setdiff(names(points), dimensions))
}


.trajectory_dimensions <- function(ena_obj, points, raw_dimensions = NULL) {
  supplied <- .trajectory_flatten_axes(raw_dimensions)
  supplied <- intersect(supplied, names(points))
  if (length(supplied)) {
    return(supplied)
  }

  if (!is.data.frame(ena_obj) && !is.null(ena_obj$rotation$nodes)) {
    node_names <- names(as.data.frame(ena_obj$rotation$nodes))
    node_dimensions <- intersect(names(points), setdiff(node_names, "code"))
    node_dimensions <- node_dimensions[vapply(
      points[node_dimensions], is.numeric, logical(1)
    )]
    if (length(node_dimensions)) {
      return(node_dimensions)
    }
  }

  numeric_columns <- names(points)[vapply(points, is.numeric, logical(1))]
  setdiff(numeric_columns, c("X", "x", "order", "time_order"))
}


.trajectory_declared_unit_vars <- function(ena_obj) {
  if (is.data.frame(ena_obj) || is.null(ena_obj$`_function.params`)) {
    return(character(0))
  }

  params <- ena_obj$`_function.params`
  declared <- params$units.by
  if (is.null(declared)) {
    declared <- params$groupVar
  }
  unique(as.character(.trajectory_or(declared, character(0))))
}


.trajectory_declared_default <- function(ena_obj, kind = c("time", "id", "group")) {
  kind <- match.arg(kind)
  if (is.data.frame(ena_obj) || is.null(ena_obj$`_function.params`)) {
    return(character(0))
  }
  key <- switch(
    kind,
    time = "trajectory.time.by",
    id = "trajectory.id.by",
    group = "trajectory.group.by"
  )
  explicit <- ena_obj$`_function.params`[[key]]
  explicit <- unique(as.character(.trajectory_or(explicit, character(0))))
  explicit[!is.na(explicit) & nzchar(explicit)]
}


.trajectory_default_variable <- function(columns, declared, kind = c("time", "id"),
                                         exclude = character(0)) {
  kind <- match.arg(kind)
  candidates <- setdiff(columns, exclude)
  if (!length(candidates)) {
    return(character(0))
  }

  declared <- intersect(declared, candidates)
  if (kind == "time" && length(declared)) {
    return(declared[[1]])
  }
  if (kind == "id" && length(declared)) {
    return(declared[[length(declared)]])
  }

  pattern <- if (kind == "time") {
    "^(time|week|wave|phase|period|session|date|order)$"
  } else {
    "^(id|name|person|participant|student|user|entity|subject)$"
  }
  matched <- candidates[grepl(pattern, candidates, ignore.case = TRUE)]
  if (length(matched)) matched[[1]] else candidates[[1]]
}


.trajectory_default_order <- function(values) {
  values <- values[!is.na(values)]
  if (!length(values)) {
    return(values)
  }

  if (inherits(values, "factor")) {
    # Factor levels often encode the study schedule. Preserve unused levels so
    # globally missing periods remain visible in the resulting path.
    return(levels(values))
  }
  if (inherits(values, "difftime")) {
    # sort()/unique() may strip the difftime class. Subset the original vector
    # instead so the UI-generated order retains both its class and its units.
    unique_values <- values[!duplicated(.trajectory_value_key(values))]
    return(unique_values[order(as.numeric(unique_values), na.last = NA)])
  }
  if (inherits(values, "Date") || inherits(values, "POSIXt") ||
      is.numeric(values) || is.logical(values)) {
    return(sort(unique(values)))
  }

  sort(unique(as.character(values)), method = "radix")
}


.trajectory_order_labels <- function(values) {
  if (inherits(values, "POSIXt")) {
    timezone <- attr(values, "tzone", exact = TRUE)
    if (is.null(timezone) || !length(timezone) || is.na(timezone[[1]]) ||
        !nzchar(timezone[[1]])) {
      timezone <- "UTC"
    } else {
      timezone <- timezone[[1]]
    }
    readable <- format(values, "%Y-%m-%dT%H:%M:%OS6%z", tz = timezone)
    epoch <- format(
      as.numeric(values), digits = 17L, scientific = TRUE, trim = TRUE
    )
    return(paste0(readable, " [epoch=", epoch, "]"))
  }
  if (inherits(values, "difftime")) {
    units <- attr(values, "units", exact = TRUE)
    if (is.null(units) || !length(units) || is.na(units[[1]]) ||
        !nzchar(units[[1]])) {
      units <- "secs"
    } else {
      units <- units[[1]]
    }
    numeric_values <- as.numeric(values)
    readable <- format(
      numeric_values, digits = 17L, scientific = TRUE, trim = TRUE
    )
    return(paste0(
      readable, " ", units, " [hex=", sprintf("%a", numeric_values), "]"
    ))
  }
  if (is.numeric(values) && !is.integer(values)) {
    readable <- format(values, digits = 17L, scientific = TRUE, trim = TRUE)
    return(paste0(readable, " [hex=", sprintf("%a", values), "]"))
  }
  as.character(values)
}


.trajectory_posix_epoch_token <- function(tokens) {
  pattern <- "\\[epoch=([^]]+)\\]$"
  has_epoch <- grepl(pattern, tokens)
  epoch <- rep(NA_real_, length(tokens))
  if (any(has_epoch)) {
    epoch[has_epoch] <- suppressWarnings(as.numeric(sub(
      paste0(".*", pattern), "\\1", tokens[has_epoch]
    )))
  }
  list(has_epoch = has_epoch, epoch = epoch)
}


.trajectory_numeric_hex_token <- function(tokens) {
  pattern <- "\\[hex=([^]]+)\\]$"
  has_hex <- grepl(pattern, tokens)
  value <- rep(NA_real_, length(tokens))
  if (any(has_hex)) {
    value[has_hex] <- suppressWarnings(as.numeric(sub(
      paste0(".*", pattern), "\\1", tokens[has_hex]
    )))
  }
  list(has_hex = has_hex, value = value)
}


.trajectory_parse_difftime_tokens <- function(tokens, observed) {
  units <- attr(observed, "units", exact = TRUE)
  if (is.null(units) || !length(units) || is.na(units[[1]]) ||
      !nzchar(units[[1]])) {
    units <- "secs"
  } else {
    units <- units[[1]]
  }

  hex_token <- .trajectory_numeric_hex_token(tokens)
  numeric_values <- hex_token$value
  fallback <- !hex_token$has_hex
  if (any(fallback)) {
    readable <- sub("[[:space:]]*\\[hex=[^]]+\\]$", "", tokens)
    readable <- sub(
      paste0("[[:space:]]+", units, "$"), "", readable
    )
    numeric_values[fallback] <- suppressWarnings(as.numeric(
      trimws(readable[fallback])
    ))
  }
  as.difftime(numeric_values, units = units)
}


.trajectory_parse_order <- function(text, observed) {
  normalized_text <- gsub("\r\n?", "\n", .trajectory_or(text, ""))
  # Generated orders use one value per line so labels containing commas remain
  # lossless. A single legacy comma-separated line is still accepted.
  has_newline <- grepl("\n", normalized_text, fixed = TRUE)
  observed_labels <- if (is.factor(observed)) {
    levels(observed)
  } else if (is.character(observed)) {
    as.character(observed[!is.na(observed)])
  } else {
    character(0)
  }
  # A one-value character order has no newline delimiter. Prefer an exact
  # observed label over legacy comma splitting so a value such as ` phase, 1 `
  # remains lossless too.
  exact_character_label <- !has_newline &&
    normalized_text %in% observed_labels
  separator <- if (has_newline || exact_character_label) "\n" else ","
  tokens <- unlist(strsplit(normalized_text, separator, fixed = TRUE))
  # Comma-separated input historically treats surrounding whitespace as UI
  # formatting. Newline-separated input is also the lossless generated format,
  # so leading/trailing whitespace can be part of an actual character value.
  if (identical(separator, ",")) tokens <- trimws(tokens)
  tokens <- tokens[nzchar(tokens)]
  if (!length(tokens)) {
    return(.trajectory_default_order(observed))
  }

  if (inherits(observed, "Date")) {
    parsed <- as.Date(tokens)
  } else if (inherits(observed, "POSIXt")) {
    timezone <- attr(observed, "tzone", exact = TRUE)
    if (is.null(timezone) || !length(timezone) || is.na(timezone[[1]]) ||
        !nzchar(timezone[[1]])) {
      timezone <- "UTC"
    } else {
      timezone <- timezone[[1]]
    }
    epoch_token <- .trajectory_posix_epoch_token(tokens)
    parsed <- as.POSIXct(rep(NA_real_, length(tokens)), origin = "1970-01-01",
                         tz = timezone)
    if (any(epoch_token$has_epoch)) {
      parsed[epoch_token$has_epoch] <- as.POSIXct(
        epoch_token$epoch[epoch_token$has_epoch], origin = "1970-01-01",
        tz = timezone
      )
    }
    readable_tokens <- sub("[[:space:]]*\\[epoch=[^]]+\\]$", "", tokens)
    fallback <- is.na(parsed)
    parsed[fallback] <- suppressWarnings(as.POSIXct(
      readable_tokens[fallback], format = "%Y-%m-%dT%H:%M:%OS%z", tz = timezone
    ))
    fallback <- is.na(parsed)
    if (any(fallback)) {
      parsed[fallback] <- suppressWarnings(as.POSIXct(
        readable_tokens[fallback],
        tz = timezone,
        tryFormats = c(
          "%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS",
          "%Y-%m-%d", "%Y/%m/%d"
        )
      ))
    }
  } else if (inherits(observed, "difftime")) {
    parsed <- .trajectory_parse_difftime_tokens(tokens, observed)
  } else if (is.integer(observed)) {
    numeric_tokens <- suppressWarnings(as.numeric(tokens))
    parsed <- suppressWarnings(as.integer(numeric_tokens))
    invalid_integer <- is.na(numeric_tokens) | numeric_tokens != parsed
    if (any(invalid_integer)) {
      stop("The time order contains a value that is not an integer.")
    }
  } else if (is.numeric(observed)) {
    hex_token <- .trajectory_numeric_hex_token(tokens)
    parsed <- hex_token$value
    fallback <- !hex_token$has_hex
    parsed[fallback] <- suppressWarnings(as.numeric(tokens[fallback]))
  } else if (is.logical(observed)) {
    parsed <- match(tolower(tokens), c("false", "true")) - 1L
    parsed <- as.logical(parsed)
  } else {
    parsed <- tokens
  }

  if (anyNA(parsed)) {
    stop("One or more ordered time values could not be parsed.")
  }

  order_key <- function(values) .trajectory_value_key(values)
  parsed_labels <- order_key(parsed)
  observed_labels <- unique(order_key(observed[!is.na(observed)]))
  if (anyDuplicated(parsed_labels)) {
    stop("Each ordered time value must appear exactly once.")
  }

  missing_values <- setdiff(observed_labels, parsed_labels)
  if (length(missing_values)) {
    stop(
      "The ordered time values omit observed data values (missing: ",
      paste(missing_values, collapse = ", "), ")."
    )
  }

  parsed
}


.trajectory_time_filter_mask <- function(values, selected_time) {
  if (is.null(selected_time) || !length(selected_time)) {
    return(rep(FALSE, length(values)))
  }
  target <- tryCatch({
    if (inherits(values, "POSIXt")) {
      timezone <- attr(values, "tzone", exact = TRUE)
      if (is.null(timezone) || !length(timezone) || is.na(timezone[[1]]) ||
          !nzchar(timezone[[1]])) timezone <- "UTC" else timezone <- timezone[[1]]
      if (inherits(selected_time, "POSIXt")) {
        as.POSIXct(selected_time, tz = timezone)
      } else {
        token <- .trajectory_posix_epoch_token(as.character(selected_time))
        if (token$has_epoch[[1]] && is.finite(token$epoch[[1]])) {
          as.POSIXct(token$epoch[[1]], origin = "1970-01-01", tz = timezone)
        } else {
          as.POSIXct(as.character(selected_time), tz = timezone)
        }
      }
    } else if (inherits(values, "Date")) {
      as.Date(selected_time)
    } else if (inherits(values, "difftime")) {
      .trajectory_parse_difftime_tokens(as.character(selected_time), values)
    } else if (is.integer(values)) {
      as.integer(as.numeric(selected_time))
    } else if (is.numeric(values)) {
      token <- .trajectory_numeric_hex_token(as.character(selected_time))
      if (token$has_hex[[1]] && is.finite(token$value[[1]])) {
        token$value[[1]]
      } else {
        as.numeric(selected_time)
      }
    } else if (is.logical(values)) {
      as.logical(selected_time)
    } else {
      as.character(selected_time)
    }
  }, error = function(error) NULL)
  if (is.null(target) || !length(target) || any(.trajectory_is_missing(target))) {
    return(rep(FALSE, length(values)))
  }
  .trajectory_value_key(values) %in% .trajectory_value_key(target)
}


.trajectory_condition_values <- function(points, group_var) {
  if (is.null(group_var) || !nzchar(group_var) || !group_var %in% names(points)) {
    return(character(0))
  }
  unique(as.character(points[[group_var]][!is.na(points[[group_var]])]))
}


.trajectory_comparison_overlap <- function(points, group_var, level_a, level_b,
                                           id_var, time_var) {
  empty <- list(
    n_a = 0L, n_b = 0L, n_overlap_ids = 0L, n_matched_id_times = 0L,
    overlap_fraction_a = 0, overlap_fraction_b = 0
  )
  required <- c(group_var, id_var, time_var)
  if (!is.data.frame(points) || anyNA(required) || any(!nzchar(required)) ||
      !all(required %in% names(points)) || !nzchar(level_a) ||
      !nzchar(level_b) || identical(level_a, level_b)) return(empty)

  valid <- !.trajectory_is_missing(points[[group_var]]) &
    !.trajectory_is_missing(points[[id_var]]) &
    !.trajectory_is_missing(points[[time_var]])
  group_values <- as.character(points[[group_var]])
  frame <- data.frame(
    group = group_values[valid],
    id = .trajectory_value_key(points[[id_var]][valid]),
    time = .trajectory_value_key(points[[time_var]][valid]),
    stringsAsFactors = FALSE
  )
  a <- frame[frame$group == level_a, , drop = FALSE]
  b <- frame[frame$group == level_b, , drop = FALSE]
  ids_a <- unique(a$id)
  ids_b <- unique(b$id)
  overlap <- intersect(ids_a, ids_b)
  pair_key <- function(data) paste(data$id, data$time, sep = "\r")
  n_pairs <- length(intersect(unique(pair_key(a)), unique(pair_key(b))))
  list(
    n_a = length(ids_a),
    n_b = length(ids_b),
    n_overlap_ids = length(overlap),
    n_matched_id_times = n_pairs,
    overlap_fraction_a = if (length(ids_a)) length(overlap) / length(ids_a) else 0,
    overlap_fraction_b = if (length(ids_b)) length(overlap) / length(ids_b) else 0
  )
}


.trajectory_comparison_overlap_message <- function(overlap) {
  paste0(
    "Raw-ID overlap: ", overlap$n_overlap_ids, " IDs (",
    round(100 * overlap$overlap_fraction_a, 1), "% of A; ",
    round(100 * overlap$overlap_fraction_b, 1), "% of B), with ",
    overlap$n_matched_id_times, " exact ID-time matches. Pairing is valid only ",
    "when an overlapping raw ID denotes the same physical entity."
  )
}


.trajectory_module_diagnostics_from <- function(value, source) {
  diagnostics <- attr(value, "trajectory_warnings", exact = TRUE)
  if (is.null(diagnostics) ||
      (is.data.frame(diagnostics) && nrow(diagnostics) == 0L) ||
      (!is.data.frame(diagnostics) && !length(diagnostics))) {
    return(data.frame(
      source = character(0), code = character(0), severity = character(0),
      message = character(0), stringsAsFactors = FALSE
    ))
  }

  if (!is.data.frame(diagnostics)) {
    diagnostics <- data.frame(
      code = "trajectory_warning", severity = "warning",
      message = as.character(diagnostics), stringsAsFactors = FALSE
    )
  } else {
    diagnostics <- as.data.frame(diagnostics)
  }

  if (!"code" %in% names(diagnostics)) diagnostics$code <- "trajectory_warning"
  if (!"severity" %in% names(diagnostics)) diagnostics$severity <- "warning"
  if (!"message" %in% names(diagnostics)) {
    diagnostics$message <- apply(diagnostics, 1L, paste, collapse = " ")
  }
  diagnostics$source <- source

  leading <- c("source", "code", "severity", "message")
  diagnostics[c(leading, setdiff(names(diagnostics), leading))]
}


.trajectory_module_diagnostic <- function(code, message, severity = "warning") {
  data.frame(
    source = "module", code = code, severity = severity, message = message,
    stringsAsFactors = FALSE
  )
}


.trajectory_remove_inherited_diagnostics <- function(diagnostics, inherited) {
  if (!is.data.frame(diagnostics) || !nrow(diagnostics) ||
      !is.data.frame(inherited) || !nrow(inherited)) {
    return(diagnostics)
  }

  # bootstrap_centroid_path() intentionally carries the base path diagnostics
  # on its result. The module exports the base path separately, so remove only
  # byte-for-byte equivalent payload rows inherited by the bootstrap result.
  # `source` is excluded from comparison because it describes the wrapper from
  # which the row was read, not the diagnostic's analytical context.
  fields <- union(
    setdiff(names(diagnostics), "source"),
    setdiff(names(inherited), "source")
  )
  normalize <- function(value) {
    missing_fields <- setdiff(fields, names(value))
    for (field in missing_fields) value[[field]] <- NA
    value <- value[fields]
    rownames(value) <- NULL
    value
  }
  candidate_payload <- normalize(diagnostics)
  inherited_payload <- normalize(inherited)
  inherited_row <- vapply(seq_len(nrow(candidate_payload)), function(index) {
    candidate <- candidate_payload[index, , drop = FALSE]
    rownames(candidate) <- NULL
    any(vapply(seq_len(nrow(inherited_payload)), function(parent_index) {
      parent <- inherited_payload[parent_index, , drop = FALSE]
      rownames(parent) <- NULL
      identical(candidate, parent)
    }, logical(1)))
  }, logical(1))

  diagnostics[!inherited_row, , drop = FALSE]
}


.trajectory_bind_diagnostics <- function(...) {
  values <- list(...)
  values <- values[vapply(values, nrow, integer(1)) > 0L]
  if (!length(values)) {
    return(data.frame(
      source = character(0), code = character(0), severity = character(0),
      message = character(0), stringsAsFactors = FALSE
    ))
  }

  all_names <- unique(unlist(lapply(values, names), use.names = FALSE))
  values <- lapply(values, function(value) {
    missing_names <- setdiff(all_names, names(value))
    for (name in missing_names) value[[name]] <- NA
    value[all_names]
  })
  do.call(rbind, values)
}


.trajectory_colors <- function(group_colors) {
  group_colors <- .trajectory_resolve_value(group_colors)
  if (is.null(group_colors) || !length(group_colors)) {
    return(NULL)
  }

  if (is.data.frame(group_colors) || is.matrix(group_colors)) {
    group_colors <- as.data.frame(group_colors, stringsAsFactors = FALSE)
    color_col <- intersect(c("color", "colour"), names(group_colors))
    group_col <- intersect(c("group", "condition", "name", "label"), names(group_colors))
    if (length(color_col) && length(group_col)) {
      colors <- as.character(group_colors[[color_col[[1]]]])
      names(colors) <- as.character(group_colors[[group_col[[1]]]])
      return(colors)
    }
    if (ncol(group_colors) >= 2L) {
      colors <- as.character(group_colors[[1]])
      names(colors) <- as.character(group_colors[[2]])
      return(colors)
    }
  }

  colors <- unlist(group_colors, use.names = TRUE)
  color_names <- names(colors)
  colors <- as.character(colors)
  names(colors) <- color_names
  colors
}


.trajectory_network_overlay <- function(ena_obj, dimensions, time_var,
                                        selected_time, group_var = NULL,
                                        selected_group = NULL) {
  unavailable <- function(message) {
    list(code_nodes = NULL, network_edges = NULL, message = message)
  }

  if (is.null(ena_obj) || is.data.frame(ena_obj) ||
      is.null(ena_obj$rotation$nodes) || is.null(ena_obj$line.weights) ||
      is.null(ena_obj$rotation$adjacency.key)) {
    return(unavailable("This ENA object has no compatible network components."))
  }

  nodes <- as.data.frame(ena_obj$rotation$nodes)
  line_weights <- as.data.frame(ena_obj$line.weights)
  adjacency <- as.data.frame(ena_obj$rotation$adjacency.key)
  required <- dimensions[seq_len(min(3L, length(dimensions)))]
  if (!length(required) || !all(required %in% names(nodes))) {
    return(unavailable("Network nodes do not contain the displayed ENA axes."))
  }
  if (!"code" %in% names(nodes) || nrow(adjacency) < 2L) {
    return(unavailable("Network node labels or adjacency pairs are unavailable."))
  }

  if (!is.null(time_var) && nzchar(time_var) && time_var %in% names(line_weights) &&
      !is.null(selected_time) && nzchar(selected_time)) {
    keep <- .trajectory_time_filter_mask(
      line_weights[[time_var]], selected_time
    )
    keep[is.na(keep)] <- FALSE
    line_weights <- line_weights[keep, , drop = FALSE]
  }
  grouped <- !is.null(group_var) && length(group_var) == 1L &&
    !is.na(group_var) && nzchar(group_var)
  selected_group <- .trajectory_or(selected_group, "")
  if (grouped && nzchar(selected_group)) {
    if (!group_var %in% names(line_weights)) {
      return(unavailable(paste0(
        "The selected network group column `", group_var,
        "` is absent from line weights."
      )))
    }
    keep <- !is.na(line_weights[[group_var]]) &
      as.character(line_weights[[group_var]]) == as.character(selected_group)
    line_weights <- line_weights[keep, , drop = FALSE]
  }
  if (!nrow(line_weights)) {
    scope <- if (grouped && nzchar(selected_group)) {
      paste0(" for ", group_var, " = ", selected_group)
    } else {
      ""
    }
    return(unavailable(paste0(
      "No network rows exist at the selected time", scope, "."
    )))
  }

  adjacency_from <- vapply(seq_len(ncol(adjacency)), function(index) {
    as.character(adjacency[[index]][[1]])
  }, character(1L))
  adjacency_to <- vapply(seq_len(ncol(adjacency)), function(index) {
    as.character(adjacency[[index]][[2]])
  }, character(1L))
  forward_names <- paste(adjacency_from, adjacency_to, sep = " & ")
  reverse_names <- paste(adjacency_to, adjacency_from, sep = " & ")
  edge_positions <- match(forward_names, names(line_weights))
  reverse_needed <- is.na(edge_positions)
  edge_positions[reverse_needed] <- match(
    reverse_names[reverse_needed], names(line_weights)
  )
  if (anyNA(edge_positions) || anyDuplicated(edge_positions)) {
    missing_edges <- forward_names[is.na(edge_positions)]
    detail <- if (length(missing_edges)) {
      paste0(" Missing: ", paste(head(missing_edges, 5L), collapse = ", "), ".")
    } else {
      " Edge columns are duplicated or ambiguous."
    }
    return(unavailable(paste0(
      "Network edge columns do not exactly match adjacency endpoints; the ",
      "overlay was withheld to prevent positional edge/weight mismapping.",
      detail
    )))
  }
  edge_columns <- names(line_weights)[edge_positions]

  mean_weights <- vapply(edge_columns, function(column) {
    values <- suppressWarnings(as.numeric(line_weights[[column]]))
    value <- mean(values, na.rm = TRUE)
    if (is.finite(value)) value else 0
  }, numeric(1))
  keep_edges <- is.finite(mean_weights) & abs(mean_weights) > sqrt(.Machine$double.eps)
  if (!any(keep_edges)) {
    return(unavailable("The selected-time mean network has no non-zero edges."))
  }

  edge_indices <- which(keep_edges)
  node_codes <- as.character(nodes$code)
  edge_rows <- lapply(edge_indices, function(index) {
    from <- as.character(adjacency[[index]][[1]])
    to <- as.character(adjacency[[index]][[2]])
    from_index <- match(from, node_codes)
    to_index <- match(to, node_codes)
    if (is.na(from_index) || is.na(to_index)) return(NULL)

    row <- data.frame(
      x = nodes[[required[[1]]]][[from_index]],
      y = nodes[[required[[2]]]][[from_index]],
      xend = nodes[[required[[1]]]][[to_index]],
      yend = nodes[[required[[2]]]][[to_index]],
      label = paste(from, "-", to),
      weight = mean_weights[[index]],
      time_value = as.character(selected_time),
      stringsAsFactors = FALSE
    )
    if (length(required) >= 3L) {
      row$z <- nodes[[required[[3]]]][[from_index]]
      row$zend <- nodes[[required[[3]]]][[to_index]]
    }
    row
  })
  edge_rows <- edge_rows[!vapply(edge_rows, is.null, logical(1))]
  if (!length(edge_rows)) {
    return(unavailable("Network edges could not be matched to code nodes."))
  }
  network_edges <- do.call(rbind, edge_rows)
  max_weight <- max(abs(network_edges$weight), na.rm = TRUE)
  network_edges$width <- 0.5 + 3.5 * abs(network_edges$weight) / max_weight
  network_edges$sign <- ifelse(network_edges$weight < 0, "negative", "positive")
  network_edges$color <- ifelse(
    network_edges$weight < 0,
    "rgba(30,90,150,0.55)",
    "rgba(160,65,45,0.55)"
  )

  code_nodes <- data.frame(
    label = node_codes,
    x = nodes[[required[[1]]]],
    y = nodes[[required[[2]]]],
    color = "rgba(60,60,60,0.75)",
    size = 6,
    stringsAsFactors = FALSE
  )
  if (length(required) >= 3L) code_nodes$z <- nodes[[required[[3]]]]

  scope_message <- if (grouped && nzchar(selected_group)) {
    paste0(group_var, " = ", selected_group)
  } else if (grouped) {
    paste0("overall across all `", group_var, "` groups")
  } else {
    "overall"
  }

  list(
    code_nodes = code_nodes,
    network_edges = network_edges,
    message = paste(
      nrow(network_edges), "non-zero mean edges at", selected_time,
      "(", scope_message, "). Contextual overlay uses all matching line-weight rows;",
      "it does not apply the trajectory cohort policy."
    )
  )
}


.trajectory_export_metadata <- function(data, metadata) {
  data <- as.data.frame(data)
  if (!nrow(data)) return(data)

  metadata_columns <- paste0(".analysis_", names(metadata))
  collisions <- unique(c(
    names(data)[startsWith(names(data), ".analysis_")],
    intersect(metadata_columns, names(data))
  ))
  if (length(collisions)) {
    stop(
      paste0(
        "Analysis export metadata column collision: ",
        paste(collisions, collapse = ", "),
        ". Rename the source time/group field or reserved `.analysis_*` column."
      ),
      call. = FALSE
    )
  }

  for (name in names(metadata)) {
    value <- metadata[[name]]
    if (length(value) > 1L) value <- paste(value, collapse = ";")
    if (!length(value) || is.null(value)) value <- NA_character_
    data[[paste0(".analysis_", name)]] <- as.character(value)
  }
  data
}


.trajectory_spreadsheet_safe_text <- function(value) {
  if (exists("ena3d_spreadsheet_safe_text", mode = "function", inherits = TRUE)) {
    return(ena3d_spreadsheet_safe_text(value))
  }
  value <- as.character(value)
  dangerous <- !is.na(value) & grepl("^[[:space:]]*[=+@-]", value)
  value[dangerous] <- paste0("'", value[dangerous])
  value
}


.trajectory_spreadsheet_safe_frame <- function(data) {
  if (exists("ena3d_spreadsheet_safe_frame", mode = "function", inherits = TRUE)) {
    return(ena3d_spreadsheet_safe_frame(data))
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE, optional = TRUE)
  original_names <- names(data)
  for (name in names(data)) {
    if (is.factor(data[[name]]) || is.character(data[[name]])) {
      data[[name]] <- .trajectory_spreadsheet_safe_text(data[[name]])
    }
  }
  dangerous_headers <- !is.na(original_names) &
    grepl("^'*[[:space:]]*[=+@-]", original_names)
  names(data)[dangerous_headers] <- paste0("'", original_names[dangerous_headers])
  if (!anyDuplicated(original_names) && anyDuplicated(names(data))) {
    stop(
      "Spreadsheet-safe CSV header escaping produced duplicate names.",
      call. = FALSE
    )
  }
  data
}


.trajectory_write_csv <- function(data, file) {
  if (exists("ena3d_write_safe_csv", mode = "function", inherits = TRUE)) {
    return(ena3d_write_safe_csv(data, file))
  }
  utils::write.csv(
    .trajectory_spreadsheet_safe_frame(data),
    file,
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
}


.trajectory_dataset_hash <- function(object, algorithm = c("sha256", "md5")) {
  algorithm <- match.arg(algorithm)
  path <- tempfile("ena3d-dataset-", fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  tryCatch({
    saveRDS(object, path, version = 3L, compress = FALSE)
    if (identical(algorithm, "sha256")) {
      if (!requireNamespace("digest", quietly = TRUE)) {
        stop("The digest package is required for SHA-256 provenance.")
      }
      digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
    } else {
      unname(tools::md5sum(path))
    }
  }, error = function(error) NA_character_)
}


.trajectory_package_versions <- function(packages = c(
    "shiny", "plotly", "rENA", "data.table"
  )) {
  versions <- vapply(packages, function(package) {
    if (!requireNamespace(package, quietly = TRUE)) return("not-installed")
    as.character(utils::packageVersion(package))
  }, character(1))
  paste(paste0(packages, "=", versions), collapse = ";")
}


.trajectory_node_legend_ui <- function(path) {
  if (!exists("trajectory_node_legend_data", mode = "function", inherits = TRUE)) {
    return(NULL)
  }
  legend_data <- trajectory_node_legend_data(path)
  if (!nrow(legend_data)) return(NULL)
  time_variable <- attr(legend_data, "time_variable", exact = TRUE)
  if (is.null(time_variable) || !length(time_variable) || is.na(time_variable[1L])) {
    time_variable <- "Time"
  }

  shiny::tags$div(
    class = "trajectory-node-legend",
    shiny::tags$h3("Trajectory nodes"),
    shiny::tags$p(
      class = "trajectory-node-legend-subtitle",
      paste0("Ordered period \u00b7 ", time_variable)
    ),
    shiny::tags$ol(
      class = "trajectory-node-legend-list",
      lapply(seq_len(nrow(legend_data)), function(index) {
        shiny::tags$li(
          class = paste(
            "trajectory-node-legend-item",
            if (isTRUE(legend_data$is_ordered[index])) "is-ordered" else "is-unordered"
          ),
          `data-node-key` = legend_data$node_key[index],
          `data-node-color` = legend_data$node_color[index],
          shiny::tags$span(
            class = "trajectory-node-legend-swatch",
            style = paste0("background-color:", legend_data$node_color[index], ";"),
            `aria-hidden` = "true"
          ),
          shiny::tags$span(
            class = "trajectory-node-legend-label",
            legend_data$node_label[index]
          )
        )
      })
    )
  )
}


.trajectory_env_value <- function(name, default = "unknown") {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) default else value
}


.trajectory_rotation_provenance <- function(ena_obj, dimensions) {
  if (is.data.frame(ena_obj)) {
    return(list(
      ena_object_class = paste(class(ena_obj), collapse = "/"),
      rotation_class = "not supplied (point table)",
      rotation_sha256 = NA_character_,
      rotation_nodes = NA_integer_,
      rotation_edges = NA_integer_,
      rotation_dimensions = paste(dimensions, collapse = ";"),
      ena_parameters = NA_character_
    ))
  }
  nodes <- ena_obj$rotation$nodes
  adjacency <- ena_obj$rotation$adjacency.key
  params <- ena_obj$`_function.params`
  keep_params <- intersect(
    c(
      "rotation", "rotation.method", "model", "units.by", "groupVar",
      "window.size", "weight.by", "norm.by", "sphere.norm"
    ),
    names(params)
  )
  parameter_text <- vapply(keep_params, function(name) {
    value <- params[[name]]
    value <- tryCatch({
      if (is.function(value)) {
        "<function>"
      } else if (is.environment(value)) {
        "<environment>"
      } else {
        if (is.list(value)) {
          value <- unlist(value, recursive = TRUE, use.names = FALSE)
        }
        head(as.character(value), 20L)
      }
    }, error = function(error) paste0("<", typeof(value), ">"))
    paste0(name, "=", paste(value, collapse = "|"))
  }, character(1))
  list(
    ena_object_class = paste(class(ena_obj), collapse = "/"),
    rotation_class = paste(class(ena_obj$rotation), collapse = "/"),
    rotation_sha256 = .trajectory_dataset_hash(
      list(nodes = nodes, adjacency_key = adjacency), "sha256"
    ),
    rotation_nodes = if (is.data.frame(nodes) || is.matrix(nodes)) nrow(nodes) else NA_integer_,
    rotation_edges = if (is.data.frame(adjacency) || is.matrix(adjacency)) ncol(adjacency) else NA_integer_,
    rotation_dimensions = paste(dimensions, collapse = ";"),
    ena_parameters = if (length(parameter_text)) {
      paste(parameter_text, collapse = ";")
    } else {
      NA_character_
    }
  )
}


.trajectory_provenance_metadata <- function(ena_obj, dimensions) {
  c(
    list(
      dataset_sha256 = .trajectory_dataset_hash(ena_obj, "sha256"),
      dataset_md5 = .trajectory_dataset_hash(ena_obj, "md5"),
      dataset_hash_algorithm = "SHA-256 of saveRDS(version=3, compress=FALSE)",
      app_version = .trajectory_env_value("ENA3D_APP_VERSION", "development"),
      build_id = .trajectory_env_value("ENA3D_BUILD_ID"),
      git_commit = .trajectory_env_value("ENA3D_GIT_COMMIT"),
      r_version = R.version.string,
      r_platform = R.version$platform,
      package_versions = .trajectory_package_versions()
    ),
    .trajectory_rotation_provenance(ena_obj, dimensions)
  )
}


.trajectory_metadata_serialize <- function(value) {
  if (is.null(value) || !length(value)) return(NA_character_)
  paste(capture.output(dput(value)), collapse = "")
}


.trajectory_bootstrap_spec_metadata <- function(spec, prefix) {
  if (is.null(spec)) spec <- list()
  values <- list(
    method = .trajectory_or(spec$method, NA_character_),
    sampling_unit = .trajectory_or(spec$sampling_unit, NA_character_),
    rows_per_sampled_id = .trajectory_or(spec$rows_per_sampled_id, NA_character_),
    n_participants = .trajectory_or(spec$n_participants, NA_integer_),
    n_sampling_units = .trajectory_or(spec$n_sampling_units, NA_integer_),
    design_requested = .trajectory_or(
      spec$bootstrap_design_requested, NA_character_
    ),
    design_resolved = .trajectory_or(spec$bootstrap_design, NA_character_),
    stratum_sizes = .trajectory_metadata_serialize(spec$stratum_sizes),
    eligible_id_keys = .trajectory_metadata_serialize(spec$eligible_id_keys),
    eligible_id_keys_by_stratum = .trajectory_metadata_serialize(
      spec$eligible_id_keys_by_stratum
    ),
    n_boot = .trajectory_or(spec$n_boot, NA_integer_),
    confidence = .trajectory_or(spec$conf_level, NA_real_),
    minimum_valid_fraction = .trajectory_or(
      spec$minimum_valid_fraction, NA_real_
    ),
    minimum_tail_replicates = .trajectory_or(
      spec$minimum_tail_replicates, NA_integer_
    ),
    minimum_valid_replicates = .trajectory_or(
      spec$minimum_valid_replicates, NA_integer_
    ),
    seed = .trajectory_or(spec$seed, NA_integer_),
    failed_replicates = .trajectory_or(spec$failed_replicates, NA_integer_),
    rng_state_restored = .trajectory_or(spec$rng_state_restored, NA)
  )
  names(values) <- paste0(prefix, names(values))
  values
}


.trajectory_bootstrap_metadata <- function(bootstrap, comparison) {
  bootstrap_spec <- if (is.null(bootstrap)) list() else {
    attr(bootstrap, "bootstrap_spec", exact = TRUE)
  }
  comparison_spec <- if (is.null(comparison)) list() else {
    attr(comparison, "bootstrap_spec", exact = TRUE)
  }
  result <- c(
    .trajectory_bootstrap_spec_metadata(bootstrap_spec, "bootstrap_"),
    .trajectory_bootstrap_spec_metadata(
      comparison_spec, "comparison_bootstrap_"
    )
  )
  # Preserve the original public field while adding the complete namespaced
  # comparison specification above.
  result$comparison_failed_replicates <- result$comparison_bootstrap_failed_replicates
  result
}


.trajectory_metadata_table <- function(metadata) {
  data.frame(
    field = names(metadata),
    value = vapply(metadata, function(value) {
      if (!length(value) || is.null(value)) return(NA_character_)
      paste(as.character(value), collapse = ";")
    }, character(1)),
    stringsAsFactors = FALSE
  )
}


.trajectory_safe_file_part <- function(value) {
  value <- gsub("[^A-Za-z0-9._-]+", "-", as.character(value))
  gsub("(^-+|-+$)", "", value)
}


.trajectory_download_controls <- function(result, ns = identity) {
  if (is.null(result)) return(NULL)
  controls <- list(
    shiny::downloadButton(ns("download_bundle"), "Analysis bundle ZIP"),
    shiny::downloadButton(ns("download_path"), "Path CSV")
  )
  if (!is.null(result$bootstrap)) {
    controls <- c(controls, list(
      shiny::downloadButton(ns("download_uncertainty"), "Uncertainty CSV")
    ))
  }
  if (!is.null(result$comparison)) {
    controls <- c(controls, list(
      shiny::downloadButton(ns("download_comparison"), "Comparison CSV")
    ))
  }
  controls <- c(controls, list(
    shiny::downloadButton(ns("download_metadata"), "Metadata CSV")
  ))
  shiny::tags$div(
    class = "trajectory-downloads",
    style = "margin-top: 0.75rem; display: flex; gap: 0.4rem; flex-wrap: wrap;",
    controls
  )
}


.trajectory_write_bundle <- function(result, file) {
  if (is.null(result) || is.null(result$path)) {
    stop("A completed trajectory result is required for the analysis bundle.")
  }
  if (!requireNamespace("zip", quietly = TRUE)) {
    stop("The zip package is required to create an analysis bundle.")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The jsonlite package is required to create an analysis manifest.")
  }

  bundle_dir <- tempfile("ena3d-trajectory-bundle-")
  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(bundle_dir, recursive = TRUE, force = TRUE), add = TRUE)
  written <- character(0)
  write_result <- function(name, data) {
    path <- file.path(bundle_dir, name)
    .trajectory_write_csv(
      .trajectory_export_metadata(data, result$metadata), path
    )
    written <<- c(written, name)
  }

  write_result("path.csv", result$path)
  if (!is.null(result$bootstrap)) {
    write_result("uncertainty.csv", result$bootstrap)
  }
  if (!is.null(result$comparison)) {
    write_result("comparison.csv", result$comparison)
  }

  diagnostics_path <- file.path(bundle_dir, "diagnostics.csv")
  diagnostics <- as.data.frame(result$diagnostics, stringsAsFactors = FALSE)
  if (!ncol(diagnostics)) {
    diagnostics <- data.frame(
      source = character(0), code = character(0), severity = character(0),
      message = character(0), stringsAsFactors = FALSE
    )
  }
  .trajectory_write_csv(diagnostics, diagnostics_path)
  written <- c(written, "diagnostics.csv")

  metadata_path <- file.path(bundle_dir, "metadata.csv")
  .trajectory_write_csv(.trajectory_metadata_table(result$metadata), metadata_path)
  written <- c(written, "metadata.csv")

  manifest <- list(
    schema = "urn:3dena:trajectory-analysis-bundle:1",
    schema_version = 1L,
    files = written,
    metadata = result$metadata,
    settings = result$settings,
    diagnostics = diagnostics
  )
  manifest_path <- file.path(bundle_dir, "manifest.json")
  jsonlite::write_json(
    manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE,
    na = "null", null = "null", digits = NA
  )
  written <- c(written, "manifest.json")

  zip::zipr(
    zipfile = file,
    files = file.path(bundle_dir, written),
    root = bundle_dir,
    include_directories = FALSE
  )
  invisible(written)
}


trajectory_server <- function(id, ena_obj, selected_axes = NULL,
                              raw_dimensions = NULL, group_colors = NULL,
                              camera = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    analysis_result <- shiny::reactiveVal(NULL)
    analysis_source <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal(
      "Load an ENA object, choose variables, then run the trajectory analysis."
    )
    bootstrap_state <- new.env(parent = emptyenv())
    bootstrap_state$active <- NULL
    bootstrap_state$generation <- 0L

    cancel_active_bootstrap <- function(reason) {
      bootstrap_state$generation <- bootstrap_state$generation + 1L
      job <- bootstrap_state$active
      bootstrap_state$active <- NULL
      if (is.null(job)) return(invisible(FALSE))
      try(job$close_progress(), silent = TRUE)
      try(job$worker$cancel(reason), silent = TRUE)
      invisible(TRUE)
    }

    session$onSessionEnded(function() {
      cancel_active_bootstrap(
        "The isolated bootstrap job was cancelled because its Shiny session ended."
      )
    })

    current_ena_obj <- shiny::reactive({
      .trajectory_resolve_value(ena_obj)
    })

    source_signature <- shiny::reactive({
      # Reading these reactives is enough to invalidate an old result. The
      # object itself remains raw and is not hashed, copied, or transformed.
      list(
        object = current_ena_obj(),
        raw_dimensions = .trajectory_flatten_axes(raw_dimensions)
      )
    })

    shiny::observeEvent(source_signature(), {
      cancelled <- cancel_active_bootstrap(
        "The isolated bootstrap job was cancelled because the dataset or rotation changed."
      )
      if (cancelled || !is.null(analysis_result())) {
        analysis_result(NULL)
        analysis_source(NULL)
        status("The ENA dataset or rotation changed. Run the trajectory analysis again.")
      }
    }, ignoreInit = TRUE, priority = 100)

    analytical_settings <- shiny::reactive({
      list(
        time_var = input$time_var,
        id_var = input$id_var,
        group_var = input$group_var,
        condition_a = input$condition_a,
        condition_b = input$condition_b,
        run_comparison = input$run_comparison,
        confirm_paired_ids = input$confirm_paired_ids,
        time_order = input$time_order,
        cohort_policy = input$cohort_policy,
        na_policy = input$na_policy,
        distance_space = input$distance_space,
        show_uncertainty = input$show_uncertainty,
        bootstrap_reps = input$bootstrap_reps,
        confidence = input$confidence,
        bootstrap_seed = input$bootstrap_seed,
        bootstrap_design = input$bootstrap_design,
        selected_axes = .trajectory_flatten_axes(selected_axes)
      )
    })

    shiny::observeEvent(analytical_settings(), {
      cancelled <- cancel_active_bootstrap(
        "The isolated bootstrap job was cancelled because trajectory settings changed."
      )
      if (cancelled || !is.null(analysis_result())) {
        analysis_result(NULL)
        analysis_source(NULL)
        status("Trajectory settings changed. Select Run / recompute to update the analysis.")
      }
    }, ignoreInit = TRUE, priority = 90)

    data_info <- shiny::reactive({
      object <- current_ena_obj()
      if (is.null(object) ||
          (!is.data.frame(object) &&
           (is.null(object$points) || !is.data.frame(object$points)))) {
        return(NULL)
      }
      points <- .trajectory_points(object)
      dimensions <- .trajectory_dimensions(object, points, raw_dimensions)
      metadata <- .trajectory_metadata_columns(object, points, dimensions)
      list(
        object = object,
        points = points,
        dimensions = dimensions,
        metadata = metadata,
        declared = .trajectory_declared_unit_vars(object),
        declared_time = .trajectory_declared_default(object, "time"),
        declared_id = .trajectory_declared_default(object, "id"),
        declared_group = .trajectory_declared_default(object, "group")
      )
    })

    id_coverage <- shiny::reactive({
      info <- data_info()
      if (is.null(info)) return(.trajectory_id_coverage(data.frame(), "", ""))
      .trajectory_id_coverage(
        info$points,
        .trajectory_or(input$time_var, ""),
        .trajectory_or(input$id_var, ""),
        .trajectory_or(input$group_var, "")
      )
    })

    output$id_coverage_status <- shiny::renderText({
      .trajectory_id_coverage_message(
        id_coverage(),
        .trajectory_or(input$id_var, ""),
        .trajectory_or(input$group_var, "")
      )
    })

    comparison_overlap <- shiny::reactive({
      info <- data_info()
      if (is.null(info)) return(.trajectory_comparison_overlap(
        data.frame(), "", "", "", "", ""
      ))
      .trajectory_comparison_overlap(
        info$points,
        .trajectory_or(input$group_var, ""),
        .trajectory_or(input$condition_a, ""),
        .trajectory_or(input$condition_b, ""),
        .trajectory_or(input$id_var, ""),
        .trajectory_or(input$time_var, "")
      )
    })

    output$comparison_overlap_status <- shiny::renderText({
      .trajectory_comparison_overlap_message(comparison_overlap())
    })

    bootstrap_cost <- shiny::reactive({
      info <- data_info()
      points <- if (is.null(info)) data.frame() else info$points
      dimensions <- if (is.null(info)) {
        character(0)
      } else if (identical(.trajectory_or(input$distance_space, "selected"), "full")) {
        info$dimensions
      } else {
        intersect(.trajectory_flatten_axes(selected_axes), info$dimensions)
      }
      .trajectory_bootstrap_cost(
        points,
        dimensions,
        input$bootstrap_reps,
        uncertainty = input$show_uncertainty,
        comparison = isTRUE(input$run_comparison) &&
          nzchar(.trajectory_or(input$group_var, ""))
      )
    })

    output$bootstrap_cost_status <- shiny::renderText({
      .trajectory_bootstrap_cost_message(bootstrap_cost())
    })

    shiny::observeEvent(data_info(), {
      info <- data_info()
      shiny::req(!is.null(info))
      metadata <- info$metadata
      shiny::req(length(metadata) >= 2L)

      old_time <- shiny::isolate(input$time_var)
      time_default <- if (!is.null(old_time) && old_time %in% metadata) {
        old_time
      } else {
        .trajectory_default_variable(
          metadata, c(info$declared_time, info$declared), "time"
        )
      }
      shiny::updateSelectInput(
        session, "time_var", choices = metadata, selected = time_default
      )
    }, ignoreNULL = FALSE)

    shiny::observeEvent(list(data_info(), input$time_var), {
      info <- data_info()
      shiny::req(!is.null(info), length(info$metadata) >= 2L)
      metadata <- info$metadata
      time_var <- input$time_var
      if (is.null(time_var) || !length(time_var) || !time_var %in% metadata) {
        time_var <- .trajectory_default_variable(
          metadata, c(info$declared_time, info$declared), "time"
        )
      }

      id_choices <- setdiff(metadata, time_var)
      old_id <- shiny::isolate(input$id_var)
      id_default <- if (!is.null(old_id) && old_id %in% id_choices) {
        old_id
      } else {
        .trajectory_default_variable(
          metadata, c(info$declared, info$declared_id), "id", exclude = time_var
        )
      }
      shiny::updateSelectInput(
        session,
        "id_var",
        choices = .trajectory_id_choices(info$points, id_choices, time_var),
        selected = id_default
      )
    }, ignoreNULL = FALSE)

    shiny::observeEvent(list(data_info(), input$time_var, input$id_var), {
      info <- data_info()
      shiny::req(!is.null(info))
      metadata <- info$metadata
      time_var <- .trajectory_or(input$time_var, character(0))
      id_var <- .trajectory_or(input$id_var, character(0))
      group_choices <- setdiff(metadata, c(time_var, id_var))
      old_group <- shiny::isolate(input$group_var)
      group_default <- if (!is.null(old_group) && old_group %in% group_choices) {
        old_group
      } else if (length(info$declared_group) &&
                 info$declared_group[[1L]] %in% group_choices) {
        info$declared_group[[1L]]
      } else {
        ""
      }
      shiny::updateSelectInput(
        session, "group_var",
        choices = c("None" = "", stats::setNames(group_choices, group_choices)),
        selected = group_default
      )
    }, ignoreNULL = FALSE)

    shiny::observe({
      info <- data_info()
      shiny::req(!is.null(info), length(info$dimensions) >= 2L)
      completed <- analysis_result()
      selected <- if (is.null(completed)) {
        .trajectory_flatten_axes(selected_axes)
      } else {
        completed$settings$dimensions
      }
      selected <- intersect(selected, info$dimensions)
      if (!length(selected)) selected <- head(info$dimensions, 3L)

      old_x <- shiny::isolate(input$axis_x)
      old_y <- shiny::isolate(input$axis_y)
      new_x <- if (!is.null(old_x) && old_x %in% selected) old_x else selected[[1]]
      new_y <- if (!is.null(old_y) && old_y %in% selected && old_y != new_x) {
        old_y
      } else if (length(selected) >= 2L) {
        selected[[2]]
      } else {
        selected[[1]]
      }
      shiny::updateSelectInput(session, "axis_x", choices = selected, selected = new_x)
      shiny::updateSelectInput(session, "axis_y", choices = selected, selected = new_y)
    })

    generate_order <- function() {
      info <- data_info()
      shiny::req(!is.null(info))
      time_var <- input$time_var
      shiny::req(!is.null(time_var), nzchar(time_var), time_var %in% names(info$points))
      order <- .trajectory_default_order(info$points[[time_var]])
      labels <- .trajectory_order_labels(order)
      shiny::updateTextAreaInput(
        session, "time_order", value = paste(labels, collapse = "\n")
      )
      shiny::updateSelectInput(
        session, "selected_time", choices = labels,
        selected = if (length(labels)) labels[[1]] else character(0)
      )
    }

    shiny::observeEvent(list(data_info(), input$time_var), {
      generate_order()
    }, ignoreNULL = TRUE)

    shiny::observeEvent(input$generate_order, {
      generate_order()
    }, ignoreInit = TRUE)

    shiny::observe({
      info <- data_info()
      shiny::req(!is.null(info))
      group_var <- .trajectory_or(input$group_var, "")
      values <- .trajectory_condition_values(info$points, group_var)
      old_a <- shiny::isolate(input$condition_a)
      old_b <- shiny::isolate(input$condition_b)
      selected_a <- if (!is.null(old_a) && old_a %in% values) {
        old_a
      } else if (length(values)) {
        values[[1]]
      } else {
        character(0)
      }
      selected_b <- if (!is.null(old_b) && old_b %in% values && old_b != selected_a) {
        old_b
      } else {
        remaining <- setdiff(values, selected_a)
        if (length(remaining)) remaining[[1]] else character(0)
      }
      shiny::updateSelectInput(
        session, "condition_a", choices = values, selected = selected_a
      )
      shiny::updateSelectInput(
        session, "condition_b", choices = values, selected = selected_b
      )
      old_overlay_group <- shiny::isolate(input$overlay_group)
      selected_overlay_group <- if (!is.null(old_overlay_group) &&
                                    old_overlay_group %in% values) {
        old_overlay_group
      } else {
        ""
      }
      shiny::updateSelectInput(
        session,
        "overlay_group",
        choices = c(
          "Overall across all trajectory groups" = "",
          stats::setNames(values, values)
        ),
        selected = selected_overlay_group
      )
    })

    shiny::observeEvent(input$run_trajectory, {
      cancel_active_bootstrap(
        "The isolated bootstrap job was cancelled by a newer analysis request."
      )
      run_generation <- bootstrap_state$generation
      cost_message <- .trajectory_bootstrap_cost_message(bootstrap_cost())
      status(paste("Running centroid trajectory analysis.", cost_message))
      analysis_result(NULL)
      analysis_source(NULL)
      progress <- shiny::Progress$new(session, min = 0, max = 1)
      progress_closed <- FALSE
      close_progress <- function() {
        if (!progress_closed) {
          progress_closed <<- TRUE
          try(progress$close(), silent = TRUE)
        }
        invisible(NULL)
      }
      progress$set(value = 0.02, message = "Trajectory analysis",
                   detail = "Validating selections")

      fail_analysis <- function(error) {
        if (!identical(bootstrap_state$generation, run_generation)) {
          return(invisible(NULL))
        }
        bootstrap_state$active <- NULL
        close_progress()
        status(paste("Trajectory analysis failed:", conditionMessage(error)))
        invisible(NULL)
      }

      tryCatch({
        info <- data_info()
        if (is.null(info)) stop("No ENA object is available.")
        object <- info$object
        points <- info$points
        full_dimensions <- info$dimensions

        if (!exists("compute_centroid_path", mode = "function", inherits = TRUE)) {
          stop("compute_centroid_path() is unavailable; source trajectory_analysis.R first.")
        }
        time_var <- .trajectory_or(input$time_var, "")
        id_var <- .trajectory_or(input$id_var, "")
        group_var <- .trajectory_or(input$group_var, "")
        condition_a <- .trajectory_or(input$condition_a, "")
        condition_b <- .trajectory_or(input$condition_b, "")
        show_uncertainty <- isTRUE(input$show_uncertainty)
        run_comparison <- isTRUE(input$run_comparison)
        paired_ids_confirmed <- isTRUE(input$confirm_paired_ids)
        current_time <- input$selected_time
        overlap <- comparison_overlap()
        if (!nzchar(time_var) || !time_var %in% names(points)) {
          stop("Select a valid time / order variable.")
        }
        if (!nzchar(id_var) || !id_var %in% names(points)) {
          stop("Select a valid repeated entity ID variable.")
        }
        if (identical(time_var, id_var)) {
          stop("Time and entity ID must be different variables.")
        }
        if (nzchar(group_var) && !group_var %in% names(points)) {
          stop("Select a valid group / condition variable.")
        }
        if (nzchar(group_var) && group_var %in% c(time_var, id_var)) {
          stop("Group / condition must differ from time and entity ID.")
        }
        coverage <- .trajectory_id_coverage(
          points, time_var, id_var, if (nzchar(group_var)) group_var else NULL
        )
        if (coverage$n_repeated_ids < 1L) {
          stop(.trajectory_id_coverage_message(coverage, id_var, group_var))
        }
        export_keys <- c(time_var, if (nzchar(group_var)) group_var else character())
        if (any(startsWith(export_keys, ".analysis_"))) {
          stop(
            "Time and group fields beginning with reserved `.analysis_` cannot be exported safely."
          )
        }

        dimensions <- .trajectory_flatten_axes(selected_axes)
        dimensions <- intersect(dimensions, full_dimensions)
        if (!length(dimensions)) dimensions <- head(full_dimensions, 3L)
        if (length(dimensions) < 2L) {
          stop("At least two numeric ENA dimensions are required.")
        }
        if (identical(input$view, "3d") &&
            length(unique(dimensions)) < 3L) {
          stop("The 3D view requires three distinct selected ENA dimensions.")
        }

        order <- .trajectory_parse_order(input$time_order, points[[time_var]])
        cohort_policy <- .trajectory_or(input$cohort_policy, "available")
        na_policy <- .trajectory_or(input$na_policy, "complete")
        distance_space <- .trajectory_or(input$distance_space, "selected")
        bootstrap_design <- match.arg(
          .trajectory_or(input$bootstrap_design, "auto"),
          c("auto", "cluster", "stratified")
        )
        raw_n_boot <- .trajectory_or(input$bootstrap_reps, 500L)
        if (!is.numeric(raw_n_boot) || length(raw_n_boot) != 1L ||
            is.na(raw_n_boot) || !is.finite(raw_n_boot) ||
            raw_n_boot != floor(raw_n_boot) ||
            raw_n_boot > .Machine$integer.max) {
          stop("Bootstrap reps must be one whole number.")
        }
        n_boot <- as.integer(raw_n_boot)
        conf_level <- as.numeric(.trajectory_or(input$confidence, 0.95))
        seed <- as.integer(.trajectory_or(input$bootstrap_seed, 2026L))
        if (is.na(n_boot) || n_boot < 2L) stop("Bootstrap reps must be at least 2.")
        if (n_boot > .trajectory_bootstrap_max_reps()) {
          stop(
            "Bootstrap reps must be at most ",
            .trajectory_bootstrap_max_reps(),
            " on the hosted application."
          )
        }
        if (!is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
          stop("Confidence must be between 0 and 1.")
        }
        if (is.na(seed) || seed < 0L) stop("Bootstrap seed must be non-negative.")
        bootstrap_requested <- show_uncertainty ||
          (run_comparison && nzchar(group_var))
        if (bootstrap_requested) {
          if (n_boot < 200L) {
            stop(
              "Hosted confidence intervals require at least 200 bootstrap repetitions."
            )
          }
          required_reps <- .trajectory_bootstrap_required_valid(
            n_boot, conf_level
          )
          tail_required <- ceiling(10 / (1 - conf_level))
          if (n_boot < required_reps) {
            stop(
              "Bootstrap reps are insufficient for this confidence level: need at least ",
              tail_required,
              " so each interval tail has five expected replicates."
            )
          }
        }
        if (run_comparison && nzchar(group_var) && !paired_ids_confirmed) {
          stop(
            paste0(
              "Confirm that the same raw ID across the two conditions denotes ",
              "the same physical entity before running a paired comparison."
            )
          )
        }
        job_dimensions <- if (identical(distance_space, "full")) {
          full_dimensions
        } else {
          dimensions
        }
        job_cost <- .trajectory_bootstrap_cost(
          points, job_dimensions, n_boot,
          uncertainty = show_uncertainty,
          comparison = run_comparison && nzchar(group_var)
        )
        .trajectory_validate_bootstrap_cost(job_cost)

        common_arguments <- list(
          points = points,
          time_var = time_var,
          id_var = id_var,
          group_vars = if (nzchar(group_var)) group_var else NULL,
          dimensions = dimensions,
          order = order,
          cohort_policy = cohort_policy,
          weights = NULL,
          na_policy = na_policy,
          distance_space = distance_space,
          full_dimensions = full_dimensions
        )
        progress$set(value = 0.10, detail = "Computing ordered centroid path")
        path <- do.call(compute_centroid_path, common_arguments)
        progress$set(value = 0.28, detail = "Centroid path complete")

        module_diagnostics <- .trajectory_module_diagnostic(
          "none", "", severity = "info"
        )[0, , drop = FALSE]
        time_values <- points[[time_var]]
        if (is.character(time_values) ||
            (is.factor(time_values) && !is.ordered(time_values))) {
          module_diagnostics <- .trajectory_bind_diagnostics(
            module_diagnostics,
            .trajectory_module_diagnostic(
              "time_order_requires_review",
              paste0(
                "The time variable is character or an unordered factor. ",
                "Verify that the explicit order shown in the control is substantively correct."
              )
            )
          )
        }

        uncertainty_arguments <- if (show_uncertainty) {
          c(common_arguments, list(
            n_boot = n_boot,
            conf_level = conf_level,
            seed = seed,
            bootstrap_design = bootstrap_design
          ))
        } else {
          NULL
        }
        comparison_arguments <- NULL
        if (nzchar(group_var) && run_comparison) {
          if (nzchar(condition_a) && nzchar(condition_b) &&
              !identical(condition_a, condition_b)) {
            points_a <- points[
              !is.na(points[[group_var]]) &
                as.character(points[[group_var]]) == condition_a,
              , drop = FALSE
            ]
            points_b <- points[
              !is.na(points[[group_var]]) &
                as.character(points[[group_var]]) == condition_b,
              , drop = FALSE
            ]
            comparison_arguments <- list(
              points_a = points_a,
              points_b = points_b,
              time_var = time_var,
              id_var = id_var,
              group_vars = NULL,
              dimensions = dimensions,
              order = order,
              cohort_policy = cohort_policy,
              weights_a = NULL,
              weights_b = NULL,
              na_policy = na_policy,
              distance_space = distance_space,
              full_dimensions = full_dimensions,
              n_boot = n_boot,
              conf_level = conf_level,
              seed = seed,
              labels = c(condition_a, condition_b),
              pair_weight_policy = "require_equal",
              bootstrap_design = bootstrap_design
            )
          } else {
            module_diagnostics <- .trajectory_bind_diagnostics(
              module_diagnostics,
              .trajectory_module_diagnostic(
                "comparison_levels_unavailable",
                "The grouped paths were computed, but comparison needs two distinct levels."
              )
            )
          }
        }

        complete_analysis <- function(job_result) {
          uncertainty <- job_result$uncertainty
          comparison <- job_result$comparison
          path_diagnostics <- .trajectory_module_diagnostics_from(path, "path")
          uncertainty_diagnostics <- .trajectory_module_diagnostics_from(
            uncertainty, "bootstrap"
          )
          uncertainty_diagnostics <- .trajectory_remove_inherited_diagnostics(
            uncertainty_diagnostics, path_diagnostics
          )
          diagnostics <- .trajectory_bind_diagnostics(
            path_diagnostics,
            uncertainty_diagnostics,
            .trajectory_module_diagnostics_from(comparison, "comparison"),
            module_diagnostics
          )
          progress$set(value = 0.93, detail = "Recording reproducibility metadata")
          metadata <- c(list(
            time_var = time_var,
            id_var = id_var,
            group_var = if (nzchar(group_var)) group_var else NA_character_,
            condition_a = if (nzchar(group_var)) condition_a else NA_character_,
            condition_b = if (nzchar(group_var)) condition_b else NA_character_,
            dimensions = dimensions,
            full_dimensions = full_dimensions,
            distance_space = distance_space,
            cohort_policy = cohort_policy,
            na_policy = na_policy,
            time_order = .trajectory_order_labels(order),
            bootstrap_enabled = show_uncertainty,
            comparison_requested = run_comparison && nzchar(group_var),
            comparison_enabled = !is.null(comparison),
            paired_id_identity_confirmed = paired_ids_confirmed,
            comparison_overlap_ids = overlap$n_overlap_ids,
            comparison_matched_id_times = overlap$n_matched_id_times,
            bootstrap_design_requested_ui = bootstrap_design,
            bootstrap_reps = if (show_uncertainty || !is.null(comparison)) {
              n_boot
            } else {
              NA_integer_
            },
            confidence = if (show_uncertainty || !is.null(comparison)) {
              conf_level
            } else {
              NA_real_
            },
            seed = if (show_uncertainty || !is.null(comparison)) seed else NA_integer_,
            raw_point_rows = nrow(points),
            id_profiles = coverage$n_ids,
            repeated_id_profiles = coverage$n_repeated_ids,
            repeated_id_row_coverage = coverage$repeated_row_fraction,
            duplicate_id_time_rows = coverage$n_duplicate_id_time_rows,
            csv_text_escape = paste0(
              "Character cells and column headers beginning with =, +, -, or @ ",
              "are prefixed with an apostrophe in CSV files to prevent ",
              "spreadsheet formula execution."
            ),
            generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
          ),
          .trajectory_provenance_metadata(object, full_dimensions),
          .trajectory_bootstrap_metadata(uncertainty, comparison))

          result <- list(
            path = path,
            bootstrap = uncertainty,
            comparison = comparison,
            diagnostics = diagnostics,
            metadata = metadata,
            settings = list(
              dimensions = dimensions,
              full_dimensions = full_dimensions,
              group_var = if (nzchar(group_var)) group_var else NULL,
              time_var = time_var,
              id_var = id_var,
              distance_space = distance_space,
              cohort_policy = cohort_policy,
              na_policy = na_policy,
              bootstrap_design_requested = bootstrap_design,
              paired_id_identity_confirmed = paired_ids_confirmed
            )
          )
          analysis_source(object)
          analysis_result(result)
          progress$set(value = 1, detail = "Analysis and provenance complete")

          time_labels <- .trajectory_order_labels(order)
          shiny::updateSelectInput(
            session, "selected_time", choices = time_labels,
            selected = if (!is.null(current_time) && current_time %in% time_labels) {
              current_time
            } else {
              time_labels[[1L]]
            }
          )
          trajectory_count <- if (nzchar(group_var)) {
            length(unique(as.character(path[[group_var]])))
          } else {
            1L
          }
          status(paste0(
            "Completed ", nrow(path), " centroid slices across ", trajectory_count,
            if (trajectory_count == 1L) " trajectory" else " trajectories",
            "; distances use the ", distance_space, " ENA space."
          ))
          invisible(result)
        }

        if (is.null(uncertainty_arguments) && is.null(comparison_arguments)) {
          complete_analysis(list(uncertainty = NULL, comparison = NULL))
          close_progress()
          return(invisible(NULL))
        }

        progress$set(
          value = 0.32,
          detail = paste(
            "Running isolated bootstrap worker with an executable deadline;",
            cost_message
          )
        )
        worker <- .trajectory_start_bootstrap_job(
          uncertainty_arguments = uncertainty_arguments,
          comparison_arguments = comparison_arguments,
          timeout_seconds = .trajectory_bootstrap_max_seconds()
        )
        bootstrap_state$active <- list(
          worker = worker,
          close_progress = close_progress,
          generation = run_generation
        )
        completed <- promises::then(worker$promise, function(job_result) {
          if (!identical(bootstrap_state$generation, run_generation)) {
            return(invisible(NULL))
          }
          bootstrap_state$active <- NULL
          on.exit(close_progress(), add = TRUE)
          complete_analysis(job_result)
        })
        promises::catch(completed, function(error) {
          fail_analysis(error)
          NULL
        })
        invisible(NULL)
      }, error = fail_analysis)
    }, ignoreInit = TRUE)

    output$status <- shiny::renderText(status())

    output$warnings <- shiny::renderUI({
      result <- analysis_result()
      if (is.null(result) || !nrow(result$diagnostics)) return(NULL)

      diagnostics <- result$diagnostics
      messages <- vapply(seq_len(nrow(diagnostics)), function(index) {
        context <- character(0)
        if ("group" %in% names(diagnostics) &&
            !is.na(diagnostics$group[[index]]) &&
            nzchar(as.character(diagnostics$group[[index]])) &&
            diagnostics$group[[index]] != "all") {
          context <- c(context, paste0("group ", diagnostics$group[[index]]))
        }
        if ("time_order" %in% names(diagnostics) &&
            !is.na(diagnostics$time_order[[index]])) {
          context <- c(context, paste0("time order ", diagnostics$time_order[[index]]))
        }
        if ("count" %in% names(diagnostics) &&
            !is.na(diagnostics$count[[index]]) && diagnostics$count[[index]] > 1L) {
          context <- c(context, paste0("count ", diagnostics$count[[index]]))
        }
        suffix <- if (length(context)) paste0(" (", paste(context, collapse = ", "), ")") else ""
        paste0(
          "[", toupper(diagnostics$severity[[index]]), "] ",
          diagnostics$message[[index]], suffix
        )
      }, character(1))
      messages <- unique(messages[!is.na(messages) & nzchar(messages)])
      shiny::tags$div(
        class = "trajectory-analysis-warnings alert alert-warning",
        style = "margin-top: 0.5rem;",
        shiny::tags$strong("Trajectory diagnostics"),
        shiny::tags$ul(lapply(messages, shiny::tags$li))
      )
    })

    output$downloads <- shiny::renderUI({
      .trajectory_download_controls(analysis_result(), session$ns)
    })

    output$node_legend <- shiny::renderUI({
      result <- analysis_result()
      if (is.null(result)) return(NULL)
      .trajectory_node_legend_ui(result$path)
    })
    shiny::outputOptions(output, "node_legend", suspendWhenHidden = FALSE)

    overlay_data <- shiny::reactive({
      result <- analysis_result()
      if (is.null(result) || !isTRUE(input$network_overlay)) {
        return(list(code_nodes = NULL, network_edges = NULL, message = "Overlay off."))
      }

      view <- .trajectory_or(input$view, "3d")
      dimensions <- if (identical(view, "2d")) {
        c(input$axis_x, input$axis_y)
      } else {
        head(result$settings$dimensions, 3L)
      }
      dimensions <- dimensions[!is.na(dimensions) & nzchar(dimensions)]
      .trajectory_network_overlay(
        analysis_source(), dimensions, result$settings$time_var,
        .trajectory_or(input$selected_time, ""),
        group_var = result$settings$group_var,
        selected_group = .trajectory_or(input$overlay_group, "")
      )
    })

    output$overlay_status <- shiny::renderText(overlay_data()$message)

    output$trajectory_plot <- plotly::renderPlotly({
      result <- analysis_result()
      shiny::validate(shiny::need(
        !is.null(result), "Run the trajectory analysis to create a plot."
      ))
      shiny::validate(shiny::need(
        exists("plot_centroid_trajectory", mode = "function", inherits = TRUE),
        "plot_centroid_trajectory() is unavailable; source trajectory_plot.R first."
      ))

      view <- .trajectory_or(input$view, "3d")
      dimensions <- if (identical(view, "2d")) {
        c(input$axis_x, input$axis_y)
      } else {
        head(result$settings$dimensions, 3L)
      }
      dimensions <- dimensions[!is.na(dimensions) & nzchar(dimensions)]
      required_count <- if (identical(view, "2d")) 2L else 3L
      shiny::validate(shiny::need(
        length(unique(dimensions)) == required_count,
        paste0("Select ", required_count, " distinct axes for the ", toupper(view), " view.")
      ))

      plot_data <- if (isTRUE(input$show_uncertainty) && !is.null(result$bootstrap)) {
        result$bootstrap
      } else {
        result$path
      }
      overlay <- overlay_data()
      plot_centroid_trajectory(
        path = plot_data,
        dimensions = dimensions,
        view = view,
        group_cols = result$settings$group_var,
        colors = .trajectory_colors(group_colors),
        camera = .trajectory_resolve_value(camera),
        display_scale = 1,
        code_nodes = overlay$code_nodes,
        network_edges = overlay$network_edges,
        selected_time = input$selected_time,
        show_warnings = TRUE,
        show_direction = !identical(input$show_direction, FALSE)
      )
    })

    output$download_bundle <- shiny::downloadHandler(
      filename = function() {
        paste0("ena3d-trajectory-analysis-", format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      contentType = "application/zip",
      content = function(file) {
        result <- analysis_result()
        shiny::req(!is.null(result))
        .trajectory_write_bundle(result, file)
      }
    )

    output$download_path <- shiny::downloadHandler(
      filename = function() {
        paste0("centroid-path-", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        result <- analysis_result()
        shiny::req(!is.null(result))
        .trajectory_write_csv(
          .trajectory_export_metadata(result$path, result$metadata), file
        )
      }
    )

    output$download_uncertainty <- shiny::downloadHandler(
      filename = function() {
        paste0("centroid-path-bootstrap-", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        result <- analysis_result()
        shiny::req(!is.null(result), !is.null(result$bootstrap))
        .trajectory_write_csv(
          .trajectory_export_metadata(result$bootstrap, result$metadata), file
        )
      }
    )

    output$download_comparison <- shiny::downloadHandler(
      filename = function() {
        result <- analysis_result()
        levels <- c(result$metadata$condition_a, result$metadata$condition_b)
        levels <- .trajectory_safe_file_part(paste(levels, collapse = "-vs-"))
        paste0("centroid-path-comparison-", levels, "-", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        result <- analysis_result()
        shiny::req(!is.null(result), !is.null(result$comparison))
        .trajectory_write_csv(
          .trajectory_export_metadata(result$comparison, result$metadata), file
        )
      }
    )

    output$download_metadata <- shiny::downloadHandler(
      filename = function() {
        paste0("centroid-path-metadata-", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        result <- analysis_result()
        shiny::req(!is.null(result))
        metadata <- .trajectory_metadata_table(result$metadata)
        if (nrow(result$diagnostics)) {
          diagnostics <- data.frame(
            field = paste0("diagnostic_", seq_len(nrow(result$diagnostics))),
            value = paste(
              result$diagnostics$severity,
              result$diagnostics$code,
              result$diagnostics$message,
              sep = ": "
            ),
            stringsAsFactors = FALSE
          )
          metadata <- rbind(metadata, diagnostics)
        }
        .trajectory_write_csv(metadata, file)
      }
    )

    # A stable, explicit test surface. No analytical result changes until the
    # Run / recompute button is pressed again.
    list(
      result = shiny::reactive(analysis_result()),
      path = shiny::reactive({
        result <- analysis_result()
        if (is.null(result)) NULL else result$path
      }),
      bootstrap = shiny::reactive({
        result <- analysis_result()
        if (is.null(result)) NULL else result$bootstrap
      }),
      comparison = shiny::reactive({
        result <- analysis_result()
        if (is.null(result)) NULL else result$comparison
      }),
      diagnostics = shiny::reactive({
        result <- analysis_result()
        if (is.null(result)) NULL else result$diagnostics
      }),
      metadata = shiny::reactive({
        result <- analysis_result()
        if (is.null(result)) NULL else result$metadata
      }),
      status = shiny::reactive(status())
    )
  })
}


# Backward-friendly aliases for applications that name modules after their files.
app_module_trajectory <- trajectory_server
ena_trajectory_server <- trajectory_server
