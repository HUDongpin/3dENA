# Plotting helpers for centroid trajectories.
#
# This file deliberately contains no Shiny code.  The functions accept the tidy
# output of compute_centroid_path() and treat it as immutable analytical data.
# Camera, projection, and display-scale arguments affect only Plotly rendering.

.trajectory_require_plotly <- function() {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop(
      paste0(
        "Centroid trajectory plotting requires the optional 'plotly' package. ",
        "Install it with install.packages('plotly')."
      ),
      call. = FALSE
    )
  }
}

.trajectory_plot_font <- function(size = 14L, color = "#25282d") {
  list(
    family = paste(
      "Inter, ui-sans-serif, -apple-system, BlinkMacSystemFont,",
      "'Segoe UI', sans-serif"
    ),
    size = as.numeric(size),
    color = as.character(color)
  )
}

.trajectory_hover_label <- function() {
  list(
    bgcolor = "#FFFFFF",
    bordercolor = "#526777",
    align = "left",
    font = .trajectory_plot_font(14L, "#102A43")
  )
}

.trajectory_plot_axis <- function(title) {
  list(
    title = list(
      text = as.character(title),
      font = .trajectory_plot_font(16L)
    ),
    tickfont = .trajectory_plot_font(14L)
  )
}

.trajectory_validate_path <- function(path) {
  if (!is.data.frame(path)) {
    stop("`path` must be a data frame returned by compute_centroid_path().", call. = FALSE)
  }
  required <- c("time_value", "time_order")
  missing <- setdiff(required, names(path))
  if (length(missing) > 0L) {
    stop(
      sprintf("`path` is missing required column(s): %s.", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
  if (nrow(path) < 1L) {
    stop("`path` must contain at least one trajectory row.", call. = FALSE)
  }
  invisible(path)
}

.trajectory_spec <- function(path) {
  spec <- attr(path, "trajectory_spec", exact = TRUE)
  if (is.null(spec)) list() else spec
}

.trajectory_group_columns <- function(path, group_cols = NULL) {
  spec <- .trajectory_spec(path)

  if (is.null(group_cols)) {
    group_cols <- spec$group_vars
    if (is.null(group_cols)) {
      group_cols <- attr(path, "group_vars", exact = TRUE)
    }
  }

  if (!is.null(group_cols)) {
    group_cols <- as.character(group_cols)
    missing <- setdiff(group_cols, names(path))
    if (length(missing) > 0L) {
      stop(
        sprintf("Unknown `group_cols`: %s.", paste(missing, collapse = ", ")),
        call. = FALSE
      )
    }
    return(unique(group_cols))
  }

  # Fallback inference is intentionally conservative.  In normal use the core
  # function records group_vars in trajectory_spec, but this makes the plotting
  # layer usable with manually constructed contract-conforming data frames too.
  known <- c(
    "time_value", "time_order", "n_total", "n_used", "n_missing",
    "n_excluded", "n_duplicate_rows", "dx", "dy", "dz",
    "step_distance", "elapsed_interval", "speed", "cumulative_distance",
    "warning", "warnings", "trajectory_warning"
  )
  time_var <- spec$time_var
  if (!is.null(time_var)) known <- c(known, as.character(time_var))

  # Public path APIs emit these analytical sample-count fields. Exclude their
  # exact contract names after CSV round trips without reserving every `n_*`
  # name, since a legitimate grouping variable may itself be called `n_region`.
  count_pattern <- paste0(
    "^n_(rows_total|total|used|missing|excluded|cohort_excluded|zero_weight|",
    "rows_missing_key|rows_missing|distance_incomplete|",
    "rows_distance_incomplete|duplicate_rows)$|",
    "^n_(a|b)_(total|valid|rows_missing_key)$|",
    "^n_(matched|unmatched_a|unmatched_b|dropped_a|dropped_b)$"
  )
  analytical_pattern <- paste0(
    "^(centroid_|delta_|displacement_|warn_|warning_)|",
    "(_lower|_upper|_boot_n|_warning)$|", count_pattern, "|",
    "^is_unordered$|^unordered$|",
    "^duplicate_|^cohort_changed$|^changing_cohort$|^missing_period$|",
    "^insufficient_sample"
  )
  candidates <- setdiff(names(path), known)
  candidates <- candidates[!grepl(analytical_pattern, candidates)]

  # A preserved original time column is often identical to time_value.  Do not
  # accidentally infer that column as a trajectory group when spec is absent.
  if (length(candidates) > 0L) {
    same_as_time <- vapply(candidates, function(column) {
      identical(as.character(path[[column]]), as.character(path$time_value))
    }, logical(1))
    candidates <- candidates[!same_as_time]
  }
  candidates
}

.trajectory_dimension_columns <- function(path, dimensions = NULL, required = NULL) {
  all_centroids <- grep("^centroid_", names(path), value = TRUE)
  if (length(all_centroids) == 0L) {
    stop("`path` has no `centroid_<dimension>` coordinate columns.", call. = FALSE)
  }

  if (is.null(dimensions)) {
    # A live analytical path declares its dimensions, which also lets legal
    # names such as `x_boot_n` or `x_lower` remain usable.  Attribute-free
    # frames (notably CSV round trips) are ambiguous, so their automatic
    # inference conservatively removes confidence/bootstrap summary columns.
    declared <- .trajectory_spec(path)$dimensions
    declared <- if (is.null(declared)) character() else {
      paste0("centroid_", as.character(declared))
    }
    declared <- declared[declared %in% all_centroids]
    available <- if (length(declared)) {
      unique(declared)
    } else {
      all_centroids[!grepl("_(lower|upper|boot_n)$", all_centroids)]
    }
    if (length(available) == 0L) {
      stop(
        paste0(
          "No centroid coordinate columns can be inferred automatically; ",
          "supply `dimensions` explicitly."
        ),
        call. = FALSE
      )
    }
    if (is.null(required)) {
      dimensions <- available[seq_len(min(3L, length(available)))]
    } else {
      if (length(available) < required) {
        stop(sprintf("At least %d centroid dimensions are required.", required), call. = FALSE)
      }
      dimensions <- available[seq_len(required)]
    }
  } else {
    # An explicit selection is authoritative.  In particular, do not reserve
    # suffixes that are only heuristics for attribute-free automatic inference.
    available <- all_centroids
    dimensions <- as.character(dimensions)
    dimensions <- vapply(dimensions, function(dimension) {
      if (dimension %in% available) {
        dimension
      } else {
        candidate <- paste0("centroid_", dimension)
        if (candidate %in% available) candidate else NA_character_
      }
    }, character(1))
    if (anyNA(dimensions)) {
      requested <- as.character(dimensions[is.na(dimensions)])
      stop(
        sprintf(
          "Unknown trajectory dimension(s). Available dimensions are: %s.",
          paste(sub("^centroid_", "", available), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    if (anyDuplicated(dimensions)) {
      stop("`dimensions` must select distinct centroid columns.", call. = FALSE)
    }
    if (!is.null(required)) {
      if (length(dimensions) < required) {
        stop(sprintf("At least %d trajectory dimensions are required.", required), call. = FALSE)
      }
      dimensions <- dimensions[seq_len(required)]
    }
  }
  dimensions
}

.trajectory_group_info <- function(path, group_cols) {
  n <- nrow(path)
  if (length(group_cols) == 0L) {
    return(data.frame(
      .trajectory_key = rep("__trajectory__", n),
      .trajectory_label = rep("Trajectory", n),
      stringsAsFactors = FALSE
    ))
  }

  values <- lapply(group_cols, function(column) {
    value <- as.character(path[[column]])
    value[is.na(value)] <- "<NA>"
    value
  })
  names(values) <- group_cols

  key <- vapply(seq_len(n), function(i) {
    pieces <- vapply(group_cols, function(column) {
      value <- values[[column]][i]
      paste0(nchar(column, type = "bytes"), ":", column, "=",
             nchar(value, type = "bytes"), ":", value)
    }, character(1))
    paste(pieces, collapse = "|")
  }, character(1))

  if (length(group_cols) == 1L) {
    label <- values[[1L]]
  } else {
    label <- vapply(seq_len(n), function(i) {
      paste(
        vapply(group_cols, function(column) {
          paste0(column, "=", values[[column]][i])
        }, character(1)),
        collapse = " · "
      )
    }, character(1))
  }

  data.frame(
    .trajectory_key = key,
    .trajectory_label = label,
    stringsAsFactors = FALSE
  )
}

.trajectory_hash <- function(value) {
  # A small, dependency-free polynomial hash.  Every multiplication stays well
  # below 2^53, making the result stable across supported R platforms.
  code <- utf8ToInt(enc2utf8(as.character(value)))
  hash <- 104729
  modulus <- 2147483647
  for (item in code) hash <- (hash * 131 + item) %% modulus
  hash
}

.trajectory_default_color <- function(key) {
  hash <- .trajectory_hash(key)
  hue <- hash %% 360
  chroma <- 58 + ((hash %/% 360) %% 3) * 6
  luminance <- 50 + ((hash %/% (360 * 3)) %% 3) * 5
  grDevices::hcl(h = hue, c = chroma, l = luminance, fixup = TRUE)
}

.trajectory_validate_colors <- function(colors) {
  invalid <- vapply(colors, function(color) {
    inherits(try(grDevices::col2rgb(color), silent = TRUE), "try-error")
  }, logical(1))
  if (any(invalid)) {
    stop(
      sprintf("Invalid trajectory color(s): %s.", paste(unique(colors[invalid]), collapse = ", ")),
      call. = FALSE
    )
  }
  invisible(colors)
}

#' Build a deterministic color map for trajectory groups.
#'
#' `groups` may be a vector of group labels or a centroid-path data frame.  For
#' data frames, `group_cols` controls the grouping and map names are stable,
#' collision-resistant internal keys; readable labels are available from the
#' `labels` attribute.  Default colors depend only on each key, not row order.
trajectory_color_map <- function(groups, group_cols = NULL, colors = NULL) {
  if (is.data.frame(groups)) {
    .trajectory_validate_path(groups)
    group_cols <- .trajectory_group_columns(groups, group_cols)
    info <- .trajectory_group_info(groups, group_cols)
    pair <- unique(info)
    pair <- pair[order(pair$.trajectory_key, method = "radix"), , drop = FALSE]
    keys <- pair$.trajectory_key
    labels <- pair$.trajectory_label
  } else {
    labels <- as.character(groups)
    labels[is.na(labels)] <- "<NA>"
    keys <- sort(unique(labels), method = "radix")
    labels <- keys
  }

  if (length(keys) == 0L) return(stats::setNames(character(), character()))
  result <- vapply(keys, .trajectory_default_color, character(1))

  if (!is.null(colors)) {
    color_names <- names(colors)
    colors <- as.character(colors)
    names(colors) <- color_names
    if (length(colors) == 0L) {
      stop("`colors` must contain at least one color.", call. = FALSE)
    }
    if (!is.null(names(colors)) && any(nzchar(names(colors)))) {
      for (i in seq_along(keys)) {
        match_at <- match(keys[i], names(colors))
        if (is.na(match_at)) match_at <- match(labels[i], names(colors))
        if (!is.na(match_at)) result[i] <- colors[match_at]
      }
    } else {
      result <- rep(colors, length.out = length(keys))
    }
  }

  .trajectory_validate_colors(result)
  names(result) <- keys
  attr(result, "labels") <- stats::setNames(labels, keys)
  result
}

.trajectory_node_keys <- function(time_order, time_value) {
  order_numeric <- suppressWarnings(as.numeric(as.character(time_order)))
  value_text <- vapply(seq_along(time_value), function(index) {
    .trajectory_format_value(time_value[index])
  }, character(1))
  keys <- character(length(order_numeric))
  ordered <- is.finite(order_numeric)
  if (any(ordered)) {
    keys[ordered] <- paste0(
      "order:",
      format(
        signif(order_numeric[ordered], 15L),
        scientific = FALSE,
        trim = TRUE
      )
    )
  }
  if (any(!ordered)) {
    keys[!ordered] <- vapply(value_text[!ordered], function(value) {
      paste0("unordered:", nchar(value, type = "bytes"), ":", value)
    }, character(1))
  }
  keys
}

.trajectory_ordered_node_colors <- function(count) {
  if (count < 1L) return(character())
  # Use only the middle-bright Viridis range. This preserves perceptual time
  # ordering without introducing the near-black purple nodes that obscure
  # Plotly interaction feedback.
  palette_size <- 256L
  palette <- grDevices::hcl.colors(palette_size, palette = "viridis")
  positions <- if (count == 1L) {
    0.65
  } else {
    seq(0.38, 0.92, length.out = count)
  }
  palette[round(positions * (palette_size - 1L)) + 1L]
}

#' Return the display-only color key for ordered trajectory centroid nodes.
#'
#' Fill color encodes the global `time_order` domain shared by all trajectory
#' groups. The original time value remains the readable node name. Unordered
#' manual rows are retained as neutral entries rather than being assigned a
#' misleading position on the ordered color scale.
trajectory_node_legend_data <- function(path) {
  .trajectory_validate_path(path)
  order_numeric <- suppressWarnings(as.numeric(as.character(path$time_order)))
  value_text <- vapply(seq_len(nrow(path)), function(index) {
    .trajectory_format_value(path$time_value[index])
  }, character(1))
  keys <- .trajectory_node_keys(path$time_order, path$time_value)
  ordered_values <- sort(unique(order_numeric[is.finite(order_numeric)]))
  ordered_colors <- .trajectory_ordered_node_colors(length(ordered_values))

  rows <- lapply(seq_along(ordered_values), function(index) {
    order_value <- ordered_values[index]
    selected <- is.finite(order_numeric) & order_numeric == order_value
    labels <- sort(unique(value_text[selected]), method = "radix")
    labels <- labels[!is.na(labels) & nzchar(labels)]
    if (!length(labels)) labels <- "NA"
    data.frame(
      node_key = unique(keys[selected])[1L],
      node_label = paste0(
        "Order ", .trajectory_format_value(order_value),
        " \u00b7 ", paste(labels, collapse = " / ")
      ),
      node_color = ordered_colors[index],
      time_order = order_value,
      time_value = paste(labels, collapse = " / "),
      is_ordered = TRUE,
      stringsAsFactors = FALSE
    )
  })

  unordered_keys <- sort(unique(keys[!is.finite(order_numeric)]), method = "radix")
  if (length(unordered_keys)) {
    rows <- c(rows, lapply(unordered_keys, function(key) {
      selected <- keys == key
      labels <- sort(unique(value_text[selected]), method = "radix")
      labels <- labels[!is.na(labels) & nzchar(labels)]
      if (!length(labels)) labels <- "NA"
      data.frame(
        node_key = key,
        node_label = paste0("Unordered \u00b7 ", paste(labels, collapse = " / ")),
        node_color = "#9AA0A6",
        time_order = NA_real_,
        time_value = paste(labels, collapse = " / "),
        is_ordered = FALSE,
        stringsAsFactors = FALSE
      )
    }))
  }

  output <- if (length(rows)) {
    do.call(rbind, rows)
  } else {
    data.frame(
      node_key = character(), node_label = character(),
      node_color = character(), time_order = numeric(),
      time_value = character(), is_ordered = logical(),
      stringsAsFactors = FALSE
    )
  }
  rownames(output) <- NULL
  spec <- .trajectory_spec(path)
  time_variable <- if (!is.null(spec$time_var) && length(spec$time_var) &&
      !is.na(spec$time_var[1L]) && nzchar(as.character(spec$time_var[1L]))) {
    .trajectory_pretty_name(as.character(spec$time_var[1L]))
  } else {
    "Time"
  }
  attr(output, "time_variable") <- time_variable
  output
}

.trajectory_warning_columns <- function(path) {
  exact <- c("warning", "warnings", "trajectory_warning")
  patterned <- names(path)[grepl(
    "^(warn_|warning_|is_unordered$|unordered$|duplicate_|cohort_changed$|changing_cohort$|missing_period$|insufficient_sample)",
    names(path)
  )]
  unique(c(intersect(exact, names(path)), patterned))
}

.trajectory_pretty_name <- function(value) {
  value <- gsub("^(warn_|warning_|is_)", "", value)
  value <- gsub("_", " ", value, fixed = TRUE)
  paste0(toupper(substring(value, 1L, 1L)), substring(value, 2L))
}

.trajectory_diagnostics <- function(path) {
  diagnostics <- attr(path, "trajectory_warnings", exact = TRUE)
  if (is.null(diagnostics)) return(data.frame(message = character(), stringsAsFactors = FALSE))
  if (is.character(diagnostics)) {
    return(data.frame(message = diagnostics, stringsAsFactors = FALSE))
  }
  if (is.data.frame(diagnostics)) {
    if (!"message" %in% names(diagnostics)) {
      diagnostics$message <- if ("code" %in% names(diagnostics)) {
        .trajectory_pretty_name(as.character(diagnostics$code))
      } else {
        "Trajectory warning"
      }
    }
    return(diagnostics)
  }
  if (is.list(diagnostics)) {
    messages <- unlist(diagnostics, recursive = TRUE, use.names = FALSE)
    return(data.frame(message = as.character(messages), stringsAsFactors = FALSE))
  }
  data.frame(message = as.character(diagnostics), stringsAsFactors = FALSE)
}

.trajectory_append_warning <- function(parts, rows, message) {
  rows <- which(rows %in% TRUE)
  if (length(rows) == 0L || !nzchar(message)) return(parts)
  for (row in rows) parts[[row]] <- c(parts[[row]], message)
  parts
}

.trajectory_row_warnings <- function(path, trace_data, group_cols) {
  parts <- rep(list(character()), nrow(trace_data))

  for (column in .trajectory_warning_columns(trace_data)) {
    value <- trace_data[[column]]
    if (is.logical(value)) {
      parts <- .trajectory_append_warning(parts, !is.na(value) & value, .trajectory_pretty_name(column))
    } else if (is.numeric(value)) {
      rows <- !is.na(value) & value > 0
      for (row in which(rows)) {
        parts[[row]] <- c(
          parts[[row]],
          paste0(.trajectory_pretty_name(column), ": ", format(value[row], trim = TRUE))
        )
      }
    } else {
      value <- as.character(value)
      for (row in which(!is.na(value) & nzchar(value))) parts[[row]] <- c(parts[[row]], value[row])
    }
  }

  if ("n_duplicate_rows" %in% names(trace_data)) {
    value <- trace_data$n_duplicate_rows
    for (row in which(!is.na(value) & value > 0)) {
      parts[[row]] <- c(parts[[row]], paste0("Duplicate entity/time rows: ", value[row]))
    }
  }
  if ("n_used" %in% names(trace_data)) {
    value <- trace_data$n_used
    parts <- .trajectory_append_warning(parts, !is.na(value) & value < 2, "Insufficient sample size")
  }
  parts <- .trajectory_append_warning(parts, is.na(trace_data$time_order), "Unordered time value")

  diagnostics <- .trajectory_diagnostics(path)
  if (nrow(diagnostics) > 0L) {
    for (i in seq_len(nrow(diagnostics))) {
      matches <- rep(TRUE, nrow(trace_data))
      scoped <- FALSE

      common_groups <- intersect(group_cols, names(diagnostics))
      for (column in common_groups) {
        diagnostic_value <- diagnostics[[column]][i]
        if (!is.na(diagnostic_value)) {
          matches <- matches & as.character(trace_data[[column]]) == as.character(diagnostic_value)
          scoped <- TRUE
        }
      }

      if ("group" %in% names(diagnostics) && !is.na(diagnostics$group[i])) {
        value <- as.character(diagnostics$group[i])
        if (identical(value, "all")) {
          group_match <- rep(TRUE, nrow(trace_data))
        } else {
          # compute_centroid_path() describes scoped diagnostics as
          # "group=value" (and "group=value, group2=value2").  Plot legends
          # stay concise, but hover matching understands that canonical form.
          core_group_label <- if (length(group_cols) == 0L) {
            rep("all", nrow(trace_data))
          } else {
            vapply(seq_len(nrow(trace_data)), function(row) {
              paste(vapply(group_cols, function(column) {
                group_value <- trace_data[[column]][row]
                if (is.na(group_value)) group_value <- "NA"
                paste0(column, "=", as.character(group_value))
              }, character(1)), collapse = ", ")
            }, character(1))
          }
          group_match <- trace_data$.trajectory_label == value |
            trace_data$.trajectory_key == value |
            core_group_label == value
          if (length(group_cols) == 1L) {
            group_match <- group_match | as.character(trace_data[[group_cols]]) == value
          }
        }
        matches <- matches & group_match
        scoped <- TRUE
      }
      if ("time_order" %in% names(diagnostics) && !is.na(diagnostics$time_order[i])) {
        matches <- matches & as.character(trace_data$time_order) == as.character(diagnostics$time_order[i])
        scoped <- TRUE
      }
      if ("time_value" %in% names(diagnostics) && !is.na(diagnostics$time_value[i])) {
        matches <- matches & as.character(trace_data$time_value) == as.character(diagnostics$time_value[i])
        scoped <- TRUE
      }

      if (scoped) {
        message <- as.character(diagnostics$message[i])
        if ("severity" %in% names(diagnostics) && !is.na(diagnostics$severity[i])) {
          message <- paste0("[", toupper(as.character(diagnostics$severity[i])), "] ", message)
        }
        parts <- .trajectory_append_warning(parts, matches, message)
      }
    }
  }

  vapply(parts, function(messages) {
    messages <- unique(messages[!is.na(messages) & nzchar(messages)])
    if (length(messages) == 0L) "None" else paste(messages, collapse = "; ")
  }, character(1))
}

.trajectory_distance_space <- function(path, dimensions) {
  spec <- .trajectory_spec(path)
  value <- spec$distance_space
  if (is.null(value)) value <- attr(path, "distance_space", exact = TRUE)
  if (is.null(value) || length(value) == 0L || is.na(value[1L])) {
    paste0("selected centroid subspace: ", paste(sub("^centroid_", "", dimensions), collapse = ", "))
  } else {
    value <- as.character(value[1L])
    if (value %in% c("selected", "full")) {
      distance_dimensions <- spec$distance_dimensions
      if (is.null(distance_dimensions) || length(distance_dimensions) == 0L) {
        distance_dimensions <- sub("^centroid_", "", dimensions)
      }
      paste0(
        if (value == "full") "full ENA rotation" else "selected centroid subspace",
        ": ", paste(distance_dimensions, collapse = ", ")
      )
    } else {
      value
    }
  }
}

#' Return ordered, export-ready Plotly trace data without recomputing centroids.
#'
#' The returned rows retain every analytical column from `path` and add group,
#' color, coordinate-alias, warning, and distance-space metadata.  `x`, `y`, and
#' `z` are direct aliases of the selected centroid columns; no display transform
#' is applied.
trajectory_trace_data <- function(path, dimensions = NULL, group_cols = NULL, colors = NULL) {
  .trajectory_validate_path(path)
  dimensions <- .trajectory_dimension_columns(path, dimensions)
  if (length(dimensions) < 2L) {
    stop("At least two centroid dimensions are required for trajectory trace data.", call. = FALSE)
  }
  if (length(dimensions) > 3L) {
    stop("At most three dimensions can be assigned to Plotly trace coordinates.", call. = FALSE)
  }

  group_cols <- .trajectory_group_columns(path, group_cols)
  info <- .trajectory_group_info(path, group_cols)
  color_map <- trajectory_color_map(path, group_cols = group_cols, colors = colors)

  original_row <- seq_len(nrow(path))
  keys <- sort(unique(info$.trajectory_key), method = "radix")
  order_index <- unlist(lapply(keys, function(key) {
    rows <- which(info$.trajectory_key == key)
    local_order <- order(
      path$time_order[rows],
      as.character(path$time_value[rows]),
      original_row[rows],
      na.last = TRUE,
      method = "radix"
    )
    rows[local_order]
  }), use.names = FALSE)

  output <- path[order_index, , drop = FALSE]
  rownames(output) <- NULL
  output$.trajectory_key <- info$.trajectory_key[order_index]
  output$.trajectory_label <- info$.trajectory_label[order_index]
  output$.trajectory_color <- unname(color_map[output$.trajectory_key])
  node_legend <- trajectory_node_legend_data(path)
  output$.trajectory_node_key <- .trajectory_node_keys(
    output$time_order, output$time_value
  )
  node_match <- match(output$.trajectory_node_key, node_legend$node_key)
  output$.trajectory_node_label <- node_legend$node_label[node_match]
  output$.trajectory_node_color <- node_legend$node_color[node_match]
  output$.trajectory_point_order <- ave(
    seq_len(nrow(output)), output$.trajectory_key, FUN = seq_along
  )
  output$x <- output[[dimensions[1L]]]
  output$y <- output[[dimensions[2L]]]
  if (length(dimensions) >= 3L) output$z <- output[[dimensions[3L]]]
  output$.x_dimension <- sub("^centroid_", "", dimensions[1L])
  output$.y_dimension <- sub("^centroid_", "", dimensions[2L])
  if (length(dimensions) >= 3L) {
    output$.z_dimension <- sub("^centroid_", "", dimensions[3L])
  }
  output$.distance_space <- .trajectory_distance_space(path, dimensions)
  output$.trajectory_warning <- .trajectory_row_warnings(path, output, group_cols)

  attr(output, "trajectory_dimensions") <- dimensions
  attr(output, "trajectory_group_cols") <- group_cols
  attr(output, "trajectory_color_map") <- color_map
  attr(output, "trajectory_node_legend") <- node_legend
  attr(output, "trajectory_warnings") <- attr(path, "trajectory_warnings", exact = TRUE)
  attr(output, "trajectory_spec") <- attr(path, "trajectory_spec", exact = TRUE)
  attr(output, "bootstrap_spec") <- attr(path, "bootstrap_spec", exact = TRUE)
  output
}

.trajectory_format_value <- function(value, digits = 6L) {
  if (length(value) == 0L || is.na(value)) return("NA")
  if (inherits(value, "Date")) return(format(value, "%Y-%m-%d"))
  if (inherits(value, "POSIXt")) return(format(value, "%Y-%m-%d %H:%M:%S %Z"))
  if (is.numeric(value)) {
    return(format(signif(value, digits), scientific = FALSE, trim = TRUE))
  }
  as.character(value)
}

.trajectory_html_escape <- function(value) {
  value <- as.character(value)
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  value <- gsub('"', "&quot;", value, fixed = TRUE)
  value <- gsub("'", "&#39;", value, fixed = TRUE)
  value
}

.trajectory_valid_interval <- function(lower, upper) {
  if (!is.numeric(lower) || !is.numeric(upper)) {
    return(rep(FALSE, max(length(lower), length(upper))))
  }
  is.finite(lower) & is.finite(upper) & lower <= upper
}

.trajectory_interval_text <- function(data, row, column) {
  lower <- paste0(column, "_lower")
  upper <- paste0(column, "_upper")
  if (!all(c(lower, upper) %in% names(data))) return("")
  lo <- data[[lower]][row]
  hi <- data[[upper]][row]
  if (!.trajectory_valid_interval(lo, hi)) return("")
  paste0(
    " [", .trajectory_format_value(lo), ", ",
    .trajectory_format_value(hi), "]"
  )
}

.trajectory_bootstrap_n <- function(path) {
  spec <- attr(path, "bootstrap_spec", exact = TRUE)
  n_boot <- if (is.list(spec)) spec$n_boot else NULL
  if (is.null(n_boot) || length(n_boot) != 1L || !is.numeric(n_boot) ||
      is.na(n_boot) || !is.finite(n_boot) || n_boot < 0) {
    return(NULL)
  }
  as.integer(n_boot)
}

.trajectory_bootstrap_count_line <- function(data, row, column, label, n_boot) {
  count_column <- paste0(column, "_boot_n")
  if (is.null(n_boot) || !count_column %in% names(data)) return("")
  count <- data[[count_column]][row]
  if (length(count) != 1L || !is.numeric(count) || is.na(count) ||
      !is.finite(count) || count < 0) {
    return("")
  }
  paste0(
    .trajectory_html_escape(label), " bootstrap replicates: ",
    .trajectory_format_value(count), " / ",
    .trajectory_format_value(n_boot)
  )
}

.trajectory_hover_text <- function(data, dimensions, n_boot = NULL) {
  if (is.null(n_boot)) n_boot <- .trajectory_bootstrap_n(data)
  vapply(seq_len(nrow(data)), function(i) {
    lines <- c(
      paste0("<b>", .trajectory_html_escape(data$.trajectory_label[i]), "</b>"),
      paste0("Time: ", .trajectory_html_escape(.trajectory_format_value(data$time_value[i]))),
      paste0("Order: ", .trajectory_html_escape(.trajectory_format_value(data$time_order[i])))
    )

    for (dimension in dimensions) {
      label <- sub("^centroid_", "", dimension)
      lines <- c(lines, paste0(
        .trajectory_html_escape(label), ": ",
        .trajectory_html_escape(.trajectory_format_value(data[[dimension]][i])),
        .trajectory_html_escape(.trajectory_interval_text(data, i, dimension))
      ))
      bootstrap_line <- .trajectory_bootstrap_count_line(
        data, i, dimension, label, n_boot
      )
      if (nzchar(bootstrap_line)) lines <- c(lines, bootstrap_line)
    }

    used <- if ("n_used" %in% names(data)) .trajectory_format_value(data$n_used[i]) else "NA"
    total <- if ("n_total" %in% names(data)) .trajectory_format_value(data$n_total[i]) else "NA"
    lines <- c(lines, paste0("n: ", used, " / ", total))
    if (".distance_space" %in% names(data)) {
      lines <- c(lines, paste0(
        "Distance space: ", .trajectory_html_escape(data$.distance_space[i])
      ))
    }

    metrics <- c(
      step_distance = "Step distance",
      cumulative_distance = "Cumulative distance",
      speed = "Speed",
      elapsed_interval = "Elapsed interval"
    )
    for (metric in names(metrics)) {
      if (metric %in% names(data)) {
        lines <- c(lines, paste0(
          metrics[[metric]], ": ", .trajectory_format_value(data[[metric]][i]),
          .trajectory_interval_text(data, i, metric)
        ))
        bootstrap_line <- .trajectory_bootstrap_count_line(
          data, i, metric, metrics[[metric]], n_boot
        )
        if (nzchar(bootstrap_line)) lines <- c(lines, bootstrap_line)
      }
    }

    if ("n_missing" %in% names(data) || "n_excluded" %in% names(data)) {
      missing <- if ("n_missing" %in% names(data)) .trajectory_format_value(data$n_missing[i]) else "NA"
      excluded <- if ("n_excluded" %in% names(data)) .trajectory_format_value(data$n_excluded[i]) else "NA"
      lines <- c(lines, paste0("Missing / excluded: ", missing, " / ", excluded))
    }
    lines <- c(lines, paste0(
      "Warnings: ", .trajectory_html_escape(data$.trajectory_warning[i])
    ))
    paste(lines, collapse = "<br>")
  }, character(1))
}

.trajectory_error_bar <- function(data, coordinate, color) {
  lower <- paste0(coordinate, "_lower")
  upper <- paste0(coordinate, "_upper")
  if (!all(c(lower, upper) %in% names(data))) return(NULL)
  value <- data[[coordinate]]
  lo <- data[[lower]]
  hi <- data[[upper]]
  if (!is.numeric(value)) return(NULL)
  valid <- .trajectory_valid_interval(lo, hi) &
    is.finite(value) & lo <= value & value <= hi
  if (!any(valid)) return(NULL)
  array <- arrayminus <- rep(NA_real_, length(value))
  array[valid] <- hi[valid] - value[valid]
  arrayminus[valid] <- value[valid] - lo[valid]
  list(
    type = "data",
    symmetric = FALSE,
    array = array,
    arrayminus = arrayminus,
    visible = TRUE,
    color = color,
    thickness = 1,
    width = 3
  )
}

.trajectory_warning_messages <- function(path, trace_data) {
  diagnostics <- .trajectory_diagnostics(path)
  messages <- if (nrow(diagnostics) > 0L) as.character(diagnostics$message) else character()
  row_messages <- trace_data$.trajectory_warning
  row_messages <- row_messages[!is.na(row_messages) & row_messages != "None"]
  # Row-level hover warnings prefix structured diagnostics with their severity.
  # Strip that presentation prefix before combining them with the same
  # diagnostic messages for the plot annotation, otherwise one warning is
  # rendered twice (plain and as "[WARNING] ...").
  row_messages <- unlist(
    strsplit(row_messages, "; ", fixed = TRUE), use.names = FALSE
  )
  row_messages <- sub(
    "^\\[(WARNING|INFO|ERROR)\\]\\s*", "", row_messages, perl = TRUE
  )
  combined <- c(messages, row_messages)
  unique(combined[nzchar(combined)])
}

.trajectory_plot_value_key <- function(value) {
  core_key <- get0(
    ".trajectory_value_key", mode = "function", inherits = TRUE,
    ifnotfound = NULL
  )
  if (is.function(core_key)) return(core_key(value))

  # Keep the standalone plotting helper class-aware even when the analytical
  # core has not been sourced (for example, in a lightweight package test).
  if (is.factor(value)) value <- as.character(value)
  if (inherits(value, "Date")) {
    key <- format(value, "%Y-%m-%d")
  } else if (inherits(value, "POSIXt") || inherits(value, "difftime") ||
             is.numeric(value)) {
    key <- format(
      as.numeric(value), digits = 17L, scientific = TRUE, trim = TRUE
    )
  } else {
    key <- as.character(value)
  }
  key[is.na(key)] <- "<NA>"
  vapply(
    key, encodeString, character(1L), quote = '"', na.encode = TRUE,
    USE.NAMES = FALSE
  )
}

.trajectory_epoch_suffix <- function(value) {
  text <- as.character(value)
  pattern <- "\\[epoch=([^][]+)\\]\\s*$"
  matches <- regexec(pattern, text, perl = TRUE)
  pieces <- regmatches(text, matches)
  vapply(pieces, function(piece) {
    if (length(piece) < 2L) return(NA_real_)
    suppressWarnings(as.numeric(piece[2L]))
  }, numeric(1))
}

.trajectory_match_time_value <- function(value, selected_time) {
  if (length(selected_time) == 0L) return(rep(FALSE, length(value)))

  selected_epoch <- .trajectory_epoch_suffix(selected_time)
  selected_epoch <- selected_epoch[is.finite(selected_epoch)]
  if (inherits(selected_time, "POSIXt")) {
    selected_epoch <- c(selected_epoch, as.numeric(selected_time))
  }

  if (inherits(value, "POSIXt")) {
    if (!length(selected_epoch)) return(rep(FALSE, length(value)))
    return(
      .trajectory_plot_value_key(as.numeric(value)) %in%
        .trajectory_plot_value_key(selected_epoch)
    )
  }

  if (is.character(value) || is.factor(value)) {
    value_text <- as.character(value)
    matched <- .trajectory_plot_value_key(value_text) %in%
      .trajectory_plot_value_key(as.character(selected_time))
    value_epoch <- .trajectory_epoch_suffix(value_text)
    if (length(selected_epoch)) {
      matched <- matched |
        (.trajectory_plot_value_key(value_epoch) %in%
           .trajectory_plot_value_key(selected_epoch) & is.finite(value_epoch))
    }
    return(matched)
  }

  if (inherits(value, "Date")) {
    selected_value <- suppressWarnings(as.Date(as.character(selected_time)))
  } else if (is.numeric(value)) {
    selected_value <- suppressWarnings(as.numeric(as.character(selected_time)))
  } else {
    selected_value <- selected_time
  }
  selected_value <- selected_value[!is.na(selected_value)]
  if (!length(selected_value)) return(rep(FALSE, length(value)))
  .trajectory_plot_value_key(value) %in% .trajectory_plot_value_key(selected_value)
}

.trajectory_filter_selected_time <- function(data, selected_time, path) {
  if (is.null(data) || is.null(selected_time) || nrow(data) == 0L) return(data)
  spec <- .trajectory_spec(path)
  candidates <- unique(c("time_value", "time_order", as.character(spec$time_var)))
  candidates <- intersect(candidates, names(data))
  if (length(candidates) == 0L) return(data)

  if (is.list(selected_time) && !is.null(names(selected_time))) {
    filters <- intersect(names(selected_time), candidates)
    keep <- rep(TRUE, nrow(data))
    for (column in filters) {
      keep <- keep & .trajectory_match_time_value(
        data[[column]], selected_time[[column]]
      )
    }
    if (length(filters) > 0L) return(data[keep, , drop = FALSE])
  }

  keep <- Reduce(`|`, lapply(candidates, function(column) {
    .trajectory_match_time_value(data[[column]], selected_time)
  }))
  data[keep, , drop = FALSE]
}

.trajectory_overlay_coordinate <- function(data, dimension, axis, endpoint = FALSE) {
  label <- sub("^centroid_", "", dimension)
  if (!endpoint) {
    candidates <- c(axis, dimension, label)
  } else {
    candidates <- c(
      paste0(axis, "end"), paste0(axis, "_end"),
      paste0(dimension, "_end"), paste0(label, "_end"),
      paste0(dimension, "_to"), paste0(label, "_to"),
      paste0("target_", dimension), paste0("target_", label)
    )
  }
  candidate <- intersect(candidates, names(data))
  if (length(candidate) == 0L) NULL else data[[candidate[1L]]]
}

.trajectory_node_id_column <- function(nodes) {
  candidate <- intersect(c("id", "node", "code", "label", "name"), names(nodes))
  if (length(candidate) == 0L) NULL else candidate[1L]
}

.trajectory_complete_edge_coordinates <- function(edges, nodes, dimensions) {
  if (is.null(nodes) || !all(c("source", "target") %in% names(edges))) return(edges)
  id_column <- .trajectory_node_id_column(nodes)
  if (is.null(id_column)) return(edges)

  source_index <- match(as.character(edges$source), as.character(nodes[[id_column]]))
  target_index <- match(as.character(edges$target), as.character(nodes[[id_column]]))
  axes <- c("x", "y", "z")[seq_along(dimensions)]
  for (i in seq_along(dimensions)) {
    start <- .trajectory_overlay_coordinate(edges, dimensions[i], axes[i], endpoint = FALSE)
    end <- .trajectory_overlay_coordinate(edges, dimensions[i], axes[i], endpoint = TRUE)
    node_coordinate <- .trajectory_overlay_coordinate(nodes, dimensions[i], axes[i], endpoint = FALSE)
    if (is.null(node_coordinate)) next
    if (is.null(start)) edges[[axes[i]]] <- node_coordinate[source_index]
    if (is.null(end)) edges[[paste0(axes[i], "end")]] <- node_coordinate[target_index]
  }
  edges
}

.trajectory_add_code_nodes <- function(plot, nodes, path, dimensions, view,
                                       selected_time, display_scale) {
  if (is.null(nodes)) return(plot)
  if (!is.data.frame(nodes)) stop("`code_nodes` must be a data frame.", call. = FALSE)
  nodes <- .trajectory_filter_selected_time(nodes, selected_time, path)
  if (nrow(nodes) == 0L) return(plot)

  axes <- c("x", "y", "z")[seq_along(dimensions)]
  coordinates <- Map(function(dimension, axis) {
    .trajectory_overlay_coordinate(nodes, dimension, axis, endpoint = FALSE)
  }, dimensions, axes)
  if (any(vapply(coordinates, is.null, logical(1)))) {
    stop(
      "`code_nodes` must contain x/y(/z), selected dimension, or centroid dimension columns.",
      call. = FALSE
    )
  }

  label_column <- .trajectory_node_id_column(nodes)
  labels <- if (is.null(label_column)) rep("Code node", nrow(nodes)) else as.character(nodes[[label_column]])
  colors <- if ("color" %in% names(nodes)) as.character(nodes$color) else rep("#333333", nrow(nodes))
  sizes <- if ("size" %in% names(nodes)) as.numeric(nodes$size) else rep(6, nrow(nodes))

  arguments <- list(
    p = plot,
    type = if (view == "3d") "scatter3d" else "scatter",
    mode = "markers+text",
    x = coordinates[[1L]],
    y = coordinates[[2L]],
    text = labels,
    textposition = "top center",
    hovertemplate = "%{text}<extra>Code node</extra>",
    marker = list(color = colors, size = sizes * display_scale),
    name = "Code nodes",
    legendgroup = "__trajectory_code_nodes__",
    showlegend = FALSE,
    meta = list(trajectory_role = "code_nodes")
  )
  if (view == "3d") arguments$z <- coordinates[[3L]]
  do.call(plotly::add_trace, arguments)
}

.trajectory_edge_width_bins <- function(width, max_bins = 6L) {
  width <- suppressWarnings(as.numeric(width))
  finite <- is.finite(width) & width > 0
  if (!any(finite)) return(rep(1L, length(width)))
  width[!finite] <- stats::median(width[finite])
  unique_width <- sort(unique(width), method = "radix")
  if (length(unique_width) <= max_bins) return(match(width, unique_width))

  breaks <- unique(stats::quantile(
    width,
    probs = seq(0, 1, length.out = max_bins + 1L),
    names = FALSE,
    type = 8L
  ))
  if (length(breaks) < 2L) return(rep(1L, length(width)))
  as.integer(cut(width, breaks = breaks, include.lowest = TRUE, labels = FALSE))
}

.trajectory_add_network_edges <- function(plot, edges, nodes, path, dimensions,
                                          view, selected_time, display_scale) {
  if (is.null(edges)) return(plot)
  if (!is.data.frame(edges)) stop("`network_edges` must be a data frame.", call. = FALSE)
  edges <- .trajectory_filter_selected_time(edges, selected_time, path)
  if (nrow(edges) == 0L) return(plot)
  selected_nodes <- if (is.null(nodes)) {
    NULL
  } else {
    .trajectory_filter_selected_time(nodes, selected_time, path)
  }
  edges <- .trajectory_complete_edge_coordinates(edges, selected_nodes, dimensions)

  axes <- c("x", "y", "z")[seq_along(dimensions)]
  starts <- Map(function(dimension, axis) {
    .trajectory_overlay_coordinate(edges, dimension, axis, endpoint = FALSE)
  }, dimensions, axes)
  ends <- Map(function(dimension, axis) {
    .trajectory_overlay_coordinate(edges, dimension, axis, endpoint = TRUE)
  }, dimensions, axes)
  if (any(vapply(c(starts, ends), is.null, logical(1)))) {
    stop(
      paste0(
        "`network_edges` must contain coordinate endpoint pairs (for example ",
        "x/xend and y/yend) or source/target values resolvable through `code_nodes`."
      ),
      call. = FALSE
    )
  }

  edge_colors <- if ("color" %in% names(edges)) as.character(edges$color) else rep("#888888", nrow(edges))
  edge_colors[is.na(edge_colors)] <- "#888888"
  labels <- if ("label" %in% names(edges)) as.character(edges$label) else {
    if (all(c("source", "target") %in% names(edges))) {
      paste(edges$source, "→", edges$target)
    } else {
      rep("Network edge", nrow(edges))
    }
  }
  weights <- if ("weight" %in% names(edges)) {
    suppressWarnings(as.numeric(edges$weight))
  } else {
    rep(NA_real_, nrow(edges))
  }
  widths <- if ("width" %in% names(edges)) {
    suppressWarnings(as.numeric(edges$width))
  } else {
    rep(1.5, nrow(edges))
  }
  finite_width <- is.finite(widths) & widths > 0
  widths[!finite_width] <- if (any(finite_width)) {
    stats::median(widths[finite_width])
  } else {
    1.5
  }
  signs <- if ("sign" %in% names(edges)) {
    as.character(edges$sign)
  } else ifelse(is.finite(weights) & weights < 0, "negative", "positive")
  signs[is.na(signs) | !nzchar(signs)] <- "unknown"
  width_bins <- .trajectory_edge_width_bins(widths)
  style_keys <- paste(edge_colors, signs, width_bins, sep = "\r")

  for (style_key in sort(unique(style_keys), method = "radix")) {
    rows <- which(style_keys == style_key)
    color <- edge_colors[rows[[1L]]]
    coordinate_vectors <- lapply(seq_along(dimensions), function(i) {
      as.vector(rbind(starts[[i]][rows], ends[[i]][rows], rep(NA_real_, length(rows))))
    })
    width <- stats::median(widths[rows], na.rm = TRUE)
    if (!is.finite(width)) width <- 1.5
    edge_hover <- vapply(rows, function(row) {
      weight_text <- if (is.finite(weights[[row]])) {
        paste0("<br>Weight: ", .trajectory_format_value(weights[[row]]))
      } else {
        ""
      }
      paste0(
        .trajectory_html_escape(labels[[row]]),
        weight_text,
        "<br>Width input: ", .trajectory_format_value(widths[[row]]),
        "<br>Displayed width bin: ", .trajectory_format_value(width)
      )
    }, character(1))
    hover <- as.vector(rbind(
      edge_hover, edge_hover, rep(NA_character_, length(rows))
    ))
    arguments <- list(
      p = plot,
      type = if (view == "3d") "scatter3d" else "scatter",
      mode = "lines",
      x = coordinate_vectors[[1L]],
      y = coordinate_vectors[[2L]],
      text = hover,
      hovertemplate = "%{text}<extra>Network</extra>",
      line = list(color = color, width = width * display_scale),
      name = "Network",
      legendgroup = "__trajectory_network__",
      showlegend = FALSE,
      connectgaps = FALSE,
      meta = list(
        trajectory_role = "network",
        edge_sign = signs[rows[[1L]]],
        width_bin = width_bins[rows[[1L]]],
        edge_count = length(rows)
      )
    )
    if (view == "3d") arguments$z <- coordinate_vectors[[3L]]
    plot <- do.call(plotly::add_trace, arguments)
  }
  plot
}

.trajectory_apply_overlay_hooks <- function(plot, hooks, context) {
  if (is.null(hooks)) return(plot)
  if (is.function(hooks)) hooks <- list(hooks)
  if (!is.list(hooks) || !all(vapply(hooks, is.function, logical(1)))) {
    stop("`overlay_hooks` must be a function or a list of functions.", call. = FALSE)
  }
  for (hook in hooks) {
    plot <- hook(plot, context)
    if (!inherits(plot, "plotly")) {
      stop("Every overlay hook must return a Plotly widget.", call. = FALSE)
    }
  }
  plot
}

.trajectory_add_warning_annotation <- function(plot, messages) {
  messages <- unique(messages[!is.na(messages) & nzchar(messages)])
  if (length(messages) == 0L) return(plot)
  shown <- head(messages, 5L)
  if (length(messages) > length(shown)) {
    shown <- c(shown, paste0("+", length(messages) - length(shown), " more warning(s)"))
  }
  text <- paste0(
    "<b>Trajectory warning</b><br>",
    paste(.trajectory_html_escape(shown), collapse = "<br>")
  )
  plotly::layout(
    plot,
    annotations = list(list(
      x = 0,
      y = 1,
      xref = "paper",
      yref = "paper",
      xanchor = "left",
      yanchor = "top",
      xshift = 8,
      yshift = -8,
      align = "left",
      showarrow = FALSE,
      text = text,
      bgcolor = "rgba(255, 245, 204, 0.95)",
      bordercolor = "#B7791F",
      borderwidth = 1,
      font = .trajectory_plot_font(14L, color = "#713F12")
    ))
  )
}

.trajectory_cross_product <- function(left, right) {
  c(
    left[2L] * right[3L] - left[3L] * right[2L],
    left[3L] * right[1L] - left[1L] * right[3L],
    left[1L] * right[2L] - left[2L] * right[1L]
  )
}

.trajectory_camera_eye <- function(camera = NULL) {
  fallback <- c(1.25, 1.25, 1.25)
  if (!is.list(camera) || !is.list(camera$eye)) return(fallback)
  eye <- suppressWarnings(as.numeric(unlist(
    camera$eye[c("x", "y", "z")], use.names = FALSE
  )))
  if (length(eye) != 3L || any(!is.finite(eye)) ||
      sqrt(sum(eye^2)) <= sqrt(.Machine$double.eps)) {
    return(fallback)
  }
  eye
}

.trajectory_arrow_scale <- function(data, coordinate_names, view) {
  values <- do.call(cbind, lapply(coordinate_names, function(name) {
    as.numeric(data[[name]])
  }))
  if (!is.matrix(values)) values <- matrix(values, ncol = length(coordinate_names))

  lower <- upper <- numeric(length(coordinate_names))
  for (column in seq_along(coordinate_names)) {
    finite <- values[is.finite(values[, column]), column]
    if (length(finite)) {
      lower[column] <- min(finite)
      upper[column] <- max(finite)
    } else {
      lower[column] <- 0
      upper[column] <- 0
    }
  }
  span <- upper - lower
  positive_span <- span[is.finite(span) & span > sqrt(.Machine$double.eps)]
  fallback <- if (length(positive_span)) max(positive_span) else 1

  # A 3D scene uses aspectmode = "data", so one common scale preserves the
  # analytical vector direction. A 2D Cartesian plot fills each axis
  # independently, so axis-specific scales keep the arrowhead visually aligned
  # with the rendered segment.
  scale <- if (identical(view, "3d")) {
    rep(fallback, length(coordinate_names))
  } else {
    ifelse(is.finite(span) & span > sqrt(.Machine$double.eps), span, fallback)
  }
  list(lower = lower, scale = scale)
}

.trajectory_direction_geometry <- function(
    data, scale_data, view = c("3d", "2d"), arrow_size = 0.0224,
    camera = NULL) {
  view <- match.arg(view)
  coordinate_names <- if (identical(view, "3d")) c("x", "y", "z") else c("x", "y")
  output <- stats::setNames(
    replicate(length(coordinate_names), numeric(), simplify = FALSE),
    coordinate_names
  )
  if (nrow(data) < 2L) {
    return(c(output, list(segment_count = 0L)))
  }

  scale <- .trajectory_arrow_scale(scale_data, coordinate_names, view)
  coordinates <- do.call(cbind, lapply(coordinate_names, function(name) {
    as.numeric(data[[name]])
  }))
  normalized <- sweep(
    sweep(coordinates, 2L, scale$lower, FUN = "-"),
    2L, scale$scale, FUN = "/"
  )
  has_time_order <- "time_order" %in% names(data)
  time_order <- if (has_time_order) {
    suppressWarnings(as.numeric(as.character(data$time_order)))
  } else {
    rep(NA_real_, nrow(data))
  }
  segment_count <- 0L
  tolerance <- sqrt(.Machine$double.eps)

  append_wing <- function(base, tip) {
    raw <- rbind(
      scale$lower + base * scale$scale,
      scale$lower + tip * scale$scale,
      rep(NA_real_, length(coordinate_names))
    )
    for (column in seq_along(coordinate_names)) {
      name <- coordinate_names[column]
      output[[name]] <<- c(output[[name]], raw[, column])
    }
  }

  for (row in 2:nrow(normalized)) {
    if (has_time_order && (
      !is.finite(time_order[row - 1L]) || !is.finite(time_order[row]) ||
      time_order[row] <= time_order[row - 1L]
    )) next
    start <- normalized[row - 1L, ]
    end <- normalized[row, ]
    if (!all(is.finite(c(start, end)))) next
    direction <- end - start
    segment_length <- sqrt(sum(direction^2))
    if (!is.finite(segment_length) || segment_length <= tolerance) next

    unit <- direction / segment_length
    head_length <- min(arrow_size, segment_length * 0.224)
    if (!is.finite(head_length) || head_length <= tolerance) next
    # The marker is redrawn above this display trace, masking the portion inside
    # its pixel-sized circle so the visible tip terminates at the node edge.
    tip <- end
    base_center <- tip - head_length * unit
    head_width <- head_length * 0.45

    if (identical(view, "2d")) {
      perpendicular <- c(-unit[2L], unit[1L])
      base_points <- list(
        base_center + head_width * perpendicular,
        base_center - head_width * perpendicular
      )
    } else {
      # Two wings are enough for a conventional arrow. Orient their shared
      # plane toward the active camera so the V remains visible after rotation.
      perpendicular <- .trajectory_cross_product(
        unit, .trajectory_camera_eye(camera)
      )
      perpendicular_length <- sqrt(sum(perpendicular^2))
      if (!is.finite(perpendicular_length) || perpendicular_length <= tolerance) {
        reference <- diag(3L)[which.min(abs(unit)), ]
        perpendicular <- .trajectory_cross_product(unit, reference)
        perpendicular_length <- sqrt(sum(perpendicular^2))
      }
      perpendicular <- perpendicular / perpendicular_length
      base_points <- list(
        base_center + head_width * perpendicular,
        base_center - head_width * perpendicular
      )
    }

    for (base in base_points) append_wing(base, tip)
    segment_count <- segment_count + 1L
  }
  c(output, list(segment_count = segment_count))
}

.trajectory_add_direction_arrows <- function(
    plot, data, scale_data, view, key, color, display_scale, line_width,
    arrow_size, camera = NULL) {
  geometry <- .trajectory_direction_geometry(
    data,
    scale_data = scale_data,
    view = view,
    arrow_size = arrow_size,
    camera = camera
  )
  if (geometry$segment_count == 0L) return(plot)

  arguments <- list(
    p = plot,
    type = if (identical(view, "3d")) "scatter3d" else "scatter",
    mode = "lines",
    x = geometry$x,
    y = geometry$y,
    name = paste0(data$.trajectory_label[1L], " direction"),
    legendgroup = key,
    showlegend = FALSE,
    hoverinfo = "skip",
    connectgaps = FALSE,
    line = list(
      color = color,
      width = max(2, line_width * display_scale * 1.25)
    ),
    meta = list(
      trajectory_role = "direction_arrows",
      trajectory_key = key,
      segment_count = geometry$segment_count
    )
  )
  if (identical(view, "3d")) arguments$z <- geometry$z
  do.call(plotly::add_trace, arguments)
}

.trajectory_add_centroid_node_markers <- function(
    plot, data, dimensions, n_boot, view, key, display_scale,
    marker_size) {
  arguments <- list(
    p = plot,
    type = if (identical(view, "3d")) "scatter3d" else "scatter",
    mode = "markers",
    x = data$x,
    y = data$y,
    text = .trajectory_hover_text(data, dimensions, n_boot = n_boot),
    hovertemplate = "%{text}<extra></extra>",
    hoverinfo = "text",
    name = paste0(data$.trajectory_label[1L], " centroid nodes"),
    legendgroup = key,
    showlegend = FALSE,
    marker = list(
      color = data$.trajectory_node_color,
      size = marker_size * display_scale,
      line = list(
        color = data$.trajectory_color,
        width = max(1, display_scale)
      )
    ),
    meta = list(
      trajectory_role = "node_markers",
      trajectory_key = key
    )
  )
  if (identical(view, "3d")) arguments$z <- data$z
  do.call(plotly::add_trace, arguments)
}

#' Plot a centroid path as either a three-dimensional path or a 2D projection.
#'
#' `display_scale` scales marker and line styling only.  `camera` is applied only
#' to the 3D Plotly scene.  Neither argument changes trace coordinates, centroid
#' metrics, or the original `path`, which is attached to the returned widget as
#' `attr(widget, "trajectory_data")` for inspection and export.
plot_centroid_trajectory <- function(
    path,
    dimensions = NULL,
    view = c("3d", "2d"),
    group_cols = NULL,
    colors = NULL,
    camera = NULL,
    display_scale = 1,
    code_nodes = NULL,
    network_edges = NULL,
    selected_time = NULL,
    overlay_hooks = NULL,
    show_warnings = TRUE,
    show_direction = TRUE,
    arrow_size = 0.0224,
    marker_size = 7,
    line_width = 3,
    axis_titles = NULL) {
  .trajectory_require_plotly()
  .trajectory_validate_path(path)
  view <- match.arg(view)
  required_dimensions <- if (view == "3d") 3L else 2L
  dimensions <- .trajectory_dimension_columns(path, dimensions, required = required_dimensions)

  if (length(display_scale) != 1L || !is.numeric(display_scale) ||
      !is.finite(display_scale) || display_scale <= 0) {
    stop("`display_scale` must be one positive finite number.", call. = FALSE)
  }
  if (length(marker_size) != 1L || !is.numeric(marker_size) ||
      !is.finite(marker_size) || marker_size <= 0) {
    stop("`marker_size` must be one positive finite number.", call. = FALSE)
  }
  if (length(line_width) != 1L || !is.numeric(line_width) ||
      !is.finite(line_width) || line_width <= 0) {
    stop("`line_width` must be one positive finite number.", call. = FALSE)
  }
  if (length(show_direction) != 1L || !is.logical(show_direction) ||
      is.na(show_direction)) {
    stop("`show_direction` must be TRUE or FALSE.", call. = FALSE)
  }
  if (length(arrow_size) != 1L || !is.numeric(arrow_size) ||
      !is.finite(arrow_size) || arrow_size <= 0 || arrow_size > 0.2) {
    stop("`arrow_size` must be one finite number greater than 0 and no more than 0.2.", call. = FALSE)
  }

  trace_data <- trajectory_trace_data(
    path,
    dimensions = dimensions,
    group_cols = group_cols,
    colors = colors
  )
  group_cols <- attr(trace_data, "trajectory_group_cols", exact = TRUE)
  color_map <- attr(trace_data, "trajectory_color_map", exact = TRUE)
  n_boot <- .trajectory_bootstrap_n(trace_data)
  labels <- sub("^centroid_", "", dimensions)
  if (!is.null(axis_titles)) {
    axis_titles <- as.character(axis_titles)
    if (length(axis_titles) < required_dimensions) {
      stop(sprintf("`axis_titles` must contain at least %d labels.", required_dimensions), call. = FALSE)
    }
    labels <- axis_titles[seq_len(required_dimensions)]
  }

  plot <- plotly::plot_ly()
  keys <- unique(trace_data$.trajectory_key)
  for (key in keys) {
    data <- trace_data[trace_data$.trajectory_key == key, , drop = FALSE]
    color <- data$.trajectory_color[1L]
    hover <- .trajectory_hover_text(data, dimensions, n_boot = n_boot)
    arguments <- list(
      p = plot,
      type = if (view == "3d") "scatter3d" else "scatter",
      mode = "lines+markers",
      x = data$x,
      y = data$y,
      text = hover,
      hovertemplate = "%{text}<extra></extra>",
      hoverinfo = "text",
      name = data$.trajectory_label[1L],
      legendgroup = key,
      showlegend = TRUE,
      connectgaps = FALSE,
      line = list(color = color, width = line_width * display_scale),
      marker = list(
        color = data$.trajectory_node_color,
        size = marker_size * display_scale,
        line = list(color = color, width = max(1, display_scale))
      ),
      error_x = .trajectory_error_bar(data, dimensions[1L], color),
      error_y = .trajectory_error_bar(data, dimensions[2L], color),
      meta = list(
        trajectory_role = "path",
        trajectory_key = key,
        analytical_dimensions = dimensions
      )
    )
    if (view == "3d") {
      arguments$z <- data$z
      arguments$error_z <- .trajectory_error_bar(data, dimensions[3L], color)
    }
    plot <- do.call(plotly::add_trace, arguments)
  }

  if (view == "3d") {
    scene <- list(
      xaxis = .trajectory_plot_axis(labels[1L]),
      yaxis = .trajectory_plot_axis(labels[2L]),
      zaxis = .trajectory_plot_axis(labels[3L]),
      aspectmode = "data"
    )
    if (!is.null(camera)) scene$camera <- camera
    plot <- plotly::layout(
      plot,
      scene = scene,
      font = .trajectory_plot_font(14L),
      hoverlabel = .trajectory_hover_label(),
      legend = list(
        font = .trajectory_plot_font(14L),
        title = list(
          text = "Trajectory",
          font = .trajectory_plot_font(14L)
        )
      ),
      margin = list(l = 0, r = 0, b = 0, t = 20)
    )
  } else {
    plot <- plotly::layout(
      plot,
      xaxis = .trajectory_plot_axis(labels[1L]),
      yaxis = .trajectory_plot_axis(labels[2L]),
      font = .trajectory_plot_font(14L),
      hoverlabel = .trajectory_hover_label(),
      legend = list(
        font = .trajectory_plot_font(14L),
        title = list(
          text = "Trajectory",
          font = .trajectory_plot_font(14L)
        )
      ),
      margin = list(l = 55, r = 15, b = 50, t = 20)
    )
  }

  # Overlay traces consume already-computed coordinates and cannot modify the
  # trajectory rows.  Edges are drawn before nodes so labels remain visible.
  plot <- .trajectory_add_network_edges(
    plot, network_edges, code_nodes, path, dimensions, view,
    selected_time, display_scale
  )
  # Direction remains visible above contextual network edges, while code nodes
  # and their labels remain the topmost overlay.
  if (isTRUE(show_direction)) {
    for (key in keys) {
      data <- trace_data[trace_data$.trajectory_key == key, , drop = FALSE]
      plot <- .trajectory_add_direction_arrows(
        plot,
        data = data,
        scale_data = trace_data,
        view = view,
        key = key,
        color = data$.trajectory_color[1L],
        display_scale = display_scale,
        line_width = line_width,
        arrow_size = arrow_size,
        camera = camera
      )
    }
    # These marker-only traces are intentionally last among the trajectory
    # layers. Their pixel-sized circles mask the arrowhead interior, making the
    # visible arrow meet the destination node boundary cleanly at any zoom.
    for (key in keys) {
      data <- trace_data[trace_data$.trajectory_key == key, , drop = FALSE]
      plot <- .trajectory_add_centroid_node_markers(
        plot,
        data = data,
        dimensions = dimensions,
        n_boot = n_boot,
        view = view,
        key = key,
        display_scale = display_scale,
        marker_size = marker_size
      )
    }
  }
  plot <- .trajectory_add_code_nodes(
    plot, code_nodes, path, dimensions, view, selected_time, display_scale
  )

  context <- list(
    view = view,
    dimensions = dimensions,
    trajectory_data = path,
    trace_data = trace_data,
    selected_time = selected_time,
    group_cols = group_cols,
    color_map = color_map,
    show_direction = show_direction,
    arrow_size = arrow_size
  )
  plot <- .trajectory_apply_overlay_hooks(plot, overlay_hooks, context)

  warning_messages <- .trajectory_warning_messages(path, trace_data)
  if (isTRUE(show_warnings)) {
    plot <- .trajectory_add_warning_annotation(plot, warning_messages)
  }

  attr(plot, "trajectory_data") <- path
  attr(plot, "trajectory_trace_data") <- trace_data
  attr(plot, "trajectory_dimensions") <- dimensions
  attr(plot, "trajectory_group_cols") <- group_cols
  attr(plot, "trajectory_color_map") <- color_map
  attr(plot, "trajectory_node_legend") <- attr(
    trace_data, "trajectory_node_legend", exact = TRUE
  )
  attr(plot, "trajectory_view") <- view
  attr(plot, "trajectory_show_direction") <- show_direction
  attr(plot, "trajectory_warnings") <- warning_messages
  plot
}

#' Plot one scatter3d lines-and-markers trace per centroid trajectory.
plot_centroid_trajectory_3d <- function(path, dimensions = NULL, ...) {
  plot_centroid_trajectory(path, dimensions = dimensions, view = "3d", ...)
}

#' Plot a two-dimensional projection of an unchanged centroid trajectory table.
plot_centroid_trajectory_2d <- function(path, dimensions = NULL, ...) {
  plot_centroid_trajectory(path, dimensions = dimensions, view = "2d", ...)
}
