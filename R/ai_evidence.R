# Deterministic, aggregate-only evidence contracts for AI interpretation.
#
# This file deliberately has no Shiny or API-client dependencies.  It accepts
# already validated ENA objects and returns a small JSON-friendly ledger.  Only
# fields explicitly constructed below can cross the AI boundary: raw point
# rows, ENA_UNIT values, participant identifiers, and unit-level networks are
# never copied into the result.
#
# Main entry point and canonical settings keys:
#
#   ena3d_ai_build_evidence(
#     ena_obj,
#     view = "overall" | "network" | "comparison" | "change" |
#            "stats" | "trajectory",
#     settings = list(
#       group_var = "condition",          # overall/network/comparison/stats
#       selected_groups = c("A", "B"),    # overall/network; NULL means all
#       axes = c("MR1", "SVD2", "SVD3"), # at most three displayed axes
#       comparison_groups = c("A", "B"),  # comparison/stats; A minus B
#       selection_type = "group",         # network; "unit" is rejected
#       change_var = "wave",              # change only; identifier vars reject
#       change_values = c("T1", "T2"),    # optional ordered slice selection
#       stats_design = "unpaired",        # stats context only
#       p_adjust_method = "holm",          # stats context only
#       alternative = "two.sided",        # stats context only
#       trajectory_group_var = "condition", # optional trajectory override
#       trajectory_time_var = "wave"       # optional trajectory override
#     ),
#     stats_result = aggregate_stats_bundle,
#     trajectory_result = trajectory_module_result,
#     min_cell_n = 5L,
#     top_n = 10L,
#     max_slices = 20L,
#     max_evidence = 64L
#   )
#
# The stats bundle may be `list(results = named_axis_results)` or a named list
# of axis results.  Each axis result is read only for aggregate scalar test
# fields and aggregate summary rows (Mean/Median/Std/Valid N).  The trajectory
# result is the public module result containing path/bootstrap/comparison/
# diagnostics/settings; only documented aggregate columns are read.


ena3d_ai_evidence_policy <- function(min_cell_n = 5L, top_n = 10L,
                                     max_slices = 20L,
                                     max_evidence = 64L,
                                     label_max_chars = 96L,
                                     text_max_chars = 240L) {
  bounded_integer <- function(value, label, minimum, maximum) {
    numeric <- suppressWarnings(as.numeric(value))
    if (length(numeric) != 1L || is.na(numeric) || !is.finite(numeric) ||
        numeric != as.integer(numeric) || numeric < minimum ||
        numeric > maximum) {
      stop(sprintf(
        "%s must be one integer between %d and %d.",
        label, minimum, maximum
      ), call. = FALSE)
    }
    as.integer(numeric)
  }

  list(
    min_cell_n = bounded_integer(min_cell_n, "min_cell_n", 2L, 1000L),
    top_n = bounded_integer(top_n, "top_n", 1L, 25L),
    max_slices = bounded_integer(max_slices, "max_slices", 1L, 30L),
    max_evidence = bounded_integer(max_evidence, "max_evidence", 1L, 96L),
    label_max_chars = bounded_integer(
      label_max_chars, "label_max_chars", 16L, 160L
    ),
    text_max_chars = bounded_integer(
      text_max_chars, "text_max_chars", 40L, 500L
    )
  )
}


ena3d_ai_clean_text <- function(value, max_chars = 240L,
                                empty = "Not specified") {
  max_chars <- suppressWarnings(as.integer(max_chars))
  if (length(max_chars) != 1L || is.na(max_chars) || max_chars < 1L) {
    stop("max_chars must be a positive integer.", call. = FALSE)
  }
  if (is.null(value) || !length(value) || is.na(value[[1L]])) {
    return(substr(empty, 1L, max_chars))
  }
  value <- as.character(value[[1L]])
  value <- iconv(value, from = "", to = "UTF-8", sub = "\uFFFD")
  if (is.na(value)) value <- empty

  # Strip C0/C1 controls, line/paragraph separators, zero-width characters,
  # and bidi overrides that can disguise the displayed meaning of a label.
  value <- gsub("[[:cntrl:]]+", " ", value, perl = TRUE)
  value <- gsub(
    "[\u200B-\u200F\u2028-\u202E\u2066-\u2069\uFEFF]",
    "", value, perl = TRUE
  )
  value <- gsub("[[:space:]]+", " ", trimws(value), perl = TRUE)

  # Labels are data, never markup or prompt delimiters.  Neutralising the
  # common delimiters also makes accidental raw-HTML rendering inert.
  value <- gsub("<", "\u2039", value, fixed = TRUE)
  value <- gsub(">", "\u203A", value, fixed = TRUE)
  value <- gsub("`", "'", value, fixed = TRUE)
  value <- gsub("\\{", "(", value, perl = TRUE)
  value <- gsub("\\}", ")", value, perl = TRUE)
  value <- gsub("[[:space:]]+", " ", trimws(value), perl = TRUE)
  if (!nzchar(value)) value <- empty
  substr(enc2utf8(value), 1L, max_chars)
}


ena3d_ai_clean_label <- function(value, max_chars = 96L) {
  ena3d_ai_clean_text(value, max_chars = max_chars, empty = "(blank)")
}


.ena3d_ai_value_text <- function(value) {
  if (length(value) != 1L || is.na(value)) return(NA_character_)
  if (inherits(value, "POSIXt")) {
    timezone <- attr(value, "tzone")
    if (is.null(timezone) || !length(timezone) || !nzchar(timezone[[1L]])) {
      timezone <- "UTC"
    }
    return(format(value, "%Y-%m-%dT%H:%M:%S%z", tz = timezone[[1L]]))
  }
  if (inherits(value, "Date")) return(format(value, "%Y-%m-%d"))
  if (inherits(value, "difftime")) {
    return(paste(as.numeric(value), attr(value, "units")))
  }
  if (is.numeric(value)) {
    if (!is.finite(value)) return(NA_character_)
    return(sprintf("%.17g", as.numeric(value)))
  }
  as.character(value)
}


.ena3d_ai_value_keys <- function(values) {
  vapply(seq_along(values), function(index) {
    value <- values[index]
    type <- if (inherits(values, "POSIXt")) {
      "datetime"
    } else if (inherits(values, "Date")) {
      "date"
    } else if (inherits(values, "difftime")) {
      "duration"
    } else if (is.numeric(values)) {
      "number"
    } else {
      "text"
    }
    paste0(type, ":", .ena3d_ai_value_text(value))
  }, character(1L))
}


.ena3d_ai_observed_values <- function(values) {
  keep <- !is.na(values)
  if (is.numeric(values) || inherits(values, c("Date", "POSIXt", "difftime"))) {
    keep <- keep & is.finite(as.numeric(values))
  }
  values <- values[keep]
  if (!length(values)) return(values)
  values[!duplicated(.ena3d_ai_value_keys(values))]
}


.ena3d_ai_label_registry <- function(values, policy) {
  values <- .ena3d_ai_observed_values(values)
  if (!length(values)) {
    return(list(values = values, keys = character(), labels = character()))
  }
  labels <- vapply(seq_along(values), function(index) {
    ena3d_ai_clean_label(
      .ena3d_ai_value_text(values[index]), policy$label_max_chars
    )
  }, character(1L))
  # Sanitisation/truncation can collapse two distinct source labels.  Keep
  # aggregate scopes distinct without exposing the unsanitised source text.
  occurrence <- ave(seq_along(labels), labels, FUN = seq_along)
  duplicate_total <- ave(seq_along(labels), labels, FUN = length)
  duplicate <- duplicate_total > 1L
  if (any(duplicate)) {
    suffix <- paste0(" [", occurrence[duplicate], "]")
    room <- pmax(1L, policy$label_max_chars - nchar(suffix, type = "chars"))
    labels[duplicate] <- paste0(
      substring(labels[duplicate], 1L, room), suffix
    )
  }
  list(
    values = values,
    keys = .ena3d_ai_value_keys(values),
    labels = labels
  )
}


.ena3d_ai_registry_labels <- function(registry, values, policy) {
  if (!length(values)) return(character())
  keys <- .ena3d_ai_value_keys(values)
  positions <- match(keys, registry$keys)
  output <- registry$labels[positions]
  missing <- is.na(positions)
  if (any(missing)) {
    output[missing] <- vapply(which(missing), function(index) {
      ena3d_ai_clean_label(
        .ena3d_ai_value_text(values[index]), policy$label_max_chars
      )
    }, character(1L))
  }
  unname(output)
}


.ena3d_ai_group_match <- function(values, selected) {
  if (is.null(selected) || !length(selected)) return(rep(FALSE, length(values)))
  observed <- as.character(values)
  selected <- as.character(unlist(selected, recursive = TRUE, use.names = FALSE))
  !is.na(values) & observed %in% selected
}


.ena3d_ai_scalar_name <- function(value, label, allow_null = FALSE) {
  if (allow_null && (is.null(value) || !length(value) ||
                     is.na(value[[1L]]) || !nzchar(as.character(value[[1L]])))) {
    return(NULL)
  }
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value)) {
    stop(sprintf("%s must be one non-empty column name.", label), call. = FALSE)
  }
  value
}


.ena3d_ai_is_identifier_name <- function(value) {
  if (is.null(value) || !length(value)) return(FALSE)
  normalized <- tolower(gsub("[^[:alnum:]]", "", as.character(value[[1L]])))
  normalized %in% c(
    "enaunit", "unit", "unitid", "participant", "participantid",
    "respondent", "respondentid", "subjectid", "userid", "username",
    "studentid", "personid", "id"
  )
}


.ena3d_ai_require_aggregate_variable <- function(value, label) {
  value <- .ena3d_ai_scalar_name(value, label)
  if (.ena3d_ai_is_identifier_name(value)) {
    stop(sprintf(
      "%s cannot be an ENA unit or participant identifier.", label
    ), call. = FALSE)
  }
  value
}


.ena3d_ai_frame <- function(value, label) {
  if (!is.data.frame(value) && !is.matrix(value)) {
    stop(sprintf("%s must be tabular.", label), call. = FALSE)
  }
  as.data.frame(value, stringsAsFactors = FALSE, optional = TRUE)
}


.ena3d_ai_points <- function(ena_obj) {
  if (!is.list(ena_obj) || is.null(ena_obj$points)) {
    stop("The ENA object must contain a points table.", call. = FALSE)
  }
  .ena3d_ai_frame(ena_obj$points, "ENA points")
}


.ena3d_ai_resolve_axes <- function(ena_obj, settings, policy,
                                   allow_empty = FALSE) {
  points <- .ena3d_ai_points(ena_obj)
  axes <- settings$axes
  if (is.null(axes) || !length(axes)) {
    axes <- names(points)[vapply(points, function(column) {
      inherits(column, "ena.dimension")
    }, logical(1L))]
  }
  if ((is.null(axes) || !length(axes)) && !allow_empty) {
    stop("settings$axes must identify the displayed ENA axes.", call. = FALSE)
  }
  axes <- as.character(unlist(axes, recursive = TRUE, use.names = FALSE))
  axes <- axes[!is.na(axes) & nzchar(axes)]
  axes <- unique(axes)
  if (length(axes) > 3L) axes <- axes[seq_len(3L)]
  missing <- setdiff(axes, names(points))
  if (length(missing)) {
    stop(sprintf(
      "Displayed axes are missing from ENA points: %s.",
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  non_numeric <- axes[!vapply(points[axes], is.numeric, logical(1L))]
  if (length(non_numeric)) {
    stop(sprintf(
      "Displayed ENA axes must be numeric: %s.",
      paste(non_numeric, collapse = ", ")
    ), call. = FALSE)
  }
  axes
}


.ena3d_ai_number <- function(value, digits = 9L) {
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || is.na(value) || !is.finite(value)) return(NULL)
  signif(value, digits = digits)
}


.ena3d_ai_integer <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || is.na(value) || !is.finite(value) || value < 0) {
    return(NULL)
  }
  as.integer(round(value))
}


.ena3d_ai_compact <- function(value) {
  if (!is.list(value)) return(value)
  value <- lapply(value, .ena3d_ai_compact)
  value[!vapply(value, is.null, logical(1L))]
}


.ena3d_ai_axis_summaries <- function(points, rows, axes, policy) {
  lapply(Filter(Negate(is.null), lapply(axes, function(axis) {
    values <- suppressWarnings(as.numeric(points[[axis]][rows]))
    values <- values[is.finite(values)]
    if (length(values) < policy$min_cell_n) return(NULL)
    list(
      axis = ena3d_ai_clean_label(axis, policy$label_max_chars),
      mean = .ena3d_ai_number(mean(values)),
      standard_deviation = .ena3d_ai_number(stats::sd(values)),
      n_used = as.integer(length(values))
    )
  })), identity)
}


.ena3d_ai_item <- function(type, scope = list(), metrics = list()) {
  list(
    type = type,
    scope = .ena3d_ai_compact(scope),
    metrics = .ena3d_ai_compact(metrics)
  )
}


.ena3d_ai_rotation_nodes <- function(ena_obj) {
  nodes <- tryCatch(ena_obj$rotation$nodes, error = function(error) NULL)
  if (is.null(nodes) || (!is.data.frame(nodes) && !is.matrix(nodes))) {
    return(NULL)
  }
  .ena3d_ai_frame(nodes, "ENA rotation nodes")
}


.ena3d_ai_axis_anchor_items <- function(ena_obj, axes, policy) {
  nodes <- .ena3d_ai_rotation_nodes(ena_obj)
  if (is.null(nodes) || !nrow(nodes)) return(list())
  code_column <- if ("code" %in% names(nodes)) "code" else names(nodes)[[1L]]
  items <- lapply(axes, function(axis) {
    if (!axis %in% names(nodes) || !is.numeric(nodes[[axis]])) return(NULL)
    values <- suppressWarnings(as.numeric(nodes[[axis]]))
    finite <- which(is.finite(values))
    if (!length(finite)) return(NULL)
    positive <- finite[[which.max(values[finite])]]
    negative <- finite[[which.min(values[finite])]]
    .ena3d_ai_item(
      "axis_anchor",
      scope = list(
        axis = ena3d_ai_clean_label(axis, policy$label_max_chars)
      ),
      metrics = list(
        positive_code = ena3d_ai_clean_label(
          nodes[[code_column]][positive], policy$label_max_chars
        ),
        positive_coordinate = .ena3d_ai_number(values[positive]),
        negative_code = ena3d_ai_clean_label(
          nodes[[code_column]][negative], policy$label_max_chars
        ),
        negative_coordinate = .ena3d_ai_number(values[negative]),
        orientation_is_arbitrary = TRUE,
        definition = paste(
          "Codes with the most positive and negative node coordinates on",
          "the displayed axis; axis signs are not substantively fixed."
        )
      )
    )
  })
  Filter(Negate(is.null), items)
}


.ena3d_ai_adjacency_pairs <- function(ena_obj) {
  adjacency <- tryCatch(
    ena_obj$rotation$adjacency.key,
    error = function(error) NULL
  )
  if (is.null(adjacency) ||
      (!is.data.frame(adjacency) && !is.matrix(adjacency))) return(NULL)
  adjacency <- as.matrix(adjacency)
  if (nrow(adjacency) == 2L) {
    return(data.frame(
      code_a = as.character(adjacency[1L, ]),
      code_b = as.character(adjacency[2L, ]),
      stringsAsFactors = FALSE
    ))
  }
  if (ncol(adjacency) == 2L) {
    return(data.frame(
      code_a = as.character(adjacency[, 1L]),
      code_b = as.character(adjacency[, 2L]),
      stringsAsFactors = FALSE
    ))
  }
  NULL
}


.ena3d_ai_edge_columns <- function(ena_obj) {
  line_weights <- tryCatch(ena_obj$line.weights, error = function(error) NULL)
  if (is.null(line_weights) ||
      (!is.data.frame(line_weights) && !is.matrix(line_weights))) return(NULL)
  line_weights <- .ena3d_ai_frame(line_weights, "ENA line weights")
  adjacency <- .ena3d_ai_adjacency_pairs(ena_obj)

  classified <- names(line_weights)[vapply(line_weights, function(column) {
    inherits(column, c("ena.co.occurrence", "ena.connection"))
  }, logical(1L))]
  numeric_columns <- names(line_weights)[vapply(
    line_weights, is.numeric, logical(1L)
  )]
  numeric_columns <- numeric_columns[!vapply(
    line_weights[numeric_columns],
    function(column) inherits(column, "ena.metadata"), logical(1L)
  )]

  if (!is.null(adjacency) && nrow(adjacency)) {
    edge_count <- nrow(adjacency)
    columns <- if (length(classified) >= edge_count) {
      classified[seq_len(edge_count)]
    } else if (length(numeric_columns) >= edge_count) {
      tail(numeric_columns, edge_count)
    } else {
      character()
    }
    if (length(columns) == edge_count) {
      adjacency$column <- columns
      return(list(weights = line_weights, edges = adjacency))
    }
  }

  columns <- unique(c(classified, numeric_columns))
  columns <- columns[grepl("\\s&\\s|&", columns)]
  if (!length(columns)) return(NULL)
  split_names <- strsplit(columns, "\\s*&\\s*", perl = TRUE)
  usable <- lengths(split_names) >= 2L
  columns <- columns[usable]
  split_names <- split_names[usable]
  if (!length(columns)) return(NULL)
  edges <- data.frame(
    code_a = vapply(split_names, `[[`, character(1L), 1L),
    code_b = vapply(split_names, function(value) {
      paste(value[-1L], collapse = " & ")
    }, character(1L)),
    column = columns,
    stringsAsFactors = FALSE
  )
  list(weights = line_weights, edges = edges)
}


.ena3d_ai_line_rows <- function(edge_info, point_rows, group_var = NULL,
                                selected_values = NULL) {
  line_weights <- edge_info$weights
  if (!is.null(group_var) && group_var %in% names(line_weights) &&
      !is.null(selected_values)) {
    return(.ena3d_ai_group_match(
      line_weights[[group_var]], selected_values
    ))
  }
  if (length(point_rows) == nrow(line_weights)) return(as.logical(point_rows))
  rep(TRUE, nrow(line_weights))
}


.ena3d_ai_mean_edges <- function(ena_obj, point_rows, policy,
                                 group_var = NULL,
                                 selected_values = NULL,
                                 include_all = FALSE) {
  edge_info <- .ena3d_ai_edge_columns(ena_obj)
  if (is.null(edge_info)) return(list(records = list(), nonzero = 0L))
  rows <- .ena3d_ai_line_rows(
    edge_info, point_rows, group_var, selected_values
  )
  if (sum(rows) < policy$min_cell_n) {
    return(list(records = list(), nonzero = 0L))
  }

  records <- lapply(seq_len(nrow(edge_info$edges)), function(index) {
    column <- edge_info$edges$column[[index]]
    values <- suppressWarnings(as.numeric(edge_info$weights[[column]][rows]))
    values <- values[is.finite(values)]
    if (length(values) < policy$min_cell_n) return(NULL)
    list(
      code_a = edge_info$edges$code_a[[index]],
      code_b = edge_info$edges$code_b[[index]],
      mean_weight = mean(values),
      n_used = length(values)
    )
  })
  records <- Filter(Negate(is.null), records)
  if (!length(records)) return(list(records = list(), nonzero = 0L))
  weights <- vapply(records, `[[`, numeric(1L), "mean_weight")
  nonzero <- sum(weights != 0)
  ordering <- order(
    -abs(weights),
    vapply(records, `[[`, character(1L), "code_a"),
    vapply(records, `[[`, character(1L), "code_b"),
    method = "radix"
  )
  records <- records[ordering]
  if (!include_all && length(records) > policy$top_n) {
    records <- records[seq_len(policy$top_n)]
  }
  list(records = records, nonzero = as.integer(nonzero))
}


.ena3d_ai_edge_items <- function(edge_summary, scope, policy,
                                 type = "edge_weight") {
  if (!length(edge_summary$records)) return(list())
  lapply(seq_along(edge_summary$records), function(rank) {
    record <- edge_summary$records[[rank]]
    .ena3d_ai_item(
      type,
      scope = scope,
      metrics = list(
        rank_by_absolute_weight = as.integer(rank),
        code_a = ena3d_ai_clean_label(
          record$code_a, policy$label_max_chars
        ),
        code_b = ena3d_ai_clean_label(
          record$code_b, policy$label_max_chars
        ),
        mean_weight = .ena3d_ai_number(record$mean_weight),
        absolute_weight = .ena3d_ai_number(abs(record$mean_weight)),
        n_used = as.integer(record$n_used)
      )
    )
  })
}


.ena3d_ai_selected_groups <- function(points, group_var, selected_groups) {
  observed <- .ena3d_ai_observed_values(points[[group_var]])
  if (is.null(selected_groups)) return(observed)
  observed[.ena3d_ai_group_match(observed, selected_groups)]
}


.ena3d_ai_group_context <- function(group_var, groups, registry, policy) {
  list(
    group_variable = ena3d_ai_clean_label(group_var, policy$label_max_chars),
    selected_groups = as.list(.ena3d_ai_registry_labels(
      registry, groups, policy
    ))
  )
}


.ena3d_ai_build_overall <- function(ena_obj, settings, policy) {
  points <- .ena3d_ai_points(ena_obj)
  group_var <- .ena3d_ai_require_aggregate_variable(
    settings$group_var, "settings$group_var"
  )
  if (!group_var %in% names(points)) {
    stop("settings$group_var is missing from ENA points.", call. = FALSE)
  }
  axes <- .ena3d_ai_resolve_axes(ena_obj, settings, policy)
  groups <- .ena3d_ai_selected_groups(
    points, group_var, settings$selected_groups
  )
  group_counts <- vapply(groups, function(group) {
    sum(.ena3d_ai_group_match(points[[group_var]], group))
  }, integer(1L))
  safe_groups <- group_counts >= policy$min_cell_n
  suppressed <- as.integer(sum(!safe_groups))
  # Establish the disclosure-safe cohort set before constructing context or
  # any combined statistic.  This prevents a safe+singleton selection from
  # changing the aggregate and enabling subtraction attacks.
  groups <- groups[safe_groups]
  registry <- .ena3d_ai_label_registry(points[[group_var]], policy)
  selected_rows <- .ena3d_ai_group_match(points[[group_var]], groups)
  items <- if (length(groups)) {
    .ena3d_ai_axis_anchor_items(ena_obj, axes, policy)
  } else {
    list()
  }

  if (sum(selected_rows) >= policy$min_cell_n) {
    items[[length(items) + 1L]] <- .ena3d_ai_item(
      "selection_summary",
      metrics = list(
        sample_size = as.integer(sum(selected_rows)),
        selected_group_count = as.integer(length(groups)),
        aggregation = "One ENA point per unit; no unit labels included."
      )
    )
  }

  for (index in seq_along(groups)) {
    rows <- .ena3d_ai_group_match(points[[group_var]], groups[index])
    count <- sum(rows)
    items[[length(items) + 1L]] <- .ena3d_ai_item(
      "group_summary",
      scope = list(
        group = .ena3d_ai_registry_labels(registry, groups[index], policy)
      ),
      metrics = list(
        sample_size = as.integer(count),
        coordinates = .ena3d_ai_axis_summaries(
          points, rows, axes, policy
        )
      )
    )
  }

  if (sum(selected_rows) >= policy$min_cell_n) {
    edges <- .ena3d_ai_mean_edges(
      ena_obj, selected_rows, policy, group_var, groups
    )
    items <- c(items, .ena3d_ai_edge_items(
      edges,
      scope = list(selection = "All selected groups combined"),
      policy = policy
    ))
  }

  list(
    context = c(
      .ena3d_ai_group_context(group_var, groups, registry, policy),
      list(axes = as.list(unname(vapply(
        axes, ena3d_ai_clean_label, character(1L),
        max_chars = policy$label_max_chars
      ))))
    ),
    items = items,
    suppressed = suppressed
  )
}


.ena3d_ai_build_network <- function(ena_obj, settings, policy) {
  selection_type <- tolower(as.character(
    settings$selection_type %||% "none"
  )[[1L]])
  if (identical(selection_type, "unit")) {
    stop(
      "Unit-level Network selections cannot be sent for AI interpretation.",
      call. = FALSE
    )
  }
  if (!identical(selection_type, "group")) {
    stop(
      paste(
        "Select an aggregate group Network before requesting AI",
        "interpretation."
      ),
      call. = FALSE
    )
  }
  points <- .ena3d_ai_points(ena_obj)
  group_var <- .ena3d_ai_require_aggregate_variable(
    settings$group_var, "settings$group_var"
  )
  if (!group_var %in% names(points)) {
    stop("settings$group_var is missing from ENA points.", call. = FALSE)
  }
  axes <- .ena3d_ai_resolve_axes(ena_obj, settings, policy)
  groups <- .ena3d_ai_selected_groups(
    points, group_var, settings$selected_groups
  )
  group_counts <- vapply(groups, function(group) {
    sum(.ena3d_ai_group_match(points[[group_var]], group))
  }, integer(1L))
  safe_groups <- group_counts >= policy$min_cell_n
  suppressed <- as.integer(sum(!safe_groups))
  groups <- groups[safe_groups]
  registry <- .ena3d_ai_label_registry(points[[group_var]], policy)
  selected_rows <- .ena3d_ai_group_match(points[[group_var]], groups)
  count <- sum(selected_rows)
  items <- if (length(groups)) {
    .ena3d_ai_axis_anchor_items(ena_obj, axes, policy)
  } else {
    list()
  }
  if (count < policy$min_cell_n) {
    # Empty/suppressed selections intentionally yield no public evidence.
  } else {
    edge_summary <- .ena3d_ai_mean_edges(
      ena_obj, selected_rows, policy, group_var, groups
    )
    items[[length(items) + 1L]] <- .ena3d_ai_item(
      "network_summary",
      metrics = list(
        sample_size = as.integer(count),
        nonzero_edge_count = edge_summary$nonzero,
        edge_ranking = "Descending absolute mean edge weight."
      )
    )
    items <- c(items, .ena3d_ai_edge_items(
      edge_summary,
      scope = list(selection = "All selected groups combined"),
      policy = policy
    ))
  }

  list(
    context = c(
      .ena3d_ai_group_context(group_var, groups, registry, policy),
      list(
        axes = as.list(unname(vapply(
          axes, ena3d_ai_clean_label, character(1L),
          max_chars = policy$label_max_chars
        ))),
        selection_type = "aggregate_group_network"
      )
    ),
    items = items,
    suppressed = suppressed
  )
}


.ena3d_ai_comparison_groups <- function(points, group_var, settings) {
  groups <- settings$comparison_groups
  if (is.null(groups) || !length(groups)) {
    groups <- c(settings$group_a, settings$group_b)
  }
  groups <- unlist(groups, recursive = TRUE, use.names = FALSE)
  groups <- groups[!is.na(groups)]
  if (length(groups) != 2L ||
      identical(as.character(groups[[1L]]), as.character(groups[[2L]]))) {
    stop(
      "settings$comparison_groups must contain two distinct group values.",
      call. = FALSE
    )
  }
  observed <- .ena3d_ai_observed_values(points[[group_var]])
  positions <- vapply(groups, function(group) {
    match(TRUE, .ena3d_ai_group_match(observed, group), nomatch = 0L)
  }, integer(1L))
  if (any(positions == 0L)) {
    stop("A comparison group is not present in ENA points.", call. = FALSE)
  }
  observed[positions]
}


.ena3d_ai_edge_differences <- function(ena_obj, rows_a, rows_b, policy,
                                       group_var, groups) {
  edge_info <- .ena3d_ai_edge_columns(ena_obj)
  if (is.null(edge_info)) return(list())
  line_a <- .ena3d_ai_line_rows(
    edge_info, rows_a, group_var, groups[1L]
  )
  line_b <- .ena3d_ai_line_rows(
    edge_info, rows_b, group_var, groups[2L]
  )
  if (sum(line_a) < policy$min_cell_n || sum(line_b) < policy$min_cell_n) {
    return(list())
  }
  records <- lapply(seq_len(nrow(edge_info$edges)), function(index) {
    column <- edge_info$edges$column[[index]]
    values_a <- suppressWarnings(as.numeric(edge_info$weights[[column]][line_a]))
    values_b <- suppressWarnings(as.numeric(edge_info$weights[[column]][line_b]))
    values_a <- values_a[is.finite(values_a)]
    values_b <- values_b[is.finite(values_b)]
    if (length(values_a) < policy$min_cell_n ||
        length(values_b) < policy$min_cell_n) return(NULL)
    mean_a <- mean(values_a)
    mean_b <- mean(values_b)
    list(
      code_a = edge_info$edges$code_a[[index]],
      code_b = edge_info$edges$code_b[[index]],
      group_a_mean = mean_a,
      group_b_mean = mean_b,
      difference = mean_a - mean_b,
      n_a = length(values_a),
      n_b = length(values_b)
    )
  })
  records <- Filter(Negate(is.null), records)
  if (!length(records)) return(list())
  difference <- vapply(records, `[[`, numeric(1L), "difference")
  ordering <- order(
    -abs(difference),
    vapply(records, `[[`, character(1L), "code_a"),
    vapply(records, `[[`, character(1L), "code_b"),
    method = "radix"
  )
  records <- records[ordering]
  if (length(records) > policy$top_n) records <- records[seq_len(policy$top_n)]
  records
}


.ena3d_ai_build_comparison <- function(ena_obj, settings, policy) {
  points <- .ena3d_ai_points(ena_obj)
  group_var <- .ena3d_ai_require_aggregate_variable(
    settings$group_var, "settings$group_var"
  )
  if (!group_var %in% names(points)) {
    stop("settings$group_var is missing from ENA points.", call. = FALSE)
  }
  axes <- .ena3d_ai_resolve_axes(ena_obj, settings, policy)
  groups <- .ena3d_ai_comparison_groups(points, group_var, settings)
  registry <- .ena3d_ai_label_registry(points[[group_var]], policy)
  rows_a <- .ena3d_ai_group_match(points[[group_var]], groups[1L])
  rows_b <- .ena3d_ai_group_match(points[[group_var]], groups[2L])
  n_a <- sum(rows_a)
  n_b <- sum(rows_b)
  comparison_is_safe <- n_a >= policy$min_cell_n &&
    n_b >= policy$min_cell_n
  labels <- if (comparison_is_safe) {
    .ena3d_ai_registry_labels(registry, groups, policy)
  } else {
    character()
  }
  items <- if (comparison_is_safe) {
    .ena3d_ai_axis_anchor_items(ena_obj, axes, policy)
  } else {
    list()
  }
  suppressed <- as.integer(sum(c(n_a, n_b) < policy$min_cell_n))

  if (comparison_is_safe) {
    items[[length(items) + 1L]] <- .ena3d_ai_item(
      "comparison_sample",
      scope = list(group_a = labels[[1L]], group_b = labels[[2L]]),
      metrics = list(
        group_a_n = as.integer(n_a),
        group_b_n = as.integer(n_b),
        direction = "group_a minus group_b"
      )
    )
    axis_differences <- lapply(axes, function(axis) {
      a <- suppressWarnings(as.numeric(points[[axis]][rows_a]))
      b <- suppressWarnings(as.numeric(points[[axis]][rows_b]))
      a <- a[is.finite(a)]
      b <- b[is.finite(b)]
      if (length(a) < policy$min_cell_n ||
          length(b) < policy$min_cell_n) return(NULL)
      list(
        axis = ena3d_ai_clean_label(axis, policy$label_max_chars),
        group_a_mean = .ena3d_ai_number(mean(a)),
        group_b_mean = .ena3d_ai_number(mean(b)),
        difference = .ena3d_ai_number(mean(a) - mean(b)),
        group_a_standard_deviation = .ena3d_ai_number(stats::sd(a)),
        group_b_standard_deviation = .ena3d_ai_number(stats::sd(b)),
        group_a_n = as.integer(length(a)),
        group_b_n = as.integer(length(b))
      )
    })
    axis_differences <- Filter(Negate(is.null), axis_differences)
    if (length(axis_differences)) {
      items[[length(items) + 1L]] <- .ena3d_ai_item(
        "centroid_difference",
        scope = list(group_a = labels[[1L]], group_b = labels[[2L]]),
        metrics = list(
          direction = "group_a minus group_b",
          coordinates = axis_differences
        )
      )
    }

    edge_differences <- .ena3d_ai_edge_differences(
      ena_obj, rows_a, rows_b, policy, group_var, groups
    )
    if (length(edge_differences)) {
      edge_items <- lapply(seq_along(edge_differences), function(rank) {
        record <- edge_differences[[rank]]
        .ena3d_ai_item(
          "edge_difference",
          scope = list(group_a = labels[[1L]], group_b = labels[[2L]]),
          metrics = list(
            rank_by_absolute_difference = as.integer(rank),
            code_a = ena3d_ai_clean_label(
              record$code_a, policy$label_max_chars
            ),
            code_b = ena3d_ai_clean_label(
              record$code_b, policy$label_max_chars
            ),
            group_a_mean = .ena3d_ai_number(record$group_a_mean),
            group_b_mean = .ena3d_ai_number(record$group_b_mean),
            difference = .ena3d_ai_number(record$difference),
            group_a_n = as.integer(record$n_a),
            group_b_n = as.integer(record$n_b),
            direction = "group_a minus group_b"
          )
        )
      })
      items <- c(items, edge_items)
    }
  }

  list(
    context = list(
      group_variable = ena3d_ai_clean_label(
        group_var, policy$label_max_chars
      ),
      group_a = if (comparison_is_safe) labels[[1L]] else NULL,
      group_b = if (comparison_is_safe) labels[[2L]] else NULL,
      difference_direction = "group_a minus group_b",
      axes = as.list(unname(vapply(
        axes, ena3d_ai_clean_label, character(1L),
        max_chars = policy$label_max_chars
      )))
    ),
    items = items,
    suppressed = suppressed
  )
}


.ena3d_ai_ordered_change_values <- function(values, requested = NULL) {
  observed <- .ena3d_ai_observed_values(values)
  if (!is.null(requested)) {
    requested <- unlist(requested, recursive = TRUE, use.names = FALSE)
    positions <- vapply(requested, function(value) {
      match(TRUE, .ena3d_ai_group_match(observed, value), nomatch = 0L)
    }, integer(1L))
    return(observed[positions[positions > 0L]])
  }
  if (is.factor(values)) {
    levels <- levels(droplevels(values))
    return(observed[match(levels, as.character(observed), nomatch = 0L)])
  }
  if (is.numeric(values) || inherits(values, c("Date", "POSIXt", "difftime"))) {
    return(observed[order(as.numeric(observed), method = "radix")])
  }
  observed
}


.ena3d_ai_change_edge_records <- function(ena_obj, points, change_var,
                                          change_values, valid_cells,
                                          registry, policy) {
  if (length(change_values) < 2L) return(list())
  all_records <- list()
  for (index in seq_len(length(change_values) - 1L)) {
    if (!valid_cells[[index]] || !valid_cells[[index + 1L]]) next
    first <- change_values[index]
    second <- change_values[index + 1L]
    rows_a <- .ena3d_ai_group_match(points[[change_var]], first)
    rows_b <- .ena3d_ai_group_match(points[[change_var]], second)
    records <- .ena3d_ai_edge_differences(
      ena_obj, rows_b, rows_a, policy, change_var, c(second, first)
    )
    if (!length(records)) next
    for (record in records) {
      record$from <- .ena3d_ai_registry_labels(registry, first, policy)
      record$to <- .ena3d_ai_registry_labels(registry, second, policy)
      all_records[[length(all_records) + 1L]] <- record
    }
  }
  if (!length(all_records)) return(list())
  differences <- vapply(all_records, `[[`, numeric(1L), "difference")
  ordering <- order(-abs(differences), method = "radix")
  all_records <- all_records[ordering]
  if (length(all_records) > policy$top_n) {
    all_records <- all_records[seq_len(policy$top_n)]
  }
  all_records
}


.ena3d_ai_build_change <- function(ena_obj, settings, policy) {
  points <- .ena3d_ai_points(ena_obj)
  change_var <- .ena3d_ai_require_aggregate_variable(
    settings$change_var, "settings$change_var"
  )
  if (!change_var %in% names(points)) {
    stop("settings$change_var is missing from ENA points.", call. = FALSE)
  }
  axes <- .ena3d_ai_resolve_axes(ena_obj, settings, policy)
  values <- .ena3d_ai_ordered_change_values(
    points[[change_var]], settings$change_values
  )
  registry <- .ena3d_ai_label_registry(points[[change_var]], policy)
  available_count <- length(values)
  truncated <- available_count > policy$max_slices
  if (truncated) values <- values[seq_len(policy$max_slices)]
  labels <- .ena3d_ai_registry_labels(registry, values, policy)
  counts <- vapply(values, function(value) {
    sum(.ena3d_ai_group_match(points[[change_var]], value))
  }, integer(1L))
  valid_cells <- counts >= policy$min_cell_n
  suppressed <- sum(!valid_cells)
  safe_labels <- labels[valid_cells]
  items <- if (any(valid_cells)) {
    .ena3d_ai_axis_anchor_items(ena_obj, axes, policy)
  } else {
    list()
  }

  for (index in seq_along(values)) {
    if (!valid_cells[[index]]) next
    rows <- .ena3d_ai_group_match(points[[change_var]], values[index])
    items[[length(items) + 1L]] <- .ena3d_ai_item(
      "change_slice",
      scope = list(change_value = labels[[index]]),
      metrics = list(
        sample_size = as.integer(counts[[index]]),
        coordinates = .ena3d_ai_axis_summaries(
          points, rows, axes, policy
        )
      )
    )
  }

  if (length(values) >= 2L) {
    for (index in seq_len(length(values) - 1L)) {
      if (!valid_cells[[index]] || !valid_cells[[index + 1L]]) next
      from_rows <- .ena3d_ai_group_match(points[[change_var]], values[index])
      to_rows <- .ena3d_ai_group_match(points[[change_var]], values[index + 1L])
      changes <- Filter(Negate(is.null), lapply(axes, function(axis) {
        first <- suppressWarnings(as.numeric(points[[axis]][from_rows]))
        second <- suppressWarnings(as.numeric(points[[axis]][to_rows]))
        first <- first[is.finite(first)]
        second <- second[is.finite(second)]
        if (length(first) < policy$min_cell_n ||
            length(second) < policy$min_cell_n) return(NULL)
        list(
          axis = ena3d_ai_clean_label(axis, policy$label_max_chars),
          from_mean = .ena3d_ai_number(mean(first)),
          to_mean = .ena3d_ai_number(mean(second)),
          difference = .ena3d_ai_number(mean(second) - mean(first))
        )
      }))
      if (length(changes)) {
        items[[length(items) + 1L]] <- .ena3d_ai_item(
          "change_centroid_step",
          scope = list(from = labels[[index]], to = labels[[index + 1L]]),
          metrics = list(
            direction = "to minus from",
            coordinates = changes
          )
        )
      }
    }
  }

  edge_records <- .ena3d_ai_change_edge_records(
    ena_obj, points, change_var, values, valid_cells, registry, policy
  )
  if (length(edge_records)) {
    edge_items <- lapply(seq_along(edge_records), function(rank) {
      record <- edge_records[[rank]]
      .ena3d_ai_item(
        "change_edge_step",
        scope = list(from = record$from, to = record$to),
        metrics = list(
          rank_by_absolute_change = as.integer(rank),
          code_a = ena3d_ai_clean_label(
            record$code_a, policy$label_max_chars
          ),
          code_b = ena3d_ai_clean_label(
            record$code_b, policy$label_max_chars
          ),
          from_mean = .ena3d_ai_number(record$group_b_mean),
          to_mean = .ena3d_ai_number(record$group_a_mean),
          difference = .ena3d_ai_number(record$difference),
          direction = "to minus from"
        )
      )
    })
    items <- c(items, edge_items)
  }

  list(
    context = list(
      change_variable = ena3d_ai_clean_label(
        change_var, policy$label_max_chars
      ),
      ordered_values = as.list(safe_labels),
      available_slice_count = as.integer(available_count),
      slice_limit_applied = isTRUE(truncated),
      axes = as.list(unname(vapply(
        axes, ena3d_ai_clean_label, character(1L),
        max_chars = policy$label_max_chars
      )))
    ),
    items = items,
    suppressed = as.integer(suppressed)
  )
}


.ena3d_ai_summary_value <- function(summary, statistic, column_index) {
  if (!is.data.frame(summary) || ncol(summary) < column_index ||
      !nrow(summary)) return(NULL)
  labels <- tolower(trimws(as.character(summary[[1L]])))
  position <- match(tolower(statistic), labels)
  if (is.na(position)) return(NULL)
  .ena3d_ai_number(summary[[column_index]][[position]])
}


.ena3d_ai_stats_counts <- function(result, design) {
  if (!is.list(result)) return(list(n_a = NULL, n_b = NULL, n_pairs = NULL))
  summary <- result$summary
  n_a <- .ena3d_ai_integer(
    result$n_group1 %||% result$n_a %||% result$valid_n_group1
  )
  n_b <- .ena3d_ai_integer(
    result$n_group2 %||% result$n_b %||% result$valid_n_group2
  )
  n_pairs <- .ena3d_ai_integer(
    result$n_pairs %||% tryCatch(result$pairs$n_pairs, error = function(e) NULL)
  )
  if (is.null(n_a)) n_a <- .ena3d_ai_integer(
    .ena3d_ai_summary_value(summary, "valid n", 2L)
  )
  if (is.null(n_b)) n_b <- .ena3d_ai_integer(
    .ena3d_ai_summary_value(summary, "valid n", 3L)
  )
  if (identical(design, "paired") && !is.null(n_pairs)) {
    n_a <- n_pairs
    n_b <- n_pairs
  }
  list(n_a = n_a, n_b = n_b, n_pairs = n_pairs)
}


# A small local infix keeps this standalone file independent of app_utils.R.
`%||%` <- function(left, right) {
  if (is.null(left) || !length(left)) right else left
}


.ena3d_ai_stats_axis_results <- function(stats_result, axes) {
  if (is.null(stats_result) || !is.list(stats_result)) return(list())
  candidates <- stats_result$results %||% stats_result$axes %||% stats_result
  if (!is.list(candidates) || is.data.frame(candidates)) return(list())
  output <- vector("list", length(axes))
  names(output) <- axes
  candidate_names <- names(candidates)
  for (index in seq_along(axes)) {
    axis <- axes[[index]]
    result <- NULL
    if (!is.null(candidate_names) && axis %in% candidate_names) {
      result <- candidates[[axis]]
    } else if (!is.null(candidate_names)) {
      position <- match(tolower(axis), tolower(candidate_names))
      if (!is.na(position)) result <- candidates[[position]]
    } else if (length(candidates) >= index) {
      result <- candidates[[index]]
    }
    output[[index]] <- result
  }
  output
}


.ena3d_ai_build_stats <- function(ena_obj, settings, policy, stats_result) {
  points <- .ena3d_ai_points(ena_obj)
  axes <- .ena3d_ai_resolve_axes(ena_obj, settings, policy)
  group_var <- .ena3d_ai_require_aggregate_variable(
    settings$group_var, "settings$group_var"
  )
  if (!group_var %in% names(points)) {
    stop("settings$group_var is missing from ENA points.", call. = FALSE)
  }
  groups <- .ena3d_ai_comparison_groups(points, group_var, settings)
  registry <- .ena3d_ai_label_registry(points[[group_var]], policy)
  actual_group_counts <- vapply(groups, function(group) {
    sum(.ena3d_ai_group_match(points[[group_var]], group))
  }, integer(1L))
  comparison_is_safe <- all(actual_group_counts >= policy$min_cell_n)
  labels <- if (comparison_is_safe) {
    .ena3d_ai_registry_labels(registry, groups, policy)
  } else {
    character()
  }
  design <- tolower(as.character(settings$stats_design %||% "unpaired")[[1L]])
  if (!design %in% c("paired", "unpaired")) design <- "unpaired"
  adjustment <- ena3d_ai_clean_label(
    settings$p_adjust_method %||% "none", policy$label_max_chars
  )
  alternative <- ena3d_ai_clean_label(
    settings$alternative %||% "two.sided", policy$label_max_chars
  )
  results <- .ena3d_ai_stats_axis_results(stats_result, axes)
  items <- list()
  suppressed <- if (comparison_is_safe) 0L else as.integer(length(axes))

  for (index in seq_along(axes)) {
    if (!comparison_is_safe) break
    result <- results[[index]]
    if (inherits(result, "error") || !is.list(result)) next
    counts <- .ena3d_ai_stats_counts(result, design)
    count_is_safe <- if (identical(design, "paired")) {
      !is.null(counts$n_pairs) && counts$n_pairs >= policy$min_cell_n
    } else {
      !is.null(counts$n_a) && !is.null(counts$n_b) &&
        counts$n_a >= policy$min_cell_n && counts$n_b >= policy$min_cell_n
    }
    if (!count_is_safe) {
      suppressed <- suppressed + 1L
      next
    }
    confidence <- suppressWarnings(as.numeric(result$conf))
    confidence <- confidence[is.finite(confidence)]
    confidence <- if (length(confidence) >= 2L) {
      list(
        lower = .ena3d_ai_number(confidence[[1L]]),
        upper = .ena3d_ai_number(confidence[[2L]]),
        level = .ena3d_ai_number(result$conf_level)
      )
    } else NULL
    summary <- result$summary
    center_name <- if (!is.null(.ena3d_ai_summary_value(
      summary, "mean", 2L
    ))) "mean" else "median"
    group_a_center <- .ena3d_ai_summary_value(summary, center_name, 2L)
    group_b_center <- .ena3d_ai_summary_value(summary, center_name, 3L)
    test_name <- result$test_type %||% result$method %||% "Not specified"
    adjusted_p <- result$p_adjusted %||% result$adjusted_p
    metrics <- list(
      design = design,
      test = ena3d_ai_clean_text(test_name, policy$text_max_chars),
      statistic = .ena3d_ai_number(result$statistic),
      effect_size = .ena3d_ai_number(result$effect_size),
      p_value = .ena3d_ai_number(result$p_value),
      adjusted_p_value = .ena3d_ai_number(adjusted_p),
      adjustment_method = adjustment,
      alternative = ena3d_ai_clean_label(
        result$alternative %||% alternative, policy$label_max_chars
      ),
      group_a_n = counts$n_a,
      group_b_n = counts$n_b,
      matched_pair_n = counts$n_pairs,
      center_statistic = if (!is.null(group_a_center) ||
                             !is.null(group_b_center)) center_name else NULL,
      group_a_center = group_a_center,
      group_b_center = group_b_center,
      group_a_standard_deviation = .ena3d_ai_summary_value(
        summary, "std.", 2L
      ),
      group_b_standard_deviation = .ena3d_ai_summary_value(
        summary, "std.", 3L
      ),
      confidence_interval = confidence,
      status = if (!is.null(result$status)) {
        ena3d_ai_clean_text(result$status, policy$text_max_chars)
      } else NULL,
      direction = "group_a minus group_b"
    )
    items[[length(items) + 1L]] <- .ena3d_ai_item(
      "inference_result",
      scope = list(
        axis = ena3d_ai_clean_label(axes[[index]], policy$label_max_chars),
        group_a = labels[[1L]],
        group_b = labels[[2L]]
      ),
      metrics = metrics
    )
  }

  list(
    context = list(
      group_variable = ena3d_ai_clean_label(
        group_var, policy$label_max_chars
      ),
      group_a = if (comparison_is_safe) labels[[1L]] else NULL,
      group_b = if (comparison_is_safe) labels[[2L]] else NULL,
      design = design,
      p_adjustment_method = adjustment,
      alternative = alternative,
      axes = as.list(unname(vapply(
        axes, ena3d_ai_clean_label, character(1L),
        max_chars = policy$label_max_chars
      )))
    ),
    items = items,
    suppressed = suppressed
  )
}


.ena3d_ai_trajectory_axes <- function(result, settings, path, policy) {
  axes <- settings$axes %||% tryCatch(
    result$settings$dimensions, error = function(error) NULL
  )
  if (is.null(axes) || !length(axes)) {
    axes <- sub("^centroid_", "", grep(
      "^centroid_", names(path), value = TRUE
    ))
  }
  axes <- unique(as.character(unlist(axes, recursive = TRUE, use.names = FALSE)))
  axes <- axes[!is.na(axes) & nzchar(axes)]
  if (length(axes) > 3L) axes <- axes[seq_len(3L)]
  axes[paste0("centroid_", axes) %in% names(path)]
}


.ena3d_ai_trajectory_metric <- function(frame, row, name) {
  if (is.null(frame) || !is.data.frame(frame) || !name %in% names(frame) ||
      row > nrow(frame)) return(NULL)
  .ena3d_ai_number(frame[[name]][[row]])
}


.ena3d_ai_trajectory_slice_scope <- function(path, row, group_var, time_var,
                                             group_registry, time_registry,
                                             policy) {
  scope <- list()
  if (!is.null(group_var) && group_var %in% names(path)) {
    scope$group <- .ena3d_ai_registry_labels(
      group_registry, path[[group_var]][row], policy
    )
  } else {
    scope$group <- "All"
  }
  if (!is.null(time_var) && time_var %in% names(path)) {
    scope$time <- .ena3d_ai_registry_labels(
      time_registry, path[[time_var]][row], policy
    )
  } else if ("time_value" %in% names(path)) {
    scope$time <- .ena3d_ai_registry_labels(
      time_registry, path$time_value[row], policy
    )
  }
  if ("time_order" %in% names(path)) {
    scope$time_order <- .ena3d_ai_integer(path$time_order[[row]])
  }
  scope
}


.ena3d_ai_build_trajectory <- function(ena_obj, settings, policy,
                                       trajectory_result) {
  result <- trajectory_result
  if (is.list(result) && is.null(result$path) && is.list(result$result)) {
    result <- result$result
  }
  if (!is.list(result) || !is.data.frame(result$path)) {
    stop(
      "trajectory_result must contain an aggregate path data frame.",
      call. = FALSE
    )
  }
  path <- as.data.frame(result$path, stringsAsFactors = FALSE, optional = TRUE)
  axes <- .ena3d_ai_trajectory_axes(result, settings, path, policy)
  if (!length(axes)) {
    stop("The trajectory path has no requested centroid axes.", call. = FALSE)
  }
  group_var <- settings$trajectory_group_var %||% tryCatch(
    result$settings$group_var, error = function(error) NULL
  )
  if (!is.null(group_var)) {
    group_var <- as.character(group_var[[1L]])
    if (!nzchar(group_var) || !group_var %in% names(path) ||
        .ena3d_ai_is_identifier_name(group_var)) group_var <- NULL
  }
  time_var <- settings$trajectory_time_var %||% tryCatch(
    result$settings$time_var, error = function(error) NULL
  )
  if (!is.null(time_var)) {
    time_var <- as.character(time_var[[1L]])
    if (!nzchar(time_var) || !time_var %in% names(path)) time_var <- NULL
  }
  if (is.null(time_var) && "time_value" %in% names(path)) time_var <- "time_value"
  group_registry <- if (!is.null(group_var)) {
    .ena3d_ai_label_registry(path[[group_var]], policy)
  } else .ena3d_ai_label_registry("All", policy)
  time_registry <- if (!is.null(time_var)) {
    .ena3d_ai_label_registry(path[[time_var]], policy)
  } else .ena3d_ai_label_registry(character(), policy)
  bootstrap <- if (is.data.frame(result$bootstrap)) {
    as.data.frame(result$bootstrap, stringsAsFactors = FALSE, optional = TRUE)
  } else path
  available_count <- nrow(path)
  row_limit <- min(available_count, policy$max_slices)
  rows <- if (row_limit) seq_len(row_limit) else integer()
  items <- .ena3d_ai_axis_anchor_items(ena_obj, axes, policy)
  suppressed <- 0L

  for (row in rows) {
    n_used <- if ("n_used" %in% names(path)) {
      .ena3d_ai_integer(path$n_used[[row]])
    } else NULL
    if (is.null(n_used) || n_used < policy$min_cell_n) {
      suppressed <- suppressed + 1L
      next
    }
    coordinates <- lapply(axes, function(axis) {
      metric <- paste0("centroid_", axis)
      list(
        axis = ena3d_ai_clean_label(axis, policy$label_max_chars),
        centroid = .ena3d_ai_trajectory_metric(path, row, metric),
        lower = .ena3d_ai_trajectory_metric(
          bootstrap, row, paste0(metric, "_lower")
        ),
        upper = .ena3d_ai_trajectory_metric(
          bootstrap, row, paste0(metric, "_upper")
        )
      )
    })
    coordinates <- lapply(coordinates, .ena3d_ai_compact)
    items[[length(items) + 1L]] <- .ena3d_ai_item(
      "trajectory_slice",
      scope = .ena3d_ai_trajectory_slice_scope(
        path, row, group_var, time_var, group_registry, time_registry, policy
      ),
      metrics = list(
        sample_size = n_used,
        coordinates = coordinates,
        step_distance = .ena3d_ai_trajectory_metric(
          path, row, "step_distance"
        ),
        elapsed_interval = .ena3d_ai_trajectory_metric(
          path, row, "elapsed_interval"
        ),
        speed = .ena3d_ai_trajectory_metric(path, row, "speed"),
        cumulative_distance = .ena3d_ai_trajectory_metric(
          path, row, "cumulative_distance"
        )
      )
    )
  }

  comparison <- result$comparison
  if (is.data.frame(comparison) && nrow(comparison)) {
    comparison <- as.data.frame(
      comparison, stringsAsFactors = FALSE, optional = TRUE
    )
    comparison_rows <- seq_len(min(nrow(comparison), policy$max_slices))
    for (row in comparison_rows) {
      n_used <- .ena3d_ai_integer(
        if ("n_used" %in% names(comparison)) comparison$n_used[[row]] else
          if ("n_matched" %in% names(comparison)) comparison$n_matched[[row]] else
            if (all(c("n_a_used", "n_b_used") %in% names(comparison))) {
              min(comparison$n_a_used[[row]], comparison$n_b_used[[row]])
            } else NULL
      )
      if (is.null(n_used) || n_used < policy$min_cell_n) {
        suppressed <- suppressed + 1L
        next
      }
      differences <- Filter(Negate(is.null), lapply(axes, function(axis) {
        difference_name <- paste0("difference_", axis)
        difference <- .ena3d_ai_trajectory_metric(
          comparison, row, difference_name
        )
        if (is.null(difference)) return(NULL)
        list(
          axis = ena3d_ai_clean_label(axis, policy$label_max_chars),
          difference = difference,
          lower = .ena3d_ai_trajectory_metric(
            comparison, row, paste0(difference_name, "_lower")
          ),
          upper = .ena3d_ai_trajectory_metric(
            comparison, row, paste0(difference_name, "_upper")
          ),
          p_value = .ena3d_ai_trajectory_metric(
            comparison, row, paste0(difference_name, "_p_value")
          ),
          adjusted_p_value = .ena3d_ai_trajectory_metric(
            comparison, row, paste0(difference_name, "_p_adjusted")
          )
        )
      }))
      if (!length(differences)) next
      items[[length(items) + 1L]] <- .ena3d_ai_item(
        "trajectory_comparison",
        scope = .ena3d_ai_trajectory_slice_scope(
          comparison, row, group_var, time_var,
          group_registry, time_registry, policy
        ),
        metrics = list(
          matched_sample_size = n_used,
          differences = lapply(differences, .ena3d_ai_compact),
          difference_direction = ena3d_ai_clean_text(
            attr(result$comparison, "comparison_spec")$difference_direction %||%
              "side_b minus side_a",
            policy$text_max_chars
          ),
          centroid_difference_distance = .ena3d_ai_trajectory_metric(
            comparison, row, "centroid_difference_distance"
          )
        )
      )
    }
  }

  # Diagnostics intentionally remain local.  Their public shape does not carry
  # a consistently verifiable aggregate denominator and messages/scopes can
  # contain identifier-like or small-cell labels.  They therefore cannot form
  # AI evidence, even when every substantive trajectory slice is suppressed.

  result_settings <- result$settings %||% list()
  list(
    context = list(
      axes = as.list(unname(vapply(
        axes, ena3d_ai_clean_label, character(1L),
        max_chars = policy$label_max_chars
      ))),
      group_variable = if (!is.null(group_var)) {
        ena3d_ai_clean_label(group_var, policy$label_max_chars)
      } else NULL,
      time_variable = if (!is.null(time_var)) {
        ena3d_ai_clean_label(time_var, policy$label_max_chars)
      } else NULL,
      distance_space = ena3d_ai_clean_label(
        result_settings$distance_space %||% "not specified",
        policy$label_max_chars
      ),
      cohort_policy = ena3d_ai_clean_label(
        result_settings$cohort_policy %||% "not specified",
        policy$label_max_chars
      ),
      missing_value_policy = ena3d_ai_clean_label(
        result_settings$na_policy %||% "not specified",
        policy$label_max_chars
      ),
      available_slice_count = as.integer(available_count),
      slice_limit_applied = available_count > policy$max_slices
    ),
    items = items,
    suppressed = as.integer(suppressed)
  )
}


ena3d_ai_data_fingerprint <- function(ena_obj) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("The digest package is required for AI evidence fingerprints.",
         call. = FALSE)
  }
  # Hash the validated analytical source to detect staleness, but disclose only
  # the one-way digest.  No source values are placed in the ledger.
  source <- list(
    points = tryCatch(ena_obj$points, error = function(error) NULL),
    line_weights = tryCatch(ena_obj$line.weights, error = function(error) NULL),
    rotation_nodes = tryCatch(
      ena_obj$rotation$nodes, error = function(error) NULL
    ),
    adjacency_key = tryCatch(
      ena_obj$rotation$adjacency.key, error = function(error) NULL
    ),
    rotation_matrix = tryCatch(
      ena_obj$rotation$rotation.matrix, error = function(error) NULL
    )
  )
  digest::digest(source, algo = "sha256", serialize = TRUE)
}


.ena3d_ai_request_fingerprint <- function(value) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("The digest package is required for AI evidence fingerprints.",
         call. = FALSE)
  }
  digest::digest(value, algo = "sha256", serialize = TRUE)
}


.ena3d_ai_validate_character <- function(value, max_chars, path) {
  if (!length(value)) return(invisible(TRUE))
  if (any(is.na(value))) {
    stop(sprintf("%s contains a missing character value.", path), call. = FALSE)
  }
  valid <- !is.na(iconv(value, from = "UTF-8", to = "UTF-8", sub = NA))
  if (!all(valid)) {
    stop(sprintf("%s contains invalid UTF-8.", path), call. = FALSE)
  }
  if (any(nchar(value, type = "chars") > max_chars)) {
    stop(sprintf("%s exceeds the character limit.", path), call. = FALSE)
  }
  if (any(grepl("[[:cntrl:]]", value, perl = TRUE))) {
    stop(sprintf("%s contains control characters.", path), call. = FALSE)
  }
  invisible(TRUE)
}


.ena3d_ai_validate_tree <- function(value, policy, path = "value", depth = 0L) {
  if (depth > 8L) {
    stop(sprintf("%s exceeds the nesting limit.", path), call. = FALSE)
  }
  if (is.data.frame(value) || is.matrix(value) || is.environment(value) ||
      is.function(value)) {
    stop(sprintf("%s contains a forbidden row-level object.", path),
         call. = FALSE)
  }
  if (is.list(value)) {
    if (length(value) > policy$max_evidence) {
      stop(sprintf("%s exceeds the list-length limit.", path), call. = FALSE)
    }
    value_names <- names(value)
    if (!is.null(value_names)) {
      if (any(!nzchar(value_names)) || anyDuplicated(value_names)) {
        stop(sprintf("%s must have unique non-empty field names.", path),
             call. = FALSE)
      }
      forbidden <- grepl(
        paste0(
          "(^|_)(ena_?unit|unit_?id|participant_?id|respondent_?id|",
          "subject_?id|person_?id|user_?id|raw_?data|raw_?rows?)($|_)"
        ),
        tolower(value_names), perl = TRUE
      )
      if (any(forbidden)) {
        stop(sprintf("%s contains a prohibited unit-level field.", path),
             call. = FALSE)
      }
    }
    for (index in seq_along(value)) {
      child <- if (!is.null(value_names)) value_names[[index]] else index
      .ena3d_ai_validate_tree(
        value[[index]], policy, paste0(path, "$", child), depth + 1L
      )
    }
    return(invisible(TRUE))
  }
  if (is.null(value)) return(invisible(TRUE))
  if (!is.atomic(value) || length(value) > policy$top_n) {
    stop(sprintf("%s contains an oversized atomic vector.", path),
         call. = FALSE)
  }
  if (is.character(value)) {
    .ena3d_ai_validate_character(value, policy$text_max_chars, path)
  } else if (is.numeric(value)) {
    if (any(!is.finite(value))) {
      stop(sprintf("%s contains a non-finite number.", path), call. = FALSE)
    }
  } else if (!is.logical(value) && !is.integer(value)) {
    stop(sprintf("%s contains an unsupported atomic type.", path),
         call. = FALSE)
  }
  invisible(TRUE)
}


ena3d_ai_validate_ledger <- function(ledger, max_evidence = 96L,
                                     text_max_chars = 500L) {
  if (!is.list(ledger)) stop("AI evidence ledger must be a list.", call. = FALSE)
  required <- c(
    "schema_version", "view", "data_fingerprint", "request_fingerprint",
    "privacy", "context", "evidence"
  )
  if (!identical(names(ledger), required)) {
    stop("AI evidence ledger has an unexpected top-level contract.",
         call. = FALSE)
  }
  if (!identical(ledger$schema_version, 1L)) {
    stop("AI evidence schema_version must be 1.", call. = FALSE)
  }
  allowed_views <- c(
    "overall", "network", "comparison", "change", "stats", "trajectory"
  )
  if (!is.character(ledger$view) || length(ledger$view) != 1L ||
      !ledger$view %in% allowed_views) {
    stop("AI evidence view is invalid.", call. = FALSE)
  }
  for (field in c("data_fingerprint", "request_fingerprint")) {
    if (!is.character(ledger[[field]]) || length(ledger[[field]]) != 1L ||
        !grepl("^[0-9a-f]{64}$", ledger[[field]])) {
      stop(sprintf("AI evidence %s is invalid.", field), call. = FALSE)
    }
  }
  if (!is.list(ledger$privacy) ||
      !identical(ledger$privacy$aggregation_only, TRUE) ||
      !identical(ledger$privacy$unit_level_data_included, FALSE) ||
      !identical(ledger$privacy$raw_rows_included, FALSE)) {
    stop("AI evidence privacy declaration is invalid.", call. = FALSE)
  }
  if (!is.list(ledger$evidence) || length(ledger$evidence) > max_evidence) {
    stop("AI evidence item count exceeds the contract.", call. = FALSE)
  }
  expected_ids <- if (length(ledger$evidence)) {
    paste0("E", seq_along(ledger$evidence))
  } else {
    character()
  }
  ids <- vapply(ledger$evidence, function(item) item$id %||% "", character(1L))
  if (!identical(ids, expected_ids) || anyDuplicated(ids)) {
    stop("AI evidence IDs must be consecutive and unique.", call. = FALSE)
  }
  for (index in seq_along(ledger$evidence)) {
    item <- ledger$evidence[[index]]
    if (!is.list(item) ||
        !identical(names(item), c("id", "type", "scope", "metrics")) ||
        !is.character(item$type) || length(item$type) != 1L ||
        !grepl("^[a-z][a-z0-9_]{0,63}$", item$type)) {
      stop(sprintf("AI evidence item E%d is invalid.", index), call. = FALSE)
    }
  }
  policy <- ena3d_ai_evidence_policy(
    min_cell_n = max(2L, as.integer(ledger$privacy$min_cell_n)),
    top_n = max(1L, min(25L, as.integer(ledger$privacy$top_n))),
    max_slices = max(1L, min(30L, as.integer(
      ledger$privacy$max_slices
    ))),
    # Nested aggregate records can legitimately have more fields than a very
    # small caller-selected evidence-item cap.  The evidence count itself was
    # checked above; keep a separate conservative structural list bound here.
    max_evidence = max(32L, min(96L, as.integer(max_evidence))),
    label_max_chars = 160L,
    text_max_chars = max(40L, min(500L, as.integer(text_max_chars)))
  )
  # Privacy contains intentional declarations named raw_rows_included; validate
  # the constructed context/evidence trees, where such fields are prohibited.
  .ena3d_ai_validate_tree(ledger$context, policy, "context")
  .ena3d_ai_validate_tree(ledger$evidence, policy, "evidence")
  invisible(ledger)
}


# Build a deterministic, bounded, aggregate-only evidence ledger.
ena3d_ai_build_evidence <- function(
    ena_obj,
    view = c("overall", "network", "comparison", "change", "stats",
             "trajectory"),
    settings = list(),
    stats_result = NULL,
    trajectory_result = NULL,
    min_cell_n = 5L,
    top_n = 10L,
    max_slices = 20L,
    max_evidence = 64L
) {
  view <- match.arg(view)
  if (!is.list(settings)) {
    stop("settings must be a named list.", call. = FALSE)
  }
  if (length(settings) &&
      (is.null(names(settings)) || any(!nzchar(names(settings))))) {
    stop("settings must be a named list.", call. = FALSE)
  }
  policy <- ena3d_ai_evidence_policy(
    min_cell_n = min_cell_n,
    top_n = top_n,
    max_slices = max_slices,
    max_evidence = max_evidence
  )
  built <- switch(
    view,
    overall = .ena3d_ai_build_overall(ena_obj, settings, policy),
    network = .ena3d_ai_build_network(ena_obj, settings, policy),
    comparison = .ena3d_ai_build_comparison(ena_obj, settings, policy),
    change = .ena3d_ai_build_change(ena_obj, settings, policy),
    stats = .ena3d_ai_build_stats(
      ena_obj, settings, policy, stats_result
    ),
    trajectory = .ena3d_ai_build_trajectory(
      ena_obj, settings, policy, trajectory_result
    )
  )
  items <- built$items
  truncated_items <- max(0L, length(items) - policy$max_evidence)
  if (truncated_items) items <- items[seq_len(policy$max_evidence)]
  items <- lapply(seq_along(items), function(index) {
    c(
      list(id = paste0("E", index)),
      items[[index]][c("type", "scope", "metrics")]
    )
  })
  privacy <- list(
    aggregation_only = TRUE,
    min_cell_n = policy$min_cell_n,
    small_cells_suppressed = as.integer(built$suppressed),
    top_n = policy$top_n,
    max_slices = policy$max_slices,
    evidence_items_truncated = as.integer(truncated_items),
    unit_level_data_included = FALSE,
    raw_rows_included = FALSE,
    label_handling = paste(
      "Untrusted labels are valid UTF-8, control-stripped, delimiter-neutral,",
      "collision-disambiguated, and length-bounded."
    )
  )
  partial <- list(
    schema_version = 1L,
    view = view,
    data_fingerprint = ena3d_ai_data_fingerprint(ena_obj),
    privacy = privacy,
    context = .ena3d_ai_compact(built$context),
    evidence = items
  )
  ledger <- list(
    schema_version = partial$schema_version,
    view = partial$view,
    data_fingerprint = partial$data_fingerprint,
    request_fingerprint = .ena3d_ai_request_fingerprint(partial),
    privacy = partial$privacy,
    context = partial$context,
    evidence = partial$evidence
  )
  class(ledger) <- c("ena3d_ai_evidence_ledger", "list")
  ena3d_ai_validate_ledger(ledger, max_evidence = policy$max_evidence)
  ledger
}


# Return only the stable fields approved for preview/provider transport.
# Dataset/request fingerprints remain on the validated local ledger for stale
# detection and privacy-safe logging; they are intentionally excluded from
# provider egress along with attributes and any future internal fields.
ena3d_ai_public_payload <- function(ledger) {
  ena3d_ai_validate_ledger(ledger)
  fields <- c(
    "schema_version", "view", "privacy", "context", "evidence"
  )
  unclass(ledger)[fields]
}
