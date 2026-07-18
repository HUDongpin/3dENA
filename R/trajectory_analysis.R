# Pure centroid-trajectory analysis for ENA point tables.
#
# This file intentionally has no Shiny, Plotly, data.table, or rENA dependency.
# The four public entry points are:
#   compute_centroid_path()
#   bootstrap_centroid_path()
#   compare_centroid_paths()
#   compare_independent_centroid_paths()

.trajectory_copy_frame <- function(points) {
  if (!(is.data.frame(points) || is.matrix(points))) {
    stop("`points` must be a data.frame or matrix.", call. = FALSE)
  }

  point_names <- colnames(points)
  if (is.null(point_names) || anyNA(point_names) || any(!nzchar(point_names))) {
    stop("`points` must have non-empty column names.", call. = FALSE)
  }

  copied <- if (is.matrix(points)) {
    lapply(seq_along(point_names), function(j) {
      points[, j, drop = TRUE]
    })
  } else {
    lapply(seq_along(point_names), function(j) {
      points[[j]][seq_len(nrow(points))]
    })
  }
  names(copied) <- point_names
  as.data.frame(copied, stringsAsFactors = FALSE, check.names = FALSE,
                optional = TRUE)
}

.trajectory_scalar_name <- function(x, argument) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(sprintf("`%s` must be one non-empty column name.", argument),
         call. = FALSE)
  }
  x
}

.trajectory_names <- function(x, argument, allow_null = FALSE) {
  if (allow_null && (is.null(x) || (is.character(x) && !length(x)))) {
    return(character())
  }
  if (!is.character(x) || !length(x) || anyNA(x) || any(!nzchar(x))) {
    stop(sprintf("`%s` must contain non-empty column names.", argument),
         call. = FALSE)
  }
  if (anyDuplicated(x)) {
    stop(sprintf("`%s` must not contain duplicate column names.", argument),
         call. = FALSE)
  }
  x
}

.trajectory_require_columns <- function(points, columns) {
  absent <- setdiff(columns, names(points))
  if (length(absent)) {
    stop(sprintf("Missing required column%s: %s.",
                 if (length(absent) == 1L) "" else "s",
                 paste(absent, collapse = ", ")), call. = FALSE)
  }
}

.trajectory_is_missing <- function(x) {
  missing <- is.na(x)
  if (is.numeric(x) || inherits(x, "Date") || inherits(x, "POSIXt") ||
      inherits(x, "difftime")) {
    missing <- missing | !is.finite(as.numeric(x))
  }
  missing
}

.trajectory_value_key <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (inherits(x, "Date")) {
    value <- format(x, "%Y-%m-%d")
  } else if (inherits(x, "POSIXt")) {
    value <- format(as.numeric(x), digits = 17L, scientific = TRUE,
                    trim = TRUE)
  } else if (inherits(x, "difftime")) {
    # `difftime` units are presentation metadata, not part of the physical
    # time key. Canonical seconds let equivalent hour/minute/second vectors
    # match across explicit orders and paired inputs.
    value <- format(as.numeric(x, units = "secs"),
                    digits = 17L, scientific = TRUE,
                    trim = TRUE)
  } else if (is.numeric(x)) {
    # Integer and double vectors represent the same analytical key space.
    # Formatting integer storage directly (for example, `1L`) used to produce
    # "1", while the semantically identical double `1` produced "1e+00".
    # Normalising storage first keeps cross-table ID, time, and group matches
    # independent of this implementation detail.
    value <- format(as.numeric(x), digits = 17L, scientific = TRUE,
                    trim = TRUE)
  } else {
    value <- as.character(x)
  }
  value[is.na(value)] <- "<NA>"
  vapply(value, encodeString, character(1L), quote = '"', na.encode = TRUE,
         USE.NAMES = FALSE)
}

.trajectory_stable_weighted_mean <- function(values, weights) {
  values <- as.numeric(values)
  weights <- as.numeric(weights)
  if (!length(values) || length(values) != length(weights) ||
      any(!is.finite(values)) || any(!is.finite(weights))) {
    return(NA_real_)
  }
  usable <- weights > 0
  if (!any(usable)) return(NA_real_)

  values <- values[usable]
  weights <- weights[usable]
  nonzero <- values != 0
  if (!any(nonzero)) return(0)

  compensated_sum <- function(x) {
    total <- 0
    correction <- 0
    for (term in x) {
      updated <- total + term
      if (abs(total) >= abs(term)) {
        correction <- correction + ((total - updated) + term)
      } else {
        correction <- correction + ((term - updated) + total)
      }
      total <- updated
    }
    total + correction
  }

  max_weight <- max(weights)
  scaled_weights <- weights / max_weight
  denominator_scaled <- sum(scaled_weights)
  raw_terms <- scaled_weights * values
  lost <- nonzero & scaled_weights == 0
  if (any(lost)) {
    # When w / max(w) underflows, form w*x first. Under this condition w is so
    # small relative to max(w) that the finite product cannot overflow; this
    # recovers cross-range contributions without taking logs and therefore
    # retains relative differences between nearby tiny weights.
    raw_terms[lost] <-
      (weights[lost] * values[lost]) / max_weight
  }

  raw_numerator <- compensated_sum(raw_terms)
  result <- if (is.finite(raw_numerator)) {
    raw_numerator / denominator_scaled
  } else {
    # Same-sign DBL_MAX-scale terms can overflow only while being summed.
    # Dividing each by the finite denominator first makes their absolute total
    # a convex-combination bound, after which compensated summation is safe.
    normalized_sum <- compensated_sum(raw_terms / denominator_scaled)
    if (is.finite(normalized_sum)) {
      normalized_sum
    } else {
      # A last-resort ratio form handles the one-ulp overflow possible when
      # several rounded DBL_MAX / n contributions add just above DBL_MAX.
      # This branch is used only after both unscaled and normalized sums are
      # non-finite, where tiny residual terms cannot be represented alongside
      # the boundary-scale result anyway.
      term_scale <- max(abs(raw_terms))
      term_scale * (
        compensated_sum(raw_terms / term_scale) / denominator_scaled
      )
    }
  }

  # A non-negative weighted mean is a convex combination. This clamp handles
  # the last ulp of floating-point rounding without changing the estimand.
  min(max(result, min(values)), max(values))
}

.trajectory_stable_norm <- function(values) {
  values <- as.numeric(values)
  if (!length(values) || any(!is.finite(values))) return(NA_real_)
  scale <- max(abs(values))
  if (scale == 0) return(0)
  scale * sqrt(sum((values / scale)^2))
}

.trajectory_stable_geometric_mean <- function(a, b) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  result <- numeric(length(a))
  positive <- a > 0 & b > 0
  result[positive] <- exp((log(a[positive]) + log(b[positive])) / 2)
  result
}

.trajectory_align_difftime_units <- function(values, reference) {
  if (!inherits(values, "difftime") || !inherits(reference, "difftime")) {
    return(values)
  }
  units <- attr(reference, "units")
  as.difftime(as.numeric(values, units = units), units = units)
}

.trajectory_difftime_equivalent <- function(a, b) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  finite <- is.finite(a) & is.finite(b)
  scale <- pmax(abs(a), abs(b))
  smallest <- .Machine$double.xmin * .Machine$double.eps
  # Eight relative epsilons correspond to roughly 8--16 ULP across a binary
  # exponent bin. This is wide enough for the rounding noise from chained
  # hour/minute/second conversion, but deliberately much narrower than a
  # display-level significant-digit rounding rule.
  tolerance <- 8 * pmax(scale * .Machine$double.eps, smallest)
  finite & abs(a - b) <= tolerance
}

.trajectory_difftime_match <- function(values, candidates) {
  values <- as.numeric(values, units = "secs")
  candidates <- as.numeric(candidates, units = "secs")
  vapply(seq_along(values), function(i) {
    equivalent <- .trajectory_difftime_equivalent(values[i], candidates)
    if (!any(equivalent)) return(NA_integer_)
    eligible <- which(equivalent)
    eligible[which.min(abs(candidates[eligible] - values[i]))]
  }, integer(1L))
}

.trajectory_difftime_unique_mask <- function(values) {
  seconds <- as.numeric(values, units = "secs")
  keep <- rep(FALSE, length(seconds))
  representatives <- numeric()
  for (i in seq_along(seconds)) {
    if (!length(representatives) ||
        !any(.trajectory_difftime_equivalent(
          seconds[i], representatives
        ))) {
      keep[i] <- TRUE
      representatives <- c(representatives, seconds[i])
    }
  }
  keep
}

.trajectory_convert_order <- function(order_values, time_values) {
  if (is.factor(time_values)) {
    labels <- as.character(order_values)
    return(factor(labels, levels = labels, ordered = is.ordered(time_values)))
  }
  if (inherits(time_values, "Date")) {
    if (inherits(order_values, "Date")) return(order_values)
    if (is.numeric(order_values)) {
      return(as.Date(order_values, origin = "1970-01-01"))
    }
    return(as.Date(as.character(order_values)))
  }
  if (inherits(time_values, "POSIXt")) {
    tz <- attr(time_values, "tzone")
    if (is.null(tz) || !length(tz) || is.na(tz[1L])) tz <- "UTC"
    if (inherits(order_values, "POSIXt")) {
      return(as.POSIXct(order_values, tz = tz[1L]))
    }
    if (is.numeric(order_values)) {
      return(as.POSIXct(order_values, origin = "1970-01-01", tz = tz[1L]))
    }
    return(as.POSIXct(as.character(order_values), tz = tz[1L]))
  }
  if (inherits(time_values, "difftime")) {
    units <- attr(time_values, "units")
    if (inherits(order_values, "difftime")) {
      return(as.difftime(as.numeric(order_values, units = units),
                         units = units))
    }
    return(as.difftime(as.numeric(order_values), units = units))
  }
  if (is.integer(time_values)) return(as.integer(order_values))
  if (is.numeric(time_values)) return(as.numeric(order_values))
  if (is.logical(time_values)) return(as.logical(order_values))
  as.character(order_values)
}

.trajectory_resolve_order <- function(time_values, order_values = NULL) {
  observed <- time_values[!.trajectory_is_missing(time_values)]
  if (!length(observed) && is.null(order_values)) {
    stop("No non-missing time values are available and `order` was not supplied.",
         call. = FALSE)
  }

  implicit_character <- FALSE
  if (is.null(order_values)) {
    if (is.factor(time_values)) {
      labels <- levels(time_values)
      order_values <- factor(labels, levels = labels,
                             ordered = is.ordered(time_values))
      source <- "factor_levels"
    } else if (inherits(time_values, "difftime")) {
      ordered <- observed[order(
        as.numeric(observed, units = "secs"), method = "radix"
      )]
      # `unique.difftime()` drops the class. Deduplicating by the resolved key
      # while subsetting the original vector preserves both class and units;
      # the narrow physical-time equivalence rule also collapses conversion
      # noise in implicit paired inputs. Explicit equivalents remain an error.
      order_values <- ordered[.trajectory_difftime_unique_mask(ordered)]
      source <- "ascending_value"
    } else if (is.numeric(time_values) || inherits(time_values, "Date") ||
               inherits(time_values, "POSIXt") || is.logical(time_values)) {
      order_values <- sort(unique(observed), na.last = NA)
      source <- "ascending_value"
    } else {
      order_values <- unique(observed)
      source <- "stable_first_appearance"
      implicit_character <- TRUE
    }
  } else {
    if (!is.atomic(order_values) || is.list(order_values) || !length(order_values)) {
      stop("`order` must be a non-empty atomic vector of time values.",
           call. = FALSE)
    }
    order_values <- .trajectory_convert_order(order_values, time_values)
    source <- "explicit"
  }

  order_keys <- .trajectory_value_key(order_values)
  if (any(.trajectory_is_missing(order_values))) {
    stop("`order` must not contain missing or non-finite values.", call. = FALSE)
  }
  duplicate_order <- if (inherits(order_values, "difftime")) {
    any(!.trajectory_difftime_unique_mask(order_values))
  } else {
    anyDuplicated(order_keys) > 0L
  }
  if (duplicate_order) {
    stop("`order` must not contain duplicate time values.", call. = FALSE)
  }
  missing_observed <- if (inherits(observed, "difftime") &&
                          inherits(order_values, "difftime")) {
    anyNA(.trajectory_difftime_match(observed, order_values))
  } else {
    observed_keys <- unique(.trajectory_value_key(observed))
    length(setdiff(observed_keys, order_keys)) > 0L
  }
  if (missing_observed) {
    stop("`order` must include every observed non-missing time value.",
         call. = FALSE)
  }

  list(values = order_values, keys = order_keys, source = source,
       implicit_character = implicit_character)
}

.trajectory_group_template <- function(points, group_vars) {
  if (!length(group_vars)) {
    return(data.frame(.trajectory_no_group = 1L)[, FALSE, drop = FALSE])
  }
  valid <- Reduce(`&`, lapply(points[group_vars], function(x) {
    !.trajectory_is_missing(x)
  }))
  groups <- points[valid, group_vars, drop = FALSE]
  groups[!duplicated(groups), , drop = FALSE]
}

.trajectory_group_mask <- function(points, group_vars, group_row) {
  if (!length(group_vars)) return(rep(TRUE, nrow(points)))
  mask <- rep(TRUE, nrow(points))
  for (name in group_vars) {
    target <- group_row[[name]][1L]
    column <- points[[name]]
    if (is.na(target)) {
      mask <- mask & is.na(column)
    } else {
      # Direct factor comparison requires identical level sets. Comparisons
      # combine independently constructed point tables, so equal labels can
      # legitimately arrive with different unused levels. The same canonical
      # keys used for participant/time matching avoid that factor-level trap.
      mask <- mask & !is.na(column) &
        (.trajectory_value_key(column) == .trajectory_value_key(target))
    }
  }
  mask[is.na(mask)] <- FALSE
  mask
}

.trajectory_group_label <- function(group_vars, group_row) {
  if (!length(group_vars)) return("all")
  paste(paste0(group_vars, "=", vapply(group_vars, function(name) {
    value <- group_row[[name]][1L]
    if (is.na(value)) "NA" else as.character(value)
  }, character(1L))), collapse = ", ")
}

.trajectory_resolve_weights <- function(points, weights) {
  if (is.null(weights)) {
    return(list(values = rep(1, nrow(points)), supplied = FALSE,
                description = "equal participant weights"))
  }
  if (is.character(weights)) {
    weight_name <- .trajectory_scalar_name(weights, "weights")
    .trajectory_require_columns(points, weight_name)
    values <- points[[weight_name]]
    description <- paste0("column:", weight_name)
  } else {
    if (!is.numeric(weights) || length(weights) != nrow(points)) {
      stop("`weights` must be NULL, one numeric column name, or a numeric vector with one value per row.",
           call. = FALSE)
    }
    values <- weights
    description <- "external numeric vector"
  }
  if (!is.numeric(values)) {
    stop("Resolved `weights` must be numeric.", call. = FALSE)
  }
  list(values = as.numeric(values), supplied = TRUE,
       description = description)
}

.trajectory_validate_common <- function(points, time_var, id_var, group_vars,
                                        dimensions, weights, na_policy,
                                        distance_space, full_dimensions) {
  time_var <- .trajectory_scalar_name(time_var, "time_var")
  id_var <- .trajectory_scalar_name(id_var, "id_var")
  group_vars <- .trajectory_names(group_vars, "group_vars", allow_null = TRUE)
  dimensions <- .trajectory_names(dimensions, "dimensions")
  na_policy <- match.arg(na_policy, c("complete", "error"))
  distance_space <- match.arg(distance_space, c("selected", "full"))

  if (time_var == id_var || time_var %in% group_vars || id_var %in% group_vars) {
    stop("`time_var`, `id_var`, and `group_vars` must refer to distinct columns.",
         call. = FALSE)
  }
  if (distance_space == "full") {
    full_dimensions <- .trajectory_names(full_dimensions, "full_dimensions")
    # A full-space distance cannot omit an axis used for the displayed
    # centroid. Preserve the caller's requested full-space order while adding
    # any selected dimensions they omitted, rather than silently calculating
    # distance in a smaller space.
    full_dimensions <- unique(c(full_dimensions, dimensions))
  } else {
    if (!is.null(full_dimensions)) {
      full_dimensions <- .trajectory_names(full_dimensions, "full_dimensions")
    } else {
      full_dimensions <- character()
    }
  }
  distance_dimensions <- if (distance_space == "selected") dimensions else full_dimensions
  analysis_dimensions <- unique(c(dimensions, distance_dimensions))
  .trajectory_require_columns(points, unique(c(time_var, id_var, group_vars,
                                                analysis_dimensions)))
  non_numeric <- analysis_dimensions[!vapply(points[analysis_dimensions],
                                              is.numeric, logical(1L))]
  if (length(non_numeric)) {
    stop(sprintf("Analytical dimension%s must be numeric: %s.",
                 if (length(non_numeric) == 1L) "" else "s",
                 paste(non_numeric, collapse = ", ")), call. = FALSE)
  }
  weight_info <- .trajectory_resolve_weights(points, weights)

  key_columns <- unique(c(time_var, id_var, group_vars))
  bad_key <- Reduce(`|`, lapply(points[key_columns], .trajectory_is_missing))
  # Selected coordinates define the centroid cohort. Extra coordinates used
  # only for a full-space distance must never silently remove a participant
  # from the displayed centroid. When those extra coordinates are incomplete,
  # the selected centroid remains estimable and the corresponding full-space
  # distance is reported as unavailable instead.
  bad_dimension <- Reduce(`|`, lapply(points[dimensions], function(x) {
    is.na(x) | !is.finite(x)
  }))
  bad_distance_dimension <- Reduce(`|`, lapply(
    points[distance_dimensions],
    function(x) is.na(x) | !is.finite(x)
  ))
  bad_weight <- is.na(weight_info$values) | !is.finite(weight_info$values) |
    weight_info$values < 0
  if (na_policy == "error" &&
      any(bad_key | bad_dimension | bad_distance_dimension | bad_weight)) {
    stop(sprintf(paste0("`na_policy = \"error\"` found invalid analytical rows ",
                        "(key=%d, selected dimension=%d, distance dimension=%d, ",
                        "weight=%d)."),
                 sum(bad_key), sum(bad_dimension),
                 sum(bad_distance_dimension), sum(bad_weight)),
         call. = FALSE)
  }

  list(time_var = time_var, id_var = id_var, group_vars = group_vars,
       dimensions = dimensions, full_dimensions = full_dimensions,
       distance_dimensions = distance_dimensions,
       analysis_dimensions = analysis_dimensions, na_policy = na_policy,
       distance_space = distance_space, weights = weight_info,
       bad_key = bad_key, bad_dimension = bad_dimension,
       bad_distance_dimension = bad_distance_dimension,
       bad_weight = bad_weight)
}

.trajectory_compute_output_names <- function(dimensions, time_var,
                                             include_bootstrap = FALSE) {
  centroid <- paste0("centroid_", dimensions)
  delta <- paste0("delta_", dimensions)
  generated <- c(
    if (time_var != "time_value") "time_value",
    "time_order", "n_rows_total", "n_total", "n_used", "n_missing",
    "n_excluded", "n_cohort_excluded", "n_zero_weight",
    "n_rows_missing_key", "n_rows_missing", "n_distance_incomplete",
    "n_rows_distance_incomplete", "n_duplicate_rows", centroid, delta,
    "dx", "dy", "dz", "step_distance", "elapsed_interval", "speed",
    "cumulative_distance"
  )
  if (include_bootstrap) {
    metrics <- c(centroid, delta, "step_distance", "speed",
                 "cumulative_distance")
    generated <- c(
      generated,
      as.vector(outer(metrics, c("_lower", "_upper", "_boot_n"), paste0))
    )
  }
  generated
}

.trajectory_comparison_output_names <- function(dimensions, time_var) {
  centroid_a <- paste0("centroid_a_", dimensions)
  centroid_b <- paste0("centroid_b_", dimensions)
  difference <- paste0("difference_", dimensions)
  generated <- c(
    if (time_var != "time_value") "time_value",
    "time_order", "n_a_total", "n_b_total", "n_a_valid", "n_b_valid",
    "n_a_rows_missing_key", "n_b_rows_missing_key",
    "n_matched", "n_used", "n_unmatched_a", "n_unmatched_b",
    "n_dropped_a", "n_dropped_b", "n_cohort_excluded",
    centroid_a, centroid_b, difference,
    paste0("delta_a_", dimensions), paste0("delta_b_", dimensions),
    paste0("delta_difference_", dimensions),
    "centroid_difference_distance", "step_distance_a", "step_distance_b",
    "step_distance_difference", "elapsed_interval", "speed_a", "speed_b",
    "speed_difference", "cumulative_distance_a", "cumulative_distance_b",
    "cumulative_distance_difference"
  )
  bootstrap_metrics <- c(
    centroid_a, centroid_b, difference, "centroid_difference_distance",
    "step_distance_a", "step_distance_b", "step_distance_difference",
    "speed_a", "speed_b", "speed_difference", "cumulative_distance_a",
    "cumulative_distance_b", "cumulative_distance_difference"
  )
  c(
    generated,
    as.vector(outer(bootstrap_metrics,
                    c("_lower", "_upper", "_boot_n"), paste0))
  )
}

.trajectory_independent_comparison_output_names <- function(dimensions,
                                                              time_var) {
  centroid_a <- paste0("centroid_a_", dimensions)
  centroid_b <- paste0("centroid_b_", dimensions)
  difference <- paste0("difference_", dimensions)
  delta_a <- paste0("delta_a_", dimensions)
  delta_b <- paste0("delta_b_", dimensions)
  delta_difference <- paste0("delta_difference_", dimensions)
  generated <- c(
    if (time_var != "time_value") "time_value",
    "time_order", "n_a_total", "n_b_total", "n_a_valid", "n_b_valid",
    "n_a_used", "n_b_used", "n_a_rows_missing_key",
    "n_b_rows_missing_key", "n_a_dropped", "n_b_dropped",
    "n_a_cohort_excluded", "n_b_cohort_excluded",
    "n_a_distance_incomplete", "n_b_distance_incomplete",
    centroid_a, centroid_b, difference, delta_a, delta_b, delta_difference,
    "centroid_difference_distance", "step_distance_a", "step_distance_b",
    "step_distance_difference", "elapsed_interval", "speed_a", "speed_b",
    "speed_difference", "cumulative_distance_a", "cumulative_distance_b",
    "cumulative_distance_difference"
  )
  bootstrap_metrics <- c(
    centroid_a, centroid_b, difference, delta_a, delta_b, delta_difference,
    "centroid_difference_distance", "step_distance_a", "step_distance_b",
    "step_distance_difference", "speed_a", "speed_b", "speed_difference",
    "cumulative_distance_a", "cumulative_distance_b",
    "cumulative_distance_difference"
  )
  test_metrics <- c(
    difference, delta_difference, "centroid_difference_distance",
    "step_distance_difference", "speed_difference",
    "cumulative_distance_difference"
  )
  c(
    generated,
    as.vector(outer(bootstrap_metrics,
                    c("_lower", "_upper", "_boot_n"), paste0)),
    as.vector(outer(test_metrics,
                    c("_p_value", "_p_adjusted", "_perm_n", "_significant"),
                    paste0))
  )
}

.trajectory_validate_output_names <- function(time_var, group_vars, generated,
                                              context) {
  preserved <- c(time_var, group_vars)
  preserved_collisions <- intersect(preserved, generated)
  generated_collisions <- unique(generated[duplicated(generated)])

  details <- character()
  if (length(preserved_collisions)) {
    details <- c(details, sprintf(
      "input time/group column(s) conflict with generated output: %s",
      paste(preserved_collisions, collapse = ", ")
    ))
  }
  if (length(generated_collisions)) {
    details <- c(details, sprintf(
      "dimension names generate duplicate output column(s): %s",
      paste(generated_collisions, collapse = ", ")
    ))
  }
  if (length(details)) {
    stop(sprintf(
      "%s output column collision (%s). Rename the conflicting input column or analytical dimension.",
      context, paste(details, collapse = "; ")
    ), call. = FALSE)
  }
  invisible(TRUE)
}

.trajectory_entity_table <- function(points, validation, order_info,
                                     group_template) {
  group_vars <- validation$group_vars
  time_var <- validation$time_var
  id_var <- validation$id_var
  dimensions <- validation$analysis_dimensions
  coord_names <- paste0(".coord_", seq_along(dimensions))
  time_values <- .trajectory_align_difftime_units(
    points[[time_var]], order_info$values
  )
  time_index <- if (inherits(time_values, "difftime") &&
                    inherits(order_info$values, "difftime")) {
    .trajectory_difftime_match(time_values, order_info$values)
  } else {
    match(.trajectory_value_key(time_values), order_info$keys)
  }
  id_keys <- .trajectory_value_key(points[[id_var]])
  group_count <- max(1L, nrow(group_template))
  period_count <- length(order_info$values)
  records <- list()
  stats <- vector("list", group_count * period_count)
  record_index <- 0L

  for (g in seq_len(group_count)) {
    group_row <- if (length(group_vars)) group_template[g, , drop = FALSE] else NULL
    group_mask <- if (length(group_vars)) {
      .trajectory_group_mask(points, group_vars, group_row)
    } else {
      rep(TRUE, nrow(points))
    }
    for (t in seq_len(period_count)) {
      grid_index <- (g - 1L) * period_count + t
      slice_mask <- group_mask & !is.na(time_index) & time_index == t
      slice_mask[is.na(slice_mask)] <- FALSE
      slice_rows <- which(slice_mask)
      rows <- slice_rows[!validation$bad_key[slice_rows]]
      slice_ids <- unique(id_keys[rows])
      duplicate_rows <- length(rows) - length(slice_ids)
      n_rows_missing_key <- sum(validation$bad_key[slice_rows])
      n_rows_missing <- if (length(rows)) {
        sum(validation$bad_dimension[rows] | validation$bad_weight[rows])
      } else 0L
      n_rows_distance_incomplete <- if (length(rows)) {
        sum(!validation$bad_dimension[rows] &
              !validation$bad_weight[rows] &
              validation$bad_distance_dimension[rows])
      } else 0L
      n_missing <- 0L
      n_zero_weight <- 0L
      n_distance_incomplete <- 0L
      n_valid <- 0L

      for (id_key in slice_ids) {
        id_rows <- rows[id_keys[rows] == id_key]
        row_valid <- !validation$bad_dimension[id_rows] &
          !validation$bad_weight[id_rows]
        positive <- row_valid & validation$weights$values[id_rows] > 0
        if (!any(positive)) {
          if (any(!row_valid)) n_missing <- n_missing + 1L
          if (any(row_valid & validation$weights$values[id_rows] == 0)) {
            n_zero_weight <- n_zero_weight + 1L
          }
          next
        }

        used_rows <- id_rows[positive]
        row_weights <- validation$weights$values[used_rows]
        coordinates <- vapply(dimensions, function(name) {
          .trajectory_stable_weighted_mean(
            points[[name]][used_rows], row_weights
          )
        }, numeric(1L))
        distance_complete <- !any(
          validation$bad_distance_dimension[used_rows]
        )
        if (!distance_complete) {
          n_distance_incomplete <- n_distance_incomplete + 1L
        }
        participant_weight <- if (validation$weights$supplied) {
          .trajectory_stable_weighted_mean(
            row_weights, rep(1, length(row_weights))
          )
        } else 1
        record_index <- record_index + 1L
        record <- data.frame(.group_index = g, .time_order = t,
                             .id_key = id_key,
                             .entity_weight = participant_weight,
                             .distance_complete = distance_complete,
                             stringsAsFactors = FALSE,
                             check.names = FALSE)
        for (j in seq_along(coord_names)) record[[coord_names[j]]] <- coordinates[j]
        records[[record_index]] <- record
        n_valid <- n_valid + 1L
      }

      stats[[grid_index]] <- data.frame(
        .group_index = g,
        .time_order = t,
        n_rows_total = length(slice_rows),
        n_total = length(slice_ids),
        n_valid = n_valid,
        n_missing = n_missing,
        n_zero_weight = n_zero_weight,
        n_rows_missing_key = n_rows_missing_key,
        n_distance_incomplete = n_distance_incomplete,
        n_rows_missing = n_rows_missing,
        n_rows_distance_incomplete = n_rows_distance_incomplete,
        n_duplicate_rows = duplicate_rows,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(records)) {
    entities <- do.call(rbind, records)
    rownames(entities) <- NULL
  } else {
    entities <- data.frame(.group_index = integer(), .time_order = integer(),
                           .id_key = character(), .entity_weight = numeric(),
                           .distance_complete = logical(),
                           stringsAsFactors = FALSE, check.names = FALSE)
    for (name in coord_names) entities[[name]] <- numeric()
  }
  list(entities = entities, stats = do.call(rbind, stats),
       coord_names = coord_names)
}

.trajectory_elapsed <- function(order_values) {
  count <- length(order_values)
  elapsed <- rep(NA_real_, count)
  if (count < 2L) {
    units <- if (inherits(order_values, "Date")) "days" else
      if (inherits(order_values, "POSIXt")) "seconds" else
        if (inherits(order_values, "difftime")) attr(order_values, "units") else
          if (is.numeric(order_values)) "time units" else "not applicable"
    return(list(values = elapsed, units = units))
  }
  if (inherits(order_values, "Date")) {
    elapsed[-1L] <- as.numeric(diff(order_values), units = "days")
    units <- "days"
  } else if (inherits(order_values, "POSIXt")) {
    elapsed[-1L] <- as.numeric(diff(order_values), units = "secs")
    units <- "seconds"
  } else if (inherits(order_values, "difftime")) {
    units <- attr(order_values, "units")
    elapsed[-1L] <- as.numeric(diff(order_values), units = units)
  } else if (is.numeric(order_values)) {
    elapsed[-1L] <- diff(as.numeric(order_values))
    units <- "time units"
  } else {
    units <- "not applicable"
  }
  list(values = elapsed, units = units)
}

.trajectory_path_metrics <- function(centroids, distance_columns, group_count,
                                     period_count, order_values) {
  row_count <- group_count * period_count
  step <- rep(NA_real_, row_count)
  cumulative <- rep(NA_real_, row_count)
  elapsed_one <- .trajectory_elapsed(order_values)
  elapsed <- rep(elapsed_one$values, times = group_count)
  speed <- rep(NA_real_, row_count)

  for (g in seq_len(group_count)) {
    rows <- ((g - 1L) * period_count + 1L):(g * period_count)
    current <- 0
    first <- centroids[rows[1L], distance_columns, drop = TRUE]
    continuous <- all(is.finite(first))
    step[rows[1L]] <- if (continuous) 0 else NA_real_
    cumulative[rows[1L]] <- if (continuous) 0 else NA_real_
    if (period_count > 1L) {
      for (j in 2:period_count) {
        previous <- centroids[rows[j - 1L], distance_columns, drop = TRUE]
        present <- centroids[rows[j], distance_columns, drop = TRUE]
        adjacent_valid <- all(is.finite(previous)) && all(is.finite(present))
        if (adjacent_valid) {
          step[rows[j]] <- .trajectory_stable_norm(present - previous)
        }
        if (continuous && adjacent_valid) {
          current <- current + step[rows[j]]
          cumulative[rows[j]] <- current
        } else {
          # Once the requested path is discontinuous, a distance from the
          # requested starting period is no longer identified. Later valid
          # adjacent segments retain their own step distances, but they must
          # never be presented as a complete cumulative path length.
          continuous <- FALSE
          cumulative[rows[j]] <- NA_real_
        }
      }
    }
  }
  positive_elapsed <- is.finite(elapsed) & elapsed > 0 & is.finite(step)
  speed[positive_elapsed] <- step[positive_elapsed] / elapsed[positive_elapsed]
  list(step_distance = step, elapsed_interval = elapsed, speed = speed,
       cumulative_distance = cumulative, elapsed_units = elapsed_one$units)
}

.trajectory_diagnostics_frame <- function(diagnostics) {
  if (!length(diagnostics)) {
    return(data.frame(code = character(), severity = character(),
                      group = character(), time_order = integer(),
                      message = character(), count = integer(),
                      stringsAsFactors = FALSE))
  }
  result <- do.call(rbind, diagnostics)
  rownames(result) <- NULL
  result
}

.trajectory_emit_diagnostics <- function(diagnostics) {
  if (!nrow(diagnostics)) return(invisible(NULL))
  warning_codes <- unique(diagnostics$code[diagnostics$severity == "warning"])
  if (length(warning_codes)) {
    warning(sprintf("Trajectory diagnostics: %s. Inspect attr(result, \"trajectory_warnings\") for details.",
                    paste(warning_codes, collapse = ", ")), call. = FALSE)
  }
  invisible(NULL)
}

#' Compute an ordered path of ENA centroids
#'
#' Duplicate rows for the same participant and period are collapsed to one
#' participant-period point before calculating a centroid. With weights, both
#' the duplicate-row collapse and the centroid are weighted. `cohort_policy =
#' "complete"` uses only participants with valid points at every requested
#' period within a trajectory group. Selected dimensions define the centroid
#' cohort; incomplete extra full-space dimensions leave that centroid intact
#' and make the affected full-space movement metrics unavailable.
compute_centroid_path <- function(
    points,
    time_var,
    id_var,
    group_vars = NULL,
    dimensions,
    order = NULL,
    cohort_policy = c("available", "complete"),
    weights = NULL,
    na_policy = c("complete", "error"),
    distance_space = c("selected", "full"),
    full_dimensions = NULL
) {
  cohort_policy <- match.arg(cohort_policy)
  na_policy <- match.arg(na_policy)
  distance_space <- match.arg(distance_space)
  data <- .trajectory_copy_frame(points)
  validation <- .trajectory_validate_common(
    data, time_var, id_var, group_vars, dimensions, weights, na_policy,
    distance_space, full_dimensions
  )
  .trajectory_validate_output_names(
    validation$time_var,
    validation$group_vars,
    .trajectory_compute_output_names(
      validation$dimensions, validation$time_var, include_bootstrap = FALSE
    ),
    "Centroid-path"
  )
  order_info <- .trajectory_resolve_order(data[[validation$time_var]], order)
  group_template <- .trajectory_group_template(data, validation$group_vars)
  if (length(validation$group_vars) && !nrow(group_template)) {
    stop("No non-missing grouping combinations are available.", call. = FALSE)
  }
  group_count <- max(1L, nrow(group_template))
  period_count <- length(order_info$values)
  grid_count <- group_count * period_count
  entity_info <- .trajectory_entity_table(data, validation, order_info,
                                           group_template)
  entities <- entity_info$entities
  slice_stats <- entity_info$stats
  analysis_dimensions <- validation$analysis_dimensions
  coord_names <- entity_info$coord_names
  centroids <- matrix(NA_real_, nrow = grid_count,
                      ncol = length(analysis_dimensions),
                      dimnames = list(NULL, analysis_dimensions))
  n_used <- integer(grid_count)
  n_cohort_excluded <- integer(grid_count)
  n_distance_incomplete <- integer(grid_count)
  used_ids <- vector("list", grid_count)

  complete_ids <- vector("list", group_count)
  for (g in seq_len(group_count)) {
    ids_by_period <- lapply(seq_len(period_count), function(t) {
      unique(entities$.id_key[entities$.group_index == g &
                                entities$.time_order == t])
    })
    complete_ids[[g]] <- if (length(ids_by_period)) {
      Reduce(intersect, ids_by_period)
    } else character()
  }

  for (g in seq_len(group_count)) {
    for (t in seq_len(period_count)) {
      index <- (g - 1L) * period_count + t
      selected <- entities$.group_index == g & entities$.time_order == t
      if (cohort_policy == "complete") {
        selected <- selected & entities$.id_key %in% complete_ids[[g]]
      }
      participant_rows <- which(selected)
      used_ids[[index]] <- entities$.id_key[participant_rows]
      n_used[index] <- length(participant_rows)
      n_cohort_excluded[index] <- slice_stats$n_valid[index] - n_used[index]
      n_distance_incomplete[index] <- if (length(participant_rows)) {
        sum(!entities$.distance_complete[participant_rows])
      } else {
        0L
      }
      if (length(participant_rows)) {
        participant_weights <- entities$.entity_weight[participant_rows]
        for (j in seq_along(analysis_dimensions)) {
          centroids[index, j] <- .trajectory_stable_weighted_mean(
            entities[[coord_names[j]]][participant_rows], participant_weights
          )
        }
      }
    }
  }

  if (length(validation$group_vars)) {
    group_rows <- rep(seq_len(group_count), each = period_count)
    result <- group_template[group_rows, validation$group_vars, drop = FALSE]
    rownames(result) <- NULL
  } else {
    result <- data.frame(row.names = seq_len(grid_count))[, FALSE, drop = FALSE]
  }
  time_values <- rep(order_info$values, times = group_count)
  result[[validation$time_var]] <- time_values
  if (validation$time_var != "time_value") result$time_value <- time_values
  result$time_order <- rep(seq_len(period_count), times = group_count)
  result$n_rows_total <- slice_stats$n_rows_total
  result$n_total <- slice_stats$n_total
  result$n_used <- n_used
  result$n_missing <- slice_stats$n_missing
  result$n_excluded <- pmax(0L, result$n_total - result$n_used)
  result$n_cohort_excluded <- n_cohort_excluded
  result$n_zero_weight <- slice_stats$n_zero_weight
  result$n_rows_missing_key <- slice_stats$n_rows_missing_key
  result$n_rows_missing <- slice_stats$n_rows_missing
  result$n_distance_incomplete <- n_distance_incomplete
  result$n_rows_distance_incomplete <-
    slice_stats$n_rows_distance_incomplete
  result$n_duplicate_rows <- slice_stats$n_duplicate_rows

  selected_indices <- match(validation$dimensions, analysis_dimensions)
  distance_indices <- match(validation$distance_dimensions, analysis_dimensions)
  for (j in seq_along(validation$dimensions)) {
    result[[paste0("centroid_", validation$dimensions[j])]] <-
      centroids[, selected_indices[j]]
  }

  delta_names <- paste0("delta_", validation$dimensions)
  for (j in seq_along(validation$dimensions)) {
    delta <- rep(NA_real_, grid_count)
    for (g in seq_len(group_count)) {
      rows <- ((g - 1L) * period_count + 1L):(g * period_count)
      if (period_count > 1L) {
        delta[rows[-1L]] <- diff(centroids[rows, selected_indices[j]])
      }
    }
    result[[delta_names[j]]] <- delta
  }
  axis_aliases <- c("dx", "dy", "dz")
  for (j in seq_along(axis_aliases)) {
    result[[axis_aliases[j]]] <- if (j <= length(delta_names)) {
      result[[delta_names[j]]]
    } else rep(NA_real_, grid_count)
  }

  path_metrics <- .trajectory_path_metrics(
    centroids, distance_indices, group_count, period_count, order_info$values
  )
  result$step_distance <- path_metrics$step_distance
  result$elapsed_interval <- path_metrics$elapsed_interval
  result$speed <- path_metrics$speed
  result$cumulative_distance <- path_metrics$cumulative_distance

  diagnostics <- list()
  add_diagnostic <- function(code, severity, group, time_order, message, count) {
    diagnostics[[length(diagnostics) + 1L]] <<- data.frame(
      code = code, severity = severity, group = group,
      time_order = as.integer(time_order), message = message,
      count = as.integer(count), stringsAsFactors = FALSE
    )
  }
  if (order_info$implicit_character) {
    add_diagnostic(
      "implicit_character_order", "warning", "all", NA_integer_,
      "Character time values use stable first-appearance order; supply `order` to make the substantive sequence explicit.",
      length(order_info$values)
    )
  }
  invalid_key_count <- sum(validation$bad_key)
  assigned_invalid_key_count <- sum(result$n_rows_missing_key)
  unassigned_invalid_key_count <- invalid_key_count - assigned_invalid_key_count
  if (invalid_key_count) {
    add_diagnostic("missing_key_rows", "warning", "all", NA_integer_,
                   paste0(
                     "Rows with missing/non-finite ID, time, or group keys ",
                     "were excluded from analytical entities; slice-assignable ",
                     "rows are exposed by `n_rows_missing_key`."
                   ),
                   invalid_key_count)
  }
  if (unassigned_invalid_key_count > 0L) {
    add_diagnostic(
      "unassigned_missing_key_rows", "warning", "all", NA_integer_,
      paste0(
        "Rows missing a time or group key cannot be assigned to a requested ",
        "group-period slice and are reported only by this global diagnostic."
      ),
      unassigned_invalid_key_count
    )
  }
  for (g in seq_len(group_count)) {
    group_row <- if (length(validation$group_vars)) {
      group_template[g, , drop = FALSE]
    } else NULL
    label <- .trajectory_group_label(validation$group_vars, group_row)
    group_indices <- ((g - 1L) * period_count + 1L):(g * period_count)
    for (t in seq_len(period_count)) {
      index <- group_indices[t]
      if (result$n_rows_total[index] == 0L) {
        add_diagnostic("missing_period", "warning", label, t,
                       "No rows were observed for this requested period.", 1L)
      }
      if (result$n_duplicate_rows[index] > 0L) {
        add_diagnostic("duplicate_id_time", "warning", label, t,
                       "Duplicate participant-period rows were collapsed before centroid calculation.",
                       result$n_duplicate_rows[index])
      }
      if (validation$distance_space == "full" &&
          result$n_used[index] > 0L &&
          result$n_distance_incomplete[index] > 0L) {
        add_diagnostic(
          "full_distance_incomplete", "warning", label, t,
          paste0(
            "The selected-axis centroid is retained, but full-space distance ",
            "is unavailable because one or more used participants have ",
            "incomplete full-rotation coordinates."
          ),
          result$n_distance_incomplete[index]
        )
      }
      if (result$n_used[index] == 1L) {
        add_diagnostic("one_entity_slice", "warning", label, t,
                       "The centroid is defined by one participant; uncertainty and variance are not estimable.",
                       1L)
      }
      participant_rows <- entities$.group_index == g & entities$.time_order == t &
        entities$.id_key %in% used_ids[[index]]
      if (result$n_used[index] > 1L && any(participant_rows)) {
        selected_values <- as.matrix(entities[participant_rows,
                                              coord_names[selected_indices],
                                              drop = FALSE])
        variances <- apply(selected_values, 2L, stats::var)
        if (all(is.finite(variances)) && all(variances == 0)) {
          add_diagnostic("zero_variance_slice", "warning", label, t,
                         "All selected coordinates have zero between-participant variance in this slice.",
                         result$n_used[index])
        }
      }
    }
    if (cohort_policy == "available" && period_count > 1L) {
      signatures <- vapply(used_ids[group_indices], function(ids) {
        paste(sort(unique(ids)), collapse = "\r")
      }, character(1L))
      if (length(unique(signatures)) > 1L) {
        add_diagnostic("changing_cohort", "warning", label, NA_integer_,
                       "Participant composition changes across requested periods under the available-cohort policy.",
                       length(unique(signatures)))
      }
    }
  }
  if (any(is.finite(result$elapsed_interval) & result$elapsed_interval <= 0,
          na.rm = TRUE)) {
    add_diagnostic("nonpositive_elapsed_interval", "warning", "all", NA_integer_,
                   "Explicit time order contains a non-positive numeric/Date-like interval; speed is undefined there.",
                   sum(is.finite(result$elapsed_interval) &
                         result$elapsed_interval <= 0, na.rm = TRUE))
  }
  diagnostics <- .trajectory_diagnostics_frame(diagnostics)

  spec <- list(
    time_var = validation$time_var,
    id_var = validation$id_var,
    group_vars = validation$group_vars,
    dimensions = validation$dimensions,
    order_values = order_info$values,
    order_source = order_info$source,
    cohort_policy = cohort_policy,
    na_policy = validation$na_policy,
    weights = validation$weights$description,
    duplicate_policy = "weighted participant-period mean, then one participant contribution",
    distance_space = validation$distance_space,
    distance_dimensions = validation$distance_dimensions,
    elapsed_interval_units = path_metrics$elapsed_units,
    missing_interval_policy = "do not bridge an unobserved/invalid adjacent centroid",
    missing_key_policy = paste0(
      "exclude rows with missing/non-finite ID, time, or group keys; count ",
      "slice-assignable rows in n_rows_missing_key and report rows lacking ",
      "time/group assignment globally"
    ),
    missing_key_counts = list(
      total = invalid_key_count,
      slice_assigned = assigned_invalid_key_count,
      unassigned = unassigned_invalid_key_count
    ),
    count_definitions = list(
      n_rows_total = "raw rows assignable to the requested group-period slice, including rows with a missing ID key",
      n_total = "unique non-missing participant IDs observed in the slice before analytical exclusions",
      n_used = "participant-period estimates used in the centroid",
      n_missing = "participants with no complete positive-weight analytical row",
      n_excluded = "observed participants not used, including missing, zero-weight, and cohort exclusions",
      n_rows_missing_key = "slice-assignable raw rows excluded because an ID key is missing/non-finite",
      n_distance_incomplete = "used participants lacking a complete coordinate vector for the requested distance space",
      n_rows_distance_incomplete = "otherwise usable raw rows lacking a complete coordinate vector for the requested distance space"
    )
  )
  class(result) <- c("centroid_path", "data.frame")
  attr(result, "trajectory_spec") <- spec
  attr(result, "trajectory_warnings") <- diagnostics
  .trajectory_emit_diagnostics(diagnostics)
  result
}

.trajectory_with_seed <- function(seed, operation) {
  if (is.null(seed)) return(operation())
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
      !is.finite(seed)) {
    stop("`seed` must be NULL or one finite numeric value.", call. = FALSE)
  }
  seed <- as.integer(seed)
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv,
                                inherits = FALSE)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  operation()
}

.trajectory_unique_name <- function(existing, stem) {
  candidate <- stem
  counter <- 0L
  while (candidate %in% existing) {
    counter <- counter + 1L
    candidate <- paste0(stem, counter)
  }
  candidate
}

.trajectory_cluster_sample <- function(points, id_var, sampled_keys,
                                       id_keys = NULL) {
  if (is.null(id_keys)) id_keys <- .trajectory_value_key(points[[id_var]])
  chunks <- vector("list", length(sampled_keys))
  for (draw in seq_along(sampled_keys)) {
    rows <- which(id_keys == sampled_keys[draw])
    chunk <- points[rows, , drop = FALSE]
    # A sampled participant needs a fresh clone ID. Otherwise repeated draws
    # would be collapsed back to one participant-period estimate downstream.
    chunk[[id_var]] <- rep.int(draw, nrow(chunk))
    chunks[[draw]] <- chunk
  }
  sampled <- do.call(rbind, chunks)
  rownames(sampled) <- NULL
  sampled
}

.trajectory_bootstrap_sampling_plan <- function(
    points, base_spec, weights, cohort_policy,
    bootstrap_design = c("auto", "cluster", "stratified")
) {
  bootstrap_design <- match.arg(bootstrap_design)
  full_dimensions <- if (base_spec$distance_space == "full") {
    base_spec$distance_dimensions
  } else {
    NULL
  }
  validation <- .trajectory_validate_common(
    points,
    time_var = base_spec$time_var,
    id_var = base_spec$id_var,
    group_vars = base_spec$group_vars,
    dimensions = base_spec$dimensions,
    weights = weights,
    na_policy = base_spec$na_policy,
    distance_space = base_spec$distance_space,
    full_dimensions = full_dimensions
  )
  order_info <- .trajectory_resolve_order(
    points[[base_spec$time_var]], base_spec$order_values
  )
  group_template <- .trajectory_group_template(points, base_spec$group_vars)
  group_count <- max(1L, nrow(group_template))
  period_count <- length(order_info$values)
  entities <- .trajectory_entity_table(
    points, validation, order_info, group_template
  )$entities

  pools <- lapply(seq_len(group_count), function(g) {
    ids_by_period <- lapply(seq_len(period_count), function(t) {
      unique(entities$.id_key[
        entities$.group_index == g & entities$.time_order == t
      ])
    })
    ids <- if (cohort_policy == "complete") {
      Reduce(intersect, ids_by_period)
    } else {
      unique(unlist(ids_by_period, use.names = FALSE))
    }
    sort(ids, method = "radix")
  })
  labels <- vapply(seq_len(group_count), function(g) {
    group_row <- if (length(base_spec$group_vars)) {
      group_template[g, , drop = FALSE]
    } else {
      NULL
    }
    .trajectory_group_label(base_spec$group_vars, group_row)
  }, character(1L))
  names(pools) <- labels

  nonempty_ids <- unlist(pools[lengths(pools) > 0L], use.names = FALSE)
  if (!length(nonempty_ids)) {
    stop(
      "No analytically eligible participant IDs are available for bootstrapping.",
      call. = FALSE
    )
  }

  resolved_design <- bootstrap_design
  if (resolved_design == "auto") {
    membership <- table(nonempty_ids)
    disjoint_groups <- group_count > 1L && all(membership == 1L)
    resolved_design <- if (disjoint_groups) "stratified" else "cluster"
  }
  if (group_count == 1L) resolved_design <- "cluster"

  list(
    pools = pools,
    labels = labels,
    group_template = group_template,
    group_vars = base_spec$group_vars,
    design = resolved_design,
    requested_design = bootstrap_design,
    participant_ids = sort(unique(nonempty_ids), method = "radix"),
    n_sampling_units = if (resolved_design == "stratified") {
      sum(lengths(pools))
    } else {
      length(unique(nonempty_ids))
    }
  )
}

.trajectory_stratified_cluster_sample <- function(
    points, id_var, id_keys, sampling_plan
) {
  chunks <- list()
  draw_index <- 0L
  group_order <- order(sampling_plan$labels, method = "radix")

  for (g in group_order) {
    pool <- sampling_plan$pools[[g]]
    if (!length(pool)) next
    sampled_keys <- sample(pool, length(pool), replace = TRUE)
    group_mask <- if (length(sampling_plan$group_vars)) {
      .trajectory_group_mask(
        points, sampling_plan$group_vars,
        sampling_plan$group_template[g, , drop = FALSE]
      )
    } else {
      rep(TRUE, nrow(points))
    }
    for (id_key in sampled_keys) {
      rows <- which(group_mask & id_keys == id_key)
      if (!length(rows)) next
      draw_index <- draw_index + 1L
      chunk <- points[rows, , drop = FALSE]
      chunk[[id_var]] <- rep.int(draw_index, nrow(chunk))
      chunks[[draw_index]] <- chunk
    }
  }

  if (!length(chunks)) return(points[FALSE, , drop = FALSE])
  sampled <- do.call(rbind, chunks)
  rownames(sampled) <- NULL
  sampled
}

.trajectory_match_path_rows <- function(reference, candidate, group_vars) {
  matched <- rep(NA_integer_, nrow(reference))
  for (i in seq_len(nrow(reference))) {
    mask <- candidate$time_order == reference$time_order[i]
    for (name in group_vars) {
      target <- reference[[name]][i]
      if (is.na(target)) {
        mask <- mask & is.na(candidate[[name]])
      } else {
        mask <- mask & !is.na(candidate[[name]]) & candidate[[name]] == target
      }
    }
    rows <- which(mask)
    if (length(rows)) matched[i] <- rows[1L]
  }
  matched
}

.trajectory_quantile <- function(x, probability) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  unname(stats::quantile(x, probs = probability, names = FALSE,
                         na.rm = TRUE, type = 7L))
}

.trajectory_roundoff_safe_ceiling <- function(value) {
  tolerance <- 8 * .Machine$double.eps * pmax(1, abs(value))
  ceiling(value - tolerance)
}

.trajectory_bootstrap_required_valid <- function(n_boot, conf_level) {
  max(
    ceiling(0.80 * n_boot),
    .trajectory_roundoff_safe_ceiling(10 / (1 - conf_level))
  )
}

.trajectory_group_row_sets <- function(data, group_vars) {
  if (!length(group_vars)) return(list(all = seq_len(nrow(data))))
  pieces <- lapply(group_vars, function(name) {
    value <- .trajectory_value_key(data[[name]])
    paste0(nchar(value, type = "bytes"), ":", value)
  })
  keys <- do.call(paste, c(pieces, sep = "\r"))
  split(seq_len(nrow(data)), keys, drop = TRUE)
}

.trajectory_metric_cluster_eligible <- function(data, metric, group_vars) {
  slice_ok <- is.finite(data$n_used) & data$n_used >= 2L
  eligible <- slice_ok
  interval_metric <- grepl("^(delta_|step_distance|speed)", metric)
  cumulative_metric <- grepl("^cumulative_distance", metric)

  if (interval_metric || cumulative_metric) {
    eligible[] <- FALSE
    for (rows in .trajectory_group_row_sets(data, group_vars)) {
      rows <- rows[order(data$time_order[rows], method = "radix")]
      if (cumulative_metric) {
        eligible[rows] <- as.logical(cumprod(slice_ok[rows]))
      } else {
        eligible[rows[1L]] <- slice_ok[rows[1L]]
        if (length(rows) > 1L) {
          eligible[rows[-1L]] <- slice_ok[rows[-1L]] &
            slice_ok[rows[-length(rows)]]
        }
      }
    }
  }
  eligible
}

.trajectory_bootstrap_diagnostics <- function(data, group_vars,
                                               cluster_failures,
                                               replicate_failures,
                                               required_valid, n_boot) {
  diagnostics <- list()
  add_rows <- function(code, failures, message) {
    affected <- which(rowSums(failures) > 0L)
    if (!length(affected)) return(invisible(NULL))
    for (row in affected) {
      group_row <- if (length(group_vars)) {
        data[row, group_vars, drop = FALSE]
      } else {
        NULL
      }
      diagnostics[[length(diagnostics) + 1L]] <<- data.frame(
        code = code,
        severity = "warning",
        group = .trajectory_group_label(group_vars, group_row),
        time_order = as.integer(data$time_order[row]),
        message = message,
        count = as.integer(sum(failures[row, ])),
        stringsAsFactors = FALSE
      )
    }
    invisible(NULL)
  }
  add_rows(
    "bootstrap_insufficient_clusters",
    cluster_failures,
    paste0(
      "Bootstrap interval bounds are unavailable for one or more metrics because ",
      "the contributing slice or interval has fewer than two participant clusters."
    )
  )
  add_rows(
    "bootstrap_insufficient_replicates",
    replicate_failures,
    paste0(
      "Bootstrap interval bounds are unavailable for one or more metrics because ",
      "fewer than ", required_valid, " of ", n_boot,
      " finite replicates were available (requires at least 80% overall and ",
      "five expected replicates in each tail)."
    )
  )
  .trajectory_diagnostics_frame(diagnostics)
}

#' Participant-cluster bootstrap intervals for a centroid path
#'
#' Each bootstrap draw samples analytically eligible participant IDs, then
#' carries every raw row for each sampled ID into the draw. Complete-cohort
#' analysis samples only IDs eligible at every requested period. Repeated draws
#' receive clone IDs so their contribution is not accidentally de-duplicated.
#' `bootstrap_design = "auto"` stratifies disjoint between-participant groups
#' and otherwise samples global participant clusters. Supplying `seed` makes
#' the result deterministic and restores the caller's RNG state on exit.
bootstrap_centroid_path <- function(
    points,
    time_var,
    id_var,
    group_vars = NULL,
    dimensions,
    order = NULL,
    cohort_policy = c("available", "complete"),
    weights = NULL,
    na_policy = c("complete", "error"),
    distance_space = c("selected", "full"),
    full_dimensions = NULL,
    n_boot = 1000L,
    conf_level = 0.95,
    seed = NULL,
    bootstrap_design = c("auto", "cluster", "stratified")
) {
  cohort_policy <- match.arg(cohort_policy)
  na_policy <- match.arg(na_policy)
  distance_space <- match.arg(distance_space)
  bootstrap_design <- match.arg(bootstrap_design)
  if (!is.numeric(n_boot) || length(n_boot) != 1L || is.na(n_boot) ||
      !is.finite(n_boot) || n_boot < 2 || n_boot != as.integer(n_boot)) {
    stop("`n_boot` must be one integer of at least 2.", call. = FALSE)
  }
  n_boot <- as.integer(n_boot)
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      is.na(conf_level) || !is.finite(conf_level) ||
      conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be strictly between 0 and 1.", call. = FALSE)
  }

  data <- .trajectory_copy_frame(points)
  id_var <- .trajectory_scalar_name(id_var, "id_var")
  .trajectory_require_columns(data, id_var)
  bootstrap_time_var <- .trajectory_scalar_name(time_var, "time_var")
  bootstrap_group_vars <- .trajectory_names(
    group_vars, "group_vars", allow_null = TRUE
  )
  bootstrap_dimensions <- .trajectory_names(dimensions, "dimensions")
  .trajectory_validate_output_names(
    bootstrap_time_var,
    bootstrap_group_vars,
    .trajectory_compute_output_names(
      bootstrap_dimensions, bootstrap_time_var, include_bootstrap = TRUE
    ),
    "Bootstrapped centroid-path"
  )
  bootstrap_weight <- weights
  base_weight <- weights
  if (is.character(weights)) {
    weight_name <- .trajectory_scalar_name(weights, "weights")
    .trajectory_require_columns(data, weight_name)
    if (identical(weight_name, id_var)) {
      # Bootstrap clones need fresh participant IDs, but an ID column may also
      # legitimately carry numeric analytical weights (`weights = id_var`).
      # Snapshot those weights before clone IDs overwrite the source column.
      temp_weight <- .trajectory_unique_name(
        names(data), ".trajectory_bootstrap_weight"
      )
      data[[temp_weight]] <- data[[weight_name]]
      bootstrap_weight <- temp_weight
    }
  } else if (!is.null(weights)) {
    temp_weight <- .trajectory_unique_name(names(data),
                                           ".trajectory_bootstrap_weight")
    if (!is.numeric(weights) || length(weights) != nrow(data)) {
      stop("External `weights` must have one numeric value per row.",
           call. = FALSE)
    }
    data[[temp_weight]] <- as.numeric(weights)
    bootstrap_weight <- temp_weight
    base_weight <- temp_weight
  }

  base_path <- compute_centroid_path(
    data, time_var = time_var, id_var = id_var, group_vars = group_vars,
    dimensions = dimensions, order = order,
    cohort_policy = cohort_policy, weights = base_weight,
    na_policy = na_policy, distance_space = distance_space,
    full_dimensions = full_dimensions
  )
  base_spec <- attr(base_path, "trajectory_spec")
  base_diagnostics <- attr(base_path, "trajectory_warnings")
  group_vars <- base_spec$group_vars

  id_keys <- .trajectory_value_key(data[[id_var]])
  sampling_plan <- .trajectory_bootstrap_sampling_plan(
    data, base_spec, bootstrap_weight, cohort_policy, bootstrap_design
  )
  participant_ids <- sampling_plan$participant_ids

  centroid_metrics <- paste0("centroid_", base_spec$dimensions)
  delta_metrics <- paste0("delta_", base_spec$dimensions)
  metrics <- c(centroid_metrics, delta_metrics, "step_distance", "speed",
               "cumulative_distance")
  replicate_values <- lapply(metrics, function(x) {
    matrix(NA_real_, nrow = nrow(base_path), ncol = n_boot)
  })
  names(replicate_values) <- metrics
  failed <- logical(n_boot)

  .trajectory_with_seed(seed, function() {
    for (b in seq_len(n_boot)) {
      sampled_data <- if (sampling_plan$design == "stratified") {
        .trajectory_stratified_cluster_sample(
          data, id_var, id_keys, sampling_plan
        )
      } else {
        pool <- sort(
          unique(unlist(sampling_plan$pools, use.names = FALSE)),
          method = "radix"
        )
        sampled_ids <- sample(pool, length(pool), replace = TRUE)
        .trajectory_cluster_sample(data, id_var, sampled_ids, id_keys)
      }
      candidate <- tryCatch(
        suppressWarnings(compute_centroid_path(
          sampled_data, time_var = base_spec$time_var, id_var = id_var,
          group_vars = group_vars, dimensions = base_spec$dimensions,
          order = base_spec$order_values, cohort_policy = cohort_policy,
          weights = bootstrap_weight, na_policy = na_policy,
          distance_space = distance_space,
          full_dimensions = base_spec$distance_dimensions
        )),
        error = function(e) NULL
      )
      if (is.null(candidate)) {
        failed[b] <<- TRUE
        next
      }
      candidate_rows <- .trajectory_match_path_rows(base_path, candidate,
                                                    group_vars)
      available <- !is.na(candidate_rows)
      for (metric in metrics) {
        if (metric %in% names(candidate)) {
          metric_values <- replicate_values[[metric]]
          metric_values[available, b] <-
            candidate[[metric]][candidate_rows[available]]
          replicate_values[[metric]] <<- metric_values
        }
      }
    }
    invisible(NULL)
  })

  alpha <- (1 - conf_level) / 2
  required_valid <- .trajectory_bootstrap_required_valid(n_boot, conf_level)
  cluster_failures <- matrix(
    FALSE, nrow = nrow(base_path), ncol = length(metrics),
    dimnames = list(NULL, metrics)
  )
  replicate_failures <- cluster_failures
  for (metric_index in seq_along(metrics)) {
    metric <- metrics[[metric_index]]
    values <- replicate_values[[metric]]
    boot_n <- rowSums(is.finite(values))
    cluster_ok <- .trajectory_metric_cluster_eligible(
      base_path, metric, group_vars
    )
    base_finite <- is.finite(base_path[[metric]])
    replicate_ok <- boot_n >= required_valid
    interval_ok <- base_finite & cluster_ok & replicate_ok
    lower <- upper <- rep(NA_real_, nrow(base_path))
    if (any(interval_ok)) {
      lower[interval_ok] <- apply(
        values[interval_ok, , drop = FALSE], 1L,
        .trajectory_quantile, probability = alpha
      )
      upper[interval_ok] <- apply(
        values[interval_ok, , drop = FALSE], 1L,
        .trajectory_quantile, probability = 1 - alpha
      )
    }
    base_path[[paste0(metric, "_lower")]] <- lower
    base_path[[paste0(metric, "_upper")]] <- upper
    base_path[[paste0(metric, "_boot_n")]] <- boot_n
    cluster_failures[, metric_index] <- base_finite & !cluster_ok
    replicate_failures[, metric_index] <- base_finite & cluster_ok & !replicate_ok
  }
  bootstrap_diagnostics <- .trajectory_bootstrap_diagnostics(
    base_path, group_vars, cluster_failures, replicate_failures,
    required_valid, n_boot
  )
  combined_diagnostics <- if (is.null(base_diagnostics) || !nrow(base_diagnostics)) {
    bootstrap_diagnostics
  } else if (!nrow(bootstrap_diagnostics)) {
    base_diagnostics
  } else {
    rbind(base_diagnostics, bootstrap_diagnostics)
  }
  class(base_path) <- c("bootstrapped_centroid_path", "centroid_path",
                        "data.frame")
  attr(base_path, "trajectory_spec") <- base_spec
  attr(base_path, "trajectory_warnings") <- combined_diagnostics
  attr(base_path, "bootstrap_spec") <- list(
    method = "participant-cluster percentile bootstrap",
    sampling_unit = id_var,
    rows_per_sampled_id = "all raw rows",
    repeated_id_policy = "fresh clone ID per draw",
    n_participants = length(participant_ids),
    n_sampling_units = sampling_plan$n_sampling_units,
    bootstrap_design_requested = sampling_plan$requested_design,
    bootstrap_design = sampling_plan$design,
    stratum_sizes = stats::setNames(
      lengths(sampling_plan$pools), sampling_plan$labels
    ),
    eligible_id_keys = participant_ids,
    eligible_id_keys_by_stratum = sampling_plan$pools,
    n_boot = n_boot,
    conf_level = conf_level,
    minimum_valid_fraction = 0.80,
    minimum_tail_replicates = 5L,
    minimum_valid_replicates = required_valid,
    seed = seed,
    failed_replicates = sum(failed),
    rng_state_restored = !is.null(seed)
  )
  .trajectory_emit_diagnostics(bootstrap_diagnostics)
  base_path
}

.trajectory_resolve_order_two <- function(time_a, time_b, order_values = NULL) {
  time_family <- function(value) {
    if (inherits(value, "Date")) return("Date")
    if (inherits(value, "POSIXt")) return("POSIXt")
    if (inherits(value, "difftime")) return("difftime")
    if (is.factor(value)) return("factor")
    if (is.numeric(value)) return("numeric")
    if (is.character(value)) return("character")
    if (is.logical(value)) return("logical")
    paste(class(value), collapse = "/")
  }
  family_a <- time_family(time_a)
  family_b <- time_family(time_b)
  if (!identical(family_a, family_b)) {
    stop("The two time columns must use compatible classes.", call. = FALSE)
  }
  if (identical(family_a, "difftime")) {
    # Resolve both sides in one unit system before deriving exact keys. This
    # removes the one-ULP asymmetry of algebraically equivalent conversions
    # such as hours * 60 versus minutes / 60 without rounding away genuinely
    # adjacent time values.
    time_b <- .trajectory_align_difftime_units(time_b, time_a)
  }
  combined <- tryCatch(c(time_a, time_b), error = function(e) NULL)
  if (is.null(combined)) {
    stop("The two time columns could not be combined.", call. = FALSE)
  }
  .trajectory_resolve_order(combined, order_values)
}

.trajectory_union_groups <- function(points_a, points_b, group_vars) {
  if (!length(group_vars)) {
    return(data.frame(.trajectory_no_group = 1L)[, FALSE, drop = FALSE])
  }
  groups_a <- .trajectory_group_template(points_a, group_vars)
  groups_b <- .trajectory_group_template(points_b, group_vars)
  groups <- rbind(groups_a, groups_b)
  groups[!duplicated(groups), , drop = FALSE]
}

.trajectory_paired_centroids <- function(pairs, group_count, period_count,
                                         coord_count, coord_a, coord_b) {
  grid_count <- group_count * period_count
  centroid_a <- matrix(NA_real_, grid_count, coord_count)
  centroid_b <- matrix(NA_real_, grid_count, coord_count)
  for (g in seq_len(group_count)) {
    for (t in seq_len(period_count)) {
      index <- (g - 1L) * period_count + t
      rows <- which(pairs$.group_index == g & pairs$.time_order == t)
      if (!length(rows)) next
      pair_weights <- pairs$.pair_weight[rows]
      for (j in seq_len(coord_count)) {
        centroid_a[index, j] <- .trajectory_stable_weighted_mean(
          pairs[[coord_a[j]]][rows], pair_weights
        )
        centroid_b[index, j] <- .trajectory_stable_weighted_mean(
          pairs[[coord_b[j]]][rows], pair_weights
        )
      }
    }
  }
  list(a = centroid_a, b = centroid_b)
}

.trajectory_comparison_sampling_plan <- function(
    pairs, group_template, group_vars,
    bootstrap_design = c("auto", "cluster", "stratified")
) {
  bootstrap_design <- match.arg(bootstrap_design)
  group_count <- max(1L, nrow(group_template))
  pools <- lapply(seq_len(group_count), function(g) {
    sort(unique(pairs$.id_key[pairs$.group_index == g]), method = "radix")
  })
  labels <- vapply(seq_len(group_count), function(g) {
    group_row <- if (length(group_vars)) group_template[g, , drop = FALSE] else NULL
    .trajectory_group_label(group_vars, group_row)
  }, character(1L))
  names(pools) <- labels
  nonempty_ids <- unlist(pools[lengths(pools) > 0L], use.names = FALSE)
  # `unlist(list(), use.names = FALSE)` returns NULL.  Normalise that empty
  # case before radix sorting so a valid comparison with zero matched IDs can
  # reach the explicit `no_matched_participants` diagnostic below instead of
  # failing with the opaque base error "argument 1 is not a vector".
  if (is.null(nonempty_ids)) nonempty_ids <- character(0)

  resolved_design <- bootstrap_design
  if (resolved_design == "auto") {
    membership <- table(nonempty_ids)
    disjoint_groups <- group_count > 1L && length(membership) &&
      all(membership == 1L)
    resolved_design <- if (disjoint_groups) "stratified" else "cluster"
  }
  if (group_count == 1L) resolved_design <- "cluster"

  list(
    pools = pools,
    labels = labels,
    design = resolved_design,
    requested_design = bootstrap_design,
    participant_ids = sort(unique(nonempty_ids), method = "radix"),
    n_sampling_units = if (resolved_design == "stratified") {
      sum(lengths(pools))
    } else {
      length(unique(nonempty_ids))
    }
  )
}

.trajectory_sample_pairs <- function(pairs, sampling_plan) {
  if (sampling_plan$design == "stratified") {
    chunks <- list()
    index <- 0L
    for (g in order(sampling_plan$labels, method = "radix")) {
      pool <- sampling_plan$pools[[g]]
      if (!length(pool)) next
      sampled_ids <- sample(pool, length(pool), replace = TRUE)
      for (id in sampled_ids) {
        rows <- pairs$.group_index == g & pairs$.id_key == id
        if (!any(rows)) next
        index <- index + 1L
        chunks[[index]] <- pairs[rows, , drop = FALSE]
      }
    }
  } else {
    pool <- sampling_plan$participant_ids
    sampled_ids <- sample(pool, length(pool), replace = TRUE)
    chunks <- lapply(sampled_ids, function(id) {
      pairs[pairs$.id_key == id, , drop = FALSE]
    })
  }
  if (!length(chunks)) return(pairs[FALSE, , drop = FALSE])
  sampled <- do.call(rbind, chunks)
  rownames(sampled) <- NULL
  sampled
}

.trajectory_group_delta <- function(values, group_count, period_count) {
  result <- rep(NA_real_, length(values))
  for (g in seq_len(group_count)) {
    rows <- ((g - 1L) * period_count + 1L):(g * period_count)
    if (period_count > 1L) result[rows[-1L]] <- diff(values[rows])
  }
  result
}

#' Compare two paired centroid paths
#'
#' The two point tables are matched by `id_var`, `time_var`, and `group_vars`
#' before either centroid is computed. Consequently, each reported difference
#' is based on the same participants on both sides and is invariant to row
#' order. Percentile intervals resample matched participant clusters and retain
#' all of each sampled participant's periods.
compare_centroid_paths <- function(
    points_a,
    points_b,
    time_var,
    id_var,
    group_vars = NULL,
    dimensions,
    order = NULL,
    cohort_policy = c("available", "complete"),
    weights_a = NULL,
    weights_b = NULL,
    na_policy = c("complete", "error"),
    distance_space = c("selected", "full"),
    full_dimensions = NULL,
    n_boot = 1000L,
    conf_level = 0.95,
    seed = NULL,
    labels = c("a", "b"),
    pair_weight_policy = c("require_equal", "geometric"),
    bootstrap_design = c("auto", "cluster", "stratified")
) {
  cohort_policy <- match.arg(cohort_policy)
  na_policy <- match.arg(na_policy)
  distance_space <- match.arg(distance_space)
  pair_weight_policy <- match.arg(pair_weight_policy)
  bootstrap_design <- match.arg(bootstrap_design)
  if (!is.character(labels) || length(labels) != 2L || anyNA(labels) ||
      any(!nzchar(labels)) || labels[1L] == labels[2L]) {
    stop("`labels` must contain two distinct non-empty labels.", call. = FALSE)
  }
  if (!is.numeric(n_boot) || length(n_boot) != 1L || is.na(n_boot) ||
      !is.finite(n_boot) || n_boot < 2 || n_boot != as.integer(n_boot)) {
    stop("`n_boot` must be one integer of at least 2.", call. = FALSE)
  }
  n_boot <- as.integer(n_boot)
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      is.na(conf_level) || !is.finite(conf_level) ||
      conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be strictly between 0 and 1.", call. = FALSE)
  }

  data_a <- .trajectory_copy_frame(points_a)
  data_b <- .trajectory_copy_frame(points_b)
  validation_a <- .trajectory_validate_common(
    data_a, time_var, id_var, group_vars, dimensions, weights_a, na_policy,
    distance_space, full_dimensions
  )
  validation_b <- .trajectory_validate_common(
    data_b, time_var, id_var, group_vars, dimensions, weights_b, na_policy,
    distance_space, full_dimensions
  )
  .trajectory_validate_output_names(
    validation_a$time_var,
    validation_a$group_vars,
    .trajectory_comparison_output_names(
      validation_a$dimensions, validation_a$time_var
    ),
    "Paired centroid-path comparison"
  )
  order_info <- .trajectory_resolve_order_two(
    data_a[[validation_a$time_var]], data_b[[validation_b$time_var]], order
  )
  group_template <- .trajectory_union_groups(
    data_a, data_b, validation_a$group_vars
  )
  if (length(validation_a$group_vars) && !nrow(group_template)) {
    stop("No non-missing grouping combinations are available.", call. = FALSE)
  }
  group_count <- max(1L, nrow(group_template))
  period_count <- length(order_info$values)
  grid_count <- group_count * period_count

  entity_a <- .trajectory_entity_table(data_a, validation_a, order_info,
                                        group_template)
  entity_b <- .trajectory_entity_table(data_b, validation_b, order_info,
                                        group_template)
  a <- entity_a$entities
  b <- entity_b$entities
  coord_count <- length(validation_a$analysis_dimensions)
  coord_a <- paste0(".a_coord_", seq_len(coord_count))
  coord_b <- paste0(".b_coord_", seq_len(coord_count))
  names(a)[match(entity_a$coord_names, names(a))] <- coord_a
  names(b)[match(entity_b$coord_names, names(b))] <- coord_b
  names(a)[names(a) == ".entity_weight"] <- ".weight_a"
  names(b)[names(b) == ".entity_weight"] <- ".weight_b"
  pairs <- merge(
    a, b,
    by = c(".group_index", ".time_order", ".id_key"),
    all = FALSE, sort = FALSE
  )
  if (pair_weight_policy == "require_equal" && nrow(pairs)) {
    tolerance <- sqrt(.Machine$double.eps) * pmax(
      1, abs(pairs$.weight_a), abs(pairs$.weight_b)
    )
    unequal <- abs(pairs$.weight_a - pairs$.weight_b) > tolerance
    if (any(unequal)) {
      stop(
        paste0(
          "Matched participant weights differ between sides. The default ",
          "`pair_weight_policy = \"require_equal\"` prevents an implicit ",
          "change of estimand; supply common weights or explicitly request ",
          "`pair_weight_policy = \"geometric\"`."
        ),
        call. = FALSE
      )
    }
    pairs$.pair_weight <- pairs$.weight_a
  } else if (nrow(pairs)) {
    pairs$.pair_weight <- .trajectory_stable_geometric_mean(
      pairs$.weight_a, pairs$.weight_b
    )
  } else {
    pairs$.pair_weight <- numeric(0)
  }

  n_matched <- integer(grid_count)
  for (g in seq_len(group_count)) {
    for (t in seq_len(period_count)) {
      index <- (g - 1L) * period_count + t
      n_matched[index] <- sum(pairs$.group_index == g &
                                pairs$.time_order == t)
    }
  }
  complete_ids <- vector("list", group_count)
  for (g in seq_len(group_count)) {
    ids_by_period <- lapply(seq_len(period_count), function(t) {
      unique(pairs$.id_key[pairs$.group_index == g & pairs$.time_order == t])
    })
    complete_ids[[g]] <- Reduce(intersect, ids_by_period)
  }
  if (cohort_policy == "complete" && nrow(pairs)) {
    keep <- rep(FALSE, nrow(pairs))
    for (g in seq_len(group_count)) {
      keep <- keep | (pairs$.group_index == g &
                        pairs$.id_key %in% complete_ids[[g]])
    }
    pairs <- pairs[keep, , drop = FALSE]
  }
  n_used <- integer(grid_count)
  for (g in seq_len(group_count)) {
    for (t in seq_len(period_count)) {
      index <- (g - 1L) * period_count + t
      n_used[index] <- sum(pairs$.group_index == g & pairs$.time_order == t)
    }
  }

  centroid <- .trajectory_paired_centroids(
    pairs, group_count, period_count, coord_count, coord_a, coord_b
  )
  selected_indices <- match(validation_a$dimensions,
                            validation_a$analysis_dimensions)
  distance_indices <- match(validation_a$distance_dimensions,
                            validation_a$analysis_dimensions)
  metrics_a <- .trajectory_path_metrics(
    centroid$a, distance_indices, group_count, period_count, order_info$values
  )
  metrics_b <- .trajectory_path_metrics(
    centroid$b, distance_indices, group_count, period_count, order_info$values
  )

  if (length(validation_a$group_vars)) {
    group_rows <- rep(seq_len(group_count), each = period_count)
    result <- group_template[group_rows, validation_a$group_vars, drop = FALSE]
    rownames(result) <- NULL
  } else {
    result <- data.frame(row.names = seq_len(grid_count))[, FALSE, drop = FALSE]
  }
  time_values <- rep(order_info$values, times = group_count)
  result[[validation_a$time_var]] <- time_values
  if (validation_a$time_var != "time_value") result$time_value <- time_values
  result$time_order <- rep(seq_len(period_count), times = group_count)
  result$n_a_total <- entity_a$stats$n_total
  result$n_b_total <- entity_b$stats$n_total
  result$n_a_valid <- entity_a$stats$n_valid
  result$n_b_valid <- entity_b$stats$n_valid
  result$n_a_rows_missing_key <- entity_a$stats$n_rows_missing_key
  result$n_b_rows_missing_key <- entity_b$stats$n_rows_missing_key
  result$n_matched <- n_matched
  result$n_used <- n_used
  result$n_unmatched_a <- pmax(0L, result$n_a_valid - result$n_matched)
  result$n_unmatched_b <- pmax(0L, result$n_b_valid - result$n_matched)
  result$n_dropped_a <- pmax(0L, result$n_a_total - result$n_a_valid)
  result$n_dropped_b <- pmax(0L, result$n_b_total - result$n_b_valid)
  result$n_cohort_excluded <- pmax(0L, result$n_matched - result$n_used)

  bootstrap_metric_names <- character()
  for (j in seq_along(validation_a$dimensions)) {
    dimension <- validation_a$dimensions[j]
    a_name <- paste0("centroid_a_", dimension)
    b_name <- paste0("centroid_b_", dimension)
    difference_name <- paste0("difference_", dimension)
    result[[a_name]] <- centroid$a[, selected_indices[j]]
    result[[b_name]] <- centroid$b[, selected_indices[j]]
    result[[difference_name]] <- result[[b_name]] - result[[a_name]]
    result[[paste0("delta_a_", dimension)]] <-
      .trajectory_group_delta(result[[a_name]], group_count, period_count)
    result[[paste0("delta_b_", dimension)]] <-
      .trajectory_group_delta(result[[b_name]], group_count, period_count)
    result[[paste0("delta_difference_", dimension)]] <-
      .trajectory_group_delta(result[[difference_name]], group_count,
                              period_count)
    bootstrap_metric_names <- c(bootstrap_metric_names, a_name, b_name,
                                difference_name)
  }
  distance_difference <- centroid$b[, distance_indices, drop = FALSE] -
    centroid$a[, distance_indices, drop = FALSE]
  result$centroid_difference_distance <- apply(
    distance_difference, 1L, function(x) {
      .trajectory_stable_norm(x)
    }
  )
  result$step_distance_a <- metrics_a$step_distance
  result$step_distance_b <- metrics_b$step_distance
  result$step_distance_difference <- result$step_distance_b -
    result$step_distance_a
  result$elapsed_interval <- metrics_a$elapsed_interval
  result$speed_a <- metrics_a$speed
  result$speed_b <- metrics_b$speed
  result$speed_difference <- result$speed_b - result$speed_a
  result$cumulative_distance_a <- metrics_a$cumulative_distance
  result$cumulative_distance_b <- metrics_b$cumulative_distance
  result$cumulative_distance_difference <- result$cumulative_distance_b -
    result$cumulative_distance_a
  bootstrap_metric_names <- c(
    bootstrap_metric_names, "centroid_difference_distance",
    "step_distance_a", "step_distance_b", "step_distance_difference",
    "speed_a", "speed_b", "speed_difference",
    "cumulative_distance_a", "cumulative_distance_b",
    "cumulative_distance_difference"
  )

  replicate_values <- lapply(bootstrap_metric_names, function(metric) {
    matrix(NA_real_, nrow = grid_count, ncol = n_boot)
  })
  names(replicate_values) <- bootstrap_metric_names
  # Sorting makes a seeded paired bootstrap invariant to raw row order and to
  # the implementation-specific ordering returned by merge().
  sampling_plan <- .trajectory_comparison_sampling_plan(
    pairs, group_template, validation_a$group_vars, bootstrap_design
  )
  participant_ids <- sampling_plan$participant_ids
  failed <- logical(n_boot)

  if (length(participant_ids)) {
    .trajectory_with_seed(seed, function() {
      for (boot in seq_len(n_boot)) {
        sampled_pairs <- .trajectory_sample_pairs(pairs, sampling_plan)
        boot_centroid <- .trajectory_paired_centroids(
          sampled_pairs, group_count, period_count, coord_count,
          coord_a, coord_b
        )
        boot_a <- .trajectory_path_metrics(
          boot_centroid$a, distance_indices, group_count, period_count,
          order_info$values
        )
        boot_b <- .trajectory_path_metrics(
          boot_centroid$b, distance_indices, group_count, period_count,
          order_info$values
        )
        boot_metrics <- list()
        for (j in seq_along(validation_a$dimensions)) {
          dimension <- validation_a$dimensions[j]
          a_name <- paste0("centroid_a_", dimension)
          b_name <- paste0("centroid_b_", dimension)
          difference_name <- paste0("difference_", dimension)
          boot_metrics[[a_name]] <- boot_centroid$a[, selected_indices[j]]
          boot_metrics[[b_name]] <- boot_centroid$b[, selected_indices[j]]
          boot_metrics[[difference_name]] <- boot_metrics[[b_name]] -
            boot_metrics[[a_name]]
        }
        boot_distance_difference <-
          boot_centroid$b[, distance_indices, drop = FALSE] -
          boot_centroid$a[, distance_indices, drop = FALSE]
        boot_metrics$centroid_difference_distance <- apply(
          boot_distance_difference, 1L, function(x) {
            .trajectory_stable_norm(x)
          }
        )
        boot_metrics$step_distance_a <- boot_a$step_distance
        boot_metrics$step_distance_b <- boot_b$step_distance
        boot_metrics$step_distance_difference <- boot_b$step_distance -
          boot_a$step_distance
        boot_metrics$speed_a <- boot_a$speed
        boot_metrics$speed_b <- boot_b$speed
        boot_metrics$speed_difference <- boot_b$speed - boot_a$speed
        boot_metrics$cumulative_distance_a <- boot_a$cumulative_distance
        boot_metrics$cumulative_distance_b <- boot_b$cumulative_distance
        boot_metrics$cumulative_distance_difference <-
          boot_b$cumulative_distance - boot_a$cumulative_distance
        for (metric in bootstrap_metric_names) {
          metric_values <- replicate_values[[metric]]
          metric_values[, boot] <- boot_metrics[[metric]]
          replicate_values[[metric]] <<- metric_values
        }
      }
      invisible(NULL)
    })
  } else {
    failed[] <- TRUE
  }

  alpha <- (1 - conf_level) / 2
  required_valid <- .trajectory_bootstrap_required_valid(n_boot, conf_level)
  cluster_failures <- matrix(
    FALSE, nrow = nrow(result), ncol = length(bootstrap_metric_names),
    dimnames = list(NULL, bootstrap_metric_names)
  )
  replicate_failures <- cluster_failures
  for (metric_index in seq_along(bootstrap_metric_names)) {
    metric <- bootstrap_metric_names[[metric_index]]
    values <- replicate_values[[metric]]
    boot_n <- rowSums(is.finite(values))
    cluster_ok <- .trajectory_metric_cluster_eligible(
      result, metric, validation_a$group_vars
    )
    base_finite <- is.finite(result[[metric]])
    replicate_ok <- boot_n >= required_valid
    interval_ok <- base_finite & cluster_ok & replicate_ok
    lower <- upper <- rep(NA_real_, nrow(result))
    if (any(interval_ok)) {
      lower[interval_ok] <- apply(
        values[interval_ok, , drop = FALSE], 1L,
        .trajectory_quantile, probability = alpha
      )
      upper[interval_ok] <- apply(
        values[interval_ok, , drop = FALSE], 1L,
        .trajectory_quantile, probability = 1 - alpha
      )
    }
    result[[paste0(metric, "_lower")]] <- lower
    result[[paste0(metric, "_upper")]] <- upper
    result[[paste0(metric, "_boot_n")]] <- boot_n
    cluster_failures[, metric_index] <- base_finite & !cluster_ok
    replicate_failures[, metric_index] <- base_finite & cluster_ok & !replicate_ok
  }
  bootstrap_diagnostics <- .trajectory_bootstrap_diagnostics(
    result, validation_a$group_vars, cluster_failures, replicate_failures,
    required_valid, n_boot
  )

  diagnostics <- list()
  add_diagnostic <- function(code, severity, group, time_order, message, count) {
    diagnostics[[length(diagnostics) + 1L]] <<- data.frame(
      code = code, severity = severity, group = group,
      time_order = as.integer(time_order), message = message,
      count = as.integer(count), stringsAsFactors = FALSE
    )
  }
  if (order_info$implicit_character) {
    add_diagnostic("implicit_character_order", "warning", "all", NA_integer_,
                   "Character time values use stable first-appearance order; supply `order` for a substantive sequence.",
                   period_count)
  }
  missing_key_total_a <- sum(validation_a$bad_key)
  missing_key_total_b <- sum(validation_b$bad_key)
  missing_key_assigned_a <- sum(result$n_a_rows_missing_key)
  missing_key_assigned_b <- sum(result$n_b_rows_missing_key)
  missing_key_unassigned_a <- missing_key_total_a - missing_key_assigned_a
  missing_key_unassigned_b <- missing_key_total_b - missing_key_assigned_b
  if (missing_key_total_a > 0L) {
    add_diagnostic(
      "missing_key_rows_a", "warning", labels[1L], NA_integer_,
      paste0(
        "Side A rows with missing/non-finite ID, time, or group keys were ",
        "excluded; slice-assignable rows are exposed by ",
        "`n_a_rows_missing_key`."
      ),
      missing_key_total_a
    )
  }
  if (missing_key_total_b > 0L) {
    add_diagnostic(
      "missing_key_rows_b", "warning", labels[2L], NA_integer_,
      paste0(
        "Side B rows with missing/non-finite ID, time, or group keys were ",
        "excluded; slice-assignable rows are exposed by ",
        "`n_b_rows_missing_key`."
      ),
      missing_key_total_b
    )
  }
  if (missing_key_unassigned_a > 0L) {
    add_diagnostic(
      "unassigned_missing_key_rows_a", "warning", labels[1L], NA_integer_,
      "Side A rows missing a time or group key cannot be assigned to a requested group-period slice.",
      missing_key_unassigned_a
    )
  }
  if (missing_key_unassigned_b > 0L) {
    add_diagnostic(
      "unassigned_missing_key_rows_b", "warning", labels[2L], NA_integer_,
      "Side B rows missing a time or group key cannot be assigned to a requested group-period slice.",
      missing_key_unassigned_b
    )
  }
  if (any(result$n_unmatched_a + result$n_unmatched_b > 0L)) {
    add_diagnostic("unmatched_participants", "warning", "all", NA_integer_,
                   "Participants present on only one side were excluded before paired centroid calculation.",
                   sum(result$n_unmatched_a + result$n_unmatched_b))
  }
  if (any(result$n_dropped_a + result$n_dropped_b > 0L)) {
    add_diagnostic("dropped_invalid_pairs", "warning", "all", NA_integer_,
                   "Participants with invalid analytical values on either side were excluded.",
                   sum(result$n_dropped_a + result$n_dropped_b))
  }
  if (!length(participant_ids)) {
    add_diagnostic("no_matched_participants", "warning", "all", NA_integer_,
                   "No valid participant IDs matched between the two paths.", 0L)
  }
  if (pair_weight_policy == "geometric") {
    add_diagnostic(
      "geometric_pair_weights", "warning", "all", NA_integer_,
      paste0(
        "Matched-side weights were combined with an explicitly requested ",
        "geometric mean; this estimand can differ from either standalone ",
        "weighted path."
      ),
      nrow(pairs)
    )
  }
  for (g in seq_len(group_count)) {
    group_row <- if (length(validation_a$group_vars)) {
      group_template[g, , drop = FALSE]
    } else {
      NULL
    }
    label <- .trajectory_group_label(validation_a$group_vars, group_row)
    group_indices <- ((g - 1L) * period_count + 1L):(g * period_count)
    used_ids_by_period <- lapply(seq_len(period_count), function(t) {
      unique(pairs$.id_key[
        pairs$.group_index == g & pairs$.time_order == t
      ])
    })
    for (t in seq_len(period_count)) {
      index <- group_indices[t]
      paired_slice <- pairs$.group_index == g & pairs$.time_order == t
      distance_incomplete_count <- if (distance_space == "full" &&
                                       any(paired_slice)) {
        sum(!pairs$.distance_complete.x[paired_slice] |
              !pairs$.distance_complete.y[paired_slice])
      } else {
        0L
      }
      if (result$n_used[index] == 0L) {
        add_diagnostic(
          "missing_paired_period", "warning", label, t,
          "No matched participant pair is available for this requested period.",
          1L
        )
      } else if (result$n_used[index] == 1L) {
        add_diagnostic(
          "one_pair_slice", "warning", label, t,
          "The paired comparison is defined by one matched participant.",
          1L
        )
      }
      if (distance_incomplete_count > 0L) {
        add_diagnostic(
          "full_distance_incomplete", "warning", label, t,
          paste0(
            "Selected-axis paired centroids are retained, but full-space ",
            "comparison distances are unavailable because the matched cohort ",
            "has incomplete full-rotation coordinates."
          ),
          distance_incomplete_count
        )
      }
    }
    if (cohort_policy == "available" && period_count > 1L) {
      signatures <- vapply(used_ids_by_period, function(ids) {
        paste(sort(unique(ids)), collapse = "\r")
      }, character(1L))
      if (length(unique(signatures)) > 1L) {
        add_diagnostic(
          "changing_matched_cohort", "warning", label, NA_integer_,
          paste0(
            "The matched participant composition changes across requested ",
            "periods under the available-cohort policy."
          ),
          length(unique(signatures))
        )
      }
    }
  }
  diagnostics <- .trajectory_diagnostics_frame(diagnostics)
  if (nrow(bootstrap_diagnostics)) {
    diagnostics <- rbind(diagnostics, bootstrap_diagnostics)
    rownames(diagnostics) <- NULL
  }

  class(result) <- c("paired_centroid_path_comparison", "data.frame")
  attr(result, "trajectory_warnings") <- diagnostics
  attr(result, "comparison_spec") <- list(
    labels = labels,
    time_var = validation_a$time_var,
    id_var = validation_a$id_var,
    group_vars = validation_a$group_vars,
    dimensions = validation_a$dimensions,
    order_values = order_info$values,
    order_source = order_info$source,
    cohort_policy = cohort_policy,
    na_policy = na_policy,
    matching = "exact id + time + group before centroid calculation",
    paired_weight_policy = pair_weight_policy,
    paired_weight = if (pair_weight_policy == "require_equal") {
      "equal matched-side participant weight"
    } else {
      "geometric mean of the two participant weights"
    },
    distance_space = distance_space,
    distance_dimensions = validation_a$distance_dimensions,
    elapsed_interval_units = metrics_a$elapsed_units,
    missing_key_policy = paste0(
      "exclude rows with missing/non-finite ID, time, or group keys; expose ",
      "slice-assignable rows in side-specific counts and report rows lacking ",
      "time/group assignment globally"
    ),
    missing_key_counts = list(
      a = list(
        total = missing_key_total_a,
        slice_assigned = missing_key_assigned_a,
        unassigned = missing_key_unassigned_a
      ),
      b = list(
        total = missing_key_total_b,
        slice_assigned = missing_key_assigned_b,
        unassigned = missing_key_unassigned_b
      )
    ),
    difference_direction = paste0(labels[2L], " - ", labels[1L])
  )
  attr(result, "bootstrap_spec") <- list(
    method = "matched-participant cluster percentile bootstrap",
    sampling_unit = validation_a$id_var,
    rows_per_sampled_id = "all matched participant-period rows",
    n_participants = length(participant_ids),
    n_sampling_units = sampling_plan$n_sampling_units,
    bootstrap_design_requested = sampling_plan$requested_design,
    bootstrap_design = sampling_plan$design,
    stratum_sizes = stats::setNames(
      lengths(sampling_plan$pools), sampling_plan$labels
    ),
    eligible_id_keys = participant_ids,
    eligible_id_keys_by_stratum = sampling_plan$pools,
    n_boot = n_boot,
    conf_level = conf_level,
    minimum_valid_fraction = 0.80,
    minimum_tail_replicates = 5L,
    minimum_valid_replicates = required_valid,
    seed = seed,
    failed_replicates = sum(failed),
    rng_state_restored = !is.null(seed)
  )
  .trajectory_emit_diagnostics(diagnostics)
  result
}

.trajectory_sort_group_template <- function(group_template, group_vars) {
  if (!length(group_vars) || nrow(group_template) < 2L) {
    return(group_template)
  }
  keys <- lapply(group_template[group_vars], .trajectory_value_key)
  ordering <- do.call(order, c(keys, list(method = "radix")))
  group_template[ordering, , drop = FALSE]
}

.trajectory_prepare_independent_entities <- function(
    entities, group_count, period_count, cohort_policy
) {
  pools <- vector("list", group_count)
  used_ids <- vector("list", group_count * period_count)
  keep <- rep(FALSE, nrow(entities))

  for (g in seq_len(group_count)) {
    ids_by_period <- lapply(seq_len(period_count), function(t) {
      unique(entities$.id_key[
        entities$.group_index == g & entities$.time_order == t
      ])
    })
    eligible <- if (cohort_policy == "complete") {
      Reduce(intersect, ids_by_period)
    } else {
      unique(unlist(ids_by_period, use.names = FALSE))
    }
    pools[[g]] <- sort(eligible, method = "radix")
    keep <- keep | (entities$.group_index == g &
                      entities$.id_key %in% eligible)
  }

  entities <- entities[keep, , drop = FALSE]
  if (nrow(entities)) {
    entity_order <- order(
      entities$.group_index, entities$.time_order, entities$.id_key,
      method = "radix"
    )
    entities <- entities[entity_order, , drop = FALSE]
    rownames(entities) <- NULL
  }

  n_used <- integer(group_count * period_count)
  n_distance_incomplete <- integer(group_count * period_count)
  for (g in seq_len(group_count)) {
    for (t in seq_len(period_count)) {
      index <- (g - 1L) * period_count + t
      rows <- entities$.group_index == g & entities$.time_order == t
      used_ids[[index]] <- entities$.id_key[rows]
      n_used[index] <- sum(rows)
      n_distance_incomplete[index] <- sum(
        rows & !entities$.distance_complete
      )
    }
  }

  list(
    entities = entities,
    pools = pools,
    used_ids = used_ids,
    n_used = n_used,
    n_distance_incomplete = n_distance_incomplete
  )
}

.trajectory_side_centroids <- function(entities, group_count, period_count,
                                       coord_names) {
  centroids <- matrix(
    NA_real_, nrow = group_count * period_count,
    ncol = length(coord_names)
  )
  for (g in seq_len(group_count)) {
    for (t in seq_len(period_count)) {
      index <- (g - 1L) * period_count + t
      rows <- which(entities$.group_index == g &
                      entities$.time_order == t)
      if (!length(rows)) next
      for (j in seq_along(coord_names)) {
        centroids[index, j] <- .trajectory_stable_weighted_mean(
          entities[[coord_names[j]]][rows], entities$.entity_weight[rows]
        )
      }
    }
  }
  centroids
}

.trajectory_independent_metric_values <- function(
    centroid_a, centroid_b, dimensions, selected_indices, distance_indices,
    group_count, period_count, order_values
) {
  metrics_a <- .trajectory_path_metrics(
    centroid_a, distance_indices, group_count, period_count, order_values
  )
  metrics_b <- .trajectory_path_metrics(
    centroid_b, distance_indices, group_count, period_count, order_values
  )
  values <- list()

  for (j in seq_along(dimensions)) {
    dimension <- dimensions[j]
    a_name <- paste0("centroid_a_", dimension)
    b_name <- paste0("centroid_b_", dimension)
    difference_name <- paste0("difference_", dimension)
    delta_a_name <- paste0("delta_a_", dimension)
    delta_b_name <- paste0("delta_b_", dimension)
    delta_difference_name <- paste0("delta_difference_", dimension)
    values[[a_name]] <- centroid_a[, selected_indices[j]]
    values[[b_name]] <- centroid_b[, selected_indices[j]]
    values[[difference_name]] <- values[[b_name]] - values[[a_name]]
    values[[delta_a_name]] <- .trajectory_group_delta(
      values[[a_name]], group_count, period_count
    )
    values[[delta_b_name]] <- .trajectory_group_delta(
      values[[b_name]], group_count, period_count
    )
    values[[delta_difference_name]] <-
      values[[delta_b_name]] - values[[delta_a_name]]
  }

  coordinate_difference <-
    centroid_b[, distance_indices, drop = FALSE] -
    centroid_a[, distance_indices, drop = FALSE]
  values$centroid_difference_distance <- apply(
    coordinate_difference, 1L, .trajectory_stable_norm
  )
  values$step_distance_a <- metrics_a$step_distance
  values$step_distance_b <- metrics_b$step_distance
  values$step_distance_difference <-
    metrics_b$step_distance - metrics_a$step_distance
  values$elapsed_interval <- metrics_a$elapsed_interval
  values$speed_a <- metrics_a$speed
  values$speed_b <- metrics_b$speed
  values$speed_difference <- metrics_b$speed - metrics_a$speed
  values$cumulative_distance_a <- metrics_a$cumulative_distance
  values$cumulative_distance_b <- metrics_b$cumulative_distance
  values$cumulative_distance_difference <-
    metrics_b$cumulative_distance - metrics_a$cumulative_distance

  test_names <- c(
    paste0("difference_", dimensions),
    paste0("delta_difference_", dimensions),
    "centroid_difference_distance", "step_distance_difference",
    "speed_difference", "cumulative_distance_difference"
  )
  list(
    values = values,
    bootstrap_names = setdiff(names(values), "elapsed_interval"),
    test_names = test_names,
    elapsed_units = metrics_a$elapsed_units
  )
}

.trajectory_sample_independent_entities <- function(entities, pools,
                                                    group_labels) {
  chunks <- list()
  chunk_index <- 0L
  group_order <- order(group_labels, method = "radix")
  for (g in group_order) {
    pool <- pools[[g]]
    if (!length(pool)) next
    sampled <- sample(pool, length(pool), replace = TRUE)
    for (id_key in sampled) {
      rows <- entities$.group_index == g & entities$.id_key == id_key
      if (!any(rows)) next
      chunk_index <- chunk_index + 1L
      chunks[[chunk_index]] <- entities[rows, , drop = FALSE]
    }
  }
  if (!length(chunks)) return(entities[FALSE, , drop = FALSE])
  sampled <- do.call(rbind, chunks)
  rownames(sampled) <- NULL
  sampled
}

.trajectory_permute_independent_entities <- function(
    entities_a, entities_b, pools_a, pools_b, group_labels
) {
  chunks_a <- list()
  chunks_b <- list()
  index_a <- 0L
  index_b <- 0L
  group_order <- order(group_labels, method = "radix")

  for (g in group_order) {
    clusters_a <- lapply(pools_a[[g]], function(id_key) {
      entities_a[entities_a$.group_index == g &
                   entities_a$.id_key == id_key, , drop = FALSE]
    })
    clusters_b <- lapply(pools_b[[g]], function(id_key) {
      entities_b[entities_b$.group_index == g &
                   entities_b$.id_key == id_key, , drop = FALSE]
    })
    clusters <- c(clusters_a, clusters_b)
    n_a <- length(clusters_a)
    total <- length(clusters)
    if (!total) next
    assigned_a <- if (n_a) {
      sample.int(total, n_a, replace = FALSE)
    } else {
      integer(0)
    }
    assigned_b <- setdiff(seq_len(total), assigned_a)
    for (cluster in clusters[assigned_a]) {
      index_a <- index_a + 1L
      chunks_a[[index_a]] <- cluster
    }
    for (cluster in clusters[assigned_b]) {
      index_b <- index_b + 1L
      chunks_b[[index_b]] <- cluster
    }
  }

  permuted_a <- if (length(chunks_a)) {
    do.call(rbind, chunks_a)
  } else {
    entities_a[FALSE, , drop = FALSE]
  }
  permuted_b <- if (length(chunks_b)) {
    do.call(rbind, chunks_b)
  } else {
    entities_b[FALSE, , drop = FALSE]
  }
  rownames(permuted_a) <- NULL
  rownames(permuted_b) <- NULL
  list(a = permuted_a, b = permuted_b)
}

.trajectory_independent_metric_eligible <- function(data, metric,
                                                    group_vars) {
  side_a <- grepl(
    "^(centroid_a_|delta_a_|step_distance_a$|speed_a$|cumulative_distance_a$)",
    metric
  )
  side_b <- grepl(
    "^(centroid_b_|delta_b_|step_distance_b$|speed_b$|cumulative_distance_b$)",
    metric
  )
  slice_ok <- if (side_a) {
    is.finite(data$n_a_used) & data$n_a_used >= 2L
  } else if (side_b) {
    is.finite(data$n_b_used) & data$n_b_used >= 2L
  } else {
    is.finite(data$n_a_used) & data$n_a_used >= 2L &
      is.finite(data$n_b_used) & data$n_b_used >= 2L
  }
  eligible <- slice_ok
  interval_metric <- grepl("^(delta_|step_distance|speed)", metric)
  cumulative_metric <- grepl("^cumulative_distance", metric)

  if (interval_metric || cumulative_metric) {
    eligible[] <- FALSE
    for (rows in .trajectory_group_row_sets(data, group_vars)) {
      rows <- rows[order(data$time_order[rows], method = "radix")]
      if (cumulative_metric) {
        eligible[rows] <- as.logical(cumprod(slice_ok[rows]))
      } else {
        eligible[rows[1L]] <- slice_ok[rows[1L]]
        if (length(rows) > 1L) {
          eligible[rows[-1L]] <- slice_ok[rows[-1L]] &
            slice_ok[rows[-length(rows)]]
        }
      }
    }
  }
  eligible
}

.trajectory_independent_structural_test <- function(data, metric,
                                                    group_vars) {
  testable <- rep(TRUE, nrow(data))
  interval_metric <- grepl(
    "^(delta_difference_|step_distance_difference$|speed_difference$|cumulative_distance_difference$)",
    metric
  )
  if (interval_metric) {
    for (rows in .trajectory_group_row_sets(data, group_vars)) {
      rows <- rows[order(data$time_order[rows], method = "radix")]
      testable[rows[1L]] <- FALSE
    }
  }
  testable
}

.trajectory_permutation_required_valid <- function(n_perm, alpha) {
  max(
    ceiling(0.80 * n_perm),
    .trajectory_roundoff_safe_ceiling(1 / alpha)
  )
}

.trajectory_permutation_diagnostics <- function(
    data, group_vars, cluster_failures, replicate_failures,
    required_valid, n_perm
) {
  diagnostics <- list()
  add_rows <- function(code, failures, message) {
    affected <- which(rowSums(failures) > 0L)
    if (!length(affected)) return(invisible(NULL))
    for (row in affected) {
      group_row <- if (length(group_vars)) {
        data[row, group_vars, drop = FALSE]
      } else {
        NULL
      }
      diagnostics[[length(diagnostics) + 1L]] <<- data.frame(
        code = code,
        severity = "warning",
        group = .trajectory_group_label(group_vars, group_row),
        time_order = as.integer(data$time_order[row]),
        message = message,
        count = as.integer(sum(failures[row, ])),
        stringsAsFactors = FALSE
      )
    }
    invisible(NULL)
  }
  add_rows(
    "permutation_insufficient_clusters",
    cluster_failures,
    paste0(
      "Permutation p-values are unavailable for one or more contrasts because ",
      "at least one independent side has fewer than two participant clusters."
    )
  )
  add_rows(
    "permutation_insufficient_replicates",
    replicate_failures,
    paste0(
      "Permutation p-values are unavailable for one or more contrasts because ",
      "fewer than ", required_valid, " of ", n_perm,
      " finite label permutations were available."
    )
  )
  .trajectory_diagnostics_frame(diagnostics)
}

#' Compare two independent centroid paths
#'
#' Participant IDs are interpreted in separate side-specific namespaces: an
#' ID written identically in `points_a` and `points_b` is not matched or paired.
#' Percentile confidence intervals resample participant clusters independently
#' within each side and trajectory group, retaining all eligible periods for
#' every sampled participant. P-values come from participant-cluster label
#' permutations that preserve the original side sizes within each trajectory
#' group. Signed contrasts use two-sided absolute statistics; centroid
#' separation uses an upper-tail distance statistic. `p_adjust_method =
#' "holm"` controls family-wise error across all finite contrasts returned by
#' the call. Supplying `seed` makes both resampling stages deterministic and
#' restores the caller's RNG state.
compare_independent_centroid_paths <- function(
    points_a,
    points_b,
    time_var,
    id_var,
    group_vars = NULL,
    dimensions,
    order = NULL,
    cohort_policy = c("available", "complete"),
    weights_a = NULL,
    weights_b = NULL,
    na_policy = c("complete", "error"),
    distance_space = c("selected", "full"),
    full_dimensions = NULL,
    n_boot = 1000L,
    n_perm = 999L,
    conf_level = 0.95,
    seed = NULL,
    labels = c("a", "b"),
    p_adjust_method = "holm"
) {
  cohort_policy <- match.arg(cohort_policy)
  na_policy <- match.arg(na_policy)
  distance_space <- match.arg(distance_space)
  p_adjust_method <- match.arg(p_adjust_method, stats::p.adjust.methods)
  if (!is.character(labels) || length(labels) != 2L || anyNA(labels) ||
      any(!nzchar(labels)) || labels[1L] == labels[2L]) {
    stop("`labels` must contain two distinct non-empty labels.", call. = FALSE)
  }
  validate_repetitions <- function(value, argument) {
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        !is.finite(value) || value < 2 || value != as.integer(value)) {
      stop(sprintf("`%s` must be one integer of at least 2.", argument),
           call. = FALSE)
    }
    as.integer(value)
  }
  n_boot <- validate_repetitions(n_boot, "n_boot")
  n_perm <- validate_repetitions(n_perm, "n_perm")
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      is.na(conf_level) || !is.finite(conf_level) ||
      conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be strictly between 0 and 1.", call. = FALSE)
  }

  data_a <- .trajectory_copy_frame(points_a)
  data_b <- .trajectory_copy_frame(points_b)
  validation_a <- .trajectory_validate_common(
    data_a, time_var, id_var, group_vars, dimensions, weights_a, na_policy,
    distance_space, full_dimensions
  )
  validation_b <- .trajectory_validate_common(
    data_b, time_var, id_var, group_vars, dimensions, weights_b, na_policy,
    distance_space, full_dimensions
  )
  .trajectory_validate_output_names(
    validation_a$time_var,
    validation_a$group_vars,
    .trajectory_independent_comparison_output_names(
      validation_a$dimensions, validation_a$time_var
    ),
    "Independent centroid-path comparison"
  )
  order_info <- .trajectory_resolve_order_two(
    data_a[[validation_a$time_var]], data_b[[validation_b$time_var]], order
  )
  group_template <- .trajectory_sort_group_template(
    .trajectory_union_groups(data_a, data_b, validation_a$group_vars),
    validation_a$group_vars
  )
  if (length(validation_a$group_vars) && !nrow(group_template)) {
    stop("No non-missing grouping combinations are available.", call. = FALSE)
  }
  group_count <- max(1L, nrow(group_template))
  period_count <- length(order_info$values)
  grid_count <- group_count * period_count
  group_labels <- vapply(seq_len(group_count), function(g) {
    group_row <- if (length(validation_a$group_vars)) {
      group_template[g, , drop = FALSE]
    } else {
      NULL
    }
    .trajectory_group_label(validation_a$group_vars, group_row)
  }, character(1L))

  entity_a <- .trajectory_entity_table(
    data_a, validation_a, order_info, group_template
  )
  entity_b <- .trajectory_entity_table(
    data_b, validation_b, order_info, group_template
  )
  prepared_a <- .trajectory_prepare_independent_entities(
    entity_a$entities, group_count, period_count, cohort_policy
  )
  prepared_b <- .trajectory_prepare_independent_entities(
    entity_b$entities, group_count, period_count, cohort_policy
  )
  names(prepared_a$pools) <- group_labels
  names(prepared_b$pools) <- group_labels
  coord_names <- entity_a$coord_names
  centroid_a <- .trajectory_side_centroids(
    prepared_a$entities, group_count, period_count, coord_names
  )
  centroid_b <- .trajectory_side_centroids(
    prepared_b$entities, group_count, period_count, coord_names
  )
  selected_indices <- match(
    validation_a$dimensions, validation_a$analysis_dimensions
  )
  distance_indices <- match(
    validation_a$distance_dimensions, validation_a$analysis_dimensions
  )
  base_metrics <- .trajectory_independent_metric_values(
    centroid_a, centroid_b, validation_a$dimensions, selected_indices,
    distance_indices, group_count, period_count, order_info$values
  )

  if (length(validation_a$group_vars)) {
    group_rows <- rep(seq_len(group_count), each = period_count)
    result <- group_template[
      group_rows, validation_a$group_vars, drop = FALSE
    ]
    rownames(result) <- NULL
  } else {
    result <- data.frame(row.names = seq_len(grid_count))[, FALSE, drop = FALSE]
  }
  time_values <- rep(order_info$values, times = group_count)
  result[[validation_a$time_var]] <- time_values
  if (validation_a$time_var != "time_value") result$time_value <- time_values
  result$time_order <- rep(seq_len(period_count), times = group_count)
  result$n_a_total <- entity_a$stats$n_total
  result$n_b_total <- entity_b$stats$n_total
  result$n_a_valid <- entity_a$stats$n_valid
  result$n_b_valid <- entity_b$stats$n_valid
  result$n_a_used <- prepared_a$n_used
  result$n_b_used <- prepared_b$n_used
  result$n_a_rows_missing_key <- entity_a$stats$n_rows_missing_key
  result$n_b_rows_missing_key <- entity_b$stats$n_rows_missing_key
  result$n_a_dropped <- pmax(0L, result$n_a_total - result$n_a_valid)
  result$n_b_dropped <- pmax(0L, result$n_b_total - result$n_b_valid)
  result$n_a_cohort_excluded <- pmax(
    0L, result$n_a_valid - result$n_a_used
  )
  result$n_b_cohort_excluded <- pmax(
    0L, result$n_b_valid - result$n_b_used
  )
  result$n_a_distance_incomplete <- prepared_a$n_distance_incomplete
  result$n_b_distance_incomplete <- prepared_b$n_distance_incomplete
  for (metric in names(base_metrics$values)) {
    result[[metric]] <- base_metrics$values[[metric]]
  }

  bootstrap_values <- lapply(base_metrics$bootstrap_names, function(metric) {
    matrix(NA_real_, nrow = grid_count, ncol = n_boot)
  })
  names(bootstrap_values) <- base_metrics$bootstrap_names
  permutation_values <- lapply(base_metrics$test_names, function(metric) {
    matrix(NA_real_, nrow = grid_count, ncol = n_perm)
  })
  names(permutation_values) <- base_metrics$test_names

  resampling <- .trajectory_with_seed(seed, function() {
    failed_bootstrap <- logical(n_boot)
    failed_permutation <- logical(n_perm)
    for (boot in seq_len(n_boot)) {
      sampled_a <- .trajectory_sample_independent_entities(
        prepared_a$entities, prepared_a$pools, group_labels
      )
      sampled_b <- .trajectory_sample_independent_entities(
        prepared_b$entities, prepared_b$pools, group_labels
      )
      candidate <- tryCatch({
        boot_centroid_a <- .trajectory_side_centroids(
          sampled_a, group_count, period_count, coord_names
        )
        boot_centroid_b <- .trajectory_side_centroids(
          sampled_b, group_count, period_count, coord_names
        )
        .trajectory_independent_metric_values(
          boot_centroid_a, boot_centroid_b, validation_a$dimensions,
          selected_indices, distance_indices, group_count, period_count,
          order_info$values
        )$values
      }, error = function(e) NULL)
      if (is.null(candidate)) {
        failed_bootstrap[boot] <- TRUE
        next
      }
      for (metric in base_metrics$bootstrap_names) {
        bootstrap_values[[metric]][, boot] <- candidate[[metric]]
      }
    }

    for (permutation in seq_len(n_perm)) {
      permuted <- .trajectory_permute_independent_entities(
        prepared_a$entities, prepared_b$entities,
        prepared_a$pools, prepared_b$pools, group_labels
      )
      candidate <- tryCatch({
        perm_centroid_a <- .trajectory_side_centroids(
          permuted$a, group_count, period_count, coord_names
        )
        perm_centroid_b <- .trajectory_side_centroids(
          permuted$b, group_count, period_count, coord_names
        )
        .trajectory_independent_metric_values(
          perm_centroid_a, perm_centroid_b, validation_a$dimensions,
          selected_indices, distance_indices, group_count, period_count,
          order_info$values
        )$values
      }, error = function(e) NULL)
      if (is.null(candidate)) {
        failed_permutation[permutation] <- TRUE
        next
      }
      for (metric in base_metrics$test_names) {
        permutation_values[[metric]][, permutation] <- candidate[[metric]]
      }
    }
    list(
      bootstrap_values = bootstrap_values,
      permutation_values = permutation_values,
      failed_bootstrap = failed_bootstrap,
      failed_permutation = failed_permutation
    )
  })
  bootstrap_values <- resampling$bootstrap_values
  permutation_values <- resampling$permutation_values

  alpha_interval <- (1 - conf_level) / 2
  significance_level <- 1 - conf_level
  required_bootstrap <- .trajectory_bootstrap_required_valid(
    n_boot, conf_level
  )
  bootstrap_cluster_failures <- matrix(
    FALSE, nrow = grid_count, ncol = length(base_metrics$bootstrap_names),
    dimnames = list(NULL, base_metrics$bootstrap_names)
  )
  bootstrap_replicate_failures <- bootstrap_cluster_failures
  for (metric_index in seq_along(base_metrics$bootstrap_names)) {
    metric <- base_metrics$bootstrap_names[[metric_index]]
    values <- bootstrap_values[[metric]]
    boot_n <- rowSums(is.finite(values))
    cluster_ok <- .trajectory_independent_metric_eligible(
      result, metric, validation_a$group_vars
    )
    base_finite <- is.finite(result[[metric]])
    replicate_ok <- boot_n >= required_bootstrap
    interval_ok <- base_finite & cluster_ok & replicate_ok
    lower <- upper <- rep(NA_real_, grid_count)
    if (any(interval_ok)) {
      lower[interval_ok] <- apply(
        values[interval_ok, , drop = FALSE], 1L,
        .trajectory_quantile, probability = alpha_interval
      )
      upper[interval_ok] <- apply(
        values[interval_ok, , drop = FALSE], 1L,
        .trajectory_quantile, probability = 1 - alpha_interval
      )
    }
    result[[paste0(metric, "_lower")]] <- lower
    result[[paste0(metric, "_upper")]] <- upper
    result[[paste0(metric, "_boot_n")]] <- boot_n
    bootstrap_cluster_failures[, metric_index] <-
      base_finite & !cluster_ok
    bootstrap_replicate_failures[, metric_index] <-
      base_finite & cluster_ok & !replicate_ok
  }
  bootstrap_diagnostics <- .trajectory_bootstrap_diagnostics(
    result, validation_a$group_vars, bootstrap_cluster_failures,
    bootstrap_replicate_failures, required_bootstrap, n_boot
  )

  required_permutation <- .trajectory_permutation_required_valid(
    n_perm, significance_level
  )
  permutation_cluster_failures <- matrix(
    FALSE, nrow = grid_count, ncol = length(base_metrics$test_names),
    dimnames = list(NULL, base_metrics$test_names)
  )
  permutation_replicate_failures <- permutation_cluster_failures
  raw_p_values <- vector("list", length(base_metrics$test_names))
  names(raw_p_values) <- base_metrics$test_names
  for (metric_index in seq_along(base_metrics$test_names)) {
    metric <- base_metrics$test_names[[metric_index]]
    values <- permutation_values[[metric]]
    perm_n <- rowSums(is.finite(values))
    structural <- .trajectory_independent_structural_test(
      result, metric, validation_a$group_vars
    )
    perm_n[!structural] <- 0L
    cluster_ok <- .trajectory_independent_metric_eligible(
      result, metric, validation_a$group_vars
    )
    base_finite <- is.finite(result[[metric]])
    replicate_ok <- perm_n >= required_permutation
    test_ok <- structural & base_finite & cluster_ok & replicate_ok
    p_value <- rep(NA_real_, grid_count)
    for (row in which(test_ok)) {
      null_values <- values[row, is.finite(values[row, ])]
      if (metric == "centroid_difference_distance") {
        observed_statistic <- result[[metric]][row]
        null_statistics <- null_values
      } else {
        observed_statistic <- abs(result[[metric]][row])
        null_statistics <- abs(null_values)
      }
      p_value[row] <-
        (1 + sum(null_statistics >= observed_statistic)) /
        (length(null_statistics) + 1)
    }
    raw_p_values[[metric]] <- p_value
    result[[paste0(metric, "_p_value")]] <- p_value
    result[[paste0(metric, "_perm_n")]] <- perm_n
    permutation_cluster_failures[, metric_index] <-
      structural & base_finite & !cluster_ok
    permutation_replicate_failures[, metric_index] <-
      structural & base_finite & cluster_ok & !replicate_ok
  }

  finite_p_count <- sum(vapply(
    raw_p_values, function(values) sum(is.finite(values)), integer(1L)
  ))
  flattened_p <- unlist(lapply(raw_p_values, function(values) {
    values[is.finite(values)]
  }), use.names = FALSE)
  adjusted_p <- if (finite_p_count) {
    stats::p.adjust(flattened_p, method = p_adjust_method)
  } else {
    numeric(0)
  }
  adjusted_index <- 0L
  for (metric in base_metrics$test_names) {
    raw <- raw_p_values[[metric]]
    finite <- is.finite(raw)
    adjusted <- rep(NA_real_, grid_count)
    if (any(finite)) {
      positions <- adjusted_index + seq_len(sum(finite))
      adjusted[finite] <- adjusted_p[positions]
      adjusted_index <- adjusted_index + sum(finite)
    }
    significant <- rep(NA, grid_count)
    significant[is.finite(adjusted)] <-
      adjusted[is.finite(adjusted)] < significance_level
    result[[paste0(metric, "_p_adjusted")]] <- adjusted
    result[[paste0(metric, "_significant")]] <- significant
  }
  permutation_diagnostics <- .trajectory_permutation_diagnostics(
    result, validation_a$group_vars, permutation_cluster_failures,
    permutation_replicate_failures, required_permutation, n_perm
  )

  diagnostics <- list()
  add_diagnostic <- function(code, severity, group, time_order, message, count) {
    diagnostics[[length(diagnostics) + 1L]] <<- data.frame(
      code = code, severity = severity, group = group,
      time_order = as.integer(time_order), message = message,
      count = as.integer(count), stringsAsFactors = FALSE
    )
  }
  if (order_info$implicit_character) {
    add_diagnostic(
      "implicit_character_order", "warning", "all", NA_integer_,
      paste0(
        "Character time values use stable first-appearance order; supply ",
        "`order` for a substantive sequence."
      ),
      period_count
    )
  }
  missing_key_total_a <- sum(validation_a$bad_key)
  missing_key_total_b <- sum(validation_b$bad_key)
  missing_key_assigned_a <- sum(result$n_a_rows_missing_key)
  missing_key_assigned_b <- sum(result$n_b_rows_missing_key)
  missing_key_unassigned_a <- missing_key_total_a - missing_key_assigned_a
  missing_key_unassigned_b <- missing_key_total_b - missing_key_assigned_b
  if (missing_key_total_a > 0L) {
    add_diagnostic(
      "missing_key_rows_a", "warning", labels[1L], NA_integer_,
      "Side A rows with missing/non-finite analytical keys were excluded.",
      missing_key_total_a
    )
  }
  if (missing_key_total_b > 0L) {
    add_diagnostic(
      "missing_key_rows_b", "warning", labels[2L], NA_integer_,
      "Side B rows with missing/non-finite analytical keys were excluded.",
      missing_key_total_b
    )
  }
  if (missing_key_unassigned_a > 0L) {
    add_diagnostic(
      "unassigned_missing_key_rows_a", "warning", labels[1L], NA_integer_,
      "Side A rows missing time/group keys cannot be assigned to a slice.",
      missing_key_unassigned_a
    )
  }
  if (missing_key_unassigned_b > 0L) {
    add_diagnostic(
      "unassigned_missing_key_rows_b", "warning", labels[2L], NA_integer_,
      "Side B rows missing time/group keys cannot be assigned to a slice.",
      missing_key_unassigned_b
    )
  }
  for (g in seq_len(group_count)) {
    group_row <- if (length(validation_a$group_vars)) {
      group_template[g, , drop = FALSE]
    } else {
      NULL
    }
    label <- .trajectory_group_label(validation_a$group_vars, group_row)
    group_indices <- ((g - 1L) * period_count + 1L):(g * period_count)
    for (t in seq_len(period_count)) {
      index <- group_indices[t]
      for (side in c("a", "b")) {
        used <- result[[paste0("n_", side, "_used")]][index]
        side_label <- labels[if (side == "a") 1L else 2L]
        if (used == 0L) {
          add_diagnostic(
            paste0("missing_independent_period_", side), "warning", label, t,
            paste0("No valid ", side_label,
                   " participant is available for this period."),
            1L
          )
        } else if (used == 1L) {
          add_diagnostic(
            paste0("one_entity_slice_", side), "warning", label, t,
            paste0("The ", side_label,
                   " centroid is defined by one participant."),
            1L
          )
        }
      }
      if (distance_space == "full" &&
          result$n_a_distance_incomplete[index] > 0L) {
        add_diagnostic(
          "full_distance_incomplete_a", "warning", label, t,
          paste0(
            "The selected-axis side A centroid is retained, but its ",
            "full-space movement metrics are unavailable."
          ),
          result$n_a_distance_incomplete[index]
        )
      }
      if (distance_space == "full" &&
          result$n_b_distance_incomplete[index] > 0L) {
        add_diagnostic(
          "full_distance_incomplete_b", "warning", label, t,
          paste0(
            "The selected-axis side B centroid is retained, but its ",
            "full-space movement metrics are unavailable."
          ),
          result$n_b_distance_incomplete[index]
        )
      }
    }
    if (cohort_policy == "available" && period_count > 1L) {
      for (side in c("a", "b")) {
        used_ids <- if (side == "a") {
          prepared_a$used_ids[group_indices]
        } else {
          prepared_b$used_ids[group_indices]
        }
        signatures <- vapply(used_ids, function(ids) {
          paste(sort(unique(ids)), collapse = "\r")
        }, character(1L))
        if (length(unique(signatures)) > 1L) {
          add_diagnostic(
            paste0("changing_cohort_", side), "warning", label, NA_integer_,
            paste0(
              "Side ", toupper(side),
              " participant composition changes across requested periods."
            ),
            length(unique(signatures))
          )
        }
      }
    }
  }
  diagnostics <- .trajectory_diagnostics_frame(diagnostics)
  for (extra in list(bootstrap_diagnostics, permutation_diagnostics)) {
    if (nrow(extra)) {
      diagnostics <- rbind(diagnostics, extra)
      rownames(diagnostics) <- NULL
    }
  }

  class(result) <- c("independent_centroid_path_comparison", "data.frame")
  attr(result, "trajectory_warnings") <- diagnostics
  attr(result, "comparison_spec") <- list(
    design = "independent groups",
    labels = labels,
    time_var = validation_a$time_var,
    id_var = validation_a$id_var,
    group_vars = validation_a$group_vars,
    dimensions = validation_a$dimensions,
    order_values = order_info$values,
    order_source = order_info$source,
    cohort_policy = cohort_policy,
    na_policy = na_policy,
    matching = "none; participant ID namespaces are side-specific",
    participant_identity = paste0(
      "Equal ID text across sides denotes different participants and is never ",
      "used for matching."
    ),
    weights_a = validation_a$weights$description,
    weights_b = validation_b$weights$description,
    distance_space = distance_space,
    distance_dimensions = validation_a$distance_dimensions,
    elapsed_interval_units = base_metrics$elapsed_units,
    difference_direction = paste0(labels[2L], " - ", labels[1L]),
    missing_key_counts = list(
      a = list(
        total = missing_key_total_a,
        slice_assigned = missing_key_assigned_a,
        unassigned = missing_key_unassigned_a
      ),
      b = list(
        total = missing_key_total_b,
        slice_assigned = missing_key_assigned_b,
        unassigned = missing_key_unassigned_b
      )
    )
  )
  attr(result, "bootstrap_spec") <- list(
    method = "independent-side participant-cluster percentile bootstrap",
    sampling_unit = paste0(validation_a$id_var,
                           " within side and trajectory group"),
    rows_per_sampled_id = "all eligible participant-period entities",
    n_participants_a = sum(lengths(prepared_a$pools)),
    n_participants_b = sum(lengths(prepared_b$pools)),
    stratum_sizes_a = stats::setNames(
      lengths(prepared_a$pools), group_labels
    ),
    stratum_sizes_b = stats::setNames(
      lengths(prepared_b$pools), group_labels
    ),
    eligible_id_keys_by_stratum_a = prepared_a$pools,
    eligible_id_keys_by_stratum_b = prepared_b$pools,
    n_boot = n_boot,
    conf_level = conf_level,
    minimum_valid_fraction = 0.80,
    minimum_tail_replicates = 5L,
    minimum_valid_replicates = required_bootstrap,
    seed = seed,
    failed_replicates = sum(resampling$failed_bootstrap),
    rng_state_restored = !is.null(seed)
  )
  attr(result, "permutation_spec") <- list(
    method = paste0(
      "Monte Carlo participant-cluster label permutation within trajectory ",
      "group strata"
    ),
    null_hypothesis = paste0(
      "Independent participant trajectories are exchangeable between ",
      labels[1L], " and ", labels[2L], " within each trajectory group."
    ),
    side_sizes_preserved = TRUE,
    signed_alternative = "two-sided absolute statistic",
    distance_alternative = "upper-tail centroid-separation statistic",
    finite_sample_correction = "(1 + exceedances) / (1 + valid permutations)",
    p_adjust_method = p_adjust_method,
    p_adjust_family = paste0(
      "all finite coordinate, change, and movement contrasts returned by this call"
    ),
    significance_level = significance_level,
    n_perm = n_perm,
    minimum_valid_fraction = 0.80,
    minimum_valid_replicates = required_permutation,
    seed = seed,
    failed_replicates = sum(resampling$failed_permutation),
    rng_state_restored = !is.null(seed)
  )
  .trajectory_emit_diagnostics(diagnostics)
  result
}
