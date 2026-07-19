ena3d_match_pairs <- function(points, group_var, group1, group2, id_var, axis) {
  data <- as.data.frame(points)
  same_group <- if (inherits(group1, "POSIXt") && inherits(group2, "POSIXt")) {
    identical(as.numeric(group1), as.numeric(group2))
  } else {
    identical(as.character(group1), as.character(group2))
  }
  if (length(group1) != 1L || length(group2) != 1L ||
      is.na(group1) || is.na(group2) || same_group) {
    stop("Paired tests require two distinct, single group values.")
  }
  required <- c(group_var, id_var, axis)
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(sprintf("Missing paired-test columns: %s", paste(missing, collapse = ", ")))
  }
  if (!is.numeric(data[[axis]])) {
    stop(sprintf("Paired-test axis %s must be numeric.", axis))
  }

  first <- data[
    ena3d_group_value_match(data[[group_var]], group1),
    c(id_var, axis), drop = FALSE
  ]
  second <- data[
    ena3d_group_value_match(data[[group_var]], group2),
    c(id_var, axis), drop = FALSE
  ]
  names(first) <- c("pair_id", "group1_value")
  names(second) <- c("pair_id", "group2_value")
  valid_pair_id <- function(value) {
    valid <- !is.na(value) & nzchar(trimws(as.character(value)))
    if (is.numeric(value) || inherits(value, "Date") ||
        inherits(value, "POSIXt") || inherits(value, "difftime")) {
      valid <- valid & is.finite(as.numeric(value))
    }
    valid
  }
  first_id_valid <- valid_pair_id(first$pair_id)
  second_id_valid <- valid_pair_id(second$pair_id)
  dropped_id_group1 <- sum(!first_id_valid)
  dropped_id_group2 <- sum(!second_id_valid)
  first <- first[first_id_valid, , drop = FALSE]
  second <- second[second_id_valid, , drop = FALSE]
  first$.pair_key <- as.character(first$pair_id)
  second$.pair_key <- as.character(second$pair_id)

  duplicate_first <- unique(first$.pair_key[duplicated(first$.pair_key)])
  duplicate_second <- unique(second$.pair_key[duplicated(second$.pair_key)])
  if (length(duplicate_first) || length(duplicate_second)) {
    stop(
      paste0(
        "Pairing ID must identify one observation per selected group. Duplicate IDs: ",
        paste(unique(c(duplicate_first, duplicate_second)), collapse = ", ")
      )
    )
  }

  first_value_valid <- is.finite(first$group1_value)
  second_value_valid <- is.finite(second$group2_value)
  dropped_value_group1 <- sum(!first_value_valid)
  dropped_value_group2 <- sum(!second_value_valid)
  first <- first[first_value_valid, , drop = FALSE]
  second <- second[second_value_valid, , drop = FALSE]

  common_keys <- sort(intersect(first$.pair_key, second$.pair_key), method = "radix")
  first_index <- match(common_keys, first$.pair_key)
  second_index <- match(common_keys, second$.pair_key)
  matched <- data.frame(
    pair_id = first$pair_id[first_index],
    group1_value = first$group1_value[first_index],
    group2_value = second$group2_value[second_index],
    check.names = FALSE
  )
  list(
    data = matched,
    n_group1 = nrow(first),
    n_group2 = nrow(second),
    n_pairs = nrow(matched),
    unmatched_group1 = sum(!first$.pair_key %in% common_keys),
    unmatched_group2 = sum(!second$.pair_key %in% common_keys),
    dropped_id_group1 = dropped_id_group1,
    dropped_id_group2 = dropped_id_group2,
    dropped_value_group1 = dropped_value_group1,
    dropped_value_group2 = dropped_value_group2
  )
}

ena3d_signed_rank_effect <- function(x, y) {
  differences <- x - y
  differences <- differences[is.finite(differences) & differences != 0]
  if (!length(differences)) return(0)
  ranks <- rank(abs(differences), ties.method = "average")
  positive <- sum(ranks[differences > 0])
  negative <- sum(ranks[differences < 0])
  (positive - negative) / (positive + negative)
}

ena3d_finite_sample <- function(x, label = "sample") {
  numeric <- suppressWarnings(as.numeric(x))
  finite <- is.finite(numeric)
  values <- numeric[finite]
  if (length(values) < 2L) {
    stop(sprintf("%s needs at least two finite observations.", label))
  }
  list(
    values = values,
    raw_n = length(numeric),
    valid_n = length(values),
    dropped_n = sum(!finite)
  )
}

ena3d_cohens_d <- function(x, y) {
  first <- ena3d_finite_sample(x, "Group 1")$values
  second <- ena3d_finite_sample(y, "Group 2")$values
  pooled_variance <- (
    (length(first) - 1L) * stats::var(first) +
      (length(second) - 1L) * stats::var(second)
  ) / (length(first) + length(second) - 2L)
  mean_difference <- mean(first) - mean(second)
  if (!is.finite(pooled_variance) || pooled_variance < 0) return(NA_real_)
  if (pooled_variance == 0) {
    return(if (mean_difference == 0) 0 else NA_real_)
  }
  mean_difference / sqrt(pooled_variance)
}

ena3d_unpaired_t <- function(x, y) {
  first <- ena3d_finite_sample(x, "Group 1")
  second <- ena3d_finite_sample(y, "Group 2")
  result <- stats::t.test(first$values, second$values)
  effect <- ena3d_cohens_d(first$values, second$values)
  list(
    effect_size = effect,
    p_value = result$p.value,
    statistic = unname(result$statistic),
    conf = result$conf.int,
    conf_level = attr(result$conf.int, "conf.level"),
    test_type = sprintf("Welch t (df %.2f)", unname(result$parameter)),
    summary = data.frame(
      Statistic = c("Mean", "Std.", "Valid N", "Dropped N"),
      Group1 = c(mean(first$values), stats::sd(first$values),
                 first$valid_n, first$dropped_n),
      Group2 = c(mean(second$values), stats::sd(second$values),
                 second$valid_n, second$dropped_n),
      check.names = FALSE
    )
  )
}

ena3d_unpaired_wilcox <- function(x, y) {
  first <- ena3d_finite_sample(x, "Group 1")
  second <- ena3d_finite_sample(y, "Group 2")
  result <- stats::wilcox.test(
    first$values, second$values, paired = FALSE, exact = FALSE
  )
  u <- unname(result$statistic)
  # Positive values consistently mean Group 1 tends to be greater than Group 2.
  effect <- (2 * u) / (first$valid_n * second$valid_n) - 1
  list(
    effect_size = effect,
    p_value = result$p.value,
    statistic = u,
    conf = NULL,
    conf_level = NULL,
    test_type = "Wilcoxon rank-sum W",
    summary = data.frame(
      Statistic = c("Median", "Valid N", "Dropped N"),
      Group1 = c(stats::median(first$values), first$valid_n,
                 first$dropped_n),
      Group2 = c(stats::median(second$values), second$valid_n,
                 second$dropped_n),
      check.names = FALSE
    )
  )
}

ena3d_adjust_p_values <- function(results, method = "holm") {
  allowed <- stats::p.adjust.methods
  if (length(method) != 1L || is.na(method) || !method %in% allowed) {
    stop(sprintf(
      "Unsupported p-value adjustment method. Choose one of: %s.",
      paste(allowed, collapse = ", ")
    ))
  }
  raw <- vapply(results, function(result) {
    if (inherits(result, "error") || is.null(result$p_value) ||
        length(result$p_value) != 1L || !is.finite(result$p_value)) {
      return(NA_real_)
    }
    result$p_value
  }, numeric(1))
  adjusted <- rep(NA_real_, length(raw))
  valid <- is.finite(raw)
  if (any(valid)) adjusted[valid] <- stats::p.adjust(raw[valid], method = method)
  adjusted
}

ena3d_paired_wilcox <- function(points, group_var, group1, group2, id_var,
                                axis, alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  pairs <- ena3d_match_pairs(points, group_var, group1, group2, id_var, axis)
  if (pairs$n_pairs < 2L) stop("At least two matched IDs are required.")
  x <- as.numeric(pairs$data$group1_value)
  y <- as.numeric(pairs$data$group2_value)
  differences <- x - y
  nonzero_pairs <- sum(is.finite(differences) & differences != 0)
  if (nonzero_pairs == 0L) {
    return(list(
      pairs = pairs,
      statistic = NA_real_,
      p_value = NA_real_,
      effect_size = 0,
      alternative = alternative,
      method = "Wilcoxon signed-rank test",
      nonzero_pairs = 0L,
      status = paste(
        "All paired differences are zero; the signed-rank statistic and",
        "p-value are not estimable."
      )
    ))
  }
  result <- stats::wilcox.test(
    x,
    y,
    paired = TRUE,
    exact = FALSE,
    alternative = alternative
  )
  list(
    pairs = pairs,
    statistic = unname(result$statistic),
    p_value = result$p.value,
    effect_size = ena3d_signed_rank_effect(x, y),
    alternative = alternative,
    method = result$method,
    nonzero_pairs = nonzero_pairs,
    status = NULL
  )
}

stats_module <- function(input, output, session, rv_data, config, state) {
  ai_result <- reactiveVal(NULL)

  aggregate_result <- function(result, adjusted_p, summary = NULL,
                               paired = FALSE) {
    if (inherits(result, "error") || !is.list(result)) return(result)
    if (is.null(summary)) summary <- result$summary
    pairs <- if (isTRUE(paired)) result$pairs else NULL
    list(
      statistic = result$statistic,
      p_value = result$p_value,
      p_adjusted = adjusted_p,
      effect_size = result$effect_size,
      conf = result$conf,
      conf_level = result$conf_level,
      test_type = if (!is.null(result$test_type)) {
        result$test_type
      } else {
        result$method
      },
      alternative = result$alternative,
      status = result$status,
      n_group1 = if (is.list(pairs)) pairs$n_group1 else NULL,
      n_group2 = if (is.list(pairs)) pairs$n_group2 else NULL,
      n_pairs = if (is.list(pairs)) pairs$n_pairs else NULL,
      summary = summary
    )
  }

  build_paired_summary <- function(result) {
    pairs <- result$pairs
    data.frame(
      Statistic = c(
        "Median", "Matched N", "Non-zero pairs", "Unmatched finite IDs",
        "Dropped invalid values", "Dropped missing/blank IDs"
      ),
      Group1 = c(
        stats::median(pairs$data$group1_value), pairs$n_pairs,
        result$nonzero_pairs, pairs$unmatched_group1,
        pairs$dropped_value_group1, pairs$dropped_id_group1
      ),
      Group2 = c(
        stats::median(pairs$data$group2_value), pairs$n_pairs,
        result$nonzero_pairs, pairs$unmatched_group2,
        pairs$dropped_value_group2, pairs$dropped_id_group2
      ),
      check.names = FALSE
    )
  }

  format_number <- function(value) {
    if (is.null(value) || length(value) != 1L || !is.finite(value)) {
      return("Not estimable")
    }
    format(value, digits = 5)
  }

  format_p <- function(value) {
    if (is.null(value) || length(value) != 1L || !is.finite(value)) {
      return("Not estimable")
    }
    format.pval(value, digits = 5, eps = 1e-05)
  }

  render_stats_box <- function(axis_name, stat_box_id, result, adjusted_p,
                               adjustment_method, statistic_dataframe = NULL,
                               status = NULL) {
    force(axis_name)
    force(stat_box_id)
    force(result)
    force(adjusted_p)
    force(adjustment_method)
    force(statistic_dataframe)
    force(status)
    if (is.null(statistic_dataframe)) statistic_dataframe <- result$summary
    names(statistic_dataframe)[2:3] <- c(input$stats_group1, input$stats_group2)
    output[[paste0(stat_box_id, "-effect_size")]] <- renderText(
      format_number(result$effect_size)
    )
    output[[paste0(stat_box_id, "-p_value")]] <- renderText(
      format_p(result$p_value)
    )
    output[[paste0(stat_box_id, "-p_adjusted")]] <- renderText(
      format_p(adjusted_p)
    )
    output[[paste0(stat_box_id, "-p_adjust_method")]] <- renderText(
      sprintf("Adjusted p (%s):", adjustment_method)
    )
    output[[paste0(stat_box_id, "-test_type")]] <- renderText(result$test_type)
    output[[paste0(stat_box_id, "-test_type_value")]] <- renderText(
      format_number(result$statistic)
    )
    output[[paste0(stat_box_id, "-axis_name")]] <- renderText(axis_name)
    output[[paste0(stat_box_id, "-data_table")]] <- renderTable(statistic_dataframe)
    output[[paste0(stat_box_id, "-conf")]] <- renderText({
      if (is.null(result$conf)) "" else {
        sprintf("%.5f, %.5f", result$conf[[1L]], result$conf[[2L]])
      }
    })
    output[[paste0(stat_box_id, "-conf_level")]] <- renderText({
      if (is.null(result$conf_level)) "" else {
        sprintf("%.0f%% confidence interval:", result$conf_level * 100)
      }
    })
    output[[paste0(stat_box_id, "-test_status")]] <- renderText({
      if (is.null(status)) "" else status
    })
  }

  render_stats_error <- function(axis_name, stat_box_id, message) {
    force(axis_name)
    force(stat_box_id)
    force(message)
    output[[paste0(stat_box_id, "-axis_name")]] <- renderText(axis_name)
    output[[paste0(stat_box_id, "-data_table")]] <- renderTable(
      data.frame(Status = message)
    )
    for (suffix in c(
      "effect_size", "p_value", "p_adjusted", "p_adjust_method", "test_type",
      "test_type_value", "conf", "conf_level", "test_status"
    )) {
      output[[paste0(stat_box_id, "-", suffix)]] <- renderText("")
    }
  }

  safe_result <- function(expression) {
    tryCatch(expression, error = function(error) error)
  }

  render_result_or_error <- function(axis_name, stat_box_id, result,
                                     adjusted_p, adjustment_method,
                                     statistic_dataframe = NULL,
                                     status = NULL) {
    if (inherits(result, "error")) {
      render_stats_error(axis_name, stat_box_id, conditionMessage(result))
    } else {
      render_stats_box(
        axis_name, stat_box_id, result, adjusted_p, adjustment_method,
        statistic_dataframe = statistic_dataframe, status = status
      )
    }
  }

  all_box_ids <- function(kind) {
    suffixes <- c("x", "y", "z")
    switch(
      kind,
      t = paste0("stats_box_", suffixes, "_axis"),
      unpaired = paste0("stats_box_", suffixes, "_axis_wilcox_unpaired"),
      paired = paste0("stats_box_", suffixes, "_axis_wilcox_paired")
    )
  }

  observeEvent({
    list(
      input$x, input$y, input$z, input$stats_group1, input$stats_group2,
      input$stats_pair_id, input$stats_paired_alternative,
      input$stats_design, input$stats_p_adjust_method,
      input$stats_test_family, rv_data$initialized
    )
  }, {
    ai_result(NULL)
    req(rv_data$initialized, state$ena_obj, input$x, input$y, input$z,
        input$stats_group1, input$stats_group2)
    points <- as.data.frame(state$ena_obj$points)
    group_var <- rv_data$ena_groupVar[[1L]]
    group1 <- points[
      ena3d_group_value_match(points[[group_var]], input$stats_group1),
      , drop = FALSE
    ]
    group2 <- points[
      ena3d_group_value_match(points[[group_var]], input$stats_group2),
      , drop = FALSE
    ]
    axes <- c(input$x, input$y, input$z)
    design <- if (is.null(input$stats_design)) "" else input$stats_design
    test_family <- input$stats_test_family
    if (is.null(test_family) || length(test_family) != 1L ||
        is.na(test_family) || !nzchar(test_family)) {
      test_family <- if (identical(design, "within")) {
        "signed_rank"
      } else {
        "welch"
      }
    }
    adjustment_method <- if (is.null(input$stats_p_adjust_method) ||
                             !nzchar(input$stats_p_adjust_method)) {
      "holm"
    } else {
      input$stats_p_adjust_method
    }

    if (!ena3d_axes_are_distinct(axes)) {
      message <- paste(
        "Choose three distinct X, Y, and Z axes before running an",
        "inferential test."
      )
      for (kind in c("t", "unpaired", "paired")) {
        for (i in seq_along(axes)) {
          render_stats_error(axes[[i]], all_box_ids(kind)[[i]], message)
        }
      }
      output$stats_design_status <- renderText(paste(
        "No inferential test has been run:", message
      ))
      output$stats_pair_status <- renderText("")
      return(invisible(NULL))
    }

    if (!design %in% c("between", "within")) {
      for (i in seq_along(axes)) {
        render_stats_error(
          axes[[i]], all_box_ids("t")[[i]],
          "Select the study design before interpreting inferential tests."
        )
        render_stats_error(
          axes[[i]], all_box_ids("unpaired")[[i]],
          "Select the study design before interpreting inferential tests."
        )
        render_stats_error(
          axes[[i]], all_box_ids("paired")[[i]],
          "Select the study design before interpreting inferential tests."
        )
      }
      output$stats_design_status <- renderText(
        "No inferential test has been run: select independent or repeated groups."
      )
      output$stats_pair_status <- renderText("")
      return(invisible(NULL))
    }

    if (identical(
      as.character(input$stats_group1),
      as.character(input$stats_group2)
    )) {
      for (kind in c("t", "unpaired", "paired")) {
        for (i in seq_along(axes)) {
          render_stats_error(
            axes[[i]], all_box_ids(kind)[[i]],
            "Choose two distinct groups before running an inferential test."
          )
        }
      }
      output$stats_design_status <- renderText(
        "No inferential test has been run: Group 1 and Group 2 must differ."
      )
      output$stats_pair_status <- renderText("")
      return(invisible(NULL))
    }

    if (identical(design, "between")) {
      t_results <- lapply(axes, function(axis) {
        safe_result(ena3d_unpaired_t(group1[[axis]], group2[[axis]]))
      })
      wilcox_results <- lapply(axes, function(axis) {
        safe_result(ena3d_unpaired_wilcox(group1[[axis]], group2[[axis]]))
      })
      t_adjusted <- ena3d_adjust_p_values(t_results, adjustment_method)
      wilcox_adjusted <- ena3d_adjust_p_values(
        wilcox_results, adjustment_method
      )
      for (i in seq_along(axes)) {
        render_result_or_error(
          axes[[i]], all_box_ids("t")[[i]], t_results[[i]], t_adjusted[[i]],
          adjustment_method
        )
        render_result_or_error(
          axes[[i]], all_box_ids("unpaired")[[i]], wilcox_results[[i]],
          wilcox_adjusted[[i]], adjustment_method
        )
        render_stats_error(
          axes[[i]], all_box_ids("paired")[[i]],
          "Paired tests are disabled for an independent-groups design."
        )
      }
      output$stats_design_status <- renderText(paste(
        "Independent-groups design selected. Welch t and rank-sum tests use",
        sprintf(
          "finite observations; each three-axis test family is adjusted with %s.",
          adjustment_method
        )
      ))
      output$stats_pair_status <- renderText(
        "Pairing is not used for an independent-groups design."
      )
      selected_results <- switch(
        test_family,
        rank_sum = wilcox_results,
        welch = t_results,
        NULL
      )
      selected_adjusted <- switch(
        test_family,
        rank_sum = wilcox_adjusted,
        welch = t_adjusted,
        NULL
      )
      if (!is.null(selected_results)) {
        aggregate <- lapply(seq_along(axes), function(index) {
          aggregate_result(
            selected_results[[index]], selected_adjusted[[index]]
          )
        })
        names(aggregate) <- axes
        ai_result(list(
          design = "unpaired",
          test_family = test_family,
          results = aggregate
        ))
      }
      return(invisible(NULL))
    }

    paired_results <- lapply(axes, function(axis) {
      safe_result({
        if (is.null(input$stats_pair_id) || !nzchar(input$stats_pair_id)) {
          stop("Select a pairing ID before running a paired test.")
        }
        alternative <- if (is.null(input$stats_paired_alternative)) {
          "two.sided"
        } else {
          input$stats_paired_alternative
        }
        ena3d_paired_wilcox(
          points, group_var, input$stats_group1, input$stats_group2,
          input$stats_pair_id, axis, alternative = alternative
        )
      })
    })
    adjusted <- ena3d_adjust_p_values(paired_results, adjustment_method)
    for (i in seq_along(axes)) {
      render_stats_error(
        axes[[i]], all_box_ids("t")[[i]],
        "Unpaired tests are disabled for a repeated/paired design."
      )
      render_stats_error(
        axes[[i]], all_box_ids("unpaired")[[i]],
        "Unpaired tests are disabled for a repeated/paired design."
      )
      paired <- paired_results[[i]]
      if (inherits(paired, "error")) {
        render_stats_error(
          axes[[i]], all_box_ids("paired")[[i]], conditionMessage(paired)
        )
      } else {
        pairs <- paired$pairs
        paired_summary_frame <- build_paired_summary(paired)
        paired$conf <- NULL
        paired$conf_level <- NULL
        paired$test_type <- sprintf("Paired Wilcoxon V (%s)", paired$alternative)
        render_result_or_error(
          axes[[i]], all_box_ids("paired")[[i]], paired, adjusted[[i]],
          adjustment_method, statistic_dataframe = paired_summary_frame,
          status = paired$status
        )
      }
    }

    usable <- paired_results[!vapply(paired_results, inherits, logical(1), "error")]
    output$stats_design_status <- renderText(paste(
      "Repeated/paired design selected. Only ID-matched signed-rank tests are",
      sprintf("reported; p-values are adjusted with %s.", adjustment_method)
    ))
    output$stats_pair_status <- renderText({
      if (!length(usable)) {
        "Paired test unavailable for the current selections."
      } else {
        pairs <- usable[[1L]]$pairs
        sprintf(
          paste0(
            "%d IDs matched; %d unmatched finite IDs in %s and %d in %s; ",
            "%d/%d invalid axis values and %d/%d missing or blank IDs were dropped."
          ),
          pairs$n_pairs, pairs$unmatched_group1, input$stats_group1,
          pairs$unmatched_group2, input$stats_group2,
          pairs$dropped_value_group1, pairs$dropped_value_group2,
          pairs$dropped_id_group1, pairs$dropped_id_group2
        )
      }
    })
    if (identical(test_family, "signed_rank")) {
      aggregate <- lapply(seq_along(axes), function(index) {
        result <- paired_results[[index]]
        summary <- if (inherits(result, "error")) NULL else {
          build_paired_summary(result)
        }
        aggregate_result(
          result, adjusted[[index]], summary = summary, paired = TRUE
        )
      })
      names(aggregate) <- axes
      ai_result(list(
        design = "paired",
        test_family = "signed_rank",
        results = aggregate
      ))
    }
  }, ignoreInit = TRUE)

  reactive(ai_result())
}
