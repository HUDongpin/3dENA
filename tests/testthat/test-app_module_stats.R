library(shiny)
source("../../R/app_utils.R")
source("../../R/app_module_stats.R")

test_that("paired observations are matched by ID rather than row position", {
  points <- data.frame(
    condition = c("before", "before", "after", "after"),
    participant = c("A", "B", "B", "A"),
    MR1 = c(1, 10, 9, 0)
  )

  result <- ena3d_match_pairs(
    points,
    group_var = "condition",
    group1 = "before",
    group2 = "after",
    id_var = "participant",
    axis = "MR1"
  )

  expect_equal(result$n_pairs, 2L)
  expect_equal(result$data$group1_value - result$data$group2_value, c(1, 1))
  expect_equal(result$unmatched_group1, 0L)
  expect_equal(result$unmatched_group2, 0L)
})

test_that("paired observations match character selections to POSIXct groups", {
  before <- as.POSIXct("2025-01-01 09:00:00", tz = "UTC")
  after <- as.POSIXct("2025-01-02 09:00:00", tz = "UTC")
  points <- data.frame(
    condition = rep(c(before, after), each = 2),
    participant = rep(c("A", "B"), 2),
    MR1 = c(2, 4, 1, 3)
  )

  result <- ena3d_match_pairs(
    points,
    group_var = "condition",
    group1 = as.character(before),
    group2 = as.character(after),
    id_var = "participant",
    axis = "MR1"
  )

  expect_equal(result$n_pairs, 2L)
  expect_equal(result$data$group1_value - result$data$group2_value, c(1, 1))
})

test_that("paired matching reports unmatched IDs", {
  points <- data.frame(
    condition = c("before", "before", "after"),
    participant = c("A", "B", "A"),
    MR1 = c(1, 2, 0)
  )
  result <- ena3d_match_pairs(points, "condition", "before", "after", "participant", "MR1")
  expect_equal(result$n_pairs, 1L)
  expect_equal(result$unmatched_group1, 1L)
  expect_equal(result$unmatched_group2, 0L)
})

test_that("paired matching reports invalid values and IDs separately", {
  points <- data.frame(
    condition = rep(c("before", "after"), each = 4),
    participant = c("A", "B", "C", "", "A", "B", "D", NA),
    MR1 = c(1, NA, 3, 4, 0, 2, Inf, 5),
    stringsAsFactors = FALSE
  )
  result <- ena3d_match_pairs(
    points, "condition", "before", "after", "participant", "MR1"
  )

  expect_equal(result$n_pairs, 1L)
  expect_equal(result$unmatched_group1, 1L)
  expect_equal(result$unmatched_group2, 1L)
  expect_equal(result$dropped_value_group1, 1L)
  expect_equal(result$dropped_value_group2, 1L)
  expect_equal(result$dropped_id_group1, 1L)
  expect_equal(result$dropped_id_group2, 1L)
})

test_that("paired matching rejects duplicate IDs within a condition", {
  points <- data.frame(
    condition = c("before", "before", "after"),
    participant = c("A", "A", "A"),
    MR1 = c(1, 2, 0)
  )
  expect_error(
    ena3d_match_pairs(points, "condition", "before", "after", "participant", "MR1"),
    "one observation"
  )
})

test_that("duplicate pairing IDs are rejected even when one axis value is missing", {
  points <- data.frame(
    condition = c("before", "before", "after"),
    participant = c("A", "A", "A"),
    MR1 = c(1, NA, 0)
  )
  expect_error(
    ena3d_match_pairs(points, "condition", "before", "after", "participant", "MR1"),
    "Duplicate IDs"
  )
})

test_that("paired Wilcoxon result is invariant to input row order", {
  points <- data.frame(
    condition = rep(c("before", "after"), each = 6),
    participant = c(LETTERS[1:6], rev(LETTERS[1:6])),
    MR1 = c(4, 9, 3, 12, 8, 15, 13, 7, 9, 2, 8, 1)
  )
  original <- ena3d_paired_wilcox(
    points, "condition", "before", "after", "participant", "MR1",
    alternative = "two.sided"
  )
  shuffled <- ena3d_paired_wilcox(
    points[c(8, 2, 11, 5, 1, 12, 4, 7, 3, 10, 6, 9), ],
    "condition", "before", "after", "participant", "MR1",
    alternative = "two.sided"
  )

  expect_equal(original$pairs$data, shuffled$pairs$data)
  expect_equal(original$statistic, shuffled$statistic)
  expect_equal(original$p_value, shuffled$p_value)
  expect_equal(original$effect_size, shuffled$effect_size)
})

test_that("paired Wilcoxon honors the selected alternative hypothesis", {
  points <- data.frame(
    condition = rep(c("before", "after"), each = 6),
    participant = c(LETTERS[1:6], rev(LETTERS[1:6])),
    MR1 = c(5, 7, 9, 11, 13, 15, 6, 5, 4, 3, 2, 1)
  )
  greater <- ena3d_paired_wilcox(
    points, "condition", "before", "after", "participant", "MR1",
    alternative = "greater"
  )
  less <- ena3d_paired_wilcox(
    points, "condition", "before", "after", "participant", "MR1",
    alternative = "less"
  )

  expect_identical(greater$alternative, "greater")
  expect_identical(less$alternative, "less")
  expect_lt(greater$p_value, less$p_value)
  expect_error(
    ena3d_paired_wilcox(
      points, "condition", "before", "after", "participant", "MR1",
      alternative = "unsupported"
    ),
    "arg"
  )
})

test_that("unpaired rank-biserial effect is positive when Group 1 is greater", {
  result <- ena3d_unpaired_wilcox(c(10, 11, 12), c(1, 2, 3))

  expect_equal(result$effect_size, 1)
  expect_equal(result$summary$Group1[result$summary$Statistic == "Valid N"], 3)
  expect_equal(result$summary$Group2[result$summary$Statistic == "Valid N"], 3)
})

test_that("Cohen's d preserves the Group 1 minus Group 2 direction", {
  greater <- ena3d_unpaired_t(c(10, 11, 12), c(1, 2, 3))
  less <- ena3d_unpaired_t(c(1, 2, 3), c(10, 11, 12))

  expect_gt(greater$effect_size, 0)
  expect_lt(less$effect_size, 0)
  expect_equal(greater$effect_size, -less$effect_size)
  expect_equal(ena3d_cohens_d(c(1, 1), c(1, 1)), 0)
  expect_true(is.na(ena3d_cohens_d(c(2, 2), c(1, 1))))
})

test_that("unpaired tests use and report the same finite analysis sample", {
  t_result <- ena3d_unpaired_t(c(1, NA, 3), c(5, 6, 7))
  w_result <- ena3d_unpaired_wilcox(c(1, NA, 3), c(5, 6, 7))

  expect_equal(t_result$summary$Group1[t_result$summary$Statistic == "Mean"], 2)
  expect_equal(t_result$summary$Group1[t_result$summary$Statistic == "Valid N"], 2)
  expect_equal(t_result$summary$Group1[t_result$summary$Statistic == "Dropped N"], 1)
  expect_equal(w_result$summary$Group1[w_result$summary$Statistic == "Median"], 2)
  expect_equal(w_result$summary$Group1[w_result$summary$Statistic == "Valid N"], 2)
  expect_true(is.finite(t_result$p_value))
  expect_true(is.finite(w_result$p_value))
})

test_that("all-zero paired differences are reported as not estimable", {
  points <- data.frame(
    condition = rep(c("before", "after"), each = 3),
    participant = rep(LETTERS[1:3], 2),
    MR1 = rep(c(1, 2, 3), 2)
  )
  result <- ena3d_paired_wilcox(
    points, "condition", "before", "after", "participant", "MR1"
  )

  expect_identical(result$nonzero_pairs, 0L)
  expect_true(is.na(result$p_value))
  expect_false(is.nan(result$p_value))
  expect_equal(result$effect_size, 0)
  expect_match(result$status, "not estimable", fixed = TRUE)
})

test_that("p-value adjustment ignores unavailable tests and adjusts the family", {
  results <- list(
    list(p_value = 0.01),
    structure(list(message = "unavailable"), class = c("simpleError", "error", "condition")),
    list(p_value = 0.04)
  )
  adjusted <- ena3d_adjust_p_values(results, "holm")

  expect_equal(adjusted, c(0.02, NA, 0.04))
  expect_error(ena3d_adjust_p_values(results, "unsupported"), "Unsupported")
})

test_that("Stats renders each axis and its own adjusted p-value", {
  points <- data.frame(
    condition = rep(c("before", "after"), each = 8),
    participant = rep(LETTERS[1:8], 2),
    MR1 = c(11:18, 1:8),
    SVD2 = c(4:11, 1:8),
    SVD3 = c(1:8, 1:8)
  )
  axes <- c("MR1", "SVD2", "SVD3")
  expected_results <- lapply(axes, function(axis) {
    ena3d_unpaired_t(
      points[points$condition == "before", axis],
      points[points$condition == "after", axis]
    )
  })
  expected_adjusted <- vapply(
    ena3d_adjust_p_values(expected_results, "holm"),
    format.pval,
    character(1),
    digits = 5,
    eps = 1e-05
  )
  rv_data <- reactiveValues(initialized = TRUE, ena_groupVar = "condition")
  state <- list(ena_obj = list(points = points))
  expected_p <- format.pval(
    ena3d_unpaired_t(11:18, 1:8)$p_value,
    digits = 5,
    eps = 1e-05
  )

  testServer(function(input, output, session) {
    stats_module(input, output, session, rv_data, list(), state)
  }, {
    session$setInputs(
      x = axes[[1L]], y = axes[[2L]], z = axes[[3L]],
      stats_group1 = "before", stats_group2 = "after",
      stats_design = "between", stats_p_adjust_method = "BH"
    )
    session$flushReact()
    session$setInputs(stats_p_adjust_method = "holm")
    session$flushReact()

    box_ids <- c("stats_box_x_axis", "stats_box_y_axis", "stats_box_z_axis")
    rendered_axes <- vapply(box_ids, function(box_id) {
      output[[paste0(box_id, "-axis_name")]]
    }, character(1))
    rendered_adjusted <- vapply(box_ids, function(box_id) {
      output[[paste0(box_id, "-p_adjusted")]]
    }, character(1))
    paired_error_axes <- vapply(
      paste0(box_ids, "_wilcox_paired"),
      function(box_id) output[[paste0(box_id, "-axis_name")]],
      character(1)
    )

    expect_identical(unname(rendered_axes), axes)
    expect_identical(unname(rendered_adjusted), expected_adjusted)
    expect_identical(unname(paired_error_axes), axes)
  })
})

test_that("paired Stats boxes retain the result for their own axis", {
  points <- data.frame(
    condition = rep(c("before", "after"), each = 6),
    participant = rep(LETTERS[1:6], 2),
    MR1 = c(10:15, 1:6),
    SVD2 = c(1, 4, 5, 9, 10, 15, 1, 2, 4, 6, 8, 11),
    SVD3 = rep(1:6, 2)
  )
  axes <- c("MR1", "SVD2", "SVD3")
  expected_results <- lapply(axes, function(axis) {
    ena3d_paired_wilcox(
      points, "condition", "before", "after", "participant", axis,
      alternative = "greater"
    )
  })
  expected <- vapply(expected_results, function(result) {
    if (is.finite(result$p_value)) {
      format.pval(result$p_value, digits = 5, eps = 1e-05)
    } else {
      "Not estimable"
    }
  }, character(1))
  expected_status <- vapply(expected_results, function(result) {
    if (is.null(result$status)) "" else result$status
  }, character(1))
  rv_data <- reactiveValues(initialized = TRUE, ena_groupVar = "condition")
  state <- list(ena_obj = list(points = points))

  testServer(function(input, output, session) {
    stats_module(input, output, session, rv_data, list(), state)
  }, {
    session$setInputs(
      x = axes[[1L]], y = axes[[2L]], z = axes[[3L]],
      stats_group1 = "before", stats_group2 = "after",
      stats_pair_id = "participant", stats_paired_alternative = "greater",
      stats_design = "within", stats_p_adjust_method = "BH"
    )
    session$flushReact()
    session$setInputs(stats_p_adjust_method = "holm")
    session$flushReact()

    box_ids <- paste0(
      c("stats_box_x_axis", "stats_box_y_axis", "stats_box_z_axis"),
      "_wilcox_paired"
    )
    rendered <- vapply(box_ids, function(box_id) {
      output[[paste0(box_id, "-p_value")]]
    }, character(1))
    rendered_status <- vapply(box_ids, function(box_id) {
      output[[paste0(box_id, "-test_status")]]
    }, character(1))

    expect_identical(unname(rendered), unname(expected))
    expect_identical(unname(rendered_status), unname(expected_status))
  })
})

test_that("Stats filters POSIXct groups selected by their display values", {
  fold <- as.POSIXct(
    c("2025-11-02 01:30:00 -0400", "2025-11-02 01:30:00 -0500"),
    format = "%Y-%m-%d %H:%M:%S %z",
    tz = "America/New_York"
  )
  before <- fold[[1L]]
  after <- fold[[2L]]
  points <- data.frame(
    condition = rep(c(before, after), each = 8),
    participant = rep(LETTERS[1:8], 2),
    MR1 = c(11:18, 1:8),
    SVD2 = c(4:11, 1:8),
    SVD3 = c(2:9, 1:8)
  )
  rv_data <- reactiveValues(initialized = TRUE, ena_groupVar = "condition")
  state <- list(ena_obj = list(points = points))
  group_labels <- unique(ena3d_group_value_labels(points$condition))
  expect_length(group_labels, 2L)
  expected_p <- format.pval(
    ena3d_unpaired_t(points$MR1[1:8], points$MR1[9:16])$p_value,
    digits = 5,
    eps = 1e-05
  )

  testServer(function(input, output, session) {
    stats_module(input, output, session, rv_data, list(), state)
  }, {
    session$setInputs(
      x = "MR1", y = "SVD2", z = "SVD3",
      stats_group1 = group_labels[[1L]],
      stats_group2 = group_labels[[2L]],
      stats_design = "between", stats_p_adjust_method = "BH"
    )
    session$flushReact()
    session$setInputs(stats_p_adjust_method = "holm")
    session$flushReact()

    expect_identical(output[["stats_box_x_axis-p_value"]], expected_p)
  })
})

test_that("paired tests require distinct condition selections", {
  points <- data.frame(
    condition = c("before", "after"),
    participant = c("A", "A"),
    MR1 = c(1, 0)
  )
  expect_error(
    ena3d_match_pairs(points, "condition", "before", "before", "participant", "MR1"),
    "distinct"
  )
})

test_that("changing the paired alternative invalidates and reruns Stats output", {
  participants <- LETTERS[1:6]
  points <- data.frame(
    condition = rep(c("before", "after"), each = 6),
    participant = c(participants, rev(participants)),
    MR1 = c(5, 7, 9, 11, 13, 15, 6, 5, 4, 3, 2, 1),
    SVD2 = c(6, 8, 10, 12, 14, 16, 7, 6, 5, 4, 3, 2),
    SVD3 = c(7, 9, 11, 13, 15, 17, 8, 7, 6, 5, 4, 3)
  )
  rv_data <- reactiveValues(initialized = TRUE, ena_groupVar = "condition")
  state <- list(ena_obj = list(points = points))

  testServer(function(input, output, session) {
    stats_module(input, output, session, rv_data, list(), state)
  }, {
    session$setInputs(
      x = "MR1", y = "SVD2", z = "SVD3",
      stats_group1 = "before", stats_group2 = "after",
      stats_pair_id = "participant",
      stats_paired_alternative = "two.sided",
      stats_design = "within",
      stats_p_adjust_method = "holm"
    )
    session$flushReact()
    session$setInputs(stats_paired_alternative = "greater")
    greater_p <- as.numeric(output[["stats_box_x_axis_wilcox_paired-p_value"]])
    greater_type <- output[["stats_box_x_axis_wilcox_paired-test_type"]]

    session$setInputs(stats_paired_alternative = "less")
    less_p <- as.numeric(output[["stats_box_x_axis_wilcox_paired-p_value"]])
    less_type <- output[["stats_box_x_axis_wilcox_paired-test_type"]]

    expect_lt(greater_p, less_p)
    expect_match(greater_type, "greater", fixed = TRUE)
    expect_match(less_type, "less", fixed = TRUE)
    expect_match(output$stats_design_status, "Repeated/paired", fixed = TRUE)
  })
})

test_that("Stats refuses a self-comparison for every study design", {
  points <- data.frame(
    condition = rep(c("before", "after"), each = 3),
    participant = rep(LETTERS[1:3], 2),
    MR1 = 1:6, SVD2 = 2:7, SVD3 = 3:8
  )
  rv_data <- reactiveValues(initialized = TRUE, ena_groupVar = "condition")
  state <- list(ena_obj = list(points = points))

  testServer(function(input, output, session) {
    stats_module(input, output, session, rv_data, list(), state)
  }, {
    session$setInputs(
      x = "MR1", y = "SVD2", z = "SVD3",
      stats_group1 = "before", stats_group2 = "after",
      stats_pair_id = "participant", stats_design = "between",
      stats_p_adjust_method = "holm"
    )
    session$flushReact()
    session$setInputs(stats_group2 = "before")

    expect_match(output$stats_design_status, "must differ", fixed = TRUE)
    expect_match(
      as.character(output[["stats_box_x_axis-data_table"]]),
      "two distinct groups",
      fixed = TRUE
    )
  })
})


test_that("Stats refuses duplicate axes before defining a correction family", {
  points <- data.frame(
    condition = rep(c("before", "after"), each = 4),
    participant = rep(LETTERS[1:4], 2),
    MR1 = c(5:8, 1:4), SVD2 = c(2:5, 1:4), SVD3 = c(4:7, 1:4)
  )
  rv_data <- reactiveValues(initialized = TRUE, ena_groupVar = "condition")
  state <- list(ena_obj = list(points = points))

  testServer(function(input, output, session) {
    stats_module(input, output, session, rv_data, list(), state)
  }, {
    session$setInputs(
      x = "MR1", y = "MR1", z = "SVD3",
      stats_group1 = "before", stats_group2 = "after",
      stats_design = "between", stats_p_adjust_method = "BH"
    )
    session$flushReact()
    session$setInputs(stats_p_adjust_method = "holm")
    session$flushReact()

    expect_match(output$stats_design_status, "three distinct", fixed = TRUE)
    expect_match(
      as.character(output[["stats_box_x_axis-data_table"]]),
      "three distinct", fixed = TRUE
    )
    expect_identical(output[["stats_box_x_axis-p_adjusted"]], "")
  })
})
