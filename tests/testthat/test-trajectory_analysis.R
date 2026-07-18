library(testthat)

if (!exists("compute_centroid_path", mode = "function")) {
  core_candidates <- c(
    file.path("R", "trajectory_analysis.R"),
    file.path("..", "..", "R", "trajectory_analysis.R"),
    file.path("..", "R", "trajectory_analysis.R")
  )
  core_file <- core_candidates[file.exists(core_candidates)][1L]
  if (is.na(core_file)) stop("Could not locate R/trajectory_analysis.R")
  source(core_file)
}

test_that("basic centroids, movement metrics, and metadata are correct", {
  points <- data.frame(
    person = c("p1", "p2", "p1", "p2"),
    wave = c(1, 1, 2, 2),
    axis_x = c(0, 2, 3, 5),
    axis_y = c(0, 0, 4, 4),
    stringsAsFactors = FALSE
  )
  original <- points
  path <- compute_centroid_path(
    points, time_var = "wave", id_var = "person",
    dimensions = c("axis_x", "axis_y")
  )

  expect_identical(points, original)
  expect_s3_class(path, "centroid_path")
  expect_equal(path$centroid_axis_x, c(1, 4))
  expect_equal(path$centroid_axis_y, c(0, 4))
  expect_equal(path$delta_axis_x, c(NA, 3))
  expect_equal(path$delta_axis_y, c(NA, 4))
  expect_equal(path$dx, path$delta_axis_x)
  expect_equal(path$dy, path$delta_axis_y)
  expect_true(all(is.na(path$dz)))
  expect_equal(path$step_distance, c(0, 5))
  expect_equal(path$elapsed_interval, c(NA, 1))
  expect_equal(path$speed, c(NA, 5))
  expect_equal(path$cumulative_distance, c(0, 5))
  expect_equal(path$n_total, c(2L, 2L))
  expect_equal(path$n_used, c(2L, 2L))

  spec <- attr(path, "trajectory_spec")
  expect_equal(spec$distance_space, "selected")
  expect_equal(spec$distance_dimensions, c("axis_x", "axis_y"))
  expect_equal(spec$elapsed_interval_units, "time units")
  expect_s3_class(attr(path, "trajectory_warnings"), "data.frame")
})

test_that("matrix and data-frame inputs produce identical centroid paths", {
  frame <- data.frame(
    id = c(1, 2, 1, 2),
    time = c(1, 1, 2, 2),
    x = c(0, 2, 4, 6),
    y = c(1, 3, 5, 7)
  )
  matrix_points <- as.matrix(frame)

  frame_path <- compute_centroid_path(
    frame, "time", "id", dimensions = c("x", "y")
  )
  matrix_path <- compute_centroid_path(
    matrix_points, "time", "id", dimensions = c("x", "y")
  )

  expect_equal(matrix_path$centroid_x, c(1, 5))
  expect_equal(matrix_path$centroid_y, c(2, 6))
  expect_equal(
    matrix_path[c("time_order", "n_used", "centroid_x", "centroid_y",
                  "step_distance")],
    frame_path[c("time_order", "n_used", "centroid_x", "centroid_y",
                 "step_distance")]
  )
})

test_that("group trajectories are independent and retain group types", {
  points <- data.frame(
    id = rep(1:2, 4),
    time = rep(rep(1:2, each = 2), 2),
    team = factor(rep(c("red", "blue"), each = 4),
                  levels = c("blue", "red")),
    x = c(0, 2, 2, 4, 10, 12, 14, 16),
    y = 0
  )
  path <- compute_centroid_path(
    points, "time", "id", group_vars = "team",
    dimensions = c("x", "y")
  )
  expect_true(is.factor(path$team))
  expect_equal(nrow(path), 4L)
  red <- path[path$team == "red", ]
  blue <- path[path$team == "blue", ]
  expect_equal(red$centroid_x, c(1, 3))
  expect_equal(blue$centroid_x, c(11, 15))
  expect_equal(red$cumulative_distance, c(0, 2))
  expect_equal(blue$cumulative_distance, c(0, 4))
})

test_that("numeric, Date, factor, and character order are stable and explicit", {
  numeric_points <- data.frame(id = 1, when = c(10, 2, 5), x = 1:3)
  numeric_path <- suppressWarnings(compute_centroid_path(
    numeric_points, "when", "id", dimensions = "x"
  ))
  expect_equal(numeric_path$when, c(2, 5, 10))
  expect_equal(numeric_path$time_order, 1:3)

  date_points <- data.frame(
    id = 1,
    when = as.Date(c("2025-01-04", "2025-01-01", "2025-01-02")),
    x = c(4, 1, 2)
  )
  date_path <- suppressWarnings(compute_centroid_path(
    date_points, "when", "id", dimensions = "x"
  ))
  expect_s3_class(date_path$when, "Date")
  expect_equal(date_path$when, as.Date(c("2025-01-01", "2025-01-02",
                                         "2025-01-04")))
  expect_equal(date_path$elapsed_interval, c(NA, 1, 2))
  expect_equal(attr(date_path, "trajectory_spec")$elapsed_interval_units,
               "days")

  factor_points <- data.frame(
    id = c(1, 1),
    phase = factor(c("post", "pre"),
                   levels = c("pre", "mid", "post"), ordered = TRUE),
    x = c(3, 1)
  )
  factor_path <- expect_warning(
    compute_centroid_path(factor_points, "phase", "id", dimensions = "x"),
    "missing_period"
  )
  expect_equal(as.character(factor_path$phase), c("pre", "mid", "post"))
  expect_true(is.ordered(factor_path$phase))
  expect_equal(factor_path$n_total, c(1L, 0L, 1L))
  expect_true(all(is.na(factor_path$elapsed_interval)))

  character_points <- data.frame(
    id = rep(1:2, each = 3),
    phase = rep(c("middle", "last", "first"), 2),
    x = 1:6,
    stringsAsFactors = FALSE
  )
  explicit <- c("first", "middle", "last")
  path_one <- compute_centroid_path(
    character_points, "phase", "id", dimensions = "x", order = explicit
  )
  set.seed(9)
  path_two <- compute_centroid_path(
    character_points[sample(nrow(character_points)), ], "phase", "id",
    dimensions = "x", order = explicit
  )
  expect_equal(path_one$phase, explicit)
  expect_equal(path_one$centroid_x, path_two$centroid_x)
  expect_equal(attr(path_one, "trajectory_spec")$order_source, "explicit")

  implicit <- expect_warning(
    compute_centroid_path(character_points, "phase", "id", dimensions = "x"),
    "implicit_character_order"
  )
  expect_equal(implicit$phase, c("middle", "last", "first"))
})

test_that("available and complete cohorts give transparent missing counts", {
  points <- data.frame(
    id = c("a", "b", "a"),
    time = c(1, 1, 2),
    x = c(0, 10, 4),
    y = 0,
    stringsAsFactors = FALSE
  )
  available <- suppressWarnings(compute_centroid_path(
    points, "time", "id", dimensions = c("x", "y"), order = 1:3,
    cohort_policy = "available"
  ))
  complete <- suppressWarnings(compute_centroid_path(
    points, "time", "id", dimensions = c("x", "y"), order = 1:2,
    cohort_policy = "complete"
  ))

  expect_equal(available$n_used, c(2L, 1L, 0L))
  expect_equal(available$n_total, c(2L, 1L, 0L))
  expect_true(is.na(available$centroid_x[3L]))
  expect_true(is.na(available$step_distance[3L]))
  expect_true("missing_period" %in%
                attr(available, "trajectory_warnings")$code)
  expect_equal(complete$n_used, c(1L, 1L))
  expect_equal(complete$n_cohort_excluded, c(1L, 0L))
  expect_equal(complete$centroid_x, c(0, 4))
})

test_that("duplicate participant-period rows are collapsed without inflation", {
  points <- data.frame(
    id = c("a", "a", "b", "a", "b"),
    time = c(1, 1, 1, 2, 2),
    x = c(0, 2, 3, 4, 6),
    y = 0,
    stringsAsFactors = FALSE
  )
  path <- expect_warning(
    compute_centroid_path(points, "time", "id",
                          dimensions = c("x", "y")),
    "duplicate_id_time"
  )
  # Participant a contributes its duplicate mean (1), once; b contributes 3.
  expect_equal(path$centroid_x, c(2, 5))
  expect_equal(path$n_rows_total, c(3L, 2L))
  expect_equal(path$n_total, c(2L, 2L))
  expect_equal(path$n_duplicate_rows, c(1L, 0L))
})

test_that("weights, missing values, and zero weights follow declared policies", {
  weighted <- data.frame(
    id = c("a", "b"), time = 1, x = c(0, 10), y = 0,
    w = c(1, 3), stringsAsFactors = FALSE
  )
  path <- compute_centroid_path(
    weighted, "time", "id", dimensions = c("x", "y"), weights = "w"
  )
  expect_equal(path$centroid_x, 7.5)

  zero <- weighted
  zero$w <- c(1, 0)
  zero_path <- suppressWarnings(compute_centroid_path(
    zero, "time", "id", dimensions = c("x", "y"), weights = zero$w
  ))
  expect_equal(zero_path$centroid_x, 0)
  expect_equal(zero_path$n_total, 2L)
  expect_equal(zero_path$n_used, 1L)
  expect_equal(zero_path$n_zero_weight, 1L)
  expect_equal(zero_path$n_excluded, 1L)

  missing <- weighted
  missing$x[2] <- NA_real_
  missing_path <- suppressWarnings(compute_centroid_path(
    missing, "time", "id", dimensions = c("x", "y")
  ))
  expect_equal(missing_path$n_missing, 1L)
  expect_equal(missing_path$n_used, 1L)
  expect_error(
    compute_centroid_path(missing, "time", "id", dimensions = c("x", "y"),
                          na_policy = "error"),
    "na_policy"
  )
  expect_error(
    compute_centroid_path(weighted, "time", "id", dimensions = c("x", "y"),
                          weights = c(1, -1)),
    NA
  )
})

test_that("selected and full distance spaces are distinct and documented", {
  points <- data.frame(
    id = rep(1:2, 2),
    time = rep(1:2, each = 2),
    x = c(0, 0, 3, 3),
    y = 0,
    z = c(0, 0, 4, 4),
    scale_factor = c(1, 10, 100, 1000)
  )
  selected <- compute_centroid_path(
    points, "time", "id", dimensions = c("x", "y"),
    distance_space = "selected"
  )
  full <- compute_centroid_path(
    points, "time", "id", dimensions = c("x", "y"),
    distance_space = "full", full_dimensions = c("x", "y", "z")
  )
  stripped <- points
  stripped$scale_factor <- NULL
  selected_again <- compute_centroid_path(
    stripped, "time", "id", dimensions = c("x", "y")
  )

  expect_equal(selected$step_distance, c(0, 3))
  expect_equal(full$step_distance, c(0, 5))
  expect_false("centroid_z" %in% names(full))
  expect_equal(selected$centroid_x, selected_again$centroid_x)
  expect_equal(selected$step_distance, selected_again$step_distance)
  expect_equal(attr(full, "trajectory_spec")$distance_dimensions,
               c("x", "y", "z"))
  expect_error(
    compute_centroid_path(points, "time", "id", dimensions = c("x", "y"),
                          distance_space = "full"),
    "full_dimensions"
  )
})

test_that("full-distance missingness does not change selected centroids", {
  points <- data.frame(
    id = c("a", "b", "a", "b"),
    time = c(1, 1, 2, 2),
    x = c(0, 10, 2, 12),
    y = 0,
    z = c(0, NA, 1, NA),
    stringsAsFactors = FALSE
  )
  selected <- compute_centroid_path(
    points, "time", "id", dimensions = c("x", "y"),
    distance_space = "selected", full_dimensions = c("x", "y", "z")
  )
  full <- suppressWarnings(compute_centroid_path(
    points, "time", "id", dimensions = c("x", "y"),
    distance_space = "full", full_dimensions = c("x", "y", "z")
  ))

  expect_equal(full$centroid_x, selected$centroid_x)
  expect_equal(full$centroid_y, selected$centroid_y)
  expect_equal(full$n_used, selected$n_used)
  expect_equal(full$centroid_x, c(5, 7))
  expect_equal(full$n_distance_incomplete, c(1L, 1L))
  expect_true(all(is.na(full$step_distance)))
  expect_true("full_distance_incomplete" %in%
                attr(full, "trajectory_warnings")$code)
})

test_that("one-entity and zero-variance slices return defined results and diagnostics", {
  one <- data.frame(id = 1, time = 1:2, x = c(1, 2), y = c(3, 4))
  one_path <- expect_warning(
    compute_centroid_path(one, "time", "id", dimensions = c("x", "y")),
    "one_entity_slice"
  )
  expect_equal(one_path$step_distance, c(0, sqrt(2)))
  expect_true("one_entity_slice" %in%
                attr(one_path, "trajectory_warnings")$code)

  constant <- data.frame(id = 1:3, time = 1, x = 2, y = -1)
  constant_path <- expect_warning(
    compute_centroid_path(constant, "time", "id",
                          dimensions = c("x", "y")),
    "zero_variance_slice"
  )
  expect_equal(constant_path$centroid_x, 2)
  expect_equal(constant_path$step_distance, 0)
})

test_that("participant bootstrap preserves clusters, is deterministic, and restores RNG", {
  points <- expand.grid(id = 1:5, time = 1:3)
  points$x <- points$id + points$time
  points$y <- points$id - points$time
  original_points <- points

  id_keys <- .trajectory_value_key(points$id)
  sampled <- .trajectory_cluster_sample(
    points, "id", rep(unique(id_keys)[1L], 2L), id_keys
  )
  expect_equal(nrow(sampled), 2L * sum(points$id == 1L))
  expect_equal(length(unique(sampled$id)), 2L)
  expect_equal(as.integer(table(sampled$id)), c(3L, 3L))

  set.seed(812)
  rng_before <- .Random.seed
  boot_one <- bootstrap_centroid_path(
    points, "time", "id", dimensions = c("x", "y"),
    n_boot = 39, seed = 44
  )
  expect_identical(points, original_points)
  expect_identical(.Random.seed, rng_before)
  boot_two <- bootstrap_centroid_path(
    points[sample(nrow(points)), ], "time", "id",
    dimensions = c("x", "y"), n_boot = 39, seed = 44
  )
  expect_equal(boot_one$centroid_x_lower, boot_two$centroid_x_lower)
  expect_equal(boot_one$centroid_x_upper, boot_two$centroid_x_upper)
  expect_true(all(boot_one$centroid_x_boot_n == 39L))
  expect_equal(attr(boot_one, "bootstrap_spec")$rows_per_sampled_id,
               "all raw rows")
  expect_equal(attr(boot_one, "bootstrap_spec")$failed_replicates, 0L)
})

test_that("complete-cohort bootstrap samples only analytically eligible IDs", {
  complete <- data.frame(
    id = "complete", time = 1:2, x = c(0, 10), y = 0,
    stringsAsFactors = FALSE
  )
  incomplete <- data.frame(
    id = paste0("incomplete", 1:9), time = 1,
    x = 100 + seq_len(9), y = 0,
    stringsAsFactors = FALSE
  )
  points <- rbind(complete, incomplete)

  boot <- suppressWarnings(bootstrap_centroid_path(
    points, "time", "id", dimensions = c("x", "y"),
    cohort_policy = "complete", n_boot = 29, seed = 19
  ))
  spec <- attr(boot, "bootstrap_spec")

  expect_equal(boot$n_used, c(1L, 1L))
  expect_true(all(boot$centroid_x_boot_n == 29L))
  expect_true(all(boot$step_distance_boot_n == 29L))
  expect_equal(spec$n_participants, 1L)
  expect_equal(spec$n_sampling_units, 1L)
  expect_equal(unname(spec$stratum_sizes), 1L)
  expect_equal(spec$failed_replicates, 0L)
})

test_that("auto bootstrap stratifies disjoint trajectory groups", {
  points <- expand.grid(
    id = c("a1", "a2", "b1", "b2"), time = 1:2,
    stringsAsFactors = FALSE
  )
  points$group <- substr(points$id, 1L, 1L)
  points$x <- match(points$id, unique(points$id)) + points$time
  points$y <- match(points$id, unique(points$id)) - points$time

  boot <- suppressWarnings(bootstrap_centroid_path(
    points, "time", "id", group_vars = "group",
    dimensions = c("x", "y"), cohort_policy = "complete",
    n_boot = 23, seed = 8, bootstrap_design = "auto"
  ))
  spec <- attr(boot, "bootstrap_spec")

  expect_equal(spec$bootstrap_design, "stratified")
  expect_equal(spec$n_participants, 4L)
  expect_equal(sort(unname(spec$stratum_sizes)), c(2L, 2L))
  expect_true(all(boot$centroid_x_boot_n == 23L))
})

test_that("paired comparison matches explicit IDs and is row-order invariant", {
  side_a <- expand.grid(id = c("p1", "p2", "p3"), time = 1:2,
                        stringsAsFactors = FALSE)
  side_a$x <- c(0, 20, 4, 2, 22, 6)
  side_a$y <- c(0, 20, 4, 2, 22, 6)
  side_b <- expand.grid(id = c("p3", "p1", "p4"), time = 1:2,
                        stringsAsFactors = FALSE)
  side_b$x <- c(5, 1, 100, 7, 3, 102)
  side_b$y <- c(6, 2, 100, 8, 4, 102)
  original_a <- side_a
  original_b <- side_b

  set.seed(710)
  rng_before <- .Random.seed
  comparison <- expect_warning(compare_centroid_paths(
    side_a, side_b, "time", "id", dimensions = c("x", "y"),
    n_boot = 39, seed = 7
  ), "unmatched_participants")
  expect_identical(side_a, original_a)
  expect_identical(side_b, original_b)
  expect_identical(.Random.seed, rng_before)
  expect_equal(comparison$n_a_total, c(3L, 3L))
  expect_equal(comparison$n_b_total, c(3L, 3L))
  expect_equal(comparison$n_matched, c(2L, 2L))
  expect_equal(comparison$n_used, c(2L, 2L))
  expect_equal(comparison$n_unmatched_a, c(1L, 1L))
  expect_equal(comparison$n_unmatched_b, c(1L, 1L))
  # Matched p1/p3 values are (0,4) versus (1,5) at time 1.
  expect_equal(comparison$centroid_a_x[1L], 2)
  expect_equal(comparison$centroid_b_x[1L], 3)
  expect_equal(comparison$difference_x, c(1, 1))
  expect_true(all(comparison$difference_x_boot_n == 39L))

  set.seed(61)
  shuffled_a <- side_a[sample(nrow(side_a)), ]
  shuffled_b <- side_b[sample(nrow(side_b)), ]
  shuffled <- suppressWarnings(compare_centroid_paths(
    shuffled_a, shuffled_b, "time", "id", dimensions = c("x", "y"),
    n_boot = 39, seed = 7
  ))
  columns <- c("centroid_a_x", "centroid_b_x", "difference_x",
               "difference_x_lower", "difference_x_upper")
  expect_equal(comparison[columns], shuffled[columns])
  expect_equal(attr(comparison, "comparison_spec")$matching,
               "exact id + time + group before centroid calculation")
})

test_that("paired complete cohort is the same at every period", {
  side_a <- data.frame(
    id = c("a", "b", "a", "b"), time = c(1, 1, 2, 2),
    x = c(0, 10, 2, 12), y = 0,
    stringsAsFactors = FALSE
  )
  side_b <- data.frame(
    id = c("a", "b", "a"), time = c(1, 1, 2),
    x = c(1, 11, 3), y = 1,
    stringsAsFactors = FALSE
  )
  comparison <- suppressWarnings(compare_centroid_paths(
    side_a, side_b, "time", "id", dimensions = c("x", "y"),
    cohort_policy = "complete", n_boot = 19, seed = 3
  ))
  expect_equal(comparison$n_matched, c(2L, 1L))
  expect_equal(comparison$n_used, c(1L, 1L))
  expect_equal(comparison$n_cohort_excluded, c(1L, 0L))
  expect_equal(comparison$difference_x, c(1, 1))
})

test_that("paired comparison diagnoses changing, one-pair, and missing cohorts", {
  side_a <- data.frame(
    id = c("a", "b", "a"), time = c(1, 1, 2),
    x = c(0, 100, 0), y = 0,
    stringsAsFactors = FALSE
  )
  side_b <- data.frame(
    id = c("a", "b", "a"), time = c(1, 1, 2),
    x = c(1, 101, 10), y = 0,
    stringsAsFactors = FALSE
  )
  comparison <- suppressWarnings(compare_centroid_paths(
    side_a, side_b, "time", "id", dimensions = c("x", "y"),
    order = 1:3, cohort_policy = "available", n_boot = 19, seed = 1
  ))
  diagnostics <- attr(comparison, "trajectory_warnings")

  expect_equal(comparison$n_used, c(2L, 1L, 0L))
  expect_true(all(c(
    "changing_matched_cohort", "one_pair_slice", "missing_paired_period"
  ) %in% diagnostics$code))
})

test_that("paired comparison returns diagnostics when no participant IDs match", {
  side_a <- expand.grid(
    id = c("a", "b"), time = 1:2,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  side_b <- expand.grid(
    id = c("c", "d"), time = 1:2,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  side_a$x <- seq_len(nrow(side_a))
  side_a$y <- -side_a$x
  side_b$x <- seq_len(nrow(side_b)) + 10
  side_b$y <- -side_b$x

  comparison <- expect_warning(
    compare_centroid_paths(
      side_a, side_b, "time", "id", dimensions = c("x", "y"),
      n_boot = 19, seed = 11
    ),
    "no_matched_participants"
  )
  diagnostics <- attr(comparison, "trajectory_warnings")
  spec <- attr(comparison, "bootstrap_spec")

  expect_s3_class(comparison, "paired_centroid_path_comparison")
  expect_equal(comparison$n_matched, c(0L, 0L))
  expect_equal(comparison$n_used, c(0L, 0L))
  expect_true(all(is.na(comparison$difference_x)))
  expect_true(all(comparison$difference_x_boot_n == 0L))
  expect_true("no_matched_participants" %in% diagnostics$code)
  expect_equal(spec$n_participants, 0L)
  expect_equal(spec$n_sampling_units, 0L)
  expect_identical(spec$eligible_id_keys, character(0))
})

test_that("full-space paired comparison retains the selected matched cohort", {
  side_a <- data.frame(
    id = c("a", "b"), time = 1,
    x = c(0, 10), y = 0, z = c(0, NA),
    stringsAsFactors = FALSE
  )
  side_b <- data.frame(
    id = c("a", "b"), time = 1,
    x = c(1, 11), y = 0, z = c(1, NA),
    stringsAsFactors = FALSE
  )
  comparison <- suppressWarnings(compare_centroid_paths(
    side_a, side_b, "time", "id", dimensions = c("x", "y"),
    distance_space = "full", full_dimensions = c("x", "y", "z"),
    n_boot = 19, seed = 2
  ))
  diagnostics <- attr(comparison, "trajectory_warnings")
  full_issue <- diagnostics[diagnostics$code == "full_distance_incomplete", ]

  expect_equal(comparison$n_used, 2L)
  expect_equal(comparison$centroid_a_x, 5)
  expect_equal(comparison$centroid_b_x, 6)
  expect_equal(comparison$difference_x, 1)
  expect_true(is.na(comparison$centroid_difference_distance))
  expect_equal(full_issue$count, 1L)
})

test_that("cumulative distance becomes unavailable after the first path break", {
  points <- data.frame(
    id = rep(c("p1", "p2"), each = 4L),
    time = rep(1:4, 2L),
    x = c(0, NA, 10, 20, 0, NA, 10, 20),
    y = 0,
    stringsAsFactors = FALSE
  )
  path <- suppressWarnings(compute_centroid_path(
    points, "time", "id", dimensions = c("x", "y"), order = 1:4
  ))

  expect_equal(path$step_distance, c(0, NA, NA, 10))
  expect_equal(path$cumulative_distance, c(0, NA, NA, NA))

  full <- data.frame(
    id = rep(c("complete", "incomplete"), each = 2L),
    time = rep(1:2, 2L),
    x = c(0, 2, 10, 12), y = 0,
    z = c(0, 1, NA, NA), stringsAsFactors = FALSE
  )
  boot <- suppressWarnings(bootstrap_centroid_path(
    full, "time", "id", dimensions = c("x", "y"),
    distance_space = "full", full_dimensions = c("x", "y", "z"),
    n_boot = 200, seed = 3
  ))
  expect_true(all(is.na(boot$step_distance)))
  expect_true(all(is.na(boot$cumulative_distance)))
  expect_true(all(is.na(boot$step_distance_lower)))
  expect_true(all(is.na(boot$step_distance_upper)))
  expect_true(all(is.na(boot$cumulative_distance_lower)))
  expect_true(all(is.na(boot$cumulative_distance_upper)))
})

test_that("bootstrap intervals require enough clusters and finite replicates", {
  points <- expand.grid(id = 1:5, time = 1:2)
  points$x <- points$id + points$time
  points$y <- points$id - points$time

  too_small <- suppressWarnings(bootstrap_centroid_path(
    points, "time", "id", dimensions = c("x", "y"),
    n_boot = 50, conf_level = 0.95, seed = 4
  ))
  expect_true(all(is.na(too_small$centroid_x_lower)))
  expect_true("bootstrap_insufficient_replicates" %in%
                attr(too_small, "trajectory_warnings")$code)
  expect_equal(attr(too_small, "bootstrap_spec")$minimum_valid_replicates, 200L)

  enough <- suppressWarnings(bootstrap_centroid_path(
    points, "time", "id", dimensions = c("x", "y"),
    n_boot = 200, conf_level = 0.95, seed = 4
  ))
  expect_true(all(is.finite(enough$centroid_x_lower)))
  expect_true(all(enough$centroid_x_boot_n == 200L))

  one <- data.frame(id = "solo", time = 1:2, x = c(1, 3), y = c(2, 4))
  one_boot <- suppressWarnings(bootstrap_centroid_path(
    one, "time", "id", dimensions = c("x", "y"),
    n_boot = 200, seed = 1
  ))
  expect_true(all(is.na(one_boot$centroid_x_lower)))
  expect_true(all(one_boot$centroid_x_boot_n == 200L))
  expect_true("bootstrap_insufficient_clusters" %in%
                attr(one_boot, "trajectory_warnings")$code)
})

test_that("group-local ID design is explicit and preserves group sample sizes", {
  points <- rbind(
    expand.grid(group = "A", id = c("1", "2"), time = 1:2,
                stringsAsFactors = FALSE),
    expand.grid(group = "B", id = c("1", "2", "3", "4"), time = 1:2,
                stringsAsFactors = FALSE)
  )
  points$x <- ifelse(
    points$group == "A", as.numeric(points$id) * 10, as.numeric(points$id)
  ) + points$time
  points$y <- 0

  automatic <- suppressWarnings(bootstrap_centroid_path(
    points, "time", "id", group_vars = "group",
    dimensions = c("x", "y"), cohort_policy = "complete",
    n_boot = 200, seed = 12, bootstrap_design = "auto"
  ))
  stratified <- suppressWarnings(bootstrap_centroid_path(
    points, "time", "id", group_vars = "group",
    dimensions = c("x", "y"), cohort_policy = "complete",
    n_boot = 200, seed = 12, bootstrap_design = "stratified"
  ))

  expect_equal(attr(automatic, "bootstrap_spec")$bootstrap_design, "cluster")
  expect_lt(min(automatic$centroid_x_boot_n[automatic$group == "A"]), 200L)
  expect_equal(attr(stratified, "bootstrap_spec")$bootstrap_design, "stratified")
  expect_equal(attr(stratified, "bootstrap_spec")$n_sampling_units, 6L)
  expect_true(all(stratified$centroid_x_boot_n == 200L))
  expect_length(attr(stratified, "bootstrap_spec")$eligible_id_keys_by_stratum, 2L)
})

test_that("grouped paired bootstrap stratifies disjoint matched ID pools", {
  make_side <- function(shift = 0) {
    data <- rbind(
      expand.grid(group = "G1", id = c("a1", "a2"), time = 1:2,
                  stringsAsFactors = FALSE),
      expand.grid(group = "G2", id = c("b1", "b2"), time = 1:2,
                  stringsAsFactors = FALSE)
    )
    data$x <- seq_len(nrow(data)) + shift
    data$y <- 0
    data
  }
  comparison <- suppressWarnings(compare_centroid_paths(
    make_side(), make_side(1), "time", "id", group_vars = "group",
    dimensions = c("x", "y"), cohort_policy = "complete",
    n_boot = 200, seed = 8, bootstrap_design = "auto"
  ))
  spec <- attr(comparison, "bootstrap_spec")

  expect_equal(spec$bootstrap_design, "stratified")
  expect_equal(unname(spec$stratum_sizes), c(2L, 2L))
  expect_true(all(comparison$difference_x_boot_n == 200L))
  expect_true(all(is.finite(comparison$difference_x_lower)))
})

test_that("paired comparison rejects ambiguous time classes and weights by default", {
  side_a <- data.frame(
    id = c("p1", "p2"), time = factor(c("t1", "t1")),
    x = c(0, 10), y = 0, w = c(100, 1)
  )
  side_b <- data.frame(
    id = c("p1", "p2"), time = c("t1", "t1"),
    x = c(0, 20), y = 0, w = c(1, 100)
  )
  expect_error(
    compare_centroid_paths(
      side_a, side_b, "time", "id", dimensions = c("x", "y"),
      n_boot = 2
    ),
    "compatible classes"
  )

  side_a$time <- as.character(side_a$time)
  expect_error(
    compare_centroid_paths(
      side_a, side_b, "time", "id", dimensions = c("x", "y"),
      weights_a = "w", weights_b = "w", n_boot = 2
    ),
    "Matched participant weights differ"
  )
  geometric <- suppressWarnings(compare_centroid_paths(
    side_a, side_b, "time", "id", dimensions = c("x", "y"),
    weights_a = "w", weights_b = "w", pair_weight_policy = "geometric",
    n_boot = 2, seed = 1
  ))
  expect_equal(geometric$difference_x, 5)
  expect_equal(attr(geometric, "comparison_spec")$paired_weight_policy,
               "geometric")
  expect_true("geometric_pair_weights" %in%
                attr(geometric, "trajectory_warnings")$code)
})

test_that("bundled newfrat yields 15 weekly centroids with 17 people each", {
  sample_candidates <- c(
    file.path("sample_data", "newfrat_enaset.Rdata"),
    file.path("..", "..", "sample_data", "newfrat_enaset.Rdata"),
    file.path("..", "sample_data", "newfrat_enaset.Rdata")
  )
  sample_file <- sample_candidates[file.exists(sample_candidates)][1L]
  expect_false(is.na(sample_file))
  loaded <- new.env(parent = emptyenv())
  load(sample_file, envir = loaded)
  enaset <- loaded[[ls(loaded)[1L]]]
  points <- enaset$points

  path <- compute_centroid_path(
    points, time_var = "Week", id_var = "Name",
    dimensions = c("SVD1", "SVD2", "SVD3")
  )
  expect_equal(nrow(path), 15L)
  expect_equal(path$time_order, 1:15)
  expect_true(all(path$n_total == 17L))
  expect_true(all(path$n_used == 17L))
  expect_true(all(is.finite(path$centroid_SVD1)))
  expect_true(all(is.finite(path$centroid_SVD2)))
  expect_true(all(is.finite(path$centroid_SVD3)))
  expect_equal(path$cumulative_distance[1L], 0)
  expect_true(path$cumulative_distance[15L] > 0)
})

test_that("generated output names cannot overwrite time or group columns", {
  compute_points <- expand.grid(
    id = 1:2, time = 1:2, n_total = c("A", "B"),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  compute_points$x <- seq_len(nrow(compute_points))
  compute_points$y <- 0
  compute_original <- compute_points
  expect_error(
    compute_centroid_path(
      compute_points, "time", "id", group_vars = "n_total",
      dimensions = c("x", "y")
    ),
    "Centroid-path output column collision.*n_total"
  )
  expect_identical(compute_points, compute_original)

  time_collision <- compute_points
  names(time_collision)[names(time_collision) == "time"] <- "time_order"
  expect_error(
    compute_centroid_path(
      time_collision, "time_order", "id", dimensions = c("x", "y")
    ),
    "Centroid-path output column collision.*time_order"
  )

  bootstrap_points <- compute_points
  names(bootstrap_points)[names(bootstrap_points) == "n_total"] <-
    "centroid_x_lower"
  expect_error(
    bootstrap_centroid_path(
      bootstrap_points, "time", "id", group_vars = "centroid_x_lower",
      dimensions = c("x", "y"), n_boot = 2, seed = 1
    ),
    "Bootstrapped centroid-path output column collision.*centroid_x_lower"
  )

  suffix_points <- compute_points
  suffix_points$x_lower <- suffix_points$x + 1
  expect_error(
    bootstrap_centroid_path(
      suffix_points, "time", "id", dimensions = c("x", "x_lower"),
      n_boot = 2, seed = 1
    ),
    "dimension names generate duplicate output column.*centroid_x_lower"
  )

  comparison_a <- compute_points
  comparison_b <- compute_points
  names(comparison_a)[names(comparison_a) == "n_total"] <- "n_used"
  names(comparison_b)[names(comparison_b) == "n_total"] <- "n_used"
  expect_error(
    compare_centroid_paths(
      comparison_a, comparison_b, "time", "id", group_vars = "n_used",
      dimensions = c("x", "y"), n_boot = 2, seed = 1
    ),
    "Paired centroid-path comparison output column collision.*n_used"
  )
  expect_identical(comparison_a$n_used, compute_points$n_total)
  expect_identical(comparison_b$n_used, compute_points$n_total)

  comparison_a$x_lower <- comparison_a$x + 1
  comparison_b$x_lower <- comparison_b$x + 1
  expect_error(
    compare_centroid_paths(
      comparison_a, comparison_b, "time", "id",
      dimensions = c("x", "x_lower"), n_boot = 2, seed = 1
    ),
    "dimension names generate duplicate output column.*centroid_a_x_lower"
  )
})

test_that("invalid contracts fail early with useful messages", {
  points <- data.frame(id = 1, time = 1, x = 0, label = "not numeric")
  expect_error(compute_centroid_path(points, "time", "id", dimensions = "z"),
               "Missing required")
  expect_error(compute_centroid_path(points, "time", "id",
                                     dimensions = "label"),
               "must be numeric")
  expect_error(compute_centroid_path(points, "time", "id", dimensions = "x",
                                     order = c(1, 1)),
               "duplicate")
  expect_error(compute_centroid_path(points, "time", "id", dimensions = "x",
                                     order = 2),
               "every observed")
  expect_error(bootstrap_centroid_path(points, "time", "id", dimensions = "x",
                                       n_boot = 1),
               "at least 2")
})
