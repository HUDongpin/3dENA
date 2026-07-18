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

test_that("finite extreme weights produce finite public-API centroids", {
  points <- data.frame(
    id = rep(c("a", "b"), each = 4L),
    time = rep(rep(1:2, each = 2L), 2L),
    x = c(0, 2, 2, 4, 8, 10, 10, 12),
    y = 0,
    weight = 1e308,
    stringsAsFactors = FALSE
  )
  path <- suppressWarnings(compute_centroid_path(
    points, "time", "id", dimensions = c("x", "y"), weights = "weight"
  ))

  expect_equal(path$centroid_x, c(5, 7))
  expect_true(all(is.finite(path$centroid_x)))
  expect_equal(path$step_distance, c(0, 2))

  side_a <- expand.grid(
    id = c("a", "b"), time = 1:2,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  side_a$x <- c(0, 10, 2, 12)
  side_a$y <- 0
  side_a$weight <- 1e308
  side_b <- side_a
  side_b$x <- side_b$x + 1
  side_b$weight <- 5e307
  comparison <- suppressWarnings(compare_centroid_paths(
    side_a, side_b, "time", "id", dimensions = c("x", "y"),
    weights_a = "weight", weights_b = "weight",
    pair_weight_policy = "geometric", n_boot = 2, seed = 11
  ))

  expect_true(all(is.finite(comparison$centroid_a_x)))
  expect_true(all(is.finite(comparison$centroid_b_x)))
  expect_equal(comparison$difference_x, c(1, 1))

  cross_range <- data.frame(
    id = c("small-weight", "large-weight"),
    time = 1,
    x = c(1e200, 1e-200),
    weight = c(1e-200, 1e200),
    stringsAsFactors = FALSE
  )
  cross_path <- suppressWarnings(compute_centroid_path(
    cross_range, "time", "id", dimensions = "x", weights = "weight"
  ))
  expect_equal(cross_path$centroid_x / 1e-200, 2, tolerance = 1e-12)

  cancellation_weights <- c(
    1e308, 1e308 * (1 + 1e-14),
    1e308, 1e308 * (1 + 1e-12)
  )
  cancellation <- data.frame(
    id = rep(c("positive", "negative"), 2L),
    time = rep(1:2, each = 2L),
    x = rep(c(1, -1), 2L),
    weight = cancellation_weights,
    stringsAsFactors = FALSE
  )
  cancellation_path <- suppressWarnings(compute_centroid_path(
    cancellation, "time", "id", dimensions = "x", weights = "weight"
  ))
  ratios <- cancellation_weights[c(2L, 4L)] /
    cancellation_weights[c(1L, 3L)]
  expected_cancellation <- (1 - ratios) / (1 + ratios)
  expect_equal(
    cancellation_path$centroid_x / expected_cancellation,
    c(1, 1), tolerance = 1e-12
  )

  smallest <- 5e-324
  structural <- data.frame(
    id = c("positive", "negative", "tiny-weight"),
    time = 1,
    x = c(1, -1, 1e308),
    weight = c(1e308, 1e308 * (1 + 1e-14), smallest)
  )
  structural_path <- suppressWarnings(compute_centroid_path(
    structural, "time", "id", dimensions = "x", weights = "weight"
  ))
  expect_equal(
    structural_path$centroid_x / expected_cancellation[1L],
    1, tolerance = 1e-12
  )

  maximum <- .Machine$double.xmax
  minimum_normal <- .Machine$double.xmin
  residual <- data.frame(
    id = c("positive", "negative", "recovered"),
    time = 1,
    x = c(
      maximum * minimum_normal,
      -maximum * (minimum_normal + smallest),
      maximum
    ),
    weight = c(1, 1, smallest)
  )
  residual_path <- suppressWarnings(compute_centroid_path(
    residual, "time", "id", dimensions = "x", weights = "weight"
  ))
  expect_equal(
    residual_path$centroid_x / .Machine$double.eps,
    1, tolerance = 1e-12
  )

  subnormal_cancellation <- data.frame(
    id = c("anchor", "positive", "negative"),
    time = 1,
    x = c(0, 1e308, -1e308),
    weight = c(1, 1e-309, 1e-309 * (1 + 1e-14))
  )
  subnormal_path <- suppressWarnings(compute_centroid_path(
    subnormal_cancellation, "time", "id",
    dimensions = "x", weights = "weight"
  ))
  subnormal_scaled <- subnormal_cancellation$weight /
    max(subnormal_cancellation$weight)
  subnormal_expected <- sum(
    subnormal_scaled * subnormal_cancellation$x
  ) / sum(subnormal_scaled)
  expect_equal(
    subnormal_path$centroid_x / subnormal_expected,
    1, tolerance = 1e-12
  )

  cancellation_values <- c(maximum, -maximum, 1e-100)
  permutations <- list(
    c(1L, 2L, 3L), c(1L, 3L, 2L), c(2L, 1L, 3L),
    c(2L, 3L, 1L), c(3L, 1L, 2L), c(3L, 2L, 1L)
  )
  permutation_centroids <- vapply(permutations, function(index) {
    ordered <- data.frame(
      id = letters[index], time = 1,
      x = cancellation_values[index], weight = 1
    )
    suppressWarnings(compute_centroid_path(
      ordered, "time", "id", dimensions = "x", weights = "weight"
    ))$centroid_x
  }, numeric(1L))
  expect_equal(
    permutation_centroids / (1e-100 / 3),
    rep(1, length(permutations)), tolerance = 1e-12
  )

  maximum_path <- suppressWarnings(compute_centroid_path(
    data.frame(id = letters[1:3], time = 1, x = maximum, weight = 1),
    "time", "id", dimensions = "x", weights = "weight"
  ))
  expect_true(is.finite(maximum_path$centroid_x))
  expect_equal(maximum_path$centroid_x, maximum)
})

test_that("large finite displacements use an overflow-safe Euclidean norm", {
  points <- data.frame(
    id = rep(c("a", "b"), each = 2L),
    time = rep(1:2, 2L),
    x = rep(c(0, 3e200), 2L),
    y = rep(c(0, 4e200), 2L),
    stringsAsFactors = FALSE
  )
  path <- suppressWarnings(compute_centroid_path(
    points, "time", "id", dimensions = c("x", "y")
  ))

  expect_true(is.finite(path$step_distance[2L]))
  expect_equal(path$step_distance, c(0, 5e200))
  expect_equal(path$cumulative_distance, c(0, 5e200))

  side_a <- data.frame(id = c("a", "b"), time = 1, x = 0, y = 0)
  side_b <- side_a
  side_b$x <- 3e200
  side_b$y <- 4e200
  comparison <- suppressWarnings(compare_centroid_paths(
    side_a, side_b, "time", "id", dimensions = c("x", "y"),
    n_boot = 2, seed = 4
  ))

  expect_true(is.finite(comparison$centroid_difference_distance))
  expect_equal(comparison$centroid_difference_distance, 5e200)
})

test_that("missing key rows are visible without double-counting", {
  side_a <- data.frame(
    id = c("a", NA, "b", "c", "d"),
    time = c(1, 1, NA, 2, 2),
    group = c("G", "G", "G", NA, "G"),
    x = 1:5,
    stringsAsFactors = FALSE
  )
  path <- suppressWarnings(compute_centroid_path(
    side_a, "time", "id", group_vars = "group", dimensions = "x",
    order = 1:2
  ))
  diagnostics <- attr(path, "trajectory_warnings")
  spec <- attr(path, "trajectory_spec")

  expect_equal(path$n_rows_total, c(2L, 1L))
  expect_equal(path$n_rows_missing_key, c(1L, 0L))
  expect_equal(path$n_total, c(1L, 1L))
  expect_equal(diagnostics$count[diagnostics$code == "missing_key_rows"], 3L)
  expect_equal(
    diagnostics$count[diagnostics$code == "unassigned_missing_key_rows"],
    2L
  )
  expect_equal(spec$missing_key_counts$total, 3L)
  expect_equal(
    spec$missing_key_counts$total,
    sum(path$n_rows_missing_key) + spec$missing_key_counts$unassigned
  )
  expect_match(spec$missing_key_policy, "slice-assignable")

  side_b <- data.frame(
    id = c("a", "d", NA, "z"),
    time = c(1, 2, 2, NA),
    group = "G",
    x = c(2, 6, 100, 100),
    stringsAsFactors = FALSE
  )
  comparison <- suppressWarnings(compare_centroid_paths(
    side_a, side_b, "time", "id", group_vars = "group", dimensions = "x",
    order = 1:2, n_boot = 2, seed = 8
  ))
  comparison_diagnostics <- attr(comparison, "trajectory_warnings")
  comparison_spec <- attr(comparison, "comparison_spec")

  expect_equal(comparison$n_a_rows_missing_key, c(1L, 0L))
  expect_equal(comparison$n_b_rows_missing_key, c(0L, 1L))
  expect_equal(
    comparison_diagnostics$count[
      comparison_diagnostics$code == "missing_key_rows_a"
    ],
    3L
  )
  expect_equal(
    comparison_diagnostics$count[
      comparison_diagnostics$code == "missing_key_rows_b"
    ],
    2L
  )
  expect_equal(
    comparison_spec$missing_key_counts$a$total,
    sum(comparison$n_a_rows_missing_key) +
      comparison_spec$missing_key_counts$a$unassigned
  )
  expect_equal(
    comparison_spec$missing_key_counts$b$total,
    sum(comparison$n_b_rows_missing_key) +
      comparison_spec$missing_key_counts$b$unassigned
  )
})

test_that("physically equivalent difftime units share one order key space", {
  hours <- as.difftime(c(1, 2), units = "hours")
  points <- data.frame(id = "a", time = hours, x = c(1, 2))
  path <- suppressWarnings(compute_centroid_path(
    points, "time", "id", dimensions = "x",
    order = as.difftime(c(60, 120), units = "mins")
  ))

  expect_s3_class(path$time, "difftime")
  expect_equal(as.numeric(path$time, units = "hours"), c(1, 2))
  expect_equal(path$elapsed_interval, c(NA, 1))
  expect_equal(attr(path, "trajectory_spec")$elapsed_interval_units, "hours")

  side_a <- data.frame(
    id = rep(c("a", "b"), each = 2L),
    time = rep(hours, 2L), x = c(0, 1, 2, 3), y = 0
  )
  side_b <- side_a
  side_b$time <- as.difftime(
    as.numeric(side_a$time, units = "mins"), units = "mins"
  )
  side_b$x <- side_b$x + 1
  comparison <- suppressWarnings(compare_centroid_paths(
    side_a, side_b, "time", "id", dimensions = c("x", "y"),
    order = as.difftime(c(3600, 7200), units = "secs"),
    n_boot = 2, seed = 13
  ))

  expect_equal(comparison$n_matched, c(2L, 2L))
  expect_equal(comparison$difference_x, c(1, 1))
  expect_equal(
    as.numeric(comparison$time, units = "hours"),
    c(1, 2)
  )

  fractional <- 0.56458364503923808
  fractional_a <- data.frame(
    id = c("a", "b"),
    time = as.difftime(rep(fractional, 2L), units = "hours"),
    x = c(0, 2)
  )
  fractional_b <- data.frame(
    id = c("a", "b"),
    time = as.difftime(rep(fractional * 60, 2L), units = "mins"),
    x = c(1, 3)
  )
  implicit_fractional <- suppressWarnings(compare_centroid_paths(
    fractional_a, fractional_b, "time", "id", dimensions = "x",
    n_boot = 2, seed = 21
  ))
  explicit_fractional <- suppressWarnings(compare_centroid_paths(
    fractional_a, fractional_b, "time", "id", dimensions = "x",
    order = as.difftime(fractional * 3600, units = "secs"),
    n_boot = 2, seed = 21
  ))

  expect_equal(nrow(implicit_fractional), 1L)
  expect_equal(implicit_fractional$n_matched, 2L)
  expect_equal(explicit_fractional$n_matched, 2L)
  expect_equal(explicit_fractional$difference_x, 1)

  small_fractional <- 2.8989675361663103e-07
  small_a <- data.frame(
    id = c("a", "b"),
    time = as.difftime(rep(small_fractional, 2L), units = "hours"),
    x = c(0, 2)
  )
  small_b <- data.frame(
    id = c("a", "b"),
    time = as.difftime(rep(small_fractional * 60, 2L), units = "mins"),
    x = c(1, 3)
  )
  implicit_small <- suppressWarnings(compare_centroid_paths(
    small_a, small_b, "time", "id", dimensions = "x",
    n_boot = 2, seed = 22
  ))
  explicit_small <- suppressWarnings(compare_centroid_paths(
    small_a, small_b, "time", "id", dimensions = "x",
    order = as.difftime(small_fractional * 3600, units = "secs"),
    n_boot = 2, seed = 22
  ))
  expect_equal(nrow(implicit_small), 1L)
  expect_equal(implicit_small$n_matched, 2L)
  expect_equal(explicit_small$n_matched, 2L)

  physical_seconds <- as.numeric(small_a$time[1L], units = "secs")
  adjacent_gap <- 64 * abs(physical_seconds) * .Machine$double.eps
  adjacent <- physical_seconds + adjacent_gap
  adjacent_points <- data.frame(
    id = "a",
    time = as.difftime(c(physical_seconds, adjacent), units = "secs"),
    x = 1:2
  )
  adjacent_path <- suppressWarnings(compute_centroid_path(
    adjacent_points, "time", "id", dimensions = "x"
  ))
  expect_equal(nrow(adjacent_path), 2L)
  expect_equal(adjacent_path$time_order, 1:2)

  expect_error(
    compute_centroid_path(
      adjacent_points[1L, ], "time", "id", dimensions = "x",
      order = as.difftime(
        c(
          physical_seconds,
          physical_seconds + 2 * abs(physical_seconds) *
            .Machine$double.eps
        ),
        units = "secs"
      )
    ),
    "duplicate time values"
  )
})
