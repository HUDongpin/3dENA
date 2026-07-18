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

test_that("full-space distance always includes selected dimensions", {
  points <- data.frame(
    id = rep(1:2, 2),
    time = rep(1:2, each = 2),
    x = c(0, 0, 3, 3),
    y = c(0, 0, 4, 4),
    z = c(0, 0, 12, 12)
  )

  path <- suppressWarnings(compute_centroid_path(
    points, "time", "id", dimensions = c("x", "y"),
    distance_space = "full", full_dimensions = "z"
  ))

  expect_equal(path$step_distance, c(0, 13))
  expect_identical(
    attr(path, "trajectory_spec")$distance_dimensions,
    c("z", "x", "y")
  )
})

test_that("integer and double analytical keys match semantically", {
  side_a <- data.frame(
    id = rep(c(1L, 2L), 2),
    time = rep(c(1L, 2L), each = 2),
    x = c(0, 10, 2, 12),
    y = 0
  )
  side_b <- data.frame(
    id = rep(c(1, 2), 2),
    time = rep(c(1, 2), each = 2),
    x = side_a$x + 1,
    y = 0
  )

  comparison <- suppressWarnings(compare_centroid_paths(
    side_a, side_b, "time", "id", dimensions = c("x", "y"),
    n_boot = 20, seed = 17
  ))

  expect_identical(.trajectory_value_key(c(1L, 2L)),
                   .trajectory_value_key(c(1, 2)))
  expect_equal(comparison$n_matched, c(2L, 2L))
  expect_equal(comparison$difference_x, c(1, 1))
  expect_false("no_matched_participants" %in%
                 attr(comparison, "trajectory_warnings")$code)
})

test_that("bootstrap preserves ID-column weight semantics after cloning", {
  points <- expand.grid(id = 1:4, time = 1:2)
  points$x <- points$id^2 + points$time
  points$y <- points$id * points$time
  points$weight_copy <- points$id

  weighted_by_id <- suppressWarnings(bootstrap_centroid_path(
    points, "time", "id", dimensions = c("x", "y"), weights = "id",
    n_boot = 100, conf_level = 0.5, seed = 29
  ))
  weighted_by_copy <- suppressWarnings(bootstrap_centroid_path(
    points, "time", "id", dimensions = c("x", "y"),
    weights = "weight_copy", n_boot = 100, conf_level = 0.5, seed = 29
  ))

  interval_columns <- grep("_(lower|upper|boot_n)$", names(weighted_by_id),
                           value = TRUE)
  expect_equal(weighted_by_id[interval_columns],
               weighted_by_copy[interval_columns])
  expect_equal(weighted_by_id$centroid_x, weighted_by_copy$centroid_x)
  expect_equal(weighted_by_id$centroid_y, weighted_by_copy$centroid_y)
  expect_identical(attr(weighted_by_id, "trajectory_spec")$weights,
                   "column:id")
})

test_that("paired comparison tolerates different factor level sets", {
  side_a <- data.frame(
    id = rep(c("p1", "p2"), 2),
    time = factor(rep(c("pre", "post"), each = 2),
                  levels = c("pre", "post", "unused-a")),
    group = factor("shared", levels = c("shared", "unused-a")),
    x = c(0, 10, 2, 12),
    y = 0
  )
  side_b <- data.frame(
    id = rep(c("p1", "p2"), 2),
    time = factor(rep(c("pre", "post"), each = 2),
                  levels = c("unused-b", "post", "pre")),
    group = factor("shared", levels = c("unused-b", "shared")),
    x = side_a$x + 3,
    y = 0
  )

  comparison <- suppressWarnings(compare_centroid_paths(
    side_a, side_b, "time", "id", group_vars = "group",
    dimensions = c("x", "y"), order = c("pre", "post"),
    n_boot = 20, seed = 31
  ))

  expect_equal(as.character(comparison$group), c("shared", "shared"))
  expect_equal(comparison$n_matched, c(2L, 2L))
  expect_equal(comparison$difference_x, c(3, 3))
})
