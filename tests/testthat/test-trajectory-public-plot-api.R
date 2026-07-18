trajectory_public_plot_candidates <- c(
  file.path("R", "trajectory_plot.R"),
  file.path("..", "..", "R", "trajectory_plot.R")
)
trajectory_public_plot_file <-
  trajectory_public_plot_candidates[file.exists(trajectory_public_plot_candidates)][1L]
if (is.na(trajectory_public_plot_file)) {
  stop("Cannot locate R/trajectory_plot.R for public plotting API tests.")
}
source(trajectory_public_plot_file, local = FALSE)
rm(trajectory_public_plot_candidates, trajectory_public_plot_file)

make_attribute_free_public_path <- function() {
  data.frame(
    condition = c("A", "A", "B", "B"),
    period = c("T1", "T2", "T1", "T2"),
    time_value = c("T1", "T2", "T1", "T2"),
    time_order = c(1L, 2L, 1L, 2L),
    n_rows_total = c(3L, 4L, 5L, 6L),
    n_total = c(3L, 4L, 5L, 6L),
    n_used = c(3L, 4L, 5L, 6L),
    n_missing = 0L,
    n_excluded = 0L,
    n_cohort_excluded = 0L,
    n_zero_weight = 0L,
    n_rows_missing = 0L,
    n_distance_incomplete = 0L,
    n_rows_distance_incomplete = 0L,
    n_duplicate_rows = 0L,
    centroid_D1 = c(1, 2, 3, 4),
    centroid_D2 = c(10, 20, 30, 40),
    centroid_D1_boot_n = c(97L, 98L, 99L, 100L),
    centroid_D3 = c(100, 200, 300, 400),
    step_distance = c(NA, 1, NA, 1),
    step_distance_boot_n = c(0L, 91L, 0L, 92L),
    speed_boot_n = c(0L, 81L, 0L, 82L),
    cumulative_distance_boot_n = c(75L, 76L, 77L, 78L),
    stringsAsFactors = FALSE
  )
}

testthat::test_that("bootstrap counts are never inferred as centroid axes", {
  path <- make_attribute_free_public_path()

  trace <- trajectory_trace_data(path)

  testthat::expect_identical(
    attr(trace, "trajectory_dimensions", exact = TRUE),
    c("centroid_D1", "centroid_D2", "centroid_D3")
  )
  testthat::expect_equal(trace$z, trace$centroid_D3)
  testthat::expect_identical(
    identical(trace$z, trace$centroid_D1_boot_n),
    FALSE
  )
})

testthat::test_that("attribute-free paths do not infer sample counts as groups", {
  path <- make_attribute_free_public_path()
  testthat::expect_identical(attr(path, "trajectory_spec", exact = TRUE), NULL)
  testthat::expect_identical(attr(path, "group_vars", exact = TRUE), NULL)

  trace <- trajectory_trace_data(
    path,
    dimensions = c("D1", "D2", "D3")
  )

  testthat::expect_identical(
    attr(trace, "trajectory_group_cols", exact = TRUE),
    "condition"
  )
  testthat::expect_setequal(unique(trace$.trajectory_label), c("A", "B"))
})

testthat::test_that("attribute-free inference preserves legitimate n-prefix groups", {
  path <- make_attribute_free_public_path()
  path$n_region <- rep(c("north", "south"), each = 2L)

  trace <- trajectory_trace_data(
    path,
    dimensions = c("D1", "D2", "D3")
  )

  testthat::expect_identical(
    attr(trace, "trajectory_group_cols", exact = TRUE),
    c("condition", "n_region")
  )
  testthat::expect_setequal(
    unique(trace$.trajectory_label),
    c("condition=A · n_region=north", "condition=B · n_region=south")
  )
})

testthat::test_that("declared or explicit dimensions may use analytical suffixes", {
  path <- data.frame(
    period = c("T1", "T2"),
    time_value = c("T1", "T2"),
    time_order = 1:2,
    centroid_x_boot_n = c(1, 2),
    centroid_y_lower = c(3, 4),
    centroid_z = c(5, 6),
    centroid_z_boot_n = c(99L, 100L),
    stringsAsFactors = FALSE
  )
  attr(path, "trajectory_spec") <- list(
    dimensions = c("x_boot_n", "y_lower", "z"),
    group_vars = character(),
    time_var = "period"
  )

  declared <- trajectory_trace_data(path)
  testthat::expect_identical(
    attr(declared, "trajectory_dimensions", exact = TRUE),
    c("centroid_x_boot_n", "centroid_y_lower", "centroid_z")
  )
  testthat::expect_equal(declared$x, path$centroid_x_boot_n)
  testthat::expect_equal(declared$y, path$centroid_y_lower)
  testthat::expect_equal(declared$z, path$centroid_z)

  attr(path, "trajectory_spec") <- NULL
  explicit <- trajectory_trace_data(
    path, dimensions = c("x_boot_n", "y_lower", "z")
  )
  testthat::expect_identical(
    unname(attr(explicit, "trajectory_dimensions", exact = TRUE)),
    c("centroid_x_boot_n", "centroid_y_lower", "centroid_z")
  )
})
