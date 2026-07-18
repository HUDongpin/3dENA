independent_analysis_candidates <- c(
  file.path("R", "trajectory_analysis.R"),
  file.path("..", "..", "R", "trajectory_analysis.R"),
  file.path("..", "R", "trajectory_analysis.R")
)
independent_analysis_file <- independent_analysis_candidates[
  file.exists(independent_analysis_candidates)
][1L]
if (is.na(independent_analysis_file)) {
  stop("Could not locate R/trajectory_analysis.R")
}
source(independent_analysis_file, local = FALSE)
rm(independent_analysis_candidates, independent_analysis_file)

test_that("independent comparison never pairs equal ID text across sides", {
  side_a <- expand.grid(
    id = paste("Student", 1:8), time = 1:2,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  side_b <- expand.grid(
    id = paste("Student", 1:10), time = 1:2,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  side_a$x <- side_a$time
  side_b$x <- side_b$time + 5
  original_a <- side_a
  original_b <- side_b

  set.seed(710)
  rng_before <- .Random.seed
  comparison <- compare_independent_centroid_paths(
    side_a, side_b, "time", "id", dimensions = "x", order = 1:2,
    cohort_policy = "complete", n_boot = 200, n_perm = 99,
    seed = 7, labels = c("Experimental", "Control")
  )

  expect_identical(side_a, original_a)
  expect_identical(side_b, original_b)
  expect_identical(.Random.seed, rng_before)
  expect_s3_class(comparison, "independent_centroid_path_comparison")
  expect_equal(comparison$n_a_used, c(8L, 8L))
  expect_equal(comparison$n_b_used, c(10L, 10L))
  expect_equal(comparison$difference_x, c(5, 5))
  expect_equal(comparison$difference_x_lower, c(5, 5))
  expect_equal(comparison$difference_x_upper, c(5, 5))
  expect_equal(
    attr(comparison, "comparison_spec")$matching,
    "none; participant ID namespaces are side-specific"
  )
  expect_equal(
    attr(comparison, "comparison_spec")$difference_direction,
    "Control - Experimental"
  )
  bootstrap_spec <- attr(comparison, "bootstrap_spec")
  expect_equal(bootstrap_spec$n_participants_a, 8L)
  expect_equal(bootstrap_spec$n_participants_b, 10L)
  expect_equal(bootstrap_spec$failed_replicates, 0L)
})

test_that("independent seeded resampling is invariant to raw row order", {
  side_a <- expand.grid(
    id = paste0("a", 1:7), time = 1:3,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  side_b <- expand.grid(
    id = paste0("b", 1:9), time = 1:3,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  side_a$x <- as.numeric(sub("a", "", side_a$id)) + side_a$time^2
  side_b$x <- as.numeric(sub("b", "", side_b$id)) / 2 + side_b$time^2

  original <- compare_independent_centroid_paths(
    side_a, side_b, "time", "id", dimensions = "x", order = 1:3,
    cohort_policy = "complete", n_boot = 79, n_perm = 79,
    conf_level = 0.80, seed = 19, p_adjust_method = "none"
  )
  set.seed(88)
  shuffled <- compare_independent_centroid_paths(
    side_a[sample(nrow(side_a)), , drop = FALSE],
    side_b[sample(nrow(side_b)), , drop = FALSE],
    "time", "id", dimensions = "x", order = 1:3,
    cohort_policy = "complete", n_boot = 79, n_perm = 79,
    conf_level = 0.80, seed = 19, p_adjust_method = "none"
  )
  columns <- c(
    "centroid_a_x", "centroid_b_x", "difference_x",
    "difference_x_lower", "difference_x_upper",
    "difference_x_p_value", "difference_x_p_adjusted",
    "step_distance_difference", "step_distance_difference_p_value"
  )
  expect_equal(original[columns], shuffled[columns], tolerance = 0)
})

test_that("independent permutation inference detects a large group difference", {
  side_a <- expand.grid(
    id = paste("Student", 1:12), time = 1:2,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  side_b <- expand.grid(
    id = paste("Student", 1:14), time = 1:2,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  side_a$x <- as.numeric(sub("Student ", "", side_a$id)) / 10 + side_a$time
  side_b$x <- as.numeric(sub("Student ", "", side_b$id)) / 10 +
    side_b$time + 5

  comparison <- compare_independent_centroid_paths(
    side_a, side_b, "time", "id", dimensions = "x", order = 1:2,
    cohort_policy = "complete", n_boot = 200, n_perm = 199,
    seed = 42, labels = c("Experimental", "Control")
  )

  expect_true(all(comparison$difference_x_lower > 0))
  expect_equal(comparison$difference_x_p_value, c(0.005, 0.005))
  expect_true(all(comparison$difference_x_p_adjusted <= 0.05))
  expect_true(all(comparison$difference_x_significant))
  expect_false(comparison$delta_difference_x_significant[2L])
  permutation_spec <- attr(comparison, "permutation_spec")
  expect_equal(permutation_spec$p_adjust_method, "holm")
  expect_equal(
    permutation_spec$finite_sample_correction,
    "(1 + exceedances) / (1 + valid permutations)"
  )
  expect_equal(permutation_spec$failed_replicates, 0L)
})

test_that("identical independent distributions are not declared significant", {
  make_side <- function(prefix) {
    data <- expand.grid(
      id = paste0(prefix, 1:8), time = 1:2,
      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
    )
    data$x <- data$time
    data
  }
  comparison <- compare_independent_centroid_paths(
    make_side("a"), make_side("b"), "time", "id",
    dimensions = "x", order = 1:2, cohort_policy = "complete",
    n_boot = 50, n_perm = 49, conf_level = 0.80, seed = 3
  )

  expect_equal(comparison$difference_x, c(0, 0))
  expect_equal(comparison$difference_x_p_value, c(1, 1))
  expect_equal(comparison$difference_x_p_adjusted, c(1, 1))
  expect_false(any(comparison$difference_x_significant))
  expect_true(is.na(comparison$step_distance_difference_p_value[1L]))
  expect_equal(comparison$step_distance_difference_perm_n[1L], 0L)
})

test_that("Monte Carlo p-value agrees with an exact participant-label test", {
  side_a <- data.frame(id = paste0("a", 1:3), time = 1, x = 0:2)
  side_b <- data.frame(id = paste0("b", 1:3), time = 1, x = 3:5)
  comparison <- compare_independent_centroid_paths(
    side_a, side_b, "time", "id", dimensions = "x", order = 1,
    cohort_policy = "complete", n_boot = 50, n_perm = 999,
    conf_level = 0.80, seed = 21, p_adjust_method = "none"
  )

  pooled <- 0:5
  assignments <- combn(seq_along(pooled), 3L)
  exact_statistics <- apply(assignments, 2L, function(selected) {
    abs(mean(pooled[-selected]) - mean(pooled[selected]))
  })
  observed <- abs(mean(side_b$x) - mean(side_a$x))
  exact_p <- mean(exact_statistics >= observed)

  expect_equal(exact_p, 0.1)
  expect_equal(
    comparison$difference_x_p_value, exact_p,
    tolerance = 0.03
  )
  expect_equal(
    comparison$difference_x_p_adjusted,
    comparison$difference_x_p_value
  )
})

test_that("complete cohorts and strata are side-specific", {
  side_a <- rbind(
    expand.grid(site = "north", id = c("1", "2", "3"), time = 1:2,
                KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE),
    expand.grid(site = "south", id = c("1", "2"), time = 1:2,
                KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  )
  side_a <- side_a[!(side_a$site == "north" & side_a$id == "3" &
                       side_a$time == 2), , drop = FALSE]
  side_b <- rbind(
    expand.grid(site = "north", id = c("1", "2", "3", "4"), time = 1:2,
                KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE),
    expand.grid(site = "south", id = c("1", "2", "3"), time = 1:2,
                KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  )
  side_a$x <- as.numeric(side_a$id) + side_a$time
  side_b$x <- as.numeric(side_b$id) + side_b$time + 1

  comparison <- compare_independent_centroid_paths(
    side_a, side_b, "time", "id", group_vars = "site",
    dimensions = "x", order = 1:2, cohort_policy = "complete",
    n_boot = 50, n_perm = 49, conf_level = 0.80, seed = 5
  )
  north <- comparison$site == "north"
  south <- comparison$site == "south"
  expect_equal(comparison$n_a_used[north], c(2L, 2L))
  expect_equal(comparison$n_b_used[north], c(4L, 4L))
  expect_equal(comparison$n_a_used[south], c(2L, 2L))
  expect_equal(comparison$n_b_used[south], c(3L, 3L))
  expect_equal(comparison$n_a_cohort_excluded[north], c(1L, 0L))
  bootstrap_spec <- attr(comparison, "bootstrap_spec")
  expect_equal(unname(bootstrap_spec$stratum_sizes_a), c(2L, 2L))
  expect_equal(unname(bootstrap_spec$stratum_sizes_b), c(4L, 3L))
})

test_that("independent comparison exposes full-space and contract failures", {
  side_a <- data.frame(
    id = c("a1", "a2"), time = 1, x = c(0, 2), z = c(0, NA)
  )
  side_b <- data.frame(
    id = c("b1", "b2"), time = 1, x = c(1, 3), z = c(1, NA)
  )
  comparison <- suppressWarnings(compare_independent_centroid_paths(
    side_a, side_b, "time", "id", dimensions = "x",
    distance_space = "full", full_dimensions = c("x", "z"),
    n_boot = 50, n_perm = 49, conf_level = 0.80, seed = 2
  ))
  diagnostics <- attr(comparison, "trajectory_warnings")
  expect_equal(comparison$difference_x, 1)
  expect_true(is.na(comparison$centroid_difference_distance))
  expect_true(all(c(
    "full_distance_incomplete_a", "full_distance_incomplete_b"
  ) %in% diagnostics$code))

  collision_a <- transform(side_a, difference_x_p_value = "A")
  collision_b <- transform(side_b, difference_x_p_value = "A")
  expect_error(
    compare_independent_centroid_paths(
      collision_a, collision_b, "time", "id",
      group_vars = "difference_x_p_value", dimensions = "x",
      n_boot = 2, n_perm = 2
    ),
    "Independent centroid-path comparison output column collision"
  )
  expect_error(
    compare_independent_centroid_paths(
      side_a, side_b, "time", "id", dimensions = "x",
      n_boot = 2, n_perm = 1
    ),
    "`n_perm` must be one integer of at least 2"
  )
})
