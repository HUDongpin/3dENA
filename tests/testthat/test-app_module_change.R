source("../../R/app_utils.R")
source("../../R/app_module_ena_unit_group_change_plot.R")
source("../../R/transition.R")

test_that("Change values and point slices follow the actively selected variable", {
  points <- data.frame(
    period = c(2, 1, 2, 1),
    cohort = c("A", "A", "B", "B"),
    MR1 = c(20, 10, 200, 100)
  )

  expect_equal(ena3d_change_group_values(points, "period"), c(1, 2))
  expect_equal(ena3d_change_group_values(points, "cohort"), c("A", "B"))
  expect_equal(
    ena3d_change_group_points(points, "period", 1)$MR1,
    c(10, 100)
  )
  expect_equal(
    ena3d_change_group_points(points, "cohort", "A")$MR1,
    c(20, 10)
  )
})

test_that("Change and network means match POSIXct groups selected as labels", {
  fold <- as.POSIXct(
    c("2025-11-02 01:30:00 -0400", "2025-11-02 01:30:00 -0500"),
    format = "%Y-%m-%d %H:%M:%S %z",
    tz = "America/New_York"
  )
  timestamps <- c(fold[[1L]], fold[[2L]], fold[[1L]])
  attr(timestamps, "tzone") <- "America/New_York"
  points <- data.frame(observed_at = timestamps, MR1 = c(1, 2, 3))
  selected <- ena3d_group_value_labels(timestamps)[[1L]]
  expect_identical(
    ena3d_change_group_points(points, "observed_at", selected)$MR1,
    c(1, 3)
  )

  object_name <- load("../../sample_data/newfrat_enaset.Rdata")
  ena_obj <- get(object_name)
  observed_at <- rep(fold, length.out = nrow(ena_obj$line.weights))
  class(observed_at) <- c("ena.metadata", "POSIXct", "POSIXt")
  attr(observed_at, "tzone") <- "America/New_York"
  ena_obj$line.weights$observed_at <- observed_at
  selected <- ena3d_group_value_labels(observed_at)[[1L]]
  expected_rows <- ena3d_group_value_match(observed_at, selected)
  expected <- colMeans(as.matrix(rENA::remove_meta_data(
    ena_obj$line.weights[expected_rows, , drop = FALSE]
  )))

  expect_equal(
    get_mean_group_lineweights(ena_obj, "observed_at", selected),
    as.vector(expected)
  )
  expect_equal(
    get_mean_group_lineweights_in_groups(
      ena_obj, "observed_at", selected
    ),
    as.vector(expected)
  )
})

test_that("Change network means follow the actively selected variable", {
  object_name <- load("../../sample_data/newfrat_enaset.Rdata")
  ena_obj <- get(object_name)
  selected_name <- as.character(ena_obj$line.weights$Name[[1L]])

  actual <- get_mean_group_lineweights(ena_obj, "Name", selected_name)
  selected_rows <- ena_obj$line.weights$Name %in% selected_name
  expected <- colMeans(as.matrix(
    rENA::remove_meta_data(ena_obj$line.weights[selected_rows, , drop = FALSE])
  ))

  expect_equal(actual, as.vector(expected))
  expect_equal(
    nrow(ena3d_change_group_points(ena_obj$points, "Name", selected_name)),
    sum(ena_obj$points$Name %in% selected_name)
  )
})

test_that("Change cache cannot be reused after any plot-defining input changes", {
  arguments <- list(
    dataset_id = "dataset-a",
    group_var = "period",
    axes = c("MR1", "SVD2", "SVD3"),
    scale_factor = 1,
    line_width = 3,
    show_grid = TRUE,
    show_zeroline = TRUE,
    axis_arrows = c(x = FALSE, y = FALSE, z = FALSE),
    show_mean = TRUE,
    show_confidence_interval = FALSE
  )
  key <- do.call(ena3d_change_cache_key, arguments)
  cache <- ena3d_tag_change_cache(list(`1` = "period-1-plot"), key)

  expect_true(ena3d_change_cache_is_valid(cache, key))
  expect_false(ena3d_change_cache_is_valid(list(`1` = "legacy-plot"), key))

  replacements <- list(
    dataset_id = "dataset-b",
    group_var = "cohort",
    axes = c("SVD4", "SVD5", "SVD6"),
    scale_factor = 2,
    line_width = 7,
    show_grid = FALSE,
    show_zeroline = FALSE,
    axis_arrows = c(x = TRUE, y = FALSE, z = FALSE),
    show_mean = FALSE,
    show_confidence_interval = TRUE
  )
  for (field in names(replacements)) {
    changed <- arguments
    changed[[field]] <- replacements[[field]]
    expect_false(
      ena3d_change_cache_is_valid(
        cache,
        do.call(ena3d_change_cache_key, changed)
      ),
      info = sprintf("cache field %s must invalidate", field)
    )
  }
})

test_that("Change rejects public-app high-cardinality variables", {
  expect_invisible(ena3d_validate_change_cardinality(letters, max_levels = 30L))
  expect_error(
    ena3d_validate_change_cardinality(seq_len(201), max_levels = 200L),
    "lower-cardinality"
  )
})

test_that("Change plot cache is bounded and least-recently-used", {
  cache <- list()
  cache <- ena3d_change_lru_put(cache, list(value = "A"), "plot-a", 2L)
  cache <- ena3d_change_lru_put(cache, list(value = "B"), "plot-b", 2L)
  hit_a <- ena3d_change_lru_get(cache, list(value = "A"))
  expect_true(hit_a$hit)
  expect_identical(hit_a$plot, "plot-a")

  cache <- ena3d_change_lru_put(
    hit_a$cache, list(value = "C"), "plot-c", 2L
  )
  expect_false(ena3d_change_lru_get(cache, list(value = "B"))$hit)
  expect_true(ena3d_change_lru_get(cache, list(value = "A"))$hit)
  expect_true(ena3d_change_lru_get(cache, list(value = "C"))$hit)
  expect_lte(length(cache), 2L)
})

test_that("Change renders only the requested value and preserves contextual title", {
  source_text <- paste(
    readLines("../../R/app_module_ena_unit_group_change_plot.R", warn = FALSE),
    collapse = "\n"
  )
  expect_match(source_text, "make_unit_group_change_plot <- function", fixed = TRUE)
  expect_false(grepl("make_unit_group_change_plots <- reactive", source_text, fixed = TRUE))
  expect_false(grepl("title=input$camera_position", source_text, fixed = TRUE))
  expect_match(source_text, "uirevision", fixed = TRUE)
})
