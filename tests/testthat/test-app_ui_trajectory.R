library(testthat)

.trajectory_ui_test_root <- c(".", "../..", "..")
.trajectory_ui_test_root <- .trajectory_ui_test_root[file.exists(
  file.path(.trajectory_ui_test_root, "R", "app_ui_trajectory.R")
)][1L]
if (is.na(.trajectory_ui_test_root)) {
  stop("Could not locate the project R directory.")
}
source(
  file.path(.trajectory_ui_test_root, "R", "app_ui_trajectory.R"),
  local = FALSE
)


test_that("trajectory UI exposes resource, selector, overlay, and Plot Tools guidance", {
  skip_if_not_installed("shiny")
  html <- htmltools::renderTags(trajectory_controls_ui("trajectory-test"))$html

  expect_match(html, 'value="500"', fixed = TRUE)
  expect_match(html, 'min="200"', fixed = TRUE)
  expect_match(html, 'max="500"', fixed = TRUE)
  expect_match(html, "trajectory-test-id_coverage_status", fixed = TRUE)
  expect_match(html, "trajectory-test-comparison_overlap_status", fixed = TRUE)
  expect_match(html, "same physical entity", fixed = TRUE)
  expect_match(html, "Global cluster", fixed = TRUE)
  expect_match(html, "Group-stratified", fixed = TRUE)
  expect_match(html, "200–500 per run", fixed = TRUE)
  expect_match(html, "trajectory-test-bootstrap_cost_status", fixed = TRUE)
  expect_match(html, "Network scope", fixed = TRUE)
  expect_match(html, "Overall across all trajectory groups", fixed = TRUE)
  expect_match(html, "Plot Tools scope", fixed = TRUE)
  expect_match(html, "legacy model views", fixed = TRUE)
  expect_match(html, "trajectory-test-show_direction", fixed = TRUE)
  expect_match(html, "Show direction arrows on path segments", fixed = TRUE)
  expect_match(
    html,
    'id="trajectory-test-show_direction"[^>]*checked="checked"'
  )
  expect_match(html, "trajectory-test-downloads", fixed = TRUE)
  expect_false(grepl("Path CSV", html, fixed = TRUE))
})

test_that("trajectory plot UI reserves a responsive external node legend", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  html <- htmltools::renderTags(trajectory_plot_ui("trajectory-test"))$html

  expect_match(html, "trajectory-plot-layout", fixed = TRUE)
  expect_match(html, "trajectory-plot-canvas", fixed = TRUE)
  expect_match(html, "trajectory-node-legend-slot", fixed = TRUE)
  expect_match(html, "trajectory-test-node_legend", fixed = TRUE)
  expect_match(html, 'role="region"', fixed = TRUE)
  expect_match(html, 'aria-label="Trajectory node color key"', fixed = TRUE)
  expect_match(html, 'tabindex="0"', fixed = TRUE)
  expect_match(html, "trajectory-test-trajectory_plot", fixed = TRUE)
})
