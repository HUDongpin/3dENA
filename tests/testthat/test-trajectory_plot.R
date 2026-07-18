trajectory_plot_candidates <- c(
  file.path("R", "trajectory_plot.R"),
  file.path("..", "..", "R", "trajectory_plot.R")
)
trajectory_plot_file <- trajectory_plot_candidates[file.exists(trajectory_plot_candidates)][1L]
if (is.na(trajectory_plot_file)) stop("Cannot locate R/trajectory_plot.R for tests.")
source(trajectory_plot_file, local = FALSE)
rm(trajectory_plot_candidates, trajectory_plot_file)

make_trajectory_plot_fixture <- function() {
  path <- data.frame(
    condition = c("B", "A", "B", "A", "A", "B"),
    week = c("Week 3", "Week 2", "Week 1", "Week 3", "Week 1", "Week 2"),
    time_value = c("Week 3", "Week 2", "Week 1", "Week 3", "Week 1", "Week 2"),
    time_order = c(3, 2, 1, 3, 1, 2),
    n_total = c(5, 5, 5, 5, 5, 5),
    n_used = c(5, 4, 5, 4, 4, 5),
    n_missing = c(0, 1, 0, 1, 1, 0),
    n_excluded = c(0, 0, 0, 0, 0, 0),
    n_duplicate_rows = c(0, 0, 0, 0, 0, 1),
    centroid_D1 = c(30, 2, 10, 3, 1, 20),
    centroid_D2 = c(300, 20, 100, 30, 10, 200),
    centroid_D3 = c(3000, 200, 1000, 300, 100, 2000),
    centroid_D1_lower = c(29.9, 1.9, 9.9, 2.9, 0.9, 19.9),
    centroid_D1_upper = c(30.1, 2.1, 10.1, 3.1, 1.1, 20.1),
    centroid_D2_lower = c(299.8, 19.8, 99.8, 29.8, 9.8, 199.8),
    centroid_D2_upper = c(300.2, 20.2, 100.2, 30.2, 10.2, 200.2),
    centroid_D3_lower = c(2999.7, 199.7, 999.7, 299.7, 99.7, 1999.7),
    centroid_D3_upper = c(3000.3, 200.3, 1000.3, 300.3, 100.3, 2000.3),
    step_distance = c(10, 1, NA, 1, NA, 10),
    elapsed_interval = c(1, 1, NA, 1, NA, 1),
    speed = c(10, 1, NA, 1, NA, 10),
    cumulative_distance = c(20, 1, 0, 2, 0, 10),
    stringsAsFactors = FALSE
  )
  attr(path, "trajectory_spec") <- list(
    group_vars = "condition",
    time_var = "week",
    dimensions = c("D1", "D2", "D3"),
    distance_space = "selected 3D subspace: D1, D2, D3"
  )
  attr(path, "trajectory_warnings") <- data.frame(
    code = c("changing_cohort", "missing_period"),
    severity = c("warning", "warning"),
    group = c("A", NA_character_),
    time_order = c(2, NA_real_),
    message = c("Cohort composition changed", "A period is missing"),
    count = c(1, 1),
    stringsAsFactors = FALSE
  )
  path
}

trajectory_path_traces <- function(plot) {
  traces <- plotly::plotly_build(plot)$x$data
  Filter(function(trace) {
    is.list(trace$meta) && identical(trace$meta$trajectory_role, "path")
  }, traces)
}

trajectory_direction_traces <- function(plot) {
  traces <- plotly::plotly_build(plot)$x$data
  Filter(function(trace) {
    is.list(trace$meta) && identical(trace$meta$trajectory_role, "direction_arrows")
  }, traces)
}

trajectory_node_marker_traces <- function(plot) {
  traces <- plotly::plotly_build(plot)$x$data
  Filter(function(trace) {
    is.list(trace$meta) && identical(trace$meta$trajectory_role, "node_markers")
  }, traces)
}

trajectory_trace_by_name <- function(traces, name) {
  traces[[which(vapply(traces, function(trace) identical(trace$name, name), logical(1)))[1L]]]
}

trajectory_relative_luminance <- function(colors) {
  rgb <- grDevices::col2rgb(colors) / 255
  linear <- ifelse(
    rgb <= 0.04045,
    rgb / 12.92,
    ((rgb + 0.055) / 1.055)^2.4
  )
  colSums(linear * c(0.2126, 0.7152, 0.0722))
}

trajectory_contrast_ratio <- function(foreground, background) {
  foreground_luminance <- trajectory_relative_luminance(foreground)
  background_luminance <- trajectory_relative_luminance(background)
  (max(foreground_luminance, background_luminance) + 0.05) /
    (min(foreground_luminance, background_luminance) + 0.05)
}

testthat::test_that("trace export is ordered, complete, and analytically unchanged", {
  path <- make_trajectory_plot_fixture()
  original <- path
  export <- trajectory_trace_data(
    path,
    dimensions = c("D1", "D2", "D3"),
    group_cols = "condition"
  )

  testthat::expect_identical(path, original)
  testthat::expect_identical(export$condition, c("A", "A", "A", "B", "B", "B"))
  testthat::expect_identical(export$time_order, c(1, 2, 3, 1, 2, 3))
  testthat::expect_equal(export$x, export$centroid_D1)
  testthat::expect_equal(export$y, export$centroid_D2)
  testthat::expect_equal(export$z, export$centroid_D3)
  testthat::expect_identical(
    unique(export$.distance_space),
    "selected 3D subspace: D1, D2, D3"
  )
  testthat::expect_true(all(c(
    ".trajectory_key", ".trajectory_label", ".trajectory_color",
    ".trajectory_node_key", ".trajectory_node_label", ".trajectory_node_color",
    ".trajectory_warning", ".trajectory_point_order"
  ) %in% names(export)))
  testthat::expect_match(
    export$.trajectory_warning[export$condition == "A" & export$time_order == 2],
    "Cohort composition changed"
  )
})

testthat::test_that("stable color mapping is independent of row order and projection", {
  testthat::skip_if_not_installed("plotly")
  path <- make_trajectory_plot_fixture()
  shuffled <- path[c(6, 3, 4, 1, 5, 2), , drop = FALSE]
  attr(shuffled, "trajectory_spec") <- attr(path, "trajectory_spec")
  attr(shuffled, "trajectory_warnings") <- attr(path, "trajectory_warnings")

  map_a <- trajectory_color_map(path, group_cols = "condition")
  map_b <- trajectory_color_map(shuffled, group_cols = "condition")
  testthat::expect_identical(map_a, map_b)
  custom <- trajectory_color_map(
    path,
    group_cols = "condition",
    colors = c(B = "#112233", A = "#AABBCC")
  )
  custom_labels <- attr(custom, "labels")
  testthat::expect_identical(unname(custom[custom_labels == "A"]), "#AABBCC")
  testthat::expect_identical(unname(custom[custom_labels == "B"]), "#112233")

  plot_3d <- plot_centroid_trajectory_3d(
    path, dimensions = c("D1", "D2", "D3"), group_cols = "condition"
  )
  plot_2d <- plot_centroid_trajectory_2d(
    shuffled, dimensions = c("D1", "D2"), group_cols = "condition"
  )
  traces_3d <- trajectory_path_traces(plot_3d)
  traces_2d <- trajectory_path_traces(plot_2d)
  colors_3d <- stats::setNames(
    vapply(traces_3d, function(trace) trace$line$color, character(1)),
    vapply(traces_3d, `[[`, character(1), "name")
  )
  colors_2d <- stats::setNames(
    vapply(traces_2d, function(trace) trace$line$color, character(1)),
    vapply(traces_2d, `[[`, character(1), "name")
  )
  testthat::expect_identical(colors_3d, colors_2d)
  for (group in c("A", "B")) {
    markers_3d <- as.character(trajectory_trace_by_name(traces_3d, group)$marker$color)
    markers_2d <- as.character(trajectory_trace_by_name(traces_2d, group)$marker$color)
    testthat::expect_identical(markers_3d, markers_2d)
    testthat::expect_identical(length(unique(markers_3d)), 3L)
  }
  testthat::expect_identical(
    attr(plot_3d, "trajectory_node_legend"),
    attr(plot_2d, "trajectory_node_legend")
  )
})

testthat::test_that("node fill colors encode the shared global time order", {
  testthat::skip_if_not_installed("plotly")
  path <- make_trajectory_plot_fixture()
  export <- trajectory_trace_data(
    path, dimensions = c("D1", "D2", "D3"), group_cols = "condition"
  )
  legend <- attr(export, "trajectory_node_legend")

  testthat::expect_identical(nrow(legend), 3L)
  testthat::expect_identical(length(unique(legend$node_color)), 3L)
  testthat::expect_identical(
    legend$node_label,
    c("Order 1 \u00b7 Week 1", "Order 2 \u00b7 Week 2", "Order 3 \u00b7 Week 3")
  )
  for (order_value in 1:3) {
    colors <- unique(export$.trajectory_node_color[export$time_order == order_value])
    testthat::expect_identical(length(colors), 1L)
    testthat::expect_identical(colors, legend$node_color[legend$time_order == order_value])
  }

  plot <- plot_centroid_trajectory_3d(
    path, dimensions = c("D1", "D2", "D3"), group_cols = "condition"
  )
  for (trace in trajectory_node_marker_traces(plot)) {
    key <- trace$meta$trajectory_key
    expected <- export$.trajectory_node_color[export$.trajectory_key == key]
    testthat::expect_identical(as.character(trace$marker$color), expected)
    testthat::expect_true(all(as.character(trace$marker$line$color) ==
      export$.trajectory_color[export$.trajectory_key == key]))
  }
})

testthat::test_that("ordered node colors exclude the dark Viridis range", {
  colors <- .trajectory_ordered_node_colors(15L)
  luminance <- trajectory_relative_luminance(colors)

  testthat::expect_identical(length(colors), 15L)
  testthat::expect_identical(length(unique(colors)), 15L)
  testthat::expect_true(all(luminance >= 0.16))
  testthat::expect_true(all(diff(luminance) > 0))
  testthat::expect_true(
    trajectory_relative_luminance(.trajectory_ordered_node_colors(1L)) >= 0.16
  )
})

testthat::test_that("2D and 3D hover labels use a fixed high-contrast style", {
  testthat::skip_if_not_installed("plotly")
  path <- make_trajectory_plot_fixture()
  plots <- list(
    plot_centroid_trajectory_3d(
      path, dimensions = c("D1", "D2", "D3"), group_cols = "condition"
    ),
    plot_centroid_trajectory_2d(
      path, dimensions = c("D1", "D2"), group_cols = "condition"
    )
  )

  for (plot in plots) {
    hoverlabel <- plotly::plotly_build(plot)$x$layout$hoverlabel
    testthat::expect_identical(hoverlabel$bgcolor, "#FFFFFF")
    testthat::expect_identical(hoverlabel$bordercolor, "#526777")
    testthat::expect_identical(hoverlabel$align, "left")
    testthat::expect_identical(hoverlabel$font$color, "#102A43")
    testthat::expect_true(
      trajectory_contrast_ratio(
        hoverlabel$font$color,
        hoverlabel$bgcolor
      ) >= 4.5
    )
  }
})

testthat::test_that("duplicate time labels remain distinct by explicit order", {
  path <- data.frame(
    time_value = c("Baseline", "Middle", "Baseline"),
    time_order = 1:3,
    centroid_D1 = 1:3,
    centroid_D2 = 2:4,
    centroid_D3 = 3:5,
    stringsAsFactors = FALSE
  )
  attr(path, "trajectory_spec") <- list(
    time_var = "phase", dimensions = c("D1", "D2", "D3"),
    group_vars = character()
  )
  legend <- trajectory_node_legend_data(path)

  testthat::expect_identical(length(unique(legend$node_key)), 3L)
  testthat::expect_identical(length(unique(legend$node_color)), 3L)
  testthat::expect_identical(
    legend$node_label[c(1, 3)],
    c("Order 1 \u00b7 Baseline", "Order 3 \u00b7 Baseline")
  )
  testthat::expect_identical(attr(legend, "time_variable"), "Phase")
})

testthat::test_that("missing coordinates do not compress the ordered color domain", {
  complete <- data.frame(
    time_value = c("T1", "T2", "T3"),
    time_order = 1:3,
    centroid_D1 = c(0, 1, 2),
    centroid_D2 = c(0, 1, 2),
    centroid_D3 = c(0, 1, 2),
    stringsAsFactors = FALSE
  )
  missing <- complete
  missing[2L, c("centroid_D1", "centroid_D2", "centroid_D3")] <- NA_real_
  attr(complete, "trajectory_spec") <- attr(missing, "trajectory_spec") <- list(
    time_var = "period", dimensions = c("D1", "D2", "D3"),
    group_vars = character()
  )

  complete_legend <- trajectory_node_legend_data(complete)
  missing_legend <- trajectory_node_legend_data(missing)
  testthat::expect_identical(complete_legend, missing_legend)
  testthat::expect_identical(nrow(missing_legend), 3L)
  testthat::expect_identical(length(unique(missing_legend$node_color)), 3L)
})

testthat::test_that("3D plot has one ordered lines-and-markers trace per group", {
  testthat::skip_if_not_installed("plotly")
  path <- make_trajectory_plot_fixture()
  plot <- plot_centroid_trajectory_3d(
    path,
    dimensions = c("D1", "D2", "D3"),
    group_cols = "condition"
  )
  traces <- trajectory_path_traces(plot)

  testthat::expect_length(traces, 2L)
  testthat::expect_identical(vapply(traces, `[[`, character(1), "name"), c("A", "B"))
  testthat::expect_true(all(vapply(traces, function(trace) {
    identical(trace$type, "scatter3d") && identical(trace$mode, "lines+markers")
  }, logical(1))))

  group_a <- trajectory_trace_by_name(traces, "A")
  group_b <- trajectory_trace_by_name(traces, "B")
  testthat::expect_equal(as.numeric(group_a$x), c(1, 2, 3))
  testthat::expect_equal(as.numeric(group_a$y), c(10, 20, 30))
  testthat::expect_equal(as.numeric(group_a$z), c(100, 200, 300))
  testthat::expect_equal(as.numeric(group_b$x), c(10, 20, 30))
  testthat::expect_equal(as.numeric(group_b$y), c(100, 200, 300))
  testthat::expect_equal(as.numeric(group_b$z), c(1000, 2000, 3000))

  # Coordinate uncertainty remains on the trajectory trace rather than adding
  # extra traces that could be mistaken for another trajectory.
  testthat::expect_equal(as.numeric(group_a$error_x$array), rep(0.1, 3), tolerance = 1e-12)
  testthat::expect_equal(as.numeric(group_a$error_x$arrayminus), rep(0.1, 3), tolerance = 1e-12)
  testthat::expect_equal(as.numeric(group_a$error_y$array), rep(0.2, 3), tolerance = 1e-12)
  testthat::expect_equal(as.numeric(group_a$error_z$array), rep(0.3, 3), tolerance = 1e-12)
})

testthat::test_that("2D and 3D paths show directional arrowheads without extra legends", {
  testthat::skip_if_not_installed("plotly")
  path <- make_trajectory_plot_fixture()
  plot_3d <- plot_centroid_trajectory_3d(
    path, dimensions = c("D1", "D2", "D3"), group_cols = "condition"
  )
  plot_2d <- plot_centroid_trajectory_2d(
    path, dimensions = c("D1", "D2"), group_cols = "condition"
  )
  arrows_3d <- trajectory_direction_traces(plot_3d)
  arrows_2d <- trajectory_direction_traces(plot_2d)
  nodes_3d <- trajectory_node_marker_traces(plot_3d)
  nodes_2d <- trajectory_node_marker_traces(plot_2d)

  testthat::expect_length(arrows_3d, 2L)
  testthat::expect_length(arrows_2d, 2L)
  testthat::expect_length(nodes_3d, 2L)
  testthat::expect_length(nodes_2d, 2L)
  testthat::expect_true(all(vapply(arrows_3d, function(trace) {
    identical(trace$type, "scatter3d") && identical(trace$mode, "lines") &&
      identical(trace$showlegend, FALSE) &&
      all(stats::na.omit(as.character(trace$hoverinfo)) == "skip") &&
      identical(trace$meta$segment_count, 2L)
  }, logical(1))))
  testthat::expect_true(all(vapply(arrows_2d, function(trace) {
    identical(trace$type, "scatter") && identical(trace$mode, "lines") &&
      identical(trace$showlegend, FALSE) &&
      all(stats::na.omit(as.character(trace$hoverinfo)) == "skip") &&
      identical(trace$meta$segment_count, 2L)
  }, logical(1))))

  path_colors <- stats::setNames(
    vapply(trajectory_path_traces(plot_3d), function(trace) trace$line$color, character(1)),
    vapply(trajectory_path_traces(plot_3d), function(trace) trace$meta$trajectory_key, character(1))
  )
  arrow_colors <- stats::setNames(
    vapply(arrows_3d, function(trace) trace$line$color, character(1)),
    vapply(arrows_3d, function(trace) trace$meta$trajectory_key, character(1))
  )
  testthat::expect_identical(arrow_colors[names(path_colors)], path_colors)
  testthat::expect_setequal(
    vapply(arrows_3d, `[[`, character(1), "legendgroup"),
    names(path_colors)
  )
  testthat::expect_identical(attr(plot_3d, "trajectory_show_direction"), TRUE)
  testthat::expect_true(all(vapply(nodes_3d, function(trace) {
    identical(trace$type, "scatter3d") && identical(trace$mode, "markers") &&
      identical(trace$showlegend, FALSE) &&
      identical(trace$hovertemplate[[1L]], "%{text}<extra></extra>")
  }, logical(1))))
  testthat::expect_true(all(vapply(nodes_2d, function(trace) {
    identical(trace$type, "scatter") && identical(trace$mode, "markers") &&
      identical(trace$showlegend, FALSE)
  }, logical(1))))

  built_roles <- vapply(plotly::plotly_build(plot_3d)$x$data, function(trace) {
    if (is.list(trace$meta)) trace$meta$trajectory_role else NA_character_
  }, character(1))
  testthat::expect_true(
    max(which(built_roles == "direction_arrows")) <
      min(which(built_roles == "node_markers"))
  )

  hidden <- plot_centroid_trajectory_3d(
    path,
    dimensions = c("D1", "D2", "D3"),
    group_cols = "condition",
    show_direction = FALSE
  )
  testthat::expect_length(trajectory_direction_traces(hidden), 0L)
  testthat::expect_length(trajectory_node_marker_traces(hidden), 0L)
  testthat::expect_identical(attr(hidden, "trajectory_show_direction"), FALSE)
})

testthat::test_that("direction geometry points forward and never bridges gaps", {
  straight <- data.frame(
    x = c(0, 1), y = c(0, 0), z = c(0, 0), stringsAsFactors = FALSE
  )
  geometry <- .trajectory_direction_geometry(
    straight, scale_data = straight, view = "3d"
  )
  testthat::expect_identical(geometry$segment_count, 1L)
  wing_starts <- seq.int(1L, length(geometry$x), by = 3L)
  wing_tips <- wing_starts + 1L
  testthat::expect_length(wing_starts, 2L)
  testthat::expect_true(all(geometry$x[wing_tips] > geometry$x[wing_starts]))
  testthat::expect_equal(geometry$x[wing_tips], rep(1, 2L), tolerance = 1e-12)
  testthat::expect_equal(geometry$y[wing_tips], c(0, 0), tolerance = 1e-12)
  testthat::expect_equal(geometry$z[wing_tips], c(0, 0), tolerance = 1e-12)
  testthat::expect_equal(
    geometry$x[wing_tips] - geometry$x[wing_starts],
    rep(0.0224, 2L),
    tolerance = 1e-12
  )
  first_wing <- c(
    geometry$x[wing_tips[1L]] - geometry$x[wing_starts[1L]],
    geometry$y[wing_tips[1L]] - geometry$y[wing_starts[1L]],
    geometry$z[wing_tips[1L]] - geometry$z[wing_starts[1L]]
  )
  testthat::expect_equal(
    sqrt(sum(first_wing[-1L]^2)),
    0.0224 * 0.45,
    tolerance = 1e-12
  )

  short <- data.frame(
    x = c(0, 0.05), y = c(0, 0), z = c(0, 0), stringsAsFactors = FALSE
  )
  short_geometry <- .trajectory_direction_geometry(
    short, scale_data = straight, view = "3d"
  )
  short_starts <- seq.int(1L, length(short_geometry$x), by = 3L)
  short_tips <- short_starts + 1L
  testthat::expect_equal(
    short_geometry$x[short_tips] - short_geometry$x[short_starts],
    rep(0.05 * 0.224, 2L),
    tolerance = 1e-12
  )
  testthat::expect_equal(formals(.trajectory_direction_geometry)$arrow_size, 0.0224)
  testthat::expect_equal(formals(plot_centroid_trajectory)$arrow_size, 0.0224)

  side_view <- .trajectory_direction_geometry(
    straight,
    scale_data = straight,
    view = "3d",
    camera = list(eye = list(x = 0, y = 0, z = 2.5))
  )
  side_starts <- seq.int(1L, length(side_view$x), by = 3L)
  testthat::expect_length(side_starts, 2L)
  testthat::expect_true(diff(side_view$y[side_starts]) != 0)
  testthat::expect_equal(side_view$z[side_starts], c(0, 0), tolerance = 1e-12)

  broken <- data.frame(
    x = c(0, 0, NA, 1, 2),
    y = c(0, 0, NA, 1, 1),
    z = c(0, 0, NA, 1, 1),
    stringsAsFactors = FALSE
  )
  broken_geometry <- .trajectory_direction_geometry(
    broken, scale_data = broken, view = "3d"
  )
  # 1 -> 2 is zero length, 2 -> 3 and 3 -> 4 contain a missing value;
  # only the final observed 4 -> 5 segment receives an arrowhead.
  testthat::expect_identical(broken_geometry$segment_count, 1L)

  unsupported_order <- data.frame(
    x = 0:4,
    y = rep(0, 5),
    z = rep(0, 5),
    time_order = c(1, 2, 2, NA, 1),
    stringsAsFactors = FALSE
  )
  unsupported_geometry <- .trajectory_direction_geometry(
    unsupported_order, scale_data = unsupported_order, view = "3d"
  )
  testthat::expect_identical(unsupported_geometry$segment_count, 1L)

  z_only <- data.frame(
    x = c(0, 0), y = c(0, 0), z = c(0, 1), time_order = 1:2
  )
  testthat::expect_identical(
    .trajectory_direction_geometry(z_only, z_only, view = "3d")$segment_count,
    1L
  )
  testthat::expect_identical(
    .trajectory_direction_geometry(z_only, z_only, view = "2d")$segment_count,
    0L
  )
})

testthat::test_that("long paths batch all arrowheads into one trace per group", {
  testthat::skip_if_not_installed("plotly")
  count <- 101L
  long_path <- data.frame(
    time_value = seq_len(count),
    time_order = seq_len(count),
    centroid_D1 = seq(0, 1, length.out = count),
    centroid_D2 = sin(seq(0, 2 * pi, length.out = count)),
    centroid_D3 = cos(seq(0, 2 * pi, length.out = count)),
    stringsAsFactors = FALSE
  )
  attr(long_path, "trajectory_spec") <- list(
    dimensions = c("D1", "D2", "D3"), group_vars = character()
  )
  plot <- plot_centroid_trajectory_3d(
    long_path, dimensions = c("D1", "D2", "D3")
  )
  arrows <- trajectory_direction_traces(plot)
  testthat::expect_length(arrows, 1L)
  testthat::expect_identical(arrows[[1L]]$meta$segment_count, 100L)
})

testthat::test_that("direction controls reject invalid values", {
  testthat::skip_if_not_installed("plotly")
  path <- make_trajectory_plot_fixture()
  testthat::expect_error(
    plot_centroid_trajectory_3d(
      path, dimensions = c("D1", "D2", "D3"), show_direction = NA
    ),
    "show_direction",
    fixed = TRUE
  )
  testthat::expect_error(
    plot_centroid_trajectory_3d(
      path, dimensions = c("D1", "D2", "D3"), arrow_size = 0.25
    ),
    "arrow_size",
    fixed = TRUE
  )
})

testthat::test_that("2D, 3D, display scale, and camera preserve analytical values", {
  testthat::skip_if_not_installed("plotly")
  path <- make_trajectory_plot_fixture()
  plot_3d_a <- plot_centroid_trajectory_3d(
    path,
    dimensions = c("D1", "D2", "D3"),
    group_cols = "condition",
    display_scale = 1,
    camera = list(eye = list(x = 1, y = 1, z = 1))
  )
  plot_3d_b <- plot_centroid_trajectory_3d(
    path,
    dimensions = c("D1", "D2", "D3"),
    group_cols = "condition",
    display_scale = 2.5,
    camera = list(eye = list(x = -2, y = 0.5, z = 1.5))
  )
  plot_2d <- plot_centroid_trajectory_2d(
    path,
    dimensions = c("D1", "D2", "D3"),
    group_cols = "condition",
    display_scale = 3
  )

  testthat::expect_identical(attr(plot_3d_a, "trajectory_data"), path)
  testthat::expect_identical(attr(plot_3d_b, "trajectory_data"), path)
  testthat::expect_identical(attr(plot_2d, "trajectory_data"), path)

  traces_3d_a <- trajectory_path_traces(plot_3d_a)
  traces_3d_b <- trajectory_path_traces(plot_3d_b)
  traces_2d <- trajectory_path_traces(plot_2d)
  for (group in c("A", "B")) {
    trace_3d_a <- trajectory_trace_by_name(traces_3d_a, group)
    trace_3d_b <- trajectory_trace_by_name(traces_3d_b, group)
    trace_2d <- trajectory_trace_by_name(traces_2d, group)
    testthat::expect_equal(as.numeric(trace_3d_a$x), as.numeric(trace_3d_b$x))
    testthat::expect_equal(as.numeric(trace_3d_a$y), as.numeric(trace_3d_b$y))
    testthat::expect_equal(as.numeric(trace_3d_a$z), as.numeric(trace_3d_b$z))
    testthat::expect_equal(as.numeric(trace_3d_a$x), as.numeric(trace_2d$x))
    testthat::expect_equal(as.numeric(trace_3d_a$y), as.numeric(trace_2d$y))
  }

  export_3d <- attr(plot_3d_b, "trajectory_trace_data")
  export_2d <- attr(plot_2d, "trajectory_trace_data")
  testthat::expect_equal(export_3d$step_distance, export_2d$step_distance)
  testthat::expect_equal(export_3d$cumulative_distance, export_2d$cumulative_distance)
  testthat::expect_equal(export_3d$speed, export_2d$speed)
})

testthat::test_that("hover and visible annotations expose trajectory diagnostics", {
  testthat::skip_if_not_installed("plotly")
  path <- make_trajectory_plot_fixture()
  plot <- plot_centroid_trajectory_3d(
    path, dimensions = c("D1", "D2", "D3"), group_cols = "condition"
  )
  built <- plotly::plotly_build(plot)
  group_a <- trajectory_trace_by_name(trajectory_path_traces(plot), "A")
  hover <- paste(group_a$text, collapse = "\n")

  testthat::expect_match(hover, "Time: Week 2", fixed = TRUE)
  testthat::expect_match(hover, "Order: 2", fixed = TRUE)
  testthat::expect_match(hover, "n: 4 / 5", fixed = TRUE)
  testthat::expect_match(
    hover,
    "Distance space: selected 3D subspace: D1, D2, D3",
    fixed = TRUE
  )
  testthat::expect_match(hover, "Step distance:", fixed = TRUE)
  testthat::expect_match(hover, "Cumulative distance:", fixed = TRUE)
  testthat::expect_match(hover, "Speed:", fixed = TRUE)
  testthat::expect_match(hover, "Warnings:", fixed = TRUE)
  testthat::expect_match(hover, "Cohort composition changed", fixed = TRUE)
  testthat::expect_identical(unique(group_a$hovertemplate), "%{text}<extra></extra>")

  testthat::expect_true(length(built$x$layout$annotations) >= 1L)
  annotation <- paste(vapply(
    built$x$layout$annotations,
    function(item) item$text,
    character(1)
  ), collapse = "\n")
  testthat::expect_match(annotation, "Trajectory warning", fixed = TRUE)
  testthat::expect_match(annotation, "A period is missing", fixed = TRUE)
  testthat::expect_equal(
    lengths(regmatches(
      annotation,
      gregexpr("Cohort composition changed", annotation, fixed = TRUE)
    )),
    1L
  )
})

testthat::test_that("hover reports finite bootstrap replicates for each available metric", {
  testthat::skip_if_not_installed("plotly")
  path <- make_trajectory_plot_fixture()
  path$centroid_D1_boot_n <- c(91L, 82L, 93L, 84L, 85L, 96L)
  path$step_distance_lower <- path$step_distance - 0.25
  path$step_distance_upper <- path$step_distance + 0.25
  path$step_distance_boot_n <- c(71L, 62L, 73L, 64L, 65L, 76L)
  attr(path, "bootstrap_spec") <- list(n_boot = 100L)

  plot <- plot_centroid_trajectory_3d(
    path, dimensions = c("D1", "D2", "D3"), group_cols = "condition"
  )
  group_a <- trajectory_trace_by_name(trajectory_path_traces(plot), "A")
  hover <- paste(group_a$text, collapse = "\n")

  testthat::expect_match(
    hover, "D1 bootstrap replicates: 82 / 100", fixed = TRUE
  )
  testthat::expect_match(
    hover, "Step distance bootstrap replicates: 62 / 100", fixed = TRUE
  )
  testthat::expect_false(grepl(
    "D2 bootstrap replicates", hover, fixed = TRUE
  ))
  testthat::expect_identical(
    attr(attr(plot, "trajectory_trace_data"), "bootstrap_spec"),
    list(n_boot = 100L)
  )
})

testthat::test_that("invalid interval bounds are omitted from hover and error bars", {
  data <- data.frame(
    coordinate = c(1, 2, 3, 4, 5),
    coordinate_lower = c(0.5, NA, 3.5, 4.1, -Inf),
    coordinate_upper = c(1.5, 2.5, 3.4, 4.5, 5.5)
  )

  testthat::expect_identical(
    .trajectory_interval_text(data, 1L, "coordinate"), " [0.5, 1.5]"
  )
  testthat::expect_identical(
    vapply(2:5, function(row) {
      .trajectory_interval_text(data, row, "coordinate")
    }, character(1)),
    c("", "", " [4.1, 4.5]", "")
  )

  error <- .trajectory_error_bar(data, "coordinate", "#123456")
  testthat::expect_equal(error$array[1L], 0.5)
  testthat::expect_equal(error$arrayminus[1L], 0.5)
  testthat::expect_true(all(is.na(error$array[-1L])))
  testthat::expect_true(all(is.na(error$arrayminus[-1L])))

  no_valid <- data
  no_valid$coordinate_lower <- c(1.1, NA, 3.5, 4.1, -Inf)
  testthat::expect_null(
    .trajectory_error_bar(no_valid, "coordinate", "#123456")
  )
})

testthat::test_that("POSIX overlay filtering uses the epoch suffix exactly", {
  path <- make_trajectory_plot_fixture()
  attr(path, "trajectory_spec")$time_var <- "timestamp"
  instants <- as.POSIXct(
    c(1730611800, 1730615400), origin = "1970-01-01", tz = "America/New_York"
  )
  testthat::expect_identical(
    format(instants, "%Y-%m-%d %H:%M", tz = "America/New_York"),
    rep("2024-11-03 01:30", 2L)
  )
  overlay <- data.frame(
    id = c("EDT instant", "EST instant"),
    timestamp = instants,
    stringsAsFactors = FALSE
  )
  selected <- paste0(
    "2024-11-03 01:30 [epoch=",
    format(as.numeric(instants[2L]), scientific = FALSE, trim = TRUE),
    "]"
  )

  filtered <- .trajectory_filter_selected_time(overlay, selected, path)
  testthat::expect_identical(filtered$id, "EST instant")
  filtered_list <- .trajectory_filter_selected_time(
    overlay, list(timestamp = selected), path
  )
  testthat::expect_identical(filtered_list$id, "EST instant")
})

testthat::test_that("selected-time network and code-node overlays are optional data hooks", {
  testthat::skip_if_not_installed("plotly")
  path <- make_trajectory_plot_fixture()
  nodes <- data.frame(
    time_value = c("Week 1", "Week 1", "Week 2", "Week 2"),
    label = c("old-a", "old-b", "new-a", "new-b"),
    x = c(0, 1, 5, 6),
    y = c(0, 1, 7, 8),
    z = c(0, 1, 9, 10),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    time_value = c("Week 1", "Week 2"),
    label = c("old edge", "selected edge"),
    x = c(0, 5), y = c(0, 7), z = c(0, 9),
    xend = c(1, 6), yend = c(1, 8), zend = c(1, 10),
    stringsAsFactors = FALSE
  )
  hook <- function(plot, context) {
    testthat::expect_identical(context$view, "3d")
    testthat::expect_identical(context$selected_time, "Week 2")
    plotly::add_trace(
      plot,
      type = "scatter3d",
      mode = "markers",
      x = 0, y = 0, z = 0,
      name = "Hook",
      showlegend = FALSE,
      meta = list(trajectory_role = "custom_hook")
    )
  }

  plot <- plot_centroid_trajectory_3d(
    path,
    dimensions = c("D1", "D2", "D3"),
    group_cols = "condition",
    code_nodes = nodes,
    network_edges = edges,
    selected_time = "Week 2",
    overlay_hooks = hook
  )
  traces <- plotly::plotly_build(plot)$x$data
  roles <- vapply(traces, function(trace) {
    if (is.list(trace$meta) && !is.null(trace$meta$trajectory_role)) {
      trace$meta$trajectory_role
    } else {
      ""
    }
  }, character(1))

  testthat::expect_equal(sum(roles == "path"), 2L)
  testthat::expect_equal(sum(roles == "network"), 1L)
  testthat::expect_equal(sum(roles == "code_nodes"), 1L)
  testthat::expect_equal(sum(roles == "custom_hook"), 1L)
  node_trace <- traces[[which(roles == "code_nodes")]]
  network_trace <- traces[[which(roles == "network")]]
  testthat::expect_identical(as.character(node_trace$text), c("new-a", "new-b"))
  testthat::expect_true(all(grepl(
    "selected edge", stats::na.omit(network_trace$text), fixed = TRUE
  )))
  testthat::expect_equal(as.numeric(stats::na.omit(network_trace$x)), c(5, 6))
})


testthat::test_that("network overlays retain bounded width/sign styles and expose weights", {
  testthat::skip_if_not_installed("plotly")
  path <- make_trajectory_plot_fixture()
  edges <- data.frame(
    time_value = rep("Week 2", 4),
    label = paste("edge", 1:4),
    x = 1:4,
    y = 11:14,
    z = 21:24,
    xend = 2:5,
    yend = 12:15,
    zend = 22:25,
    weight = c(0.1, 0.5, -0.2, -0.9),
    width = c(0.5, 2, 1, 4),
    sign = c("positive", "positive", "negative", "negative"),
    color = c("#aa0000", "#aa0000", "#0000aa", "#0000aa"),
    stringsAsFactors = FALSE
  )

  plot <- plot_centroid_trajectory_3d(
    path,
    dimensions = c("D1", "D2", "D3"),
    group_cols = "condition",
    network_edges = edges,
    selected_time = "Week 2"
  )
  traces <- plotly::plotly_build(plot)$x$data
  network <- Filter(function(trace) {
    is.list(trace$meta) && identical(trace$meta$trajectory_role, "network")
  }, traces)

  testthat::expect_length(network, 4L)
  displayed_widths <- sort(vapply(network, function(trace) {
    as.numeric(trace$line$width)
  }, numeric(1)))
  testthat::expect_equal(displayed_widths, sort(edges$width))
  testthat::expect_setequal(
    vapply(network, function(trace) trace$meta$edge_sign, character(1)),
    c("positive", "negative")
  )
  hover <- paste(unlist(lapply(network, `[[`, "text")), collapse = "\n")
  testthat::expect_match(hover, "Weight: 0.1", fixed = TRUE)
  testthat::expect_match(hover, "Weight: -0.9", fixed = TRUE)
  testthat::expect_match(hover, "Width input: 4", fixed = TRUE)
  testthat::expect_match(hover, "Displayed width bin: 4", fixed = TRUE)

  many_bins <- .trajectory_edge_width_bins(seq_len(50), max_bins = 6L)
  testthat::expect_lte(length(unique(many_bins)), 6L)
})


testthat::test_that("trajectory plots expose readable chart typography", {
  testthat::skip_if_not_installed("plotly")
  path <- make_trajectory_plot_fixture()
  plot <- plot_centroid_trajectory_3d(
    path,
    dimensions = c("D1", "D2", "D3"),
    group_cols = "condition"
  )
  layout <- plotly::plotly_build(plot)$x$layout

  testthat::expect_gte(layout$font$size, 14)
  testthat::expect_gte(layout$legend$font$size, 14)
  testthat::expect_gte(layout$legend$title$font$size, 14)
  testthat::expect_gte(layout$scene$xaxis$tickfont$size, 14)
  testthat::expect_gte(layout$scene$xaxis$title$font$size, 16)
})
