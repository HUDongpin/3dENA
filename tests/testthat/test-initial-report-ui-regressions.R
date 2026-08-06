library(testthat)

.ui_regression_roots <- c(".", "../..", "..")
.ui_regression_root <- .ui_regression_roots[file.exists(
  file.path(.ui_regression_roots, "R", "app_module_overall_model.R")
)][1L]
if (is.na(.ui_regression_root)) stop("Could not locate the 3D ENA project root.")
.ui_regression_root <- normalizePath(.ui_regression_root)

.ui_regression_old_wd <- getwd()
tryCatch(
  {
    setwd(file.path(.ui_regression_root, "R"))
    source("app_module_overall_model.R", local = FALSE)
    source("app_module_network.R", local = FALSE)
    source("app_module_ena_unit_group_change_plot.R", local = FALSE)
    source("app_module_ena_comparison_plot.R", local = FALSE)
  },
  finally = setwd(.ui_regression_old_wd)
)


.load_ui_newfrat_fixture <- function() {
  fixture <- new.env(parent = emptyenv())
  load(
    file.path(.ui_regression_root, "sample_data", "newfrat_enaset.Rdata"),
    envir = fixture
  )
  objects <- mget(ls(fixture, all.names = TRUE), envir = fixture)
  matches <- Filter(function(value) {
    !is.null(value$points) && is.data.frame(value$points)
  }, objects)
  expect_length(matches, 1L)
  matches[[1L]]
}


test_that("Overall uses named data.table columns and aligned secondary hover values", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("plotly")

  ena <- .load_ui_newfrat_fixture()
  points <- data.table::as.data.table(ena$points)
  original <- data.table::copy(points)
  selected_weeks <- c(2L, 10L)
  selected_rows <- which(points[["Week"]] %in% selected_weeks)

  prepared <- ena3d_prepare_overall_points(
    points,
    group_var = "Week",
    selected_groups = selected_weeks,
    hover_var = "Name"
  )

  expect_s3_class(prepared, "data.frame")
  expect_false(data.table::is.data.table(prepared))
  expect_equal(nrow(prepared), length(selected_rows))
  expect_identical(prepared$Week, as.character(points$Week[selected_rows]))
  expect_identical(
    prepared$.ena3d_group_hover,
    as.character(points$Name[selected_rows])
  )
  expect_identical(points, original)
  expect_equal(ena3d_overall_group_count(points, "Week"), 15L)

  all_weeks <- sort(unique(points$Week))
  all_points <- ena3d_prepare_overall_points(
    points, "Week", all_weeks, "Name"
  )
  colors <- grDevices::hcl.colors(length(all_weeks), "Dark 3")
  plot <- ena3d_add_overall_points_trace(
    plotly::plot_ly(),
    points = all_points,
    group_var = "Week",
    dimensions = c("SVD1", "SVD2", "SVD3"),
    colors = colors,
    hover_label = "Name"
  )
  traces <- plotly::plotly_build(plot)$x$data

  expect_length(traces, 15L)
  expect_setequal(
    vapply(traces, function(trace) as.character(trace$name), character(1)),
    as.character(all_weeks)
  )
  trace_colors <- vapply(
    traces, function(trace) as.character(trace$marker$color[[1L]]),
    character(1)
  )
  expect_length(unique(trace_colors), 15L)

  for (trace in traces) {
    week <- as.character(trace$name)
    expected_names <- as.character(points$Name[as.character(points$Week) == week])
    expect_identical(as.character(trace$text), expected_names)
    expect_true(all(grepl("Name: %{text}", trace$hovertemplate, fixed = TRUE)))
  }

  full_color_map <- stats::setNames(colors, as.character(all_weeks))
  selected_points <- ena3d_prepare_overall_points(
    points, "Week", selected_weeks, "Name"
  )
  selected_plot <- ena3d_add_overall_points_trace(
    plotly::plot_ly(), selected_points, "Week",
    c("SVD1", "SVD2", "SVD3"), full_color_map, "Name"
  )
  selected_traces <- plotly::plotly_build(selected_plot)$x$data
  expect_length(selected_traces, 2L)
  for (trace in selected_traces) {
    week <- as.character(trace$name)
    expect_identical(
      as.character(trace$marker$color[[1L]]),
      unname(full_color_map[[week]])
    )
  }
})


test_that("Overall keeps adjacent numeric groups independently selectable", {
  adjacent <- c(1, 1 + .Machine$double.eps)
  points <- data.frame(
    group = rep(adjacent, each = 2L),
    unit = c("a", "b", "c", "d"),
    SVD1 = c(0, 2, 10, 12),
    stringsAsFactors = FALSE
  )
  labels <- ena3d_group_value_labels(points$group)

  expect_identical(
    unique(sprintf("%a", points$group)),
    c("0x1p+0", "0x1.0000000000001p+0")
  )
  expect_length(unique(labels), 2L)

  first <- ena3d_prepare_overall_points(
    points, "group", labels[[1L]], "unit"
  )
  second <- ena3d_prepare_overall_points(
    points, "group", labels[[3L]], "unit"
  )

  expect_identical(first$unit, c("a", "b"))
  expect_identical(second$unit, c("c", "d"))
  expect_identical(mean(first$SVD1), 1)
  expect_identical(mean(second$SVD1), 11)
  expect_identical(intersect(first$unit, second$unit), character())

  repeated <- ena3d_group_value_labels(c(1, 1))
  expect_identical(repeated[[1L]], repeated[[2L]])
})


test_that("Overall honors configured group colors and fills missing groups", {
  configured <- cbind(
    color = c("#123456", "#abcdef"),
    group = c("A", "B")
  )
  colors <- ena3d_overall_color_map(
    c("B", "A", "C"),
    configured_colors = configured,
    fallback_colors = "#fedcba"
  )

  expect_identical(unname(colors[c("B", "A")]), c("#abcdef", "#123456"))
  expect_identical(unname(colors[["C"]]), "#fedcba")
})


test_that("legacy point filters accept POSIXct groups selected by their UI labels", {
  fold <- as.POSIXct(
    c("2025-11-02 01:30:00 -0400", "2025-11-02 01:30:00 -0500"),
    format = "%Y-%m-%d %H:%M:%S %z",
    tz = "America/New_York"
  )
  timestamps <- rep(fold, each = 2L)
  points <- data.frame(
    observed_at = timestamps,
    unit = c("a", "b", "c", "d"),
    MR1 = c(1, 2, 3, 4),
    check.names = FALSE
  )
  labels <- ena3d_group_value_labels(timestamps)
  selected <- labels[[1L]]

  overall <- ena3d_prepare_overall_points(
    points, "observed_at", selected, "unit"
  )
  comparison <- get_points_with_group(points, "observed_at", selected)

  expect_identical(overall$unit, c("a", "b"))
  expect_identical(comparison$unit, c("a", "b"))
  expect_identical(unique(overall$observed_at), selected)

  colors <- cbind(color = c("#123456", "#654321"), group = unique(labels))
  expect_identical(get_group_color(colors, "group", unique(labels)[[2L]]), "#654321")
})


test_that("Network point traces retain dimensions from exchange data frames", {
  skip_if_not_installed("plotly")

  fold <- as.POSIXct(
    c("2025-11-02 01:30:00 -0400", "2025-11-02 01:30:00 -0500"),
    format = "%Y-%m-%d %H:%M:%S %z",
    tz = "America/New_York"
  )
  points <- data.frame(
    ENA_UNIT = structure(c("u1", "u2"),
                         class = c("ena.metadata", "character")),
    observed_at = structure(fold,
                            class = c("ena.metadata", "POSIXct", "POSIXt")),
    MR1 = structure(c(1, 2), class = c("ena.dimension", "numeric")),
    SVD2 = structure(c(3, 4), class = c("ena.dimension", "numeric")),
    SVD3 = structure(c(5, 6), class = c("ena.dimension", "numeric")),
    check.names = FALSE
  )
  labels <- ena3d_group_value_labels(points$observed_at)
  selected <- ena3d_network_group_points(
    points, "observed_at", labels[[2L]]
  )

  expect_identical(names(selected), names(points))
  expect_identical(as.character(selected$ENA_UNIT), "u2")
  expect_s3_class(selected$MR1, "ena.dimension")

  plot <- plotly::add_trace(
    plotly::plot_ly(), data = selected,
    x = tilde_var_or_null("MR1"),
    y = tilde_var_or_null("SVD2"),
    z = tilde_var_or_null("SVD3"),
    text = ~ENA_UNIT,
    type = "scatter3d", mode = "markers"
  )
  built <- plotly::plotly_build(plot)
  expect_identical(as.numeric(built$x$data[[1L]]$x), 2)
  expect_identical(as.character(built$x$data[[1L]]$text), "u2")
})


test_that("axis changes swap collisions and always remain three-dimensional", {
  dimensions <- c("MR1", "SVD2", "SVD3")
  previous <- c(x = "MR1", y = "SVD2", z = "SVD3")

  expect_identical(
    ena3d_normalize_axis_selection(dimensions),
    previous
  )
  expect_identical(
    ena3d_normalize_axis_selection(dimensions, "SVD2"),
    c(x = "SVD2", y = "MR1", z = "SVD3")
  )
  expect_true(ena3d_axes_are_distinct(previous))
  expect_false(ena3d_axes_are_distinct(c("MR1", "MR1", "SVD3")))
  expect_identical(
    ena3d_resolve_axis_change(
      dimensions, previous,
      current = c(x = "SVD2", y = "SVD2", z = "SVD3"),
      changed = "x"
    ),
    c(x = "SVD2", y = "MR1", z = "SVD3")
  )
  expect_identical(
    ena3d_resolve_axis_change(
      dimensions, previous,
      current = c(x = "MR1", y = "SVD3", z = "SVD3"),
      changed = "y"
    ),
    c(x = "MR1", y = "SVD3", z = "SVD2")
  )

  four_dimensions <- c(dimensions, "SVD4")
  expect_identical(
    ena3d_resolve_axis_change(
      four_dimensions, previous,
      current = c(x = "SVD4", y = "SVD2", z = "SVD3"),
      changed = "x"
    ),
    c(x = "SVD4", y = "SVD2", z = "SVD3")
  )
})


test_that("plot formulas preserve accepted quoting characters exactly", {
  dimensions <- c(
    paste0("dimension", "`", "one"),
    paste0("dimension", "\\", "two"),
    "dimension with spaces"
  )
  formulas <- lapply(dimensions, tilde_var_or_null)

  expect_identical(
    vapply(formulas, function(formula) all.vars(formula)[[1L]], character(1L)),
    dimensions
  )
  expect_true(all(vapply(formulas, inherits, logical(1L), "formula")))
})


test_that("Network, Change, and Comparison plots explicitly autorange", {
  skip_if_not_installed("plotly")

  helpers <- list(
    network = ena3d_network_axis_layout,
    change = ena3d_change_axis_layout,
    comparison = ena3d_comparison_axis_layout
  )
  for (helper in helpers) {
    axis <- helper("SVD1", showgrid = TRUE, zeroline = FALSE)
    expect_true(isTRUE(axis$autorange))
    expect_false("range" %in% names(axis))
    expect_identical(axis$title$text, "SVD1")
    expect_gte(axis$title$font$size, 16)
    expect_gte(axis$tickfont$size, 14)
    expect_true(isTRUE(axis$showgrid))
    expect_false(isTRUE(axis$zeroline))
  }

  far_coordinates <- data.frame(
    x = c(-250, 400), y = c(-125, 300), z = c(-500, 625)
  )
  axis <- ena3d_network_axis_layout("x")
  plot <- plotly::plot_ly(
    far_coordinates, x = ~x, y = ~y, z = ~z,
    type = "scatter3d", mode = "markers"
  )
  plot <- plotly::layout(
    plot,
    scene = list(xaxis = axis, yaxis = axis, zaxis = axis)
  )
  built <- plotly::plotly_build(plot)
  expect_true(isTRUE(built$x$layout$scene$xaxis$autorange))
  expect_null(built$x$layout$scene$xaxis$range)
  expect_equal(range(as.numeric(built$x$data[[1L]]$x)), c(-250, 400))

  themed <- ena3d_apply_plotly_typography(plotly::plot_ly(
    far_coordinates, x = ~x, y = ~y, z = ~z,
    type = "scatter3d", mode = "markers"
  ))
  themed_layout <- plotly::plotly_build(themed)$x$layout
  expect_gte(themed_layout$font$size, 14)
  expect_gte(themed_layout$legend$font$size, 14)

  compatibility_plot <- set_default_axis_range(plotly::plot_ly(
    far_coordinates, x = ~x, y = ~y, z = ~z,
    type = "scatter3d", mode = "markers"
  ))
  compatibility_layout <- plotly::plotly_build(compatibility_plot)$x$layout$scene
  expect_true(isTRUE(compatibility_layout$xaxis$autorange))
  expect_null(compatibility_layout$xaxis$range)

  files <- c(
    "app_module_network.R",
    "app_module_ena_unit_group_change_plot.R",
    "app_module_ena_comparison_plot.R"
  )
  text <- paste(vapply(files, function(file) {
    paste(readLines(file.path(.ui_regression_root, "R", file), warn = FALSE),
          collapse = "\n")
  }, character(1)), collapse = "\n")
  expect_false(grepl("set_default_axis_range(", text, fixed = TRUE))
  expect_false(grepl("range = c(-", text, fixed = TRUE))
})


test_that("Network selector readiness is computed without a reactive write loop", {
  selectors <- list(
    first = list(
      color_selector_id = "color_1",
      points_toggle_id = "points_1",
      show_mean_btn_id = "mean_1",
      show_conf_int_btn_id = "ci_1"
    ),
    second = list(
      color_selector_id = "color_2",
      points_toggle_id = "points_2",
      show_mean_btn_id = "mean_2",
      show_conf_int_btn_id = "ci_2"
    )
  )
  complete_inputs <- list(
    color_1 = "#000000", points_1 = TRUE, mean_1 = TRUE, ci_1 = FALSE,
    color_2 = "#ffffff", points_2 = TRUE, mean_2 = FALSE, ci_2 = TRUE
  )

  expect_true(ena3d_group_selectors_ready(complete_inputs, selectors))
  expect_false(ena3d_group_selectors_ready(complete_inputs[-1L], selectors))
  expect_false(ena3d_group_selectors_ready(complete_inputs, list()))

  network_text <- paste(
    readLines(
      file.path(.ui_regression_root, "R", "app_module_network.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_false(grepl(
    "data$group_selectors[[i]][['ready']] <- TRUE",
    network_text,
    fixed = TRUE
  ))
})


test_that("group color controls use one dynamic observer without nested leaks", {
  server_text <- paste(
    readLines(file.path(.ui_regression_root, "R", "app_server.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(server_text, "One dynamic observer owns all color inputs", fixed = TRUE)
  expect_false(grepl(
    "observeEvent(eventExpr = input[[color_selector_id]]",
    server_text,
    fixed = TRUE
  ))
  expect_match(server_text, "color_values <- lapply(selectors", fixed = TRUE)
})


test_that("camera presets do not overwrite contextual plot titles", {
  files <- c(
    "app_module_overall_model.R", "app_module_network.R",
    "app_module_ena_comparison_plot.R",
    "app_module_ena_unit_group_change_plot.R"
  )
  text <- paste(vapply(files, function(file) {
    paste(
      readLines(file.path(.ui_regression_root, "R", file), warn = FALSE),
      collapse = "\n"
    )
  }, character(1)), collapse = "\n")
  expect_false(grepl("title=input$camera_position", text, fixed = TRUE))
  expect_false(grepl("title = input$camera_position", text, fixed = TRUE))
  expect_match(text, "Overall ENA model", fixed = TRUE)
  expect_match(text, "uirevision", fixed = TRUE)
})


test_that("one fullscreen control targets the visible Plotly widget", {
  app_text <- paste(
    readLines(file.path(.ui_regression_root, "R", "app.R"), warn = FALSE),
    collapse = "\n"
  )
  plot_ui_text <- paste(
    readLines(
      file.path(.ui_regression_root, "R", "app_ui_main_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  button_pattern <- paste0(
    "actionButton\\s*\\(\\s*NS\\s*\\(\\s*\"main_app\"\\s*,\\s*",
    "'fullscreen_btn'\\s*\\)"
  )
  button_matches <- gregexpr(button_pattern, app_text, perl = TRUE)[[1L]]
  expect_equal(sum(button_matches > 0L), 1L)
  expect_match(app_text, "main_app-fullscreen_btn", fixed = TRUE)
  expect_match(app_text, "document.addEventListener('fullscreenchange'", fixed = TRUE)
  expect_match(app_text, "window.dispatchEvent(new Event('resize'))", fixed = TRUE)
  expect_match(
    app_text,
    ".plot-container .plotly.html-widget",
    fixed = TRUE
  )
  expect_match(app_text, "requestFullscreen", fixed = TRUE)
  expect_match(app_text, "enterFallbackFullscreen", fixed = TRUE)
  expect_match(app_text, "ena3d-fullscreen-fallback", fixed = TRUE)
  expect_match(app_text, "Exit fullscreen plot", fixed = TRUE)
  expect_match(app_text, "getBoundingClientRect", fixed = TRUE)
  expect_false(grepl("fullscreen_this", plot_ui_text, fixed = TRUE))
  expect_false(grepl("fullscreen_btn_", plot_ui_text, fixed = TRUE))
})


test_that("sidebar toggle uses supported DOM text APIs and guarded elements", {
  app_text <- paste(
    readLines(file.path(.ui_regression_root, "R", "app.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_false(grepl("getInnerHTML", app_text, fixed = TRUE))
  expect_false(grepl("setHTML", app_text, fixed = TRUE))
  expect_match(app_text, "toggleButton.textContent", fixed = TRUE)
  expect_match(
    app_text,
    "sidebar && toggleButton && plotContainer && mainLayout",
    fixed = TRUE
  )
  expect_match(app_text, "window.dispatchEvent(new Event('resize'))", fixed = TRUE)
})


test_that("sidebar toggle stages its motion before hiding controls", {
  app_text <- paste(
    readLines(file.path(.ui_regression_root, "R", "app.R"), warn = FALSE),
    collapse = "\n"
  )
  shell_css <- paste(
    readLines(
      file.path(.ui_regression_root, "R", "www", "app_shell.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  style_text <- paste(app_text, shell_css, sep = "\n")

  css_blocks <- regmatches(
    style_text,
    gregexpr("[^{}]+\\{[^{}]*\\}", style_text, perl = TRUE)
  )[[1L]]
  transition_block_for <- function(selector) {
    blocks <- css_blocks[
      grepl(selector, css_blocks, fixed = TRUE) &
        grepl("transition", css_blocks, fixed = TRUE)
    ]
    expect_gte(length(blocks), 1L)
    if (!length(blocks)) return(NA_character_)
    paste(blocks, collapse = "\n")
  }

  sidebar_transition <- transition_block_for(".ena3d-sidebar-column")
  plot_transition <- transition_block_for(".plot-container")
  details_transition <- transition_block_for(".mysidebar .right-side")

  expect_match(style_text, "220ms", fixed = TRUE)
  expect_match(style_text, "ease-out", fixed = TRUE)
  if (!is.na(sidebar_transition)) {
    expect_match(sidebar_transition, "width", fixed = TRUE)
  }
  if (!is.na(plot_transition)) {
    expect_match(plot_transition, "width", fixed = TRUE)
  }
  if (!is.na(details_transition)) {
    expect_match(details_transition, "opacity", fixed = TRUE)
    expect_match(details_transition, "transform", fixed = TRUE)
  }

  control_start <- regexpr(
    "const setSidebarControlState",
    app_text,
    fixed = TRUE
  )[[1L]]
  finalize_start <- regexpr(
    "const finalizeSidebarTransition",
    app_text,
    fixed = TRUE
  )[[1L]]
  begin_start <- regexpr(
    "const beginSidebarTransition",
    app_text,
    fixed = TRUE
  )[[1L]]
  listener_start <- regexpr(
    "if (sidebarColumn)",
    substr(app_text, begin_start, nchar(app_text)),
    fixed = TRUE
  )[[1L]]
  expect_gt(control_start, 0L)
  expect_gt(finalize_start, control_start)
  expect_gt(begin_start, finalize_start)
  expect_gt(listener_start, 0L)

  if (control_start > 0L && finalize_start > control_start) {
    control_handler <- substr(app_text, control_start, finalize_start - 1L)
    expect_match(control_handler, "aria-expanded", fixed = TRUE)
    expect_match(control_handler, "aria-label", fixed = TRUE)
  }
  if (begin_start > 0L && listener_start > 0L) {
    begin_end <- begin_start + listener_start - 2L
    begin_handler <- substr(app_text, begin_start, begin_end)

    expect_match(begin_handler, "setSidebarControlState", fixed = TRUE)
    expect_match(begin_handler, "is-transitioning", fixed = TRUE)
    expect_match(begin_handler, "requestAnimationFrame", fixed = TRUE)
    expect_false(grepl("classList.toggle('hide'", begin_handler, fixed = TRUE))
    expect_false(grepl("classList.add('hide'", begin_handler, fixed = TRUE))
    expect_false(grepl("window.dispatchEvent(new Event('resize'))", begin_handler,
      fixed = TRUE
    ))
  }

  click_start <- regexpr(
    "toggleButton.addEventListener('click'",
    app_text,
    fixed = TRUE
  )[[1L]]
  expect_gt(click_start, begin_start)
  if (click_start > 0L) {
    click_handler <- substr(app_text, click_start, click_start + 400L)
    expect_match(click_handler, "beginSidebarTransition", fixed = TRUE)
  }
})


test_that("sidebar toggle finalizes on transition end with reduced-motion fallback", {
  app_text <- paste(
    readLines(file.path(.ui_regression_root, "R", "app.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(app_text, "transitionend", fixed = TRUE)
  expect_match(app_text, "propertyName", fixed = TRUE)
  expect_match(app_text, "prefers-reduced-motion: reduce", fixed = TRUE)

  finalize_start <- regexpr(
    "const finalizeSidebarTransition",
    app_text,
    fixed = TRUE
  )[[1L]]
  transition_start <- regexpr(
    "const beginSidebarTransition",
    app_text,
    fixed = TRUE
  )[[1L]]
  expect_gt(finalize_start, 0L)
  expect_gt(transition_start, finalize_start)
  if (finalize_start > 0L && transition_start > finalize_start) {
    finalize_handler <- substr(app_text, finalize_start, transition_start - 1L)

    expect_match(finalize_handler, "classList.toggle('hide'", fixed = TRUE)
    expect_match(
      finalize_handler,
      "classList.remove('is-transitioning'",
      fixed = TRUE
    )
    expect_match(finalize_handler, "dispatchFinalPlotResize", fixed = TRUE)
  }

  dispatch_start <- regexpr(
    "const dispatchFinalPlotResize",
    app_text,
    fixed = TRUE
  )[[1L]]
  control_start <- regexpr(
    "const setSidebarControlState",
    app_text,
    fixed = TRUE
  )[[1L]]
  expect_gt(dispatch_start, 0L)
  expect_gt(control_start, dispatch_start)
  if (dispatch_start > 0L && control_start > dispatch_start) {
    dispatch_handler <- substr(app_text, dispatch_start, control_start - 1L)
    expect_match(
      dispatch_handler,
      "window.dispatchEvent(new Event('resize'))",
      fixed = TRUE
    )
    expect_match(dispatch_handler, "requestAnimationFrame", fixed = TRUE)
  }

  transition_end_start <- regexpr(
    "addEventListener('transitionend'",
    app_text,
    fixed = TRUE
  )[[1L]]
  expect_gt(transition_end_start, transition_start)
  if (transition_end_start > 0L && transition_end_start > transition_start) {
    transition_end_handler <- substr(
      app_text,
      transition_end_start,
      transition_end_start + 900L
    )
    expect_match(
      transition_end_handler,
      "finalizeSidebarTransition",
      fixed = TRUE
    )
    expect_match(transition_end_handler, "event.target", fixed = TRUE)
    expect_match(transition_end_handler, "event.propertyName", fixed = TRUE)
  }

  listener_start <- regexpr(
    "if (sidebarColumn)",
    substr(app_text, transition_start, nchar(app_text)),
    fixed = TRUE
  )[[1L]]
  expect_gt(listener_start, 0L)
  if (transition_start > 0L && listener_start > 0L) {
    transition_handler <- substr(
      app_text,
      transition_start,
      transition_start + listener_start - 2L
    )
    expect_match(transition_handler, "!motionPreference.matches", fixed = TRUE)
    expect_match(transition_handler, "if (!shouldAnimate)", fixed = TRUE)
    expect_match(transition_handler, "finalizeSidebarTransition", fixed = TRUE)
    expect_match(transition_handler, "SIDEBAR_MOTION_FALLBACK_MS", fixed = TRUE)
    expect_match(transition_handler, "transitionGeneration", fixed = TRUE)
  }

  expect_match(
    app_text,
    "@media (max-width: 991.98px), (prefers-reduced-motion: reduce)",
    fixed = TRUE
  )
  expect_match(app_text, "transition:none !important", fixed = TRUE)
})
