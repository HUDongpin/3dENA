library(testthat)
library(shiny)

.loader_test_root <- c(".", "../..", "..")
.loader_test_root <- .loader_test_root[file.exists(
  file.path(.loader_test_root, "R", "app_module_load_dataset.R")
)][1L]
if (is.na(.loader_test_root)) stop("Could not locate the project R directory.")

source(file.path(.loader_test_root, "R", "app_utils.R"), local = FALSE)
source(file.path(.loader_test_root, "R", "app_module_load_dataset.R"), local = FALSE)

# load_ena_data() creates this UI through the normal app_ui_model_tab.R source.
# A minimal test double keeps this file focused on loading/state behavior.
if (!exists("group_selector_ui", mode = "function")) {
  group_selector_ui <- function(...) shiny::div()
}

.loader_fixture_path <- function(name = "sample_enaset.Rdata") {
  file.path(.loader_test_root, "sample_data", name)
}

.clone_ena <- function(value) unserialize(serialize(value, NULL))

.save_rdata <- function(values) {
  path <- tempfile(fileext = ".RData")
  environment <- list2env(values, parent = emptyenv())
  save(list = names(values), file = path, envir = environment)
  path
}

.read_trusted_rdata <- function(path, limits = ena3d_data_limits()) {
  ena3d_read_ena_object(path, source_kind = "bundled", limits = limits)
}


test_that("loader isolates .RData objects and selects exactly one ena.set", {
  ena <- .read_trusted_rdata(.loader_fixture_path())
  path <- .save_rdata(list(ena = ena, notes = "unrelated object"))
  on.exit(unlink(path), add = TRUE)

  loaded <- .read_trusted_rdata(path)
  expect_s3_class(loaded, "ena.set")
  expect_equal(nrow(loaded$points), 4L)

  no_ena <- .save_rdata(list(first = 1, second = data.frame(x = 1)))
  on.exit(unlink(no_ena), add = TRUE)
  expect_error(.read_trusted_rdata(no_ena), "found 0 .*none")

  two_ena <- .save_rdata(list(first = ena, second = .clone_ena(ena)))
  on.exit(unlink(two_ena), add = TRUE)
  expect_error(.read_trusted_rdata(two_ena), "found 2 .*first, second")
})


test_that("loader rejects malformed ENA schemas with actionable messages", {
  ena <- .read_trusted_rdata(.loader_fixture_path())

  missing_field <- .clone_ena(ena)
  missing_field$line.weights <- NULL
  path <- .save_rdata(list(bad = missing_field))
  on.exit(unlink(path), add = TRUE)
  expect_error(.read_trusted_rdata(path), "required fields are missing: line.weights")

  row_mismatch <- .clone_ena(ena)
  row_mismatch$meta.data <- as.data.frame(row_mismatch$meta.data)[-1L, , drop = FALSE]
  path <- .save_rdata(list(bad = row_mismatch))
  on.exit(unlink(path), add = TRUE)
  expect_error(.read_trusted_rdata(path), "same number of rows")

  unknown_node <- .clone_ena(ena)
  adjacency <- as.data.frame(unknown_node$rotation$adjacency.key)
  adjacency[[1L]][[1L]] <- "NOT_A_NODE"
  unknown_node$rotation$adjacency.key <- adjacency
  path <- .save_rdata(list(bad = unknown_node))
  on.exit(unlink(path), add = TRUE)
  expect_error(.read_trusted_rdata(path), "Unknown codes: NOT_A_NODE")

  misaligned <- .clone_ena(ena)
  misaligned$line.weights <- as.data.frame(misaligned$line.weights)[
    rev(seq_len(nrow(misaligned$line.weights))), , drop = FALSE
  ]
  path <- .save_rdata(list(bad = misaligned))
  on.exit(unlink(path), add = TRUE)
  expect_error(
    .read_trusted_rdata(path),
    "metadata column `ENA_UNIT` must have identical type and row-aligned values"
  )

  missing_group_weights <- .clone_ena(ena)
  declared_group <- as.character(missing_group_weights$`_function.params`$groupVar[[1L]])
  missing_group_weights$line.weights[[declared_group]] <- NULL
  path <- .save_rdata(list(bad = missing_group_weights))
  on.exit(unlink(path), add = TRUE)
  expect_error(
    .read_trusted_rdata(path),
    "metadata columns are absent from `line.weights`"
  )

  blank_group <- .clone_ena(ena)
  blank_group$meta.data[[declared_group]][[1L]] <- ""
  blank_group$points[[declared_group]][[1L]] <- ""
  blank_group$line.weights[[declared_group]][[1L]] <- ""
  path <- .save_rdata(list(bad = blank_group))
  on.exit(unlink(path), add = TRUE)
  expect_error(
    .read_trusted_rdata(path),
    "grouping column.*contains missing or blank values"
  )

  misaligned_group <- .clone_ena(ena)
  misaligned_group$line.weights[[declared_group]] <-
    rev(misaligned_group$line.weights[[declared_group]])
  path <- .save_rdata(list(bad = misaligned_group))
  on.exit(unlink(path), add = TRUE)
  expect_error(
    .read_trusted_rdata(path),
    "metadata column.*must have identical type and row-aligned values"
  )

  duplicate_pair <- .clone_ena(ena)
  duplicate_pair$rotation$adjacency.key[, 2L] <-
    duplicate_pair$rotation$adjacency.key[, 1L]
  path <- .save_rdata(list(bad = duplicate_pair))
  on.exit(unlink(path), add = TRUE)
  expect_error(.read_trusted_rdata(path), "duplicate node pairs")

  reordered_edges <- .clone_ena(ena)
  edge_names <- setdiff(names(reordered_edges$line.weights),
                        names(reordered_edges$meta.data))
  reordered_names <- names(reordered_edges$line.weights)
  first_edge <- match(edge_names[[1L]], reordered_names)
  second_edge <- match(edge_names[[2L]], reordered_names)
  reordered_names[c(first_edge, second_edge)] <-
    reordered_names[c(second_edge, first_edge)]
  reordered_edges$line.weights <-
    reordered_edges$line.weights[, ..reordered_names]
  path <- .save_rdata(list(bad = reordered_edges))
  on.exit(unlink(path), add = TRUE)
  expect_error(.read_trusted_rdata(path), "adjacency-key order")
})


test_that("all bundled samples satisfy the ENA 3D schema", {
  fixtures <- list.files(
    file.path(.loader_test_root, "sample_data"),
    pattern = "\\.[Rr][Dd]ata$",
    full.names = TRUE
  )
  expect_length(fixtures, 3L)
  for (fixture in fixtures) {
    expect_s3_class(.read_trusted_rdata(fixture), "ena.set")
  }
})


test_that("loader preserves POSIXct primary groups and exposes stable labels", {
  ena <- .clone_ena(.read_trusted_rdata(.loader_fixture_path()))
  fold <- as.POSIXct(
    c("2025-11-02 01:30:00 -0400", "2025-11-02 01:30:00 -0500"),
    format = "%Y-%m-%d %H:%M:%S %z",
    tz = "America/New_York"
  )
  expect_identical(as.character(fold[[1L]]), as.character(fold[[2L]]))
  timestamps <- rep(fold, length.out = nrow(ena$points))
  metadata_times <- timestamps
  class(metadata_times) <- c("ena.metadata", "POSIXct", "POSIXt")
  ena$meta.data$observed_at <- metadata_times
  ena$points$observed_at <- metadata_times
  ena$line.weights$observed_at <- metadata_times
  ena$`_function.params`$groupVar <- "observed_at"
  ena$`_function.params`$units.by <- "observed_at"
  ena$`_function.params`$groups <- unique(as.character(timestamps))
  ena$`_function.params`$unit.groups <- unique(as.character(timestamps))

  path <- .save_rdata(list(posix_group = ena))
  on.exit(unlink(path), add = TRUE)
  prepared <- ena3d_prepare_dataset(path, source_kind = "bundled")

  expect_s3_class(prepared$ena_obj$points$observed_at, "POSIXct")
  expect_identical(
    prepared$groups,
    unique(ena3d_group_value_labels(timestamps))
  )
  expect_length(prepared$groups, 2L)
  expect_match(prepared$groups[[1L]], "-0400", fixed = TRUE)
  expect_match(prepared$groups[[2L]], "-0500", fixed = TRUE)
  expect_false(anyDuplicated(names(
    ena3d_group_selector_metadata(prepared$groups)
  )) > 0L)
  expect_equal(
    nrow(get_points_with_group(
      prepared$ena_obj$points, "observed_at", prepared$groups[[1L]]
    )),
    sum(ena3d_group_value_match(timestamps, prepared$groups[[1L]]))
  )
})


test_that("network selector labels the empty-network option explicitly", {
  choices <- ena3d_network_choices(c("A", "B"), c("unit-1", "unit-2"))
  none_value <- ena3d_network_selector_encode("none")
  html <- htmltools::renderTags(shiny::selectInput(
    "network_selector", "Network", choices = choices,
    selected = none_value
  ))$html

  expect_identical(names(choices)[[1L]], "No Network")
  expect_match(
    html,
    paste0("<option value=\"", none_value,
           "\" selected>No Network</option>"),
    fixed = TRUE
  )
  expect_false(grepl(">Option<", html, fixed = TRUE))
})


test_that("network selector metadata is built before read-only UI rendering", {
  selectors <- ena3d_group_selector_metadata(c("Group A", "Group/A"))

  expect_identical(names(selectors), c("Group A", "Group/A"))
  expect_identical(selectors[["Group A"]][["group_name"]], "Group A")
  expect_false(identical(
    selectors[["Group A"]][["button_id"]],
    selectors[["Group/A"]][["button_id"]]
  ))

  loader_text <- paste(
    readLines(file.path(.loader_test_root, "R", "app_module_load_dataset.R"),
              warn = FALSE),
    collapse = "\n"
  )
  render_start <- regexpr(
    "output$network_groups_container <- renderUI({", loader_text, fixed = TRUE
  )[[1L]]
  render_end <- regexpr(
    "network_choices <- ena3d_network_choices", loader_text, fixed = TRUE
  )[[1L]]
  expect_gt(render_start, 0L)
  expect_gt(render_end, render_start)
  render_body <- substr(loader_text, render_start, render_end - 1L)
  expect_false(grepl("rv_data$group_selectors <-", render_body, fixed = TRUE))
  expect_false(grepl("rv_data$group_selectors[[", render_body, fixed = TRUE))
})


test_that("dataset reset clears every dataset-derived cache", {
  rv <- new.env(parent = emptyenv())
  state <- new.env(parent = emptyenv())
  rv$myList <- list(old = TRUE)
  rv$unit_group_change_plots <- list(stale_plot = "OLD")
  rv$current_unit_change_plot_camera <- list(stale = TRUE)
  rv$dataset_id <- "old-data"
  rv$ena_groups <- "old-group"
  rv$ena_groupVar <- "old-variable"
  rv$ena_points_plot_ready <- TRUE
  rv$initialized <- TRUE
  rv$model_tab_clicked <- TRUE
  rv$comparison_plot <- list(stale = TRUE)
  rv$reactiveFunctions <- list(stale = TRUE)
  rv$group_colors <- matrix("old", nrow = 1L)
  rv$group_selectors <- list(stale = TRUE)
  rv$group_options <- list(stale = TRUE)
  rv$active_dataset <- list(name = "old")
  state$ena_obj <- list(old = TRUE)
  state$is_app_initialized <- TRUE

  ena3d_reset_data_state(rv, state)

  expect_identical(rv$unit_group_change_plots, list())
  expect_identical(rv$myList, list())
  expect_identical(rv$current_unit_change_plot_camera, list())
  expect_null(rv$dataset_id)
  expect_false(rv$initialized)
  expect_identical(rv$comparison_plot, list())
  expect_identical(rv$reactiveFunctions, list())
  expect_identical(rv$group_selectors, list())
  expect_null(rv$active_dataset)
  expect_null(state$ena_obj)
  expect_false(state$is_app_initialized)
})


test_that("switching bundled samples commits one transaction and drops stale plots", {
  paths <- c(
    small = .loader_fixture_path("sample_enaset.Rdata"),
    longitudinal = .loader_fixture_path("newfrat_enaset.Rdata")
  )

  testServer(
    function(input, output, session) {
      rv <- reactiveValues(
        myList = list(), unit_group_change_plots = list(),
        current_unit_change_plot_camera = list(), ena_groups = character(),
        ena_groupVar = character(),
        ena_points_plot_ready = FALSE, initialized = FALSE,
        model_tab_clicked = FALSE, comparison_plot = list(),
        reactiveFunctions = list(), group_colors = list(),
        group_selectors = list(), group_options = list(), dataset_id = NULL
      )
      state <- new.env(parent = emptyenv())
      state$ena_obj <- NULL
      state$is_app_initialized <- FALSE
      session$userData$rv <- rv
      session$userData$state <- state

      observeEvent(input$fixture, {
        load_ena_data(
          input, output, session, paths[[input$fixture]], rv, state,
          source_kind = "bundled"
        )
      })
    },
    {
      session$setInputs(fixture = "small")
      session$flushReact()
      first_id <- session$userData$rv$dataset_id
      expect_equal(nrow(session$userData$state$ena_obj$points), 4L)
      expect_identical(session$userData$rv$ena_groupVar, "groupid")

      session$userData$rv$unit_group_change_plots <- list(stale_plot = "OLD")
      session$userData$rv$myList <- list(stale = "OLD")
      session$userData$rv$comparison_plot <- list(stale = "OLD")

      session$setInputs(fixture = "longitudinal")
      session$flushReact()

      expect_equal(nrow(session$userData$state$ena_obj$points), 255L)
      expect_false(identical(session$userData$rv$dataset_id, first_id))
      expect_match(session$userData$rv$dataset_id, "[0-9a-f]{64}$")
      expect_identical(
        session$userData$rv$ena_groupVar,
        c("Week", "Name")
      )
      expect_identical(session$userData$rv$unit_group_change_plots, list())
      expect_identical(session$userData$rv$myList, list())
      expect_identical(session$userData$rv$comparison_plot, list())
      active <- session$userData$rv$active_dataset
      expect_identical(active$name, "newfrat_enaset.Rdata")
      expect_identical(active$rows, 255L)
      expect_identical(active$nodes, 17L)
      expect_identical(active$group_variables, 2L)
      expect_identical(active$group_levels, 15L)
      expect_identical(active$dimensions, 136L)
      expect_match(active$sha256, "^[0-9a-f]{64}$")
      expect_false(grepl(.loader_test_root, active$name, fixed = TRUE))
      card_html <- htmltools::renderTags(
        ena3d_active_dataset_card(active)
      )$html
      expect_match(card_html, "Active dataset", fixed = TRUE)
      expect_match(card_html, active$sha256, fixed = TRUE)
    }
  )
})


test_that("a failed validation cannot replace live dataset state", {
  valid <- .read_trusted_rdata(.loader_fixture_path())
  malformed <- .clone_ena(valid)
  malformed$rotation$nodes <- NULL
  path <- .save_rdata(list(bad = malformed))
  on.exit(unlink(path), add = TRUE)

  rv <- new.env(parent = emptyenv())
  rv$dataset_id <- "known-good"
  rv$unit_group_change_plots <- list(good = "KEEP")
  state <- new.env(parent = emptyenv())
  state$ena_obj <- valid
  state$is_app_initialized <- TRUE

  expect_error(
    load_ena_data(
      NULL, NULL, NULL, path, rv, state, source_kind = "bundled"
    ),
    "rotation\\$nodes"
  )
  expect_identical(rv$dataset_id, "known-good")
  expect_identical(rv$unit_group_change_plots, list(good = "KEEP"))
  expect_identical(state$ena_obj, valid)
  expect_true(state$is_app_initialized)
})
