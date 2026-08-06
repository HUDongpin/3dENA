library(testthat)
library(shiny)

.model_control_roots <- c(".", "../..", "..")
.model_control_root <- .model_control_roots[file.exists(
  file.path(.model_control_roots, "R", "app_module_load_dataset.R")
)][1L]
if (is.na(.model_control_root)) {
  stop("Could not locate the 3D ENA project root.")
}
.model_control_root <- normalizePath(.model_control_root, mustWork = TRUE)

source(file.path(.model_control_root, "R", "app_utils.R"), local = FALSE)
source(
  file.path(.model_control_root, "R", "app_module_load_dataset.R"),
  local = FALSE
)
source(file.path(.model_control_root, "R", "app_ui_model_tab.R"), local = FALSE)

.model_control_old_wd <- getwd()
tryCatch(
  {
    setwd(file.path(.model_control_root, "R"))
    source("app_module_ena_comparison_plot.R", local = FALSE)
    source("app_module_ena_unit_group_change_plot.R", local = FALSE)
    source("app_module_overall_model.R", local = FALSE)
  },
  finally = setwd(.model_control_old_wd)
)


test_that("Comparison selection resolution never represents A vs A", {
  first_changed <- ena3d_comparison_selection(
    c("A", "B", "C"), "A", "A", prefer = "group_1"
  )
  second_changed <- ena3d_comparison_selection(
    c("A", "B", "C"), "A", "A", prefer = "group_2"
  )

  expect_true(first_changed$valid)
  expect_identical(
    c(first_changed$group_1, first_changed$group_2), c("A", "B")
  )
  expect_false(first_changed$group_2 %in% first_changed$choices_1)
  expect_false(first_changed$group_1 %in% first_changed$choices_2)

  expect_true(second_changed$valid)
  expect_identical(
    c(second_changed$group_1, second_changed$group_2), c("B", "A")
  )
  expect_false(second_changed$group_2 %in% second_changed$choices_1)
  expect_false(second_changed$group_1 %in% second_changed$choices_2)

  unavailable <- ena3d_comparison_selection("A", "A", "A")
  expect_false(unavailable$valid)
  expect_identical(unavailable$group_1, "A")
  expect_length(unavailable$group_2, 0L)
  expect_length(unavailable$choices_2, 0L)
  expect_match(unavailable$message, "at least two distinct groups", fixed = TRUE)
})


test_that("Comparison colors normalize safely and invalid text uses defaults", {
  expect_identical(
    ena3d_normalize_comparison_color(" red ", "#BF382A"),
    "#FF0000"
  )
  expect_identical(
    ena3d_normalize_comparison_color("#11223344", "#BF382A"),
    "#11223344"
  )
  expect_identical(
    ena3d_normalize_comparison_color("not-a-color", "#BF382A"),
    "#BF382A"
  )
  expect_identical(
    ena3d_normalize_comparison_color(NULL, "#0C4B8E"),
    "#0C4B8E"
  )
  expect_identical(
    ena3d_normalize_comparison_color(c("red", "blue"), "#0C4B8E"),
    "#0C4B8E"
  )
  expect_identical(
    ena3d_normalize_comparison_color("invalid", "also-invalid"),
    "#000000"
  )

  source_text <- paste(
    readLines(
      file.path(
        .model_control_root, "R", "app_module_ena_comparison_plot.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_false(grepl(
    "colors=input$comparison_group_", source_text, fixed = TRUE
  ))
  expect_match(source_text, "comparison_colors()[[\"group_1\"]]", fixed = TRUE)
  expect_match(source_text, "comparison_colors()[[\"group_2\"]]", fixed = TRUE)
})


test_that("Comparison observers auto-switch the opposite group", {
  testServer(
    function(input, output, session) {
      groups <- reactiveVal(c("A", "B", "C"))
      selection <- ena3d_comparison_selection_server(
        input, session, groups
      )
      session$userData$groups <- groups
      session$userData$selection <- selection
    },
    {
      session$setInputs(compare_group_1 = "A", compare_group_2 = "A")
      session$flushReact()
      resolved <- session$userData$selection()
      expect_true(resolved$valid)
      expect_false(identical(resolved$group_1, resolved$group_2))

      # Mock sessions do not apply updateSelectInput() messages back into
      # input. Mirror the browser's resolved values before simulating the next
      # user selection.
      session$setInputs(
        compare_group_1 = resolved$group_1,
        compare_group_2 = resolved$group_2
      )
      session$flushReact()
      baseline <- session$userData$selection()
      session$setInputs(compare_group_2 = baseline$group_1)
      session$flushReact()
      switched <- session$userData$selection()
      expect_true(switched$valid)
      expect_false(identical(switched$group_1, switched$group_2))
      expect_identical(switched$group_2, baseline$group_1)
      expect_false(switched$group_2 %in% switched$choices_1)
      expect_false(switched$group_1 %in% switched$choices_2)

      session$userData$groups("only")
      session$flushReact()
      unavailable <- session$userData$selection()
      expect_false(unavailable$valid)
      expect_length(unavailable$group_2, 0L)
    }
  )
})


test_that("Comparison observer initializes from a newly available dataset", {
  testServer(
    function(input, output, session) {
      groups <- reactiveVal(character())
      selection <- ena3d_comparison_selection_server(input, session, groups)
      session$userData$groups <- groups
      session$userData$selection <- selection
    },
    {
      session$flushReact()
      expect_false(session$userData$selection()$valid)

      session$userData$groups(c("A", "B"))
      session$flushReact()
      resolved <- session$userData$selection()
      expect_true(resolved$valid)
      expect_identical(resolved$group_1, "A")
      expect_identical(resolved$group_2, "B")
      expect_identical(resolved$message, "Comparing A vs B.")
    }
  )
})


test_that("Change selector state clears high-cardinality values", {
  ready <- ena3d_change_selector_state(c("pre", "post"), max_levels = 2L)
  expect_true(ready$enabled)
  expect_identical(ready$choices, c("pre", "post"))
  expect_identical(ready$selected, "pre")

  blocked <- ena3d_change_selector_state(seq_len(4L), max_levels = 3L)
  expect_false(blocked$enabled)
  expect_identical(blocked$reason, "high_cardinality")
  expect_length(blocked$choices, 0L)
  expect_length(blocked$selected, 0L)
  expect_match(blocked$message, "lower-cardinality", fixed = TRUE)

  empty <- ena3d_change_selector_state(character(), max_levels = 3L)
  expect_false(empty$enabled)
  expect_identical(empty$reason, "no_values")
  expect_length(empty$choices, 0L)

  switching <- ena3d_change_selector_state_for_points(
    data.frame(new_group = c("A", "B")),
    "old_group"
  )
  expect_false(switching$enabled)
  expect_identical(switching$reason, "variable_updating")
  expect_length(switching$choices, 0L)
  expect_match(switching$message, "updating", fixed = TRUE)

  recovered_during_switch <- ena3d_change_selector_state_for_points(
    data.frame(new_group = c("A", "B")),
    "old_group",
    fallback_group_var = "new_group"
  )
  expect_true(recovered_during_switch$enabled)
  expect_identical(recovered_during_switch$group_var, "new_group")
  expect_identical(recovered_during_switch$choices, c("A", "B"))
})


test_that("Change observer blocks high cardinality and recovers", {
  testServer(
    function(input, output, session) {
      captured <- new.env(parent = emptyenv())
      captured$messages <- list()
      original_send <- session$sendInputMessage
      session$sendInputMessage <- function(input_id, message) {
        captured$messages[[length(captured$messages) + 1L]] <- list(
          input_id = input_id, message = message
        )
        original_send(input_id, message)
      }
      points <- reactiveVal(data.frame(
        low = rep(c("pre", "post"), length.out = 101L),
        high = seq_len(101L),
        stringsAsFactors = FALSE
      ))
      dataset_id <- reactiveVal("dataset-a")
      initialized <- reactiveVal(TRUE)
      selection <- ena3d_change_selection_server(
        input, session, points, dataset_id, initialized
      )
      session$userData$selection <- selection
      session$userData$captured <- captured
    },
    {
      session$setInputs(group_change_var = "high", unit_change = "stale")
      session$flushReact()
      blocked <- session$userData$selection()
      expect_false(blocked$enabled)
      expect_identical(blocked$reason, "high_cardinality")
      expect_length(blocked$choices, 0L)
      expect_length(blocked$selected, 0L)
      unit_messages <- Filter(function(entry) {
        identical(entry$input_id, "unit_change")
      }, session$userData$captured$messages)
      blocked_message <- tail(unit_messages, 1L)[[1L]]$message
      expect_identical(as.character(blocked_message$options), "")
      expect_length(blocked_message$value, 0L)

      session$setInputs(group_change_var = "low")
      session$flushReact()
      recovered <- session$userData$selection()
      expect_true(recovered$enabled)
      expect_identical(recovered$choices, c("pre", "post"))
      expect_identical(recovered$selected, "pre")
      unit_messages <- Filter(function(entry) {
        identical(entry$input_id, "unit_change")
      }, session$userData$captured$messages)
      recovered_message <- tail(unit_messages, 1L)[[1L]]$message
      expect_match(as.character(recovered_message$options), ">pre</option>",
                   fixed = TRUE)
      expect_identical(recovered_message$value, "pre")
    }
  )
})


test_that("Change observer initializes from authoritative dataset state", {
  testServer(
    function(input, output, session) {
      points <- reactiveVal(data.frame(
        condition = c("A", "B"), stringsAsFactors = FALSE
      ))
      dataset_id <- reactiveVal(NULL)
      initialized <- reactiveVal(FALSE)
      default_group_var <- reactiveVal(character())
      selection <- ena3d_change_selection_server(
        input, session, points, dataset_id, initialized, default_group_var
      )
      session$userData$dataset_id <- dataset_id
      session$userData$initialized <- initialized
      session$userData$default_group_var <- default_group_var
      session$userData$selection <- selection
    },
    {
      session$setInputs(group_change_var = "stale-variable")
      session$userData$default_group_var("condition")
      session$userData$dataset_id("dataset-a")
      session$userData$initialized(TRUE)
      session$flushReact()

      resolved <- session$userData$selection()
      expect_true(resolved$enabled)
      expect_identical(resolved$group_var, "condition")
      expect_identical(resolved$choices, c("A", "B"))
      expect_identical(resolved$selected, "A")
    }
  )
})


test_that("Overall contextual layout survives an empty group selection", {
  skip_if_not_installed("plotly")
  camera <- list(eye = list(x = 0, y = 0, z = 2.5))
  plot <- ena3d_apply_overall_layout(
    plotly::plot_ly(
      x = 0, y = 0, z = 0, type = "scatter3d", mode = "markers"
    ),
    camera = camera,
    camera_position = "x_y"
  )
  layout <- plotly::plotly_build(plot)$x$layout

  expect_identical(as.character(layout$title), "Overall ENA model")
  expect_identical(layout$scene$camera$eye, camera$eye)
  expect_identical(layout$scene$uirevision, "overall-camera-x_y")

  source_text <- paste(
    readLines(
      file.path(.model_control_root, "R", "app_module_overall_model.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_false(grepl(
    "if(length(selected_groups) == 0)", source_text, fixed = TRUE
  ))
  expect_match(source_text, "ena3d_apply_overall_layout(", fixed = TRUE)
})


test_that("Model controls expose truthful and accessible empty states", {
  network_html <- htmltools::renderTags(model_network_ui("model"))$html
  change_html <- htmltools::renderTags(model_group_change_ui("model"))$html
  comparison_html <- htmltools::renderTags(
    model_two_group_comparison_ui("model")
  )$html

  expect_match(network_html, "Load a dataset first", fixed = TRUE)
  expect_false(grepl(">a</option>", network_html, fixed = TRUE))
  expect_false(grepl(">A</option>", network_html, fixed = TRUE))
  expect_match(change_html, "Selected value", fixed = TRUE)
  expect_match(change_html, "change_value_status", fixed = TRUE)
  expect_match(change_html, 'role="status"', fixed = TRUE)
  expect_match(change_html, 'aria-live="polite"', fixed = TRUE)
  expect_match(comparison_html, "comparison_status", fixed = TRUE)
  expect_match(comparison_html, 'role="status"', fixed = TRUE)

  loaded_choices <- ena3d_network_choices("A", "unit-1")
  expect_identical(names(loaded_choices)[[1L]], "No Network")
})
