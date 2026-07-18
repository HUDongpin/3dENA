# UI for centroid trajectory analysis.
#
# The controls and plot are exposed separately as well as through trajectory_ui().
# This lets the existing application place the controls in its Model sidebar and
# the Plotly output in its main plot area without duplicating input/output IDs.

trajectory_controls_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$div(
      class = "trajectory-analysis-controls",
      shiny::tags$h4("Centroid trajectory"),
      shiny::selectInput(
        ns("time_var"),
        "Time / order variable",
        choices = character(0)
      ),
      shiny::selectInput(
        ns("id_var"),
        "Entity ID (repeated unit)",
        choices = character(0)
      ),
      shiny::tags$small(
        class = "trajectory-id-coverage",
        shiny::textOutput(ns("id_coverage_status"), inline = TRUE)
      ),
      shiny::selectInput(
        ns("group_var"),
        "Group / condition (optional)",
        choices = c("None" = "")
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] !== ''", ns("group_var")),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::selectInput(
              ns("condition_a"),
              "Compare level A",
              choices = character(0)
            )
          ),
          shiny::column(
            6,
            shiny::selectInput(
              ns("condition_b"),
              "Compare level B",
              choices = character(0)
            )
          )
        ),
        shiny::checkboxInput(
          ns("run_comparison"),
          "Compute an exact paired A/B trajectory comparison",
          value = FALSE
        ),
        shiny::helpText(
          "The paired comparison matches the same entity IDs within each time ",
          "period and uses the bootstrap settings below."
        ),
        shiny::tags$small(
          class = "trajectory-comparison-overlap",
          shiny::textOutput(ns("comparison_overlap_status"), inline = TRUE)
        ),
        shiny::checkboxInput(
          ns("confirm_paired_ids"),
          paste0(
            "I confirm that the same raw ID in A and B is the same physical ",
            "entity (not a group-local reused number)"
          ),
          value = FALSE
        )
      ),
      shiny::textAreaInput(
        ns("time_order"),
        "Ordered time values",
        value = "",
        rows = 3,
        placeholder = "One value per line; a comma-separated legacy line is also accepted"
      ),
      shiny::actionButton(
        ns("generate_order"),
        "Generate default order",
        icon = shiny::icon("arrow-down-short-wide")
      ),
      shiny::helpText(
        "Review the generated order, especially for labeled character values. ",
        "Generated values use one line each so labels may contain commas. ",
        "Every observed time value must appear exactly once. You may also add ",
        "expected periods with no observations so gaps remain explicit."
      ),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::selectInput(
            ns("cohort_policy"),
            "Cohort policy",
            choices = c(
              "Available at each time" = "available",
              "Complete cohort across time" = "complete"
            ),
            selected = "available"
          )
        ),
        shiny::column(
          6,
          shiny::selectInput(
            ns("na_policy"),
            "Missing-value policy",
            choices = c(
              "Use complete analytical rows" = "complete",
              "Stop on missing values" = "error"
            ),
            selected = "complete"
          )
        )
      ),
      shiny::radioButtons(
        ns("distance_space"),
        "Distance calculation space",
        choices = c(
          "Selected ENA axes" = "selected",
          "Full ENA rotation" = "full"
        ),
        selected = "selected",
        inline = TRUE
      ),
      shiny::radioButtons(
        ns("view"),
        "View",
        choices = c("3D" = "3d", "2D projection" = "2d"),
        selected = "3d",
        inline = TRUE
      ),
      shiny::checkboxInput(
        ns("show_direction"),
        "Show direction arrows on path segments",
        value = TRUE
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] === '2d'", ns("view")),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::selectInput(
              ns("axis_x"),
              "2D horizontal axis",
              choices = character(0)
            )
          ),
          shiny::column(
            6,
            shiny::selectInput(
              ns("axis_y"),
              "2D vertical axis",
              choices = character(0)
            )
          )
        )
      ),
      shiny::checkboxInput(
        ns("show_uncertainty"),
        "Estimate and show participant-clustered bootstrap uncertainty",
        value = FALSE
      ),
      shiny::conditionalPanel(
        condition = sprintf(
          "input['%s'] === true || input['%s'] === true",
          ns("show_uncertainty"), ns("run_comparison")
        ),
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::numericInput(
              ns("bootstrap_reps"),
              "Bootstrap reps",
              value = 500L,
              min = 200L,
              max = 500L,
              step = 50L
            )
          ),
          shiny::column(
            4,
            shiny::numericInput(
              ns("confidence"),
              "Confidence",
              value = 0.95,
              min = 0.5,
              max = 0.98,
              step = 0.01
            )
          ),
          shiny::column(
            4,
            shiny::numericInput(
              ns("bootstrap_seed"),
              "Seed",
              value = 2026L,
              min = 0L,
              step = 1L
            )
          )
        ),
        shiny::selectInput(
          ns("bootstrap_design"),
          "Participant resampling design",
          choices = c(
            "Auto: infer from ID overlap" = "auto",
            "Global cluster: same raw ID is one entity across groups" = "cluster",
            "Group-stratified: IDs are group-local / groups independent" = "stratified"
          ),
          selected = "auto"
        ),
        shiny::helpText(
          "Auto uses global clusters when eligible raw IDs overlap between groups; ",
          "otherwise it preserves each group's sample size. Choose explicitly ",
          "when the study's ID namespace is known. At least 80% of replicates and ",
          "five expected replicates per confidence-interval tail are required."
        ),
        shiny::helpText(
          "The hosted application defaults to 500 repetitions and accepts ",
          "200–500 per run; full-rotation bootstraps can take ",
          "substantially longer."
        ),
        shiny::tags$small(
          class = "trajectory-bootstrap-cost",
          shiny::textOutput(ns("bootstrap_cost_status"), inline = TRUE)
        )
      ),
      shiny::checkboxInput(
        ns("network_overlay"),
        "Overlay the mean ENA network at one selected time (overall by default)",
        value = FALSE
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] === true", ns("network_overlay")),
        shiny::selectInput(
          ns("selected_time"),
          "Network time",
          choices = character(0)
        ),
        shiny::selectInput(
          ns("overlay_group"),
          "Network scope",
          choices = c("Overall across all trajectory groups" = "")
        ),
        shiny::tags$small(shiny::textOutput(ns("overlay_status"), inline = TRUE))
      ),
      shiny::tags$div(
        class = "trajectory-plot-tools-scope alert alert-info",
        role = "note",
        shiny::tags$strong("Plot Tools scope"),
        shiny::tags$p(
          style = "margin-bottom: 0;",
          "X/Y/Z axes and Camera Position apply here. Scale Factor, Edge Width ",
          "Factor, grid, zero-line, and axis-arrow controls apply only to the ",
          "legacy model views. Trajectory coordinates are intentionally never ",
          "rescaled by display controls."
        )
      ),
      shiny::tags$hr(),
      shiny::actionButton(
        ns("run_trajectory"),
        "Run / recompute trajectory",
        icon = shiny::icon("play"),
        class = "btn-primary"
      ),
      shiny::uiOutput(ns("downloads"))
    )
  )
}


trajectory_plot_ui <- function(id, height = "72vh") {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$div(
      class = "trajectory-analysis-status",
      shiny::tags$strong(shiny::textOutput(ns("status"), inline = TRUE)),
      shiny::uiOutput(ns("warnings"))
    ),
    shiny::tags$div(
      class = "trajectory-plot-layout",
      shiny::tags$div(
        class = "trajectory-plot-canvas",
        plotly::plotlyOutput(ns("trajectory_plot"), height = height)
      ),
      htmltools::tagAppendAttributes(
        shiny::uiOutput(ns("node_legend"), container = shiny::tags$aside),
        class = "trajectory-node-legend-slot",
        role = "region",
        `aria-label` = "Trajectory node color key",
        tabindex = "0"
      )
    )
  )
}


trajectory_ui <- function(id, plot_height = "72vh",
                          controls_width = 4L, plot_width = 8L) {
  controls_width <- as.integer(controls_width)
  plot_width <- as.integer(plot_width)

  if (is.na(controls_width) || is.na(plot_width) ||
      controls_width < 1L || plot_width < 1L ||
      controls_width + plot_width > 12L) {
    stop("controls_width and plot_width must be positive and sum to at most 12.")
  }

  shiny::fluidRow(
    shiny::column(controls_width, trajectory_controls_ui(id)),
    shiny::column(plot_width, trajectory_plot_ui(id, height = plot_height))
  )
}


# Backward-friendly aliases for applications that name modules after their files.
app_ui_trajectory <- trajectory_ui
ena_trajectory_ui <- trajectory_ui
