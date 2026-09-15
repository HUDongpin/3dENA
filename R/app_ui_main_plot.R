ena3d_plot_slot_ui <- function(slot_id, tab_value, ns, ...) {
  shiny::conditionalPanel(
    condition = sprintf("input.mytabs === '%s'", tab_value),
    ns = ns,
    tags$div(
      class = "ena3d-plot-slot",
      id = slot_id,
      ...
    )
  )
}


plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tagAppendAttributes(
      shiny::textOutput(ns("plot_mode_label")),
      class = "ena3d-plot-mode-label",
      role = "status",
      `aria-live` = "polite"
    ),
    ena3d_plot_slot_ui(
      ns("ena_points_plot_slot"),
      "comparison_plot",
      ns,
      plotlyOutput(ns("ena_points_plot"), height = "90vh")
    ),
    ena3d_plot_slot_ui(
      ns("ena_unit_group_change_plot_slot"),
      "group_change",
      ns,
      plotlyOutput(ns("ena_unit_group_change_plot"), height = "90vh")
    ),
    ena3d_plot_slot_ui(
      ns("ena_overall_plot_slot"),
      "overall_model",
      ns,
      plotlyOutput(ns("ena_overall_plot"), height = "90vh")
    ),
    ena3d_plot_slot_ui(
      ns("ena_network_plot_slot"),
      "network",
      ns,
      plotlyOutput(ns("ena_network_plot"), height = "90vh")
    ),
    ena3d_plot_slot_ui(
      ns("ena_trajectory_panel"),
      "trajectory",
      ns,
      trajectory_plot_ui(ns("trajectory"), height = "90vh")
    )
  )
}
