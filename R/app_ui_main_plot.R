
ena3d_plot_slot_ui <- function(slot_id, ..., hidden = TRUE) {
  tags$div(
    class = if (isTRUE(hidden)) {
      "ena3d-plot-slot ena3d-plot-hidden"
    } else {
      "ena3d-plot-slot"
    },
    id = slot_id,
    `aria-hidden` = if (isTRUE(hidden)) "true" else "false",
    ...
  )
}


plot_ui <- function(id) {
  # This ns <- NS structure creates a
  # "namespacing" function, that will
  # prefix all ids with a string
  ns <- NS(id)
  tagList(
    tags$p(
      id = ns("plot_mode_label"),
      class = "ena3d-plot-mode-label",
      role = "status",
      `aria-live` = "polite",
      "Showing: Overall ENA model"
    ),
    ena3d_plot_slot_ui(
      ns("ena_points_plot_slot"),
      plotlyOutput(ns("ena_points_plot"), height = "90vh")
    ),
    ena3d_plot_slot_ui(
      ns("ena_unit_group_change_plot_slot"),
      plotlyOutput(ns("ena_unit_group_change_plot"), height = "90vh")
    ),
    ena3d_plot_slot_ui(
      ns("ena_overall_plot_slot"),
      plotlyOutput(ns("ena_overall_plot"), height = "90vh"),
      hidden = FALSE
    ),
    ena3d_plot_slot_ui(
      ns("ena_network_plot_slot"),
      plotlyOutput(ns("ena_network_plot"), height = "90vh")
    ),
    ena3d_plot_slot_ui(
      ns("ena_trajectory_panel"),
      trajectory_plot_ui(ns("trajectory"), height = "90vh")
    )
  )
}
