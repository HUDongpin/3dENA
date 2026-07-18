
plot_ui <- function(id) {
  # This ns <- NS structure creates a
  # "namespacing" function, that will
  # prefix all ids with a string
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("ena_points_plot"), height = "90vh"),
    plotlyOutput(ns("ena_unit_group_change_plot"), height = "90vh"),
    plotlyOutput(ns("ena_overall_plot"), height = "90vh"),
    plotlyOutput(ns("ena_network_plot"), height = "90vh"),
    tags$div(
      id = ns("ena_trajectory_panel"),
      trajectory_plot_ui(ns("trajectory"), height = "90vh")
    )
  )
}
