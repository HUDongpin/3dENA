plot_settings_ui <- function(id) {
  # This ns <- NS structure creates a
  # "namespacing" function, that will
  # prefix all ids with a string
  ns <- NS(id)
  tagList(
        selectInput(ns("x"), "X axis",choices=list()),
        selectInput(ns("y"), "Y axis",choices=list()),
        selectInput(ns("z"), "Z axis",choices=list()),
        hr(),
        sliderInput(ns("scale_factor"), "Scale Factor", value = 1, min = 1, max = 10),
        sliderInput(ns("line_width"), "Edge Width Factor", value = 3, min = 1, max = 10),
        hr(),
        checkboxInput(ns("show_grid"), "Show Grid", value = TRUE),
        checkboxInput(ns("show_zeroline"), "Show Zero Lines", value = TRUE),
        hr(),
        checkboxInput(ns("show_x_axis_arrow"), "Show X-axis Arrow", value = TRUE),
        checkboxInput(ns("show_y_axis_arrow"), "Show Y-axis Arrow", value = TRUE),
        checkboxInput(ns("show_z_axis_arrow"), "Show Z-axis Arrow", value = TRUE),

      )
}
