model_ui <- function(id) {
  # This ns <- NS structure creates a
  # "namespacing" function, that will
  # prefix all ids with a string
  ns <- NS(id)
  tagList(

    tabsetPanel(
      id = ns("mytabs"),
      tabPanel("Overall",value = "overall_model", model_overall_model_ui(id)),
      tabPanel("Networks",value = "network", model_network_ui(id)),
      tabPanel("Comparison",value = "comparison_plot", model_two_group_comparison_ui(id)),
      tabPanel("Change",value = "group_change", model_group_change_ui(id)),
      tabPanel("Trajectory", value = "trajectory", trajectory_controls_ui(ns("trajectory"))),
      # tabPanel("Two Group",value = "two_group",model_two_group_change_ui(id))

    )
  )

}
model_two_group_change_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("change_group_1"), "Group 1",choices=list()),
    selectInput(ns("change_group_2"), "Group 2", choices=list()),
    sliderInput(ns("group_change"), "Group Change", value = 1, min = 1, max = 10)
  )
}
model_two_group_comparison_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("compare_group_1"), "Group 1",choices=list()),
    textInput(ns("comparison_group_1_color"), "Group 1 color", "#BF382A"),
    checkboxInput(ns("compare_group_1_show_mean"), "Show Mean", value = FALSE),
    checkboxInput(ns("compare_group_1_show_confidence_interval"), "Show Confidence Interval", value = FALSE),
    hr(),
    selectInput(ns("compare_group_2"), "Group 2", choices=list()),
    textInput(ns("comparison_group_2_color"), "Group 2 color", "#0C4B8E"),
    checkboxInput(ns("compare_group_2_show_mean"), "Show Mean", value = FALSE),
    checkboxInput(ns("compare_group_2_show_confidence_interval"), "Show Confidence Interval", value = FALSE),
  )
}

model_overall_model_ui <- function(id){
  ns <- NS(id)
  tagList(
    # actionButton(ns('g1'),'Group 1'),
    # actionButton(ns('g2'),'Group 1')
    # virtualSelectInput(
    #   inputId = "id",
    #   label = "Select:",
    #   choices = list(
    #     "Spring" = c("March", "April", "May"),
    #     "Summer" = c("June", "July", "August"),
    #     "Autumn" = c("September", "October", "November"),
    #     "Winter" = c("December", "January", "February")
    #   ),
    #   showValueAsTags = TRUE,
    #   search = TRUE,
    #   multiple = TRUE
    # ),
    
    uiOutput(ns('group_colors_container'))
  )
}

model_network_ui <- function(id){
  ns <- NS(id)
  tagList(
    # actionButton(ns('g1'),'Group 1'),
    # actionButton(ns('g2'),'Group 1')
    # virtualSelectInput(
    #   inputId = "id",
    #   label = "Select:",
    #   choices = list(
    #     "Spring" = c("March", "April", "May"),
    #     "Summer" = c("June", "July", "August"),
    #     "Autumn" = c("September", "October", "November"),
    #     "Winter" = c("December", "January", "February")
    #   ),
    #   showValueAsTags = TRUE,
    #   search = TRUE,
    #   multiple = TRUE
    # ),
    
    selectInput(
      inputId = ns("network_selector"),
      label = "Show Network", 
      choices = list(
        lower = c("a", "b", "c", "d"),
        upper = c("A", 
                  "B", "C", "D"))
    ),
    hr(),
    

    
    
    uiOutput(ns('network_groups_container')),
  )
}

model_group_change_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("group_change_var"), "Select Group Variable",choices=list()),
    # sliderInput(ns("main_group_change"), "Unit Change", value = 1, min = 1, max = 10)
    selectInput(inputId = ns("unit_change"),
                label = "Units",
                choices = c(1,5,10,15,20,25,30)),
    hr(),
    checkboxInput(ns("group_change_show_mean"), "Show Mean", value = TRUE),
    checkboxInput(ns("group_change_show_confidence_interval"), "Show Confidence Interval", value = TRUE),
  )
}

group_selector_ui <- function(button_id,
                              points_toggle_id='1',
                              color_selector_id='1',
                              show_mean_btn_id='1',
                              show_conf_int_btn_id='1',
                              group_name='1',
                              group_color='grey'){
  #ns <- NS(id)
  # button_id <- ns(paste0("button"))
  # points_toggle_id <- ns(paste0("points_toggle"))
  # color_selector_id <- ns(paste0("color_selector"))
  # show_mean_btn_id <- ns(paste0("show_mean_btn"))
  # show_conf_int_btn_id <- ns(paste0("show_conf_int_btn"))
  if(is.null(button_id)){
    return(tagList())
  }
  wellPanel(
    tags$strong(group_name),
    textInput(color_selector_id, "Color", value = group_color, width = "100%"),
    checkboxInput(points_toggle_id, "Show points", value = TRUE),
    checkboxInput(show_mean_btn_id, "Show mean", value = TRUE),
    checkboxInput(show_conf_int_btn_id, "Show confidence interval", value = TRUE)
  )
}

