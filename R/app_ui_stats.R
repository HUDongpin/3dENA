stats_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "stats-panel",
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "app_ui_stats.css")
    ),
    selectInput(
      ns("stats_design"),
      "Study design",
      choices = c(
        "Select the design before inference" = "",
        "Independent groups (between participants)" = "between",
        "Repeated/paired groups (within participant)" = "within"
      ),
      selected = ""
    ),
    selectInput(
      ns("stats_p_adjust_method"),
      "Multiple-testing adjustment",
      choices = c(
        "Holm (recommended)" = "holm",
        "Benjamini-Hochberg FDR" = "BH",
        "Bonferroni" = "bonferroni",
        "None (raw p-values)" = "none"
      ),
      selected = "holm"
    ),
    helpText(paste(
      "Only tests compatible with the selected design are calculated.",
      "Adjusted p-values cover all displayed axes/tests in that family."
    )),
    div(class = "stats-status", role = "status", textOutput(ns("stats_design_status"))),
    fluidRow(
      column(6, selectInput(ns("stats_group1"), "Group 1", choices = c())),
      column(6, selectInput(ns("stats_group2"), "Group 2", choices = c()))
    ),
    selectInput(ns("stats_pair_id"), "Pairing ID for paired tests", choices = c()),
    selectInput(
      ns("stats_paired_alternative"),
      "Paired Wilcoxon alternative hypothesis",
      choices = c(
        "Two-sided: Group 1 differs from Group 2" = "two.sided",
        "Greater: Group 1 is greater than Group 2" = "greater",
        "Less: Group 1 is less than Group 2" = "less"
      ),
      selected = "two.sided"
    ),
    helpText(paste(
      "For a repeated design, results are computed only after matching both",
      "groups by this ID. Independent tests are disabled."
    )),
    div(class = "stats-status", role = "status", textOutput(ns("stats_pair_status"))),
    tabsetPanel(
      tabPanel(
        "Welch t (independent)",
        stats_box(ns("stats_box_x_axis"), "X-axis"), hr(),
        stats_box(ns("stats_box_y_axis"), "Y-axis"), hr(),
        stats_box(ns("stats_box_z_axis"), "Z-axis")
      ),
      tabPanel(
        "Rank-sum (independent)",
        stats_box(ns("stats_box_x_axis_wilcox_unpaired"), "X-axis"), hr(),
        stats_box(ns("stats_box_y_axis_wilcox_unpaired"), "Y-axis"), hr(),
        stats_box(ns("stats_box_z_axis_wilcox_unpaired"), "Z-axis")
      ),
      tabPanel(
        "Signed-rank (paired)",
        stats_box(ns("stats_box_x_axis_wilcox_paired"), "X-axis"), hr(),
        stats_box(ns("stats_box_y_axis_wilcox_paired"), "Y-axis"), hr(),
        stats_box(ns("stats_box_z_axis_wilcox_paired"), "Z-axis")
      )
    )
  )
}

stats_box <- function(id, axis) {
  ns <- NS(id)
  div(
    class = "stats-box",
    fluidRow(
      column(6, h5(paste0(axis, ":"))),
      column(6, textOutput(ns("axis_name")))
    ) %>% tagAppendAttributes(class = "stats-box-row"),
    fluidRow(
      tableOutput(ns("data_table"))
    ) %>% tagAppendAttributes(class = "stats-box-data-table"),
    fluidRow(
      column(6, "Effect size (Group 1 - Group 2):"),
      column(6, textOutput(ns("effect_size")))
    ) %>% tagAppendAttributes(class = "stats-box-row"),
    fluidRow(
      column(6, "Raw p-value:"),
      column(6, textOutput(ns("p_value")))
    ) %>% tagAppendAttributes(class = "stats-box-row"),
    fluidRow(
      column(6, textOutput(ns("p_adjust_method"))),
      column(6, textOutput(ns("p_adjusted")))
    ) %>% tagAppendAttributes(class = "stats-box-row"),
    fluidRow(
      column(6, textOutput(ns("test_type"))),
      column(6, textOutput(ns("test_type_value")))
    ) %>% tagAppendAttributes(class = "stats-box-row"),
    fluidRow(
      column(6, textOutput(ns("conf_level"))),
      column(6, textOutput(ns("conf")))
    ) %>% tagAppendAttributes(class = "stats-box-row"),
    div(
      class = "stats-box-status",
      role = "status",
      textOutput(ns("test_status"))
    )
  )
}
