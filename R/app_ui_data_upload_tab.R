data_upload_ui <- function(id, sample_data_files = character()) {
  # This ns <- NS structure creates a
  # "namespacing" function, that will
  # prefix all ids with a string
  ns <- NS(id)
  tagList(
    tags$section(
      class = "raw-import-workflow",
      tags$h4(class = "h5", "Build ENA from raw Excel or CSV"),
      tags$p(
        class = "text-muted",
        "Upload coded rows, map participant, sequence, group, and code fields, ",
        "then build the ENA model in this browser session."
      ),
      fileInput(
        ns("raw_data_file"),
        "Raw coded data (.csv, .xlsx, or .xls; maximum 5 MB)",
        accept = c(".csv", ".xlsx", ".xls"),
        multiple = FALSE
      ),
      uiOutput(ns("raw_reader_options")),
      uiOutput(ns("raw_upload_status")),
      tableOutput(ns("raw_data_preview")),
      uiOutput(ns("raw_mapping_ui")),
      uiOutput(ns("raw_model_status"))
    ),
    tags$hr(),
    tags$section(
      class = "prepared-import-workflow",
      tags$h4(class = "h5", "Open prepared ENA data"),
    fileInput(
      ns("ena_exchange_file"),
      "Open an .ena3d.json exchange file (maximum 2 MB)",
      accept = ".ena3d.json",
      multiple = FALSE
    ),
    selectizeInput(
      ns("sample_data"),
      "Trusted sample dataset",
      choices = c(
        "Select a sample dataset" = "",
        stats::setNames(sample_data_files, sample_data_files)
      ),
      selected = "",
      options = list(dropdownParent = "body")
    )),
    uiOutput(ns("active_dataset_card"))
  )
}
