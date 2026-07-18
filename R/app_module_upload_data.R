upload_data <- function(input, output, session, rv_data, state, config = list()) {
  limits <- if (!is.null(config$data_limits)) {
    config$data_limits
  } else {
    ena3d_data_limits()
  }
  raw_upload <- reactiveVal(NULL)
  raw_stage <- reactiveVal(NULL)
  raw_upload_status <- reactiveVal(NULL)
  raw_model_status <- reactiveVal(NULL)

  status_ui <- function(status) {
    if (is.null(status)) return(NULL)
    type <- status$type
    css <- switch(
      type,
      success = "alert alert-success",
      error = "alert alert-danger",
      warning = "alert alert-warning",
      "alert alert-info"
    )
    tags$div(
      class = css,
      role = if (identical(type, "error")) "alert" else "status",
      `aria-live` = "polite",
      tags$strong(status$title),
      if (!is.null(status$detail) && nzchar(status$detail)) {
        tags$p(style = "margin: 0.35rem 0 0;", status$detail)
      }
    )
  }

  stage_raw_table <- function(resolved, sheet = NULL) {
    table <- ena3d_read_raw_table(
      resolved$path,
      client_name = resolved$name,
      sheet = sheet,
      limits = limits
    )
    defaults <- ena3d_suggest_raw_mapping(table$data, limits = limits)
    staged <- c(table, list(
      source = resolved,
      defaults = defaults
    ))
    raw_stage(staged)
    raw_model_status(NULL)
    raw_upload_status(list(
      type = "success",
      title = sprintf("Loaded %s", resolved$name),
      detail = sprintf(
        "%s rows × %s columns%s. Review every suggested field before building.",
        format(table$rows, big.mark = ","),
        format(table$columns, big.mark = ","),
        if (!is.na(table$sheet)) paste0(" from worksheet “", table$sheet, "”") else ""
      )
    ))
    invisible(staged)
  }

  output$raw_reader_options <- renderUI({
    upload <- raw_upload()
    if (is.null(upload) || !upload$resolved$extension %in% c("xlsx", "xls")) {
      return(NULL)
    }
    current <- raw_stage()
    selected <- if (!is.null(current) && !is.na(current$sheet)) {
      current$sheet
    } else {
      upload$sheets[[1L]]
    }
    selectInput(
      session$ns("raw_excel_sheet"),
      "Worksheet",
      choices = upload$sheets,
      selected = selected
    )
  })

  output$raw_upload_status <- renderUI(status_ui(raw_upload_status()))
  output$raw_model_status <- renderUI(status_ui(raw_model_status()))

  output$raw_data_preview <- renderTable({
    staged <- raw_stage()
    req(staged)
    utils::head(staged$data, 6L)
  }, rownames = TRUE, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$raw_mapping_ui <- renderUI({
    staged <- raw_stage()
    req(staged)
    choices <- staged$column_names
    defaults <- staged$defaults
    tagList(
      tags$h5("Map fields"),
      tags$p(
        class = "text-muted",
        "Unit identifiers are combined into one participant key. Include the ",
        "group field when the same student labels are reused in different groups."
      ),
      selectizeInput(
        session$ns("raw_unit_columns"),
        "Unit identifier field(s)",
        choices = choices,
        selected = defaults$units,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      selectizeInput(
        session$ns("raw_conversation_columns"),
        "Conversation / sequence field(s)",
        choices = choices,
        selected = defaults$conversation,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      selectizeInput(
        session$ns("raw_code_columns"),
        "Code columns (three or more)",
        choices = choices,
        selected = defaults$codes,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      selectizeInput(
        session$ns("raw_metadata_columns"),
        "Additional unit-level metadata (optional)",
        choices = choices,
        selected = defaults$metadata,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      selectInput(
        session$ns("raw_group_column"),
        "Primary grouping field",
        choices = choices,
        selected = defaults$group
      ),
      selectInput(
        session$ns("raw_model"),
        "ENA model",
        choices = c(
          "Endpoint (one network per unit)" = "EndPoint",
          "Accumulated trajectory" = "AccumulatedTrajectory",
          "Separate trajectory" = "SeparateTrajectory"
        ),
        selected = defaults$model
      ),
      selectInput(
        session$ns("raw_window"),
        "Co-occurrence window",
        choices = c(
          "Moving stanza window" = "MovingStanzaWindow",
          "Whole conversation" = "Conversation"
        ),
        selected = defaults$window
      ),
      numericInput(
        session$ns("raw_window_size_back"),
        "Previous rows in stanza window",
        value = defaults$window_size_back,
        min = 1L,
        max = 100L,
        step = 1L
      ),
      radioButtons(
        session$ns("raw_rotation"),
        "Rotation",
        choices = c(
          "SVD (recommended for general exploration)" = "SVD",
          "Means rotation using first two group levels" = "Means"
        ),
        selected = defaults$rotation
      ),
      actionButton(
        session$ns("raw_build_ena"),
        "Build ENA and open 3D analysis",
        class = "btn-primary",
        `aria-describedby` = session$ns("raw_model_status")
      )
    )
  })

  observeEvent(input$raw_data_file, {
    tryCatch(
      {
        resolved <- ena3d_resolve_raw_upload(
          input$raw_data_file,
          limits = limits,
          upload_root = tempdir()
        )
        sheets <- ena3d_excel_sheets(
          resolved$path, resolved$extension, limits = limits
        )
        raw_upload(list(resolved = resolved, sheets = sheets))
        stage_raw_table(
          resolved,
          sheet = if (length(sheets)) sheets[[1L]] else NULL
        )
        ena3d_security_log(
          "public_raw_table_staged",
          fields = list(
            extension = resolved$extension,
            file_bytes = resolved$size,
            rows = raw_stage()$rows,
            columns = raw_stage()$columns
          )
        )
      },
      error = function(error) {
        raw_upload(NULL)
        raw_stage(NULL)
        raw_model_status(NULL)
        raw_upload_status(list(
          type = "error",
          title = "Raw data could not be loaded",
          detail = conditionMessage(error)
        ))
        ena3d_security_log(
          "public_raw_table_rejected",
          level = "WARN",
          fields = list(error_class = class(error)[[1L]])
        )
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$raw_excel_sheet, {
    upload <- raw_upload()
    req(upload, input$raw_excel_sheet)
    current <- raw_stage()
    if (!is.null(current) && identical(current$sheet, input$raw_excel_sheet)) {
      return(invisible(NULL))
    }
    tryCatch(
      stage_raw_table(upload$resolved, sheet = input$raw_excel_sheet),
      error = function(error) {
        raw_upload_status(list(
          type = "error",
          title = "Worksheet could not be loaded",
          detail = conditionMessage(error)
        ))
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$raw_build_ena, {
    staged <- raw_stage()
    req(staged)
    mapping <- list(
      units = input$raw_unit_columns,
      conversation = input$raw_conversation_columns,
      codes = input$raw_code_columns,
      metadata = input$raw_metadata_columns,
      group = input$raw_group_column,
      model = input$raw_model,
      window = input$raw_window,
      window_size_back = input$raw_window_size_back,
      rotation = input$raw_rotation
    )
    raw_model_status(list(
      type = "info",
      title = "Building ENA model",
      detail = "Validating mappings, accumulating co-occurrences, and rotating the model."
    ))

    tryCatch(
      withProgress(message = "Building ENA model", value = 0, {
        incProgress(0.15, detail = "Validating field mapping")
        built <- ena3d_build_ena_from_raw(
          staged$data, mapping, limits = limits
        )
        incProgress(0.65, detail = "Opening the model in 3D ENA")
        generated_path <- tempfile("ena3d-raw-model-", fileext = ".RData")
        on.exit(unlink(generated_path), add = TRUE)
        generated_ena <- built$ena_obj
        save(generated_ena, file = generated_path, version = 3L)
        loaded <- load_ena_data(
          input,
          output,
          session,
          generated_path,
          rv_data,
          state,
          source_kind = "trusted_native",
          limits = limits,
          display_name = paste0(staged$source$name, " (modeled)"),
          app_version = if (!is.null(config$app_version)) {
            config$app_version
          } else {
            Sys.getenv("ENA3D_APP_VERSION", unset = "development")
          },
          build_id = if (!is.null(config$build_id)) {
            config$build_id
          } else {
            Sys.getenv("ENA3D_BUILD_ID", unset = "development")
          }
        )
        incProgress(0.2, detail = "Ready")
        raw_model_status(list(
          type = "success",
          title = "ENA model is active",
          detail = sprintf(
            paste0(
              "%s coded rows produced %s ENA points, %s units, %s nodes, ",
              "and %s dimensions. Open Model → Trajectory for path and bootstrap analysis."
            ),
            format(built$raw_rows, big.mark = ","),
            format(built$points, big.mark = ","),
            format(built$units, big.mark = ","),
            format(built$nodes, big.mark = ","),
            format(length(built$dimensions), big.mark = ",")
          )
        ))
        ena3d_security_log(
          "public_raw_ena_built",
          fields = list(
            extension = staged$source$extension,
            raw_rows = built$raw_rows,
            point_rows = nrow(loaded$points),
            units = built$units,
            nodes = built$nodes,
            dimensions = length(built$dimensions),
            model = built$mapping$model
          )
        )
      }),
      error = function(error) {
        raw_model_status(list(
          type = "error",
          title = "ENA model was not built",
          detail = paste(
            conditionMessage(error),
            "The previously active dataset remains unchanged."
          )
        ))
        ena3d_security_log(
          "public_raw_ena_rejected",
          level = "WARN",
          fields = list(error_class = class(error)[[1L]])
        )
      }
    )
  }, ignoreInit = TRUE)

  # Keep a server-side guard even though the public UI never creates this
  # native-R input. A client can forge Shiny input messages, so browser data
  # must never reach native deserialization or expression evaluation.
  observeEvent(input$ena_data_file, {
    ena3d_security_log(
      "public_native_upload_blocked",
      level = "WARN",
      fields = list(reason = "native_r_serialization_disabled")
    )
    showNotification(
      ena3d_public_upload_message(),
      type = "warning",
      duration = NULL
    )
  }, ignoreInit = TRUE)

  observeEvent(input$ena_exchange_file, {
    upload <- input$ena_exchange_file
    tryCatch(
      {
        resolved <- ena3d_resolve_exchange_upload(
          upload,
          limits = limits,
          upload_root = tempdir()
        )
        loaded <- load_ena_data(
          input,
          output,
          session,
          resolved$path,
          rv_data,
          state,
          source_kind = "exchange",
          limits = limits,
          display_name = resolved$name,
          app_version = if (!is.null(config$app_version)) {
            config$app_version
          } else {
            Sys.getenv("ENA3D_APP_VERSION", unset = "development")
          },
          build_id = if (!is.null(config$build_id)) {
            config$build_id
          } else {
            Sys.getenv("ENA3D_BUILD_ID", unset = "development")
          }
        )
        ena3d_security_log(
          "public_exchange_loaded",
          fields = list(
            file_bytes = resolved$size,
            point_rows = nrow(loaded$points),
            nodes = nrow(loaded$rotation$nodes)
          )
        )
        showNotification(
          "The .ena3d.json exchange file is active.",
          type = "message",
          duration = 5
        )
      },
      error = function(error) {
        ena3d_security_log(
          "public_exchange_rejected",
          level = "WARN",
          fields = list(error_class = class(error)[[1L]])
        )
        showNotification(
          paste(
            conditionMessage(error),
            "The previously active dataset remains unchanged."
          ),
          type = "error",
          duration = NULL
        )
      }
    )
  }, ignoreInit = TRUE)
}
