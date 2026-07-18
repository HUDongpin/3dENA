sample_data_load_and_select <- function(input, output, session, rv_data, config, state) {
  limits <- if (!is.null(config$data_limits)) {
    config$data_limits
  } else {
    ena3d_data_limits()
  }
  # Choices are rendered into the initial HTML by data_upload_ui(). Sending an
  # update before the browser binds the select input can be lost on a new
  # WebSocket session, leaving only the placeholder visible.

  observeEvent(input$sample_data, {
    if (is.null(input$sample_data) || !nzchar(input$sample_data) ||
        identical(input$sample_data, "Select a sample dataset")) {
      return(invisible(NULL))
    }

    requested_name <- input$sample_data
    tryCatch(
      {
        file_path <- ena3d_resolve_trusted_sample(
          config$sample_data_path,
          requested_name
        )
        loaded <- load_ena_data(
          input,
          output,
          session,
          file_path,
          rv_data,
          state,
          source_kind = "bundled",
          limits = limits,
          display_name = requested_name,
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
          "trusted_sample_loaded",
          fields = list(
            sample = basename(file_path),
            point_rows = nrow(loaded$points),
            nodes = nrow(loaded$rotation$nodes)
          )
        )
      },
      error = function(error) {
        ena3d_security_log(
          "trusted_sample_load_failed",
          level = "ERROR",
          fields = list(
            sample = basename(as.character(requested_name)),
            error_class = class(error)[[1L]]
          )
        )
        showNotification(
          paste(
            "Could not load the trusted sample. The event was recorded;",
            "the previously active dataset remains unchanged. Contact the",
            "application operator if the problem persists."
          ),
          type = "error",
          duration = NULL
        )
      }
    )
  }, ignoreInit = TRUE)
}
