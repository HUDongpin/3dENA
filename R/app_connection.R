ena3d_runtime_profile <- function(
    value = Sys.getenv("ENA3D_RUNTIME_PROFILE", unset = "development")) {
  profile <- tolower(trimws(as.character(value)[[1L]]))
  allowed <- c("development", "persistent", "ephemeral-preview")
  if (!profile %in% allowed) {
    stop(
      paste0(
        "ENA3D_RUNTIME_PROFILE must be one of: ",
        paste(allowed, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  profile
}


ena3d_connection_policy <- function(profile) {
  switch(
    ena3d_runtime_profile(profile),
    persistent = "host-existing-session-only",
    development = "reload-required",
    `ephemeral-preview` = "reload-required"
  )
}


ena3d_validate_runtime_host <- function(
    profile,
    shiny_server_version = Sys.getenv("SHINY_SERVER_VERSION", unset = "")) {
  profile <- ena3d_runtime_profile(profile)
  server_version <- trimws(as.character(shiny_server_version)[[1L]])
  if (identical(profile, "persistent") && !nzchar(server_version)) {
    stop(
      paste(
        "The persistent runtime profile requires Posit Shiny Server.",
        "A plain shiny::runApp process cannot preserve an existing session",
        "across a transport interruption."
      ),
      call. = FALSE
    )
  }
  invisible(server_version)
}


ena3d_disable_new_session_reconnect <- function(session) {
  # Shiny Server reconnects its robust transport to the existing R session.
  # Shiny's application-level reconnect setting is a different mechanism: it
  # may start a new R session and replay browser inputs. That is unsafe for
  # uploaded files, reactiveValues, and in-flight analyses in this app.
  session$allowReconnect(FALSE)
  invisible(FALSE)
}


ena3d_connection_probe_nonce <- function(value) {
  if (!is.list(value) || is.data.frame(value) || length(value$nonce) != 1L ||
      !is.character(value$nonce) || is.na(value$nonce)) {
    return(NULL)
  }
  nonce <- value$nonce[[1L]]
  if (nchar(nonce, type = "bytes") < 1L ||
      nchar(nonce, type = "bytes") > 128L ||
      !grepl("^[A-Za-z0-9._:-]+$", nonce)) {
    return(NULL)
  }
  nonce
}


ena3d_session_proof_id <- function(session) {
  token <- session$token
  if (length(token) != 1L || !is.character(token) || is.na(token) ||
      !nzchar(token)) {
    stop("The Shiny session did not provide a valid session token.", call. = FALSE)
  }
  # The browser only needs a stable equality proof, not the bearer token.
  digest::digest(
    paste0("ena3d-existing-session:", token),
    algo = "sha256",
    serialize = FALSE
  )
}


ena3d_connection_proof_payload <- function(probe, session_id) {
  nonce <- ena3d_connection_probe_nonce(probe)
  if (is.null(nonce) || length(session_id) != 1L ||
      !is.character(session_id) || is.na(session_id) || !nzchar(session_id)) {
    return(NULL)
  }
  list(nonce = nonce, session_id = session_id)
}


ena3d_register_connection_proof <- function(input, session) {
  session_id <- ena3d_session_proof_id(session)
  shiny::observeEvent(
    input$ena3d_connection_probe,
    {
      payload <- ena3d_connection_proof_payload(
        input$ena3d_connection_probe,
        session_id
      )
      if (!is.null(payload)) {
        session$sendCustomMessage("ena3d-session-proof", payload)
      }
    },
    ignoreInit = TRUE,
    priority = 10000
  )
  invisible(session_id)
}


ena3d_connection_assets <- function(version) {
  cache_key <- utils::URLencode(as.character(version)[[1L]], reserved = TRUE)
  htmltools::tagList(
    shiny::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = paste0("connection_guard.css?v=", cache_key)
    ),
    shiny::tags$script(
      defer = NA,
      src = paste0("connection_guard.js?v=", cache_key)
    )
  )
}


ena3d_connection_guard_ui <- function() {
  htmltools::tagList(
    shiny::tags$div(
      id = "ena3d-connection-guard",
      class = "ena3d-connection-guard",
      hidden = NA,
      `data-state` = "connected",
      `data-session-proof` = "pending",
      `data-show-delay-ms` = "300",
      `data-proof-timeout-ms` = "8000",
      `data-proof-retry-ms` = "1000",
      `aria-hidden` = "true",
      shiny::tags$div(
        class = "ena3d-connection-backdrop",
        `aria-hidden` = "true"
      ),
      shiny::tags$section(
        class = "ena3d-connection-dialog",
        role = "alertdialog",
        `aria-modal` = "true",
        `aria-labelledby` = "ena3d-connection-title",
        `aria-describedby` = paste(
          "ena3d-connection-message",
          "ena3d-connection-warning"
        ),
        tabindex = "-1",
        shiny::tags$div(
          class = "ena3d-connection-status-mark",
          `aria-hidden` = "true",
          shiny::tags$span(class = "ena3d-connection-spinner")
        ),
        shiny::tags$p(class = "ena3d-connection-kicker", "SESSION CONNECTION"),
        shiny::tags$h2(
          id = "ena3d-connection-title",
          "Connection interrupted"
        ),
        shiny::tags$p(
          id = "ena3d-connection-message",
          class = "ena3d-connection-message",
          `aria-live` = "assertive",
          "Trying to reconnect. Keep this page open."
        ),
        shiny::tags$p(
          id = "ena3d-connection-warning",
          class = "ena3d-connection-warning",
          paste(
            "A new session will never be presented as a recovery.",
            "If this session cannot be restored, uploads and calculations",
            "must be started again after reloading."
          )
        ),
        shiny::tags$div(
          class = "ena3d-connection-actions",
          shiny::tags$button(
            id = "ena3d-connection-reload",
            class = "ena3d-connection-reload",
            type = "button",
            "Reload page"
          )
        )
      )
    ),
    shiny::tags$div(
      id = "ena3d-connection-live",
      class = "ena3d-visually-hidden",
      role = "status",
      `aria-live` = "polite",
      `aria-atomic` = "true"
    )
  )
}
