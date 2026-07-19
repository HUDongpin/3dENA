# Server lifecycle for evidence-grounded Qwen interpretation.
#
# Numerical evidence is prepared locally by ai_evidence.R.  This module never
# sends raw ENA rows to the provider and never treats model output as HTML.

.ena3d_ai_process_registry <- new.env(parent = emptyenv())


.ena3d_ai_resolve <- function(value) {
  if (is.function(value)) value() else value
}


.ena3d_ai_condition <- function(message, class) {
  structure(
    list(message = as.character(message), call = NULL),
    class = c(class, "ena3d_ai_error", "error", "condition")
  )
}


.ena3d_ai_prune_processes <- function() {
  for (key in ls(.ena3d_ai_process_registry, all.names = TRUE)) {
    process <- get(key, envir = .ena3d_ai_process_registry, inherits = FALSE)
    alive <- tryCatch(isTRUE(process$is_alive()), error = function(error) FALSE)
    if (!alive) rm(list = key, envir = .ena3d_ai_process_registry)
  }
  invisible(NULL)
}


# Run the outbound request outside the Shiny worker.  The child reads the
# credential from its inherited environment or mounted secret file; the secret
# is deliberately never passed in callr arguments.
ena3d_ai_start_qwen_job <- function(
    evidence, mode, language, research_context = NULL,
    client_file, timeout_seconds = 60, max_processes = 4L,
    poll_interval = 0.1) {
  for (package in c("callr", "later", "promises")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop(sprintf("The `%s` package is required for AI interpretation.", package),
           call. = FALSE)
    }
  }
  if (!is.character(client_file) || length(client_file) != 1L ||
      !file.exists(client_file)) {
    stop("The Qwen client source file is unavailable.", call. = FALSE)
  }
  timeout_seconds <- as.numeric(timeout_seconds)
  max_processes <- as.integer(max_processes)
  if (!is.finite(timeout_seconds) || timeout_seconds <= 0) {
    stop("AI timeout must be a positive finite number.", call. = FALSE)
  }
  if (is.na(max_processes) || max_processes < 1L) {
    stop("AI process limit must be a positive integer.", call. = FALSE)
  }

  .ena3d_ai_prune_processes()
  if (length(ls(.ena3d_ai_process_registry, all.names = TRUE)) >= max_processes) {
    stop(
      "The server is already handling the maximum number of AI requests. Try again shortly.",
      call. = FALSE
    )
  }

  started_at <- unname(proc.time()[["elapsed"]])
  process <- callr::r_bg(
    func = function(client_file, evidence, mode, language, research_context) {
      client <- new.env(parent = globalenv())
      sys.source(client_file, envir = client)
      config <- get(
        "ena3d_qwen_config_from_env", envir = client, inherits = FALSE
      )(load_secret = TRUE)
      get("ena3d_qwen_interpret", envir = client, inherits = FALSE)(
        evidence = evidence,
        mode = mode,
        language = language,
        research_context = research_context,
        config = config
      )
    },
    args = list(
      client_file = normalizePath(client_file, mustWork = TRUE),
      evidence = evidence,
      mode = mode,
      language = language,
      research_context = research_context
    ),
    stdout = "|",
    stderr = "|",
    supervise = TRUE
  )
  registry_key <- as.character(process$get_pid())
  assign(registry_key, process, envir = .ena3d_ai_process_registry)

  settled <- FALSE
  reject_callback <- NULL
  release <- function() {
    if (exists(registry_key, envir = .ena3d_ai_process_registry,
               inherits = FALSE)) {
      rm(list = registry_key, envir = .ena3d_ai_process_registry)
    }
    invisible(NULL)
  }
  terminate <- function() {
    if (tryCatch(process$is_alive(), error = function(error) FALSE)) {
      try(process$kill_tree(), silent = TRUE)
    }
    invisible(NULL)
  }
  settle <- function(callback, value) {
    if (settled) return(invisible(FALSE))
    settled <<- TRUE
    release()
    callback(value)
    invisible(TRUE)
  }

  promise <- promises::promise(function(resolve, reject) {
    reject_callback <<- reject
    poll <- NULL
    poll <- function() {
      if (settled) return(invisible(NULL))
      elapsed <- unname(proc.time()[["elapsed"]]) - started_at
      if (elapsed >= timeout_seconds) {
        terminate()
        settle(reject, .ena3d_ai_condition(
          "The AI request exceeded its time limit and was cancelled.",
          "ena3d_ai_timeout"
        ))
        return(invisible(NULL))
      }
      alive <- tryCatch(process$is_alive(), error = function(error) FALSE)
      if (!alive) {
        value <- tryCatch(process$get_result(), error = function(error) error)
        if (inherits(value, "error")) {
          # The client is responsible for sanitizing provider errors.  Do not
          # append child stdout/stderr here because either could contain data.
          settle(reject, .ena3d_ai_condition(
            conditionMessage(value), "ena3d_ai_provider_error"
          ))
        } else {
          settle(resolve, value)
        }
        return(invisible(NULL))
      }
      later::later(poll, delay = poll_interval)
      invisible(NULL)
    }
    later::later(poll, delay = 0)
  })

  cancel <- function(reason = "The AI request was cancelled.") {
    if (settled) return(invisible(FALSE))
    terminate()
    condition <- .ena3d_ai_condition(reason, "ena3d_ai_cancelled")
    if (is.function(reject_callback)) {
      settle(reject_callback, condition)
    } else {
      settled <<- TRUE
      release()
    }
    invisible(TRUE)
  }

  structure(
    list(promise = promise, cancel = cancel, process = process),
    class = "ena3d_ai_job"
  )
}


.ena3d_ai_bound_context <- function(value, max_chars = 1500L) {
  if (is.null(value) || !length(value)) return(NULL)
  value <- enc2utf8(paste(as.character(value), collapse = " "))
  value <- gsub("[[:cntrl:]]+", " ", value)
  value <- trimws(gsub("[[:space:]]+", " ", value))
  if (!nzchar(value)) return(NULL)
  substr(value, 1L, as.integer(max_chars))
}


.ena3d_ai_current_view <- function(workspace_section, model_tab) {
  if (identical(workspace_section, "Stats")) return("stats")
  if (!identical(workspace_section, "Model")) return(NULL)
  if (is.null(model_tab) || length(model_tab) != 1L || is.na(model_tab)) {
    return(NULL)
  }
  switch(
    as.character(model_tab),
    overall_model = "overall",
    network = "network",
    comparison_plot = "comparison",
    group_change = "change",
    trajectory = "trajectory",
    NULL
  )
}


.ena3d_ai_scope_label <- function(view) {
  if (is.null(view) || length(view) != 1L || is.na(view)) {
    return("No interpretable analysis selected")
  }
  switch(
    view,
    overall = "Model › Overall",
    network = "Model › Networks",
    comparison = "Model › Comparison",
    change = "Model › Change",
    trajectory = "Model › Trajectory",
    stats = "Stats",
    "No interpretable analysis selected"
  )
}


.ena3d_ai_format_items <- function(items) {
  if (is.null(items) || !length(items)) return("")
  paste0("• ", as.character(unlist(items, use.names = FALSE)), collapse = "\n")
}


.ena3d_ai_format_claims <- function(claims) {
  if (is.null(claims) || !length(claims)) return("")
  paste(vapply(seq_along(claims), function(index) {
    claim <- claims[[index]]
    references <- paste(
      as.character(unlist(claim$evidence_ids, use.names = FALSE)),
      collapse = ", "
    )
    sprintf(
      "%d. %s\nEvidence: %s · Model confidence: %s (not a statistical grade)",
      index, as.character(claim$text), references, as.character(claim$strength)
    )
  }, character(1)), collapse = "\n\n")
}


.ena3d_ai_format_referenced_evidence <- function(ledger, interpretation) {
  if (is.null(ledger) || is.null(interpretation) ||
      is.null(ledger$evidence) || !length(ledger$evidence)) return("")
  references <- unique(unlist(lapply(
    interpretation$claims,
    function(claim) as.character(unlist(claim$evidence_ids, use.names = FALSE))
  ), use.names = FALSE))
  entries <- ledger$evidence
  fallback_names <- names(entries)
  if (is.null(fallback_names)) fallback_names <- paste0("E", seq_along(entries))
  entry_id <- function(entry, fallback) {
    if (is.list(entry) && !is.null(entry$id) && length(entry$id)) {
      return(as.character(entry$id[[1L]]))
    }
    fallback
  }
  names_or_ids <- vapply(
    seq_along(entries),
    function(index) entry_id(entries[[index]], fallback_names[[index]]),
    character(1)
  )
  matched <- match(references, names_or_ids)
  matched <- matched[!is.na(matched)]
  if (!length(matched)) return("")
  paste(vapply(matched, function(index) {
    entry <- entries[[index]]
    paste0(
      names_or_ids[[index]], ": ",
      jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null", na = "null",
                       digits = NA)
    )
  }, character(1)), collapse = "\n")
}


.ena3d_ai_provider_envelope <- function(
    evidence, mode, language, research_context) {
  if (exists(
    "ena3d_qwen_request_envelope", mode = "function", inherits = TRUE
  )) {
    return(get(
      "ena3d_qwen_request_envelope", mode = "function", inherits = TRUE
    )(
      evidence = evidence,
      mode = mode,
      language = language,
      research_context = research_context
    ))
  }

  # Keep standalone module tests and downstream reuse possible when the Qwen
  # client has not been sourced. This fallback mirrors the public helper's
  # stable field order and contains only the provider data envelope.
  list(
    task = list(
      mode = mode,
      output_language = language,
      instructions = switch(
        mode,
        quick = "Prioritize a concise descriptive interpretation.",
        deep = paste(
          "Provide a careful interpretation with uncertainty, limitations,",
          "and useful follow-up analyses."
        ),
        challenge = paste(
          "Stress-test the most obvious interpretation and emphasize credible",
          "alternative explanations and disconfirming checks."
        )
      )
    ),
    evidence = evidence,
    optional_research_context = if (
      is.null(research_context) || !nzchar(research_context)
    ) NULL else research_context
  )
}


.ena3d_ai_envelope_json <- function(envelope, pretty = FALSE) {
  as.character(jsonlite::toJSON(
    envelope,
    pretty = isTRUE(pretty),
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    dataframe = "rows",
    digits = NA,
    POSIXt = "ISO8601",
    UTC = TRUE
  ))
}


.ena3d_ai_envelope_hash <- function(envelope) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("The digest package is required for preview binding.", call. = FALSE)
  }
  digest::digest(
    .ena3d_ai_envelope_json(envelope),
    algo = "sha256",
    serialize = FALSE
  )
}


# `settings` is a reactive list assembled by the parent ENA module.  Keeping
# that assembly outside this module prevents the AI layer from reaching into
# unrelated Shiny inputs and makes the outbound contract testable.
ai_interpretation_server <- function(
    id, enabled, page_active, workspace_section, model_tab, ena_obj,
    settings, data_version = NULL, stats_result = NULL,
    trajectory_result = NULL, config = list(),
    job_starter = ena3d_ai_start_qwen_job) {
  shiny::moduleServer(id, function(input, output, session) {
    defaults <- list(
      min_cell_n = 5L,
      top_n = 10L,
      context_max_chars = 1500L,
      timeout_seconds = 60,
      max_processes = 4L,
      max_requests_per_hour = 10L,
      max_evidence_bytes = 65536L,
      qwen_client_file = file.path(getwd(), "qwen_client.R")
    )
    config <- utils::modifyList(defaults, config)
    state <- shiny::reactiveValues(
      preview_open = FALSE,
      preview_envelope = NULL,
      preview_hash = NULL,
      consent_hash = NULL,
      ledger = NULL,
      result_ledger = NULL,
      interpretation = NULL,
      meta = NULL,
      error = NULL,
      status = "Choose an analysis, review the data preview, and request an interpretation.",
      stale = FALSE,
      active_job = NULL,
      generation = 0L,
      request_times = as.POSIXct(character())
    )

    is_enabled <- shiny::reactive(isTRUE(.ena3d_ai_resolve(enabled)))
    current_view <- shiny::reactive({
      .ena3d_ai_current_view(
        .ena3d_ai_resolve(workspace_section), .ena3d_ai_resolve(model_tab)
      )
    })
    current_request_options <- shiny::reactive({
      mode <- input$mode
      if (is.null(mode) || length(mode) != 1L ||
          !mode %in% c("quick", "deep", "challenge")) {
        mode <- "quick"
      }
      list(
        mode = mode,
        language = if (identical(input$language, "zh")) "Chinese" else "English",
        research_context = .ena3d_ai_bound_context(
          input$research_context, config$context_max_chars
        )
      )
    })
    source_signature <- shiny::reactive({
      resolved_settings <- .ena3d_ai_resolve(settings)
      resolved_data_version <- .ena3d_ai_resolve(data_version)
      if (is.null(resolved_data_version)) {
        object <- .ena3d_ai_resolve(ena_obj)
        resolved_data_version <- if (is.null(object)) {
          NULL
        } else if (exists(
          "ena3d_ai_data_fingerprint", mode = "function", inherits = TRUE
        )) {
          ena3d_ai_data_fingerprint(object)
        } else {
          object
        }
      }
      list(
        page = isTRUE(.ena3d_ai_resolve(page_active)),
        view = current_view(),
        data_version = resolved_data_version,
        settings = resolved_settings,
        interpretation_options = list(
          mode = input$mode,
          language = input$language,
          research_context = .ena3d_ai_bound_context(
            input$research_context, config$context_max_chars
          )
        ),
        trajectory = if (identical(current_view(), "trajectory")) {
          .ena3d_ai_resolve(trajectory_result)
        } else {
          NULL
        },
        stats = if (identical(current_view(), "stats")) {
          .ena3d_ai_resolve(stats_result)
        } else {
          NULL
        }
      )
    })

    preview_ready <- shiny::reactive({
      !is.null(state$preview_envelope) &&
        !is.null(state$preview_hash) &&
        identical(
          state$preview_hash,
          .ena3d_ai_envelope_hash(state$preview_envelope)
        )
    })
    consent_ready <- shiny::reactive({
      isTRUE(input$consent) &&
        isTRUE(preview_ready()) &&
        identical(state$consent_hash, state$preview_hash)
    })

    reset_preview_consent <- function(clear_ledger = FALSE) {
      state$preview_open <- FALSE
      state$preview_envelope <- NULL
      state$preview_hash <- NULL
      state$consent_hash <- NULL
      if (isTRUE(clear_ledger)) state$ledger <- NULL
      shiny::updateCheckboxInput(session, "consent", value = FALSE)
      invisible(NULL)
    }

    send_ui_state <- function(open = NULL) {
      state_name <- if (!is_enabled()) {
        "disabled"
      } else if (!is.null(state$active_job)) {
        "loading"
      } else if (!is.null(state$error)) {
        "error"
      } else if (!is.null(state$interpretation)) {
        "ready"
      } else {
        "idle"
      }
      message <- list(
        id = session$ns("root"),
        state = state_name,
        stale = isTRUE(state$stale),
        preview_ready = isTRUE(preview_ready()),
        consent_ready = isTRUE(consent_ready())
      )
      if (!is.null(open)) message$open <- isTRUE(open)
      session$sendCustomMessage(
        "ena3d-ai-interpretation-state",
        message
      )
      invisible(NULL)
    }

    cancel_active <- function(reason) {
      job <- state$active_job
      state$active_job <- NULL
      state$generation <- state$generation + 1L
      if (is.list(job) && is.function(job$cancel)) {
        try(job$cancel(reason), silent = TRUE)
        return(invisible(TRUE))
      }
      invisible(FALSE)
    }

    session$onSessionEnded(function() {
      shiny::isolate(cancel_active(
        "The AI request was cancelled because the session ended."
      ))
    })

    shiny::observe({
      is_enabled()
      state$active_job
      state$error
      state$interpretation
      state$stale
      state$preview_hash
      state$consent_hash
      send_ui_state()
    })
    session$onFlushed(function() shiny::isolate(send_ui_state()), once = TRUE)
    shiny::observeEvent(input$consent, {
      state$consent_hash <- if (
        isTRUE(input$consent) && isTRUE(preview_ready())
      ) state$preview_hash else NULL
    }, ignoreInit = FALSE)
    shiny::observeEvent(list(
      input$mode, input$language, input$research_context
    ), {
      had_bound_preview <- !is.null(state$preview_hash) ||
        !is.null(state$consent_hash)
      reset_preview_consent()
      state$error <- NULL
      if (had_bound_preview) {
        state$status <- paste(
          "Interpretation options changed. Review the current provider data",
          "envelope and consent again."
        )
      }
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$cancel, {
      if (cancel_active("The AI request was cancelled by the user.")) {
        state$status <- "AI request cancelled."
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(.ena3d_ai_resolve(page_active), {
      if (!isTRUE(.ena3d_ai_resolve(page_active))) {
        cancel_active("The AI request was cancelled after leaving the 3D ENA page.")
        reset_preview_consent(clear_ledger = TRUE)
        send_ui_state(open = FALSE)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(source_signature(), {
      had_result <- !is.null(state$interpretation)
      was_active <- cancel_active(
        "The AI request was cancelled because the analytical results changed."
      )
      reset_preview_consent(clear_ledger = TRUE)
      state$error <- NULL
      if (had_result) {
        state$stale <- TRUE
        state$status <- paste(
          "The analysis changed. This interpretation is stale; generate a new one."
        )
      } else if (was_active) {
        state$status <- "The analysis changed before interpretation completed."
      }
    }, ignoreInit = TRUE)

    build_ledger <- function() {
      if (!exists("ena3d_ai_build_evidence", mode = "function", inherits = TRUE)) {
        stop("The ENA evidence builder is unavailable.", call. = FALSE)
      }
      object <- .ena3d_ai_resolve(ena_obj)
      if (is.null(object)) stop("Load an ENA dataset first.", call. = FALSE)
      view <- current_view()
      if (is.null(view)) stop("Select an ENA model or Stats result first.", call. = FALSE)
      resolved_settings <- .ena3d_ai_resolve(settings)
      trajectory <- if (identical(view, "trajectory")) {
        .ena3d_ai_resolve(trajectory_result)
      } else {
        NULL
      }
      stats <- if (identical(view, "stats")) {
        .ena3d_ai_resolve(stats_result)
      } else {
        NULL
      }
      ledger <- ena3d_ai_build_evidence(
        ena_obj = object,
        view = view,
        settings = resolved_settings,
        stats_result = stats,
        trajectory_result = trajectory,
        min_cell_n = as.integer(config$min_cell_n),
        top_n = as.integer(config$top_n)
      )
      substantive <- vapply(ledger$evidence, function(item) {
        !item$type %in% c("axis_anchor", "trajectory_diagnostic")
      }, logical(1L))
      if (!length(substantive) || !any(substantive)) {
        stop(
          "No privacy-safe aggregate evidence is available for the current selection.",
          call. = FALSE
        )
      }
      payload <- ena3d_ai_public_payload(ledger)
      encoded <- jsonlite::toJSON(
        payload, auto_unbox = TRUE, null = "null", na = "null", digits = NA
      )
      if (nchar(encoded, type = "bytes") > as.numeric(config$max_evidence_bytes)) {
        stop("The aggregate evidence packet exceeds the configured byte limit.",
             call. = FALSE)
      }
      state$ledger <- ledger
      ledger
    }

    shiny::observeEvent(input$preview_toggle, {
      if (isTRUE(state$preview_open)) {
        state$preview_open <- FALSE
        return(invisible(NULL))
      }
      if (isTRUE(preview_ready())) {
        state$preview_open <- TRUE
        state$status <- paste(
          "The current provider data envelope is open. Confirm consent to",
          "enable interpretation."
        )
        return(invisible(NULL))
      }
      state$error <- NULL
      tryCatch({
        ledger <- build_ledger()
        options <- current_request_options()
        envelope <- .ena3d_ai_provider_envelope(
          evidence = ena3d_ai_public_payload(ledger),
          mode = options$mode,
          language = options$language,
          research_context = options$research_context
        )
        state$preview_envelope <- envelope
        state$preview_hash <- .ena3d_ai_envelope_hash(envelope)
        state$consent_hash <- NULL
        shiny::updateCheckboxInput(session, "consent", value = FALSE)
        state$preview_open <- TRUE
        state$status <- paste(
          "Review the exact provider data envelope below, then confirm consent",
          "before sending it."
        )
      }, error = function(error) {
        reset_preview_consent()
        state$error <- conditionMessage(error)
        state$status <- "The provider data envelope preview is unavailable."
      })
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$interpret, {
      state$error <- NULL
      if (!is_enabled()) {
        state$error <- paste(
          "AI interpretation is disabled on this deployment. Set ENA3D_AI_ENABLED=true",
          "and configure a server-side Qwen credential."
        )
        return(invisible(NULL))
      }
      if (!isTRUE(.ena3d_ai_resolve(page_active))) {
        state$error <- "AI interpretation is available only on the 3D ENA page."
        return(invisible(NULL))
      }
      if (!isTRUE(preview_ready())) {
        reset_preview_consent()
        state$error <- paste(
          "Open and review the exact current provider data envelope before",
          "requesting an interpretation."
        )
        state$status <- "Provider data envelope review is required."
        return(invisible(NULL))
      }
      if (!isTRUE(consent_ready())) {
        state$consent_hash <- NULL
        shiny::updateCheckboxInput(session, "consent", value = FALSE)
        state$error <- paste(
          "After reviewing the current provider data envelope, confirm consent",
          "before requesting an interpretation."
        )
        state$status <- "Consent to the reviewed provider data envelope is required."
        return(invisible(NULL))
      }

      now <- Sys.time()
      recent <- state$request_times[
        difftime(now, state$request_times, units = "hours") < 1
      ]
      if (length(recent) >= as.integer(config$max_requests_per_hour)) {
        state$error <- "This session has reached its hourly AI request limit."
        return(invisible(NULL))
      }

      ledger <- tryCatch(build_ledger(), error = function(error) error)
      if (inherits(ledger, "error")) {
        state$error <- conditionMessage(ledger)
        return(invisible(NULL))
      }
      evidence <- ena3d_ai_public_payload(ledger)
      options <- current_request_options()
      send_envelope <- tryCatch(
        .ena3d_ai_provider_envelope(
          evidence = evidence,
          mode = options$mode,
          language = options$language,
          research_context = options$research_context
        ),
        error = function(error) error
      )
      if (inherits(send_envelope, "error")) {
        reset_preview_consent()
        state$error <- conditionMessage(send_envelope)
        state$status <- "The current provider data envelope is invalid."
        return(invisible(NULL))
      }
      send_hash <- .ena3d_ai_envelope_hash(send_envelope)
      if (!identical(send_hash, state$preview_hash) ||
          !identical(send_envelope, state$preview_envelope)) {
        reset_preview_consent()
        state$error <- paste(
          "The provider data envelope changed after preview. Open and review",
          "the current envelope, then consent again."
        )
        state$status <- "Provider data envelope review expired."
        return(invisible(NULL))
      }

      mode <- options$mode
      language <- options$language
      research_context <- options$research_context

      cancel_active("A newer AI interpretation request was started.")
      generation <- state$generation
      state$request_times <- c(recent, now)
      state$interpretation <- NULL
      state$result_ledger <- NULL
      state$meta <- NULL
      state$stale <- FALSE
      state$status <- "Qwen is interpreting the aggregate evidence…"
      # The reviewed envelope and its consent are single-use. Consume them
      # before the outbound job starts so every attempt requires a fresh review.
      reset_preview_consent()

      job <- tryCatch(
        job_starter(
          evidence = evidence,
          mode = mode,
          language = language,
          research_context = research_context,
          client_file = config$qwen_client_file,
          timeout_seconds = config$timeout_seconds,
          max_processes = config$max_processes
        ),
        error = function(error) error
      )
      if (inherits(job, "error")) {
        state$error <- conditionMessage(job)
        state$status <- "AI interpretation could not start."
        return(invisible(NULL))
      }
      state$active_job <- job
      completed <- promises::then(job$promise, function(value) {
        shiny::isolate({
          if (!identical(state$generation, generation)) return(invisible(NULL))
          state$active_job <- NULL
          state$result_ledger <- ledger
          state$interpretation <- value$interpretation
          state$meta <- value$meta
          state$status <- "Interpretation completed. Verify every claim against its evidence."
          if (exists("ena3d_security_log", mode = "function", inherits = TRUE)) {
            usage <- value$meta$usage
            ena3d_security_log(
              "ai_interpretation_completed",
              fields = list(
                view = current_view(),
                model = value$meta$model,
                latency_ms = value$meta$latency_ms,
                input_tokens = usage$prompt_tokens,
                output_tokens = usage$completion_tokens,
                request_hash = substr(ledger$request_fingerprint, 1L, 16L)
              )
            )
          }
          invisible(value)
        })
      })
      promises::catch(completed, function(error) {
        shiny::isolate({
          if (!identical(state$generation, generation)) return(invisible(NULL))
          state$active_job <- NULL
          if (!inherits(error, "ena3d_ai_cancelled")) {
            state$error <- conditionMessage(error)
            state$status <- "AI interpretation failed; the ENA analysis is unchanged."
            if (exists("ena3d_security_log", mode = "function", inherits = TRUE)) {
              ena3d_security_log(
                "ai_interpretation_failed", level = "WARN",
                fields = list(
                  view = current_view(), error_class = class(error)[[1L]]
                )
              )
            }
          }
          NULL
        })
      })
      invisible(NULL)
    }, ignoreInit = TRUE)

    output$scope <- shiny::renderText(.ena3d_ai_scope_label(current_view()))
    output$status_summary <- shiny::renderText({
      if (!is_enabled()) {
        "AI is disabled until a server-side Qwen configuration is provided."
      } else {
        state$status
      }
    })
    output$stale_notice <- shiny::renderText({
      if (!isTRUE(state$stale)) "" else {
        "The underlying ENA result changed. Generate a new interpretation."
      }
    })
    output$preview <- shiny::renderText({
      if (!isTRUE(state$preview_open)) return("")
      envelope <- state$preview_envelope
      if (is.null(envelope)) return("")
      .ena3d_ai_envelope_json(envelope, pretty = TRUE)
    })
    output$error_message <- shiny::renderText({
      if (is.null(state$error)) "" else state$error
    })
    output$disabled_message <- shiny::renderText({
      if (is_enabled()) "" else paste(
        "The application remains fully usable. An operator can enable this panel",
        "with ENA3D_AI_ENABLED=true and a server-side Qwen credential."
      )
    })
    output$result_meta <- shiny::renderText({
      if (is.null(state$meta)) return("")
      usage <- state$meta$usage
      token_text <- if (is.list(usage) &&
                        is.numeric(usage$total_tokens) &&
                        length(usage$total_tokens) == 1L &&
                        is.finite(usage$total_tokens)) {
        paste0(" · ", usage$total_tokens, " tokens")
      } else {
        ""
      }
      paste0(
        "Model ", state$meta$model,
        " · ", state$meta$latency_ms, " ms",
        token_text
      )
    })
    output$result_headline <- shiny::renderText({
      if (is.null(state$interpretation)) "" else state$interpretation$headline
    })
    output$result_claims <- shiny::renderText({
      if (is.null(state$interpretation)) return("")
      .ena3d_ai_format_claims(state$interpretation$claims)
    })
    output$result_evidence <- shiny::renderText({
      .ena3d_ai_format_referenced_evidence(
        state$result_ledger, state$interpretation
      )
    })
    output$result_caveats <- shiny::renderText({
      if (is.null(state$interpretation)) return("")
      .ena3d_ai_format_items(state$interpretation$caveats)
    })
    output$result_alternatives <- shiny::renderText({
      if (is.null(state$interpretation)) return("")
      .ena3d_ai_format_items(state$interpretation$alternative_explanations)
    })
    output$result_next_checks <- shiny::renderText({
      if (is.null(state$interpretation)) return("")
      .ena3d_ai_format_items(state$interpretation$next_checks)
    })

    list(
      ledger = shiny::reactive(state$ledger),
      interpretation = shiny::reactive(state$interpretation),
      stale = shiny::reactive(state$stale),
      status = shiny::reactive(state$status),
      preview_hash = shiny::reactive(state$preview_hash),
      preview_ready = preview_ready,
      consent_ready = consent_ready
    )
  })
}
