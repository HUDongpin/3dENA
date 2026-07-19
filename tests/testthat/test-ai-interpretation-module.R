library(testthat)
library(shiny)

.ai_module_roots <- c(".", "../..", "..")
.ai_module_root <- .ai_module_roots[file.exists(file.path(
  .ai_module_roots, "R", "app_module_ai_interpretation.R"
))][1L]
if (is.na(.ai_module_root)) stop("Could not locate the project root.")
.ai_module_root <- normalizePath(.ai_module_root, mustWork = TRUE)

source(file.path(.ai_module_root, "R", "qwen_client.R"), local = FALSE)
source(file.path(.ai_module_root, "R", "ai_evidence.R"), local = FALSE)
source(
  file.path(.ai_module_root, "R", "app_module_ai_interpretation.R"),
  local = FALSE
)


.ai_module_fixture <- function() {
  count <- 12L
  groups <- rep(c("Group A", "Group B"), each = count / 2L)
  points <- data.frame(
    ENA_UNIT = paste0("PRIVATE_UNIT_", seq_len(count)),
    condition = groups,
    MR1 = seq_len(count),
    SVD2 = 2 * seq_len(count),
    SVD3 = rev(seq_len(count)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  line_weights <- data.frame(
    ENA_UNIT = points$ENA_UNIT,
    condition = groups,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  line_weights[["Code A & Code B"]] <- seq_len(count) / count
  nodes <- data.frame(
    code = c("Code A", "Code B"),
    MR1 = c(-1, 1),
    SVD2 = c(1, -1),
    SVD3 = c(-0.5, 0.5),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  list(
    points = points,
    line.weights = line_weights,
    rotation = list(
      nodes = nodes,
      adjacency.key = rbind(c("Code A"), c("Code B"))
    )
  )
}


.ai_module_settings <- function() {
  list(
    group_var = "condition",
    selected_groups = c("Group A", "Group B"),
    axes = c("MR1", "SVD2", "SVD3")
  )
}


.ai_module_result <- function(evidence_id) {
  list(
    interpretation = list(
      headline = "The aggregate ENA pattern is clearly differentiated.",
      claims = list(list(
        text = "The selected aggregate contains interpretable evidence.",
        evidence_ids = list(evidence_id),
        strength = "strong"
      )),
      caveats = list("This is an aggregate descriptive interpretation."),
      alternative_explanations = list("Group composition may explain the pattern."),
      next_checks = list("Inspect uncertainty and compare the group networks.")
    ),
    meta = list(
      model = "qwen-test-double",
      latency_ms = 42,
      usage = list(
        prompt_tokens = 10L,
        completion_tokens = 5L,
        total_tokens = 15L
      )
    )
  )
}


.ai_module_first_substantive_id <- function(payload) {
  entries <- payload$evidence
  types <- vapply(entries, `[[`, character(1L), "type")
  entries[[which(types != "axis_anchor")[[1L]]]]$id
}


.ai_module_wait_for <- function(session, condition, timeout = 1) {
  deadline <- unname(proc.time()[["elapsed"]]) + timeout
  repeat {
    later::run_now(0.01)
    session$flushReact()
    if (isTRUE(condition())) return(TRUE)
    if (unname(proc.time()[["elapsed"]]) >= deadline) return(FALSE)
  }
}


test_that("page and view mapping exposes only active Model or Stats results", {
  expected <- c(
    overall_model = "overall",
    network = "network",
    comparison_plot = "comparison",
    group_change = "change",
    trajectory = "trajectory"
  )
  for (model_tab in names(expected)) {
    expect_identical(
      .ena3d_ai_current_view("Model", model_tab),
      unname(expected[[model_tab]])
    )
  }

  expect_identical(.ena3d_ai_current_view("Stats", NULL), "stats")
  expect_null(.ena3d_ai_current_view("Data", "overall_model"))
  expect_null(.ena3d_ai_current_view("Plot Tools", "network"))
  expect_null(.ena3d_ai_current_view("Model", "unknown"))
  expect_null(.ena3d_ai_current_view("Model", NULL))
  expect_identical(
    .ena3d_ai_scope_label(NULL),
    "No interpretable analysis selected"
  )
})


test_that("research context is normalized, optional, and hard bounded", {
  expect_null(.ena3d_ai_bound_context(NULL, max_chars = 10L))
  expect_null(.ena3d_ai_bound_context(" \n\t ", max_chars = 10L))
  expect_identical(
    .ena3d_ai_bound_context(
      c("  Alpha\n", "\tBeta  Gamma "),
      max_chars = 100L
    ),
    "Alpha Beta Gamma"
  )
  expect_identical(
    .ena3d_ai_bound_context("Alpha Beta Gamma", max_chars = 10L),
    "Alpha Beta"
  )
})


test_that("disabled integration never invokes the Qwen job starter", {
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  forbidden_starter <- function(...) {
    calls$count <- calls$count + 1L
    stop("The disabled integration must not start a job.", call. = FALSE)
  }

  testServer(
    ai_interpretation_server,
    args = list(
      enabled = FALSE,
      page_active = TRUE,
      workspace_section = "Model",
      model_tab = "overall_model",
      ena_obj = .ai_module_fixture(),
      settings = .ai_module_settings(),
      job_starter = forbidden_starter
    ),
    {
      session$setInputs(consent = FALSE, mode = "quick", language = "en")
      session$flushReact()
      session$setInputs(preview_toggle = 1L)
      session$flushReact()
      session$setInputs(consent = TRUE)
      session$flushReact()
      session$setInputs(interpret = 1L)
      session$flushReact()

      expect_identical(calls$count, 0L)
      expect_match(output$error_message, "disabled on this deployment")
      expect_match(output$disabled_message, "application remains fully usable")
      expect_match(output$status_summary, "AI is disabled")
      expect_null(session$returned$interpretation())
    }
  )
})


test_that("server guardrails require the active page and explicit consent", {
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  forbidden_starter <- function(...) {
    calls$count <- calls$count + 1L
    stop("A guarded request must not start a job.", call. = FALSE)
  }
  common <- list(
    enabled = TRUE,
    workspace_section = "Model",
    model_tab = "overall_model",
    ena_obj = .ai_module_fixture(),
    settings = .ai_module_settings(),
    job_starter = forbidden_starter
  )

  testServer(
    ai_interpretation_server,
    args = c(common, list(page_active = TRUE)),
    {
      session$setInputs(consent = FALSE, mode = "quick", language = "en")
      session$flushReact()
      session$setInputs(preview_toggle = 1L)
      session$flushReact()
      session$setInputs(interpret = 1L)
      session$flushReact()

      expect_identical(calls$count, 0L)
      expect_true(session$returned$preview_ready())
      expect_match(output$error_message, "confirm consent")
      expect_null(session$returned$interpretation())
    }
  )

  testServer(
    ai_interpretation_server,
    args = c(common, list(page_active = FALSE)),
    {
      session$setInputs(consent = FALSE, mode = "quick", language = "en")
      session$flushReact()
      session$setInputs(preview_toggle = 1L)
      session$flushReact()
      session$setInputs(consent = TRUE)
      session$flushReact()
      session$setInputs(interpret = 1L)
      session$flushReact()

      expect_identical(calls$count, 0L)
      expect_match(output$error_message, "only on the 3D ENA page")
      expect_null(session$returned$interpretation())
    }
  )
})


test_that("server rejects consent that is not bound to a reviewed preview", {
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  forbidden_starter <- function(...) {
    calls$count <- calls$count + 1L
    stop("An unreviewed envelope must never start a job.", call. = FALSE)
  }

  testServer(
    ai_interpretation_server,
    args = list(
      enabled = TRUE,
      page_active = TRUE,
      workspace_section = "Model",
      model_tab = "overall_model",
      ena_obj = .ai_module_fixture(),
      settings = .ai_module_settings(),
      job_starter = forbidden_starter
    ),
    {
      session$setInputs(consent = TRUE, mode = "quick", language = "en")
      session$flushReact()
      session$setInputs(interpret = 1L)
      session$flushReact()

      expect_identical(calls$count, 0L)
      expect_false(session$returned$preview_ready())
      expect_false(session$returned$consent_ready())
      expect_match(output$error_message, "Open and review the exact current")
      expect_match(output$status_summary, "review is required")
      expect_null(session$returned$interpretation())
    }
  )
})


test_that("preview contains the exact bounded aggregate request and no raw IDs", {
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  forbidden_starter <- function(...) {
    calls$count <- calls$count + 1L
    stop("Previewing evidence must not start a Qwen job.", call. = FALSE)
  }

  testServer(
    ai_interpretation_server,
    args = list(
      enabled = TRUE,
      page_active = TRUE,
      workspace_section = "Model",
      model_tab = "overall_model",
      ena_obj = .ai_module_fixture(),
      settings = .ai_module_settings(),
      config = list(context_max_chars = 12L),
      job_starter = forbidden_starter
    ),
    {
      session$setInputs(
        mode = "challenge",
        language = "zh",
        research_context = "  Alpha\n Beta\tGamma  "
      )
      session$flushReact()
      session$setInputs(preview_toggle = 1L)
      session$flushReact()

      preview_text <- output$preview
      preview <- jsonlite::fromJSON(preview_text, simplifyVector = FALSE)
      expect_identical(calls$count, 0L)
      expect_identical(preview$task$mode, "challenge")
      expect_identical(preview$task$output_language, "Chinese")
      expect_identical(preview$optional_research_context, "Alpha Beta G")
      expect_identical(preview$evidence$view, "overall")
      expect_true(preview$evidence$privacy$aggregation_only)
      expect_false(preview$evidence$privacy$unit_level_data_included)
      expect_false(preview$evidence$privacy$raw_rows_included)
      expect_true(any(vapply(
        preview$evidence$evidence,
        function(item) identical(item$type, "selection_summary"),
        logical(1L)
      )))
      expect_false(grepl("PRIVATE_UNIT_", preview_text, fixed = TRUE))
      expect_false(grepl('"ENA_UNIT"', preview_text, fixed = TRUE))
      expect_s3_class(session$returned$ledger(), "ena3d_ai_evidence_ledger")
      expect_true(session$returned$preview_ready())
      expect_match(session$returned$preview_hash(), "^[0-9a-f]{64}$")
      expect_false(session$returned$consent_ready())
    }
  )
})


test_that("option and analytical source changes invalidate preview consent", {
  settings <- reactiveVal(.ai_module_settings())
  data_version <- reactiveVal("dataset-a")
  model_tab <- reactiveVal("overall_model")
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  forbidden_starter <- function(...) {
    calls$count <- calls$count + 1L
    stop("Invalidation tests must not start a job.", call. = FALSE)
  }

  testServer(
    ai_interpretation_server,
    args = list(
      enabled = TRUE,
      page_active = TRUE,
      workspace_section = "Model",
      model_tab = model_tab,
      ena_obj = .ai_module_fixture(),
      settings = settings,
      data_version = data_version,
      job_starter = forbidden_starter
    ),
    {
      preview_click <- 0L
      review_and_consent <- function() {
        session$setInputs(consent = FALSE)
        session$flushReact()
        preview_click <<- preview_click + 1L
        session$setInputs(preview_toggle = preview_click)
        session$flushReact()
        expect_true(session$returned$preview_ready())
        session$setInputs(consent = TRUE)
        session$flushReact()
        expect_true(session$returned$consent_ready())
      }
      expect_invalidated <- function() {
        expect_false(session$returned$preview_ready())
        expect_false(session$returned$consent_ready())
        expect_null(session$returned$preview_hash())
        expect_identical(output$preview, "")
      }

      session$setInputs(
        mode = "quick", language = "en", research_context = "Initial context"
      )
      session$flushReact()

      review_and_consent()
      session$setInputs(mode = "deep")
      session$flushReact()
      expect_invalidated()

      review_and_consent()
      session$setInputs(language = "zh")
      session$flushReact()
      expect_invalidated()

      review_and_consent()
      session$setInputs(research_context = "Changed context")
      session$flushReact()
      expect_invalidated()

      review_and_consent()
      data_version("dataset-b")
      session$flushReact()
      expect_invalidated()

      review_and_consent()
      changed_settings <- .ai_module_settings()
      changed_settings$selected_groups <- "Group A"
      settings(changed_settings)
      session$flushReact()
      expect_invalidated()

      review_and_consent()
      model_tab("network")
      session$flushReact()
      expect_invalidated()

      expect_identical(calls$count, 0L)
    }
  )
})


test_that("successful fake job renders structured text and later becomes stale", {
  settings <- reactiveVal(.ai_module_settings())
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  calls$args <- NULL
  calls$evidence_id <- NULL

  successful_starter <- function(
      evidence, mode, language, research_context, client_file,
      timeout_seconds, max_processes) {
    calls$count <- calls$count + 1L
    calls$args <- list(
      evidence = evidence,
      mode = mode,
      language = language,
      research_context = research_context,
      client_file = client_file,
      timeout_seconds = timeout_seconds,
      max_processes = max_processes
    )
    calls$evidence_id <- .ai_module_first_substantive_id(evidence)
    list(
      promise = promises::promise_resolve(
        .ai_module_result(calls$evidence_id)
      ),
      cancel = function(reason) invisible(TRUE)
    )
  }

  testServer(
    ai_interpretation_server,
    args = list(
      enabled = TRUE,
      page_active = TRUE,
      workspace_section = "Model",
      model_tab = "overall_model",
      ena_obj = .ai_module_fixture(),
      settings = settings,
      config = list(context_max_chars = 10L),
      job_starter = successful_starter
    ),
    {
      session$setInputs(
        consent = FALSE,
        mode = "deep",
        language = "zh",
        research_context = "Alpha Beta Gamma"
      )
      session$flushReact()
      session$setInputs(preview_toggle = 1L)
      session$flushReact()
      reviewed_envelope_text <- output$preview
      reviewed_hash <- session$returned$preview_hash()
      expect_true(session$returned$preview_ready())
      expect_false(session$returned$consent_ready())

      session$setInputs(consent = TRUE)
      session$flushReact()
      expect_true(session$returned$consent_ready())
      session$setInputs(interpret = 1L)
      session$flushReact()

      completed <- .ai_module_wait_for(
        session,
        function() !is.null(session$returned$interpretation())
      )
      expect_true(completed)
      expect_identical(calls$count, 1L)
      expect_identical(calls$args$mode, "deep")
      expect_identical(calls$args$language, "Chinese")
      expect_identical(calls$args$research_context, "Alpha Beta")
      expect_true(calls$args$evidence$privacy$aggregation_only)
      outbound_text <- paste(capture.output(str(calls$args$evidence)), collapse = "\n")
      expect_false(grepl("PRIVATE_UNIT_", outbound_text, fixed = TRUE))
      sent_envelope <- ena3d_qwen_request_envelope(
        evidence = calls$args$evidence,
        mode = calls$args$mode,
        language = calls$args$language,
        research_context = calls$args$research_context
      )
      expect_identical(
        reviewed_envelope_text,
        .ena3d_ai_envelope_json(sent_envelope, pretty = TRUE)
      )
      expect_identical(reviewed_hash, .ena3d_ai_envelope_hash(sent_envelope))
      expect_false(session$returned$preview_ready())
      expect_false(session$returned$consent_ready())

      expect_identical(
        output$result_headline,
        "The aggregate ENA pattern is clearly differentiated."
      )
      expect_match(output$result_claims, "1. The selected aggregate")
      expect_match(
        output$result_claims,
        paste0(
          "Evidence: ", calls$evidence_id,
          " · Model confidence: strong (not a statistical grade)"
        ),
        fixed = TRUE
      )
      expect_match(output$result_evidence, paste0(calls$evidence_id, ":"))
      expect_match(output$result_caveats, "aggregate descriptive")
      expect_match(output$result_alternatives, "Group composition")
      expect_match(output$result_next_checks, "Inspect uncertainty")
      expect_identical(
        output$result_meta,
        "Model qwen-test-double · 42 ms · 15 tokens"
      )
      expect_match(output$status_summary, "Interpretation completed")
      expect_false(session$returned$stale())

      session$setInputs(interpret = 2L)
      session$flushReact()
      expect_identical(calls$count, 1L)
      expect_match(output$error_message, "Open and review the exact current")

      changed_settings <- .ai_module_settings()
      changed_settings$selected_groups <- "Group A"
      settings(changed_settings)
      session$flushReact()

      expect_true(session$returned$stale())
      expect_match(output$stale_notice, "underlying ENA result changed")
      expect_identical(
        output$result_headline,
        "The aggregate ENA pattern is clearly differentiated."
      )
      expect_match(output$result_evidence, paste0(calls$evidence_id, ":"))
    }
  )
})


test_that("analytical source changes cancel a pending job and ignore late results", {
  settings <- reactiveVal(.ai_module_settings())
  control <- new.env(parent = emptyenv())
  control$count <- 0L
  control$cancel_reasons <- character()
  control$resolve <- NULL
  control$evidence_id <- NULL

  pending_starter <- function(evidence, ...) {
    control$count <- control$count + 1L
    control$evidence_id <- .ai_module_first_substantive_id(evidence)
    pending <- promises::promise(function(resolve, reject) {
      control$resolve <- resolve
    })
    list(
      promise = pending,
      cancel = function(reason) {
        control$cancel_reasons <- c(control$cancel_reasons, reason)
        invisible(TRUE)
      }
    )
  }

  testServer(
    ai_interpretation_server,
    args = list(
      enabled = TRUE,
      page_active = TRUE,
      workspace_section = "Model",
      model_tab = "overall_model",
      ena_obj = .ai_module_fixture(),
      settings = settings,
      job_starter = pending_starter
    ),
    {
      session$setInputs(consent = FALSE, mode = "quick", language = "en")
      session$flushReact()
      session$setInputs(preview_toggle = 1L)
      session$flushReact()
      session$setInputs(consent = TRUE)
      session$flushReact()
      session$setInputs(interpret = 1L)
      session$flushReact()

      expect_identical(control$count, 1L)
      expect_true(is.function(control$resolve))
      expect_match(output$status_summary, "Qwen is interpreting")

      changed_settings <- .ai_module_settings()
      changed_settings$selected_groups <- "Group B"
      settings(changed_settings)
      session$flushReact()

      expect_length(control$cancel_reasons, 1L)
      expect_match(control$cancel_reasons[[1L]], "analytical results changed")
      expect_match(output$status_summary, "changed before interpretation completed")
      expect_null(session$returned$interpretation())
      expect_false(session$returned$stale())

      control$resolve(.ai_module_result(control$evidence_id))
      .ai_module_wait_for(session, function() TRUE, timeout = 0.05)
      expect_null(session$returned$interpretation())
      expect_match(output$status_summary, "changed before interpretation completed")
    }
  )
})


test_that("dataset version changes invalidate pending and completed requests", {
  fixture <- .ai_module_fixture()
  settings <- .ai_module_settings()
  pending_version <- reactiveVal("dataset-version-a")
  pending_control <- new.env(parent = emptyenv())
  pending_control$count <- 0L
  pending_control$cancel_reasons <- character()
  pending_control$resolve <- NULL
  pending_control$evidence_id <- NULL

  pending_starter <- function(evidence, ...) {
    pending_control$count <- pending_control$count + 1L
    pending_control$evidence_id <- .ai_module_first_substantive_id(evidence)
    pending <- promises::promise(function(resolve, reject) {
      pending_control$resolve <- resolve
    })
    list(
      promise = pending,
      cancel = function(reason) {
        pending_control$cancel_reasons <- c(
          pending_control$cancel_reasons,
          reason
        )
        invisible(TRUE)
      }
    )
  }

  testServer(
    ai_interpretation_server,
    args = list(
      enabled = TRUE,
      page_active = TRUE,
      workspace_section = "Model",
      model_tab = "overall_model",
      ena_obj = fixture,
      settings = settings,
      data_version = pending_version,
      job_starter = pending_starter
    ),
    {
      session$setInputs(consent = FALSE, mode = "quick", language = "en")
      session$flushReact()
      session$setInputs(preview_toggle = 1L)
      session$flushReact()
      session$setInputs(consent = TRUE)
      session$flushReact()
      session$setInputs(interpret = 1L)
      session$flushReact()

      expect_identical(pending_control$count, 1L)
      expect_true(is.function(pending_control$resolve))
      expect_match(output$status_summary, "Qwen is interpreting")

      pending_version("dataset-version-b")
      session$flushReact()

      expect_length(pending_control$cancel_reasons, 1L)
      expect_match(
        pending_control$cancel_reasons[[1L]],
        "analytical results changed"
      )
      expect_match(output$status_summary, "changed before interpretation completed")
      expect_null(session$returned$ledger())
      expect_null(session$returned$interpretation())
      expect_false(session$returned$stale())

      pending_control$resolve(
        .ai_module_result(pending_control$evidence_id)
      )
      .ai_module_wait_for(session, function() TRUE, timeout = 0.05)
      expect_null(session$returned$interpretation())
      expect_match(output$status_summary, "changed before interpretation completed")
    }
  )

  completed_version <- reactiveVal("dataset-version-a")
  completed_control <- new.env(parent = emptyenv())
  completed_control$count <- 0L
  completed_control$evidence_id <- NULL

  completed_starter <- function(evidence, ...) {
    completed_control$count <- completed_control$count + 1L
    completed_control$evidence_id <- .ai_module_first_substantive_id(evidence)
    list(
      promise = promises::promise_resolve(
        .ai_module_result(completed_control$evidence_id)
      ),
      cancel = function(reason) invisible(TRUE)
    )
  }

  testServer(
    ai_interpretation_server,
    args = list(
      enabled = TRUE,
      page_active = TRUE,
      workspace_section = "Model",
      model_tab = "overall_model",
      ena_obj = fixture,
      settings = settings,
      data_version = completed_version,
      job_starter = completed_starter
    ),
    {
      session$setInputs(consent = FALSE, mode = "deep", language = "en")
      session$flushReact()
      session$setInputs(preview_toggle = 1L)
      session$flushReact()
      session$setInputs(consent = TRUE)
      session$flushReact()
      session$setInputs(interpret = 1L)
      session$flushReact()

      completed <- .ai_module_wait_for(
        session,
        function() !is.null(session$returned$interpretation())
      )
      expect_true(completed)
      expect_identical(completed_control$count, 1L)
      expect_false(session$returned$stale())
      expect_identical(
        output$result_headline,
        "The aggregate ENA pattern is clearly differentiated."
      )

      completed_version("dataset-version-b")
      session$flushReact()

      expect_true(session$returned$stale())
      expect_match(output$stale_notice, "underlying ENA result changed")
      expect_match(output$status_summary, "interpretation is stale")
      expect_identical(completed_control$count, 1L)
      expect_identical(
        output$result_headline,
        "The aggregate ENA pattern is clearly differentiated."
      )
      expect_match(
        output$result_evidence,
        paste0(completed_control$evidence_id, ":")
      )
    }
  )
})
