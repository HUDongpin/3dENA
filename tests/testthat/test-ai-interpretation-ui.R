library(testthat)

.ai_ui_roots <- c(".", "..", "../..")
.ai_ui_root <- .ai_ui_roots[file.exists(
  file.path(.ai_ui_roots, "R", "app_ui_ai_interpretation.R")
)][1L]
if (is.na(.ai_ui_root)) stop("Could not locate the 3D ENA project root.")
.ai_ui_root <- normalizePath(.ai_ui_root)

if (!requireNamespace("shiny", quietly = TRUE) ||
    !requireNamespace("htmltools", quietly = TRUE)) {
  skip("AI interpretation UI tests require shiny and htmltools.")
}

.ai_ui_env <- new.env(parent = globalenv())
sys.source(
  file.path(.ai_ui_root, "R", "app_ui_ai_interpretation.R"),
  envir = .ai_ui_env
)


test_that("AI interpretation controls are namespaced and consent gated", {
  html <- htmltools::renderTags(
    .ai_ui_env$ai_interpretation_ui("ai", context_max_chars = 1500L)
  )$html

  required_ids <- c(
    "ai-root", "ai-toggle", "ai-drawer", "ai-close", "ai-mode",
    "ai-language", "ai-research_context", "ai-consent",
    "ai-preview_toggle", "ai-preview", "ai-interpret", "ai-cancel"
  )
  for (id in required_ids) {
    expect_match(html, paste0('id="', id, '"'), fixed = TRUE)
  }

  expect_match(html, 'value="quick"', fixed = TRUE)
  expect_match(html, 'value="deep"', fixed = TRUE)
  expect_match(html, 'value="challenge"', fixed = TRUE)
  expect_match(html, 'value="en"', fixed = TRUE)
  expect_match(html, 'value="zh"', fixed = TRUE)
  expect_match(html, '<textarea id="ai-research_context"', fixed = TRUE)
  expect_match(
    html,
    '<textarea id="ai-research_context"[^>]*maxlength="1500"'
  )
  expect_match(
    html,
    paste0(
      'id="ai-consent" type="checkbox" class="shiny-input-checkbox" ',
      'disabled="disabled" aria-disabled="true" ',
      'aria-describedby="ai-preview_requirement ai-privacy_notice"'
    ),
    fixed = TRUE
  )
  expect_match(html, 'data-preview-ready="false"', fixed = TRUE)
  expect_match(html, 'data-consent-ready="false"', fixed = TRUE)
  expect_match(html, 'disabled="disabled"', fixed = TRUE)
  expect_match(html, 'aria-disabled="true"', fixed = TRUE)
  expect_match(html, "Alibaba Cloud Qwen", fixed = TRUE)
  expect_match(html, "Review exact provider data envelope", fixed = TRUE)
  expect_match(html, "Exact provider data envelope", fixed = TRUE)
  expect_match(
    html,
    "Transport headers, the API key, and the fixed system prompt",
    fixed = TRUE
  )
  expect_match(html, "for this one interpretation request", fixed = TRUE)
})


test_that("AI interpretation has accessible drawer and state regions", {
  html <- htmltools::renderTags(
    .ai_ui_env$ai_interpretation_ui("interpretation")
  )$html

  expect_match(html, 'role="dialog"', fixed = TRUE)
  expect_match(html, 'aria-modal="true"', fixed = TRUE)
  expect_match(html, 'aria-haspopup="dialog"', fixed = TRUE)
  expect_match(html, 'aria-live="polite"', fixed = TRUE)
  expect_match(html, 'role="alert"', fixed = TRUE)
  expect_match(html, 'data-state="idle"', fixed = TRUE)
  expect_match(html, 'data-stale="false"', fixed = TRUE)

  state_ids <- c(
    "interpretation-empty_state", "interpretation-loading_state",
    "interpretation-error_state", "interpretation-disabled_state",
    "interpretation-result"
  )
  for (id in state_ids) {
    expect_match(html, paste0('id="', id, '"'), fixed = TRUE)
  }
})


test_that("all model-derived output slots are escaped text bindings", {
  source_text <- paste(
    readLines(
      file.path(.ai_ui_root, "R", "app_ui_ai_interpretation.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  html <- htmltools::renderTags(
    .ai_ui_env$ai_interpretation_ui("safe")
  )$html

  output_ids <- c(
    "safe-scope", "safe-status_summary", "safe-stale_notice",
    "safe-preview", "safe-error_message", "safe-result_meta",
    "safe-result_headline", "safe-result_claims", "safe-result_evidence",
    "safe-result_caveats", "safe-result_alternatives",
    "safe-result_next_checks"
  )
  for (id in output_ids) {
    expect_match(html, paste0('id="', id, '"'), fixed = TRUE)
  }

  expect_false(grepl("shiny::uiOutput(", source_text, fixed = TRUE))
  expect_false(grepl("shiny::htmlOutput(", source_text, fixed = TRUE))
  expect_match(source_text, "shiny::textOutput(", fixed = TRUE)
  expect_match(source_text, "shiny::verbatimTextOutput(", fixed = TRUE)
})


test_that("client gating mirrors server preview and consent readiness", {
  source_text <- paste(
    readLines(
      file.path(.ai_ui_root, "R", "app_ui_ai_interpretation.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(source_text, "message.preview_ready", fixed = TRUE)
  expect_match(source_text, "message.consent_ready", fixed = TRUE)
  expect_match(
    source_text,
    "consent.checked && previewReady && consentReady",
    fixed = TRUE
  )
  expect_match(source_text, "targetConsent.checked = false", fixed = TRUE)
  expect_match(
    source_text,
    "targetConsent.disabled = unavailable || !previewReady",
    fixed = TRUE
  )
})


test_that("AI interpretation CSS stays page scoped and becomes a bottom sheet", {
  css <- paste(
    readLines(
      file.path(.ai_ui_root, "R", "www", "ai_interpretation.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(css, ".ena3d-tool-page .ena-ai-drawer", fixed = TRUE)
  expect_match(css, "transform: translateX(102%);", fixed = TRUE)
  expect_match(css, "@media (max-width: 991.98px)", fixed = TRUE)
  expect_match(css, "transform: translateY(102%);", fixed = TRUE)
  expect_match(css, "height: 100dvh;", fixed = TRUE)
  expect_match(css, "max-height: 92dvh;", fixed = TRUE)
  expect_match(css, "prefers-reduced-motion: reduce", fixed = TRUE)
})


test_that("research context bound rejects unsafe limits", {
  expect_error(
    .ai_ui_env$ai_interpretation_ui("ai", context_max_chars = 99L),
    "between 100 and 5000"
  )
  expect_error(
    .ai_ui_env$ai_interpretation_ui("ai", context_max_chars = 5001L),
    "between 100 and 5000"
  )
  expect_error(
    .ai_ui_env$ai_interpretation_ui("ai", context_max_chars = 100.5),
    "whole number"
  )
})
