library(testthat)

.qwen_test_root <- c(".", "../..", "..")
.qwen_test_root <- .qwen_test_root[file.exists(
  file.path(.qwen_test_root, "R", "qwen_client.R")
)][1L]
if (is.na(.qwen_test_root)) stop("Could not locate the project root.")
.qwen_test_root <- normalizePath(.qwen_test_root, mustWork = TRUE)
source(file.path(.qwen_test_root, "R", "qwen_client.R"), local = FALSE)

.qwen_env_names <- c(
  "ENA3D_AI_ENABLED", "ENA3D_QWEN_REGION", "ENA3D_QWEN_BASE_URL",
  "ENA3D_QWEN_MODEL", "ENA3D_QWEN_TIMEOUT_SECONDS",
  "ENA3D_QWEN_CONNECT_TIMEOUT_SECONDS", "ENA3D_QWEN_MAX_REQUEST_BYTES",
  "ENA3D_QWEN_MAX_RESPONSE_BYTES", "ENA3D_QWEN_MAX_CONTEXT_BYTES",
  "ENA3D_QWEN_MAX_COMPLETION_TOKENS", "ENA3D_QWEN_THINKING_BUDGET",
  "ENA3D_QWEN_MAX_TOKENS", "ENA3D_QWEN_TEMPERATURE",
  "DASHSCOPE_API_KEY", "DASHSCOPE_API_KEY_FILE"
)

.qwen_with_env <- function(values = list()) {
  old <- Sys.getenv(.qwen_env_names, unset = NA_character_)
  Sys.unsetenv(.qwen_env_names)
  if (length(values)) do.call(Sys.setenv, values)
  function() {
    Sys.unsetenv(.qwen_env_names)
    present <- !is.na(old)
    if (any(present)) do.call(Sys.setenv, as.list(old[present]))
  }
}

.qwen_evidence <- function() {
  list(
    view = "overall",
    context = list(causal_design = FALSE),
    evidence = list(
      list(
        id = "E1",
        type = "centroid",
        scope = list(selection = "All groups"),
        metrics = list(value = 0.25, sample_size = 12L)
      ),
      list(
        id = "E2",
        type = "edge",
        scope = list(selection = "All groups"),
        metrics = list(value = 0.71, p_value = 0.04)
      )
    )
  )
}

.qwen_interpretation <- function(evidence_id = "E1", strength = "strong") {
  list(
    headline = "The aggregate network has a clear center.",
    claims = list(list(
      text = "The centroid lies on the positive side of the first axis.",
      evidence_ids = list(evidence_id),
      strength = strength
    )),
    caveats = list("This is an aggregate descriptive result."),
    alternative_explanations = list("The pattern may differ by subgroup."),
    next_checks = list("Inspect uncertainty around the centroid.")
  )
}

.qwen_api_response <- function(interpretation = .qwen_interpretation(),
                               status = 200L,
                               finish_reason = "stop") {
  content <- jsonlite::toJSON(
    interpretation,
    auto_unbox = TRUE,
    null = "null"
  )
  body <- list(
    id = "chatcmpl-test-1",
    model = "untrusted-server-model-name",
    choices = list(list(
      index = 0,
      message = list(role = "assistant", content = as.character(content)),
      finish_reason = finish_reason
    )),
    usage = list(prompt_tokens = 120, completion_tokens = 60, total_tokens = 180)
  )
  list(
    status_code = status,
    headers = list(`x-request-id` = "request-header-id"),
    content = charToRaw(as.character(jsonlite::toJSON(
      body,
      auto_unbox = TRUE,
      null = "null"
    )))
  )
}


test_that("configuration is fail-closed and bounded", {
  restore <- .qwen_with_env()
  on.exit(restore(), add = TRUE)

  config <- ena3d_qwen_config_from_env()
  expect_false(config$enabled)
  expect_identical(config$region, "cn-beijing")
  expect_identical(config$model, "qwen3.7-max-2026-06-08")
  expect_identical(config$max_completion_tokens, 4096L)
  expect_identical(config$thinking_budget, 1536L)
  expect_identical(
    config$endpoint,
    "https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions"
  )
  expect_false(config$secret_configured)

  Sys.setenv(ENA3D_AI_ENABLED = "perhaps")
  expect_error(
    ena3d_qwen_config_from_env(),
    class = "ena3d_qwen_config_error"
  )
  Sys.setenv(ENA3D_AI_ENABLED = "true", ENA3D_QWEN_TIMEOUT_SECONDS = "121")
  expect_error(
    ena3d_qwen_config_from_env(),
    class = "ena3d_qwen_config_error"
  )
})


test_that("only HTTPS endpoints allowlisted for the selected region are accepted", {
  restore <- .qwen_with_env(list(
    ENA3D_AI_ENABLED = "true",
    DASHSCOPE_API_KEY = "sk-test-credential"
  ))
  on.exit(restore(), add = TRUE)

  Sys.setenv(ENA3D_QWEN_BASE_URL = "http://dashscope.aliyuncs.com/compatible-mode/v1")
  expect_error(ena3d_qwen_config_from_env(), class = "ena3d_qwen_config_error")

  Sys.setenv(
    ENA3D_QWEN_BASE_URL = paste0(
      "https://dashscope.aliyuncs.com.attacker.example/compatible-mode/v1"
    )
  )
  expect_error(ena3d_qwen_config_from_env(), class = "ena3d_qwen_config_error")

  Sys.setenv(
    ENA3D_QWEN_REGION = "ap-southeast-1",
    ENA3D_QWEN_BASE_URL = "https://dashscope.aliyuncs.com/compatible-mode/v1"
  )
  expect_error(ena3d_qwen_config_from_env(), class = "ena3d_qwen_config_error")

  Sys.setenv(
    ENA3D_QWEN_BASE_URL = paste0(
      "https://llm-workspace-7.ap-southeast-1.maas.aliyuncs.com/",
      "compatible-mode/v1/"
    )
  )
  config <- ena3d_qwen_config_from_env()
  expect_identical(
    config$base_url,
    paste0(
      "https://llm-workspace-7.ap-southeast-1.maas.aliyuncs.com/",
      "compatible-mode/v1"
    )
  )
})


test_that("Qwen 3.7 Max model IDs are pinned and region scoped", {
  restore <- .qwen_with_env(list(
    ENA3D_AI_ENABLED = "true",
    DASHSCOPE_API_KEY = "sk-test-credential"
  ))
  on.exit(restore(), add = TRUE)

  singapore <- ena3d_qwen_config_from_env()
  expect_identical(singapore$model, "qwen3.7-max-2026-06-08")

  Sys.setenv(ENA3D_QWEN_REGION = "us-east-1")
  us <- ena3d_qwen_config_from_env()
  expect_identical(us$model, "qwen3.7-max-us")
  expect_identical(
    us$endpoint,
    "https://dashscope-us.aliyuncs.com/compatible-mode/v1/chat/completions"
  )

  Sys.setenv(ENA3D_QWEN_MODEL = "qwen3.7-max")
  expect_error(ena3d_qwen_config_from_env(), class = "ena3d_qwen_config_error")
  Sys.setenv(ENA3D_QWEN_MODEL = "qwen3.7-max-2026-06-08")
  expect_error(ena3d_qwen_config_from_env(), class = "ena3d_qwen_config_error")

  Sys.setenv(
    ENA3D_QWEN_REGION = "cn-beijing",
    ENA3D_QWEN_MODEL = "qwen3.7-max-us"
  )
  expect_error(ena3d_qwen_config_from_env(), class = "ena3d_qwen_config_error")

  Sys.setenv(ENA3D_QWEN_MODEL = "qwen3.7-max-preview")
  expect_error(ena3d_qwen_config_from_env(), class = "ena3d_qwen_config_error")
})


test_that("completion and thinking budgets are explicit and internally bounded", {
  restore <- .qwen_with_env(list(
    ENA3D_AI_ENABLED = "true",
    DASHSCOPE_API_KEY = "sk-test-credential",
    ENA3D_QWEN_MAX_COMPLETION_TOKENS = "2048",
    ENA3D_QWEN_THINKING_BUDGET = "1536"
  ))
  on.exit(restore(), add = TRUE)

  config <- ena3d_qwen_config_from_env()
  expect_identical(config$max_completion_tokens, 2048L)
  expect_identical(config$thinking_budget, 1536L)

  Sys.setenv(ENA3D_QWEN_THINKING_BUDGET = "1537")
  expect_error(ena3d_qwen_config_from_env(), class = "ena3d_qwen_config_error")

  Sys.setenv(
    ENA3D_QWEN_THINKING_BUDGET = "1536",
    ENA3D_QWEN_MAX_COMPLETION_TOKENS = "1000"
  )
  expect_error(ena3d_qwen_config_from_env(), class = "ena3d_qwen_config_error")
})


test_that("server secrets load from env or file and redact when printed", {
  credential <- "sk-SECRET-MUST-NEVER-PRINT"
  restore <- .qwen_with_env(list(
    ENA3D_AI_ENABLED = "true",
    DASHSCOPE_API_KEY = credential
  ))
  on.exit(restore(), add = TRUE)

  config <- ena3d_qwen_config_from_env(load_secret = TRUE)
  expect_s3_class(config$secret, "ena3d_qwen_secret")
  printed <- paste(capture.output(print(config)), collapse = "\n")
  secret_printed <- paste(capture.output(print(config$secret)), collapse = "\n")
  expect_false(grepl(credential, printed, fixed = TRUE))
  expect_false(grepl(credential, secret_printed, fixed = TRUE))
  expect_match(printed, "REDACTED", fixed = TRUE)

  path <- tempfile("qwen-secret-")
  writeLines(credential, path, useBytes = TRUE)
  on.exit(unlink(path), add = TRUE)
  Sys.unsetenv("DASHSCOPE_API_KEY")
  Sys.setenv(DASHSCOPE_API_KEY_FILE = path)
  file_secret <- ena3d_qwen_load_api_key()
  expect_s3_class(file_secret, "ena3d_qwen_secret")
  expect_false(grepl(
    credential,
    paste(capture.output(str(file_secret)), collapse = "\n"),
    fixed = TRUE
  ))

  Sys.setenv(DASHSCOPE_API_KEY = credential)
  error <- tryCatch(ena3d_qwen_load_api_key(), error = identity)
  expect_s3_class(error, "ena3d_qwen_config_error")
  expect_false(grepl(credential, conditionMessage(error), fixed = TRUE))
})


test_that("secret sources are preflighted without reading mounted file contents", {
  restore <- .qwen_with_env(list(ENA3D_AI_ENABLED = "true"))
  on.exit(restore(), add = TRUE)

  Sys.setenv(DASHSCOPE_API_KEY = "short")
  expect_error(
    ena3d_qwen_config_from_env(load_secret = FALSE),
    class = "ena3d_qwen_authentication_error"
  )
  Sys.setenv(DASHSCOPE_API_KEY = "sk-invalid key")
  expect_error(
    ena3d_qwen_config_from_env(load_secret = FALSE),
    class = "ena3d_qwen_authentication_error"
  )
  Sys.unsetenv("DASHSCOPE_API_KEY")

  Sys.setenv(DASHSCOPE_API_KEY_FILE = tempfile("missing-qwen-secret-"))
  expect_error(
    ena3d_qwen_config_from_env(load_secret = FALSE),
    class = "ena3d_qwen_config_error"
  )

  directory <- tempfile("qwen-secret-directory-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  Sys.setenv(DASHSCOPE_API_KEY_FILE = directory)
  expect_error(
    ena3d_qwen_config_from_env(load_secret = FALSE),
    class = "ena3d_qwen_config_error"
  )

  empty <- tempfile("empty-qwen-secret-")
  file.create(empty)
  on.exit(unlink(empty), add = TRUE)
  Sys.setenv(DASHSCOPE_API_KEY_FILE = empty)
  expect_error(
    ena3d_qwen_config_from_env(load_secret = FALSE),
    class = "ena3d_qwen_config_error"
  )

  oversized <- tempfile("oversized-qwen-secret-")
  writeBin(as.raw(rep(65L, 4097L)), oversized)
  on.exit(unlink(oversized), add = TRUE)
  Sys.setenv(DASHSCOPE_API_KEY_FILE = oversized)
  expect_error(
    ena3d_qwen_config_from_env(load_secret = FALSE),
    class = "ena3d_qwen_config_error"
  )

  # Valid metadata is sufficient in the long-lived process.  Invalid content is
  # intentionally discovered only when the request child loads the secret.
  invalid_content <- tempfile("qwen-secret-content-")
  writeLines("invalid key text", invalid_content, useBytes = TRUE)
  on.exit(unlink(invalid_content), add = TRUE)
  Sys.setenv(DASHSCOPE_API_KEY_FILE = invalid_content)
  config <- ena3d_qwen_config_from_env(load_secret = FALSE)
  expect_true(config$secret_configured)
  expect_null(config[["secret", exact = TRUE]])
  expect_error(
    ena3d_qwen_config_from_env(load_secret = TRUE),
    class = "ena3d_qwen_authentication_error"
  )
})


test_that("disabled integration fails before credential or transport access", {
  restore <- .qwen_with_env()
  on.exit(restore(), add = TRUE)
  called <- FALSE
  transport <- function(request, config) {
    called <<- TRUE
    stop("must not run")
  }
  expect_error(
    ena3d_qwen_interpret(.qwen_evidence(), transport = transport),
    class = "ena3d_qwen_disabled_error"
  )
  expect_false(called)
})


test_that("the public request-envelope helper is exact, pure, and deterministic", {
  evidence <- .qwen_evidence()
  envelope <- ena3d_qwen_request_envelope(
    evidence,
    mode = "challenge",
    language = "Chinese",
    research_context = "Aggregate context only."
  )
  expect_named(
    envelope,
    c("task", "evidence", "optional_research_context"),
    ignore.order = FALSE
  )
  expect_named(
    envelope$task,
    c("mode", "output_language", "instructions"),
    ignore.order = FALSE
  )
  expect_identical(envelope$task$mode, "challenge")
  expect_identical(envelope$task$output_language, "Chinese")
  expect_identical(envelope$evidence, evidence)
  expect_identical(
    envelope$optional_research_context,
    "Aggregate context only."
  )

  without_context <- ena3d_qwen_request_envelope(evidence, "quick", "English")
  expect_true("optional_research_context" %in% names(without_context))
  expect_null(without_context$optional_research_context)

  malformed <- evidence
  malformed$evidence[[1L]]$metrics <- NULL
  expect_error(
    ena3d_qwen_request_envelope(malformed, "quick", "English"),
    class = "ena3d_qwen_schema_error"
  )
})


test_that("interpret sends JSON mode and returns only validated output metadata", {
  credential <- "sk-SECRET-MUST-NEVER-LEAK"
  restore <- .qwen_with_env(list(
    ENA3D_AI_ENABLED = "true",
    DASHSCOPE_API_KEY = credential
  ))
  on.exit(restore(), add = TRUE)
  seen <- NULL
  fake_transport <- function(request, config) {
    seen <<- request
    .qwen_api_response()
  }

  result <- ena3d_qwen_interpret(
    .qwen_evidence(),
    mode = "quick",
    language = "English",
    research_context = "Aggregate instructional context only.",
    transport = fake_transport
  )
  expect_s3_class(result, "ena3d_qwen_result")
  expect_identical(result$interpretation$claims[[1L]]$evidence_ids, "E1")
  expect_identical(result$meta$model, "qwen3.7-max-2026-06-08")
  expect_identical(result$meta$usage$total_tokens, 180)
  expect_true(is.numeric(result$meta$latency_ms))
  expect_false(any(c("request", "prompt", "evidence", "content") %in% names(result)))

  payload <- jsonlite::parse_json(seen$body, simplifyVector = FALSE)
  expect_identical(payload$response_format$type, "json_object")
  expect_false(payload$enable_thinking)
  expect_null(payload$thinking_budget)
  expect_identical(payload$max_completion_tokens, 4096L)
  expect_null(payload$max_tokens)
  expect_false(payload$enable_search)
  expect_false(payload$stream)
  expect_match(payload$messages[[1L]]$content, "Return only one JSON object")
  expect_match(payload$messages[[2L]]$content, '"id":"E1"', fixed = TRUE)
  expect_match(seen$headers$Authorization, credential, fixed = TRUE)

  displayed <- paste(capture.output(str(result)), collapse = "\n")
  expect_false(grepl(credential, displayed, fixed = TRUE))
  expect_false(grepl("Aggregate instructional context", displayed, fixed = TRUE))
  expect_false(grepl("untrusted-server-model-name", displayed, fixed = TRUE))
})


test_that("deep and challenge requests enable thinking and retain the selected mode", {
  restore <- .qwen_with_env(list(
    ENA3D_AI_ENABLED = "true",
    DASHSCOPE_API_KEY = "sk-test-credential"
  ))
  on.exit(restore(), add = TRUE)

  for (mode in c("deep", "challenge")) {
    seen <- NULL
    ena3d_qwen_interpret(
      .qwen_evidence(),
      mode = mode,
      transport = function(request, config) {
        seen <<- request
        .qwen_api_response()
      }
    )
    payload <- jsonlite::parse_json(seen$body, simplifyVector = FALSE)
    expect_true(payload$enable_thinking)
    expect_identical(payload$thinking_budget, 1536L)
    expect_identical(payload$max_completion_tokens, 4096L)
    expect_null(payload$max_tokens)
    expect_match(payload$messages[[2L]]$content, paste0('"mode":"', mode, '"'),
                 fixed = TRUE)
  }
})


test_that("request and response bodies obey hard byte budgets", {
  restore <- .qwen_with_env(list(
    ENA3D_AI_ENABLED = "true",
    DASHSCOPE_API_KEY = "sk-test-credential",
    ENA3D_QWEN_MAX_REQUEST_BYTES = "4096",
    ENA3D_QWEN_MAX_RESPONSE_BYTES = "1024"
  ))
  on.exit(restore(), add = TRUE)

  huge_evidence <- .qwen_evidence()
  huge_evidence$aggregate_note <- paste(rep("x", 5000), collapse = "")
  expect_error(
    ena3d_qwen_interpret(
      huge_evidence,
      transport = function(request, config) .qwen_api_response()
    ),
    class = "ena3d_qwen_limit_error"
  )

  expect_error(
    ena3d_qwen_interpret(
      .qwen_evidence(),
      transport = function(request, config) list(
        status_code = 200L,
        headers = list(),
        content = as.raw(rep(65L, 1025L))
      )
    ),
    class = "ena3d_qwen_limit_error"
  )
})


test_that("the hostile interpretation schema is enforced", {
  valid <- .qwen_interpretation()
  evidence <- .qwen_evidence()
  normalized <- ena3d_qwen_validate_interpretation(valid, evidence)
  expect_identical(normalized$claims[[1L]]$strength, "strong")

  unknown <- .qwen_interpretation(evidence_id = "E999")
  expect_error(
    ena3d_qwen_validate_interpretation(unknown, evidence),
    class = "ena3d_qwen_schema_error"
  )

  bad_strength <- .qwen_interpretation(strength = "certain")
  expect_error(
    ena3d_qwen_validate_interpretation(bad_strength, evidence),
    class = "ena3d_qwen_schema_error"
  )

  extra <- .qwen_interpretation()
  extra$html <- "<script>alert(1)</script>"
  expect_error(
    ena3d_qwen_validate_interpretation(extra, evidence),
    class = "ena3d_qwen_schema_error"
  )

  controlled <- .qwen_interpretation()
  controlled$headline <- "unsafe\nheadline"
  expect_error(
    ena3d_qwen_validate_interpretation(controlled, evidence),
    class = "ena3d_qwen_schema_error"
  )

  empty_claims <- .qwen_interpretation()
  empty_claims$claims <- list()
  expect_error(
    ena3d_qwen_validate_interpretation(empty_claims, evidence),
    class = "ena3d_qwen_schema_error"
  )
})


test_that("numeric claims must occur in metrics of cited evidence", {
  evidence <- .qwen_evidence()

  numeric_headline <- .qwen_interpretation()
  numeric_headline$headline <- "The centroid is 0.25."
  expect_error(
    ena3d_qwen_validate_interpretation(numeric_headline, evidence),
    class = "ena3d_qwen_schema_error"
  )

  supported <- .qwen_interpretation()
  supported$claims[[1L]]$text <- paste(
    "The centroid is 0.25 across 12 units; the same value is 25%."
  )
  expect_no_error(ena3d_qwen_validate_interpretation(supported, evidence))

  scientific <- .qwen_interpretation()
  scientific$claims[[1L]]$text <- "The centroid is 2.5e-1."
  expect_no_error(ena3d_qwen_validate_interpretation(scientific, evidence))

  full_width <- .qwen_interpretation()
  full_width$claims[[1L]]$text <- "质心值为０．２５。"
  expect_no_error(ena3d_qwen_validate_interpretation(full_width, evidence))

  invented <- .qwen_interpretation()
  invented$claims[[1L]]$text <- "The centroid is 999."
  expect_error(
    ena3d_qwen_validate_interpretation(invented, evidence),
    class = "ena3d_qwen_schema_error"
  )

  wrong_citation <- .qwen_interpretation()
  wrong_citation$claims[[1L]]$text <- "The edge value is 0.71."
  expect_error(
    ena3d_qwen_validate_interpretation(wrong_citation, evidence),
    class = "ena3d_qwen_schema_error"
  )

  full_width_invented <- .qwen_interpretation()
  full_width_invented$claims[[1L]]$text <- "质心值为９９９。"
  expect_error(
    ena3d_qwen_validate_interpretation(full_width_invented, evidence),
    class = "ena3d_qwen_schema_error"
  )
})


test_that("causal assertions require the explicit causal-design marker", {
  evidence <- .qwen_evidence()
  causal <- .qwen_interpretation()
  causal$headline <- "The treatment caused the network change."
  causal$claims[[1L]]$text <- "The treatment caused the network change."
  expect_error(
    ena3d_qwen_validate_interpretation(causal, evidence),
    class = "ena3d_qwen_schema_error"
  )

  causal$headline <- "The network changed."
  expect_error(
    ena3d_qwen_validate_interpretation(causal, evidence),
    class = "ena3d_qwen_schema_error"
  )

  chinese <- .qwen_interpretation()
  chinese$claims[[1L]]$text <- "干预导致了网络变化。"
  expect_error(
    ena3d_qwen_validate_interpretation(chinese, evidence),
    class = "ena3d_qwen_schema_error"
  )

  caution <- .qwen_interpretation()
  caution$claims[[1L]]$text <- "The descriptive result does not establish causality."
  expect_no_error(ena3d_qwen_validate_interpretation(caution, evidence))

  negation_evasion <- .qwen_interpretation()
  negation_evasion$claims[[1L]]$text <- "The treatment not only caused the change."
  expect_error(
    ena3d_qwen_validate_interpretation(negation_evasion, evidence),
    class = "ena3d_qwen_schema_error"
  )

  evidence$context$causal_design <- TRUE
  expect_no_error(ena3d_qwen_validate_interpretation(causal, evidence))
  expect_no_error(ena3d_qwen_validate_interpretation(chinese, evidence))

  nested_marker <- .qwen_evidence()
  nested_marker$evidence[[1L]]$metrics$causal_design <- TRUE
  expect_error(
    ena3d_qwen_validate_interpretation(causal, nested_marker),
    class = "ena3d_qwen_schema_error"
  )
})


test_that("HTTP failures expose safe classes but never provider response bodies", {
  credential <- "sk-SECRET-MUST-NOT-APPEAR"
  restore <- .qwen_with_env(list(
    ENA3D_AI_ENABLED = "true",
    DASHSCOPE_API_KEY = credential
  ))
  on.exit(restore(), add = TRUE)
  provider_body <- paste("provider echoed", credential, "and private prompt")

  error <- tryCatch(
    ena3d_qwen_interpret(
      .qwen_evidence(),
      transport = function(request, config) list(
        status_code = 429L,
        headers = list(`x-request-id` = "safe-request-id"),
        content = charToRaw(provider_body)
      )
    ),
    error = identity
  )
  expect_s3_class(error, "ena3d_qwen_rate_limit_error")
  expect_true(error$retryable)
  expect_identical(error$request_id, "safe-request-id")
  expect_false(grepl(credential, conditionMessage(error), fixed = TRUE))
  expect_false(grepl("private prompt", conditionMessage(error), fixed = TRUE))
})


test_that("malformed or incomplete API completions are rejected", {
  restore <- .qwen_with_env(list(
    ENA3D_AI_ENABLED = "true",
    DASHSCOPE_API_KEY = "sk-test-credential"
  ))
  on.exit(restore(), add = TRUE)

  expect_error(
    ena3d_qwen_interpret(
      .qwen_evidence(),
      transport = function(request, config) list(
        status_code = 200L,
        headers = list(),
        content = charToRaw("not json")
      )
    ),
    class = "ena3d_qwen_response_error"
  )

  expect_error(
    ena3d_qwen_interpret(
      .qwen_evidence(),
      transport = function(request, config) {
        .qwen_api_response(finish_reason = "length")
      }
    ),
    class = "ena3d_qwen_response_error"
  )

  malformed_interpretation <- .qwen_interpretation()
  malformed_interpretation$claims[[1L]]$evidence_ids <- list("E404")
  expect_error(
    ena3d_qwen_interpret(
      .qwen_evidence(),
      transport = function(request, config) {
        .qwen_api_response(malformed_interpretation)
      }
    ),
    class = "ena3d_qwen_schema_error"
  )
})


test_that("curl transport refuses a URL outside its validated configuration", {
  restore <- .qwen_with_env(list(
    ENA3D_AI_ENABLED = "true",
    DASHSCOPE_API_KEY = "sk-test-credential"
  ))
  on.exit(restore(), add = TRUE)
  config <- ena3d_qwen_config_from_env()
  request <- list(
    url = "https://attacker.example/chat/completions",
    headers = list(Authorization = "Bearer redacted"),
    body = "{}",
    timeout_seconds = 5,
    connect_timeout_seconds = 1,
    max_response_bytes = 1024
  )
  expect_error(
    ena3d_qwen_curl_transport(request, config),
    class = "ena3d_qwen_config_error"
  )
})
