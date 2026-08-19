library(testthat)

.connection_test_roots <- c(".", "..", "../..")
.connection_test_root <- .connection_test_roots[file.exists(
  file.path(.connection_test_roots, "R", "app_connection.R")
)][1L]
if (is.na(.connection_test_root)) {
  stop("Could not locate the 3D ENA project root.")
}
.connection_test_root <- normalizePath(.connection_test_root, mustWork = TRUE)

if (!requireNamespace("shiny", quietly = TRUE) ||
    !requireNamespace("htmltools", quietly = TRUE)) {
  skip("Connection-recovery tests require shiny and htmltools.")
}

.connection_test_env <- new.env(parent = globalenv())
sys.source(
  file.path(.connection_test_root, "R", "app_connection.R"),
  envir = .connection_test_env
)


test_that("runtime and connection policy fail closed", {
  expect_identical(
    .connection_test_env$ena3d_runtime_profile("persistent"),
    "persistent"
  )
  expect_identical(
    .connection_test_env$ena3d_runtime_profile(" EPHEMERAL-PREVIEW "),
    "ephemeral-preview"
  )
  expect_error(
    .connection_test_env$ena3d_runtime_profile("serverless-production"),
    "ENA3D_RUNTIME_PROFILE must be one of"
  )

  expect_identical(
    .connection_test_env$ena3d_connection_policy("persistent"),
    "host-existing-session-only"
  )
  expect_identical(
    .connection_test_env$ena3d_connection_policy("development"),
    "reload-required"
  )
  expect_identical(
    .connection_test_env$ena3d_connection_policy("ephemeral-preview"),
    "reload-required"
  )
  expect_error(
    .connection_test_env$ena3d_validate_runtime_host(
      "persistent",
      "1.5.23.1030",
      server_info = list(shinyServer = FALSE, version = "1.5.23.1030")
    ),
    "requires an actual Posit Shiny Server host"
  )
  expect_error(
    .connection_test_env$ena3d_validate_runtime_host(
      "persistent",
      "1.5.23.1030",
      server_info = list(shinyServer = TRUE)
    ),
    "requires version metadata from shiny::serverInfo"
  )
  expect_error(
    .connection_test_env$ena3d_validate_runtime_host(
      "persistent",
      "",
      server_info = list(shinyServer = TRUE, version = "1.5.23.1030")
    ),
    "requires Shiny Server version metadata"
  )
  expect_error(
    .connection_test_env$ena3d_validate_runtime_host(
      "persistent",
      "1.5.23.1030",
      server_info = list(shinyServer = TRUE, version = "9.9.9-forged")
    ),
    "host version metadata is inconsistent"
  )
  expect_identical(
    .connection_test_env$ena3d_validate_runtime_host(
      "persistent",
      "1.5.23.1030",
      server_info = list(shinyServer = TRUE, version = "1.5.23.1030")
    ),
    "1.5.23.1030"
  )
})


test_that("environment metadata alone cannot satisfy the adapter guard", {
  previous_version <- Sys.getenv("SHINY_SERVER_VERSION", unset = NA_character_)
  on.exit({
    if (is.na(previous_version)) {
      Sys.unsetenv("SHINY_SERVER_VERSION")
    } else {
      Sys.setenv(SHINY_SERVER_VERSION = previous_version)
    }
  }, add = TRUE)
  Sys.setenv(SHINY_SERVER_VERSION = "1.5.23.1030")

  expect_false(isTRUE(shiny::serverInfo()$shinyServer))
  expect_error(
    .connection_test_env$ena3d_validate_runtime_host("persistent"),
    "requires an actual Posit Shiny Server host"
  )
})


test_that("application-level new-session reconnection is always disabled", {
  received <- NULL
  session <- new.env(parent = emptyenv())
  session$allowReconnect <- function(value) {
    received <<- value
  }

  expect_false(
    .connection_test_env$ena3d_disable_new_session_reconnect(session)
  )
  expect_false(received)
})


test_that("session proof accepts only bounded scalar nonces", {
  valid <- list(nonce = "connection-proof_2026-07-29:01")
  expect_identical(
    .connection_test_env$ena3d_connection_probe_nonce(valid),
    valid$nonce
  )
  expect_null(.connection_test_env$ena3d_connection_probe_nonce(NULL))
  expect_null(.connection_test_env$ena3d_connection_probe_nonce(list(nonce = 1)))
  expect_null(.connection_test_env$ena3d_connection_probe_nonce(
    list(nonce = c("one", "two"))
  ))
  expect_null(.connection_test_env$ena3d_connection_probe_nonce(
    list(nonce = "contains a space")
  ))
  expect_null(.connection_test_env$ena3d_connection_probe_nonce(
    list(nonce = paste(rep("a", 129L), collapse = ""))
  ))

  session <- new.env(parent = emptyenv())
  session$token <- "browser-bearer-token-must-not-be-returned"
  proof_id <- .connection_test_env$ena3d_session_proof_id(session)
  expect_match(proof_id, "^[a-f0-9]{64}$")
  expect_false(identical(proof_id, session$token))
  expect_identical(
    .connection_test_env$ena3d_connection_proof_payload(valid, proof_id),
    list(nonce = valid$nonce, session_id = proof_id)
  )
})


test_that("connection guard is blocking, accessible, and honest about state loss", {
  html <- htmltools::renderTags(
    .connection_test_env$ena3d_connection_guard_ui()
  )$html

  expect_match(html, 'id="ena3d-connection-guard"', fixed = TRUE)
  expect_match(html, 'data-state="connected"', fixed = TRUE)
  expect_match(html, 'data-session-proof="pending"', fixed = TRUE)
  expect_match(html, 'role="alertdialog"', fixed = TRUE)
  expect_match(html, 'aria-modal="true"', fixed = TRUE)
  expect_match(html, 'aria-live="assertive"', fixed = TRUE)
  expect_match(html, "Trying to reconnect", fixed = TRUE)
  expect_match(html, "A new session will never be presented", fixed = TRUE)
  expect_match(html, "uploads and calculations", fixed = TRUE)
  expect_match(html, 'id="ena3d-connection-reload"', fixed = TRUE)
  expect_match(html, "Reload page", fixed = TRUE)
  expect_match(html, 'id="ena3d-connection-live"', fixed = TRUE)
  expect_match(html, 'role="status"', fixed = TRUE)
})


test_that("connection assets keep Shiny's disconnect signal and add recovery UI", {
  css <- paste(
    readLines(
      file.path(.connection_test_root, "R", "www", "connection_guard.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  javascript <- paste(
    readLines(
      file.path(.connection_test_root, "R", "www", "connection_guard.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_false(grepl("#shiny-disconnected-overlay", css, fixed = TRUE))
  expect_match(css, "pointer", fixed = TRUE)
  expect_match(css, "z-index: 100100", fixed = TRUE)
  expect_match(javascript, "shiny:disconnected", fixed = TRUE)
  expect_match(javascript, "shiny:connected", fixed = TRUE)
  expect_match(javascript, "ena3d-session-proof", fixed = TRUE)
  expect_match(
    javascript,
    "proof.nonce !== pendingProof.nonce",
    fixed = TRUE
  )
  expect_match(javascript, "sessionId === expectedSessionId", fixed = TRUE)
  expect_match(javascript, 'data-session-proof", "ready"', fixed = TRUE)
  expect_match(javascript, "session-replaced", fixed = TRUE)
  expect_match(javascript, "proofRetryTimer", fixed = TRUE)
  expect_match(javascript, "timedOut.attempt + 1", fixed = TRUE)
  expect_match(javascript, "terminalState", fixed = TRUE)
  expect_match(
    javascript,
    "Array.from(inertState.entries())",
    fixed = TRUE
  )
  expect_match(javascript, "clearAnnouncement()", fixed = TRUE)
  expect_match(javascript, "ss-connect-dialog", fixed = TRUE)
  expect_match(javascript, "element.inert = true", fixed = TRUE)
  expect_match(javascript, "window.location.reload()", fixed = TRUE)
  expect_match(javascript, "Session connection ended", fixed = TRUE)
  expect_false(grepl("allowReconnect(\"force\")", javascript, fixed = TRUE))
})


test_that("production source never suppresses Shiny's native disconnect overlay", {
  production_files <- list.files(
    file.path(.connection_test_root, "R"),
    pattern = "\\.(R|js|css)$",
    recursive = TRUE,
    full.names = TRUE
  )
  production_source <- paste(
    unlist(lapply(production_files, readLines, warn = FALSE), use.names = FALSE),
    collapse = "\n"
  )

  expect_false(grepl(
    "shiny-disconnected-overlay",
    production_source,
    fixed = TRUE
  ))
})
