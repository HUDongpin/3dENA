library(testthat)
library(shiny)

.security_test_root <- c(".", "../..", "..")
.security_test_root <- .security_test_root[file.exists(
  file.path(.security_test_root, "R", "security_utils.R")
)][1L]
if (is.na(.security_test_root)) stop("Could not locate the project root.")
.security_test_root <- normalizePath(.security_test_root, mustWork = TRUE)

source(file.path(.security_test_root, "R", "security_utils.R"), local = FALSE)
source(file.path(.security_test_root, "R", "app_utils.R"), local = FALSE)
source(file.path(.security_test_root, "R", "app_module_load_dataset.R"), local = FALSE)
source(file.path(.security_test_root, "R", "app_module_upload_data.R"), local = FALSE)
source(file.path(.security_test_root, "R", "app_ui_data_upload_tab.R"), local = FALSE)

.security_fixture <- file.path(
  .security_test_root, "sample_data", "sample_enaset.Rdata"
)

.security_save_rdata <- function(values) {
  path <- tempfile(fileext = ".RData")
  env <- list2env(values, parent = emptyenv())
  save(list = names(values), file = path, envir = env)
  path
}


test_that("browser-originated R serialization is denied before deserialization", {
  old_option <- getOption("ena3d.active_binding_executed")
  on.exit(options(ena3d.active_binding_executed = old_option), add = TRUE)
  options(ena3d.active_binding_executed = FALSE)

  active_object <- new.env(parent = emptyenv())
  class(active_object) <- "ena.set"
  makeActiveBinding(
    "points",
    function(value) {
      base::options(ena3d.active_binding_executed = TRUE)
      NULL
    },
    active_object
  )
  path <- .security_save_rdata(list(malicious = active_object))
  on.exit(unlink(path), add = TRUE)

  expect_error(
    ena3d_read_ena_object(path),
    "Native R uploads are disabled",
    fixed = TRUE
  )
  expect_false(isTRUE(getOption("ena3d.active_binding_executed")))
})


test_that("only direct children of the trusted sample directory resolve", {
  sample_root <- file.path(.security_test_root, "sample_data")
  allowed <- ena3d_resolve_trusted_sample(sample_root, basename(.security_fixture))
  expect_identical(allowed, normalizePath(.security_fixture))
  expect_setequal(
    ena3d_list_trusted_samples(sample_root),
    c("newfrat_enaset.Rdata", "sample_enaset.Rdata", "student_enaset.RData")
  )

  expect_error(
    ena3d_resolve_trusted_sample(sample_root, "../R/app.R"),
    "invalid"
  )
  expect_error(
    ena3d_resolve_trusted_sample(sample_root, normalizePath(.security_fixture)),
    "invalid"
  )
  expect_error(
    ena3d_resolve_trusted_sample(sample_root, "missing.RData"),
    "does not exist"
  )
})


test_that("trusted serialized fixtures obey hard file and object budgets", {
  limits <- ena3d_data_limits()
  limits$max_file_bytes <- 1
  expect_error(
    ena3d_read_ena_object(
      .security_fixture, source_kind = "bundled", limits = limits
    ),
    "file size exceeds"
  )

  ena <- ena3d_read_ena_object(.security_fixture, source_kind = "bundled")
  limits <- ena3d_data_limits()
  limits$max_point_rows <- 3
  expect_error(
    ena3d_validate_ena_object(ena, limits = limits),
    "point row count exceeds"
  )

  limits <- ena3d_data_limits()
  limits$max_nodes <- 11
  expect_error(
    ena3d_validate_ena_object(ena, limits = limits),
    "node count exceeds"
  )

  limits <- ena3d_data_limits()
  limits$max_table_cells <- 10
  expect_error(
    ena3d_validate_ena_object(ena, limits = limits),
    "total table cell count exceeds"
  )
})


test_that("the public data UI exposes safe raw and exchange inputs", {
  html <- htmltools::renderTags(data_upload_ui("main_app"))$html
  expect_false(grepl("ena_data_file", html, fixed = TRUE))
  expect_equal(
    lengths(regmatches(html, gregexpr('type="file"', html, fixed = TRUE))),
    2L
  )
  expect_match(html, 'accept=".csv,.xlsx,.xls"', fixed = TRUE)
  expect_match(html, 'accept=".ena3d.json"', fixed = TRUE)
  expect_false(grepl('accept="[^"]*application/json', html))
  expect_false(grepl("data-security-notice", html, fixed = TRUE))
  expect_false(grepl(
    "Raw Excel/CSV and safe JSON exchange are supported",
    html,
    fixed = TRUE
  ))
  expect_false(grepl("Do not send identifiable research data", html, fixed = TRUE))
  expect_match(html, "main_app-sample_data", fixed = TRUE)
})


test_that("a forged upload input cannot replace active state", {
  testServer(
    function(input, output, session) {
      rv <- reactiveValues(dataset_id = "known-good", initialized = TRUE)
      state <- new.env(parent = emptyenv())
      state$ena_obj <- list(known = "good")
      state$is_app_initialized <- TRUE
      session$userData$rv <- rv
      session$userData$state <- state
      upload_data(input, output, session, rv, state)
    },
    {
      session$setInputs(ena_data_file = list(
        name = "attacker.RData",
        datapath = tempfile(fileext = ".RData")
      ))
      session$flushReact()
      expect_identical(session$userData$rv$dataset_id, "known-good")
      expect_identical(session$userData$state$ena_obj, list(known = "good"))
      expect_true(session$userData$state$is_app_initialized)
    }
  )
})


test_that("ordinary JSON and native-R suffixes fail the server upload guard", {
  path <- tempfile()
  writeLines("{}", path)
  on.exit(unlink(path), add = TRUE)

  expect_error(
    ena3d_resolve_exchange_upload(list(name = "ordinary.json", datapath = path)),
    "Only files ending in .ena3d.json"
  )
  expect_error(
    ena3d_resolve_exchange_upload(list(name = "native.RData", datapath = path)),
    "Only files ending in .ena3d.json"
  )
})


test_that("security logs are one-line structured records", {
  expect_message(
    line <- ena3d_security_log(
      "test_event",
      fields = list(reason = "line one\nline two", count = 2L)
    ),
    "event=test_event"
  )
  expect_match(line, "reason=line_one_line_two", fixed = TRUE)
  expect_match(line, "count=2", fixed = TRUE)
  expect_false(grepl("\n", line, fixed = TRUE))
})
