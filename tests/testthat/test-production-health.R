library(testthat)

.health_test_root <- c(".", "../..", "..")
.health_test_root <- .health_test_root[file.exists(
  file.path(.health_test_root, "R", "app.R")
)][1L]
if (is.na(.health_test_root)) stop("Could not locate the project root.")
.health_test_root <- normalizePath(.health_test_root, mustWork = TRUE)


test_that("the versioned JSON health endpoint is reachable over HTTP", {
  skip_if_not_installed("processx")
  skip_if_not_installed("curl")
  skip_if_not_installed("httpuv")

  port <- httpuv::randomPort()
  app_dir <- normalizePath(file.path(.health_test_root, "R"), mustWork = TRUE)
  expression <- sprintf(
    "shiny::runApp(%s, host='127.0.0.1', port=%dL, launch.browser=FALSE)",
    encodeString(app_dir, quote = '"'),
    port
  )
  process <- processx::process$new(
    file.path(R.home("bin"), "Rscript"),
    c("-e", expression),
    env = c(
      ENA3D_BUILD_ID = "health-smoke",
      ENA3D_APP_VERSION = "0.2.0-test"
    ),
    stdout = "|",
    stderr = "|",
    cleanup_tree = TRUE
  )
  on.exit({
    if (process$is_alive()) process$kill()
  }, add = TRUE)

  url <- sprintf(
    "http://127.0.0.1:%d/ena3d-health/healthz.json",
    port
  )
  response <- NULL
  deadline <- Sys.time() + 15
  repeat {
    response <- tryCatch(
      curl::curl_fetch_memory(url, handle = curl::new_handle(timeout = 1)),
      error = function(error) NULL
    )
    if (!is.null(response) && identical(response$status_code, 200L)) break
    if (!process$is_alive()) {
      stop(
        "Shiny exited before the health endpoint became ready:\n",
        paste(process$read_all_error(), collapse = "\n")
      )
    }
    if (Sys.time() >= deadline) {
      stop("Timed out waiting for the JSON health endpoint.")
    }
    Sys.sleep(0.1)
  }

  health <- jsonlite::fromJSON(rawToChar(response$content))
  expect_identical(health$status, "ok")
  expect_identical(health$app, "ENA 3D")
  expect_identical(health$version, "0.2.0-test")
  expect_identical(health$build, "health-smoke")
  expect_gte(health$trusted_samples, 1L)
})
