library(testthat)

.health_test_root <- c(".", "../..", "..")
.health_test_root <- .health_test_root[file.exists(
  file.path(.health_test_root, "R", "app.R")
)][1L]
if (is.na(.health_test_root)) stop("Could not locate the project root.")
.health_test_root <- normalizePath(.health_test_root, mustWork = TRUE)


test_that("the JSON health endpoint reports authoritative Vercel provenance", {
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
    wd = .health_test_root,
    env = c(
      ENA3D_BUILD_ID = "stale-health-smoke",
      ENA3D_APP_VERSION = "0.2.0-test",
      VERCEL = "1",
      VERCEL_GIT_COMMIT_SHA =
        "0123456789abcdef0123456789abcdef01234567"
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
  expect_identical(health$app, "3D ENA")
  expect_identical(health$version, "0.2.0-test")
  expect_identical(
    health$build,
    "0123456789abcdef0123456789abcdef01234567"
  )
  expect_gte(health$trusted_samples, 1L)
})


test_that("the cold Vercel shell serves Papers before loading analysis", {
  skip_if_not_installed("processx")
  skip_if_not_installed("curl")
  skip_if_not_installed("httpuv")

  build <- "89abcdef0123456789abcdef0123456789abcdef"
  prebuilt <- tempfile("ena3d-cold-shell-", fileext = ".html")
  writeChar(
    paste0(
      "<!doctype html><html><head><title>Papers | 3D ENA</title></head>",
      "<body data-build=\"__ENA3D_BUILD_ID__\">Cold shell</body>",
      "<script src=\"plotly-main-2.11.1/plotly-latest.min.js\"></script>",
      "</html>"
    ),
    prebuilt,
    eos = NULL,
    useBytes = TRUE
  )
  on.exit(unlink(prebuilt), add = TRUE)

  port <- httpuv::randomPort()
  expression <- sprintf(
    paste0(
      "source('R/app.R'); ",
      "stopifnot(!isTRUE(.ena3d_analysis_runtime_loaded)); ",
      "stopifnot(!('plotly' %%in%% loadedNamespaces())); ",
      "shiny::runApp(ena3d_app, host='127.0.0.1', port=%dL, ",
      "launch.browser=FALSE)"
    ),
    port
  )
  process <- processx::process$new(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "-e", expression),
    wd = .health_test_root,
    env = c(
      VERCEL = "1",
      VERCEL_GIT_COMMIT_SHA = build,
      ENA3D_BUILD_ID = "stale-cold-shell",
      ENA3D_APP_VERSION = "0.2.0-test",
      ENA3D_INLINE_ASSETS = "true",
      ENA3D_PREBUILT_UI_PATH = prebuilt,
      ENA3D_RUNTIME_PROFILE = "ephemeral-preview"
    ),
    stdout = "|",
    stderr = "|",
    cleanup_tree = TRUE
  )
  on.exit({
    if (process$is_alive()) process$kill()
  }, add = TRUE)

  started <- Sys.time()
  url <- sprintf("http://127.0.0.1:%d/papers?cold_probe=test", port)
  response <- NULL
  deadline <- started + 8
  repeat {
    response <- tryCatch(
      curl::curl_fetch_memory(
        url,
        handle = curl::new_handle(timeout = 1, followlocation = FALSE)
      ),
      error = function(error) NULL
    )
    if (!is.null(response)) break
    if (!process$is_alive()) {
      stop(
        "Shiny exited before the cold Papers request was accepted:\n",
        paste(process$read_all_error(), collapse = "\n")
      )
    }
    if (Sys.time() >= deadline) {
      stop("Cold shell did not accept the first direct Papers request in 8s.")
    }
    Sys.sleep(0.05)
  }

  expect_identical(response$status_code, 200L)
  expect_false(grepl("(?im)^location:", rawToChar(response$headers), perl = TRUE))
  expect_match(rawToChar(response$content), build, fixed = TRUE)
  expect_lt(as.numeric(difftime(Sys.time(), started, units = "secs")), 8)

  plotly_response <- curl::curl_fetch_memory(sprintf(
    "http://127.0.0.1:%d/plotly-main-2.11.1/plotly-latest.min.js",
    port
  ))
  expect_identical(plotly_response$status_code, 200L)
  expect_gt(length(plotly_response$content), 1024^2)
})


test_that("invalid optional AI budgets fail closed without stopping ENA", {
  skip_if_not_installed("processx")

  result <- processx::run(
    file.path(R.home("bin"), "Rscript"),
    c("-e", "source('R/app.R'); stopifnot(!isTRUE(config$ai$available))"),
    wd = .health_test_root,
    env = c(
      ENA3D_BUILD_ID = "health-ai-fail-closed",
      ENA3D_APP_VERSION = "0.2.0-test",
      ENA3D_AI_ENABLED = "false",
      ENA3D_AI_MIN_CELL_N = "not-a-number"
    ),
    error_on_status = FALSE,
    echo = FALSE,
    timeout = 30
  )

  expect_identical(result$status, 0L, info = result$stderr)
})
