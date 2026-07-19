library(testthat)

.analytics_test_roots <- c(".", "..", "../..")
.analytics_test_root <- .analytics_test_roots[file.exists(
  file.path(.analytics_test_roots, "R", "inline_ui.R")
)][1L]
if (is.na(.analytics_test_root)) stop("Could not locate the 3D ENA project root.")
.analytics_test_root <- normalizePath(.analytics_test_root, mustWork = TRUE)

if (!requireNamespace("shiny", quietly = TRUE) ||
    !requireNamespace("htmltools", quietly = TRUE)) {
  skip("Vercel Analytics tests require shiny and htmltools.")
}

.analytics_test_env <- new.env(parent = globalenv())
sys.source(
  file.path(.analytics_test_root, "R", "inline_ui.R"),
  envir = .analytics_test_env
)


test_that("Vercel Analytics renders the framework-independent bootstrap", {
  analytics <- htmltools::renderTags(
    .analytics_test_env$ena3d_vercel_analytics_tags()
  )$html

  expect_match(analytics, "window.va = window.va || function", fixed = TRUE)
  expect_match(analytics, "window.vaq = window.vaq || []", fixed = TRUE)
  expect_match(
    analytics,
    'src="/_vercel/insights/script.js"',
    fixed = TRUE
  )
  expect_match(analytics, "defer", fixed = TRUE)
  expect_match(
    analytics,
    'data-analytics-provider="vercel"',
    fixed = TRUE
  )
})


test_that("production asset inlining preserves the Vercel analytics route", {
  analytics <- htmltools::renderTags(
    .analytics_test_env$ena3d_vercel_analytics_tags()
  )$html
  page <- paste0("<html><head>", analytics, "</head><body></body></html>")
  inlined <- .analytics_test_env$ena3d_inline_ui_assets(
    page,
    www_dir = tempfile("ena3d-www-")
  )

  expect_match(
    inlined,
    'src="/_vercel/insights/script.js"',
    fixed = TRUE
  )
  expect_match(inlined, "window.va = window.va || function", fixed = TRUE)
})
