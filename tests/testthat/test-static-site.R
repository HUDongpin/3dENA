library(testthat)

.static_site_roots <- c(".", "..", "../..")
.static_site_root <- .static_site_roots[file.exists(
  file.path(.static_site_roots, "R", "static_site.R")
)][1L]
if (is.na(.static_site_root)) stop("Could not locate the 3D ENA project root.")
.static_site_root <- normalizePath(.static_site_root)
options(sass.cache = file.path(tempdir(), "ena3d-static-site-sass"))

if (!requireNamespace("shiny", quietly = TRUE) ||
    !requireNamespace("htmltools", quietly = TRUE) ||
    !requireNamespace("bslib", quietly = TRUE)) {
  skip("Static-site tests require shiny, htmltools, and bslib.")
}

.static_site_env <- new.env(parent = globalenv())
.static_site_env$tags <- shiny::tags
.static_site_env$tagList <- htmltools::tagList
sys.source(
  file.path(.static_site_root, "R", "inline_ui.R"),
  envir = .static_site_env
)
sys.source(
  file.path(.static_site_root, "R", "app_ui_site.R"),
  envir = .static_site_env
)
sys.source(
  file.path(.static_site_root, "R", "site_routes.R"),
  envir = .static_site_env
)
sys.source(
  file.path(.static_site_root, "R", "static_site.R"),
  envir = .static_site_env
)

.static_site_config <- list(
  app_version = "0.2.0-test",
  build_id = "0123456789abcdef0123456789abcdef01234567"
)

test_that("the public content document has no Shiny session or connection guard", {
  html <- .static_site_env$ena3d_render_static_site(
    .static_site_config,
    file.path(.static_site_root, "R", "www"),
    inline_assets = FALSE
  )

  expect_match(html, 'data-site-mode="static"', fixed = TRUE)
  expect_match(html, 'name="ena3d-build"', fixed = TRUE)
  expect_match(html, .static_site_config$build_id, fixed = TRUE)
  expect_match(html, 'src="/site_routes.js?v=', fixed = TRUE)
  expect_match(html, 'href="/app"', fixed = TRUE)
  expect_match(html, 'href="/papers"', fixed = TRUE)
  expect_match(html, 'href="/team"', fixed = TRUE)
  expect_match(html, 'href="/about"', fixed = TRUE)
  expect_match(html, "Make epistemic connections", fixed = TRUE)
  expect_match(html, "Cite the work behind 3D ENA.", fixed = TRUE)
  expect_match(html, "Meet the team.", fixed = TRUE)
  expect_match(html, "Dr. Peter Hu Dongpin", fixed = TRUE)

  for (forbidden in c(
    "shiny-javascript",
    "shiny-css",
    "shiny.min.js",
    "connection_guard.js",
    "ena3d-connection-guard",
    "shiny:disconnected",
    "Shiny.addCustomMessageHandler",
    "__sockjs__",
    "plotly-latest.min.js"
  )) {
    expect_false(grepl(forbidden, html, fixed = TRUE), info = forbidden)
  }
})

test_that("the self-contained static document omits the optional web-font import", {
  html <- .static_site_env$ena3d_render_static_site(
    .static_site_config,
    file.path(.static_site_root, "R", "www"),
    inline_assets = TRUE
  )

  expect_match(
    html,
    "/* optional web-font import omitted for the prebuilt shell */",
    fixed = TRUE
  )
  expect_false(grepl('@import url("data:text/css', html, fixed = TRUE))
  expect_false(grepl("/url(%22data:text/css", html, fixed = TRUE))
  expect_false(grepl("url(fonts/", html, fixed = TRUE))
  expect_lt(nchar(html, type = "bytes"), 4 * 1024^2)
})

test_that("static calls to action are real shareable workspace links", {
  html <- .static_site_env$ena3d_render_static_site(
    .static_site_config,
    file.path(.static_site_root, "R", "www"),
    inline_assets = FALSE
  )

  expect_match(
    html,
    '<a id="launch_ena" href="/app"',
    fixed = TRUE
  )
  expect_match(
    html,
    '<a id="explore_trajectory" href="/app?workspace=trajectory"',
    fixed = TRUE
  )
  expect_match(
    html,
    '<a id="launch_ena_note" href="/app"',
    fixed = TRUE
  )
  expect_match(
    html,
    '<a id="launch_ena_about" href="/app"',
    fixed = TRUE
  )
  expect_false(grepl('<button id="launch_ena"', html, fixed = TRUE))
  expect_false(grepl('<button id="explore_trajectory"', html, fixed = TRUE))
})

test_that("the static response is no-store HTML", {
  handler <- .static_site_env$ena3d_static_site_handler("<!doctype html>static")
  response <- handler(list(PATH_INFO = "/team", REQUEST_METHOD = "GET"))

  expect_s3_class(response, "httpResponse")
  expect_identical(response$status, 200L)
  expect_identical(response$content_type, "text/html; charset=UTF-8")
  expect_identical(response$headers[["Cache-Control"]], "no-store")
  expect_identical(response$content, "<!doctype html>static")

  head_response <- handler(list(
    PATH_INFO = "/team",
    REQUEST_METHOD = "HEAD"
  ))
  expect_identical(head_response$status, 200L)
  expect_identical(head_response$content, "")

  post_response <- handler(list(
    PATH_INFO = "/team",
    REQUEST_METHOD = "POST"
  ))
  expect_identical(post_response$status, 405L)
  expect_identical(post_response$headers$Allow, "GET, HEAD")
  expect_identical(post_response$content, "Method Not Allowed")
})
