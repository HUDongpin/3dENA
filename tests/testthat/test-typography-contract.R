library(testthat)

.typography_roots <- c(".", "..", "../..")
.typography_root <- .typography_roots[file.exists(
  file.path(.typography_roots, "R", "www", "app_shell.css")
)][1L]
if (is.na(.typography_root)) stop("Could not locate the 3D ENA project root.")
.typography_root <- normalizePath(.typography_root, mustWork = TRUE)

.read_typography_file <- function(...) {
  paste(readLines(file.path(.typography_root, ...), warn = FALSE), collapse = "\n")
}


test_that("site typography uses a 16px root and semantic readable-size tokens", {
  css <- .read_typography_file("R", "www", "app_shell.css")
  app <- .read_typography_file("R", "app.R")

  expect_match(css, "font-size: 100%;", fixed = TRUE)
  expect_match(css, "--ena-type-meta: 0.8125rem;", fixed = TRUE)
  expect_match(css, "--ena-type-ui: 0.9375rem;", fixed = TRUE)
  expect_match(css, "--ena-type-body: 1rem;", fixed = TRUE)
  expect_match(css, "--ena-type-lede: 1.125rem;", fixed = TRUE)
  expect_false(grepl("font-size:0.9rem", app, fixed = TRUE))
  expect_false(grepl("font-size: 0.62rem", css, fixed = TRUE))
  expect_false(grepl("font-size: 0.68rem", css, fixed = TRUE))
  expect_false(grepl("font-size: 0.7rem", css, fixed = TRUE))
})


test_that("navigation, controls, metadata, and mobile inputs use the font scale", {
  css <- .read_typography_file("R", "www", "app_shell.css")
  app <- .read_typography_file("R", "app.R")
  stats <- .read_typography_file("R", "www", "app_ui_stats.css")

  expect_match(css, ".navbar-nav .nav-link", fixed = TRUE)
  expect_match(css, "font-size: var(--ena-type-ui);", fixed = TRUE)
  expect_match(css, ".ena3d-tool-page .form-control", fixed = TRUE)
  expect_match(css, "min-height: 2.75rem;", fixed = TRUE)
  expect_match(css, "font-size: var(--ena-type-body);", fixed = TRUE)
  expect_match(app, "font-size:var(--ena-type-meta)", fixed = TRUE)
  expect_match(app, "font-size:var(--ena-type-small)", fixed = TRUE)
  expect_match(stats, "font-size: 1rem;", fixed = TRUE)
})


test_that("workspace layout reserves readable control widths and stacks on tablets", {
  app <- .read_typography_file("R", "app.R")
  css <- .read_typography_file("R", "www", "app_shell.css")

  expect_match(app, "column(5,", fixed = TRUE)
  expect_match(app, "column(7,", fixed = TRUE)
  expect_match(app, "widths = c(3, 9)", fixed = TRUE)
  expect_match(app, "white-space:normal", fixed = TRUE)
  expect_match(app, ".plot-tool-bar #main_app-fullscreen_btn", fixed = TRUE)
  expect_match(app, "flex:1 1 27rem", fixed = TRUE)
  expect_match(css, "grid-template-columns: repeat(4, minmax(0, 1fr));", fixed = TRUE)
  expect_match(css, ".ena3d-main-layout > .ena3d-sidebar-column", fixed = TRUE)
})


test_that("interactive accent tokens meet WCAG AA contrast on light surfaces", {
  css <- .read_typography_file("R", "www", "app_shell.css")
  app <- .read_typography_file("R", "app.R")
  token <- function(name) {
    match <- regexec(
      paste0("--", name, ":[[:space:]]*(#[0-9a-fA-F]{6})"), css
    )
    captures <- regmatches(css, match)[[1L]]
    if (length(captures) != 2L) stop("Missing color token: ", name)
    captures[[2L]]
  }
  luminance <- function(color) {
    rgb <- grDevices::col2rgb(color)[, 1L] / 255
    rgb <- ifelse(
      rgb <= 0.04045,
      rgb / 12.92,
      ((rgb + 0.055) / 1.055)^2.4
    )
    sum(c(0.2126, 0.7152, 0.0722) * rgb)
  }
  contrast <- function(first, second) {
    values <- c(luminance(first), luminance(second))
    (max(values) + 0.05) / (min(values) + 0.05)
  }

  teal <- token("ena-cyan-dark")
  coral <- token("ena-coral")
  for (background in c("#f4f1e9", "#f0f2f5", "#f7f8fa")) {
    expect_gte(contrast(teal, background), 4.5)
    expect_gte(contrast(coral, background), 4.5)
  }
  expect_match(app, paste0('primary = "', teal, '"'), fixed = TRUE)
  expect_match(app, paste0('secondary = "', coral, '"'), fixed = TRUE)
})
