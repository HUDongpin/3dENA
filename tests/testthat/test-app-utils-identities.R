library(testthat)

.app_utils_identity_roots <- c(".", "../..", "..")
.app_utils_identity_root <- .app_utils_identity_roots[file.exists(
  file.path(.app_utils_identity_roots, "R", "app_utils.R")
)][1L]
if (is.na(.app_utils_identity_root)) {
  stop("Could not locate the 3D ENA project root.")
}
source(
  file.path(.app_utils_identity_root, "R", "app_utils.R"),
  local = FALSE
)


test_that("typed identity keys cover every accepted scalar family", {
  fold <- as.POSIXct(
    c(1730611800, 1730615400),
    origin = "1970-01-01",
    tz = "America/New_York"
  )
  families <- list(
    datetime = fold,
    date = as.Date(c("2026-01-01", "2026-01-02")),
    duration = as.difftime(c(1, 1 + .Machine$double.eps), units = "secs"),
    ordered = ordered(c("first", "second")),
    factor = factor(c("first", "second")),
    double = c(1, 1 + .Machine$double.eps),
    integer = c(1L, 2L),
    logical = c(FALSE, TRUE),
    character = c("", "<NA>"),
    fallback = as.raw(c(1L, 2L))
  )

  keys <- lapply(families, ena3d_value_identity_keys)
  expect_true(all(vapply(keys, function(value) {
    length(value) == 2L && length(unique(value)) == 2L &&
      all(as.integer(charToRaw(paste0(value, collapse = ""))) <= 127L)
  }, logical(1L))))
  expect_identical(
    keys,
    lapply(families, ena3d_value_identity_keys)
  )
  expect_length(unique(vapply(
    list(1, 1L, "1", factor("1")),
    function(value) ena3d_value_identity_keys(value)[[1L]],
    character(1L)
  )), 4L)
})


test_that("typed identity keys canonicalize missing and non-finite values", {
  values <- c(NA_real_, NaN, Inf, -Inf)
  keys <- ena3d_value_identity_keys(values)

  expect_length(unique(keys), 1L)
  expect_match(keys[[1L]], ":n$")
  expect_identical(
    ena3d_value_identity_keys(c(NA_character_, "<NA>"))[[1L]],
    ena3d_value_identity_keys(c(NA_character_, "<NA>"))[[1L]]
  )
  expect_false(identical(
    ena3d_value_identity_keys(c(NA_character_, "<NA>"))[[1L]],
    ena3d_value_identity_keys(c(NA_character_, "<NA>"))[[2L]]
  ))
})


test_that("plot colors reject invalid free text without breaking a plot", {
  expect_identical(ena3d_normalize_plot_color(" #A1b2C3 "), "#A1b2C3")
  expect_identical(ena3d_normalize_plot_color("steelblue"), "steelblue")
  expect_identical(
    ena3d_normalize_plot_color("not a colour", fallback = "#123456"),
    "#123456"
  )
  expect_identical(
    ena3d_normalize_plot_color("not a colour", fallback = "also invalid"),
    "#808080"
  )

  colors <- cbind(color = c("not a colour", "#654321"), group = c("A", "B"))
  expect_identical(get_group_color(colors, "group", "A"), "#808080")
  expect_identical(get_group_color(colors, "group", "B"), "#654321")
})
