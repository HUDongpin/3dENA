library(testthat)

.csv_security_root <- c(".", "../..", "..")
.csv_security_root <- .csv_security_root[file.exists(
  file.path(.csv_security_root, "R", "security_utils.R")
)][1L]
if (is.na(.csv_security_root)) stop("Could not locate the project root.")
.csv_security_root <- normalizePath(.csv_security_root, mustWork = TRUE)

source(
  file.path(.csv_security_root, "R", "security_utils.R"),
  local = FALSE
)


test_that("spreadsheet formula escaping covers text and collision-safe headers", {
  expect_identical(
    ena3d_spreadsheet_safe_text(c(
      "=1+1", " +SUM(A1:A2)", "\t-cmd", "\n@user", "ordinary", NA
    )),
    c("'=1+1", "' +SUM(A1:A2)", "'\t-cmd", "'\n@user", "ordinary", NA)
  )

  headers <- c("=formula", "'=formula", "''=formula", " safe", "ordinary")
  expect_identical(
    ena3d_spreadsheet_safe_headers(headers),
    c("'=formula", "''=formula", "'''=formula", " safe", "ordinary")
  )
  expect_false(anyDuplicated(ena3d_spreadsheet_safe_headers(headers)) > 0L)
})


test_that("safe CSV frames protect headers without mutating their source", {
  source_frame <- data.frame(
    "=1+1" = c("@SUM(A1:A2)", "ordinary"),
    "'=1+1" = factor(c("-cmd", "safe")),
    numeric_value = c(-2, 2),
    check.names = FALSE
  )
  original <- source_frame

  escaped <- ena3d_spreadsheet_safe_frame(source_frame)

  expect_identical(names(escaped), c("'=1+1", "''=1+1", "numeric_value"))
  expect_identical(escaped[[1L]], c("'@SUM(A1:A2)", "ordinary"))
  expect_identical(escaped[[2L]], c("'-cmd", "safe"))
  expect_identical(escaped$numeric_value, c(-2, 2))
  expect_identical(source_frame, original)
})


test_that("safe CSV writer serializes inert header and cell text", {
  exported <- data.frame(
    "=1+1" = c("+SUM(A1:A2)", "ordinary"),
    value = c(-1, 1),
    check.names = FALSE
  )
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)

  ena3d_write_safe_csv(exported, path)

  round_tripped <- utils::read.csv(
    path,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = character(0)
  )
  expect_identical(names(round_tripped), c("'=1+1", "value"))
  expect_identical(round_tripped[[1L]], c("'+SUM(A1:A2)", "ordinary"))
  expect_identical(round_tripped$value, c(-1L, 1L))
})
