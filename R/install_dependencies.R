required_packages <- c(
  "shiny",
  "plotly",
  "data.table",
  "R6",
  "rENA",
  "bslib",
  "scales",
  "digest",
  "jsonlite",
  "readxl",
  "zip",
  "callr",
  "later",
  "promises"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    sprintf(
      "Missing required packages: %s. Install them before starting ENA 3D; the app no longer modifies the R library at startup.",
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}
