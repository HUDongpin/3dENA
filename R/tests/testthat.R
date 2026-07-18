# Compatibility entry for the historical app-local test location. The
# repository-level suite is authoritative and does not require shinytest2.
file_args <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
script_file <- file_args[file.exists(file_args)][1L]
if (is.na(script_file)) {
  stop("Run this compatibility entry with Rscript.", call. = FALSE)
}
project_root <- normalizePath(file.path(dirname(script_file), "..", ".."), mustWork = TRUE)
source(file.path(project_root, "tests", "testthat.R"), local = new.env(parent = globalenv()))
