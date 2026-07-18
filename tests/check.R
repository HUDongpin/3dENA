file_args <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
script_file <- file_args[file.exists(file_args)][1L]
if (is.na(script_file)) {
  project_root <- normalizePath(getwd(), mustWork = TRUE)
} else {
  project_root <- normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
}

if (!file.exists(file.path(project_root, "R", "app.R"))) {
  stop("Could not locate the ENA 3D project root.", call. = FALSE)
}

source_roots <- file.path(project_root, c("R", "tests", "tools"))
r_files <- unlist(lapply(source_roots, function(root) {
  list.files(
    root,
    pattern = "\\.[Rr]$",
    recursive = TRUE,
    full.names = TRUE
  )
}), use.names = FALSE)
renv_sources <- file.path(project_root, "renv", c("activate.R", "bootstrap.R"))
r_files <- sort(unique(c(r_files, renv_sources[file.exists(renv_sources)])))
parse_errors <- lapply(r_files, function(path) {
  tryCatch({
    parse(file = path)
    NULL
  }, error = function(error) {
    sprintf("%s: %s", path, conditionMessage(error))
  })
})
parse_errors <- Filter(Negate(is.null), parse_errors)
if (length(parse_errors)) {
  stop(
    paste(c("R source parsing failed:", unlist(parse_errors)), collapse = "\n"),
    call. = FALSE
  )
}
message(sprintf("Parsed %d R source/test files successfully.", length(r_files)))

old_wd <- setwd(project_root)
on.exit(setwd(old_wd), add = TRUE)
source(file.path(project_root, "tests", "testthat.R"), local = new.env(parent = globalenv()))
