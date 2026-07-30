# Shared helpers for the offline R static-analysis and coverage detectors.
#
# Reports produced with these helpers deliberately contain only project-relative
# paths, fixed configuration, package versions, counts, and sanitized detector
# messages. They never inspect dotenv files, credential files, or live services.

ena3d_audit_find_project_root <- function(starts = getwd()) {
  starts <- unique(normalizePath(starts, mustWork = FALSE))
  for (start in starts) {
    candidate <- start
    repeat {
      if (file.exists(file.path(candidate, "R", "app.R")) &&
          file.exists(file.path(candidate, "renv.lock")) &&
          file.exists(file.path(candidate, "tests", "check.R"))) {
        return(normalizePath(candidate, mustWork = TRUE))
      }
      parent <- dirname(candidate)
      if (identical(parent, candidate)) break
      candidate <- parent
    }
  }
  stop("Could not locate the 3D ENA project root.", call. = FALSE)
}


ena3d_audit_relative_path <- function(path, project_root) {
  root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root, "/")
  if (identical(normalized, root)) return(".")
  if (startsWith(normalized, prefix)) {
    return(substring(normalized, nchar(prefix) + 1L))
  }
  "<OUTSIDE_PROJECT>"
}


ena3d_audit_activate_project_library <- function(project_root) {
  configured <- Sys.getenv("RENV_PATHS_LIBRARY", unset = "")
  candidates <- c(
    if (nzchar(configured)) configured else character(),
    file.path(project_root, "renv", "library")
  )
  candidates <- unique(candidates[dir.exists(candidates)])
  if (length(candidates)) {
    .libPaths(c(normalizePath(candidates, mustWork = TRUE), .libPaths()))
  }
  invisible(.libPaths())
}


ena3d_audit_sanitize_text <- function(value, project_root, max_chars = 1000L) {
  if (is.null(value) || !length(value)) return("")
  value <- enc2utf8(paste(as.character(value), collapse = " "))
  root <- normalizePath(project_root, winslash = "/", mustWork = FALSE)
  value <- gsub(root, "<PROJECT_ROOT>", value, fixed = TRUE)
  temporary <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
  value <- gsub(temporary, "<TEMP_DIR>", value, fixed = TRUE)
  value <- gsub(
    paste0(
      "(?i)(api[ _-]?key|access[ _-]?token|password|secret)",
      "[[:space:]]*[:=][[:space:]]*[^[:space:]]+"
    ),
    "\\1=<REDACTED>",
    value,
    perl = TRUE
  )
  value <- gsub("[[:cntrl:]]+", " ", value, perl = TRUE)
  value <- gsub("[[:space:]]+", " ", trimws(value), perl = TRUE)
  substr(value, 1L, as.integer(max_chars))
}


ena3d_audit_parse_cli <- function(arguments, defaults) {
  result <- defaults
  index <- 1L
  while (index <= length(arguments)) {
    argument <- arguments[[index]]
    if (identical(argument, "--help")) {
      result$help <- TRUE
      index <- index + 1L
      next
    }
    if (!startsWith(argument, "--")) {
      stop(sprintf("Unexpected argument: %s", argument), call. = FALSE)
    }
    name <- substring(argument, 3L)
    if (!name %in% names(defaults)) {
      stop(sprintf("Unknown option: --%s", name), call. = FALSE)
    }
    if (index == length(arguments)) {
      stop(sprintf("Option --%s requires a value.", name), call. = FALSE)
    }
    result[[name]] <- arguments[[index + 1L]]
    index <- index + 2L
  }
  result
}


ena3d_audit_output_directory <- function(value, project_root) {
  if (!nzchar(value)) stop("The output directory cannot be empty.", call. = FALSE)
  if (!grepl("^(/|[A-Za-z]:[/\\\\])", value)) {
    value <- file.path(project_root, value)
  }
  dir.create(value, recursive = TRUE, showWarnings = FALSE)
  normalizePath(value, mustWork = TRUE)
}


ena3d_audit_package_versions <- function(expected) {
  actual <- vapply(names(expected), function(package) {
    if (!requireNamespace(package, quietly = TRUE)) return(NA_character_)
    as.character(utils::packageVersion(package))
  }, character(1L))
  matches <- !is.na(actual) & actual == unname(expected)
  list(
    expected = as.list(expected),
    actual = as.list(actual),
    exact_match = as.list(stats::setNames(matches, names(expected))),
    complete = all(matches)
  )
}


ena3d_audit_write_json <- function(value, path) {
  temporary <- tempfile(
    pattern = paste0(".", basename(path), "."),
    tmpdir = dirname(path)
  )
  on.exit(unlink(temporary), add = TRUE)
  jsonlite::write_json(
    value,
    temporary,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null",
    digits = 10
  )
  if (!file.rename(temporary, path)) {
    stop(sprintf("Could not publish JSON report: %s", basename(path)),
         call. = FALSE)
  }
  invisible(path)
}


ena3d_audit_source_functions <- function(path, project_root) {
  expressions <- parse(file = path, keep.source = TRUE)
  source_refs <- attr(expressions, "srcref")
  found <- list()
  for (index in seq_along(expressions)) {
    expression <- expressions[[index]]
    if (!is.call(expression) || length(expression) < 3L) next
    operator <- as.character(expression[[1L]])[[1L]]
    if (!operator %in% c("<-", "=")) next
    definition <- expression[[3L]]
    if (!is.call(definition) ||
        !identical(definition[[1L]], as.name("function"))) next
    source_ref <- if (length(source_refs) >= index) source_refs[[index]] else NULL
    line <- if (is.null(source_ref)) NA_integer_ else as.integer(source_ref[[1L]])
    found[[length(found) + 1L]] <- list(
      file = ena3d_audit_relative_path(path, project_root),
      name = paste(deparse(expression[[2L]]), collapse = ""),
      line = line,
      expression = definition
    )
  }
  found
}


ena3d_audit_exit_status <- function(mode, incomplete, blocked) {
  if (identical(mode, "report-only")) return(0L)
  if (isTRUE(incomplete)) return(2L)
  if (isTRUE(blocked)) return(1L)
  0L
}
