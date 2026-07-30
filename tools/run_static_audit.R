#!/usr/bin/env Rscript

# Offline, detection-only static analysis for first-party R runtime sources.

.ena3d_static_script_path <- function() {
  arguments <- commandArgs(FALSE)
  file_argument <- sub("^--file=", "", grep("^--file=", arguments, value = TRUE))
  existing <- file_argument[file.exists(file_argument)]
  if (length(existing)) normalizePath(existing[[1L]], mustWork = TRUE) else NA_character_
}

.ena3d_static_script <- .ena3d_static_script_path()
.ena3d_static_starts <- c(
  getwd(),
  if (!is.na(.ena3d_static_script)) dirname(.ena3d_static_script) else character()
)
.ena3d_static_root <- NULL
for (.ena3d_static_start in unique(.ena3d_static_starts)) {
  .ena3d_static_candidate <- normalizePath(.ena3d_static_start, mustWork = FALSE)
  repeat {
    if (file.exists(file.path(.ena3d_static_candidate, "tests", "audit",
                              "static_coverage_utils.R"))) {
      .ena3d_static_root <- .ena3d_static_candidate
      break
    }
    .ena3d_static_parent <- dirname(.ena3d_static_candidate)
    if (identical(.ena3d_static_parent, .ena3d_static_candidate)) break
    .ena3d_static_candidate <- .ena3d_static_parent
  }
  if (!is.null(.ena3d_static_root)) break
}
if (is.null(.ena3d_static_root)) {
  stop("Could not locate tests/audit/static_coverage_utils.R.", call. = FALSE)
}
source(file.path(.ena3d_static_root, "tests", "audit",
                 "static_coverage_utils.R"), local = TRUE)

options(warn = 1L)
project_root <- ena3d_audit_find_project_root(.ena3d_static_starts)
ena3d_audit_activate_project_library(project_root)
arguments <- ena3d_audit_parse_cli(
  commandArgs(TRUE),
  list(mode = "report-only", output = "output/audit/static", help = FALSE)
)
if (isTRUE(arguments$help)) {
  cat(paste(
    "Usage: Rscript tools/run_static_audit.R",
    "  --mode report-only|strict",
    "  --output OUTPUT_DIRECTORY",
    sep = "\n"
  ), "\n")
  quit(status = 0L)
}
if (!arguments$mode %in% c("report-only", "strict")) {
  stop("--mode must be report-only or strict.", call. = FALSE)
}
output_directory <- ena3d_audit_output_directory(arguments$output, project_root)

expected_versions <- c(
  lintr = "3.4.0",
  cyclocomp = "1.1.2",
  codetools = "0.2.20",
  jsonlite = "2.0.0"
)
tooling <- ena3d_audit_package_versions(expected_versions)
source_files <- sort(list.files(
  file.path(project_root, "R"),
  pattern = "\\.[Rr]$",
  recursive = FALSE,
  full.names = TRUE
))

detector_errors <- list()
lintr_findings <- list()
codetools_diagnostics <- list()
complexity_rows <- list()

if (isTRUE(tooling$complete)) {
  linters <- list(
    all_equal_linter = lintr::all_equal_linter(),
    class_equals_linter = lintr::class_equals_linter(),
    duplicate_argument_linter = lintr::duplicate_argument_linter(),
    equals_na_linter = lintr::equals_na_linter(),
    for_loop_index_linter = lintr::for_loop_index_linter(),
    sample_int_linter = lintr::sample_int_linter(),
    seq_linter = lintr::seq_linter(),
    sprintf_linter = lintr::sprintf_linter(),
    terminal_close_linter = lintr::terminal_close_linter(),
    vector_logic_linter = lintr::vector_logic_linter()
  )
  for (path in source_files) {
    detected <- tryCatch(
      lintr::lint(path, linters = linters, cache = FALSE),
      error = function(error) error
    )
    if (inherits(detected, "error")) {
      detector_errors[[length(detector_errors) + 1L]] <- list(
        detector = "lintr",
        file = ena3d_audit_relative_path(path, project_root),
        message = ena3d_audit_sanitize_text(
          conditionMessage(detected), project_root
        )
      )
      next
    }
    for (finding in detected) {
      lintr_findings[[length(lintr_findings) + 1L]] <- list(
        file = ena3d_audit_relative_path(finding$filename, project_root),
        line = unname(as.integer(finding$line_number)),
        column = unname(as.integer(finding$column_number)),
        linter = as.character(finding$linter),
        type = as.character(finding$type),
        message = ena3d_audit_sanitize_text(finding$message, project_root)
      )
    }
  }

  function_records <- unlist(lapply(
    source_files,
    function(path) {
      tryCatch(
        ena3d_audit_source_functions(path, project_root),
        error = function(error) {
          detector_errors[[length(detector_errors) + 1L]] <<- list(
            detector = "function-parser",
            file = ena3d_audit_relative_path(path, project_root),
            message = ena3d_audit_sanitize_text(
              conditionMessage(error), project_root
            )
          )
          list()
        }
      )
    }
  ), recursive = FALSE)

  for (record in function_records) {
    function_value <- tryCatch(
      eval(record$expression, envir = new.env(parent = globalenv())),
      error = function(error) error
    )
    if (inherits(function_value, "error")) {
      detector_errors[[length(detector_errors) + 1L]] <- list(
        detector = "function-evaluation",
        file = record$file,
        function_name = record$name,
        message = ena3d_audit_sanitize_text(
          conditionMessage(function_value), project_root
        )
      )
      next
    }

    complexity <- tryCatch(
      suppressWarnings(cyclocomp::cyclocomp(function_value)),
      error = function(error) error
    )
    if (inherits(complexity, "error")) {
      detector_errors[[length(detector_errors) + 1L]] <- list(
        detector = "cyclocomp",
        file = record$file,
        function_name = record$name,
        message = ena3d_audit_sanitize_text(
          conditionMessage(complexity), project_root
        )
      )
    } else {
      complexity_rows[[length(complexity_rows) + 1L]] <- list(
        file = record$file,
        function_name = record$name,
        line = record$line,
        complexity = as.integer(complexity)
      )
    }

    usage <- character()
    usage_error <- tryCatch({
      codetools::checkUsage(
        function_value,
        name = record$name,
        report = function(...) {
          usage <<- c(usage, paste0(..., collapse = ""))
        },
        all = FALSE,
        suppressUndefined = TRUE,
        suppressPartialMatchArgs = TRUE
      )
      NULL
    }, error = function(error) error)
    if (inherits(usage_error, "error")) {
      detector_errors[[length(detector_errors) + 1L]] <- list(
        detector = "codetools",
        file = record$file,
        function_name = record$name,
        message = ena3d_audit_sanitize_text(
          conditionMessage(usage_error), project_root
        )
      )
    } else if (length(usage)) {
      for (diagnostic in usage) {
        codetools_diagnostics[[length(codetools_diagnostics) + 1L]] <- list(
          file = record$file,
          function_name = record$name,
          line = record$line,
          classification = if (grepl(
            "possible error in layout\\(", diagnostic, perl = TRUE
          )) "dispatch-review" else "usage-review",
          message = ena3d_audit_sanitize_text(diagnostic, project_root)
        )
      }
    }
  }
}

complexity_review_threshold <- 25L
complexity_gate_threshold <- 100L
complexity_rows <- complexity_rows[order(vapply(
  complexity_rows,
  function(row) -row$complexity,
  numeric(1L)
))]
complexity_review <- Filter(
  function(row) row$complexity > complexity_review_threshold,
  complexity_rows
)
complexity_gate <- Filter(
  function(row) row$complexity > complexity_gate_threshold,
  complexity_rows
)

incomplete <- !isTRUE(tooling$complete) || length(detector_errors) > 0L
blocked <- length(lintr_findings) > 0L || length(complexity_gate) > 0L
status <- if (incomplete) {
  "incomplete"
} else if (blocked) {
  "findings"
} else {
  "pass"
}
report <- list(
  schema_version = 1L,
  detector = "3dena-r-static-audit",
  mode = arguments$mode,
  status = status,
  offline = TRUE,
  source_scope = "R/*.[Rr]",
  tooling = tooling,
  configuration = list(
    linters = c(
      "all_equal_linter", "class_equals_linter",
      "duplicate_argument_linter", "equals_na_linter",
      "for_loop_index_linter", "sample_int_linter", "seq_linter",
      "sprintf_linter", "terminal_close_linter", "vector_logic_linter"
    ),
    complexity_review_threshold = complexity_review_threshold,
    complexity_gate_threshold = complexity_gate_threshold,
    codetools_undefined_symbols_suppressed = TRUE,
    codetools_diagnostics_are_informational = TRUE
  ),
  summary = list(
    source_files = length(source_files),
    functions_checked = length(complexity_rows),
    lintr_findings = length(lintr_findings),
    codetools_diagnostics = length(codetools_diagnostics),
    complexity_review_candidates = length(complexity_review),
    complexity_gate_findings = length(complexity_gate),
    detector_errors = length(detector_errors)
  ),
  lintr_findings = lintr_findings,
  codetools_diagnostics = codetools_diagnostics,
  complexity_review = complexity_review,
  complexity_gate = complexity_gate,
  detector_errors = detector_errors,
  gate = list(
    incomplete = incomplete,
    blocked = blocked,
    strict_exit_status = ena3d_audit_exit_status("strict", incomplete, blocked)
  ),
  replay = paste(
    "Rscript tools/run_static_audit.R --mode", arguments$mode,
    "--output output/audit/static"
  )
)
report_path <- file.path(output_directory, "r-static-audit.json")
ena3d_audit_write_json(report, report_path)
message(sprintf(
  "R static audit: %s (%d lintr, %d complexity-gate, %d detector errors).",
  status, length(lintr_findings), length(complexity_gate),
  length(detector_errors)
))
quit(status = ena3d_audit_exit_status(arguments$mode, incomplete, blocked))
