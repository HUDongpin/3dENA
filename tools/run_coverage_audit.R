#!/usr/bin/env Rscript

# Deterministic, subsystem-level R coverage for the offline bug audit.

.ena3d_coverage_script_path <- function() {
  arguments <- commandArgs(FALSE)
  file_argument <- sub("^--file=", "", grep("^--file=", arguments, value = TRUE))
  existing <- file_argument[file.exists(file_argument)]
  if (length(existing)) normalizePath(existing[[1L]], mustWork = TRUE) else NA_character_
}

.ena3d_coverage_script <- .ena3d_coverage_script_path()
.ena3d_coverage_starts <- c(
  getwd(),
  if (!is.na(.ena3d_coverage_script)) dirname(.ena3d_coverage_script) else character()
)
.ena3d_coverage_root <- NULL
for (.ena3d_coverage_start in unique(.ena3d_coverage_starts)) {
  .ena3d_coverage_candidate <- normalizePath(.ena3d_coverage_start,
                                              mustWork = FALSE)
  repeat {
    if (file.exists(file.path(.ena3d_coverage_candidate, "tests", "audit",
                              "static_coverage_utils.R"))) {
      .ena3d_coverage_root <- .ena3d_coverage_candidate
      break
    }
    .ena3d_coverage_parent <- dirname(.ena3d_coverage_candidate)
    if (identical(.ena3d_coverage_parent, .ena3d_coverage_candidate)) break
    .ena3d_coverage_candidate <- .ena3d_coverage_parent
  }
  if (!is.null(.ena3d_coverage_root)) break
}
if (is.null(.ena3d_coverage_root)) {
  stop("Could not locate tests/audit/static_coverage_utils.R.", call. = FALSE)
}
source(file.path(.ena3d_coverage_root, "tests", "audit",
                 "static_coverage_utils.R"), local = TRUE)

options(warn = 1L)
project_root <- ena3d_audit_find_project_root(.ena3d_coverage_starts)
ena3d_audit_activate_project_library(project_root)
arguments <- ena3d_audit_parse_cli(
  commandArgs(TRUE),
  list(
    mode = "report-only",
    output = "output/audit/coverage",
    manifest = "tests/audit/coverage_manifest.json",
    baseline = "tests/audit/coverage_baseline.json",
    help = FALSE
  )
)
if (isTRUE(arguments$help)) {
  cat(paste(
    "Usage: Rscript tools/run_coverage_audit.R",
    "  --mode report-only|strict",
    "  --output OUTPUT_DIRECTORY",
    "  --manifest tests/audit/coverage_manifest.json",
    "  --baseline tests/audit/coverage_baseline.json",
    sep = "\n"
  ), "\n")
  quit(status = 0L)
}
if (!arguments$mode %in% c("report-only", "strict")) {
  stop("--mode must be report-only or strict.", call. = FALSE)
}
resolve_input <- function(value) {
  if (!grepl("^(/|[A-Za-z]:[/\\\\])", value)) {
    value <- file.path(project_root, value)
  }
  normalizePath(value, mustWork = FALSE)
}
manifest_path <- resolve_input(arguments$manifest)
baseline_path <- resolve_input(arguments$baseline)
output_directory <- ena3d_audit_output_directory(arguments$output, project_root)

# The detector process starts with AI disabled and provider credential names
# removed. Individual Qwen tests use only synthetic credentials and injected
# transports, restoring this credential-free state after each test.
Sys.unsetenv(c("DASHSCOPE_API_KEY", "DASHSCOPE_API_KEY_FILE"))
Sys.setenv(ENA3D_AI_ENABLED = "false")

expected_versions <- c(
  covr = "3.6.5",
  testthat = "3.2.1.1",
  jsonlite = "2.0.0"
)
tooling <- ena3d_audit_package_versions(expected_versions)
detector_errors <- list()
contract_results <- list()
subsystem_results <- list()

manifest <- if (file.exists(manifest_path)) {
  tryCatch(
    jsonlite::read_json(manifest_path, simplifyVector = FALSE),
    error = function(error) error
  )
} else {
  simpleError("Coverage manifest is missing.")
}
if (inherits(manifest, "error")) {
  detector_errors[[length(detector_errors) + 1L]] <- list(
    detector = "manifest",
    message = ena3d_audit_sanitize_text(conditionMessage(manifest), project_root)
  )
  manifest <- list(schema_version = NA_integer_, seed = NA_integer_,
                   subsystems = list())
}

baseline <- if (file.exists(baseline_path)) {
  tryCatch(
    jsonlite::read_json(baseline_path, simplifyVector = FALSE),
    error = function(error) error
  )
} else {
  simpleError("Coverage baseline is missing.")
}
if (inherits(baseline, "error")) {
  detector_errors[[length(detector_errors) + 1L]] <- list(
    detector = "baseline",
    message = ena3d_audit_sanitize_text(conditionMessage(baseline), project_root)
  )
  baseline <- list(schema_version = NA_integer_, tolerance_percentage_points = 0.01,
                   subsystems = list())
}

# The standard test suite executes with tests/testthat as its working directory.
# Preserving that contract is required for a small number of legacy fixtures
# and also prevents the root marker "." from being mistaken for path leakage.
old_working_directory <- setwd(file.path(project_root, "tests", "testthat"))
on.exit(setwd(old_working_directory), add = TRUE)
old_lc_all <- Sys.getenv("LC_ALL", unset = NA_character_)
Sys.unsetenv("LC_ALL")
on.exit({
  if (is.na(old_lc_all)) Sys.unsetenv("LC_ALL") else Sys.setenv(LC_ALL = old_lc_all)
}, add = TRUE)

coverage_filename <- function(filename) {
  if (!grepl("^(/|[A-Za-z]:[/\\\\])", filename)) {
    filename <- file.path(project_root, filename)
  }
  ena3d_audit_relative_path(filename, project_root)
}
or_default <- function(value, default) {
  if (is.null(value) || !length(value)) default else value
}
quiet_file_coverage <- function(source_paths, test_paths, warning_handler) {
  transcript <- tempfile("ena3d-coverage-transcript-")
  connection <- file(transcript, open = "wt", encoding = "UTF-8")
  sink(connection, type = "output")
  sink(connection, type = "message")
  on.exit({
    sink(type = "message")
    sink(type = "output")
    close(connection)
    unlink(transcript)
  }, add = TRUE)
  withCallingHandlers(
    covr::file_coverage(
      source_files = source_paths,
      test_files = test_paths,
      parent_env = globalenv()
    ),
    warning = warning_handler
  )
}

if (isTRUE(tooling$complete) && length(manifest$subsystems)) {
  for (subsystem_index in seq_along(manifest$subsystems)) {
    subsystem <- manifest$subsystems[[subsystem_index]]
    subsystem_id <- as.character(subsystem$id)
    source_paths <- file.path(project_root, unlist(subsystem$sources,
                                                   use.names = FALSE))
    test_paths <- file.path(project_root, unlist(subsystem$tests,
                                                 use.names = FALSE))
    missing_files <- c(
      unlist(subsystem$sources, use.names = FALSE)[!file.exists(source_paths)],
      unlist(subsystem$tests, use.names = FALSE)[!file.exists(test_paths)]
    )
    if (length(missing_files)) {
      detector_errors[[length(detector_errors) + 1L]] <- list(
        detector = "coverage-input",
        subsystem = subsystem_id,
        message = sprintf("Missing declared files: %s",
                          paste(missing_files, collapse = ", "))
      )
      next
    }

    for (contract in subsystem$risk_test_contracts) {
      contract_path <- file.path(project_root, as.character(contract$test_file))
      declared <- as.character(contract$test_file) %in%
        unlist(subsystem$tests, use.names = FALSE)
      source_text <- if (file.exists(contract_path)) {
        paste(readLines(contract_path, warn = FALSE, encoding = "UTF-8"),
              collapse = "\n")
      } else ""
      named <- nzchar(source_text) && grepl(
        as.character(contract$test_name), source_text, fixed = TRUE
      )
      contract_results[[length(contract_results) + 1L]] <- list(
        subsystem = subsystem_id,
        risk = as.character(contract$risk),
        test_file = as.character(contract$test_file),
        test_name = as.character(contract$test_name),
        declared_in_subsystem = declared,
        named_test_present = named,
        status = if (declared && named) "mapped" else "missing"
      )
    }

    set.seed(as.integer(manifest$seed) + subsystem_index - 1L)
    warnings <- character()
    coverage <- tryCatch(
      quiet_file_coverage(
        source_paths,
        test_paths,
        warning_handler = function(condition) {
          warnings <<- c(
            warnings,
            ena3d_audit_sanitize_text(conditionMessage(condition), project_root)
          )
          invokeRestart("muffleWarning")
        }
      ),
      error = function(error) error
    )
    if (inherits(coverage, "error")) {
      detector_errors[[length(detector_errors) + 1L]] <- list(
        detector = "covr",
        subsystem = subsystem_id,
        message = ena3d_audit_sanitize_text(conditionMessage(coverage), project_root)
      )
      subsystem_results[[length(subsystem_results) + 1L]] <- list(
        id = subsystem_id,
        status = "incomplete",
        seed = as.integer(manifest$seed) + subsystem_index - 1L,
        warnings = unique(warnings),
        error = ena3d_audit_sanitize_text(conditionMessage(coverage), project_root)
      )
      next
    }

    details <- as.data.frame(coverage)
    details$covered <- details$value > 0
    details$file <- vapply(details$filename, coverage_filename, character(1L))
    file_names <- sort(unique(details$file))
    file_rows <- lapply(file_names, function(filename) {
      selected <- details$file == filename
      total <- sum(selected)
      covered <- sum(details$covered[selected])
      list(
        file = filename,
        covered_expressions = covered,
        total_expressions = total,
        percent = if (total) 100 * covered / total else 0
      )
    })
    total <- nrow(details)
    covered_count <- sum(details$covered)
    subsystem_results[[length(subsystem_results) + 1L]] <- list(
      id = subsystem_id,
      status = "measured",
      seed = as.integer(manifest$seed) + subsystem_index - 1L,
      covered_expressions = covered_count,
      total_expressions = total,
      percent = if (total) 100 * covered_count / total else 0,
      source_files = unname(unlist(subsystem$sources, use.names = FALSE)),
      test_files = unname(unlist(subsystem$tests, use.names = FALSE)),
      warnings = unique(warnings),
      files = file_rows
    )
    message(sprintf(
      "Coverage %s: %.2f%% (%d/%d expressions).",
      subsystem_id,
      subsystem_results[[length(subsystem_results)]]$percent,
      covered_count,
      total
    ))
  }
}

tolerance <- as.numeric(or_default(
  baseline$tolerance_percentage_points, 0.01
))
baseline_rows <- or_default(baseline$subsystems, list())
baseline_by_id <- stats::setNames(baseline_rows, vapply(
  baseline_rows,
  function(row) as.character(row$id),
  character(1L)
))
regressions <- list()
for (current in subsystem_results) {
  if (!identical(current$status, "measured")) next
  prior <- baseline_by_id[[current$id]]
  if (is.null(prior)) {
    regressions[[length(regressions) + 1L]] <- list(
      subsystem = current$id,
      reason = "missing-baseline-subsystem",
      baseline_percent = NULL,
      current_percent = current$percent
    )
    next
  }
  if (current$percent + tolerance < as.numeric(prior$percent)) {
    regressions[[length(regressions) + 1L]] <- list(
      subsystem = current$id,
      reason = "coverage-regression",
      baseline_percent = as.numeric(prior$percent),
      current_percent = current$percent,
      tolerance_percentage_points = tolerance
    )
  }
}
missing_contracts <- Filter(
  function(contract) !identical(contract$status, "mapped"),
  contract_results
)
expected_subsystems <- vapply(
  manifest$subsystems, function(subsystem) as.character(subsystem$id), character(1L)
)
measured_subsystems <- vapply(
  Filter(function(row) identical(row$status, "measured"), subsystem_results),
  function(row) row$id,
  character(1L)
)
incomplete <- !isTRUE(tooling$complete) ||
  length(detector_errors) > 0L ||
  length(missing_contracts) > 0L ||
  !setequal(expected_subsystems, measured_subsystems)
blocked <- length(regressions) > 0L
status <- if (incomplete) {
  "incomplete"
} else if (blocked) {
  "regressed"
} else {
  "pass"
}
report <- list(
  schema_version = 1L,
  detector = "3dena-r-subsystem-coverage",
  mode = arguments$mode,
  status = status,
  offline = TRUE,
  tooling = tooling,
  manifest = list(
    path = ena3d_audit_relative_path(manifest_path, project_root),
    schema_version = manifest$schema_version,
    seed = manifest$seed,
    subsystem_count = length(manifest$subsystems),
    risk_contract_count = length(contract_results)
  ),
  baseline = list(
    path = ena3d_audit_relative_path(baseline_path, project_root),
    schema_version = baseline$schema_version,
    tolerance_percentage_points = tolerance
  ),
  summary = list(
    expected_subsystems = length(expected_subsystems),
    measured_subsystems = length(measured_subsystems),
    mapped_risk_tests = length(contract_results) - length(missing_contracts),
    missing_risk_tests = length(missing_contracts),
    regressions = length(regressions),
    detector_errors = length(detector_errors)
  ),
  subsystems = subsystem_results,
  risk_test_contracts = contract_results,
  regressions = regressions,
  detector_errors = detector_errors,
  gate = list(
    incomplete = incomplete,
    blocked = blocked,
    strict_exit_status = ena3d_audit_exit_status("strict", incomplete, blocked)
  ),
  replay = paste(
    "Rscript tools/run_coverage_audit.R --mode", arguments$mode,
    "--output output/audit/coverage"
  )
)
report_path <- file.path(output_directory, "r-coverage-audit.json")
ena3d_audit_write_json(report, report_path)
message(sprintf(
  "R coverage audit: %s (%d/%d subsystems, %d regressions, %d errors).",
  status, length(measured_subsystems), length(expected_subsystems),
  length(regressions), length(detector_errors)
))
quit(status = ena3d_audit_exit_status(arguments$mode, incomplete, blocked))
