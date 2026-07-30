file_arguments <- sub(
  "^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)
)
script_file <- file_arguments[file.exists(file_arguments)][1L]
if (is.na(script_file)) {
  stop("Could not resolve tools/run_bug_audit.R.", call. = FALSE)
}
project_root <- normalizePath(
  file.path(dirname(script_file), ".."), mustWork = TRUE
)
activation_file <- file.path(project_root, "renv", "activate.R")
if (file.exists(activation_file)) {
  source(activation_file, local = TRUE)
}
source(
  file.path(project_root, "tests", "audit", "audit_harness.R"),
  local = TRUE
)


audit_cli_usage <- function() {
  paste(
    "Usage:",
    "  Rscript tools/run_bug_audit.R --mode report-only --output output/audit",
    "  Rscript tools/run_bug_audit.R --mode strict --output output/audit",
    "Options:",
    "  --mode VALUE     report-only (always exits zero after a completed run) or strict",
    "  --output PATH    artifact directory; relative paths resolve from the project root",
    "  --seed INTEGER   deterministic replay seed (default: 20260719)",
    "  --only IDS       comma-separated check IDs for focused replay",
    "  --help           show this message",
    sep = "\n"
  )
}


audit_cli_parse <- function(arguments) {
  options <- list(
    mode = "report-only",
    output = "output/audit",
    seed = 20260719L,
    only = character(),
    help = FALSE
  )
  index <- 1L
  while (index <= length(arguments)) {
    argument <- arguments[[index]]
    if (identical(argument, "--help") || identical(argument, "-h")) {
      options$help <- TRUE
      index <- index + 1L
      next
    }
    matched <- regexec("^--([^=]+)=(.*)$", argument)
    pieces <- regmatches(argument, matched)[[1L]]
    if (length(pieces)) {
      key <- pieces[[2L]]
      value <- pieces[[3L]]
    } else if (argument %in% c("--mode", "--output", "--seed", "--only")) {
      if (index == length(arguments)) {
        stop(sprintf("Missing value for %s.", argument), call. = FALSE)
      }
      key <- sub("^--", "", argument)
      index <- index + 1L
      value <- arguments[[index]]
    } else {
      stop(sprintf("Unknown audit option: %s", argument), call. = FALSE)
    }
    if (!key %in% c("mode", "output", "seed", "only")) {
      stop(sprintf("Unknown audit option: --%s", key), call. = FALSE)
    }
    if (identical(key, "only")) {
      options$only <- c(
        options$only,
        trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
      )
    } else {
      options[[key]] <- value
    }
    index <- index + 1L
  }
  if (!options$mode %in% c("report-only", "strict")) {
    stop("--mode must be report-only or strict.", call. = FALSE)
  }
  seed <- suppressWarnings(as.integer(options$seed))
  if (length(seed) != 1L || is.na(seed) || seed < 1L) {
    stop("--seed must be one positive integer.", call. = FALSE)
  }
  options$seed <- seed
  options$only <- unique(options$only[nzchar(options$only)])
  options
}


arguments <- commandArgs(trailingOnly = TRUE)
options <- tryCatch(
  audit_cli_parse(arguments),
  error = function(error) {
    message(conditionMessage(error))
    message(audit_cli_usage())
    quit(save = "no", status = 64L)
  }
)
if (isTRUE(options$help)) {
  cat(audit_cli_usage(), "\n", sep = "")
  quit(save = "no", status = 0L)
}

output_directory <- options$output
if (!grepl("^(/|[A-Za-z]:[/\\\\])", output_directory)) {
  output_directory <- file.path(project_root, output_directory)
}

result <- tryCatch(
  audit_run(
    project_root = project_root,
    mode = options$mode,
    seed = options$seed,
    only = options$only
  ),
  error = function(error) {
    message("Audit harness failed before a report could be completed: ",
            audit_safe_text(conditionMessage(error), project_root))
    quit(save = "no", status = 2L)
  }
)
artifacts <- tryCatch(
  audit_write_artifacts(result, output_directory),
  error = function(error) {
    message("Audit artifact writing failed: ",
            audit_safe_text(conditionMessage(error), project_root))
    quit(save = "no", status = 2L)
  }
)

cat(audit_json(list(
  audit = "3D ENA systematic bug detection",
  mode = result$report$mode,
  overall_status = result$report$overall_status,
  release_gate = result$report$release_gate$decision,
  effective_exit_code = result$exit_code,
  report_sha256 = artifacts$report_sha256,
  artifacts = list(
    report = "audit-report.json",
    findings = "audit-findings.jsonl",
    checksum = "audit-report.sha256"
  )
), pretty = TRUE), "\n", sep = "")

quit(save = "no", status = as.integer(result$exit_code))
