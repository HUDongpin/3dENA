file_arguments <- sub(
  "^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)
)
script_file <- file_arguments[file.exists(file_arguments)][1L]
if (is.na(script_file)) {
  stop("Could not resolve tools/run_property_fuzz.R.", call. = FALSE)
}
project_root <- normalizePath(
  file.path(dirname(script_file), ".."), mustWork = TRUE
)
activation_file <- file.path(project_root, "renv", "activate.R")
if (file.exists(activation_file)) source(activation_file, local = TRUE)
source(file.path(project_root, "tests", "audit", "audit_harness.R"),
       local = TRUE)
source(file.path(project_root, "tests", "audit", "property_fuzz_runner.R"),
       local = TRUE)


ena3d_property_usage <- function() {
  paste(
    "Usage:",
    "  Rscript tools/run_property_fuzz.R --mode report-only --output output/audit",
    "  Rscript tools/run_property_fuzz.R --mode strict --output output/audit",
    "Options:",
    "  --seed INTEGER        deterministic seed (default: 20260719)",
    "  --iterations INTEGER  mutation cases (default: 64)",
    "  --replay-case INTEGER execute one indexed mutation case",
    "  --help                show this message",
    sep = "\n"
  )
}


ena3d_property_parse <- function(arguments) {
  options <- list(
    mode = "report-only", output = "output/audit", seed = 20260719L,
    iterations = 64L, replay_case = NULL, help = FALSE
  )
  index <- 1L
  while (index <= length(arguments)) {
    argument <- arguments[[index]]
    if (argument %in% c("--help", "-h")) {
      options$help <- TRUE
      index <- index + 1L
      next
    }
    equals <- regexec("^--([^=]+)=(.*)$", argument)
    pieces <- regmatches(argument, equals)[[1L]]
    if (length(pieces)) {
      key <- gsub("-", "_", pieces[[2L]], fixed = TRUE)
      value <- pieces[[3L]]
    } else {
      key <- gsub("-", "_", sub("^--", "", argument), fixed = TRUE)
      if (!key %in% c("mode", "output", "seed", "iterations", "replay_case") ||
          index == length(arguments)) {
        stop(sprintf("Unknown or incomplete option: %s", argument),
             call. = FALSE)
      }
      index <- index + 1L
      value <- arguments[[index]]
    }
    if (!key %in% c("mode", "output", "seed", "iterations", "replay_case")) {
      stop(sprintf("Unknown option: %s", argument), call. = FALSE)
    }
    options[[key]] <- value
    index <- index + 1L
  }
  if (!options$mode %in% c("report-only", "strict")) {
    stop("--mode must be report-only or strict.", call. = FALSE)
  }
  for (name in c("seed", "iterations")) {
    value <- suppressWarnings(as.integer(options[[name]]))
    if (length(value) != 1L || is.na(value) || value < 1L) {
      stop(sprintf("--%s must be one positive integer.", name), call. = FALSE)
    }
    options[[name]] <- value
  }
  if (!is.null(options$replay_case)) {
    value <- suppressWarnings(as.integer(options$replay_case))
    if (length(value) != 1L || is.na(value) || value < 1L ||
        value > options$iterations) {
      stop("--replay-case must be between 1 and --iterations.", call. = FALSE)
    }
    options$replay_case <- value
  }
  options
}


options <- tryCatch(
  ena3d_property_parse(commandArgs(trailingOnly = TRUE)),
  error = function(condition) {
    message(conditionMessage(condition))
    message(ena3d_property_usage())
    quit(save = "no", status = 64L)
  }
)
if (isTRUE(options$help)) {
  cat(ena3d_property_usage(), "\n", sep = "")
  quit(save = "no", status = 0L)
}
output <- options$output
if (!grepl("^(/|[A-Za-z]:[/\\\\])", output)) {
  output <- file.path(project_root, output)
}
result <- tryCatch(
  ena3d_run_property_fuzz(
    project_root, options$mode, output, options$seed, options$iterations,
    replay_case = options$replay_case
  ),
  error = function(condition) {
    message(
      "Property/fuzz audit failed before reporting: ",
      audit_safe_text(conditionMessage(condition), project_root)
    )
    quit(save = "no", status = 2L)
  }
)
cat(audit_json(list(
  audit = result$report$audit_name,
  mode = result$report$mode,
  status = result$report$status,
  effective_exit_code = result$exit_code,
  report_sha256 = result$report_sha256
), pretty = TRUE), "\n", sep = "")
quit(save = "no", status = result$exit_code)
