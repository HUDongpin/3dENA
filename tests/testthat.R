ena3d_find_project_root <- function(starts = getwd()) {
  starts <- unique(normalizePath(starts, mustWork = FALSE))
  for (start in starts) {
    candidate <- start
    repeat {
      if (file.exists(file.path(candidate, "R", "app.R")) &&
          file.exists(file.path(candidate, "tests", "testthat.R"))) {
        return(normalizePath(candidate, mustWork = TRUE))
      }
      parent <- dirname(candidate)
      if (identical(parent, candidate)) break
      candidate <- parent
    }
  }
  stop("Could not locate the 3D ENA project root.", call. = FALSE)
}

file_args <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
start_paths <- c(getwd(), dirname(file_args[file.exists(file_args)]))
project_root <- ena3d_find_project_root(start_paths)

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("The standard test suite requires the `testthat` package.", call. = FALSE)
}

# A forced LC_ALL prevents packages from changing only the message locale and
# creates hundreds of irrelevant warnings on otherwise successful tests. Keep
# LANG/LC_CTYPE intact, temporarily release LC_ALL, and restore the caller's
# environment even when testthat errors.
old_lc_all <- Sys.getenv("LC_ALL", unset = NA_character_)
Sys.unsetenv("LC_ALL")
results <- tryCatch(
  testthat::test_dir(
    file.path(project_root, "tests", "testthat"),
    reporter = "summary",
    stop_on_failure = FALSE,
    load_package = "none"
  ),
  finally = {
    if (is.na(old_lc_all)) {
      Sys.unsetenv("LC_ALL")
    } else {
      Sys.setenv(LC_ALL = old_lc_all)
    }
  }
)

result_summary <- as.data.frame(results)
failed <- sum(result_summary$failed, na.rm = TRUE)
errored <- sum(result_summary$error, na.rm = TRUE)
if (failed > 0L || errored > 0L) {
  stop(
    sprintf("3D ENA tests failed: %d failed expectations and %d errors.",
            failed, errored),
    call. = FALSE
  )
}

invisible(results)
