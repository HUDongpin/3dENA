# Deterministic, detection-only property and mutation audit for 3D ENA.
#
# Fixtures are bundled or synthetic. The report stores only case identifiers,
# counts, booleans, seeds, and condition classes; it never stores input rows.

ena3d_property_record <- function(id, title, subsystem, status, observations,
                                  seed, iterations = NULL,
                                  detector_error = NULL) {
  finding_id <- switch(
    id,
    "ENA-FUZZ-002" = "ENA-BUG-006",
    "ENA-FUZZ-006" = "ENA-BUG-007",
    NULL
  )
  replay <- paste(
    "Rscript tools/run_property_fuzz.R --mode report-only",
    "--output output/audit --seed", as.integer(seed),
    "--iterations", as.integer(iterations %||% 64L)
  )
  list(
    id = id,
    title = title,
    subsystem = subsystem,
    status = status,
    finding_id = finding_id,
    release_blocking = status %in% c("failed", "detector_error"),
    observations = observations,
    replay = replay,
    detector_error = detector_error
  )
}


`%||%` <- function(value, fallback) {
  if (is.null(value) || !length(value)) fallback else value
}


ena3d_property_capture <- function(expression, project_root) {
  warnings <- character()
  value <- tryCatch(
    withCallingHandlers(
      expression,
      warning = function(condition) {
        warnings <<- c(warnings, class(condition)[[1L]])
        invokeRestart("muffleWarning")
      }
    ),
    error = function(condition) condition
  )
  list(
    value = if (inherits(value, "error")) NULL else value,
    error = if (inherits(value, "error")) {
      list(
        class = class(value)[[1L]],
        message = audit_safe_text(conditionMessage(value), project_root)
      )
    } else NULL,
    warning_classes = sort(unique(warnings))
  )
}


ena3d_property_exchange_environment <- function(project_root) {
  audit_source_environment(
    project_root,
    c(
      "R/security_utils.R",
      "R/app_utils.R",
      "R/transition.R",
      "R/ena3d_exchange.R",
      "R/app_module_load_dataset.R",
      "R/raw_data_import.R"
    )
  )
}


ena3d_property_bundled_round_trip <- function(project_root, seed, iterations) {
  id <- "ENA-FUZZ-001"
  captured <- ena3d_property_capture({
    exchange <- ena3d_property_exchange_environment(project_root)
    fixtures <- file.path(
      project_root,
      "sample_data",
      c(
        "class1_timepoints_enaset.RData",
        "sample_enaset.Rdata",
        "newfrat_enaset.Rdata",
        "student_enaset.RData"
      )
    )
    rows <- lapply(fixtures, function(path) {
      native <- exchange$ena3d_read_ena_object(path, source_kind = "bundled")
      first <- tempfile(fileext = ".ena3d.json")
      second <- tempfile(fileext = ".ena3d.json")
      on.exit(unlink(c(first, second)), add = TRUE)
      first_result <- exchange$ena3d_write_exchange_file(native, first)
      restored <- exchange$ena3d_read_exchange_file(first)
      second_result <- exchange$ena3d_write_exchange_file(restored, second)
      list(
        deterministic_sha = identical(first_result$sha256,
                                       second_result$sha256),
        deterministic_bytes = identical(first_result$bytes,
                                         second_result$bytes),
        payload_identity = identical(
          exchange$ena3d_exchange_payload(native),
          exchange$ena3d_exchange_payload(restored)
        )
      )
    })
    list(
      dataset_count = length(rows),
      deterministic_sha = all(vapply(
        rows, `[[`, logical(1L), "deterministic_sha"
      )),
      deterministic_bytes = all(vapply(
        rows, `[[`, logical(1L), "deterministic_bytes"
      )),
      native_payload_object_identity = all(vapply(
        rows, `[[`, logical(1L), "payload_identity"
      ))
    )
  }, project_root)
  if (!is.null(captured$error)) {
    return(ena3d_property_record(
      id, "Every bundled dataset has a deterministic exchange round trip",
      "exchange", "detector_error", list(), seed, iterations,
      captured$error
    ))
  }
  result <- captured$value
  passed <- identical(result$dataset_count, 3L) &&
    isTRUE(result$deterministic_sha) &&
    isTRUE(result$deterministic_bytes)
  ena3d_property_record(
    id, "Every bundled dataset has a deterministic exchange round trip",
    "exchange", if (passed) "passed" else "failed",
    c(result, list(warning_classes = as.list(captured$warning_classes))),
    seed, iterations
  )
}


ena3d_property_typed_round_trip <- function(project_root, seed, iterations) {
  id <- "ENA-FUZZ-002"
  captured <- ena3d_property_capture({
    exchange <- ena3d_property_exchange_environment(project_root)
    native <- exchange$ena3d_read_ena_object(
      file.path(project_root, "sample_data", "sample_enaset.Rdata"),
      source_kind = "bundled"
    )
    row_count <- nrow(native$points)
    recycle <- function(values) rep(values, length.out = row_count)
    fold <- as.POSIXct(
      c(1730611800, 1730615400),
      origin = "1970-01-01",
      tz = "America/New_York"
    )
    columns <- list(
      "audit text α" = recycle(c(
        "plain", "emoji-🙂", " leading and trailing ", "comma,value"
      )),
      "audit integer" = as.integer(recycle(c(-1L, 0L, 1L, NA_integer_))),
      "audit double" = as.numeric(recycle(c(
        -0, .Machine$double.xmin * .Machine$double.eps,
        1 + .Machine$double.eps, NA_real_
      ))),
      "audit date" = as.Date(recycle(c("1969-12-31", "2026-07-19", NA))),
      "audit datetime" = recycle(fold),
      "audit elapsed" = as.difftime(
        recycle(c(0, 1 / 60, 48, NA_real_)), units = "hours"
      ),
      "audit factor" = factor(
        recycle(c("late", "early", NA_character_)),
        levels = c("early", "middle", "late")
      ),
      "audit ordered" = ordered(
        recycle(c("late", "early", NA_character_)),
        levels = c("early", "middle", "late")
      ),
      "audit logical" = recycle(c(TRUE, FALSE, NA))
    )
    for (name in names(columns)) {
      values <- columns[[name]]
      class(values) <- unique(c("ena.metadata", class(values)))
      native$meta.data[[name]] <- values
      native$points[[name]] <- values
      native$line.weights[[name]] <- values
    }
    exchange$ena3d_validate_ena_object(native)
    path <- tempfile(fileext = ".ena3d.json")
    on.exit(unlink(path), add = TRUE)
    exchange$ena3d_write_exchange_file(native, path)
    restored <- exchange$ena3d_read_exchange_file(path)
    identities <- vapply(names(columns), function(name) {
      identical(
        exchange$ena3d_exchange_encode_column(
          native$meta.data[[name]], name, "original"
        ),
        exchange$ena3d_exchange_encode_column(
          restored$meta.data[[name]], name, "restored"
        )
      )
    }, logical(1L))
    list(
      typed_column_count = length(columns),
      exact_encoded_identity_count = sum(identities),
      all_exact = all(identities),
      mismatched_types = as.list(sub("^audit ", "", names(columns)[!identities])),
      dst_fold_epoch_count = length(unique(as.numeric(
        restored$meta.data[["audit datetime"]]
      )))
    )
  }, project_root)
  if (!is.null(captured$error)) {
    return(ena3d_property_record(
      id, "Accepted typed metadata values round-trip exactly",
      "typed exchange", "detector_error", list(), seed, iterations,
      captured$error
    ))
  }
  result <- captured$value
  passed <- isTRUE(result$all_exact) &&
    identical(result$typed_column_count, result$exact_encoded_identity_count) &&
    identical(result$dst_fold_epoch_count, 2L)
  ena3d_property_record(
    id, "Accepted typed metadata values round-trip exactly",
    "typed exchange", if (passed) "passed" else "failed",
    c(result, list(warning_classes = as.list(captured$warning_classes))),
    seed, iterations
  )
}


ena3d_property_mutate_raw <- function(bytes, kind) {
  if (!length(bytes)) return(bytes)
  if (identical(kind, "delete")) {
    return(bytes[-sample.int(length(bytes), 1L)])
  }
  if (identical(kind, "replace")) {
    position <- sample.int(length(bytes), 1L)
    replacements <- charToRaw("{}[],:\"0123456789ntruefalse")
    bytes[[position]] <- sample(replacements, 1L)
    return(bytes)
  }
  if (identical(kind, "insert")) {
    position <- sample.int(length(bytes) + 1L, 1L) - 1L
    inserted <- sample(as.raw(c(0x09, 0x0a, 0x0d, 0x22, 0x5b, 0x7b)), 1L)
    return(c(
      if (position) bytes[seq_len(position)] else raw(),
      inserted,
      if (position < length(bytes)) bytes[(position + 1L):length(bytes)] else raw()
    ))
  }
  if (identical(kind, "truncate")) {
    return(bytes[seq_len(sample.int(length(bytes), 1L))])
  }
  stop("Unknown mutation kind.", call. = FALSE)
}


ena3d_property_exchange_mutations <- function(project_root, seed, iterations,
                                               replay_case = NULL) {
  id <- "ENA-FUZZ-003"
  captured <- ena3d_property_capture({
    exchange <- ena3d_property_exchange_environment(project_root)
    source_path <- file.path(
      project_root, "tests", "e2e", "fixtures", "small-valid.ena3d.json"
    )
    connection <- file(source_path, open = "rb")
    on.exit(close(connection), add = TRUE)
    canonical <- readBin(connection, what = "raw", n = file.info(source_path)$size)
    kinds <- c("delete", "replace", "insert", "truncate")
    selected_cases <- if (is.null(replay_case)) {
      seq_len(iterations)
    } else {
      as.integer(replay_case)
    }
    outcomes <- lapply(selected_cases, function(case_index) {
      set.seed(as.integer(seed + case_index * 104729L))
      kind <- kinds[[((case_index - 1L) %% length(kinds)) + 1L]]
      mutated <- ena3d_property_mutate_raw(canonical, kind)
      path <- tempfile(fileext = ".ena3d.json")
      canonical_path <- tempfile(fileext = ".ena3d.json")
      on.exit(unlink(c(path, canonical_path)), add = TRUE)
      output <- file(path, open = "wb")
      writeBin(mutated, output)
      close(output)
      decoded <- tryCatch(
        exchange$ena3d_read_exchange_file(path),
        error = function(condition) condition
      )
      if (inherits(decoded, "error")) {
        return(list(
          case = as.integer(case_index), kind = kind, outcome = "rejected",
          condition_class = class(decoded)[[1L]]
        ))
      }
      stable <- tryCatch({
        exchange$ena3d_validate_ena_object(decoded)
        exchange$ena3d_write_exchange_file(decoded, canonical_path)
        exchange$ena3d_read_exchange_file(canonical_path)
        TRUE
      }, error = function(condition) condition)
      if (inherits(stable, "error")) {
        return(list(
          case = as.integer(case_index), kind = kind,
          outcome = "accepted_unstable",
          condition_class = class(stable)[[1L]]
        ))
      }
      list(
        case = as.integer(case_index), kind = kind,
        outcome = "accepted_stable", condition_class = NULL
      )
    })
    outcome <- vapply(outcomes, `[[`, character(1L), "outcome")
    failures <- outcomes[outcome == "accepted_unstable"]
    list(
      cases_executed = length(outcomes),
      rejected = sum(outcome == "rejected"),
      accepted_stable = sum(outcome == "accepted_stable"),
      accepted_unstable = sum(outcome == "accepted_unstable"),
      failing_cases = lapply(failures, function(item) {
        list(
          case = item$case,
          kind = item$kind,
          condition_class = item$condition_class,
          replay = paste(
            "Rscript tools/run_property_fuzz.R --mode report-only",
            "--output output/audit --seed", as.integer(seed),
            "--iterations", as.integer(iterations),
            "--replay-case", item$case
          )
        )
      })
    )
  }, project_root)
  if (!is.null(captured$error)) {
    return(ena3d_property_record(
      id, "Mutated exchange inputs reject cleanly or canonicalize stably",
      "exchange mutation fuzz", "detector_error", list(), seed, iterations,
      captured$error
    ))
  }
  result <- captured$value
  passed <- identical(result$accepted_unstable, 0L) &&
    identical(result$cases_executed,
              if (is.null(replay_case)) iterations else 1L)
  ena3d_property_record(
    id, "Mutated exchange inputs reject cleanly or canonicalize stably",
    "exchange mutation fuzz", if (passed) "passed" else "failed",
    c(result, list(warning_classes = as.list(captured$warning_classes))),
    seed, iterations
  )
}


ena3d_property_boundaries <- function(project_root, seed, iterations) {
  id <- "ENA-FUZZ-004"
  captured <- ena3d_property_capture({
    exchange <- ena3d_property_exchange_environment(project_root)
    boundary <- 10L
    within <- vapply(c(boundary - 1L, boundary), function(value) {
      !inherits(tryCatch(
        exchange$ena3d_assert_within(value, boundary, "synthetic boundary"),
        error = function(condition) condition
      ), "error")
    }, logical(1L))
    above <- inherits(tryCatch(
      exchange$ena3d_assert_within(boundary + 1L, boundary,
                                   "synthetic boundary"),
      error = function(condition) condition
    ), "error")

    fixture <- file.path(
      project_root, "tests", "e2e", "fixtures", "small-valid.ena3d.json"
    )
    fixture_bytes <- as.integer(file.info(fixture)$size)
    read_at_limit <- function(limit) {
      limits <- exchange$ena3d_data_limits()
      limits$max_exchange_file_bytes <- limit
      tryCatch({
        exchange$ena3d_read_exchange_file(fixture, limits = limits)
        "accepted"
      }, error = function(condition) "rejected")
    }

    raw_at_limit <- function(rows) {
      limits <- exchange$ena3d_data_limits()
      limits$max_raw_rows <- boundary
      limits$max_raw_columns <- 2L
      limits$max_raw_cells <- 2L * boundary
      frame <- data.frame(
        id = seq_len(rows),
        code = rep("audit", rows),
        stringsAsFactors = FALSE
      )
      tryCatch({
        exchange$ena3d_validate_raw_frame(frame, limits = limits)
        "accepted"
      }, error = function(condition) "rejected")
    }

    list(
      scalar_limit_minus_one_accepted = isTRUE(within[[1L]]),
      scalar_exact_limit_accepted = isTRUE(within[[2L]]),
      scalar_limit_plus_one_rejected = isTRUE(above),
      exchange_exact_byte_limit = read_at_limit(fixture_bytes),
      exchange_limit_minus_one = read_at_limit(fixture_bytes - 1L),
      raw_rows_limit_minus_one = raw_at_limit(boundary - 1L),
      raw_rows_exact_limit = raw_at_limit(boundary),
      raw_rows_limit_plus_one = raw_at_limit(boundary + 1L)
    )
  }, project_root)
  if (!is.null(captured$error)) {
    return(ena3d_property_record(
      id, "Resource limits accept through the cap and reject cap plus one",
      "resource boundaries", "detector_error", list(), seed, iterations,
      captured$error
    ))
  }
  result <- captured$value
  passed <- isTRUE(result$scalar_limit_minus_one_accepted) &&
    isTRUE(result$scalar_exact_limit_accepted) &&
    isTRUE(result$scalar_limit_plus_one_rejected) &&
    identical(result$exchange_exact_byte_limit, "accepted") &&
    identical(result$exchange_limit_minus_one, "rejected") &&
    identical(result$raw_rows_limit_minus_one, "accepted") &&
    identical(result$raw_rows_exact_limit, "accepted") &&
    identical(result$raw_rows_limit_plus_one, "rejected")
  ena3d_property_record(
    id, "Resource limits accept through the cap and reject cap plus one",
    "resource boundaries", if (passed) "passed" else "failed",
    c(result, list(warning_classes = as.list(captured$warning_classes))),
    seed, iterations
  )
}


ena3d_property_trajectory_policies <- function(project_root, seed, iterations) {
  id <- "ENA-FUZZ-005"
  captured <- ena3d_property_capture({
    trajectory <- audit_source_environment(
      project_root, "R/trajectory_analysis.R"
    )
    incomplete <- data.frame(
      id = c("a", "b", "a"), time = c(1, 1, 2),
      x = c(0, 10, 4), y = 0, stringsAsFactors = FALSE
    )
    available <- trajectory$compute_centroid_path(
      incomplete, "time", "id", dimensions = c("x", "y"), order = 1:3,
      cohort_policy = "available"
    )
    complete <- trajectory$compute_centroid_path(
      incomplete, "time", "id", dimensions = c("x", "y"), order = 1:2,
      cohort_policy = "complete"
    )
    duplicates <- data.frame(
      id = c("a", "a", "b", "a", "b"),
      time = c(1, 1, 1, 2, 2),
      x = c(0, 2, 3, 4, 6), y = 0,
      stringsAsFactors = FALSE
    )
    collapsed <- trajectory$compute_centroid_path(
      duplicates, "time", "id", dimensions = c("x", "y")
    )
    missing <- incomplete[1:2, , drop = FALSE]
    missing$x[[2L]] <- NA_real_
    missing_rejected <- inherits(tryCatch(
      trajectory$compute_centroid_path(
        missing, "time", "id", dimensions = c("x", "y"),
        na_policy = "error"
      ),
      error = function(condition) condition
    ), "error")
    smallest <- .Machine$double.xmin * .Machine$double.eps
    extremes <- data.frame(
      id = c("a", "b"), time = c(1, 1),
      x = c(.Machine$double.xmax, .Machine$double.xmax),
      y = c(smallest, -smallest),
      weight = c(.Machine$double.xmax, .Machine$double.xmax),
      stringsAsFactors = FALSE
    )
    extreme_path <- trajectory$compute_centroid_path(
      extremes, "time", "id", dimensions = c("x", "y"),
      weights = "weight", na_policy = "error"
    )
    list(
      available_counts_exact = identical(available$n_used, c(2L, 1L, 0L)),
      complete_counts_exact = identical(complete$n_used, c(1L, 1L)),
      complete_centroid_exact = isTRUE(all.equal(
        complete$centroid_x, c(0, 4), tolerance = 0
      )),
      duplicate_centroid_exact = isTRUE(all.equal(
        collapsed$centroid_x, c(2, 5), tolerance = 0
      )),
      duplicate_count_exact = identical(
        collapsed$n_duplicate_rows, c(1L, 0L)
      ),
      missing_error_policy_rejected = missing_rejected,
      extreme_centroid_finite = all(is.finite(unlist(
        extreme_path[c("centroid_x", "centroid_y")], use.names = FALSE
      ))),
      extreme_x_exact = identical(
        extreme_path$centroid_x[[1L]], .Machine$double.xmax
      ),
      near_zero_y_bounded = abs(extreme_path$centroid_y[[1L]]) <= smallest
    )
  }, project_root)
  if (!is.null(captured$error)) {
    return(ena3d_property_record(
      id, "Trajectory missing, duplicate, cohort, and extreme-value policies hold",
      "trajectory numerics", "detector_error", list(), seed, iterations,
      captured$error
    ))
  }
  result <- captured$value
  passed <- all(vapply(result, isTRUE, logical(1L)))
  ena3d_property_record(
    id, "Trajectory missing, duplicate, cohort, and extreme-value policies hold",
    "trajectory numerics", if (passed) "passed" else "failed",
    c(result, list(warning_classes = as.list(captured$warning_classes))),
    seed, iterations
  )
}


ena3d_property_csv_boundaries <- function(project_root, seed, iterations) {
  id <- "ENA-FUZZ-006"
  captured <- ena3d_property_capture({
    security <- audit_source_environment(project_root, "R/security_utils.R")
    source <- data.frame(
      "'=dangerous header" = c("=1+1", "+cmd", "-2+3", "@probe"),
      unicode = c("汉字", "emoji-🙂", "line\nbreak", "tab\tvalue"),
      delimiter = c("comma,value", "quote\"value", "carriage\rreturn", "plain"),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    expected <- security$ena3d_spreadsheet_safe_frame(source)
    path <- tempfile(fileext = ".csv")
    on.exit(unlink(path), add = TRUE)
    write_error <- tryCatch({
      security$ena3d_write_safe_csv(source, path)
      NULL
    }, error = function(condition) condition)
    if (!is.null(write_error)) {
      list(
        exact_round_trip = FALSE,
        rejected_unsupported_controls = grepl(
          "cannot preserve carriage-return or newline", conditionMessage(write_error),
          fixed = TRUE
        ),
        partial_file_published = file.exists(path),
        dangerous_cell_prefix_count = 0L,
        dangerous_header_prefix_count = 0L,
        row_count = 0L,
        column_count = 0L
      )
    } else {
      restored <- utils::read.csv(
        path, check.names = FALSE, stringsAsFactors = FALSE,
        fileEncoding = "UTF-8"
      )
      risky <- function(values) grepl("^[-=+@]", values)
      list(
        exact_round_trip = identical(restored, expected),
        rejected_unsupported_controls = FALSE,
        partial_file_published = FALSE,
        dangerous_cell_prefix_count = sum(risky(unlist(restored, use.names = FALSE))),
        dangerous_header_prefix_count = sum(risky(names(restored))),
        row_count = nrow(restored),
        column_count = ncol(restored)
      )
    }
  }, project_root)
  if (!is.null(captured$error)) {
    return(ena3d_property_record(
      id, "CSV delimiters, Unicode, controls, and formulas round-trip safely",
      "CSV boundary", "detector_error", list(), seed, iterations,
      captured$error
    ))
  }
  result <- captured$value
  passed <- (
    isTRUE(result$rejected_unsupported_controls) &&
      !isTRUE(result$partial_file_published)
  ) || (
    isTRUE(result$exact_round_trip) &&
      identical(result$dangerous_cell_prefix_count, 0L) &&
      identical(result$dangerous_header_prefix_count, 0L) &&
      identical(result$row_count, 4L) && identical(result$column_count, 3L)
  )
  ena3d_property_record(
    id, "CSV delimiters, Unicode, controls, and formulas round-trip safely",
    "CSV boundary", if (passed) "passed" else "failed",
    c(result, list(warning_classes = as.list(captured$warning_classes))),
    seed, iterations
  )
}


ena3d_run_property_fuzz <- function(project_root, mode, output, seed,
                                    iterations, replay_case = NULL) {
  project_root <- normalizePath(project_root, mustWork = TRUE)
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (old_seed_exists) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)

  checks <- list(
    ena3d_property_bundled_round_trip(project_root, seed, iterations),
    ena3d_property_typed_round_trip(project_root, seed, iterations),
    ena3d_property_exchange_mutations(
      project_root, seed, iterations, replay_case = replay_case
    ),
    ena3d_property_boundaries(project_root, seed, iterations),
    ena3d_property_trajectory_policies(project_root, seed, iterations),
    ena3d_property_csv_boundaries(project_root, seed, iterations)
  )
  failed <- vapply(checks, function(check) {
    check$status %in% c("failed", "detector_error")
  }, logical(1L))
  report <- list(
    schema_version = 1L,
    audit_name = "3D ENA deterministic property and mutation audit",
    mode = mode,
    seed = as.integer(seed),
    mutation_iterations = as.integer(iterations),
    replay_case = if (is.null(replay_case)) NULL else as.integer(replay_case),
    status = if (any(failed)) "failed" else "passed",
    safety = list(
      bundled_or_synthetic_fixtures_only = TRUE,
      credentials_read = FALSE,
      live_ai_calls = 0L,
      raw_rows_in_report = FALSE
    ),
    summary = list(
      check_count = length(checks),
      passed = sum(!failed),
      failed = sum(failed),
      mutation_cases = if (is.null(replay_case)) iterations else 1L
    ),
    checks = checks
  )
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  report_path <- file.path(output, "property-fuzz-report.json")
  writeLines(audit_json(report, pretty = TRUE), report_path, useBytes = TRUE)
  checksum <- audit_file_sha256(report_path)
  writeLines(checksum %||% "unavailable",
             file.path(output, "property-fuzz-report.sha256"),
             useBytes = TRUE)
  strict_status <- if (any(failed)) 1L else 0L
  list(
    report = report,
    report_path = report_path,
    report_sha256 = checksum,
    exit_code = if (identical(mode, "strict")) strict_status else 0L
  )
}
