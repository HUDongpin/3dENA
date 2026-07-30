# Detection-only, offline audit harness for 3D ENA.
#
# This file deliberately sources the smallest possible runtime surfaces and
# never loads the Qwen client or reads provider credentials.  All fixtures are
# synthetic.  Reports contain only fixed descriptions, booleans, counts,
# version strings, and source hashes.

.audit_schema_version <- 1L
.audit_default_seed <- 20260719L


audit_safe_text <- function(value, project_root = NULL, max_chars = 500L) {
  if (is.null(value) || !length(value)) return("")
  value <- enc2utf8(paste(as.character(value), collapse = " "))
  if (!is.null(project_root) && nzchar(project_root)) {
    value <- gsub(project_root, "<PROJECT_ROOT>", value, fixed = TRUE)
  }
  temporary <- normalizePath(tempdir(), mustWork = FALSE)
  if (nzchar(temporary)) {
    value <- gsub(temporary, "<TEMP_DIR>", value, fixed = TRUE)
  }
  value <- gsub("[[:cntrl:]]+", " ", value, perl = TRUE)
  value <- gsub("[[:space:]]+", " ", trimws(value), perl = TRUE)
  value <- gsub(
    paste0(
      "(?i)(api[ _-]?key|access[ _-]?token|password|secret)",
      "[[:space:]]*[:=][[:space:]]*[^[:space:]]+"
    ),
    "\\1=<REDACTED>",
    value,
    perl = TRUE
  )
  substr(value, 1L, as.integer(max_chars))
}


audit_condition_record <- function(condition, project_root) {
  classes <- class(condition)
  list(
    class = if (length(classes)) classes[[1L]] else "condition",
    message = audit_safe_text(conditionMessage(condition), project_root)
  )
}


audit_tree_contains_text <- function(value, needles) {
  if (is.null(value) || !length(value)) return(FALSE)
  if (is.data.frame(value)) value <- unclass(value)
  if (is.list(value)) {
    return(any(vapply(
      value,
      audit_tree_contains_text,
      logical(1L),
      needles = needles
    )))
  }
  if (!is.character(value)) return(FALSE)
  any(vapply(needles, function(needle) {
    any(grepl(needle, value, fixed = TRUE))
  }, logical(1L)))
}


audit_capture <- function(expression, project_root) {
  warnings <- list()
  value <- tryCatch(
    withCallingHandlers(
      expression,
      warning = function(condition) {
        warnings[[length(warnings) + 1L]] <<-
          audit_condition_record(condition, project_root)
        invokeRestart("muffleWarning")
      }
    ),
    error = function(condition) condition
  )
  list(
    value = if (inherits(value, "error")) NULL else value,
    error = if (inherits(value, "error")) {
      audit_condition_record(value, project_root)
    } else NULL,
    warnings = warnings
  )
}


audit_source_environment <- function(project_root, files) {
  # Match ordinary R runtime lookup (including the default stats package)
  # while keeping sourced project definitions out of the global environment.
  environment <- new.env(parent = globalenv())
  for (relative_path in files) {
    path <- file.path(project_root, relative_path)
    if (!file.exists(path)) {
      stop(sprintf("Required audit source is missing: %s", relative_path),
           call. = FALSE)
    }
    sys.source(path, envir = environment, keep.source = FALSE)
  }
  environment
}


audit_finding <- function(id, title, severity, confidence, subsystem,
                          status, fixture, expected, actual, observations,
                          root_cause, impact, regression_test, seed,
                          detector_error = NULL) {
  list(
    id = id,
    kind = "seeded_finding",
    title = title,
    severity = severity,
    confidence = confidence,
    subsystem = subsystem,
    status = status,
    release_blocking = identical(status, "reproduced") &&
      severity %in% c("S0", "S1"),
    fixture = fixture,
    expected = expected,
    actual = actual,
    observations = observations,
    root_cause_location = root_cause,
    impact = impact,
    regression_test_specification = regression_test,
    replay = sprintf(
      paste(
        "Rscript tools/run_bug_audit.R --mode report-only",
        "--output output/audit --seed %d --only %s"
      ),
      as.integer(seed), id
    ),
    detector_error = detector_error
  )
}


audit_probe <- function(id, title, subsystem, status, observations, seed,
                        detector_error = NULL) {
  list(
    id = id,
    kind = "property_probe",
    title = title,
    severity_if_failed = "S1",
    subsystem = subsystem,
    status = status,
    release_blocking = status %in% c("failed", "detector_error"),
    observations = observations,
    replay = sprintf(
      paste(
        "Rscript tools/run_bug_audit.R --mode report-only",
        "--output output/audit --seed %d --only %s"
      ),
      as.integer(seed), id
    ),
    detector_error = detector_error
  )
}


audit_probe_ai_name_leak <- function(project_root, seed) {
  id <- "ENA-BUG-001"
  captured <- audit_capture({
    ai <- audit_source_environment(project_root, "R/ai_evidence.R")
    # Fingerprints are outside this privacy detector's scope.  Replacing only
    # these pure hash helpers keeps the evidence path executable when an
    # intentionally incomplete renv library lacks digest; no provider helper
    # or transport is loaded.
    ai$ena3d_ai_data_fingerprint <- function(...) strrep("0", 64L)
    ai$.ena3d_ai_request_fingerprint <- function(...) strrep("1", 64L)
    row_count <- 10L
    identifier_markers <- c(
      "AUDIT_SYNTHETIC_PERSON_ALPHA",
      "AUDIT_SYNTHETIC_PERSON_BETA"
    )
    points <- data.frame(
      Name = rep(identifier_markers, each = row_count / 2L),
      MR1 = seq_len(row_count),
      SVD2 = 2 * seq_len(row_count),
      SVD3 = rev(seq_len(row_count)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    line_weights <- data.frame(
      Name = points$Name,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    line_weights[["Audit Code A & Audit Code B"]] <-
      seq_len(row_count) / row_count
    ena_object <- list(
      points = points,
      line.weights = line_weights,
      rotation = list(
        nodes = data.frame(
          code = c("Audit Code A", "Audit Code B"),
          MR1 = c(-1, 1),
          SVD2 = c(1, -1),
          SVD3 = c(-0.5, 0.5),
          check.names = FALSE,
          stringsAsFactors = FALSE
        ),
        adjacency.key = rbind(c("Audit Code A"), c("Audit Code B"))
      )
    )
    guard_classifies_name <- ai$.ena3d_ai_is_identifier_name("Name")
    built <- audit_capture(
      ai$ena3d_ai_build_evidence(
        ena_object,
        view = "change",
        settings = list(
          change_var = "Name",
          axes = c("MR1", "SVD2", "SVD3")
        ),
        min_cell_n = 5L
      ),
      project_root
    )
    if (!is.null(built$error)) {
      list(
        guard_classifies_name = isTRUE(guard_classifies_name),
        build_rejected = TRUE,
        expected_privacy_rejection = isTRUE(guard_classifies_name) &&
          grepl("identifier", built$error$message, ignore.case = TRUE),
        payload_contains_synthetic_identifiers = FALSE,
        raw_rows_declared = NA
      )
    } else {
      payload <- ai$ena3d_ai_public_payload(built$value)
      leaked <- audit_tree_contains_text(payload, identifier_markers)
      list(
        guard_classifies_name = isTRUE(guard_classifies_name),
        build_rejected = FALSE,
        expected_privacy_rejection = FALSE,
        payload_contains_synthetic_identifiers = leaked,
        raw_rows_declared = isTRUE(payload$privacy$raw_rows_included)
      )
    }
  }, project_root)

  if (!is.null(captured$error)) {
    return(audit_finding(
      id, "Identifier-like Name values can cross the AI Change boundary",
      "S1", "high", "AI privacy", "detector_error",
      "Ten synthetic rows with two identifier-like Name labels and five rows per label.",
      "The identifier-like variable is rejected before evidence construction.",
      "The detector could not complete.", list(),
      "R/ai_evidence.R::.ena3d_ai_is_identifier_name and .ena3d_ai_build_change",
      "A privacy-boundary regression may expose participant identifiers to a provider.",
      "Build Change evidence with a Name column at the cell threshold and assert rejection plus absence of every synthetic identifier from the public payload.",
      seed, captured$error
    ))
  }

  result <- captured$value
  reproduced <- !isTRUE(result$build_rejected) &&
    isTRUE(result$payload_contains_synthetic_identifiers)
  resolved <- isTRUE(result$expected_privacy_rejection) ||
    (!isTRUE(result$build_rejected) &&
       !isTRUE(result$payload_contains_synthetic_identifiers) &&
       !isTRUE(result$raw_rows_declared))
  status <- if (reproduced) "reproduced" else if (resolved) {
    "not_reproduced"
  } else "detector_error"
  detector_error <- if (identical(status, "detector_error")) {
    list(
      class = "audit_inconclusive",
      message = "The evidence build outcome did not satisfy the vulnerable or protected contract."
    )
  } else NULL

  audit_finding(
    id, "Identifier-like Name values can cross the AI Change boundary",
    "S1", "high", "AI privacy", status,
    "Ten synthetic rows with two identifier-like Name labels and five rows per label.",
    "The identifier-like variable is rejected before evidence construction.",
    if (reproduced) {
      "Change evidence succeeded and its public payload contained synthetic participant labels."
    } else {
      "The public evidence boundary did not expose the synthetic participant labels."
    },
    list(
      name_is_guarded_as_identifier = isTRUE(result$guard_classifies_name),
      evidence_build_rejected = isTRUE(result$build_rejected),
      synthetic_identifier_values_in_public_payload =
        isTRUE(result$payload_contains_synthetic_identifiers),
      raw_rows_declared_in_public_payload = isTRUE(result$raw_rows_declared),
      fingerprint_helpers_mocked = TRUE,
      provider_transport_loaded = FALSE,
      provider_calls = 0L,
      warning_count = length(captured$warnings)
    ),
    "R/ai_evidence.R::.ena3d_ai_is_identifier_name and .ena3d_ai_build_change",
    "A privacy-boundary regression may expose participant identifiers to a provider.",
    "Build Change evidence with a Name column at the cell threshold and assert rejection plus absence of every synthetic identifier from the public payload.",
    seed, detector_error
  )
}


audit_probe_tuple_key_collision <- function(project_root, seed) {
  id <- "ENA-BUG-002"
  captured <- audit_capture({
    raw_import <- audit_source_environment(
      project_root,
      c("R/security_utils.R", "R/app_utils.R", "R/raw_data_import.R")
    )
    fixture <- data.frame(
      unit_part_a = c(paste0("alpha", "\r", "beta"), "alpha"),
      unit_part_b = c("gamma", paste0("beta", "\r", "gamma")),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    keys <- raw_import$ena3d_unit_key(
      fixture, c("unit_part_a", "unit_part_b")
    )
    list(
      distinct_tuple_count = nrow(unique(fixture)),
      distinct_encoded_key_count = length(unique(keys)),
      collision = length(unique(keys)) < nrow(unique(fixture))
    )
  }, project_root)

  if (!is.null(captured$error)) {
    return(audit_finding(
      id, "Carriage-return tuple-key collision merges distinct raw units",
      "S1", "high", "raw data ingestion", "detector_error",
      "Two distinct synthetic two-column tuples with the same separator-joined byte stream.",
      "Every distinct unit tuple receives a distinct internal key.",
      "The detector could not complete.", list(),
      "R/raw_data_import.R::ena3d_unit_key",
      "Distinct ENA units can be merged, corrupting grouping and downstream numerical results.",
      "Construct adversarial multi-column unit tuples containing carriage returns and assert injective keys or rejection before commit.",
      seed, captured$error
    ))
  }

  result <- captured$value
  reproduced <- isTRUE(result$collision)
  audit_finding(
    id, "Carriage-return tuple-key collision merges distinct raw units",
    "S1", "high", "raw data ingestion",
    if (reproduced) "reproduced" else "not_reproduced",
    "Two distinct synthetic two-column tuples with the same separator-joined byte stream.",
    "Every distinct unit tuple receives a distinct internal key.",
    if (reproduced) {
      "Two distinct tuples produced one internal key."
    } else {
      "The two distinct tuples retained distinct internal keys or were rejected."
    },
    list(
      distinct_tuple_count = as.integer(result$distinct_tuple_count),
      distinct_encoded_key_count =
        as.integer(result$distinct_encoded_key_count),
      key_collision = reproduced,
      warning_count = length(captured$warnings)
    ),
    "R/raw_data_import.R::ena3d_unit_key",
    "Distinct ENA units can be merged, corrupting grouping and downstream numerical results.",
    "Construct adversarial multi-column unit tuples containing carriage returns and assert injective keys or rejection before commit.",
    seed
  )
}


audit_probe_formula_identifier <- function(project_root, seed) {
  id <- "ENA-BUG-003"
  captured <- audit_capture({
    formula_helpers <- audit_source_environment(
      project_root,
      c("R/security_utils.R", "R/app_utils.R", "R/ena3d_exchange.R")
    )
    identifiers <- c(
      backtick = paste0("audit", "`", "dimension"),
      backslash = paste0("audit", "\\", "dimension")
    )
    accepted <- vapply(names(identifiers), function(kind) {
      value <- identifiers[[kind]]
      identical(
        formula_helpers$ena3d_exchange_scalar_string(
          value, "synthetic dimension", identifier = TRUE
        ),
        value
      )
    }, logical(1L))
    formula_results <- lapply(identifiers, function(identifier) {
      attempt <- audit_capture(
        formula_helpers$tilde_var_or_null(identifier), project_root
      )
      list(
        errored = !is.null(attempt$error),
        exact_round_trip = is.null(attempt$error) &&
          identical(all.vars(attempt$value), unname(identifier))
      )
    })
    list(
      exchange_accepts_backtick = isTRUE(accepted[["backtick"]]),
      exchange_accepts_backslash = isTRUE(accepted[["backslash"]]),
      backtick_formula_errored =
        isTRUE(formula_results$backtick$errored),
      backtick_formula_exact =
        isTRUE(formula_results$backtick$exact_round_trip),
      backslash_formula_errored =
        isTRUE(formula_results$backslash$errored),
      backslash_formula_exact =
        isTRUE(formula_results$backslash$exact_round_trip)
    )
  }, project_root)

  if (!is.null(captured$error)) {
    return(audit_finding(
      id, "Accepted dimension identifiers can fail or misresolve as formulas",
      "S1", "high", "plot formula construction", "detector_error",
      "Two accepted synthetic dimension identifiers: one containing a backtick and one containing a backslash.",
      "Every accepted dimension identifier becomes a formula referring to that exact column.",
      "The detector could not complete.", list(),
      "R/app_utils.R::tilde_var_or_null",
      "A valid imported dataset can crash a plot or silently bind the wrong dimension.",
      "For accepted identifiers containing quoting characters, assert formula creation succeeds and all.vars() exactly equals the original identifier.",
      seed, captured$error
    ))
  }

  result <- captured$value
  vulnerable_backtick <- isTRUE(result$exchange_accepts_backtick) &&
    (isTRUE(result$backtick_formula_errored) ||
       !isTRUE(result$backtick_formula_exact))
  vulnerable_backslash <- isTRUE(result$exchange_accepts_backslash) &&
    (isTRUE(result$backslash_formula_errored) ||
       !isTRUE(result$backslash_formula_exact))
  reproduced <- vulnerable_backtick || vulnerable_backslash
  audit_finding(
    id, "Accepted dimension identifiers can fail or misresolve as formulas",
    "S1", "high", "plot formula construction",
    if (reproduced) "reproduced" else "not_reproduced",
    "Two accepted synthetic dimension identifiers: one containing a backtick and one containing a backslash.",
    "Every accepted dimension identifier becomes a formula referring to that exact column.",
    if (reproduced) {
      "At least one accepted identifier caused a parse failure or resolved to a different column name."
    } else {
      "Both identifiers were rejected at ingestion or round-tripped through formula construction exactly."
    },
    list(
      exchange_accepts_backtick_identifier =
        isTRUE(result$exchange_accepts_backtick),
      backtick_formula_parse_error =
        isTRUE(result$backtick_formula_errored),
      backtick_formula_exact_round_trip =
        isTRUE(result$backtick_formula_exact),
      exchange_accepts_backslash_identifier =
        isTRUE(result$exchange_accepts_backslash),
      backslash_formula_parse_error =
        isTRUE(result$backslash_formula_errored),
      backslash_formula_exact_round_trip =
        isTRUE(result$backslash_formula_exact),
      warning_count = length(captured$warnings)
    ),
    "R/app_utils.R::tilde_var_or_null",
    "A valid imported dataset can crash a plot or silently bind the wrong dimension.",
    "For accepted identifiers containing quoting characters, assert formula creation succeeds and all.vars() exactly equals the original identifier.",
    seed
  )
}


audit_probe_dst_pair_collision <- function(project_root, seed) {
  id <- "ENA-BUG-004"
  captured <- audit_capture({
    stats_helpers <- audit_source_environment(
      project_root,
      c("R/app_utils.R", "R/app_module_stats.R")
    )
    fold_ids <- as.POSIXct(
      c(1730611800, 1730615400),
      origin = "1970-01-01",
      tz = "America/New_York"
    )
    exact_count <- length(unique(as.numeric(fold_ids)))
    rendered_count <- length(unique(as.character(fold_ids)))
    points <- data.frame(
      condition = rep(c("before", "after"), each = 2L),
      pair_id = rep(fold_ids, 2L),
      MR1 = c(2, 5, 1, 3),
      stringsAsFactors = FALSE
    )
    matched <- audit_capture(
      stats_helpers$ena3d_match_pairs(
        points, "condition", "before", "after", "pair_id", "MR1"
      ),
      project_root
    )
    list(
      exact_identifier_count = exact_count,
      rendered_identifier_count = rendered_count,
      matcher_errored = !is.null(matched$error),
      matcher_reported_duplicate = !is.null(matched$error) &&
        grepl("Duplicate IDs", matched$error$message, fixed = TRUE),
      matched_pair_count = if (is.null(matched$error)) {
        as.integer(matched$value$n_pairs)
      } else NA_integer_
    )
  }, project_root)

  if (!is.null(captured$error)) {
    return(audit_finding(
      id, "Distinct POSIXct pairing IDs collide during a daylight-saving fold",
      "S1", "high", "Stats paired analysis", "detector_error",
      "Two exact synthetic POSIXct instants sharing one local wall-clock label, repeated across two conditions.",
      "The two exact instants remain distinct and form two matched pairs.",
      "The detector could not complete.", list(),
      "R/app_module_stats.R::ena3d_match_pairs",
      "Valid repeated-measures data can be rejected or paired incorrectly.",
      "Use exact typed keys for POSIXct pairing IDs and assert both instants in a daylight-saving fold form separate matched pairs.",
      seed, captured$error
    ))
  }

  result <- captured$value
  platform_can_exercise_fold <-
    identical(result$exact_identifier_count, 2L) &&
    identical(result$rendered_identifier_count, 1L)
  if (!platform_can_exercise_fold) {
    return(audit_finding(
      id, "Distinct POSIXct pairing IDs collide during a daylight-saving fold",
      "S1", "high", "Stats paired analysis", "detector_error",
      "Two exact synthetic POSIXct instants sharing one local wall-clock label, repeated across two conditions.",
      "The two exact instants remain distinct and form two matched pairs.",
      "The host timezone database could not construct the required fold fixture.",
      list(
        exact_identifier_count = as.integer(result$exact_identifier_count),
        rendered_identifier_count =
          as.integer(result$rendered_identifier_count)
      ),
      "R/app_module_stats.R::ena3d_match_pairs",
      "Valid repeated-measures data can be rejected or paired incorrectly.",
      "Use exact typed keys for POSIXct pairing IDs and assert both instants in a daylight-saving fold form separate matched pairs.",
      seed,
      list(
        class = "audit_fixture_unavailable",
        message = "The platform did not render the selected DST-fold instants as one wall-clock label."
      )
    ))
  }

  reproduced <- isTRUE(result$matcher_errored) ||
    !identical(result$matched_pair_count, 2L)
  audit_finding(
    id, "Distinct POSIXct pairing IDs collide during a daylight-saving fold",
    "S1", "high", "Stats paired analysis",
    if (reproduced) "reproduced" else "not_reproduced",
    "Two exact synthetic POSIXct instants sharing one local wall-clock label, repeated across two conditions.",
    "The two exact instants remain distinct and form two matched pairs.",
    if (reproduced) {
      "The matcher rejected or failed to retain both exact pairs after character key conversion."
    } else {
      "The matcher retained two distinct exact pairs."
    },
    list(
      exact_identifier_count = as.integer(result$exact_identifier_count),
      rendered_identifier_count = as.integer(result$rendered_identifier_count),
      matcher_errored = isTRUE(result$matcher_errored),
      matcher_reported_duplicate =
        isTRUE(result$matcher_reported_duplicate),
      matched_pair_count = result$matched_pair_count,
      warning_count = length(captured$warnings)
    ),
    "R/app_module_stats.R::ena3d_match_pairs",
    "Valid repeated-measures data can be rejected or paired incorrectly.",
    "Use exact typed keys for POSIXct pairing IDs and assert both instants in a daylight-saving fold form separate matched pairs.",
    seed
  )
}


audit_probe_trajectory_condition_collision <- function(project_root, seed) {
  id <- "ENA-BUG-005"
  captured <- audit_capture({
    trajectory <- audit_source_environment(
      project_root,
      c(
        "R/app_utils.R", "R/trajectory_analysis.R",
        "R/app_module_trajectory.R"
      )
    )
    distinct_values <- c(1, 1 + .Machine$double.eps)
    points <- data.frame(
      condition = distinct_values,
      participant = c("AUDIT-A", "AUDIT-B"),
      stringsAsFactors = FALSE
    )
    rendered <- as.character(points$condition)
    choices <- trajectory$.trajectory_condition_values(points, "condition")
    selected_rows <- !is.na(points$condition) &
      as.character(points$condition) == choices[[1L]]
    list(
      exact_condition_count = length(unique(points$condition)),
      rendered_condition_count = length(unique(rendered)),
      ui_choice_count = length(choices),
      rows_selected_by_single_choice = sum(selected_rows)
    )
  }, project_root)

  if (!is.null(captured$error)) {
    return(audit_finding(
      id, "Distinct typed trajectory conditions collapse to one UI value",
      "S1", "high", "trajectory condition filtering", "detector_error",
      "Two adjacent representable synthetic doubles used as condition values.",
      "Each distinct typed value has a distinct stable selection token and filters only its own rows.",
      "The detector could not complete.", list(),
      "R/app_module_trajectory.R::.trajectory_condition_values and comparison filtering",
      "A valid comparison can merge distinct cohorts and produce scientifically incorrect trajectories or statistics.",
      "Create two adjacent-double condition levels and assert two choices, typed round-trip selection, and disjoint row masks.",
      seed, captured$error
    ))
  }

  result <- captured$value
  reproduced <- identical(result$exact_condition_count, 2L) &&
    identical(result$rendered_condition_count, 1L) &&
    identical(result$ui_choice_count, 1L) &&
    identical(result$rows_selected_by_single_choice, 2L)
  audit_finding(
    id, "Distinct typed trajectory conditions collapse to one UI value",
    "S1", "high", "trajectory condition filtering",
    if (reproduced) "reproduced" else "not_reproduced",
    "Two adjacent representable synthetic doubles used as condition values.",
    "Each distinct typed value has a distinct stable selection token and filters only its own rows.",
    if (reproduced) {
      "Character conversion exposed one choice whose comparison mask selected both exact values."
    } else {
      "The two exact values remained independently selectable and filterable."
    },
    list(
      exact_condition_count = as.integer(result$exact_condition_count),
      rendered_condition_count =
        as.integer(result$rendered_condition_count),
      ui_choice_count = as.integer(result$ui_choice_count),
      rows_selected_by_single_choice =
        as.integer(result$rows_selected_by_single_choice),
      warning_count = length(captured$warnings)
    ),
    "R/app_module_trajectory.R::.trajectory_condition_values and comparison filtering",
    "A valid comparison can merge distinct cohorts and produce scientifically incorrect trajectories or statistics.",
    "Create two adjacent-double condition levels and assert two choices, typed round-trip selection, and disjoint row masks.",
    seed
  )
}


audit_probe_exchange_numeric_precision <- function(project_root, seed) {
  id <- "ENA-BUG-006"
  captured <- audit_capture({
    exchange <- audit_source_environment(
      project_root,
      c("R/security_utils.R", "R/app_utils.R", "R/ena3d_exchange.R")
    )
    original <- c(1, 1 + .Machine$double.eps)
    encoded <- exchange$ena3d_exchange_encode_column(
      original, "synthetic numeric identity", "synthetic numeric identity"
    )
    serialized <- jsonlite::toJSON(
      encoded,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      digits = exchange$ENA3D_EXCHANGE_JSON_DIGITS,
      pretty = FALSE
    )
    parsed <- jsonlite::parse_json(serialized, simplifyVector = FALSE)
    restored <- exchange$ena3d_exchange_decode_values(
      parsed$values, parsed$type, "synthetic numeric identity",
      specification = parsed
    )
    list(
      original_exact_value_count = length(unique(original)),
      restored_exact_value_count = length(unique(restored)),
      first_value_preserved = identical(original[[1L]], restored[[1L]]),
      second_value_preserved = identical(original[[2L]], restored[[2L]]),
      original_values_equal_after_round_trip = identical(original, restored)
    )
  }, project_root)

  if (!is.null(captured$error)) {
    return(audit_finding(
      id, "Exchange JSON collapses distinct adjacent numeric values",
      "S1", "high", "exchange typed-value integrity", "detector_error",
      "Two adjacent representable synthetic doubles encoded through the production JSON settings.",
      "Both finite values round-trip exactly or the writer rejects unsupported precision before publishing.",
      "The detector could not complete.", list(),
      "R/ena3d_exchange.R::ena3d_write_exchange_file numeric serialization",
      "Distinct numeric identifiers, conditions, or coordinates can merge or change after a valid exchange round trip.",
      "Round-trip adjacent IEEE-754 doubles (and difftime fractions) through a complete exchange file; assert bit-exact identity or explicit rejection before file publication.",
      seed, captured$error
    ))
  }

  result <- captured$value
  reproduced <- identical(result$original_exact_value_count, 2L) &&
    identical(result$restored_exact_value_count, 1L) &&
    !isTRUE(result$original_values_equal_after_round_trip)
  audit_finding(
    id, "Exchange JSON collapses distinct adjacent numeric values",
    "S1", "high", "exchange typed-value integrity",
    if (reproduced) "reproduced" else "not_reproduced",
    "Two adjacent representable synthetic doubles encoded through the production JSON settings.",
    "Both finite values round-trip exactly or the writer rejects unsupported precision before publishing.",
    if (reproduced) {
      "The second value serialized as the first, reducing two exact numeric identities to one."
    } else {
      "The two exact values remained distinct or were rejected before publication."
    },
    list(
      original_exact_value_count =
        as.integer(result$original_exact_value_count),
      restored_exact_value_count =
        as.integer(result$restored_exact_value_count),
      first_value_preserved = isTRUE(result$first_value_preserved),
      second_value_preserved = isTRUE(result$second_value_preserved),
      exact_vector_identity =
        isTRUE(result$original_values_equal_after_round_trip),
      warning_count = length(captured$warnings)
    ),
    "R/ena3d_exchange.R::ena3d_write_exchange_file numeric serialization",
    "Distinct numeric identifiers, conditions, or coordinates can merge or change after a valid exchange round trip.",
    "Round-trip adjacent IEEE-754 doubles (and difftime fractions) through a complete exchange file; assert bit-exact identity or explicit rejection before file publication.",
    seed
  )
}


audit_probe_csv_control_round_trip <- function(project_root, seed) {
  id <- "ENA-BUG-007"
  captured <- audit_capture({
    security <- audit_source_environment(project_root, "R/security_utils.R")
    original <- data.frame(
      value = c("synthetic carriage\rreturn", "synthetic carriage\nreturn"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    path <- tempfile(fileext = ".csv")
    on.exit(unlink(path), add = TRUE)
    write_error <- tryCatch({
      security$ena3d_write_safe_csv(original, path)
      NULL
    }, error = function(condition) condition)
    if (!is.null(write_error)) {
      list(
        original_distinct_value_count = length(unique(original$value)),
        restored_distinct_value_count = NA_integer_,
        exact_round_trip = FALSE,
        rejected_before_write = grepl(
          "cannot preserve carriage-return or newline",
          conditionMessage(write_error), fixed = TRUE
        ),
        partial_file_published = file.exists(path)
      )
    } else {
      restored <- utils::read.csv(
        path,
        check.names = FALSE,
        stringsAsFactors = FALSE,
        fileEncoding = "UTF-8"
      )
      list(
        original_distinct_value_count = length(unique(original$value)),
        restored_distinct_value_count = length(unique(restored$value)),
        exact_round_trip = identical(original, restored),
        rejected_before_write = FALSE,
        partial_file_published = FALSE
      )
    }
  }, project_root)

  if (!is.null(captured$error)) {
    return(audit_finding(
      id, "CSV round trips collapse carriage-return and newline cell values",
      "S2", "high", "CSV download integrity", "detector_error",
      "Two synthetic text cells differing only by carriage return versus newline.",
      "Both accepted values round-trip distinctly or the writer rejects the unsupported control character.",
      "The detector could not complete.", list(),
      "R/security_utils.R::ena3d_write_safe_csv control-character handling",
      "Downloaded CSV data can merge distinct accepted text values in common R re-import paths.",
      "Export carriage-return and newline variants, re-import the CSV, and assert exact distinct values or rejection before writing.",
      seed, captured$error
    ))
  }

  result <- captured$value
  reproduced <- identical(result$original_distinct_value_count, 2L) &&
    identical(result$restored_distinct_value_count, 1L) &&
    !isTRUE(result$exact_round_trip)
  resolved <- isTRUE(result$exact_round_trip) ||
    (isTRUE(result$rejected_before_write) &&
       !isTRUE(result$partial_file_published))
  audit_finding(
    id, "CSV round trips collapse carriage-return and newline cell values",
    "S2", "high", "CSV download integrity",
    if (reproduced) "reproduced" else if (resolved) {
      "not_reproduced"
    } else "detector_error",
    "Two synthetic text cells differing only by carriage return versus newline.",
    "Both accepted values round-trip distinctly or the writer rejects the unsupported control character.",
    if (reproduced) {
      "The carriage return was normalized to newline on re-import, collapsing two values to one."
    } else {
      "The two values remained distinct or the unsupported value was rejected."
    },
    list(
      original_distinct_value_count =
        as.integer(result$original_distinct_value_count),
      restored_distinct_value_count =
        as.integer(result$restored_distinct_value_count),
      exact_round_trip = isTRUE(result$exact_round_trip),
      rejected_before_write = isTRUE(result$rejected_before_write),
      partial_file_published = isTRUE(result$partial_file_published),
      warning_count = length(captured$warnings)
    ),
    "R/security_utils.R::ena3d_write_safe_csv control-character handling",
    "Downloaded CSV data can merge distinct accepted text values in common R re-import paths.",
    "Export carriage-return and newline variants, re-import the CSV, and assert exact distinct values or rejection before writing.",
    seed
  )
}


audit_probe_network_no_selection_render <- function(project_root, seed) {
  id <- "ENA-BUG-010"
  captured <- audit_capture({
    helpers <- audit_source_environment(project_root, "R/app_utils.R")
    helpers$add_trace <- plotly::add_trace
    helpers$add_text <- plotly::add_text
    helpers$`%>%` <- magrittr::`%>%`
    axis_result <- helpers$add_x_3d_axis(NULL)
    typography_result <- audit_capture(
      helpers$ena3d_apply_plotly_typography(axis_result),
      project_root
    )
    list(
      post_axis_class = class(axis_result)[[1L]],
      typography_errored = !is.null(typography_result$error),
      layout_dispatch_error = !is.null(typography_result$error) &&
        grepl("no applicable method for 'layout'", typography_result$error$message,
              fixed = TRUE)
    )
  }, project_root)

  if (!is.null(captured$error)) {
    return(audit_finding(
      id, "The default No Network state raises a Plotly render error",
      "S1", "high", "Network reactive rendering", "detector_error",
      "A valid loaded dataset with the default No Network selection and an enabled axis overlay.",
      "The Network surface renders an intentional empty state without a Shiny output error.",
      "The detector could not complete.", list(),
      "R/app_module_network.R::ena_network_plot_output unconditional post-processing",
      "The default valid Network state emits a server-side render failure and can leave the plot output broken.",
      "Load a trusted dataset, retain No Network with axis toggles enabled, and assert no shiny-output-error, no layout dispatch warning, and a usable empty-state surface.",
      seed, captured$error
    ))
  }

  result <- captured$value
  reproduced <- identical(result$post_axis_class, "list") &&
    isTRUE(result$typography_errored) &&
    isTRUE(result$layout_dispatch_error)
  audit_finding(
    id, "The default No Network state raises a Plotly render error",
    "S1", "high", "Network reactive rendering",
    if (reproduced) "reproduced" else "not_reproduced",
    "A valid loaded dataset with the default No Network selection and an enabled axis overlay.",
    "The Network surface renders an intentional empty state without a Shiny output error.",
    if (reproduced) {
      "Axis post-processing converted the absent plot to a plain list, then Plotly layout dispatch failed."
    } else {
      "The absent plot was handled as an intentional empty state."
    },
    list(
      post_axis_class = result$post_axis_class,
      typography_errored = isTRUE(result$typography_errored),
      layout_dispatch_error = isTRUE(result$layout_dispatch_error),
      warning_count = length(captured$warnings)
    ),
    "R/app_module_network.R::ena_network_plot_output unconditional post-processing",
    "The default valid Network state emits a server-side render failure and can leave the plot output broken.",
    "Load a trusted dataset, retain No Network with axis toggles enabled, and assert no shiny-output-error, no layout dispatch warning, and a usable empty-state surface.",
    seed
  )
}


audit_numerical_fixture <- function() {
  points <- expand.grid(
    participant_number = seq_len(8L),
    wave = seq_len(3L),
    condition = c("A", "B"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  points$participant <- paste0(
    points$condition, "-", points$participant_number
  )
  shift <- ifelse(points$condition == "B", 0.75, 0)
  points$x <- points$participant_number + points$wave + shift
  points$y <- points$participant_number / 3 - 2 * points$wave - shift
  points$z <- points$participant_number * 0.2 + points$wave^2 + 2 * shift
  points[c("participant", "wave", "condition", "x", "y", "z")]
}


audit_probe_trajectory_invariance <- function(project_root, seed) {
  id <- "ENA-PROP-001"
  captured <- audit_capture({
    trajectory <- audit_source_environment(
      project_root, "R/trajectory_analysis.R"
    )
    points <- audit_numerical_fixture()
    shuffled_index <- order(
      (seq_len(nrow(points)) * 37L) %% 101L,
      method = "radix"
    )
    original <- trajectory$compute_centroid_path(
      points, "wave", "participant", group_vars = "condition",
      dimensions = c("x", "y", "z"), order = 1:3,
      cohort_policy = "complete", na_policy = "error"
    )
    shuffled <- trajectory$compute_centroid_path(
      points[shuffled_index, , drop = FALSE],
      "wave", "participant", group_vars = "condition",
      dimensions = c("x", "y", "z"), order = 1:3,
      cohort_policy = "complete", na_policy = "error"
    )
    permuted_axes <- trajectory$compute_centroid_path(
      points, "wave", "participant", group_vars = "condition",
      dimensions = c("z", "x", "y"), order = 1:3,
      cohort_policy = "complete", na_policy = "error"
    )
    comparable <- c(
      "condition", "wave", "n_used", "centroid_x", "centroid_y",
      "centroid_z", "step_distance", "cumulative_distance"
    )
    canonical_rows <- function(path) {
      path[
        order(as.character(path$condition), path$wave, method = "radix"),
        comparable,
        drop = FALSE
      ]
    }
    list(
      row_order_invariant = isTRUE(all.equal(
        as.data.frame(canonical_rows(original)),
        as.data.frame(canonical_rows(shuffled)),
        tolerance = 1e-14,
        check.attributes = FALSE
      )),
      axis_order_distance_invariant = isTRUE(all.equal(
        original[c("step_distance", "cumulative_distance")],
        permuted_axes[c("step_distance", "cumulative_distance")],
        tolerance = 1e-14,
        check.attributes = FALSE
      )),
      path_rows = nrow(original),
      all_centroids_finite = all(is.finite(unlist(
        original[c("centroid_x", "centroid_y", "centroid_z")],
        use.names = FALSE
      )))
    )
  }, project_root)

  if (!is.null(captured$error)) {
    return(audit_probe(
      id, "Centroid paths are invariant to row and selected-axis order",
      "trajectory numerics", "detector_error", list(), seed,
      captured$error
    ))
  }
  result <- captured$value
  passed <- isTRUE(result$row_order_invariant) &&
    isTRUE(result$axis_order_distance_invariant) &&
    isTRUE(result$all_centroids_finite) &&
    length(captured$warnings) == 0L
  audit_probe(
    id, "Centroid paths are invariant to row and selected-axis order",
    "trajectory numerics", if (passed) "passed" else "failed",
    list(
      row_order_invariant = isTRUE(result$row_order_invariant),
      selected_axis_order_distance_invariant =
        isTRUE(result$axis_order_distance_invariant),
      numeric_tolerance = 1e-14,
      all_centroids_finite = isTRUE(result$all_centroids_finite),
      path_rows = as.integer(result$path_rows),
      warning_count = length(captured$warnings)
    ),
    seed
  )
}


audit_probe_bootstrap_replay <- function(project_root, seed) {
  id <- "ENA-PROP-002"
  captured <- audit_capture({
    trajectory <- audit_source_environment(
      project_root, "R/trajectory_analysis.R"
    )
    points <- audit_numerical_fixture()
    set.seed(as.integer((seed %% 1000000L) + 911L))
    rng_before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    first <- trajectory$bootstrap_centroid_path(
      points, "wave", "participant", group_vars = "condition",
      dimensions = c("x", "y", "z"), order = 1:3,
      cohort_policy = "complete", na_policy = "error",
      n_boot = 59L, conf_level = 0.80, seed = seed,
      bootstrap_design = "stratified"
    )
    rng_after_first <- get(
      ".Random.seed", envir = .GlobalEnv, inherits = FALSE
    )
    second <- trajectory$bootstrap_centroid_path(
      points, "wave", "participant", group_vars = "condition",
      dimensions = c("x", "y", "z"), order = 1:3,
      cohort_policy = "complete", na_policy = "error",
      n_boot = 59L, conf_level = 0.80, seed = seed,
      bootstrap_design = "stratified"
    )
    rng_after_second <- get(
      ".Random.seed", envir = .GlobalEnv, inherits = FALSE
    )
    bootstrap_columns <- grep(
      "(_lower|_upper|_boot_n)$", names(first), value = TRUE
    )
    list(
      seeded_results_identical = identical(
        as.data.frame(first[bootstrap_columns]),
        as.data.frame(second[bootstrap_columns])
      ),
      rng_restored_after_first = identical(rng_before, rng_after_first),
      rng_restored_after_second = identical(rng_before, rng_after_second),
      finite_interval_count = sum(vapply(
        first[grep("(_lower|_upper)$", names(first), value = TRUE)],
        function(values) sum(is.finite(values)),
        integer(1L)
      )),
      failed_replicates = as.integer(
        attr(first, "bootstrap_spec")$failed_replicates
      )
    )
  }, project_root)

  if (!is.null(captured$error)) {
    return(audit_probe(
      id, "Seeded bootstrap is deterministic and restores caller RNG state",
      "trajectory bootstrap", "detector_error", list(), seed,
      captured$error
    ))
  }
  result <- captured$value
  passed <- isTRUE(result$seeded_results_identical) &&
    isTRUE(result$rng_restored_after_first) &&
    isTRUE(result$rng_restored_after_second) &&
    result$finite_interval_count > 0L &&
    identical(result$failed_replicates, 0L) &&
    length(captured$warnings) == 0L
  audit_probe(
    id, "Seeded bootstrap is deterministic and restores caller RNG state",
    "trajectory bootstrap", if (passed) "passed" else "failed",
    list(
      seeded_results_identical = isTRUE(result$seeded_results_identical),
      rng_restored_after_first = isTRUE(result$rng_restored_after_first),
      rng_restored_after_second = isTRUE(result$rng_restored_after_second),
      finite_interval_count = as.integer(result$finite_interval_count),
      failed_replicates = as.integer(result$failed_replicates),
      bootstrap_replicates = 59L,
      warning_count = length(captured$warnings)
    ),
    seed
  )
}


audit_probe_distance_space <- function(project_root, seed) {
  id <- "ENA-PROP-003"
  captured <- audit_capture({
    trajectory <- audit_source_environment(
      project_root, "R/trajectory_analysis.R"
    )
    points <- data.frame(
      participant = rep(c("p1", "p2"), each = 2L),
      wave = rep(1:2, 2L),
      x = c(0, 3, 2, 5),
      y = c(0, 4, 2, 6),
      z = c(0, 12, 2, 14),
      stringsAsFactors = FALSE
    )
    selected <- trajectory$compute_centroid_path(
      points, "wave", "participant", dimensions = c("x", "y"),
      order = 1:2, distance_space = "selected"
    )
    full <- trajectory$compute_centroid_path(
      points, "wave", "participant", dimensions = c("x", "y"),
      order = 1:2, distance_space = "full",
      full_dimensions = c("x", "y", "z")
    )
    list(
      selected_distance = unname(selected$step_distance[[2L]]),
      full_distance = unname(full$step_distance[[2L]]),
      selected_expected = isTRUE(all.equal(
        selected$step_distance[[2L]], 5, tolerance = 1e-14
      )),
      full_expected = isTRUE(all.equal(
        full$step_distance[[2L]], 13, tolerance = 1e-14
      ))
    )
  }, project_root)

  if (!is.null(captured$error)) {
    return(audit_probe(
      id, "Selected and full rotation spaces produce the declared distances",
      "trajectory distance", "detector_error", list(), seed,
      captured$error
    ))
  }
  result <- captured$value
  passed <- isTRUE(result$selected_expected) &&
    isTRUE(result$full_expected) && length(captured$warnings) == 0L
  audit_probe(
    id, "Selected and full rotation spaces produce the declared distances",
    "trajectory distance", if (passed) "passed" else "failed",
    list(
      selected_space_distance = result$selected_distance,
      full_space_distance = result$full_distance,
      selected_space_expected = isTRUE(result$selected_expected),
      full_space_expected = isTRUE(result$full_expected),
      warning_count = length(captured$warnings)
    ),
    seed
  )
}


audit_probe_p_adjustment <- function(project_root, seed) {
  id <- "ENA-PROP-004"
  captured <- audit_capture({
    stats_helpers <- audit_source_environment(
      project_root,
      c("R/app_utils.R", "R/app_module_stats.R")
    )
    raw <- c(0.001, 0.02, 0.04, NA_real_)
    results <- lapply(raw, function(value) {
      if (is.na(value)) structure(list(message = "synthetic"), class = "error")
      else list(p_value = value)
    })
    observed <- stats_helpers$ena3d_adjust_p_values(results, "holm")
    expected <- c(stats::p.adjust(raw[is.finite(raw)], "holm"), NA_real_)
    list(
      matches_reference = isTRUE(all.equal(
        observed, expected, tolerance = 0, check.attributes = FALSE
      )),
      missing_result_remains_missing = is.na(observed[[4L]]),
      adjusted_count = sum(is.finite(observed))
    )
  }, project_root)

  if (!is.null(captured$error)) {
    return(audit_probe(
      id, "Multiplicity correction matches the independent stats reference",
      "Stats multiplicity", "detector_error", list(), seed,
      captured$error
    ))
  }
  result <- captured$value
  passed <- isTRUE(result$matches_reference) &&
    isTRUE(result$missing_result_remains_missing) &&
    length(captured$warnings) == 0L
  audit_probe(
    id, "Multiplicity correction matches the independent stats reference",
    "Stats multiplicity", if (passed) "passed" else "failed",
    list(
      matches_stats_reference = isTRUE(result$matches_reference),
      missing_result_remains_missing =
        isTRUE(result$missing_result_remains_missing),
      adjusted_value_count = as.integer(result$adjusted_count),
      warning_count = length(captured$warnings)
    ),
    seed
  )
}


audit_system_output <- function(command, arguments = character()) {
  if (!nzchar(Sys.which(command))) return(NULL)
  output <- suppressWarnings(tryCatch(
    system2(command, arguments, stdout = TRUE, stderr = FALSE),
    error = function(condition) {
      invisible(condition)
      character()
    }
  ))
  status <- attr(output, "status", exact = TRUE)
  if (!is.null(status) && status != 0L) return(NULL)
  output <- trimws(as.character(output))
  output <- output[nzchar(output)]
  if (length(output)) output[[1L]] else NULL
}


audit_file_sha256 <- function(path) {
  if (!file.exists(path) || isTRUE(file.info(path)$isdir)) return(NULL)
  commands <- list(
    list(command = "shasum", arguments = c("-a", "256", shQuote(path))),
    list(command = "sha256sum", arguments = shQuote(path)),
    list(command = "openssl", arguments = c("dgst", "-sha256", shQuote(path)))
  )
  for (candidate in commands) {
    output <- audit_system_output(candidate$command, candidate$arguments)
    if (is.null(output)) next
    matches <- regmatches(
      output,
      regexpr("[0-9A-Fa-f]{64}", output, perl = TRUE)
    )
    if (length(matches) && nzchar(matches[[1L]])) {
      return(tolower(matches[[1L]]))
    }
  }
  NULL
}


audit_renv_status <- function(project_root) {
  if (!requireNamespace("renv", quietly = TRUE)) {
    return(list(
      available = FALSE,
      synchronized = FALSE,
      locked_package_count = 0L,
      missing_package_count = NA_integer_,
      version_mismatch_count = NA_integer_
    ))
  }
  result <- tryCatch({
    lock <- renv::lockfile_read(file.path(project_root, "renv.lock"))
    packages <- lock$Packages
    package_names <- names(packages)
    expected_versions <- vapply(packages, function(record) {
      if (is.null(record$Version)) NA_character_ else as.character(record$Version)
    }, character(1L))
    visible_versions <- vapply(package_names, function(package_name) {
      tryCatch(
        as.character(utils::packageVersion(package_name)),
        error = function(condition) {
          invisible(condition)
          NA_character_
        }
      )
    }, character(1L))
    missing <- is.na(visible_versions)
    mismatch <- !missing & !is.na(expected_versions)
    mismatch_indices <- which(mismatch)
    mismatch[mismatch_indices] <- vapply(mismatch_indices, function(index) {
      utils::compareVersion(
        visible_versions[[index]],
        expected_versions[[index]]
      ) != 0L
    }, logical(1L))
    list(
      available = TRUE,
      synchronized = !any(missing) && !any(mismatch),
      locked_package_count = as.integer(length(package_names)),
      missing_package_count = as.integer(sum(missing)),
      version_mismatch_count = as.integer(sum(mismatch))
    )
  }, error = function(condition) {
    invisible(condition)
    list(
      available = TRUE,
      synchronized = FALSE,
      locked_package_count = 0L,
      missing_package_count = NA_integer_,
      version_mismatch_count = NA_integer_
    )
  })
  result
}


audit_baseline <- function(project_root, mode, seed) {
  git_arguments <- c("-C", shQuote(project_root))
  commit <- audit_system_output("git", c(git_arguments, "rev-parse", "HEAD"))
  branch <- audit_system_output(
    "git", c(git_arguments, "branch", "--show-current")
  )
  dirty_output <- if (nzchar(Sys.which("git"))) {
    suppressWarnings(tryCatch(
      system2(
        "git",
        c(git_arguments, "status", "--porcelain"),
        stdout = TRUE,
        stderr = FALSE
      ),
      error = function(condition) {
        invisible(condition)
        character()
      }
    ))
  } else character()
  source_paths <- c(
    "R/ai_evidence.R",
    "R/app_utils.R",
    "R/app_module_stats.R",
    "R/app_module_network.R",
    "R/app_module_trajectory.R",
    "R/raw_data_import.R",
    "R/security_utils.R",
    "R/ena3d_exchange.R",
    "R/trajectory_analysis.R",
    "tests/audit/audit_harness.R",
    "tools/run_bug_audit.R"
  )
  source_hashes <- lapply(source_paths, function(relative_path) {
    audit_file_sha256(file.path(project_root, relative_path))
  })
  names(source_hashes) <- source_paths
  package_names <- c("digest", "jsonlite", "rENA", "renv")
  package_versions <- lapply(package_names, function(package_name) {
    if (requireNamespace(package_name, quietly = TRUE)) {
      as.character(utils::packageVersion(package_name))
    } else NULL
  })
  names(package_versions) <- package_names
  system <- Sys.info()
  list(
    git = list(
      commit = if (!is.null(commit) && grepl("^[0-9a-f]{40}$", commit)) {
        commit
      } else NULL,
      branch = branch,
      tracked_worktree_dirty = length(dirty_output) > 0L
    ),
    runtime = list(
      r_version = paste(R.version$major, R.version$minor, sep = "."),
      node_version = audit_system_output("node", "--version"),
      os = unname(system[["sysname"]]),
      os_release = unname(system[["release"]]),
      architecture = unname(system[["machine"]]),
      timezone = Sys.timezone()
    ),
    locks = list(
      renv_lock_sha256 = audit_file_sha256(
        file.path(project_root, "renv.lock")
      ),
      package_lock_sha256 = audit_file_sha256(
        file.path(project_root, "package-lock.json")
      ),
      renv = audit_renv_status(project_root)
    ),
    packages = package_versions,
    source_sha256 = source_hashes,
    configuration = list(
      mode = mode,
      seed = as.integer(seed),
      ai_execution = "disabled",
      provider_transport = "not_loaded",
      fixture_data = "synthetic_only",
      live_services = "not_accessed",
      credential_sources = "not_accessed"
    )
  )
}


audit_check_registry <- function() {
  list(
    `ENA-BUG-001` = audit_probe_ai_name_leak,
    `ENA-BUG-002` = audit_probe_tuple_key_collision,
    `ENA-BUG-003` = audit_probe_formula_identifier,
    `ENA-BUG-004` = audit_probe_dst_pair_collision,
    `ENA-BUG-005` = audit_probe_trajectory_condition_collision,
    `ENA-BUG-006` = audit_probe_exchange_numeric_precision,
    `ENA-BUG-007` = audit_probe_csv_control_round_trip,
    `ENA-BUG-010` = audit_probe_network_no_selection_render,
    `ENA-PROP-001` = audit_probe_trajectory_invariance,
    `ENA-PROP-002` = audit_probe_bootstrap_replay,
    `ENA-PROP-003` = audit_probe_distance_space,
    `ENA-PROP-004` = audit_probe_p_adjustment
  )
}


audit_run <- function(project_root, mode = c("report-only", "strict"),
                      seed = .audit_default_seed, only = character()) {
  mode <- match.arg(mode)
  project_root <- normalizePath(project_root, mustWork = TRUE)
  seed <- suppressWarnings(as.integer(seed))
  if (length(seed) != 1L || is.na(seed) || seed < 1L) {
    stop("Audit seed must be one positive integer.", call. = FALSE)
  }
  registry <- audit_check_registry()
  required_ids <- names(registry)
  only <- unique(as.character(only))
  only <- only[!is.na(only) & nzchar(only)]
  unknown <- setdiff(only, required_ids)
  if (length(unknown)) {
    stop(sprintf(
      "Unknown audit check identifier(s): %s", paste(unknown, collapse = ", ")
    ), call. = FALSE)
  }
  selected_ids <- if (length(only)) only else required_ids
  baseline <- audit_baseline(project_root, mode, seed)
  checks <- lapply(selected_ids, function(id) {
    registry[[id]](project_root, seed)
  })
  names(checks) <- selected_ids
  findings <- unname(Filter(
    function(check) identical(check$kind, "seeded_finding"), checks
  ))
  properties <- unname(Filter(
    function(check) identical(check$kind, "property_probe"), checks
  ))

  incomplete_reasons <- character()
  if (!isTRUE(baseline$locks$renv$available)) {
    incomplete_reasons <- c(incomplete_reasons, "renv_status_unavailable")
  } else if (!isTRUE(baseline$locks$renv$synchronized)) {
    incomplete_reasons <- c(incomplete_reasons, "renv_lock_not_synchronized")
  }
  if (is.null(baseline$git$commit)) {
    incomplete_reasons <- c(incomplete_reasons, "git_metadata_unavailable")
  }
  if (is.null(baseline$locks$renv_lock_sha256) ||
      is.null(baseline$locks$package_lock_sha256)) {
    incomplete_reasons <- c(incomplete_reasons, "lock_hash_unavailable")
  }
  if (any(vapply(baseline$source_sha256, is.null, logical(1L)))) {
    incomplete_reasons <- c(incomplete_reasons, "source_hash_unavailable")
  }
  if (!identical(baseline$runtime$r_version, "4.4.1")) {
    incomplete_reasons <- c(incomplete_reasons, "unexpected_r_version")
  }
  if (is.null(baseline$runtime$node_version)) {
    incomplete_reasons <- c(incomplete_reasons, "node_runtime_unavailable")
  }
  if (length(selected_ids) != length(required_ids)) {
    incomplete_reasons <- c(incomplete_reasons, "partial_check_selection")
  }
  detector_errors <- vapply(
    checks,
    function(check) identical(check$status, "detector_error"),
    logical(1L)
  )
  if (any(detector_errors)) {
    incomplete_reasons <- c(incomplete_reasons, "detector_error")
  }
  incomplete_reasons <- unique(incomplete_reasons)

  blocking_findings <- vapply(findings, function(finding) {
    isTRUE(finding$release_blocking)
  }, logical(1L))
  failed_properties <- vapply(properties, function(probe) {
    probe$status %in% c("failed", "detector_error")
  }, logical(1L))
  blocker_ids <- c(
    vapply(findings[blocking_findings], `[[`, character(1L), "id"),
    vapply(properties[failed_properties], `[[`, character(1L), "id")
  )
  overall_status <- if (length(incomplete_reasons)) {
    "incomplete"
  } else if (length(blocker_ids)) {
    "release_blocked"
  } else {
    "pass"
  }
  strict_exit_code <- if (length(incomplete_reasons)) {
    2L
  } else if (length(blocker_ids)) {
    1L
  } else 0L
  effective_exit_code <- if (identical(mode, "strict")) strict_exit_code else 0L

  report <- list(
    schema_version = .audit_schema_version,
    audit_name = "3D ENA systematic bug-detection harness",
    mode = mode,
    seed = seed,
    overall_status = overall_status,
    baseline = baseline,
    safety = list(
      synthetic_fixtures_only = TRUE,
      credentials_read = FALSE,
      live_ai_calls = 0L,
      live_service_calls = 0L,
      raw_user_rows_in_report = FALSE
    ),
    summary = list(
      checks_selected = length(checks),
      checks_required = length(required_ids),
      seeded_findings_reproduced = sum(vapply(
        findings,
        function(finding) identical(finding$status, "reproduced"),
        logical(1L)
      )),
      release_blocker_count = length(blocker_ids),
      property_probe_pass_count = sum(vapply(
        properties,
        function(probe) identical(probe$status, "passed"),
        logical(1L)
      )),
      property_probe_failure_count = sum(failed_properties),
      detector_error_count = sum(detector_errors),
      incomplete_reason_count = length(incomplete_reasons)
    ),
    release_gate = list(
      decision = if (identical(overall_status, "pass")) "pass" else "fail",
      strict_exit_code = strict_exit_code,
      effective_exit_code = effective_exit_code,
      blocker_ids = as.list(unname(blocker_ids)),
      incomplete_reasons = as.list(incomplete_reasons),
      policy = paste(
        "Fail strict mode for an incomplete audit, a detector error,",
        "an open S0/S1 seeded finding, or a failed numerical property probe."
      )
    ),
    findings = findings,
    property_probes = properties,
    required_check_ids = as.list(required_ids),
    selected_check_ids = as.list(selected_ids)
  )
  structure(
    list(report = report, exit_code = effective_exit_code),
    class = "ena3d_bug_audit_result"
  )
}


audit_json <- function(value, pretty = TRUE) {
  quote_string <- function(text) {
    if (length(text) != 1L || is.na(text)) return("null")
    encodeString(enc2utf8(text), quote = '"', na.encode = FALSE)
  }
  scalar <- function(item) {
    if (is.character(item)) return(quote_string(item))
    if (is.logical(item)) return(if (is.na(item)) "null" else {
      if (isTRUE(item)) "true" else "false"
    })
    if (is.integer(item)) {
      return(if (is.na(item)) "null" else as.character(item))
    }
    if (is.numeric(item)) {
      if (is.na(item) || !is.finite(item)) return("null")
      return(sprintf("%.17g", item))
    }
    if (inherits(item, "POSIXt")) {
      return(quote_string(format(item, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))
    }
    quote_string(as.character(item))
  }
  encode <- function(item, depth = 0L) {
    if (is.null(item)) return("null")
    if (is.data.frame(item)) {
      item <- lapply(seq_len(nrow(item)), function(index) {
        lapply(item, function(column) column[[index]])
      })
    }
    if (is.atomic(item)) {
      if (length(item) == 0L) return("[]")
      if (length(item) == 1L) return(scalar(item))
      values <- vapply(seq_along(item), function(index) {
        scalar(item[[index]])
      }, character(1L))
      if (!isTRUE(pretty)) return(paste0("[", paste(values, collapse = ","), "]"))
      padding <- strrep("  ", depth + 1L)
      closing <- strrep("  ", depth)
      return(paste0(
        "[\n", padding, paste(values, collapse = paste0(",\n", padding)),
        "\n", closing, "]"
      ))
    }
    if (!is.list(item)) return(scalar(item))
    item_names <- names(item)
    is_object <- !is.null(item_names) && length(item_names) == length(item) &&
      all(nzchar(item_names))
    if (!length(item)) return(if (is_object) "{}" else "[]")
    values <- vapply(seq_along(item), function(index) {
      encode(item[[index]], depth + 1L)
    }, character(1L))
    if (is_object) {
      values <- paste0(
        vapply(item_names, quote_string, character(1L)),
        if (isTRUE(pretty)) ": " else ":",
        values
      )
      open <- "{"
      close <- "}"
    } else {
      open <- "["
      close <- "]"
    }
    if (!isTRUE(pretty)) {
      return(paste0(open, paste(values, collapse = ","), close))
    }
    padding <- strrep("  ", depth + 1L)
    closing <- strrep("  ", depth)
    paste0(
      open, "\n", padding,
      paste(values, collapse = paste0(",\n", padding)),
      "\n", closing, close
    )
  }
  encode(value)
}


audit_write_artifacts <- function(result, output_directory) {
  if (!inherits(result, "ena3d_bug_audit_result")) {
    stop("result must be returned by audit_run().", call. = FALSE)
  }
  if (!is.character(output_directory) || length(output_directory) != 1L ||
      is.na(output_directory) || !nzchar(output_directory)) {
    stop("Audit output must be one non-empty directory path.", call. = FALSE)
  }
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_directory)) {
    stop("Could not create the audit output directory.", call. = FALSE)
  }
  output_directory <- normalizePath(output_directory, mustWork = TRUE)
  report_path <- file.path(output_directory, "audit-report.json")
  findings_path <- file.path(output_directory, "audit-findings.jsonl")
  checksum_path <- file.path(output_directory, "audit-report.sha256")

  writeLines(audit_json(result$report, pretty = TRUE), report_path,
             useBytes = TRUE)
  finding_lines <- vapply(
    result$report$findings,
    function(finding) audit_json(finding, pretty = FALSE),
    character(1L)
  )
  writeLines(finding_lines, findings_path, useBytes = TRUE)
  checksum <- audit_file_sha256(report_path)
  writeLines(
    sprintf("%s  audit-report.json", checksum),
    checksum_path,
    useBytes = TRUE
  )
  list(
    report = report_path,
    findings = findings_path,
    checksum = checksum_path,
    report_sha256 = checksum
  )
}
