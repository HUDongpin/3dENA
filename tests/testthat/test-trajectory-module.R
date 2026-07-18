library(testthat)
library(shiny)

.trajectory_test_root <- c(".", "../..", "..")
.trajectory_test_root <- .trajectory_test_root[file.exists(
  file.path(.trajectory_test_root, "R", "trajectory_analysis.R")
)][1L]
if (is.na(.trajectory_test_root)) stop("Could not locate the project R directory.")
source(file.path(.trajectory_test_root, "R", "trajectory_analysis.R"), local = FALSE)
source(file.path(.trajectory_test_root, "R", "trajectory_plot.R"), local = FALSE)
source(file.path(.trajectory_test_root, "R", "app_module_trajectory.R"), local = FALSE)

.wait_for_trajectory_condition <- function(condition, timeout = 15) {
  deadline <- unname(proc.time()[["elapsed"]]) + timeout
  repeat {
    if (isTRUE(condition())) return(TRUE)
    if (unname(proc.time()[["elapsed"]]) >= deadline) return(FALSE)
    later::run_now(0.05)
  }
}

.load_newfrat_trajectory_fixture <- function() {
  fixture <- new.env(parent = emptyenv())
  suppressWarnings(load(
    file.path(.trajectory_test_root, "sample_data", "newfrat_enaset.Rdata"),
    envir = fixture
  ))
  objects <- mget(ls(fixture, all.names = TRUE), envir = fixture)
  matches <- Filter(function(value) {
    !is.null(value$points) && is.data.frame(value$points)
  }, objects)
  expect_length(matches, 1L)
  matches[[1L]]
}


test_that("trajectory node legend renders every ordered node name and color", {
  path <- data.frame(
    time_value = c("Baseline", "Middle", "Follow-up"),
    time_order = 1:3,
    centroid_D1 = 1:3,
    centroid_D2 = 2:4,
    centroid_D3 = 3:5,
    stringsAsFactors = FALSE
  )
  attr(path, "trajectory_spec") <- list(
    time_var = "phase", dimensions = c("D1", "D2", "D3"),
    group_vars = character()
  )
  legend_data <- trajectory_node_legend_data(path)
  html <- htmltools::renderTags(.trajectory_node_legend_ui(path))$html

  expect_match(html, "Trajectory nodes", fixed = TRUE)
  expect_match(html, "Ordered period \u00b7 Phase", fixed = TRUE)
  expect_match(html, "Order 1 \u00b7 Baseline", fixed = TRUE)
  expect_match(html, "Order 3 \u00b7 Follow-up", fixed = TRUE)
  expect_equal(lengths(regmatches(
    html, gregexpr("trajectory-node-legend-item", html, fixed = TRUE)
  )), 3L)
  for (color in legend_data$node_color) {
    expect_match(html, color, fixed = TRUE)
  }
})


test_that("trajectory order UI round-trips labels, gaps, factors, and POSIX times", {
  labels <- c("pre", "intervention", "post")
  expect_identical(
    .trajectory_parse_order(paste(labels, collapse = "\n"), labels),
    labels
  )
  expect_identical(
    .trajectory_parse_order("pre, intervention, post", labels),
    labels
  )
  comma_labels <- c("baseline, pre", "follow-up, final")
  expect_identical(
    .trajectory_parse_order(paste(comma_labels, collapse = "\n"), comma_labels),
    comma_labels
  )

  scheduled <- factor(
    c("baseline", "follow-up"),
    levels = c("baseline", "midpoint", "follow-up"),
    ordered = TRUE
  )
  expect_identical(
    .trajectory_default_order(scheduled),
    c("baseline", "midpoint", "follow-up")
  )
  expect_identical(
    .trajectory_parse_order("baseline, midpoint, follow-up", scheduled),
    c("baseline", "midpoint", "follow-up")
  )

  expect_equal(
    .trajectory_parse_order("1, 2, 3", c(1, 3)),
    c(1, 2, 3)
  )
  expect_error(
    .trajectory_parse_order("1, 2", c(1, 3)),
    "omit observed"
  )

  timestamps <- as.POSIXct(
    c("2025-01-01 09:30:00", "2025-01-02 10:45:30"),
    tz = "Asia/Taipei"
  )
  rendered <- .trajectory_order_labels(timestamps)
  parsed <- .trajectory_parse_order(paste(rendered, collapse = "\n"), timestamps)
  expect_equal(as.numeric(parsed), as.numeric(timestamps), tolerance = 1e-6)

  precise_numeric <- c(pi, exp(1))
  numeric_labels <- .trajectory_order_labels(
    .trajectory_default_order(precise_numeric)
  )
  numeric_parsed <- .trajectory_parse_order(
    paste(numeric_labels, collapse = "\n"), precise_numeric
  )
  expect_identical(numeric_parsed, sort(precise_numeric))
  expect_true(all(grepl("\\[hex=", numeric_labels)))

  precise_times <- as.POSIXct(
    c(1735695000.1234567, 1735698600.7654321),
    origin = "1970-01-01", tz = "Asia/Taipei"
  )
  precise_labels <- .trajectory_order_labels(precise_times)
  precise_parsed <- .trajectory_parse_order(
    paste(precise_labels, collapse = "\n"), precise_times
  )
  expect_identical(as.numeric(precise_parsed), as.numeric(precise_times))
  expect_identical(
    .trajectory_time_filter_mask(precise_times, precise_labels[[2L]]),
    c(FALSE, TRUE)
  )
})


test_that("trajectory order UI round-trips boundary whitespace and difftime units", {
  whitespace_values <- c(" baseline", "follow-up ")
  whitespace_order <- .trajectory_default_order(whitespace_values)
  whitespace_labels <- .trajectory_order_labels(whitespace_order)
  expect_identical(
    .trajectory_parse_order(
      paste(whitespace_labels, collapse = "\n"), whitespace_values
    ),
    whitespace_order
  )
  single_whitespace <- " phase, 1 "
  expect_identical(
    .trajectory_parse_order(
      .trajectory_order_labels(
        .trajectory_default_order(single_whitespace)
      ),
      single_whitespace
    ),
    single_whitespace
  )

  elapsed <- as.difftime(c(2, 1, 2), units = "hours")
  elapsed_order <- .trajectory_default_order(elapsed)
  expect_s3_class(elapsed_order, "difftime")
  expect_identical(attr(elapsed_order, "units", exact = TRUE), "hours")
  expect_equal(as.numeric(elapsed_order), c(1, 2))

  elapsed_labels <- .trajectory_order_labels(elapsed_order)
  elapsed_parsed <- .trajectory_parse_order(
    paste(elapsed_labels, collapse = "\n"), elapsed
  )
  expect_s3_class(elapsed_parsed, "difftime")
  expect_identical(attr(elapsed_parsed, "units", exact = TRUE), "hours")
  expect_identical(as.numeric(elapsed_parsed), as.numeric(elapsed_order))
  expect_identical(
    .trajectory_time_filter_mask(elapsed, elapsed_labels[[1L]]),
    c(FALSE, TRUE, FALSE)
  )
})


test_that("trajectory selectors exclude rENA internals and quantify repeated-ID coverage", {
  points <- data.frame(
    ENA_UNIT = paste0("unit-", 1:5),
    X = seq_len(5),
    KEYCOL = letters[1:5],
    wave = c(1, 2, 1, 2, 1),
    person = c("p1", "p1", "p2", "p2", "p2"),
    d1 = 1:5,
    d2 = 6:10,
    d3 = 11:15,
    stringsAsFactors = FALSE
  )
  metadata <- .trajectory_metadata_columns(
    points, points, c("d1", "d2", "d3")
  )
  expect_identical(metadata, c("wave", "person"))

  coverage <- .trajectory_id_coverage(points, "wave", "person")
  expect_equal(coverage$n_ids, 2L)
  expect_equal(coverage$n_repeated_ids, 2L)
  expect_equal(coverage$n_duplicate_id_time_rows, 1L)
  expect_match(
    .trajectory_id_coverage_message(coverage, "person"),
    "2 of 2 ID profiles"
  )

  choices <- .trajectory_id_choices(
    points,
    c("ENA_UNIT", "KEYCOL", "person"),
    "wave"
  )
  expect_identical(unname(choices), "person")
  expect_match(names(choices), "2/2 repeated ID profiles", fixed = TRUE)

  grouped <- expand.grid(
    condition = c("A", "B"), person = c("p1", "p2"), wave = 1:2,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  overlap <- .trajectory_comparison_overlap(
    grouped, "condition", "A", "B", "person", "wave"
  )
  expect_equal(overlap$n_overlap_ids, 2L)
  expect_equal(overlap$n_matched_id_times, 4L)
  expect_match(
    .trajectory_comparison_overlap_message(overlap),
    "same physical entity"
  )
})


test_that("bootstrap jobs have a hosted hard cap and a visible cost estimate", {
  points <- data.frame(row = seq_len(255))
  selected <- .trajectory_bootstrap_cost(
    points, paste0("d", 1:3), 20,
    uncertainty = TRUE, comparison = FALSE
  )
  full <- .trajectory_bootstrap_cost(
    points, paste0("d", 1:136), 20,
    uncertainty = TRUE, comparison = FALSE
  )
  expect_identical(.trajectory_bootstrap_max_reps(), 500L)
  expect_gt(full$seconds, selected$seconds)
  expect_match(.trajectory_bootstrap_cost_message(full), "Estimated hosted-server cost")
  expect_match(.trajectory_bootstrap_cost_message(full), "rough estimate")
  expect_invisible(.trajectory_validate_bootstrap_cost(selected, max_seconds = 60))
  expect_error(
    .trajectory_validate_bootstrap_cost(full, max_seconds = 5),
    "run the analysis offline"
  )
})


test_that("isolated bootstrap deadline leaves the event loop live and kills its worker", {
  skip_if_not_installed("callr")
  skip_if_not_installed("later")
  skip_if_not_installed("promises")

  worker_file <- tempfile("slow-trajectory-worker-", fileext = ".R")
  writeLines(c(
    "bootstrap_centroid_path <- function(...) {",
    "  Sys.sleep(5)",
    "  data.frame(unexpected = TRUE)",
    "}",
    "compare_centroid_paths <- function(...) data.frame(unexpected = TRUE)"
  ), worker_file)
  on.exit(unlink(worker_file), add = TRUE)

  heartbeat <- FALSE
  later::later(function() heartbeat <<- TRUE, delay = 0.01)
  job <- .trajectory_start_bootstrap_job(
    uncertainty_arguments = list(),
    timeout_seconds = 0.3,
    analysis_file = worker_file,
    poll_interval = 0.01
  )
  on.exit(job$cancel(), add = TRUE)
  rejection <- NULL
  promises::then(
    job$promise,
    onRejected = function(error) {
      rejection <<- error
      NULL
    }
  )

  # A timer scheduled in the parent must fire while the child is still doing
  # work.  This distinguishes the implementation from a synchronous
  # callr::r(..., timeout=) wrapper, which would still block Shiny.
  expect_true(.wait_for_trajectory_condition(function() heartbeat, timeout = 1))
  expect_true(job$process$is_alive())
  expect_true(.wait_for_trajectory_condition(
    function() !is.null(rejection), timeout = 3
  ))

  expect_s3_class(rejection, "trajectory_bootstrap_timeout")
  expect_match(conditionMessage(rejection), "executable 0.3-second limit")
  expect_match(conditionMessage(rejection), "worker was terminated")
  expect_false(job$process$is_alive())
  .trajectory_prune_bootstrap_processes()
  expect_length(ls(.trajectory_bootstrap_process_registry), 0L)
})


test_that("trajectory diagnostics omit only path warnings inherited by uncertainty", {
  points <- data.frame(
    id = "p1", time = 1:2, x = c(1, 2), y = c(2, 4)
  )
  path <- suppressWarnings(compute_centroid_path(
    points, "time", "id", dimensions = c("x", "y")
  ))
  uncertainty <- suppressWarnings(bootstrap_centroid_path(
    points, "time", "id", dimensions = c("x", "y"),
    n_boot = 2, seed = 1
  ))
  path_diagnostics <- .trajectory_module_diagnostics_from(path, "path")
  uncertainty_diagnostics <- .trajectory_module_diagnostics_from(
    uncertainty, "bootstrap"
  )

  uncertainty_only <- .trajectory_remove_inherited_diagnostics(
    uncertainty_diagnostics, path_diagnostics
  )
  diagnostics <- .trajectory_bind_diagnostics(
    path_diagnostics, uncertainty_only
  )

  one_entity <- diagnostics$code == "one_entity_slice"
  expect_equal(sum(one_entity), 2L)
  expect_true(all(diagnostics$source[one_entity] == "path"))
  bootstrap_only <- diagnostics$code == "bootstrap_insufficient_clusters"
  expect_equal(sum(bootstrap_only), 2L)
  expect_true(all(diagnostics$source[bootstrap_only] == "bootstrap"))

  # Matching code/message values with a genuinely different slice context are
  # distinct diagnostics and must not be removed.
  distinct <- uncertainty_diagnostics[
    uncertainty_diagnostics$code == "one_entity_slice", , drop = FALSE
  ]
  distinct$time_order[[1L]] <- 99L
  retained <- .trajectory_remove_inherited_diagnostics(
    distinct[1L, , drop = FALSE], path_diagnostics
  )
  expect_equal(nrow(retained), 1L)
  expect_identical(retained$time_order, 99L)
})


test_that("trajectory CSV exports neutralize spreadsheet formulas and retain provenance", {
  unsafe <- data.frame(
    group = c("=1+1", "+SUM(A1:A2)", "-cmd", "@user", "ordinary"),
    value = c(-2, -1, 0, 1, 2),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(unsafe)[1L] <- "=unsafe-header"
  exported <- .trajectory_spreadsheet_safe_frame(unsafe)
  expect_identical(
    exported[[1L]],
    c("'=1+1", "'+SUM(A1:A2)", "'-cmd", "'@user", "ordinary")
  )
  expect_identical(names(exported)[1L], "'=unsafe-header")
  expect_identical(exported$value, c(-2, -1, 0, 1, 2))

  points <- data.frame(wave = 1:2, person = c("p1", "p1"), d1 = 1:2)
  provenance <- .trajectory_provenance_metadata(points, "d1")
  expect_match(provenance$dataset_sha256, "^[0-9a-f]{64}$")
  expect_match(provenance$dataset_md5, "^[0-9a-f]{32}$")
  expect_true(all(c(
    "app_version", "build_id", "git_commit", "r_version", "r_platform",
    "package_versions", "rotation_class", "rotation_sha256",
    "rotation_dimensions"
  ) %in% names(provenance)))

  boot_points <- expand.grid(person = paste0("p", 1:3), wave = 1:2)
  boot_points$d1 <- seq_len(nrow(boot_points))
  boot_points$d2 <- 0
  boot <- suppressWarnings(bootstrap_centroid_path(
    boot_points, "wave", "person", dimensions = c("d1", "d2"),
    n_boot = 2, seed = 1
  ))
  boot_metadata <- .trajectory_bootstrap_metadata(boot, NULL)
  expect_equal(boot_metadata$bootstrap_design_resolved, "cluster")
  expect_equal(boot_metadata$bootstrap_n_sampling_units, 3L)
  expect_match(boot_metadata$bootstrap_stratum_sizes, "3L")
  expect_match(boot_metadata$bootstrap_eligible_id_keys, "p1")

  colliding <- data.frame(
    `.analysis_time_var` = "group-A", value = 1, check.names = FALSE
  )
  expect_error(
    .trajectory_export_metadata(colliding, list(time_var = "wave")),
    "metadata column collision"
  )
  reserved <- data.frame(
    `.analysis_unrelated` = "source", value = 1, check.names = FALSE
  )
  expect_error(
    .trajectory_export_metadata(reserved, list(time_var = "wave")),
    "metadata column collision"
  )

  expect_null(.trajectory_download_controls(NULL))
  controls <- as.character(.trajectory_download_controls(list(
    path = points, bootstrap = NULL, comparison = NULL
  )))
  expect_match(controls, "Analysis bundle ZIP", fixed = TRUE)
  expect_match(controls, "Path CSV", fixed = TRUE)
  expect_match(controls, "Metadata CSV", fixed = TRUE)
  expect_false(grepl("Uncertainty CSV", controls, fixed = TRUE))
  expect_false(grepl("Comparison CSV", controls, fixed = TRUE))

  bundle_result <- list(
    path = points,
    bootstrap = NULL,
    comparison = NULL,
    diagnostics = data.frame(
      source = "path", code = "example", severity = "info",
      message = "bundle test", stringsAsFactors = FALSE
    ),
    metadata = provenance,
    settings = list(time_var = "wave", id_var = "person")
  )
  bundle_file <- tempfile(fileext = ".zip")
  on.exit(unlink(bundle_file), add = TRUE)
  written <- .trajectory_write_bundle(bundle_result, bundle_file)
  expect_true(file.exists(bundle_file))
  expect_true(all(c(
    "path.csv", "diagnostics.csv", "metadata.csv", "manifest.json"
  ) %in% written))
  archived <- zip::zip_list(bundle_file)$filename
  expect_true(all(written %in% archived))

  extract_dir <- tempfile("ena3d-bundle-test-")
  dir.create(extract_dir)
  on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)
  utils::unzip(bundle_file, files = "manifest.json", exdir = extract_dir)
  manifest <- jsonlite::read_json(file.path(extract_dir, "manifest.json"))
  expect_identical(manifest$schema_version, 1L)
  expect_identical(manifest$schema, "urn:3dena:trajectory-analysis-bundle:1")
  expect_identical(manifest$metadata$dataset_sha256, provenance$dataset_sha256)
})


test_that("grouped network overlays state and honor their scope", {
  ena <- list(
    rotation = list(
      nodes = data.frame(
        code = c("a", "b", "c"),
        D1 = c(0, 1, 2), D2 = c(0, 1, 0), D3 = c(1, 0, 1)
      ),
      adjacency.key = data.frame(
        ab = c("a", "b"), ac = c("a", "c"), bc = c("b", "c")
      )
    ),
    line.weights = data.frame(
      wave = c(1, 1),
      condition = c("A", "B"),
      `a & b` = c(1, 9),
      `a & c` = c(2, 8),
      `b & c` = c(3, 7),
      check.names = FALSE
    ),
    meta.data = data.frame(wave = c(1, 1), condition = c("A", "B"))
  )

  overall <- .trajectory_network_overlay(
    ena, c("D1", "D2", "D3"), "wave", "1",
    group_var = "condition", selected_group = ""
  )
  selected <- .trajectory_network_overlay(
    ena, c("D1", "D2", "D3"), "wave", "1",
    group_var = "condition", selected_group = "A"
  )
  expect_match(overall$message, "overall across all `condition` groups", fixed = TRUE)
  expect_match(selected$message, "condition = A", fixed = TRUE)
  expect_equal(selected$network_edges$weight, c(1, 2, 3))
  expect_equal(overall$network_edges$weight, c(5, 5, 5))
  expect_true(all(c("width", "sign", "color") %in% names(selected$network_edges)))

  no_metadata <- ena
  no_metadata$meta.data <- NULL
  no_metadata$line.weights <- no_metadata$line.weights[
    c("condition", "b & c", "wave", "a & b", "a & c")
  ]
  exact <- .trajectory_network_overlay(
    no_metadata, c("D1", "D2", "D3"), "wave", "1",
    group_var = "condition", selected_group = "A"
  )
  expect_equal(exact$network_edges$weight, c(1, 2, 3))

  missing_edge <- no_metadata
  missing_edge$line.weights[["a & c"]] <- NULL
  withheld <- .trajectory_network_overlay(
    missing_edge, c("D1", "D2", "D3"), "wave", "1",
    group_var = "condition", selected_group = "A"
  )
  expect_null(withheld$network_edges)
  expect_match(withheld$message, "withheld")

  posix_ena <- ena
  posix_ena$line.weights$wave <- as.POSIXct(
    c(1735695000.1234567, 1735698600.7654321),
    origin = "1970-01-01", tz = "Asia/Taipei"
  )
  posix_ena$meta.data$wave <- posix_ena$line.weights$wave
  selected_label <- .trajectory_order_labels(posix_ena$line.weights$wave)[[1L]]
  posix_overlay <- .trajectory_network_overlay(
    posix_ena, c("D1", "D2", "D3"), "wave", selected_label,
    group_var = "condition", selected_group = "A"
  )
  expect_equal(posix_overlay$network_edges$weight, c(1, 2, 3))
})


test_that("trajectory module runs only on request and preserves raw results across views", {
  ena <- .load_newfrat_trajectory_fixture()
  dimensions <- intersect(
    names(as.data.frame(ena$points)),
    setdiff(names(as.data.frame(ena$rotation$nodes)), "code")
  )
  selected <- head(dimensions, 3L)

  testServer(
    trajectory_server,
    args = list(
      ena_obj = ena,
      selected_axes = selected,
      raw_dimensions = dimensions,
      group_colors = NULL,
      camera = NULL
    ),
    {
      expect_null(analysis_result())

      session$setInputs(
        time_var = "Week",
        id_var = "Name",
        group_var = "",
        time_order = paste(0:14, collapse = ", "),
        cohort_policy = "available",
        na_policy = "complete",
        distance_space = "selected",
        view = "3d",
        show_uncertainty = FALSE,
        run_comparison = FALSE,
        bootstrap_reps = 20,
        confidence = 0.95,
        bootstrap_seed = 2026,
        network_overlay = FALSE
      )
      session$flushReact()
      expect_null(analysis_result())

      session$setInputs(run_trajectory = 1)
      session$flushReact()
      completed <- analysis_result()

      expect_s3_class(completed$path, "data.frame")
      expect_equal(nrow(completed$path), 15L)
      expect_equal(completed$path$n_used, rep(17L, 15L))
      expect_match(status(), "Completed 15 centroid slices")
      expect_identical(completed$settings$dimensions, selected)
      expect_match(completed$metadata$dataset_sha256, "^[0-9a-f]{64}$")
      expect_match(completed$metadata$dataset_md5, "^[0-9a-f]{32}$")
      expect_match(completed$metadata$rotation_sha256, "^[0-9a-f]{64}$")
      expect_equal(completed$metadata$repeated_id_profiles, 17L)
      expect_true(all(c(
        "app_version", "build_id", "git_commit", "r_version",
        "package_versions", "rotation_sha256", "rotation_nodes",
        "rotation_edges"
      ) %in% names(completed$metadata)))

      session$setInputs(view = "2d", axis_x = selected[[1]], axis_y = selected[[2]])
      session$flushReact()
      expect_identical(analysis_result(), completed)
    }
  )
})


test_that("trajectory module wires clustered uncertainty and exact paired comparison", {
  points <- expand.grid(
    condition = c("A", "B"),
    person = paste0("p", 1:5),
    wave = 1:3,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  person_index <- as.numeric(sub("p", "", points$person))
  condition_shift <- ifelse(points$condition == "B", 0.25, 0)
  points$d1 <- person_index + points$wave + condition_shift
  points$d2 <- person_index / 2 - points$wave - condition_shift
  points$d3 <- points$wave * 0.4 + condition_shift

  testServer(
    trajectory_server,
    args = list(
      ena_obj = points,
      selected_axes = c("d1", "d2", "d3"),
      raw_dimensions = c("d1", "d2", "d3")
    ),
    {
      session$setInputs(
        time_var = "wave",
        id_var = "person",
        group_var = "condition",
        condition_a = "A",
        condition_b = "B",
        time_order = "1, 2, 3",
        cohort_policy = "complete",
        na_policy = "complete",
        distance_space = "selected",
        view = "3d",
        show_uncertainty = TRUE,
        run_comparison = TRUE,
        confirm_paired_ids = TRUE,
        bootstrap_design = "auto",
        bootstrap_reps = 200,
        confidence = 0.90,
        bootstrap_seed = 42,
        network_overlay = FALSE
      )
      session$flushReact()
      session$setInputs(run_trajectory = 1)
      session$flushReact()
      completed_in_time <- .wait_for_trajectory_condition(function() {
        session$flushReact()
        !is.null(analysis_result())
      }, timeout = 20)
      expect_true(completed_in_time, info = status())

      completed <- analysis_result()
      expect_equal(nrow(completed$path), 6L)
      expect_equal(nrow(completed$bootstrap), 6L)
      expect_equal(nrow(completed$comparison), 3L)
      expect_equal(completed$comparison$n_matched, rep(5L, 3L))
      expect_equal(completed$comparison$difference_d1, rep(0.25, 3L), tolerance = 1e-12)
      expect_true(all(c(
        "centroid_d1_lower", "centroid_d1_upper"
      ) %in% names(completed$bootstrap)))
      expect_true(isTRUE(completed$metadata$bootstrap_enabled))
      expect_equal(completed$metadata$bootstrap_failed_replicates, 0L)
      expect_equal(completed$metadata$comparison_failed_replicates, 0L)
      expect_equal(completed$metadata$bootstrap_design_resolved, "cluster")
      expect_equal(completed$metadata$bootstrap_n_sampling_units, 5L)
      expect_equal(completed$metadata$comparison_overlap_ids, 5L)
      expect_true(isTRUE(completed$metadata$paired_id_identity_confirmed))
    }
  )
})


test_that("trajectory module completes a zero-overlap comparison with diagnostics", {
  points_a <- expand.grid(
    condition = "A", person = c("a", "b"), wave = 1:2,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  points_b <- expand.grid(
    condition = "B", person = c("c", "d"), wave = 1:2,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  points <- rbind(points_a, points_b)
  points$d1 <- seq_len(nrow(points))
  points$d2 <- -points$d1
  points$d3 <- points$wave

  testServer(
    trajectory_server,
    args = list(
      ena_obj = points,
      selected_axes = c("d1", "d2", "d3"),
      raw_dimensions = c("d1", "d2", "d3")
    ),
    {
      session$setInputs(
        time_var = "wave", id_var = "person", group_var = "condition",
        condition_a = "A", condition_b = "B", time_order = "1, 2",
        cohort_policy = "complete", na_policy = "complete",
        distance_space = "selected", view = "3d",
        show_uncertainty = FALSE, run_comparison = TRUE,
        confirm_paired_ids = TRUE, bootstrap_design = "auto",
        bootstrap_reps = 200, confidence = 0.95, bootstrap_seed = 7,
        network_overlay = FALSE
      )
      session$flushReact()
      session$setInputs(run_trajectory = 1)
      session$flushReact()
      completed_in_time <- .wait_for_trajectory_condition(function() {
        session$flushReact()
        !is.null(analysis_result())
      }, timeout = 15)
      expect_true(completed_in_time, info = status())

      completed <- analysis_result()
      expect_s3_class(
        completed$comparison, "paired_centroid_path_comparison"
      )
      expect_equal(completed$comparison$n_matched, c(0L, 0L))
      expect_true("no_matched_participants" %in% completed$diagnostics$code)
      expect_equal(completed$metadata$comparison_overlap_ids, 0L)
      expect_match(status(), "Completed 4 centroid slices")
    }
  )
})


test_that("trajectory module requires scientific bootstrap size and paired-ID confirmation", {
  points <- expand.grid(
    condition = c("A", "B"), person = paste0("p", 1:3), wave = 1:2,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  points$d1 <- seq_len(nrow(points))
  points$d2 <- -points$d1
  points$d3 <- points$wave

  testServer(
    trajectory_server,
    args = list(
      ena_obj = points,
      selected_axes = c("d1", "d2", "d3"),
      raw_dimensions = c("d1", "d2", "d3")
    ),
    {
      session$setInputs(
        time_var = "wave", id_var = "person", group_var = "condition",
        condition_a = "A", condition_b = "B", time_order = "1, 2",
        cohort_policy = "complete", na_policy = "complete",
        distance_space = "selected", view = "3d",
        show_uncertainty = FALSE, run_comparison = TRUE,
        confirm_paired_ids = FALSE, bootstrap_design = "auto",
        bootstrap_reps = 200, confidence = 0.95, bootstrap_seed = 1,
        network_overlay = FALSE
      )
      session$flushReact()
      session$setInputs(run_trajectory = 1)
      session$flushReact()
      expect_null(analysis_result())
      expect_match(status(), "same raw ID")
    }
  )

  ungrouped <- points[points$condition == "A", ]
  testServer(
    trajectory_server,
    args = list(
      ena_obj = ungrouped,
      selected_axes = c("d1", "d2", "d3"),
      raw_dimensions = c("d1", "d2", "d3")
    ),
    {
      session$setInputs(
        time_var = "wave", id_var = "person", group_var = "",
        time_order = "1, 2", cohort_policy = "complete",
        na_policy = "complete", distance_space = "selected", view = "3d",
        show_uncertainty = TRUE, run_comparison = FALSE,
        bootstrap_design = "auto", bootstrap_reps = 50,
        confidence = 0.95, bootstrap_seed = 1, network_overlay = FALSE
      )
      session$flushReact()
      session$setInputs(run_trajectory = 1)
      session$flushReact()
      expect_null(analysis_result())
      expect_match(status(), "at least 200")
    }
  )
})


test_that("trajectory module rejects duplicate selected axes for a 3D run", {
  points <- expand.grid(
    person = c("p1", "p2"), wave = 1:2,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  points$d1 <- seq_len(nrow(points))
  points$d2 <- -points$d1
  points$d3 <- points$wave

  testServer(
    trajectory_server,
    args = list(
      ena_obj = points,
      selected_axes = c("d1", "d1", "d2"),
      raw_dimensions = c("d1", "d2", "d3")
    ),
    {
      session$setInputs(
        time_var = "wave", id_var = "person", group_var = "",
        time_order = "1, 2", cohort_policy = "available",
        na_policy = "complete", distance_space = "selected", view = "3d",
        show_uncertainty = FALSE, run_comparison = FALSE,
        bootstrap_reps = 20, confidence = 0.95, bootstrap_seed = 1,
        network_overlay = FALSE
      )
      session$flushReact()
      session$setInputs(run_trajectory = 1)
      session$flushReact()

      expect_null(analysis_result())
      expect_match(status(), "three distinct selected ENA dimensions")
    }
  )
})


test_that("trajectory module blocks cross-sectional IDs and oversized bootstrap jobs", {
  cross_sectional <- data.frame(
    wave = 1:4,
    person = paste0("p", 1:4),
    d1 = 1:4,
    d2 = 4:1,
    d3 = c(0, 1, 0, 1)
  )

  testServer(
    trajectory_server,
    args = list(
      ena_obj = cross_sectional,
      selected_axes = c("d1", "d2", "d3"),
      raw_dimensions = c("d1", "d2", "d3")
    ),
    {
      session$setInputs(
        time_var = "wave",
        id_var = "person",
        group_var = "",
        time_order = "1, 2, 3, 4",
        cohort_policy = "available",
        na_policy = "complete",
        distance_space = "selected",
        view = "3d",
        show_uncertainty = FALSE,
        run_comparison = FALSE,
        bootstrap_reps = 20,
        confidence = 0.95,
        bootstrap_seed = 1,
        network_overlay = FALSE
      )
      session$flushReact()
      expect_match(output$id_coverage_status, "cross-sectional only")
      session$setInputs(run_trajectory = 1)
      session$flushReact()
      expect_null(analysis_result())
      expect_match(status(), "cross-sectional only")
    }
  )

  repeated <- cross_sectional
  repeated$person <- rep(c("p1", "p2"), 2)
  testServer(
    trajectory_server,
    args = list(
      ena_obj = repeated,
      selected_axes = c("d1", "d2", "d3"),
      raw_dimensions = c("d1", "d2", "d3")
    ),
    {
      session$setInputs(
        time_var = "wave",
        id_var = "person",
        group_var = "",
        time_order = "1, 2, 3, 4",
        cohort_policy = "available",
        na_policy = "complete",
        distance_space = "selected",
        view = "3d",
        show_uncertainty = TRUE,
        run_comparison = FALSE,
        bootstrap_reps = 501,
        confidence = 0.95,
        bootstrap_seed = 1,
        network_overlay = FALSE
      )
      session$flushReact()
      session$setInputs(run_trajectory = 1)
      session$flushReact()
      expect_null(analysis_result())
      expect_match(status(), "at most 500")
    }
  )
})


test_that("changing the dataset invalidates every completed trajectory result", {
  first <- data.frame(
    wave = rep(1:2, each = 3),
    person = rep(paste0("p", 1:3), 2),
    d1 = 1:6,
    d2 = 6:1,
    d3 = c(0, 1, 2, 1, 2, 3)
  )
  second <- first
  second$d1 <- second$d1 + 100
  dataset <- reactiveVal(first)

  testServer(
    trajectory_server,
    args = list(
      ena_obj = dataset,
      selected_axes = c("d1", "d2", "d3"),
      raw_dimensions = c("d1", "d2", "d3")
    ),
    {
      session$setInputs(
        time_var = "wave",
        id_var = "person",
        group_var = "",
        time_order = "1, 2",
        cohort_policy = "available",
        na_policy = "complete",
        distance_space = "selected",
        view = "3d",
        show_uncertainty = FALSE,
        run_comparison = FALSE,
        bootstrap_reps = 10,
        confidence = 0.95,
        bootstrap_seed = 1,
        network_overlay = FALSE
      )
      session$flushReact()
      session$setInputs(run_trajectory = 1)
      session$flushReact()
      expect_equal(nrow(analysis_result()$path), 2L)

      dataset(second)
      session$flushReact()
      expect_null(analysis_result())
      expect_match(status(), "dataset or rotation changed")
    }
  )
})
