library(testthat)
library(shiny)

.exchange_roots <- c(".", "../..", "..")
.exchange_root <- .exchange_roots[file.exists(
  file.path(.exchange_roots, "R", "ena3d_exchange.R")
)][1L]
if (is.na(.exchange_root)) stop("Could not locate the 3D ENA project root.")
.exchange_root <- normalizePath(.exchange_root, mustWork = TRUE)

source(file.path(.exchange_root, "R", "security_utils.R"), local = FALSE)
source(file.path(.exchange_root, "R", "app_utils.R"), local = FALSE)
source(file.path(.exchange_root, "R", "transition.R"), local = FALSE)
source(file.path(.exchange_root, "R", "ena3d_exchange.R"), local = FALSE)
source(
  file.path(.exchange_root, "R", "app_module_load_dataset.R"), local = FALSE
)
source(
  file.path(.exchange_root, "R", "app_module_upload_data.R"), local = FALSE
)
source(file.path(.exchange_root, "R", "trajectory_analysis.R"), local = FALSE)
source(
  file.path(.exchange_root, "R", "app_ui_data_upload_tab.R"), local = FALSE
)

.exchange_old_wd <- getwd()
tryCatch(
  {
    setwd(file.path(.exchange_root, "R"))
    source("app_ui_model_tab.R", local = FALSE)
    source("app_module_overall_model.R", local = FALSE)
    source("app_module_trajectory.R", local = FALSE)
  },
  finally = setwd(.exchange_old_wd)
)

.exchange_fixtures <- file.path(
  .exchange_root,
  "sample_data",
  c(
    "sample_enaset.Rdata",
    "newfrat_enaset.Rdata",
    "student_enaset.RData"
  )
)

.exchange_native <- function(path = .exchange_fixtures[[1L]]) {
  ena3d_read_ena_object(path, source_kind = "bundled")
}

.exchange_payload <- function() {
  ena3d_exchange_payload(.exchange_native())
}

.exchange_write_payload <- function(payload) {
  path <- tempfile(fileext = ".ena3d.json")
  jsonlite::write_json(
    payload,
    path,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA,
    pretty = FALSE
  )
  path
}

.exchange_plain_values <- function(frame) {
  frame <- as.data.frame(frame, stringsAsFactors = FALSE, optional = TRUE)
  lapply(frame, function(values) {
    attributes(values) <- NULL
    values
  })
}


test_that("all bundled samples have deterministic uploadable round trips", {
  limits <- ena3d_data_limits()
  for (fixture in .exchange_fixtures) {
    native <- .exchange_native(fixture)
    first_path <- tempfile(fileext = ".ena3d.json")
    second_path <- tempfile(fileext = ".ena3d.json")
    on.exit(unlink(c(first_path, second_path)), add = TRUE)

    first <- ena3d_write_exchange_file(native, first_path, limits = limits)
    second <- ena3d_write_exchange_file(native, second_path, limits = limits)
    expect_identical(first$sha256, second$sha256, info = basename(fixture))
    expect_identical(first$bytes, second$bytes, info = basename(fixture))
    expect_true(
      first$bytes <= limits$max_exchange_file_bytes,
      info = basename(fixture)
    )

    restored <- ena3d_read_exchange_file(first_path, limits = limits)
    expect_true(inherits(restored, "ena.set"), info = basename(fixture))
    expect_silent(ena3d_validate_ena_object(restored, limits = limits))
    expect_equal(
      .exchange_plain_values(restored$meta.data),
      .exchange_plain_values(native$meta.data),
      tolerance = 1e-12,
      info = basename(fixture)
    )
    expect_equal(
      .exchange_plain_values(restored$points),
      .exchange_plain_values(native$points),
      tolerance = 1e-12,
      info = basename(fixture)
    )

    metadata_names <- names(restored$meta.data)
    dimensions <- ena3d_dimension_names(restored)
    edge_names <- setdiff(names(restored$line.weights), metadata_names)
    expect_true(all(vapply(
      restored$points[metadata_names], inherits, logical(1), "ena.metadata"
    )))
    expect_true(all(vapply(
      restored$points[dimensions], inherits, logical(1), "ena.dimension"
    )))
    expect_true(all(vapply(
      restored$line.weights[edge_names],
      inherits,
      logical(1),
      "ena.co.occurrence"
    )))
    expect_identical(
      names(as.data.frame(rENA::remove_meta_data(restored$line.weights))),
      edge_names
    )

    group_var <- get_ena_group_var(restored)[[1L]]
    group_values <- unique(restored$points[[group_var]])
    prepared <- ena3d_prepare_overall_points(
      restored$points,
      group_var,
      selected_groups = group_values,
      hover_var = metadata_names[[1L]]
    )
    expect_equal(nrow(prepared), nrow(restored$points))
    expect_equal(
      ena3d_overall_group_count(restored$points, group_var),
      length(group_values)
    )
    trajectory_points <- .trajectory_points(restored)
    expect_identical(
      .trajectory_dimensions(restored, trajectory_points), dimensions
    )
    expect_true(group_var %in% .trajectory_metadata_columns(
      restored, trajectory_points, dimensions
    ))
  }
})


test_that("exchange-restored line weights keep metadata out of group means", {
  path <- tempfile(fileext = ".ena3d.json")
  on.exit(unlink(path), add = TRUE)
  ena3d_write_exchange_file(.exchange_native(), path)
  restored <- ena3d_read_exchange_file(path)
  group_var <- get_ena_group_var(restored)[[1L]]
  labels <- ena3d_group_value_labels(restored$line.weights[[group_var]])
  selected <- labels[[1L]]
  rows <- ena3d_group_value_match(
    restored$line.weights[[group_var]], selected
  )
  numeric_weights <- rENA::remove_meta_data(restored$line.weights)
  expected <- colMeans(as.matrix(numeric_weights[rows, , drop = FALSE]))
  selected_points <- get_points_with_group(
    restored$points, group_var, selected
  )

  expect_s3_class(restored$line.weights, "data.frame")
  expect_identical(
    names(rENA::remove_meta_data(selected_points)),
    ena3d_dimension_names(restored)
  )
  expect_equal(
    get_mean_group_lineweights(restored, group_var, selected),
    as.vector(expected)
  )
  expect_equal(
    get_mean_group_lineweights_in_groups(restored, group_var, selected),
    as.vector(expected)
  )
})


test_that("the trusted offline converter emits JSON and SHA-256 sidecar", {
  output <- tempfile(fileext = ".ena3d.json")
  checksum <- paste0(output, ".sha256")
  on.exit(unlink(c(output, checksum)), add = TRUE)
  command <- file.path(
    .exchange_root, "tools", "convert_trusted_rdata_to_ena3d_json.R"
  )
  result <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      shQuote(command),
      "--trusted-native-input",
      shQuote(.exchange_fixtures[[1L]]),
      shQuote(output)
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(result, "status")
  if (is.null(status)) status <- 0L
  expect_identical(status, 0L)
  expect_true(file.exists(output))
  expect_true(file.exists(checksum))
  sha256 <- digest::digest(file = output, algo = "sha256")
  expect_match(paste(result, collapse = "\n"), paste0("exchange_sha256=", sha256),
               fixed = TRUE)
  expect_match(readLines(checksum, warn = FALSE), sha256, fixed = TRUE)
  expect_silent(ena3d_read_exchange_file(output))
})


test_that("dates, datetimes, difftimes, and ordered factors round trip", {
  native <- .exchange_native()
  metadata_columns <- list(
    study_date = as.Date(c("2024-01-01", "2024-01-01", "2024-01-03", "2024-01-03")),
    observed_at = as.POSIXct(
      c(
        "2024-01-01 09:00:00", "2024-01-01 09:00:00",
        "2024-01-03 09:00:00", "2024-01-03 09:00:00"
      ),
      tz = "America/New_York"
    ),
    elapsed = as.difftime(c(0, 0, 48, 48), units = "hours"),
    phase = ordered(
      c("late", "late", "early", "early"),
      levels = c("early", "middle", "late")
    )
  )
  for (name in names(metadata_columns)) {
    values <- metadata_columns[[name]]
    class(values) <- unique(c("ena.metadata", class(values)))
    native$meta.data[[name]] <- values
    native$points[[name]] <- values
    native$line.weights[[name]] <- values
  }
  expect_silent(ena3d_validate_ena_object(native))

  path <- tempfile(fileext = ".ena3d.json")
  on.exit(unlink(path), add = TRUE)
  ena3d_write_exchange_file(native, path)
  restored <- ena3d_read_exchange_file(path)

  expect_s3_class(restored$points$study_date, "Date")
  expect_s3_class(restored$points$observed_at, "POSIXct")
  expect_identical(attr(restored$points$observed_at, "tzone"),
                   "America/New_York")
  expect_s3_class(restored$points$elapsed, "difftime")
  expect_identical(attr(restored$points$elapsed, "units"), "hours")
  expect_true(is.ordered(restored$points$phase))
  expect_identical(levels(restored$points$phase),
                   c("early", "middle", "late"))
  expect_identical(.trajectory_default_order(restored$points$phase),
                   c("early", "middle", "late"))

  dimensions <- ena3d_dimension_names(restored)[1:3]
  path_result <- compute_centroid_path(
    as.data.frame(restored$points),
    time_var = "study_date",
    id_var = "username",
    dimensions = dimensions
  )
  expect_equal(path_result$elapsed_interval, c(NA, 2))
  expect_identical(
    attr(path_result, "trajectory_spec")$elapsed_interval_units,
    "days"
  )
})


test_that("strict schema rejects unknown, duplicate, and ill-typed fields", {
  payload <- .exchange_payload()
  payload$unexpected <- TRUE
  path <- .exchange_write_payload(payload)
  on.exit(unlink(path), add = TRUE)
  expect_error(ena3d_read_exchange_file(path), "unknown: unexpected", fixed = TRUE)

  canonical <- tempfile(fileext = ".ena3d.json")
  on.exit(unlink(canonical), add = TRUE)
  ena3d_write_exchange_file(.exchange_native(), canonical)
  text <- paste(readLines(canonical, warn = FALSE), collapse = "")
  duplicate_text <- sub(
    '"format":"ena3d-exchange"',
    '"format":"ena3d-exchange","format":"ena3d-exchange"',
    text,
    fixed = TRUE
  )
  duplicate_path <- tempfile(fileext = ".ena3d.json")
  on.exit(unlink(duplicate_path), add = TRUE)
  writeLines(duplicate_text, duplicate_path, useBytes = TRUE)
  expect_error(
    ena3d_read_exchange_file(duplicate_path),
    "duplicate field(s): format",
    fixed = TRUE
  )

  payload <- .exchange_payload()
  character_column <- which(vapply(
    payload$tables$meta_data$columns,
    function(column) identical(column$type, "character"),
    logical(1)
  ))[[1L]]
  payload$tables$meta_data$columns[[character_column]]$values[1L] <- list(42L)
  path <- .exchange_write_payload(payload)
  on.exit(unlink(path), add = TRUE)
  expect_error(ena3d_read_exchange_file(path), "must be one UTF-8 string")
})


test_that("malicious nested cells and excessive JSON are inert and rejected", {
  old_value <- getOption("ena3d.exchange_payload_executed")
  on.exit(options(ena3d.exchange_payload_executed = old_value), add = TRUE)
  options(ena3d.exchange_payload_executed = FALSE)

  payload <- .exchange_payload()
  payload$tables$meta_data$columns[[1L]]$values[1L] <- list(list(
    class = "function",
    body = "base::options(ena3d.exchange_payload_executed=TRUE)"
  ))
  path <- .exchange_write_payload(payload)
  on.exit(unlink(path), add = TRUE)
  expect_error(ena3d_read_exchange_file(path), "must be one UTF-8 string")
  expect_false(isTRUE(getOption("ena3d.exchange_payload_executed")))

  deep_path <- tempfile(fileext = ".ena3d.json")
  on.exit(unlink(deep_path), add = TRUE)
  writeLines(paste0(strrep("[", 17L), "0", strrep("]", 17L)), deep_path)
  expect_error(ena3d_read_exchange_file(deep_path), "maximum depth of 16")

  limits <- ena3d_data_limits()
  limits$max_exchange_file_bytes <- 10
  expect_error(
    ena3d_read_exchange_file(deep_path, limits = limits),
    ".ena3d.json file size exceeds",
    fixed = TRUE
  )

  forbidden_calls <- c("load", "readRDS", "eval")
  reader_calls <- all.names(body(ena3d_read_exchange_file),
                            functions = TRUE, unique = TRUE)
  upload_calls <- all.names(body(upload_data), functions = TRUE, unique = TRUE)
  expect_false(any(forbidden_calls %in% reader_calls))
  expect_false(any(forbidden_calls %in% upload_calls))
})


test_that("row alignment, duplicate columns, and edge order are strict", {
  payload <- .exchange_payload()
  metadata_index <- 3L
  payload$tables$points$columns[[metadata_index]]$values[[1L]] <-
    "misaligned-metadata"
  path <- .exchange_write_payload(payload)
  on.exit(unlink(path), add = TRUE)
  expect_error(ena3d_read_exchange_file(path), "identical type and row-aligned")

  payload <- .exchange_payload()
  metadata_column <- payload$tables$points$columns[[metadata_index]]
  metadata_column$type <- "factor"
  metadata_column$levels <- unname(as.list(unique(unlist(
    metadata_column$values, use.names = FALSE
  ))))
  metadata_column <- metadata_column[c("name", "type", "levels", "values")]
  payload$tables$points$columns[[metadata_index]] <- metadata_column
  path <- .exchange_write_payload(payload)
  on.exit(unlink(path), add = TRUE)
  expect_error(ena3d_read_exchange_file(path), "identical type and row-aligned")

  payload <- .exchange_payload()
  payload$tables$points$columns <- c(
    payload$tables$points$columns,
    list(payload$tables$points$columns[[1L]])
  )
  path <- .exchange_write_payload(payload)
  on.exit(unlink(path), add = TRUE)
  expect_error(ena3d_read_exchange_file(path), "duplicate column names")

  payload <- .exchange_payload()
  edge_start <- length(payload$tables$meta_data$columns) + 1L
  edge_columns <- payload$tables$line_weights$columns
  edge_columns[c(edge_start, edge_start + 1L)] <-
    edge_columns[c(edge_start + 1L, edge_start)]
  payload$tables$line_weights$columns <- edge_columns
  path <- .exchange_write_payload(payload)
  on.exit(unlink(path), add = TRUE)
  expect_error(ena3d_read_exchange_file(path), "exactly the same order")
})


test_that("exchange decoding applies configured structural resource limits", {
  path <- tempfile(fileext = ".ena3d.json")
  on.exit(unlink(path), add = TRUE)
  ena3d_write_exchange_file(.exchange_native(), path)

  limits <- ena3d_data_limits()
  limits$max_nodes <- 11
  expect_error(
    ena3d_read_exchange_file(path, limits = limits),
    "node count exceeds"
  )

  limits <- ena3d_data_limits()
  limits$max_point_rows <- 3
  expect_error(
    ena3d_read_exchange_file(path, limits = limits),
    "point row count exceeds"
  )

  limits <- ena3d_data_limits()
  limits$max_table_cells <- 10
  expect_error(
    ena3d_read_exchange_file(path, limits = limits),
    "total exchange table cell count exceeds"
  )
})


test_that("the published structural schema and authoritative reader agree", {
  schema <- jsonlite::read_json(
    file.path(.exchange_root, "docs", "ena3d-exchange-v1.schema.json"),
    simplifyVector = FALSE
  )
  expect_identical(
    schema[["$schema"]],
    "https://json-schema.org/draft/2020-12/schema"
  )
  expect_false(is.null(schema$properties$tables$properties$adjacency_key))

  expect_silent(ena3d_read_exchange_file(
    file.path(.exchange_root, "tests", "e2e", "fixtures",
              "small-valid.ena3d.json")
  ))
  ordinary <- tempfile(fileext = ".ena3d.json")
  on.exit(unlink(ordinary), add = TRUE)
  writeLines("{}", ordinary)
  expect_error(ena3d_read_exchange_file(ordinary), "wrong fields")
})


test_that("nodes and edge weights must be complete finite numbers", {
  nonfinite <- jsonlite::parse_json("[1e999]", simplifyVector = FALSE)
  expect_error(
    ena3d_exchange_decode_values(nonfinite, "double", "probe"),
    "finite number"
  )

  payload <- .exchange_payload()
  node_dimension <- payload$tables$nodes$columns[[2L]]
  node_dimension$values[1L] <- list(NULL)
  payload$tables$nodes$columns[[2L]] <- node_dimension
  path <- .exchange_write_payload(payload)
  on.exit(unlink(path), add = TRUE)
  expect_error(ena3d_read_exchange_file(path), "node dimensions must contain only finite")

  payload <- .exchange_payload()
  edge_start <- length(payload$tables$meta_data$columns) + 1L
  edge <- payload$tables$line_weights$columns[[edge_start]]
  edge$values[1L] <- list(NULL)
  payload$tables$line_weights$columns[[edge_start]] <- edge
  path <- .exchange_write_payload(payload)
  on.exit(unlink(path), add = TRUE)
  expect_error(ena3d_read_exchange_file(path), "edge columns must contain only finite")
})


test_that("exchange upload commits only after complete validation", {
  valid_path <- tempfile(fileext = ".ena3d.json")
  invalid_path <- tempfile(fileext = ".ena3d.json")
  on.exit(unlink(c(valid_path, invalid_path)), add = TRUE)
  ena3d_write_exchange_file(.exchange_native(), valid_path)
  writeLines("{}", invalid_path)

  testServer(
    function(input, output, session) {
      rv <- reactiveValues(
        myList = list(), unit_group_change_plots = list(),
        current_unit_change_plot_camera = list(), ena_groups = character(),
        ena_groupVar = character(), ena_points_plot_ready = FALSE,
        initialized = FALSE, model_tab_clicked = FALSE,
        comparison_plot = list(), reactiveFunctions = list(),
        group_colors = list(), group_selectors = list(),
        group_options = list(), dataset_id = NULL, active_dataset = NULL
      )
      state <- new.env(parent = emptyenv())
      state$ena_obj <- NULL
      state$is_app_initialized <- FALSE
      session$userData$rv <- rv
      session$userData$state <- state
      upload_data(
        input, output, session, rv, state,
        config = list(
          data_limits = ena3d_data_limits(),
          app_version = "exchange-test",
          build_id = "exchange-build"
        )
      )
    },
    {
      session$flushReact()
      session$setInputs(ena_exchange_file = data.frame(
        name = "valid.ena3d.json",
        size = file.info(valid_path)$size,
        type = "application/json",
        datapath = valid_path,
        stringsAsFactors = FALSE
      ))
      session$flushReact()
      expect_true(session$userData$state$is_app_initialized)
      expect_s3_class(session$userData$state$ena_obj, "ena.set")
      known_id <- session$userData$rv$dataset_id
      known_object <- session$userData$state$ena_obj
      expect_identical(
        session$userData$rv$active_dataset$name,
        "valid.ena3d.json"
      )

      session$setInputs(ena_exchange_file = data.frame(
        name = "invalid.ena3d.json",
        size = file.info(invalid_path)$size,
        type = "application/json",
        datapath = invalid_path,
        stringsAsFactors = FALSE
      ))
      session$flushReact()
      expect_identical(session$userData$rv$dataset_id, known_id)
      expect_identical(session$userData$state$ena_obj, known_object)
      expect_true(session$userData$state$is_app_initialized)
    }
  )
})


test_that("trusted sample choices exist in initial HTML", {
  sample_names <- basename(.exchange_fixtures)
  html <- htmltools::renderTags(data_upload_ui(
    "main_app", sample_data_files = sample_names
  ))$html
  for (sample_name in sample_names) {
    expect_match(html, sample_name, fixed = TRUE)
  }
  expect_match(html, '"dropdownParent":"body"', fixed = TRUE)
})
