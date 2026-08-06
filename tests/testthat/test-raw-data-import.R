library(testthat)
library(shiny)

.raw_roots <- c(".", "../..", "..")
.raw_root <- .raw_roots[file.exists(
  file.path(.raw_roots, "R", "raw_data_import.R")
)][1L]
if (is.na(.raw_root)) stop("Could not locate the 3D ENA project root.")
.raw_root <- normalizePath(.raw_root, mustWork = TRUE)

source(file.path(.raw_root, "R", "security_utils.R"), local = FALSE)
source(file.path(.raw_root, "R", "app_utils.R"), local = FALSE)
source(file.path(.raw_root, "R", "transition.R"), local = FALSE)
source(file.path(.raw_root, "R", "ena3d_exchange.R"), local = FALSE)
source(file.path(.raw_root, "R", "app_module_load_dataset.R"), local = FALSE)
source(file.path(.raw_root, "R", "raw_data_import.R"), local = FALSE)
source(file.path(.raw_root, "R", "app_module_upload_data.R"), local = FALSE)
source(file.path(.raw_root, "R", "app_ui_data_upload_tab.R"), local = FALSE)
source(file.path(.raw_root, "R", "trajectory_analysis.R"), local = FALSE)
source(file.path(.raw_root, "R", "trajectory_plot.R"), local = FALSE)
source(file.path(.raw_root, "R", "app_module_trajectory.R"), local = FALSE)


.raw_fixture <- function() {
  data <- expand.grid(
    Group = c("Experimental", "Control"),
    Lesson = c("Lesson 1", "Lesson 2"),
    Name = paste("Student", 1:4),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  index <- seq_len(nrow(data))
  data$EC <- as.numeric(index %% 2L == 0L | index %% 5L == 0L)
  data$ICT <- as.numeric(index %% 3L == 0L | index %% 4L == 0L)
  data$MCO <- as.numeric(index %% 4L %in% c(0L, 1L))
  data$ATT <- as.numeric(index %% 5L %in% c(0L, 1L, 2L))
  data
}


.raw_mapping <- function(units = c("Group", "Name")) {
  list(
    units = units,
    conversation = "Lesson",
    codes = c("EC", "ICT", "MCO", "ATT"),
    metadata = character(),
    group = "Group",
    model = "AccumulatedTrajectory",
    window = "MovingStanzaWindow",
    window_size_back = 4L,
    rotation = "SVD"
  )
}


test_that("CSV parsing preserves raw headers and detects common separators", {
  fixture <- .raw_fixture()
  for (separator in c(",", ";", "\t")) {
    path <- tempfile(fileext = ".csv")
    on.exit(unlink(path), add = TRUE)
    utils::write.table(
      fixture, path, sep = separator, row.names = FALSE, quote = TRUE
    )
    parsed <- ena3d_read_raw_table(path, "coded-data.csv")
    expect_identical(names(parsed$data), names(fixture))
    expect_equal(parsed$rows, nrow(fixture))
    expect_equal(parsed$columns, ncol(fixture))
  }
})


test_that("CSV parsing preserves identifier lexemes before field mapping", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c(
    "StudentID,Wave,C1,C2,C3",
    "001,T1,1,0,1",
    "1,T1,0,1,0",
    "9007199254740992,T2,1,1,0",
    "9007199254740993,T2,0,1,1",
    "NA,T3,1,0,0"
  ), path, useBytes = TRUE)

  parsed <- ena3d_read_raw_table(path, "identifiers.csv")$data

  expect_true(all(vapply(parsed, is.character, logical(1L))))
  expect_identical(
    parsed$StudentID,
    c("001", "1", "9007199254740992", "9007199254740993", "NA")
  )
  expect_length(unique(parsed$StudentID), 5L)
})


test_that("raw upload guard accepts only bounded spreadsheet files", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(.raw_fixture(), path, row.names = FALSE)
  upload <- list(
    name = "coded-data.csv",
    datapath = path,
    size = file.info(path)$size,
    type = "text/csv"
  )
  resolved <- ena3d_resolve_raw_upload(upload, upload_root = tempdir())
  expect_identical(resolved$extension, "csv")
  expect_match(resolved$sha256, "^[0-9a-f]{64}$")

  upload$name <- "coded-data.RData"
  expect_error(ena3d_resolve_raw_upload(upload, upload_root = tempdir()),
               "Only .csv, .xlsx, and .xls")

  upload$name <- "coded-data.csv"
  limits <- ena3d_data_limits()
  limits$max_raw_file_bytes <- 1
  expect_error(
    ena3d_resolve_raw_upload(upload, limits = limits, upload_root = tempdir()),
    "raw spreadsheet file size exceeds"
  )
})


test_that("automatic mapping protects reused student labels across groups", {
  mapping <- ena3d_suggest_raw_mapping(.raw_fixture())
  expect_identical(mapping$units, c("Group", "Name"))
  expect_identical(mapping$conversation, "Lesson")
  expect_identical(mapping$group, "Group")
  expect_setequal(mapping$codes, c("EC", "ICT", "MCO", "ATT"))
  expect_identical(mapping$model, "AccumulatedTrajectory")

  expect_error(
    ena3d_validate_raw_mapping(.raw_fixture(), .raw_mapping(units = "Name")),
    "Add the grouping field to the unit identifier"
  )
})


test_that("automatic mapping recognizes common concatenated ID headers", {
  for (id_column in c("StudentID", "ParticipantID", "UserID", "PersonName")) {
    fixture <- expand.grid(
      identity = paste0("P", 1:6),
      Lesson = c("T1", "T2"),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    names(fixture)[[1L]] <- id_column
    fixture$Group <- rep(c("A", "A", "A", "B", "B", "B"), times = 2L)
    index <- seq_len(nrow(fixture))
    fixture$C1 <- as.integer(index %% 2L == 0L)
    fixture$C2 <- as.integer(index %% 3L == 0L)
    fixture$C3 <- as.integer(index %% 4L == 0L)

    mapping <- ena3d_suggest_raw_mapping(fixture)
    expect_identical(mapping$units, id_column, info = id_column)
    expect_identical(mapping$group, "Group", info = id_column)
    expect_identical(
      ena3d_validate_raw_mapping(fixture, mapping)$unit_count,
      6L,
      info = id_column
    )
  }
})


test_that("automatic group fallback preserves adjacent numeric levels", {
  adjacent <- c(1, 1 + .Machine$double.eps)
  fixture <- expand.grid(
    StudentID = paste0("P", 1:6),
    Wave = c("T1", "T2"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  fixture$ArmValue <- rep(rep(adjacent, each = 3L), times = 2L)
  fixture <- fixture[c("ArmValue", "StudentID", "Wave")]
  index <- seq_len(nrow(fixture))
  fixture$C1 <- as.integer(index %% 2L == 0L)
  fixture$C2 <- as.integer(index %% 3L == 0L)
  fixture$C3 <- as.integer(index %% 4L == 0L)

  mapping <- ena3d_suggest_raw_mapping(fixture)
  validated <- ena3d_validate_raw_mapping(fixture, mapping)

  expect_identical(mapping$group, "ArmValue")
  expect_identical(mapping$units, "StudentID")
  expect_length(validated$group_levels, 2L)
  expect_length(unique(ena3d_value_identity_keys(validated$group_levels)), 2L)
})


test_that("raw mapping rejects unit and conversation role overlap", {
  mapping <- .raw_mapping(units = "Name")
  mapping$conversation <- "Name"
  expect_error(
    ena3d_validate_raw_mapping(.raw_fixture(), mapping),
    "Unit identifier columns cannot also be conversation fields"
  )
})


test_that("raw mapping rejects non-finite typed Group values", {
  fixture <- expand.grid(
    StudentID = paste0("P", 1:3),
    Wave = c("T1", "T2"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  fixture$Group <- rep(c(Inf, Inf, Inf, -Inf, -Inf, -Inf))
  index <- seq_len(nrow(fixture))
  fixture$C1 <- as.integer(index %% 2L == 0L)
  fixture$C2 <- as.integer(index %% 3L == 0L)
  fixture$C3 <- as.integer(index %% 4L == 0L)
  mapping <- list(
    units = "StudentID",
    conversation = "Wave",
    codes = c("C1", "C2", "C3"),
    metadata = character(),
    group = "Group",
    model = "AccumulatedTrajectory",
    window = "MovingStanzaWindow",
    window_size_back = 4L,
    rotation = "SVD"
  )

  expect_error(
    ena3d_validate_raw_mapping(fixture, mapping),
    "non-finite typed values: Group"
  )
  expect_error(
    ena3d_build_ena_from_raw(fixture, mapping),
    "non-finite typed values: Group"
  )
})


test_that("raw mapping rejects non-finite typed unit metadata", {
  fixture <- .raw_fixture()
  fixture$NumericMetadata <- ifelse(
    fixture$Lesson == "Lesson 1", Inf, -Inf
  )
  mapping <- .raw_mapping()
  mapping$metadata <- "NumericMetadata"

  expect_error(
    ena3d_validate_raw_mapping(fixture, mapping),
    "non-finite typed values: NumericMetadata"
  )
  expect_false(any(ena3d_raw_nonfinite_typed(c("Inf", "-Inf"))))
  typed_boundaries <- list(
    as.Date(c(0, Inf), origin = "1970-01-01"),
    as.POSIXct(c(0, Inf), origin = "1970-01-01", tz = "UTC"),
    as.difftime(c(1, Inf), units = "days")
  )
  for (values in typed_boundaries) {
    expect_identical(
      ena3d_raw_nonfinite_typed(values), c(FALSE, TRUE)
    )
  }
})


test_that("raw unit tuple keys are typed and collision-free", {
  fixture <- data.frame(
    first = c(
      paste0("alpha", "\r", "beta"), "alpha", NA_character_, "<NA>",
      "alpha|beta"
    ),
    second = c(
      "gamma", paste0("beta", "\r", "gamma"), "value", "value",
      "gamma"
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  keys <- ena3d_unit_key(fixture, c("first", "second"))

  expect_length(unique(keys), nrow(fixture))
  expect_false(any(grepl("[\r\n]", keys)))
  expect_false(identical(keys[[3L]], keys[[4L]]))
  expect_identical(keys, ena3d_unit_key(fixture, c("first", "second")))
})


test_that("mapped raw rows construct a validator-compatible 3D ENA set", {
  built <- ena3d_build_ena_from_raw(.raw_fixture(), .raw_mapping())
  expect_s3_class(built$ena_obj, "ena.set")
  expect_silent(ena3d_validate_ena_object(built$ena_obj))
  expect_identical(get_ena_group_var(built$ena_obj), "Group")
  expect_identical(
    built$ena_obj$`_function.params`$trajectory.time.by, "Lesson"
  )
  expect_identical(
    built$ena_obj$`_function.params`$trajectory.id.by, "ENA3D_UNIT_ID"
  )
  expect_identical(
    built$ena_obj$`_function.params`$trajectory.group.by, "Group"
  )
  metadata <- names(built$ena_obj$meta.data)
  expect_identical(
    .trajectory_default_variable(
      metadata,
      c(.trajectory_declared_default(built$ena_obj, "time"),
        .trajectory_declared_unit_vars(built$ena_obj)),
      "time"
    ),
    "Lesson"
  )
  expect_identical(
    .trajectory_default_variable(
      metadata,
      c(.trajectory_declared_unit_vars(built$ena_obj),
        .trajectory_declared_default(built$ena_obj, "id")),
      "id",
      exclude = "Lesson"
    ),
    "ENA3D_UNIT_ID"
  )
  expect_true("Lesson" %in% names(built$ena_obj$meta.data))
  expect_true(length(built$dimensions) >= 3L)
  expect_equal(built$units, 8L)
  expect_equal(built$points, 16L)
  expect_equal(built$nodes, 4L)
  expect_equal(
    length(unique(as.character(built$ena_obj$points$ENA_UNIT))),
    8L
  )
})


test_that("constructed ENA uses canonical IDs and restores unit metadata", {
  units <- data.frame(
    Group = c("A", "A", "A", "B", "B", "B"),
    UnitPart1 = c("A.B", "A", "D", "F", "H", "J"),
    UnitPart2 = c("C", "B.C", "E", "G", "I", "K"),
    CohortNote = c("alpha", "beta", "gamma", "delta", "epsilon", "zeta"),
    stringsAsFactors = FALSE
  )
  fixture <- merge(
    units,
    data.frame(Lesson = c("T1", "T2"), stringsAsFactors = FALSE),
    by = NULL
  )
  index <- seq_len(nrow(fixture))
  fixture$C1 <- as.integer(index %% 2L == 0L | index %% 5L == 0L)
  fixture$C2 <- as.integer(index %% 3L == 0L | index %% 4L == 0L)
  fixture$C3 <- as.integer(index %% 4L %in% c(0L, 1L))
  fixture$C4 <- as.integer(index %% 5L %in% c(0L, 1L, 2L))
  mapping <- list(
    units = c("UnitPart1", "UnitPart2"),
    conversation = "Lesson",
    codes = c("C1", "C2", "C3", "C4"),
    metadata = "CohortNote",
    group = "Group",
    model = "AccumulatedTrajectory",
    window = "MovingStanzaWindow",
    window_size_back = 4L,
    rotation = "SVD"
  )

  built <- ena3d_build_ena_from_raw(fixture, mapping)
  points <- as.data.frame(built$ena_obj$points)
  metadata_columns <- c(
    "UnitPart1", "UnitPart2", "CohortNote", "Group"
  )

  expect_silent(ena3d_validate_ena_object(built$ena_obj))
  expect_identical(built$units, 6L)
  expect_length(unique(as.character(points$ENA_UNIT)), 6L)
  expect_identical(
    as.character(points$ENA_UNIT), as.character(points$ENA3D_UNIT_ID)
  )
  for (table_name in c("meta.data", "points", "line.weights")) {
    table <- built$ena_obj[[table_name]]
    expect_true(all(metadata_columns %in% names(table)), info = table_name)
    expect_false(any(startsWith(names(table), ".ENA3D_UNIT_KEY")),
                 info = table_name)
  }
  expect_true(all(vapply(metadata_columns, function(column) {
    identical(built$ena_obj$meta.data[[column]],
              built$ena_obj$points[[column]]) &&
      identical(built$ena_obj$points[[column]],
                built$ena_obj$line.weights[[column]])
  }, logical(1L))))

  identities <- unique(points[c("UnitPart1", "UnitPart2", "ENA_UNIT")])
  first <- identities$UnitPart1 == "A.B" & identities$UnitPart2 == "C"
  second <- identities$UnitPart1 == "A" & identities$UnitPart2 == "B.C"
  expect_true(any(first) && any(second))
  expect_false(identical(
    as.character(identities$ENA_UNIT[first]),
    as.character(identities$ENA_UNIT[second])
  ))
})


test_that("restored metadata cannot overwrite generated ENA coordinates", {
  fixture <- .raw_fixture()
  fixture$SVD1 <- paste(fixture$Group, fixture$Name, sep = ":")
  mapping <- .raw_mapping()
  mapping$metadata <- "SVD1"

  expect_error(
    ena3d_build_ena_from_raw(fixture, mapping),
    "conflict with rENA-generated dimensions or edges: SVD1"
  )
})


test_that("group names cannot trigger repaired ENA coordinate collisions", {
  fixture <- .raw_fixture()
  names(fixture)[names(fixture) == "Group"] <- "SVD1"
  mapping <- .raw_mapping(units = c("SVD1", "Name"))
  mapping$group <- "SVD1"

  expect_error(
    ena3d_build_ena_from_raw(fixture, mapping),
    "conflict with rENA-generated dimensions or edges: SVD1"
  )
})


test_that("raw-built ENA reaches path, bootstrap, comparison, and 3D plot APIs", {
  built <- ena3d_build_ena_from_raw(.raw_fixture(), .raw_mapping())
  points <- as.data.frame(built$ena_obj$points)
  dimensions <- built$dimensions[1:3]
  order <- c("Lesson 1", "Lesson 2")

  path <- compute_centroid_path(
    points,
    time_var = "Lesson",
    id_var = "ENA3D_UNIT_ID",
    group_vars = "Group",
    dimensions = dimensions,
    order = order,
    cohort_policy = "complete",
    na_policy = "error"
  )
  bootstrap <- suppressWarnings(bootstrap_centroid_path(
    points,
    time_var = "Lesson",
    id_var = "ENA3D_UNIT_ID",
    group_vars = "Group",
    dimensions = dimensions,
    order = order,
    cohort_policy = "complete",
    na_policy = "error",
    n_boot = 25L,
    seed = 20260718L
  ))
  comparison <- suppressWarnings(compare_independent_centroid_paths(
    points[points$Group == "Experimental", , drop = FALSE],
    points[points$Group == "Control", , drop = FALSE],
    time_var = "Lesson",
    id_var = "ENA3D_UNIT_ID",
    dimensions = dimensions,
    order = order,
    cohort_policy = "complete",
    na_policy = "error",
    n_boot = 25L,
    n_perm = 19L,
    seed = 20260718L,
    labels = c("Experimental", "Control")
  ))
  widget <- plot_centroid_trajectory_3d(bootstrap, dimensions = dimensions)

  expect_s3_class(path, "centroid_path")
  expect_equal(nrow(path), 4L)
  expect_s3_class(bootstrap, "bootstrapped_centroid_path")
  expect_s3_class(comparison, "independent_centroid_path_comparison")
  expect_equal(nrow(comparison), 2L)
  expect_s3_class(widget, "plotly")
  expect_identical(
    unname(attr(widget, "trajectory_dimensions")), paste0("centroid_", dimensions)
  )
  expect_identical(attr(widget, "trajectory_data"), bootstrap)
})


test_that("Excel workbooks are read as plain tables", {
  skip_if_not_installed("readxl")
  path <- file.path(.raw_root, "tests", "testthat", "test_data",
                    "testing_data.xlsx")
  sheets <- ena3d_excel_sheets(path, "xlsx")
  expect_true(length(sheets) >= 1L)
  parsed <- ena3d_read_raw_table(path, "testing_data.xlsx", sheet = sheets[[1L]])
  expect_true(parsed$rows > 0L)
  expect_true(parsed$columns >= 4L)
})


test_that("raw mapping UI is exposed without enabling native R upload", {
  html <- htmltools::renderTags(data_upload_ui("main_app"))$html
  expect_match(html, "main_app-raw_data_file", fixed = TRUE)
  expect_match(html, 'accept=".csv,.xlsx,.xls"', fixed = TRUE)
  expect_match(html, "main_app-ena_exchange_file", fixed = TRUE)
  expect_match(html, 'accept=".ena3d.json"', fixed = TRUE)
  expect_false(grepl("ena_data_file", html, fixed = TRUE))
})


test_that("raw upload module commits only a successfully constructed ENA model", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(.raw_fixture(), path, row.names = FALSE)

  testServer(
    function(input, output, session) {
      rv <- reactiveValues()
      state <- new.env(parent = emptyenv())
      state$ena_obj <- NULL
      state$is_app_initialized <- FALSE
      session$userData$rv <- rv
      session$userData$state <- state
      upload_data(
        input, output, session, rv, state,
        config = list(
          data_limits = ena3d_data_limits(),
          app_version = "test",
          build_id = "raw-import-test"
        )
      )
    },
    {
      session$flushReact()
      session$setInputs(raw_data_file = data.frame(
        name = "coded-data.csv",
        size = file.info(path)$size,
        type = "text/csv",
        datapath = path,
        stringsAsFactors = FALSE
      ))
      session$flushReact()

      session$setInputs(
        raw_unit_columns = c("Group", "Name"),
        raw_conversation_columns = "Lesson",
        raw_code_columns = c("EC", "ICT", "MCO", "ATT"),
        raw_metadata_columns = character(),
        raw_group_column = "Group",
        raw_model = "AccumulatedTrajectory",
        raw_window = "MovingStanzaWindow",
        raw_window_size_back = 4,
        raw_rotation = "SVD",
        raw_build_ena = 1
      )
      session$flushReact()

      expect_true(session$userData$state$is_app_initialized)
      expect_s3_class(session$userData$state$ena_obj, "ena.set")
      expect_identical(session$userData$rv$ena_groupVar, "Group")
      expect_match(session$userData$rv$active_dataset$name,
                   "coded-data.csv (modeled)", fixed = TRUE)
      expect_equal(nrow(session$userData$state$ena_obj$points), 16L)
    }
  )
})
