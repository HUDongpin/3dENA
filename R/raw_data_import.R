# Safe raw spreadsheet import and ENA construction.

ena3d_raw_extensions <- function() c("csv", "xlsx", "xls")


ena3d_resolve_raw_upload <- function(upload, limits = ena3d_data_limits(),
                                     upload_root = tempdir()) {
  if (is.data.frame(upload)) {
    if (nrow(upload) != 1L) {
      stop("Select exactly one Excel or CSV file.", call. = FALSE)
    }
    upload <- as.list(upload[1L, , drop = FALSE])
  }
  if (!is.list(upload)) {
    stop("The raw-data upload metadata is invalid.", call. = FALSE)
  }

  client_name <- upload[["name"]]
  data_path <- upload[["datapath"]]
  if (!is.character(client_name) || length(client_name) != 1L ||
      is.na(client_name) || !nzchar(client_name) ||
      !identical(basename(client_name), client_name)) {
    stop("The uploaded filename is invalid.", call. = FALSE)
  }
  extension <- tolower(tools::file_ext(client_name))
  if (!extension %in% ena3d_raw_extensions()) {
    stop("Only .csv, .xlsx, and .xls raw-data files are accepted.", call. = FALSE)
  }
  if (!is.character(data_path) || length(data_path) != 1L ||
      is.na(data_path) || !nzchar(data_path) || !file.exists(data_path) ||
      isTRUE(file.info(data_path)$isdir)) {
    stop("The uploaded raw-data file is unavailable.", call. = FALSE)
  }

  root <- normalizePath(upload_root, mustWork = TRUE)
  resolved <- normalizePath(data_path, mustWork = TRUE)
  root_prefix <- paste0(root, .Platform$file.sep)
  if (!identical(resolved, root) && !startsWith(resolved, root_prefix)) {
    stop("The raw-data upload path is outside the server upload directory.",
         call. = FALSE)
  }

  file_size <- as.numeric(file.info(resolved)$size)
  ena3d_assert_within(
    file_size, limits$max_raw_file_bytes, "raw spreadsheet file size"
  )
  list(
    path = resolved,
    name = client_name,
    extension = extension,
    size = file_size,
    sha256 = digest::digest(file = resolved, algo = "sha256")
  )
}


ena3d_raw_magic_check <- function(path, extension,
                                  limits = ena3d_data_limits()) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  header <- readBin(connection, what = "raw", n = 8L)
  if (identical(extension, "xlsx")) {
    zip_magic <- as.raw(c(0x50, 0x4b, 0x03, 0x04))
    if (length(header) < 4L || !identical(header[1:4], zip_magic)) {
      stop("The .xlsx extension does not match an Excel workbook.", call. = FALSE)
    }
    archive <- tryCatch(
      utils::unzip(path, list = TRUE),
      error = function(error) {
        stop(sprintf("The Excel archive is invalid: %s", conditionMessage(error)),
             call. = FALSE)
      }
    )
    if (!nrow(archive) || !"Length" %in% names(archive)) {
      stop("The Excel archive contains no readable workbook entries.",
           call. = FALSE)
    }
    uncompressed_bytes <- sum(as.numeric(archive$Length))
    ena3d_assert_within(
      uncompressed_bytes,
      limits$max_raw_archive_bytes,
      "uncompressed Excel archive size"
    )
  } else if (identical(extension, "xls")) {
    ole_magic <- as.raw(c(0xd0, 0xcf, 0x11, 0xe0, 0xa1, 0xb1, 0x1a, 0xe1))
    if (length(header) < 8L || !identical(header[1:8], ole_magic)) {
      stop("The .xls extension does not match a legacy Excel workbook.",
           call. = FALSE)
    }
  } else if (any(header == as.raw(0x00))) {
    stop("CSV input must be plain text and may not contain NUL bytes.",
         call. = FALSE)
  }
  invisible(TRUE)
}


ena3d_excel_sheets <- function(path, extension = tolower(tools::file_ext(path)),
                               limits = ena3d_data_limits()) {
  if (!extension %in% c("xlsx", "xls")) return(character())
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Excel support is unavailable because the readxl package is missing.",
         call. = FALSE)
  }
  ena3d_raw_magic_check(path, extension, limits = limits)
  sheets <- readxl::excel_sheets(path)
  sheets <- enc2utf8(as.character(sheets))
  if (!length(sheets)) stop("The Excel workbook contains no worksheets.", call. = FALSE)
  sheets
}


ena3d_detect_csv_separator <- function(path) {
  candidates <- c(",", ";", "\t")
  scores <- vapply(candidates, function(separator) {
    counts <- tryCatch(
      utils::count.fields(
        path, sep = separator, quote = "\"", comment.char = "",
        blank.lines.skip = TRUE
      ),
      error = function(...) numeric()
    )
    counts <- counts[is.finite(counts)]
    if (!length(counts)) return(1)
    stats::median(counts)
  }, numeric(1))
  candidates[[which.max(scores)]]
}


ena3d_validate_raw_frame <- function(data, limits = ena3d_data_limits()) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, optional = TRUE)
  if (!nrow(data) || !ncol(data)) {
    stop("The selected table must contain a header and at least one data row.",
         call. = FALSE)
  }
  ena3d_assert_within(nrow(data), limits$max_raw_rows, "raw-data row count")
  ena3d_assert_within(ncol(data), limits$max_raw_columns, "raw-data column count")
  ena3d_assert_within(
    nrow(data) * ncol(data), limits$max_raw_cells, "raw-data cell count"
  )

  column_names <- enc2utf8(names(data))
  if (anyNA(column_names) || any(!nzchar(trimws(column_names)))) {
    stop("Every raw-data column must have a non-empty header.", call. = FALSE)
  }
  if (anyDuplicated(column_names)) {
    duplicates <- unique(column_names[duplicated(column_names)])
    stop(
      sprintf("Raw-data column headers must be unique: %s.",
              paste(duplicates, collapse = ", ")),
      call. = FALSE
    )
  }
  if (any(nchar(column_names, type = "bytes") > 256L) ||
      any(grepl("[[:cntrl:]]", column_names))) {
    stop("Raw-data headers contain control characters or exceed 256 bytes.",
         call. = FALSE)
  }
  if (any(vapply(data, is.list, logical(1)))) {
    stop("Nested/list-valued spreadsheet columns are not supported.",
         call. = FALSE)
  }
  names(data) <- column_names
  data
}


ena3d_read_raw_table <- function(path, client_name = basename(path), sheet = NULL,
                                 limits = ena3d_data_limits()) {
  extension <- tolower(tools::file_ext(client_name))
  if (!extension %in% ena3d_raw_extensions()) {
    stop("Only .csv, .xlsx, and .xls raw-data files are accepted.", call. = FALSE)
  }
  ena3d_assert_within(
    as.numeric(file.info(path)$size), limits$max_raw_file_bytes,
    "raw spreadsheet file size"
  )
  ena3d_raw_magic_check(path, extension, limits = limits)

  if (identical(extension, "csv")) {
    separator <- ena3d_detect_csv_separator(path)
    data <- tryCatch(
      utils::read.table(
        path,
        header = TRUE,
        sep = separator,
        quote = "\"",
        comment.char = "",
        stringsAsFactors = FALSE,
        check.names = FALSE,
        na.strings = c("", "NA"),
        fileEncoding = "UTF-8-BOM",
        strip.white = FALSE
      ),
      error = function(error) {
        stop(sprintf("Could not parse the CSV file: %s", conditionMessage(error)),
             call. = FALSE)
      }
    )
    selected_sheet <- NA_character_
  } else {
    sheets <- ena3d_excel_sheets(path, extension, limits = limits)
    if (is.null(sheet) || !length(sheet) || is.na(sheet) || !nzchar(sheet)) {
      sheet <- sheets[[1L]]
    }
    if (!sheet %in% sheets) {
      stop("The selected Excel worksheet does not exist.", call. = FALSE)
    }
    data <- tryCatch(
      readxl::read_excel(
        path,
        sheet = sheet,
        na = c("", "NA"),
        .name_repair = "minimal"
      ),
      error = function(error) {
        stop(sprintf("Could not parse the Excel worksheet: %s",
                     conditionMessage(error)), call. = FALSE)
      }
    )
    selected_sheet <- sheet
  }

  data <- ena3d_validate_raw_frame(data, limits = limits)
  list(
    data = data,
    sheet = selected_sheet,
    rows = nrow(data),
    columns = ncol(data),
    column_names = names(data)
  )
}


ena3d_raw_nonblank <- function(values) {
  !is.na(values) & nzchar(trimws(as.character(values)))
}


ena3d_raw_is_binary_code <- function(values) {
  if (is.logical(values)) return(any(values, na.rm = TRUE))
  numeric_values <- suppressWarnings(as.numeric(as.character(values)))
  observed <- numeric_values[!is.na(values)]
  if (!length(observed) || anyNA(observed) || any(!is.finite(observed))) {
    return(FALSE)
  }
  all(observed %in% c(0, 1)) && length(unique(observed)) > 1L
}


ena3d_unit_key <- function(data, columns) {
  values <- lapply(data[columns], function(column) {
    value <- as.character(column)
    value[is.na(value)] <- "<NA>"
    value
  })
  do.call(paste, c(values, sep = "\r"))
}


ena3d_suggest_raw_mapping <- function(data, limits = ena3d_data_limits()) {
  data <- ena3d_validate_raw_frame(data, limits = limits)
  columns <- names(data)
  match_names <- function(pattern) {
    columns[grepl(pattern, columns, ignore.case = TRUE, perl = TRUE)]
  }

  group_candidates <- match_names(
    "(^|[_. -])(group|condition|treatment|cohort|class)([_. -]|$)"
  )
  if (!length(group_candidates)) {
    categorical <- columns[vapply(data, function(values) {
      count <- length(unique(as.character(values[ena3d_raw_nonblank(values)])))
      count >= 2L && count <= min(10L, limits$max_group_levels)
    }, logical(1))]
    group_candidates <- categorical[!vapply(data[categorical],
                                             ena3d_raw_is_binary_code, logical(1))]
  }
  group <- if (length(group_candidates)) group_candidates[[1L]] else columns[[1L]]

  conversation_candidates <- match_names(
    "(^|[_. -])(lesson|week|time|session|phase|wave|conversation|stanza|turn|date)([_. -]|$)"
  )
  conversation <- if (length(conversation_candidates)) {
    conversation_candidates[[1L]]
  } else {
    setdiff(columns, group)[[1L]]
  }

  id_candidates <- match_names(
    "(^|[_. -])(id|name|user|participant|student|case|person)([_. -]|$)"
  )
  id_candidates <- setdiff(id_candidates, conversation)
  units <- if (length(id_candidates)) id_candidates[[1L]] else group

  if (group %in% columns && !group %in% units) {
    id_key <- ena3d_unit_key(data, units)
    groups_per_id <- tapply(
      as.character(data[[group]]), id_key,
      function(values) length(unique(values[ena3d_raw_nonblank(values)]))
    )
    if (any(groups_per_id > 1L, na.rm = TRUE)) units <- c(group, units)
  }

  code_candidates <- columns[vapply(data, ena3d_raw_is_binary_code, logical(1))]
  codes <- setdiff(code_candidates, unique(c(group, conversation, units)))
  metadata <- setdiff(group, unique(c(units, conversation, codes)))
  temporal <- grepl(
    "lesson|week|time|session|phase|wave|date",
    paste(conversation, collapse = " "), ignore.case = TRUE
  )

  list(
    units = units,
    conversation = conversation,
    codes = codes,
    metadata = metadata,
    group = group,
    model = if (temporal) "AccumulatedTrajectory" else "EndPoint",
    window = "MovingStanzaWindow",
    window_size_back = 4L,
    rotation = "SVD"
  )
}


ena3d_coerce_raw_codes <- function(data, code_columns) {
  for (column in code_columns) {
    original <- data[[column]]
    if (is.logical(original)) {
      values <- as.numeric(original)
    } else {
      values <- suppressWarnings(as.numeric(as.character(original)))
    }
    if (length(values) != length(original) ||
        any(is.na(values) & !is.na(original)) || anyNA(values) ||
        any(!is.finite(values)) || any(values < 0)) {
      stop(
        sprintf(
          "Code column `%s` must contain complete, finite, non-negative numbers.",
          column
        ),
        call. = FALSE
      )
    }
    data[[column]] <- as.numeric(values > 0)
  }
  data
}


ena3d_validate_raw_mapping <- function(data, mapping,
                                       limits = ena3d_data_limits()) {
  data <- ena3d_validate_raw_frame(data, limits = limits)
  if (!is.list(mapping)) stop("The field mapping is invalid.", call. = FALSE)
  selected <- function(name) {
    value <- unique(as.character(mapping[[name]]))
    value[!is.na(value) & nzchar(value)]
  }
  units <- selected("units")
  conversation <- selected("conversation")
  codes <- selected("codes")
  metadata <- selected("metadata")
  group <- selected("group")

  if (!length(units)) stop("Choose at least one ENA unit identifier.", call. = FALSE)
  if (!length(conversation)) {
    stop("Choose at least one conversation/sequence field.", call. = FALSE)
  }
  if (length(codes) < 3L) {
    stop("Choose at least three code columns for a 3D ENA model.", call. = FALSE)
  }
  if (length(group) != 1L) {
    stop("Choose exactly one primary grouping field.", call. = FALSE)
  }
  all_selected <- unique(c(units, conversation, codes, metadata, group))
  missing <- setdiff(all_selected, names(data))
  if (length(missing)) {
    stop(sprintf("Mapped columns are absent: %s.", paste(missing, collapse = ", ")),
         call. = FALSE)
  }
  code_overlap <- intersect(codes, unique(c(units, conversation, metadata)))
  if (length(code_overlap)) {
    stop(sprintf("Code columns cannot also have another role: %s.",
                 paste(code_overlap, collapse = ", ")), call. = FALSE)
  }
  metadata_overlap <- intersect(metadata, unique(c(units, conversation)))
  if (length(metadata_overlap)) {
    stop(sprintf("Metadata columns duplicate unit/conversation roles: %s.",
                 paste(metadata_overlap, collapse = ", ")), call. = FALSE)
  }
  if (group %in% codes) {
    stop("The primary grouping field cannot be a code column.", call. = FALSE)
  }
  if (!group %in% c(units, metadata)) {
    metadata <- c(metadata, group)
    metadata <- setdiff(metadata, conversation)
  }
  if (!group %in% c(units, metadata)) {
    stop(
      "The primary grouping field must be a unit identifier or unit-level metadata.",
      call. = FALSE
    )
  }

  required_complete <- unique(c(units, conversation, group))
  incomplete <- required_complete[vapply(data[required_complete], function(values) {
    any(!ena3d_raw_nonblank(values))
  }, logical(1))]
  if (length(incomplete)) {
    stop(sprintf("Mapped identifiers contain missing or blank values: %s.",
                 paste(incomplete, collapse = ", ")), call. = FALSE)
  }

  unit_key <- ena3d_unit_key(data, units)
  unit_count <- length(unique(unit_key))
  if (unit_count < 3L) {
    stop("At least three unique ENA units are required for a 3D model.",
         call. = FALSE)
  }
  ena3d_assert_within(unit_count, limits$max_units, "unique ENA unit count")

  grouping_values <- as.character(data[[group]])
  groups_per_unit <- tapply(grouping_values, unit_key, function(values) {
    length(unique(values))
  })
  if (any(groups_per_unit != 1L)) {
    stop(
      paste0(
        "The selected unit identifier maps some names to multiple groups. ",
        "Add the grouping field to the unit identifier so same labels in ",
        "different groups remain different people."
      ),
      call. = FALSE
    )
  }

  if (length(metadata)) {
    inconsistent_metadata <- metadata[vapply(data[metadata], function(values) {
      counts <- tapply(as.character(values), unit_key, function(unit_values) {
        length(unique(unit_values[!is.na(unit_values)]))
      })
      any(counts > 1L, na.rm = TRUE)
    }, logical(1))]
    if (length(inconsistent_metadata)) {
      stop(sprintf(
        "Unit-level metadata changes within an ENA unit: %s.",
        paste(inconsistent_metadata, collapse = ", ")
      ), call. = FALSE)
    }
  }

  group_levels <- unique(grouping_values)
  ena3d_assert_within(
    length(group_levels), limits$max_group_levels,
    sprintf("level count for grouping column `%s`", group)
  )
  data <- ena3d_coerce_raw_codes(data, codes)
  empty_codes <- codes[vapply(data[codes], function(values) !any(values > 0),
                              logical(1))]
  if (length(empty_codes)) {
    stop(sprintf("Selected code columns contain no occurrences: %s.",
                 paste(empty_codes, collapse = ", ")), call. = FALSE)
  }

  value_or <- function(value, fallback) {
    if (is.null(value) || !length(value)) fallback else value
  }
  model <- match.arg(
    as.character(value_or(mapping$model, "EndPoint")),
    c("EndPoint", "AccumulatedTrajectory", "SeparateTrajectory")
  )
  window <- match.arg(
    as.character(value_or(mapping$window, "MovingStanzaWindow")),
    c("MovingStanzaWindow", "Conversation")
  )
  window_size_back <- suppressWarnings(as.integer(mapping$window_size_back))
  if (length(window_size_back) != 1L || is.na(window_size_back) ||
      window_size_back < 1L || window_size_back > 100L) {
    stop("Window size must be an integer from 1 through 100.", call. = FALSE)
  }
  rotation <- match.arg(
    as.character(value_or(mapping$rotation, "SVD")), c("SVD", "Means")
  )
  if (identical(rotation, "Means") && length(group_levels) < 2L) {
    stop("Means rotation requires at least two group levels.", call. = FALSE)
  }

  list(
    data = data,
    mapping = list(
      units = units,
      conversation = conversation,
      codes = codes,
      metadata = metadata,
      group = group,
      model = model,
      window = window,
      window_size_back = window_size_back,
      rotation = rotation,
      rotation_groups = if (identical(rotation, "Means")) {
        group_levels[1:2]
      } else {
        character()
      }
    ),
    unit_count = unit_count,
    group_levels = group_levels
  )
}


ena3d_normalize_constructed_ena <- function(ena_obj, group_var) {
  points <- ena_obj$points
  metadata_names <- names(ena_obj$meta.data)
  point_metadata <- names(points)[vapply(
    points, inherits, logical(1), "ena.metadata"
  )]
  trajectory_metadata <- setdiff(point_metadata, metadata_names)

  if (length(trajectory_metadata)) {
    old_metadata <- data.table::copy(ena_obj$meta.data)
    old_weights <- data.table::copy(ena_obj$line.weights)
    edge_names <- setdiff(names(old_weights), metadata_names)
    point_frame <- as.data.frame(
      points, stringsAsFactors = FALSE, optional = TRUE
    )
    weight_frame <- as.data.frame(
      old_weights, stringsAsFactors = FALSE, optional = TRUE
    )
    added <- data.table::as.data.table(point_frame[trajectory_metadata])
    for (name in trajectory_metadata) {
      class(added[[name]]) <- unique(c("ena.metadata", class(added[[name]])))
    }
    ena_obj$meta.data <- data.table::as.data.table(cbind(old_metadata, added))
    ena_obj$line.weights <- data.table::as.data.table(cbind(
      weight_frame[metadata_names], added, weight_frame[edge_names]
    ))
    class(ena_obj$line.weights) <- unique(c(
      "ena.line.weights", "ena.matrix", class(ena_obj$line.weights)
    ))
  }
  ena_obj$`_function.params`$groupVar <- group_var
  ena_obj
}


ena3d_add_constructed_participant_id <- function(
    ena_obj, stem = "ENA3D_UNIT_ID") {
  existing <- unique(c(
    names(ena_obj$meta.data), names(ena_obj$points), names(ena_obj$line.weights)
  ))
  column <- stem
  suffix <- 2L
  while (column %in% existing) {
    column <- paste0(stem, "_", suffix)
    suffix <- suffix + 1L
  }

  values <- as.character(ena_obj$points[["ENA_UNIT"]])
  added <- structure(
    list(rENA::as.ena.metadata(values)),
    names = column,
    row.names = .set_row_names(length(values)),
    class = "data.frame"
  )

  metadata_frame <- as.data.frame(
    ena_obj$meta.data, stringsAsFactors = FALSE, optional = TRUE
  )
  point_frame <- as.data.frame(
    ena_obj$points, stringsAsFactors = FALSE, optional = TRUE
  )
  weight_frame <- as.data.frame(
    ena_obj$line.weights, stringsAsFactors = FALSE, optional = TRUE
  )
  metadata_names <- names(metadata_frame)
  dimension_names <- setdiff(names(point_frame), metadata_names)
  edge_names <- setdiff(names(weight_frame), metadata_names)

  ena_obj$meta.data <- data.table::as.data.table(cbind(metadata_frame, added))
  ena_obj$points <- data.table::as.data.table(cbind(
    point_frame[metadata_names], added, point_frame[dimension_names]
  ))
  class(ena_obj$points) <- unique(c(
    "ena.points", "ena.matrix", class(ena_obj$points)
  ))
  ena_obj$line.weights <- data.table::as.data.table(cbind(
    weight_frame[metadata_names], added, weight_frame[edge_names]
  ))
  class(ena_obj$line.weights) <- unique(c(
    "ena.line.weights", "ena.matrix", class(ena_obj$line.weights)
  ))
  list(ena_obj = ena_obj, column = column)
}


ena3d_build_ena_from_raw <- function(data, mapping,
                                     limits = ena3d_data_limits()) {
  validated <- ena3d_validate_raw_mapping(data, mapping, limits = limits)
  prepared_data <- validated$data
  specification <- validated$mapping
  means_rotation <- identical(specification$rotation, "Means")

  ena_obj <- tryCatch(
    rENA::ena(
      data = prepared_data,
      codes = specification$codes,
      units = specification$units,
      conversation = specification$conversation,
      metadata = if (length(specification$metadata)) {
        specification$metadata
      } else {
        NULL
      },
      model = specification$model,
      weight.by = "binary",
      window = specification$window,
      window.size.back = specification$window_size_back,
      groupVar = if (means_rotation) specification$group else NULL,
      groups = if (means_rotation) specification$rotation_groups else NULL,
      runTest = FALSE,
      include.plots = FALSE,
      print.plots = FALSE
    ),
    error = function(error) {
      stop(sprintf("ENA construction failed: %s", conditionMessage(error)),
           call. = FALSE)
    }
  )
  ena_obj <- ena3d_normalize_constructed_ena(ena_obj, specification$group)
  participant_id <- ena3d_add_constructed_participant_id(ena_obj)
  ena_obj <- participant_id$ena_obj
  ena_obj$`_function.params`$trajectory.time.by <-
    specification$conversation[[length(specification$conversation)]]
  ena_obj$`_function.params`$trajectory.id.by <- participant_id$column
  ena_obj$`_function.params`$trajectory.group.by <- specification$group
  dimensions <- ena3d_dimension_names(ena_obj)
  if (length(dimensions) < 3L) {
    stop(
      "The mapped data produced fewer than three ENA dimensions; add units or codes.",
      call. = FALSE
    )
  }
  ena3d_validate_ena_object(
    ena_obj, object_name = "Constructed raw-data ENA model", limits = limits
  )

  list(
    ena_obj = ena_obj,
    mapping = specification,
    raw_rows = nrow(prepared_data),
    units = validated$unit_count,
    groups = validated$group_levels,
    dimensions = dimensions,
    nodes = nrow(ena_obj$rotation$nodes),
    points = nrow(ena_obj$points),
    participant_id = participant_id$column
  )
}
