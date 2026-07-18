# Security and production-boundary helpers for ENA 3D.
#
# Native R serialization is an executable object format.  These helpers do not
# attempt to make arbitrary .RData files safe; they make the trust boundary
# explicit and keep server-packaged fixtures within conservative resource
# budgets.

ena3d_env_number <- function(name, default, minimum = 1, maximum = Inf) {
  raw <- Sys.getenv(name, unset = "")
  value <- if (nzchar(raw)) suppressWarnings(as.numeric(raw)) else as.numeric(default)
  if (length(value) != 1L || is.na(value) || !is.finite(value) ||
      value < minimum || value > maximum) {
    stop(
      sprintf(
        "%s must be one number between %s and %s.",
        name,
        format(minimum, scientific = FALSE),
        if (is.finite(maximum)) format(maximum, scientific = FALSE) else "Inf"
      ),
      call. = FALSE
    )
  }
  value
}


ena3d_data_limits <- function() {
  list(
    # Public exchange files are plain JSON, but still receive a hard
    # pre-parse byte limit. The configurable value can never exceed 10 MiB.
    max_exchange_file_bytes = ena3d_env_number(
      "ENA3D_MAX_EXCHANGE_FILE_BYTES", 2 * 1024^2, maximum = 10 * 1024^2
    ),
    # Raw spreadsheets are parsed as plain tabular data. They never enter an R
    # deserializer, but still receive byte, shape, and cell-count limits before
    # ENA construction.
    max_raw_file_bytes = ena3d_env_number(
      "ENA3D_MAX_RAW_FILE_BYTES", 5 * 1024^2, maximum = 25 * 1024^2
    ),
    max_raw_archive_bytes = ena3d_env_number(
      "ENA3D_MAX_RAW_ARCHIVE_BYTES", 100 * 1024^2, maximum = 512 * 1024^2
    ),
    max_raw_rows = ena3d_env_number(
      "ENA3D_MAX_RAW_ROWS", 100000, maximum = 500000
    ),
    max_raw_columns = ena3d_env_number(
      "ENA3D_MAX_RAW_COLUMNS", 500, maximum = 2000
    ),
    max_raw_cells = ena3d_env_number(
      "ENA3D_MAX_RAW_CELLS", 10000000, maximum = 50000000
    ),
    # This is a supply-chain guard for bundled fixtures, not permission to
    # accept serialized objects from a browser.
    max_file_bytes = ena3d_env_number(
      "ENA3D_MAX_TRUSTED_FILE_BYTES", 25 * 1024^2, maximum = 100 * 1024^2
    ),
    max_loaded_bytes = ena3d_env_number(
      "ENA3D_MAX_LOADED_BYTES", 512 * 1024^2, maximum = 2 * 1024^3
    ),
    max_saved_objects = ena3d_env_number(
      "ENA3D_MAX_SAVED_OBJECTS", 20, maximum = 100
    ),
    max_point_rows = ena3d_env_number(
      "ENA3D_MAX_POINT_ROWS", 50000, maximum = 250000
    ),
    max_nodes = ena3d_env_number(
      "ENA3D_MAX_NODES", 50, maximum = 100
    ),
    max_dimensions = ena3d_env_number(
      "ENA3D_MAX_DIMENSIONS", 200, maximum = 500
    ),
    max_metadata_columns = ena3d_env_number(
      "ENA3D_MAX_METADATA_COLUMNS", 100, maximum = 500
    ),
    max_table_cells = ena3d_env_number(
      "ENA3D_MAX_TABLE_CELLS", 20000000, maximum = 100000000
    ),
    max_group_levels = ena3d_env_number(
      "ENA3D_MAX_GROUP_LEVELS", 50, maximum = 200
    ),
    max_units = ena3d_env_number(
      "ENA3D_MAX_UNITS", 50000, maximum = 250000
    )
  )
}


ena3d_assert_within <- function(value, limit, label) {
  value <- as.numeric(value)
  limit <- as.numeric(limit)
  if (length(value) != 1L || is.na(value) || !is.finite(value) || value > limit) {
    stop(
      sprintf(
        "%s exceeds the configured limit (%s > %s).",
        label,
        format(value, scientific = FALSE),
        format(limit, scientific = FALSE)
      ),
      call. = FALSE
    )
  }
  invisible(value)
}


ena3d_normalize_log_value <- function(value, max_chars = 240L) {
  if (is.null(value) || !length(value)) return("-")
  value <- paste(as.character(value), collapse = ",")
  value <- gsub("[[:cntrl:]]+", " ", value)
  value <- gsub("[[:space:]]+", "_", trimws(value))
  if (!nzchar(value)) value <- "-"
  substr(value, 1L, max_chars)
}


ena3d_spreadsheet_safe_text <- function(value) {
  value <- as.character(value)
  dangerous <- !is.na(value) & grepl("^[[:space:]]*[=+@-]", value)
  value[dangerous] <- paste0("'", value[dangerous])
  value
}


ena3d_spreadsheet_safe_headers <- function(value) {
  value <- as.character(value)

  # Escape an already-apostrophe-prefixed dangerous header as well. This makes
  # the transformation collision-safe: `=x`, `'=x`, and `''=x` become three
  # distinct, inert headers instead of the first two both becoming `'=x`.
  dangerous_or_escaped <- !is.na(value) &
    grepl("^'*[[:space:]]*[=+@-]", value)
  value[dangerous_or_escaped] <- paste0("'", value[dangerous_or_escaped])
  value
}


ena3d_spreadsheet_safe_frame <- function(data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, optional = TRUE)
  original_names <- names(data)

  for (index in seq_along(data)) {
    if (is.factor(data[[index]]) || is.character(data[[index]])) {
      data[[index]] <- ena3d_spreadsheet_safe_text(data[[index]])
    }
  }

  names(data) <- ena3d_spreadsheet_safe_headers(original_names)
  if (!anyDuplicated(original_names) && anyDuplicated(names(data))) {
    stop(
      "Spreadsheet-safe CSV header escaping produced duplicate names.",
      call. = FALSE
    )
  }
  data
}


ena3d_write_safe_csv <- function(data, file) {
  utils::write.csv(
    ena3d_spreadsheet_safe_frame(data),
    file,
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
}


ena3d_security_log <- function(event, level = "INFO", fields = list()) {
  if (!is.character(event) || length(event) != 1L || !nzchar(event)) {
    stop("Security log events require one non-empty event name.", call. = FALSE)
  }
  if (is.null(names(fields)) || any(!nzchar(names(fields)))) {
    if (length(fields)) stop("Security log fields must be named.", call. = FALSE)
  }

  record <- c(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    level = toupper(level),
    event = event,
    build = Sys.getenv("ENA3D_BUILD_ID", unset = "development"),
    vapply(fields, ena3d_normalize_log_value, character(1))
  )
  line <- paste(
    "ena3d_event",
    paste0(names(record), "=", unname(record), collapse = " ")
  )
  message(line)
  invisible(line)
}


ena3d_public_upload_message <- function() {
  paste(
    "Native R uploads are disabled because R data files can contain",
    "executable objects. Use a versioned .ena3d.json exchange file, a",
    "trusted sample dataset, or the approved offline conversion workflow."
  )
}


ena3d_resolve_exchange_upload <- function(upload, limits = ena3d_data_limits(),
                                          upload_root = tempdir()) {
  if (is.data.frame(upload)) {
    if (nrow(upload) != 1L) {
      stop("Select exactly one .ena3d.json exchange file.", call. = FALSE)
    }
    upload <- as.list(upload[1L, , drop = FALSE])
  }
  if (!is.list(upload)) {
    stop("The exchange upload metadata is invalid.", call. = FALSE)
  }

  client_name <- upload[["name"]]
  data_path <- upload[["datapath"]]
  if (!is.character(client_name) || length(client_name) != 1L ||
      is.na(client_name) || !nzchar(client_name) ||
      !identical(basename(client_name), client_name) ||
      !grepl("\\.ena3d\\.json$", client_name, ignore.case = TRUE)) {
    stop("Only files ending in .ena3d.json are accepted.", call. = FALSE)
  }
  if (!is.character(data_path) || length(data_path) != 1L ||
      is.na(data_path) || !nzchar(data_path) || !file.exists(data_path) ||
      isTRUE(file.info(data_path)$isdir)) {
    stop("The uploaded exchange file is unavailable.", call. = FALSE)
  }

  root <- normalizePath(upload_root, mustWork = TRUE)
  resolved <- normalizePath(data_path, mustWork = TRUE)
  root_prefix <- paste0(root, .Platform$file.sep)
  if (!identical(resolved, root) && !startsWith(resolved, root_prefix)) {
    stop("The upload path is outside the server upload directory.", call. = FALSE)
  }

  file_size <- file.info(resolved)$size
  ena3d_assert_within(
    file_size,
    limits$max_exchange_file_bytes,
    ".ena3d.json file size"
  )
  list(
    path = resolved,
    name = client_name,
    size = as.numeric(file_size)
  )
}


ena3d_resolve_trusted_sample <- function(sample_root, requested_name) {
  if (!is.character(requested_name) || length(requested_name) != 1L ||
      is.na(requested_name) || !nzchar(requested_name) ||
      !identical(basename(requested_name), requested_name)) {
    stop("The requested sample name is invalid.", call. = FALSE)
  }
  if (!grepl("\\.[Rr][Dd]ata$", requested_name)) {
    stop("Trusted samples must use the .RData or .Rdata extension.", call. = FALSE)
  }

  root <- normalizePath(sample_root, mustWork = TRUE)
  candidate <- file.path(root, requested_name)
  if (!file.exists(candidate) || isTRUE(file.info(candidate)$isdir)) {
    stop("The requested trusted sample does not exist.", call. = FALSE)
  }
  resolved <- normalizePath(candidate, mustWork = TRUE)
  if (!identical(dirname(resolved), root)) {
    stop("The requested sample resolves outside the trusted sample directory.",
         call. = FALSE)
  }
  resolved
}


ena3d_list_trusted_samples <- function(sample_root) {
  root <- normalizePath(sample_root, mustWork = TRUE)
  candidates <- list.files(
    root,
    pattern = "\\.[Rr][Dd]ata$",
    full.names = FALSE,
    recursive = FALSE
  )
  candidates[vapply(candidates, function(name) {
    !inherits(
      try(ena3d_resolve_trusted_sample(root, name), silent = TRUE),
      "try-error"
    )
  }, logical(1))]
}
