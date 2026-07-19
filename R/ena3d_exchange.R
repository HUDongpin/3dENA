# 3D ENA exchange format v1.
#
# This module intentionally accepts only JSON scalar data. It never evaluates
# expressions or deserializes native R objects. Class markers needed by rENA
# are assigned below from fixed server-side rules, never from file content.

ENA3D_EXCHANGE_FORMAT <- "ena3d-exchange"
ENA3D_EXCHANGE_VERSION <- 1L


ena3d_exchange_fail <- function(message) {
  stop(paste0("Invalid .ena3d.json exchange: ", message), call. = FALSE)
}


ena3d_exchange_assert_object <- function(value, fields, context) {
  if (!is.list(value) || is.null(names(value)) ||
      any(is.na(names(value))) || any(!nzchar(names(value)))) {
    ena3d_exchange_fail(sprintf("%s must be a JSON object.", context))
  }
  if (anyDuplicated(names(value))) {
    duplicated_names <- unique(names(value)[duplicated(names(value))])
    ena3d_exchange_fail(sprintf(
      "%s contains duplicate field(s): %s.",
      context,
      paste(duplicated_names, collapse = ", ")
    ))
  }
  missing_fields <- setdiff(fields, names(value))
  unknown_fields <- setdiff(names(value), fields)
  if (length(missing_fields) || length(unknown_fields) ||
      length(names(value)) != length(fields)) {
    details <- c(
      if (length(missing_fields)) {
        paste0("missing: ", paste(missing_fields, collapse = ", "))
      },
      if (length(unknown_fields)) {
        paste0("unknown: ", paste(unknown_fields, collapse = ", "))
      }
    )
    ena3d_exchange_fail(sprintf(
      "%s has the wrong fields (%s).",
      context,
      paste(details, collapse = "; ")
    ))
  }
  invisible(value)
}


ena3d_exchange_assert_array <- function(value, context, nonempty = TRUE) {
  if (!is.list(value) || !is.null(names(value))) {
    ena3d_exchange_fail(sprintf("%s must be a JSON array.", context))
  }
  if (isTRUE(nonempty) && !length(value)) {
    ena3d_exchange_fail(sprintf("%s must not be empty.", context))
  }
  invisible(value)
}


ena3d_exchange_scalar_string <- function(value, context, identifier = FALSE) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !validUTF8(value)) {
    ena3d_exchange_fail(sprintf("%s must be one UTF-8 string.", context))
  }
  if (isTRUE(identifier) &&
      (!nzchar(value) || nchar(value, type = "bytes") > 256L ||
       grepl("[[:cntrl:]]", value))) {
    ena3d_exchange_fail(sprintf(
      "%s must be a non-empty identifier of at most 256 UTF-8 bytes.",
      context
    ))
  }
  value
}


ena3d_exchange_string_array <- function(value, context, identifier = FALSE) {
  ena3d_exchange_assert_array(value, context)
  decoded <- vapply(seq_along(value), function(index) {
    ena3d_exchange_scalar_string(
      value[[index]],
      sprintf("%s[%d]", context, index),
      identifier = identifier
    )
  }, character(1))
  if (anyDuplicated(decoded)) {
    ena3d_exchange_fail(sprintf("%s contains duplicate values.", context))
  }
  decoded
}


ena3d_exchange_decode_values <- function(values, type, context,
                                         specification = list()) {
  ena3d_exchange_assert_array(values, paste0(context, ".values"))
  if (type %in% c("factor", "ordered")) {
    levels <- ena3d_exchange_string_array(
      specification$levels, paste0(context, ".levels")
    )
    decoded <- ena3d_exchange_decode_values(values, "character", context)
    unknown <- setdiff(unique(decoded[!is.na(decoded)]), levels)
    if (length(unknown)) {
      ena3d_exchange_fail(sprintf(
        "%s.values contain values absent from levels: %s.",
        context,
        paste(unknown, collapse = ", ")
      ))
    }
    return(factor(
      decoded,
      levels = levels,
      ordered = identical(type, "ordered")
    ))
  }
  if (identical(type, "date")) {
    decoded <- ena3d_exchange_decode_values(values, "character", context)
    nonmissing <- !is.na(decoded)
    if (any(nonmissing & !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", decoded))) {
      ena3d_exchange_fail(sprintf(
        "%s.values must use ISO 8601 YYYY-MM-DD dates or null.", context
      ))
    }
    result <- as.Date(decoded, format = "%Y-%m-%d")
    if (any(nonmissing & is.na(result))) {
      ena3d_exchange_fail(sprintf("%s.values contain an invalid date.", context))
    }
    return(result)
  }
  if (identical(type, "datetime")) {
    timezone <- ena3d_exchange_scalar_string(
      specification$timezone, paste0(context, ".timezone")
    )
    if (!timezone %in% unique(c("UTC", OlsonNames()))) {
      ena3d_exchange_fail(sprintf(
        "%s.timezone must be UTC or an IANA timezone name.", context
      ))
    }
    seconds <- ena3d_exchange_decode_values(values, "double", context)
    return(as.POSIXct(seconds, origin = "1970-01-01", tz = timezone))
  }
  if (identical(type, "difftime")) {
    units <- ena3d_exchange_scalar_string(
      specification$units, paste0(context, ".units")
    )
    allowed_units <- c("secs", "mins", "hours", "days", "weeks")
    if (!units %in% allowed_units) {
      ena3d_exchange_fail(sprintf(
        "%s.units must be one of %s.",
        context,
        paste(allowed_units, collapse = ", ")
      ))
    }
    amount <- ena3d_exchange_decode_values(values, "double", context)
    return(as.difftime(amount, units = units))
  }
  decode_one <- switch(
    type,
    logical = function(value, index) {
      if (is.null(value)) return(NA)
      if (!is.logical(value) || length(value) != 1L || is.na(value)) {
        ena3d_exchange_fail(sprintf(
          "%s.values[%d] must be boolean or null.", context, index
        ))
      }
      value
    },
    integer = function(value, index) {
      if (is.null(value)) return(NA_integer_)
      if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
          !is.finite(value) || value != trunc(value) ||
          abs(value) > .Machine$integer.max) {
        ena3d_exchange_fail(sprintf(
          "%s.values[%d] must be a finite 32-bit integer or null.",
          context, index
        ))
      }
      as.integer(value)
    },
    double = function(value, index) {
      if (is.null(value)) return(NA_real_)
      if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
          !is.finite(value)) {
        ena3d_exchange_fail(sprintf(
          "%s.values[%d] must be a finite number or null.", context, index
        ))
      }
      as.numeric(value)
    },
    character = function(value, index) {
      if (is.null(value)) return(NA_character_)
      ena3d_exchange_scalar_string(
        value,
        sprintf("%s.values[%d]", context, index)
      )
    },
    ena3d_exchange_fail(sprintf("%s.type is not supported.", context))
  )
  vapply(seq_along(values), function(index) {
    decode_one(values[[index]], index)
  }, switch(type, logical = logical(1), integer = integer(1),
            double = numeric(1), character = character(1)))
}


ena3d_exchange_decode_table <- function(specification, table_name, limits) {
  context <- paste0("tables.", table_name)
  ena3d_exchange_assert_object(specification, "columns", context)
  columns <- specification$columns
  ena3d_exchange_assert_array(columns, paste0(context, ".columns"))

  maximum_columns <- as.integer(
    limits$max_metadata_columns + limits$max_dimensions +
      choose(limits$max_nodes, 2L) + 1L
  )
  ena3d_assert_within(
    length(columns), maximum_columns, paste0(context, " column count")
  )

  decoded <- vector("list", length(columns))
  column_names <- character(length(columns))
  for (index in seq_along(columns)) {
    column_context <- sprintf("%s.columns[%d]", context, index)
    ena3d_exchange_assert_object(
      columns[[index]], names(columns[[index]]), column_context
    )
    if (!all(c("name", "type", "values") %in% names(columns[[index]]))) {
      ena3d_exchange_fail(sprintf(
        "%s must contain name, type, and values.", column_context
      ))
    }
    column_names[[index]] <- ena3d_exchange_scalar_string(
      columns[[index]]$name,
      paste0(column_context, ".name"),
      identifier = TRUE
    )
    type <- ena3d_exchange_scalar_string(
      columns[[index]]$type,
      paste0(column_context, ".type")
    )
    expected_fields <- switch(
      type,
      factor = c("name", "type", "levels", "values"),
      ordered = c("name", "type", "levels", "values"),
      datetime = c("name", "type", "timezone", "values"),
      difftime = c("name", "type", "units", "values"),
      date = c("name", "type", "values"),
      logical = c("name", "type", "values"),
      integer = c("name", "type", "values"),
      double = c("name", "type", "values"),
      character = c("name", "type", "values"),
      ena3d_exchange_fail(sprintf(
        paste0(
          "%s.type must be one of logical, integer, double, character, ",
          "date, datetime, difftime, factor, or ordered."
        ),
        column_context
      ))
    )
    ena3d_exchange_assert_object(
      columns[[index]], expected_fields, column_context
    )
    decoded[[index]] <- ena3d_exchange_decode_values(
      columns[[index]]$values,
      type,
      column_context,
      specification = columns[[index]]
    )
  }
  if (anyDuplicated(column_names)) {
    ena3d_exchange_fail(sprintf("%s contains duplicate column names.", context))
  }
  row_counts <- unique(vapply(decoded, length, integer(1)))
  if (length(row_counts) != 1L || row_counts[[1L]] == 0L) {
    ena3d_exchange_fail(sprintf(
      "%s columns must have one identical, non-zero row count.", context
    ))
  }

  names(decoded) <- column_names
  structure(
    decoded,
    class = "data.frame",
    row.names = seq_len(row_counts[[1L]])
  )
}


ena3d_exchange_mark_columns <- function(frame, columns, marker) {
  for (name in columns) {
    class(frame[[name]]) <- unique(c(marker, class(frame[[name]])))
  }
  frame
}


ena3d_exchange_restore_object <- function(tables, dimensions, group_variables,
                                          limits) {
  metadata <- tables$meta_data
  points <- tables$points
  line_weights <- tables$line_weights
  nodes <- tables$nodes
  adjacency <- tables$adjacency_key
  metadata_names <- names(metadata)

  if (!identical(names(points), c(metadata_names, dimensions))) {
    ena3d_exchange_fail(
      "points columns must be metadata columns followed by dimensions in declared order."
    )
  }
  if (!identical(names(nodes), c("code", dimensions))) {
    ena3d_exchange_fail(
      "nodes columns must be code followed by dimensions in declared order."
    )
  }
  if (nrow(points) != nrow(metadata) || nrow(line_weights) != nrow(metadata)) {
    ena3d_exchange_fail(
      "meta_data, points, and line_weights must have identical row counts."
    )
  }
  if (!"ENA_UNIT" %in% metadata_names) {
    ena3d_exchange_fail("meta_data must contain ENA_UNIT.")
  }
  if (!all(group_variables %in% metadata_names)) {
    ena3d_exchange_fail("group_variables must name metadata columns.")
  }
  if (!is.character(nodes$code)) {
    ena3d_exchange_fail("nodes.code must have type character.")
  }
  if (nrow(adjacency) != 2L) {
    ena3d_exchange_fail("adjacency_key must have exactly two rows.")
  }
  if (!all(vapply(adjacency, is.character, logical(1)))) {
    ena3d_exchange_fail("every adjacency_key column must have type character.")
  }
  if (!all(vapply(points[dimensions], is.numeric, logical(1))) ||
      !all(vapply(nodes[dimensions], is.numeric, logical(1)))) {
    ena3d_exchange_fail("point and node dimensions must be numeric.")
  }
  if (!all(vapply(nodes[dimensions], function(values) {
    all(!is.na(values) & is.finite(values))
  }, logical(1)))) {
    ena3d_exchange_fail("node dimensions must contain only finite values.")
  }

  edge_names <- names(adjacency)
  expected_edge_names <- vapply(seq_along(adjacency), function(index) {
    paste(adjacency[[index]][[1L]], adjacency[[index]][[2L]], sep = " & ")
  }, character(1))
  if (!identical(edge_names, expected_edge_names)) {
    ena3d_exchange_fail(
      "adjacency_key column names must use '<from> & <to>' in endpoint order."
    )
  }
  if (!identical(names(line_weights), c(metadata_names, edge_names))) {
    ena3d_exchange_fail(paste(
      "line_weights columns must be metadata columns followed by adjacency",
      "edge columns in exactly the same order."
    ))
  }
  if (!all(vapply(line_weights[edge_names], is.numeric, logical(1)))) {
    ena3d_exchange_fail("line-weight edge columns must be numeric.")
  }
  if (!all(vapply(line_weights[edge_names], function(values) {
    all(!is.na(values) & is.finite(values))
  }, logical(1)))) {
    ena3d_exchange_fail(
      "line-weight edge columns must contain only finite values."
    )
  }
  for (metadata_name in metadata_names) {
    if (!identical(metadata[[metadata_name]], points[[metadata_name]]) ||
        !identical(metadata[[metadata_name]],
                   line_weights[[metadata_name]])) {
      ena3d_exchange_fail(sprintf(
        paste0(
          "metadata column %s must have identical type and row-aligned values ",
          "across meta_data, points, and line_weights."
        ),
        metadata_name
      ))
    }
  }

  ena3d_assert_within(nrow(points), limits$max_point_rows, "point row count")
  ena3d_assert_within(nrow(nodes), limits$max_nodes, "node count")
  ena3d_assert_within(
    length(dimensions), limits$max_dimensions, "ENA dimension count"
  )
  ena3d_assert_within(
    length(metadata_names), limits$max_metadata_columns,
    "metadata column count"
  )
  total_cells <- sum(vapply(tables, function(table) {
    as.numeric(nrow(table)) * as.numeric(ncol(table))
  }, numeric(1)))
  ena3d_assert_within(total_cells, limits$max_table_cells,
                      "total exchange table cell count")

  metadata <- ena3d_exchange_mark_columns(
    metadata, metadata_names, "ena.metadata"
  )
  points <- ena3d_exchange_mark_columns(points, metadata_names, "ena.metadata")
  points <- ena3d_exchange_mark_columns(points, dimensions, "ena.dimension")
  line_weights <- ena3d_exchange_mark_columns(
    line_weights, metadata_names, "ena.metadata"
  )
  line_weights <- ena3d_exchange_mark_columns(
    line_weights, edge_names, "ena.co.occurrence"
  )
  nodes <- ena3d_exchange_mark_columns(nodes, "code", "ena.metadata")
  nodes <- ena3d_exchange_mark_columns(nodes, dimensions, "ena.dimension")

  class(points) <- c("ena.points", "ena.matrix", "data.frame")
  class(line_weights) <- c("ena.line.weights", "ena.matrix", "data.frame")
  class(nodes) <- c("ena.nodes", "data.frame")
  rotation <- structure(
    list(
      adjacency.key = adjacency,
      codes = as.character(nodes$code),
      nodes = nodes
    ),
    class = c("ena.rotation.set", "list")
  )
  primary_groups <- unique(as.character(points[[group_variables[[1L]]]]))
  ena_object <- structure(
    list(
      meta.data = metadata,
      line.weights = line_weights,
      rotation = rotation,
      points = points,
      `_function.params` = list(
        groupVar = group_variables,
        units.by = group_variables,
        groups = primary_groups,
        unit.groups = primary_groups
      )
    ),
    class = c("ena.set", "list")
  )

  ena3d_validate_ena_object(
    ena_object,
    object_name = ".ena3d.json object",
    limits = limits
  )
  ena_object
}


ena3d_exchange_decode <- function(payload, limits = ena3d_data_limits()) {
  ena3d_exchange_assert_object(
    payload,
    c("format", "version", "dimensions", "group_variables", "tables"),
    "top level"
  )
  format_name <- ena3d_exchange_scalar_string(payload$format, "format")
  if (!identical(format_name, ENA3D_EXCHANGE_FORMAT)) {
    ena3d_exchange_fail(sprintf(
      "format must be '%s'.", ENA3D_EXCHANGE_FORMAT
    ))
  }
  if (!is.numeric(payload$version) || length(payload$version) != 1L ||
      is.na(payload$version) || !is.finite(payload$version) ||
      payload$version != ENA3D_EXCHANGE_VERSION) {
    ena3d_exchange_fail(sprintf(
      "version must be %d.", ENA3D_EXCHANGE_VERSION
    ))
  }
  dimensions <- ena3d_exchange_string_array(
    payload$dimensions, "dimensions", identifier = TRUE
  )
  if (length(dimensions) < 3L) {
    ena3d_exchange_fail("at least three dimensions are required.")
  }
  group_variables <- ena3d_exchange_string_array(
    payload$group_variables, "group_variables", identifier = TRUE
  )

  table_names <- c(
    "meta_data", "points", "line_weights", "nodes", "adjacency_key"
  )
  ena3d_exchange_assert_object(payload$tables, table_names, "tables")
  tables <- lapply(table_names, function(table_name) {
    ena3d_exchange_decode_table(payload$tables[[table_name]], table_name, limits)
  })
  names(tables) <- table_names
  ena3d_exchange_restore_object(
    tables, dimensions, group_variables, limits
  )
}


ena3d_exchange_preflight_json <- function(bytes, maximum_depth = 16L) {
  if (!is.raw(bytes) || !length(bytes)) {
    ena3d_exchange_fail("the file is empty.")
  }
  if (length(bytes) >= 3L &&
      identical(as.integer(bytes[1:3]), c(239L, 187L, 191L))) {
    ena3d_exchange_fail("UTF-8 byte-order marks are not permitted.")
  }

  values <- as.integer(bytes)
  depth <- 0L
  in_string <- FALSE
  escaped <- FALSE
  for (value in values) {
    if (in_string) {
      if (escaped) {
        escaped <- FALSE
      } else if (value == 92L) {
        escaped <- TRUE
      } else if (value == 34L) {
        in_string <- FALSE
      }
      next
    }
    if (value == 34L) {
      in_string <- TRUE
    } else if (value %in% c(123L, 91L)) {
      depth <- depth + 1L
      if (depth > maximum_depth) {
        ena3d_exchange_fail(sprintf(
          "JSON nesting exceeds the maximum depth of %d.", maximum_depth
        ))
      }
    } else if (value %in% c(125L, 93L)) {
      depth <- depth - 1L
      if (depth < 0L) ena3d_exchange_fail("JSON delimiters are unbalanced.")
    }
  }
  if (in_string || depth != 0L) {
    ena3d_exchange_fail("JSON strings or delimiters are incomplete.")
  }
  invisible(bytes)
}


ena3d_read_exchange_file <- function(file_path, limits = ena3d_data_limits()) {
  if (!is.character(file_path) || length(file_path) != 1L ||
      is.na(file_path) || !file.exists(file_path) ||
      isTRUE(file.info(file_path)$isdir)) {
    stop("The .ena3d.json exchange file does not exist.", call. = FALSE)
  }
  connection <- file(file_path, open = "rb")
  on.exit(close(connection), add = TRUE)
  byte_limit <- as.integer(limits$max_exchange_file_bytes)
  bytes <- readBin(connection, what = "raw", n = byte_limit + 1L)
  ena3d_assert_within(
    length(bytes), byte_limit, ".ena3d.json file size"
  )
  ena3d_exchange_preflight_json(bytes)

  text <- tryCatch(
    rawToChar(bytes),
    error = function(error) ena3d_exchange_fail("the file is not valid UTF-8 text.")
  )
  text <- iconv(text, from = "UTF-8", to = "UTF-8", sub = NA_character_)
  if (length(text) != 1L || is.na(text)) {
    ena3d_exchange_fail("the file is not valid UTF-8 text.")
  }
  Encoding(text) <- "UTF-8"
  payload <- tryCatch(
    jsonlite::parse_json(text, simplifyVector = FALSE),
    error = function(error) ena3d_exchange_fail("the JSON syntax is invalid.")
  )
  ena3d_assert_within(
    as.numeric(object.size(payload)), limits$max_loaded_bytes,
    "parsed exchange object size"
  )
  ena3d_exchange_decode(payload, limits = limits)
}


ena3d_exchange_encode_column <- function(column, name, context) {
  ena3d_exchange_scalar_string(name, paste0(context, ".name"), identifier = TRUE)
  extras <- list()
  if (is.factor(column)) {
    type <- if (is.ordered(column)) "ordered" else "factor"
    levels <- enc2utf8(levels(column))
    if (!length(levels) || any(!validUTF8(levels))) {
      stop(sprintf("%s has invalid or empty factor levels.", context),
           call. = FALSE)
    }
    extras$levels <- unname(as.list(levels))
    plain <- as.character(column)
  } else if (inherits(column, "Date")) {
    type <- "date"
    plain <- as.character(column)
  } else if (inherits(column, "POSIXt")) {
    type <- "datetime"
    timezone <- attr(column, "tzone", exact = TRUE)
    if (is.null(timezone) || !length(timezone) || is.na(timezone[[1L]]) ||
        !nzchar(timezone[[1L]])) {
      timezone <- "UTC"
    } else {
      timezone <- as.character(timezone[[1L]])
    }
    if (!timezone %in% unique(c("UTC", OlsonNames()))) {
      stop(sprintf("%s has an unsupported timezone.", context), call. = FALSE)
    }
    extras$timezone <- timezone
    plain <- as.numeric(column)
  } else if (inherits(column, "difftime")) {
    type <- "difftime"
    units <- attr(column, "units", exact = TRUE)
    allowed_units <- c("secs", "mins", "hours", "days", "weeks")
    if (is.null(units) || length(units) != 1L || !units %in% allowed_units) {
      stop(sprintf("%s has unsupported difftime units.", context),
           call. = FALSE)
    }
    extras$units <- as.character(units)
    plain <- as.numeric(column, units = units)
  } else if (is.logical(column)) {
    type <- "logical"
    plain <- column
  } else if (is.integer(column)) {
    type <- "integer"
    plain <- column
  } else if (is.double(column)) {
    type <- "double"
    plain <- column
  } else if (is.character(column)) {
    type <- "character"
    plain <- column
  } else {
    stop(sprintf("%s has unsupported type %s.", context, typeof(column)),
         call. = FALSE)
  }

  if (is.numeric(plain) && any(!is.na(plain) & !is.finite(plain))) {
    stop(sprintf("%s contains a non-finite number.", context), call. = FALSE)
  }
  if (is.character(plain) && any(!is.na(plain) & !validUTF8(plain))) {
    stop(sprintf("%s contains invalid UTF-8 text.", context), call. = FALSE)
  }
  values <- lapply(seq_along(plain), function(row) {
    if (is.na(plain[[row]])) return(NULL)
    if (is.logical(plain)) return(as.logical(plain[[row]]))
    if (is.integer(plain)) return(as.integer(plain[[row]]))
    if (is.double(plain)) return(as.numeric(plain[[row]]))
    enc2utf8(as.character(plain[[row]]))
  })
  c(
    list(name = name, type = type),
    extras,
    list(values = unname(values))
  )
}


ena3d_exchange_encode_table <- function(table, table_name) {
  table <- as.data.frame(table, stringsAsFactors = FALSE, optional = TRUE)
  columns <- lapply(seq_along(table), function(index) {
    name <- names(table)[[index]]
    ena3d_exchange_encode_column(
      table[[index]], name, paste0(table_name, ".", name)
    )
  })
  list(columns = unname(columns))
}


ena3d_exchange_payload <- function(ena_object,
                                   limits = ena3d_data_limits()) {
  ena3d_validate_ena_object(
    ena_object, object_name = "Trusted ENA object", limits = limits
  )
  dimensions <- ena3d_dimension_names(ena_object)
  group_variables <- get_ena_group_var(ena_object)
  group_variables <- unique(as.character(group_variables))
  group_variables <- group_variables[
    !is.na(group_variables) & nzchar(group_variables)
  ]
  metadata_names <- names(as.data.frame(ena_object$meta.data))
  metadata_frame <- as.data.frame(
    ena_object$meta.data, stringsAsFactors = FALSE, optional = TRUE
  )
  points_frame <- as.data.frame(
    ena_object$points, stringsAsFactors = FALSE, optional = TRUE
  )
  line_weights_frame <- as.data.frame(
    ena_object$line.weights, stringsAsFactors = FALSE, optional = TRUE
  )
  nodes_frame <- as.data.frame(
    ena_object$rotation$nodes, stringsAsFactors = FALSE, optional = TRUE
  )
  adjacency_frame <- as.data.frame(
    ena_object$rotation$adjacency.key,
    stringsAsFactors = FALSE,
    optional = TRUE
  )
  adjacency_names <- vapply(seq_along(adjacency_frame), function(index) {
    paste(
      adjacency_frame[[index]][[1L]],
      adjacency_frame[[index]][[2L]],
      sep = " & "
    )
  }, character(1))
  names(adjacency_frame) <- adjacency_names

  tables <- list(
    meta_data = metadata_frame,
    points = points_frame[c(metadata_names, dimensions)],
    line_weights = line_weights_frame[c(metadata_names, adjacency_names)],
    nodes = nodes_frame[c("code", dimensions)],
    adjacency_key = adjacency_frame
  )
  list(
    format = ENA3D_EXCHANGE_FORMAT,
    version = ENA3D_EXCHANGE_VERSION,
    dimensions = unname(as.list(dimensions)),
    group_variables = unname(as.list(group_variables)),
    tables = lapply(names(tables), function(table_name) {
      ena3d_exchange_encode_table(tables[[table_name]], table_name)
    }) |> stats::setNames(names(tables))
  )
}


ena3d_write_exchange_file <- function(ena_object, file_path,
                                      limits = ena3d_data_limits()) {
  if (!is.character(file_path) || length(file_path) != 1L ||
      is.na(file_path) || !grepl("\\.ena3d\\.json$", file_path,
                                 ignore.case = TRUE)) {
    stop("The output path must end in .ena3d.json.", call. = FALSE)
  }
  parent <- normalizePath(dirname(file_path), mustWork = TRUE)
  output <- file.path(parent, basename(file_path))
  if (file.exists(output)) {
    stop("Refusing to overwrite an existing exchange file.", call. = FALSE)
  }

  payload <- ena3d_exchange_payload(ena_object, limits = limits)
  json <- jsonlite::toJSON(
    payload,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA,
    pretty = FALSE
  )
  temporary <- tempfile(
    pattern = ".ena3d-exchange-", tmpdir = parent, fileext = ".tmp"
  )
  on.exit(unlink(temporary), add = TRUE)
  connection <- file(temporary, open = "wb")
  tryCatch(
    writeBin(charToRaw(enc2utf8(as.character(json))), connection),
    finally = close(connection)
  )
  ena3d_assert_within(
    file.info(temporary)$size,
    limits$max_exchange_file_bytes,
    "canonical .ena3d.json file size"
  )
  if (!file.rename(temporary, output)) {
    stop("Could not atomically publish the exchange file.", call. = FALSE)
  }
  list(
    path = output,
    bytes = as.numeric(file.info(output)$size),
    sha256 = digest::digest(file = output, algo = "sha256")
  )
}
