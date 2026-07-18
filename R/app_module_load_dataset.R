if (!exists("ena3d_data_limits", mode = "function")) {
  .ena3d_security_candidates <- c(
    "security_utils.R",
    file.path("R", "security_utils.R"),
    file.path("..", "R", "security_utils.R"),
    file.path("..", "..", "R", "security_utils.R")
  )
  .ena3d_security_file <- .ena3d_security_candidates[file.exists(
    .ena3d_security_candidates
  )][1L]
  if (is.na(.ena3d_security_file)) {
    stop("Could not locate security_utils.R.", call. = FALSE)
  }
  source(.ena3d_security_file)
  rm(.ena3d_security_candidates, .ena3d_security_file)
}

if (!exists("ena3d_read_exchange_file", mode = "function")) {
  .ena3d_exchange_candidates <- c(
    "ena3d_exchange.R",
    file.path("R", "ena3d_exchange.R"),
    file.path("..", "R", "ena3d_exchange.R"),
    file.path("..", "..", "R", "ena3d_exchange.R")
  )
  .ena3d_exchange_file <- .ena3d_exchange_candidates[file.exists(
    .ena3d_exchange_candidates
  )][1L]
  if (is.na(.ena3d_exchange_file)) {
    stop("Could not locate ena3d_exchange.R.", call. = FALSE)
  }
  source(.ena3d_exchange_file)
  rm(.ena3d_exchange_candidates, .ena3d_exchange_file)
}


ena3d_reset_data_state <- function(rv_data, state) {
  # Every value below is derived from the active dataset. Keep this list in
  # one place so uploads and bundled samples cannot follow different cache
  # invalidation paths.
  rv_data$myList <- list()
  rv_data$unit_group_change_plots <- list()
  rv_data$current_unit_change_plot_camera <- list()
  rv_data$dataset_id <- NULL
  rv_data$ena_groups <- character()
  rv_data$ena_groupVar <- character()
  rv_data$ena_points_plot_ready <- FALSE
  rv_data$initialized <- FALSE
  rv_data$model_tab_clicked <- FALSE
  rv_data$comparison_plot <- list()
  rv_data$reactiveFunctions <- list()
  rv_data$group_colors <- matrix(character(), ncol = 2L)
  rv_data$group_selectors <- list()
  rv_data$group_options <- list()
  rv_data$active_dataset <- NULL
  state$ena_obj <- NULL
  state$is_app_initialized <- FALSE
}

ena3d_validate_ena_object <- function(ena_obj, object_name = "ENA object",
                                      limits = ena3d_data_limits()) {
  fail <- function(message) {
    stop(sprintf("%s is not compatible with ENA 3D: %s", object_name, message),
         call. = FALSE)
  }

  if (!inherits(ena_obj, "ena.set")) {
    fail("it does not inherit from class `ena.set`.")
  }
  if (!is.list(ena_obj) || is.environment(ena_obj)) {
    fail("it must be a plain list-based ena.set, not an environment or active object.")
  }

  ena3d_assert_within(
    as.numeric(object.size(ena_obj)), limits$max_loaded_bytes, "ENA object size"
  )

  required_paths <- list(
    points = ena_obj$points,
    `meta.data` = ena_obj$meta.data,
    `rotation$nodes` = ena_obj$rotation$nodes,
    `rotation$adjacency.key` = ena_obj$rotation$adjacency.key,
    `line.weights` = ena_obj$line.weights
  )
  missing_paths <- names(required_paths)[vapply(required_paths, is.null, logical(1))]
  if (length(missing_paths)) {
    fail(sprintf("required fields are missing: %s.",
                 paste(missing_paths, collapse = ", ")))
  }

  table_paths <- c("points", "meta.data", "rotation$nodes", "line.weights")
  bad_tables <- table_paths[!vapply(required_paths[table_paths], is.data.frame, logical(1))]
  if (length(bad_tables)) {
    fail(sprintf("these fields must be data frames: %s.",
                 paste(bad_tables, collapse = ", ")))
  }
  empty_tables <- table_paths[vapply(required_paths[table_paths], nrow, integer(1)) == 0L]
  if (length(empty_tables)) {
    fail(sprintf("these tables must not be empty: %s.",
                 paste(empty_tables, collapse = ", ")))
  }

  points <- ena_obj$points
  metadata <- ena_obj$meta.data
  nodes <- ena_obj$rotation$nodes
  adjacency <- ena_obj$rotation$adjacency.key
  line_weights <- ena_obj$line.weights

  ena3d_assert_within(nrow(points), limits$max_point_rows, "point row count")
  ena3d_assert_within(nrow(nodes), limits$max_nodes, "node count")
  ena3d_assert_within(
    ncol(metadata), limits$max_metadata_columns, "metadata column count"
  )
  table_cells <- nrow(points) * ncol(points) +
    nrow(metadata) * ncol(metadata) +
    nrow(nodes) * ncol(nodes) +
    nrow(line_weights) * ncol(line_weights)
  ena3d_assert_within(table_cells, limits$max_table_cells, "total table cell count")

  if (nrow(metadata) != nrow(points)) {
    fail("`meta.data` and `points` must have the same number of rows.")
  }
  if (nrow(line_weights) != nrow(points)) {
    fail("`line.weights` and `points` must have the same number of rows.")
  }
  if (anyDuplicated(names(points)) || anyDuplicated(names(metadata)) ||
      anyDuplicated(names(nodes)) || anyDuplicated(names(line_weights))) {
    fail("table column names must be unique.")
  }

  missing_metadata <- setdiff(names(metadata), names(points))
  if (length(missing_metadata)) {
    fail(sprintf("metadata columns are absent from `points`: %s.",
                 paste(missing_metadata, collapse = ", ")))
  }
  missing_line_metadata <- setdiff(names(metadata), names(line_weights))
  if (length(missing_line_metadata)) {
    fail(sprintf("metadata columns are absent from `line.weights`: %s.",
                 paste(missing_line_metadata, collapse = ", ")))
  }
  for (metadata_name in names(metadata)) {
    if (!identical(metadata[[metadata_name]], points[[metadata_name]]) ||
        !identical(metadata[[metadata_name]], line_weights[[metadata_name]])) {
      fail(sprintf(
        paste0(
          "metadata column `%s` must have identical type and row-aligned ",
          "values across `meta.data`, `points`, and `line.weights`."
        ),
        metadata_name
      ))
    }
  }
  if (!"ENA_UNIT" %in% names(points) || !"ENA_UNIT" %in% names(line_weights)) {
    fail("both `points` and `line.weights` must contain `ENA_UNIT`.")
  }
  if (!identical(as.character(points[["ENA_UNIT"]]),
                 as.character(line_weights[["ENA_UNIT"]]))) {
    fail("`points` and `line.weights` must be aligned in the same `ENA_UNIT` row order.")
  }
  declared_group_vars <- ena_obj$`_function.params`$groupVar
  if (is.null(declared_group_vars)) {
    declared_group_vars <- ena_obj$`_function.params`$units.by
  }
  declared_group_vars <- unique(as.character(declared_group_vars))
  declared_group_vars <- declared_group_vars[
    !is.na(declared_group_vars) & nzchar(declared_group_vars)
  ]
  missing_point_groups <- setdiff(declared_group_vars, names(points))
  if (length(missing_point_groups)) {
    fail(sprintf(
      "declared grouping columns are absent from `points`: %s.",
      paste(missing_point_groups, collapse = ", ")
    ))
  }
  missing_line_weight_groups <- setdiff(declared_group_vars, names(line_weights))
  if (length(missing_line_weight_groups)) {
    fail(sprintf(
      "declared grouping columns are absent from `line.weights`: %s.",
      paste(missing_line_weight_groups, collapse = ", ")
    ))
  }
  for (group_var in declared_group_vars) {
    point_groups <- as.character(points[[group_var]])
    weight_groups <- as.character(line_weights[[group_var]])
    if (anyNA(point_groups) || any(!nzchar(trimws(point_groups)))) {
      fail(sprintf(
        "declared grouping column `%s` in `points` contains missing or blank values.",
        group_var
      ))
    }
    if (anyNA(weight_groups) || any(!nzchar(trimws(weight_groups)))) {
      fail(sprintf(
        "declared grouping column `%s` in `line.weights` contains missing or blank values.",
        group_var
      ))
    }
    if (!identical(point_groups, weight_groups)) {
      fail(sprintf(
        paste0(
          "declared grouping column `%s` must be aligned between `points` ",
          "and `line.weights` in the same row order."
        ),
        group_var
      ))
    }
  }

  dimensions <- setdiff(names(points), names(metadata))
  if (length(dimensions) < 3L) {
    fail("at least three point-coordinate dimensions are required.")
  }
  ena3d_assert_within(
    length(dimensions), limits$max_dimensions, "ENA dimension count"
  )
  nonnumeric_dimensions <- dimensions[!vapply(
    as.data.frame(points)[dimensions], is.numeric, logical(1)
  )]
  if (length(nonnumeric_dimensions)) {
    fail(sprintf("point dimensions must be numeric: %s.",
                 paste(nonnumeric_dimensions, collapse = ", ")))
  }
  nonfinite_dimensions <- dimensions[vapply(
    as.data.frame(points)[dimensions],
    function(values) any(!is.na(values) & !is.finite(values)),
    logical(1)
  )]
  if (length(nonfinite_dimensions)) {
    fail(sprintf("point dimensions contain non-finite values: %s.",
                 paste(nonfinite_dimensions, collapse = ", ")))
  }

  if (!"code" %in% names(nodes)) {
    fail("`rotation$nodes` must contain a `code` column.")
  }
  node_codes <- as.character(nodes[["code"]])
  if (anyNA(node_codes) || any(!nzchar(node_codes)) || anyDuplicated(node_codes)) {
    fail("node codes must be non-missing and unique.")
  }
  missing_node_dimensions <- setdiff(dimensions, names(nodes))
  if (length(missing_node_dimensions)) {
    fail(sprintf("node coordinates are missing point dimensions: %s.",
                 paste(missing_node_dimensions, collapse = ", ")))
  }
  nonnumeric_node_dimensions <- dimensions[!vapply(
    as.data.frame(nodes)[dimensions], is.numeric, logical(1)
  )]
  if (length(nonnumeric_node_dimensions)) {
    fail(sprintf("node dimensions must be numeric: %s.",
                 paste(nonnumeric_node_dimensions, collapse = ", ")))
  }
  invalid_node_dimensions <- dimensions[vapply(
    as.data.frame(nodes)[dimensions],
    function(values) any(is.na(values) | !is.finite(values)),
    logical(1)
  )]
  if (length(invalid_node_dimensions)) {
    fail(sprintf("node dimensions must be complete and finite: %s.",
                 paste(invalid_node_dimensions, collapse = ", ")))
  }

  if (!(is.data.frame(adjacency) || is.matrix(adjacency))) {
    fail("`rotation$adjacency.key` must be a two-row matrix or data frame.")
  }
  if (nrow(adjacency) != 2L || ncol(adjacency) == 0L) {
    fail("`rotation$adjacency.key` must contain two endpoint rows and at least one edge.")
  }
  expected_edges <- choose(length(node_codes), 2L)
  if (ncol(adjacency) != expected_edges) {
    fail(sprintf(
      "the adjacency key has %d edges; %d nodes require %d pairwise edges.",
      ncol(adjacency), length(node_codes), expected_edges
    ))
  }
  adjacency_codes <- as.character(unlist(as.data.frame(adjacency), use.names = FALSE))
  unknown_codes <- setdiff(unique(adjacency_codes), node_codes)
  if (anyNA(adjacency_codes) || any(!nzchar(adjacency_codes)) || length(unknown_codes)) {
    detail <- if (length(unknown_codes)) {
      paste0(" Unknown codes: ", paste(unknown_codes, collapse = ", "), ".")
    } else {
      ""
    }
    fail(paste0("the adjacency key contains missing or unknown node codes.", detail))
  }
  from_codes <- as.character(unlist(as.data.frame(adjacency[1L, , drop = FALSE])))
  to_codes <- as.character(unlist(as.data.frame(adjacency[2L, , drop = FALSE])))
  if (any(from_codes == to_codes)) {
    fail("the adjacency key must not contain self-pairs.")
  }
  pair_keys <- vapply(seq_along(from_codes), function(index) {
    paste(sort(c(from_codes[[index]], to_codes[[index]])), collapse = "\r")
  }, character(1))
  if (anyDuplicated(pair_keys)) {
    fail("the adjacency key contains duplicate node pairs and is not complete.")
  }
  expected_pair_keys <- utils::combn(sort(node_codes), 2L, function(pair) {
    paste(pair, collapse = "\r")
  })
  if (!setequal(pair_keys, expected_pair_keys)) {
    fail("the adjacency key does not contain every unique pair of node codes exactly once.")
  }

  edge_columns <- setdiff(names(line_weights), names(metadata))
  if (length(edge_columns) != expected_edges) {
    fail(sprintf(
      "`line.weights` has %d edge columns; the adjacency key requires %d.",
      length(edge_columns), expected_edges
    ))
  }
  expected_edge_columns <- paste(from_codes, to_codes, sep = " & ")
  if (!identical(edge_columns, expected_edge_columns)) {
    fail(paste0(
      "line-weight edge columns must follow the adjacency-key order and use ",
      "the '<from> & <to>' names."
    ))
  }
  nonnumeric_edges <- edge_columns[!vapply(
    as.data.frame(line_weights)[edge_columns], is.numeric, logical(1)
  )]
  if (length(nonnumeric_edges)) {
    fail(sprintf("line-weight edge columns must be numeric: %s.",
                 paste(nonnumeric_edges, collapse = ", ")))
  }
  invalid_edges <- edge_columns[vapply(
    as.data.frame(line_weights)[edge_columns],
    function(values) any(is.na(values) | !is.finite(values)),
    logical(1)
  )]
  if (length(invalid_edges)) {
    fail(sprintf("line-weight edges must be complete and finite: %s.",
                 paste(invalid_edges, collapse = ", ")))
  }

  for (group_var in declared_group_vars) {
    ena3d_assert_within(
      length(unique(as.character(points[[group_var]]))),
      limits$max_group_levels,
      sprintf(
        "level count for grouping column `%s`; choose a lower-cardinality field",
        group_var
      )
    )
  }
  ena3d_assert_within(
    length(unique(as.character(points[["ENA_UNIT"]]))),
    limits$max_units,
    "unique ENA unit count"
  )

  invisible(ena_obj)
}

ena3d_read_ena_object <- function(file_path,
                                  source_kind = c(
                                    "untrusted", "bundled", "exchange",
                                    "trusted_native"
                                  ),
                                  limits = ena3d_data_limits()) {
  source_kind <- match.arg(source_kind)
  if (identical(source_kind, "exchange")) {
    return(ena3d_read_exchange_file(file_path, limits = limits))
  }
  if (!source_kind %in% c("bundled", "trusted_native")) {
    stop(ena3d_public_upload_message(), call. = FALSE)
  }
  if (!file.exists(file_path)) {
    stop(sprintf("ENA data file does not exist: %s", file_path), call. = FALSE)
  }

  file_size <- file.info(file_path)$size
  ena3d_assert_within(file_size, limits$max_file_bytes, "trusted .RData file size")

  data_env <- new.env(parent = emptyenv())
  object_names <- tryCatch(
    load(file = file_path, envir = data_env),
    error = function(error) {
      stop(
        sprintf("Could not read %s as an .RData file: %s",
                basename(file_path), conditionMessage(error)),
        call. = FALSE
      )
    }
  )
  ena3d_assert_within(
    length(object_names), limits$max_saved_objects, "saved object count"
  )
  loaded_bytes <- sum(vapply(object_names, function(name) {
    as.numeric(object.size(data_env[[name]]))
  }, numeric(1)))
  ena3d_assert_within(loaded_bytes, limits$max_loaded_bytes, "loaded object size")
  ena_names <- object_names[vapply(
    object_names,
    function(name) inherits(data_env[[name]], "ena.set"),
    logical(1)
  )]

  if (length(ena_names) != 1L) {
    stop(
      sprintf(
        "Expected exactly one ena.set object in %s; found %d (%s).",
        basename(file_path),
        length(ena_names),
        if (length(ena_names)) paste(ena_names, collapse = ", ") else "none"
      ),
      call. = FALSE
    )
  }

  ena_obj <- data_env[[ena_names[[1L]]]]
  ena3d_validate_ena_object(
    ena_obj,
    sprintf("Object `%s`", ena_names[[1L]]),
    limits = limits
  )

  ena_obj
}

ena3d_dimension_names <- function(ena_obj) {
  point_names <- names(ena_obj$points)
  metadata_names <- names(ena_obj$meta.data)
  dimensions <- setdiff(point_names, metadata_names)
  if (length(dimensions) < 3L) {
    dimensions <- point_names[vapply(
      ena_obj$points,
      function(column) inherits(column, "ena.dimension") || is.numeric(column),
      logical(1)
    )]
    dimensions <- setdiff(dimensions, metadata_names)
  }
  unique(dimensions)
}

ena3d_order_values <- function(values) {
  if (is.factor(values)) {
    return(levels(droplevels(values)))
  }
  if (inherits(values, "POSIXt")) {
    labels <- ena3d_group_value_labels(values)
    return(unique(labels[order(values, na.last = NA)]))
  }
  if (inherits(values, "Date") || is.numeric(values)) {
    return(sort(unique(values), na.last = NA))
  }
  unique(as.character(values[!is.na(values)]))
}

ena3d_pair_id_choices <- function(ena_obj, group_vars) {
  metadata_names <- intersect(names(ena_obj$points), names(ena_obj$meta.data))
  candidates <- setdiff(metadata_names, group_vars[[1L]])
  preferred <- unique(c(
    group_vars[-1L],
    grep("(^id$|name|user|participant|student)", candidates, ignore.case = TRUE, value = TRUE),
    "ENA_UNIT",
    candidates
  ))
  preferred[preferred %in% candidates]
}


.ena3d_network_selector_prefix <- "ena3d-network-v1:"

ena3d_network_selector_encode <- function(type = c("none", "group", "unit"),
                                          value = NULL) {
  type <- match.arg(type)
  if (identical(type, "none")) {
    if (!is.null(value) && length(value)) {
      stop("The no-network selector must not contain a value.", call. = FALSE)
    }
    return(paste0(.ena3d_network_selector_prefix, "none"))
  }

  if (length(value) != 1L || is.na(value)) {
    stop("A Network group or unit selector needs one non-missing value.",
         call. = FALSE)
  }
  value <- enc2utf8(as.character(value))
  raw_value <- as.integer(charToRaw(value))
  payload <- if (length(raw_value)) {
    paste(sprintf("%02x", raw_value), collapse = "")
  } else {
    ""
  }
  paste0(.ena3d_network_selector_prefix, type, ":", payload)
}

ena3d_network_selector_decode <- function(selection) {
  if (!is.character(selection) || length(selection) != 1L ||
      is.na(selection) || !nzchar(selection)) {
    return(NULL)
  }

  none_value <- ena3d_network_selector_encode("none")
  if (identical(selection, none_value)) {
    return(list(type = "none", value = NULL))
  }

  prefix_pattern <- paste0("^", .ena3d_network_selector_prefix)
  matches <- regexec(
    paste0(prefix_pattern, "(group|unit):([[:xdigit:]]*)$"),
    selection,
    perl = TRUE
  )
  parts <- regmatches(selection, matches)[[1L]]
  if (length(parts) != 3L) {
    return(NULL)
  }

  payload <- parts[[3L]]
  if (nchar(payload, type = "bytes") %% 2L != 0L) {
    return(NULL)
  }
  byte_pairs <- if (nzchar(payload)) {
    substring(
      payload,
      seq.int(1L, nchar(payload), by = 2L),
      seq.int(2L, nchar(payload), by = 2L)
    )
  } else {
    character()
  }
  decoded <- tryCatch(
    rawToChar(as.raw(strtoi(byte_pairs, base = 16L))),
    error = function(...) NULL
  )
  if (is.null(decoded)) {
    return(NULL)
  }
  list(type = parts[[2L]], value = enc2utf8(decoded))
}

ena3d_network_choices <- function(groups, units) {
  groups <- as.character(groups)
  units <- as.character(units)
  groups <- groups[!is.na(groups)]
  units <- units[!is.na(units)]

  group_choices <- stats::setNames(
    vapply(groups, function(value) {
      ena3d_network_selector_encode("group", value)
    }, character(1)),
    groups
  )
  unit_choices <- stats::setNames(
    vapply(units, function(value) {
      ena3d_network_selector_encode("unit", value)
    }, character(1)),
    units
  )

  list(
    "No Network" = ena3d_network_selector_encode("none"),
    Groups = group_choices,
    Units = unit_choices
  )
}

ena3d_group_selector_metadata <- function(groups) {
  groups <- as.character(groups)
  selectors <- lapply(seq_along(groups), function(i) {
    group_name <- groups[[i]]
    safe_stem <- paste0("group-", i, "-", make.names(group_name))
    c(
      button_id = paste0(safe_stem, "-btn"),
      points_toggle_id = paste0(safe_stem, "-points-btn"),
      color_selector_id = paste0(safe_stem, "-color-selector"),
      show_mean_btn_id = paste0(safe_stem, "-show-mean-btn"),
      show_conf_int_btn_id = paste0(safe_stem, "-show-conf-int-btn"),
      group_name = group_name
    )
  })
  stats::setNames(selectors, groups)
}


ena3d_active_dataset_summary <- function(prepared, display_name,
                                         app_version, build_id) {
  safe_name <- basename(as.character(display_name)[[1L]])
  list(
    name = safe_name,
    rows = nrow(prepared$ena_obj$points),
    nodes = nrow(prepared$ena_obj$rotation$nodes),
    group_variables = length(prepared$group_vars),
    group_levels = length(prepared$groups),
    dimensions = length(prepared$dimensions),
    sha256 = prepared$content_sha256,
    app_version = as.character(app_version)[[1L]],
    build_id = as.character(build_id)[[1L]]
  )
}


ena3d_active_dataset_card <- function(summary) {
  req(summary)
  tags$div(
    class = "card active-dataset-card",
    role = "status",
    `aria-live` = "polite",
    tags$div(
      class = "card-body",
      tags$h4(class = "card-title h6", "Active dataset"),
      tags$p(class = "card-text", tags$strong(summary$name)),
      tags$dl(
        class = "row active-dataset-details",
        tags$dt(class = "col-5", "Rows"),
        tags$dd(class = "col-7", format(summary$rows, big.mark = ",")),
        tags$dt(class = "col-5", "Nodes"),
        tags$dd(class = "col-7", format(summary$nodes, big.mark = ",")),
        tags$dt(class = "col-5", "Groups"),
        tags$dd(
          class = "col-7",
          sprintf(
            "%d levels / %d variables",
            summary$group_levels,
            summary$group_variables
          )
        ),
        tags$dt(class = "col-5", "Dimensions"),
        tags$dd(class = "col-7", summary$dimensions),
        tags$dt(class = "col-5", "Version"),
        tags$dd(class = "col-7", summary$app_version),
        tags$dt(class = "col-5", "Build"),
        tags$dd(class = "col-7", summary$build_id),
        tags$dt(class = "col-12", "Content SHA-256"),
        tags$dd(
          class = "col-12",
          tags$code(class = "dataset-hash", summary$sha256)
        )
      )
    )
  )
}

ena3d_prepare_dataset <- function(file_path,
                                  source_kind = c(
                                    "untrusted", "bundled", "exchange",
                                    "trusted_native"
                                  ),
                                  limits = ena3d_data_limits()) {
  source_kind <- match.arg(source_kind)
  ena_obj <- ena3d_read_ena_object(
    file_path,
    source_kind = source_kind,
    limits = limits
  )
  dimensions <- ena3d_dimension_names(ena_obj)
  if (length(dimensions) < 3L) {
    stop("The ENA object must provide at least three coordinate dimensions.", call. = FALSE)
  }

  group_vars <- get_ena_group_var(ena_obj)
  group_vars <- as.character(group_vars[!is.na(group_vars) & nzchar(group_vars)])
  if (!length(group_vars)) {
    stop("The ENA object does not declare a grouping or units.by variable.", call. = FALSE)
  }
  if (!all(group_vars %in% names(ena_obj$points))) {
    stop("One or more declared ENA grouping variables are absent from points.", call. = FALSE)
  }

  group_values <- ena_obj$points[[group_vars[[1L]]]]
  group_labels <- ena3d_group_value_labels(group_values)
  groups <- unique(group_labels[!is.na(group_values)])
  if (!length(groups)) {
    stop("The primary ENA grouping variable contains no usable values.", call. = FALSE)
  }

  file_info <- file.info(file_path)
  content_sha256 <- digest::digest(file = file_path, algo = "sha256")
  list(
    ena_obj = ena_obj,
    dimensions = dimensions,
    group_vars = group_vars,
    groups = groups,
    unit_choices = ena3d_order_values(group_values),
    pair_ids = ena3d_pair_id_choices(ena_obj, group_vars),
    group_colors = cbind(color = ena3d_palette(length(groups)), group = groups),
    network_units = as.character(unique(ena_obj$line.weights[["ENA_UNIT"]])),
    content_sha256 = content_sha256,
    dataset_id = paste(
      normalizePath(file_path, mustWork = TRUE),
      file_info$size,
      as.numeric(file_info$mtime),
      content_sha256,
      sep = "::"
    )
  )
}

load_ena_data <- function(input, output, session, file_path, rv_data, state,
                          source_kind = c(
                            "untrusted", "bundled", "exchange",
                            "trusted_native"
                          ),
                          limits = ena3d_data_limits(),
                          display_name = basename(file_path),
                          app_version = Sys.getenv(
                            "ENA3D_APP_VERSION", unset = "development"
                          ),
                          build_id = Sys.getenv(
                            "ENA3D_BUILD_ID", unset = "development"
                          )) {
  source_kind <- match.arg(source_kind)
  # Finish all file/schema/derived-value work before touching live state.
  prepared <- ena3d_prepare_dataset(
    file_path,
    source_kind = source_kind,
    limits = limits
  )
  ena_obj <- prepared$ena_obj
  dimensions <- prepared$dimensions
  group_vars <- prepared$group_vars

  # A dataset can use completely different dimensions and grouping values.
  # Freeze every input that is about to be replaced so dependent renderers do
  # not combine the new ENA object with stale browser values during the same
  # reactive flush. Shiny unfreezes each value when its update arrives.
  if (!is.null(input)) {
    for (input_id in c(
      "x", "y", "z", "group_change_var", "unit_change",
      "change_group_1", "change_group_2",
      "stats_group1", "stats_group2", "stats_pair_id",
      "compare_group_1", "compare_group_2", "network_selector"
    )) {
      freezeReactiveValue(input, input_id)
    }
  }

  ena3d_reset_data_state(rv_data, state)
  state$ena_obj <- ena_obj
  rv_data$ena_groupVar <- group_vars
  rv_data$ena_groups <- prepared$groups
  # Build selector metadata once during the ordinary server transaction. The
  # renderUI expression below must remain read-only; mutating reactiveValues
  # from inside it invalidates the same output and creates a render loop.
  rv_data$group_selectors <- ena3d_group_selector_metadata(prepared$groups)
  rv_data$dataset_id <- prepared$dataset_id
  rv_data$active_dataset <- ena3d_active_dataset_summary(
    prepared,
    display_name = display_name,
    app_version = app_version,
    build_id = build_id
  )

  output$active_dataset_card <- renderUI({
    ena3d_active_dataset_card(rv_data$active_dataset)
  })

  updateSelectInput(session, "x", choices = dimensions, selected = dimensions[[1L]])
  updateSelectInput(session, "y", choices = dimensions, selected = dimensions[[2L]])
  updateSelectInput(session, "z", choices = dimensions, selected = dimensions[[3L]])
  updateSelectInput(session, "group_change_var", choices = group_vars, selected = group_vars[[1L]])

  unit_choices <- prepared$unit_choices
  updateSelectInput(
    session,
    "unit_change",
    choices = as.character(unit_choices),
    selected = if (length(unit_choices)) as.character(unit_choices[[1L]]) else character()
  )

  for (id in c("change_group_1", "change_group_2", "stats_group1", "stats_group2",
               "compare_group_1", "compare_group_2")) {
    updateSelectInput(
      session,
      id,
      choices = rv_data$ena_groups,
      selected = rv_data$ena_groups[[1L]]
    )
  }
  if (length(rv_data$ena_groups) > 1L) {
    updateSelectInput(session, "stats_group2", selected = rv_data$ena_groups[[2L]])
    updateSelectInput(session, "compare_group_2", selected = rv_data$ena_groups[[2L]])
  }

  pair_ids <- prepared$pair_ids
  updateSelectInput(
    session,
    "stats_pair_id",
    choices = pair_ids,
    selected = if (length(pair_ids)) pair_ids[[1L]] else character()
  )

  group_colors <- prepared$group_colors
  rv_data$group_colors <- group_colors

  output$group_colors_container <- renderUI({
    checkboxGroupInput(
      session$ns("select_group"),
      "Choose Group:",
      choiceNames = rv_data$ena_groups,
      choiceValues = rv_data$ena_groups,
      selected = rv_data$ena_groups
    )
  })

  output$network_groups_container <- renderUI({
    selector_metadata <- rv_data$group_selectors
    selectors <- lapply(selector_metadata, function(info) {
      group_name <- info[["group_name"]]
      group_selector_ui(
        button_id = session$ns(info[["button_id"]]),
        points_toggle_id = session$ns(info[["points_toggle_id"]]),
        color_selector_id = session$ns(info[["color_selector_id"]]),
        show_mean_btn_id = session$ns(info[["show_mean_btn_id"]]),
        show_conf_int_btn_id = session$ns(info[["show_conf_int_btn_id"]]),
        group_name = group_name,
        group_color = get_group_color(group_colors, "group", group_name)
      )
    })
    do.call(tagList, selectors)
  })

  network_choices <- ena3d_network_choices(
    groups = rv_data$ena_groups,
    units = prepared$network_units
  )
  updateSelectInput(
    session,
    "network_selector",
    choices = network_choices,
    selected = ena3d_network_selector_encode("none")
  )

  rv_data$initialized <- TRUE
  state$is_app_initialized <- TRUE
  invisible(ena_obj)
}
