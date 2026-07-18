options(stringsAsFactors = FALSE, warn = 1)

project_root <- normalizePath(
  "/Users/peter/Desktop/3dENA/ENA_3d-main", mustWork = TRUE
)
output_root <- file.path(project_root, "output", "bundled-real-e2e")
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(digest)
  library(htmlwidgets)
  library(jsonlite)
  library(plotly)
})

source(file.path(project_root, "R", "security_utils.R"), local = FALSE)
source(file.path(project_root, "R", "app_utils.R"), local = FALSE)
source(file.path(project_root, "R", "transition.R"), local = FALSE)
source(file.path(project_root, "R", "app_module_load_dataset.R"), local = FALSE)
source(file.path(project_root, "R", "ena3d_exchange.R"), local = FALSE)
source(file.path(project_root, "R", "trajectory_analysis.R"), local = FALSE)
source(file.path(project_root, "R", "trajectory_plot.R"), local = FALSE)
source(file.path(project_root, "R", "app_module_trajectory.R"), local = FALSE)

bootstrap_repetitions <- 500L
permutation_repetitions <- 999L
confidence_level <- 0.95
base_seed <- 20260718L

dataset_specs <- list(
  newfrat = list(
    filename = "newfrat_enaset.Rdata",
    seed = base_seed + 1L,
    analysis_kind = "longitudinal participant trajectory",
    time_var = "Week",
    id_var = "Name",
    order = as.character(0:14),
    cohort_policy = "complete",
    comparison_design = "paired early-versus-late aligned weeks",
    comparison_direction = "Late weeks 8-14 - early weeks 0-6"
  ),
  sample = list(
    filename = "sample_enaset.Rdata",
    seed = base_seed + 2L,
    analysis_kind = "ordered cross-sectional group profile",
    time_var = "groupid",
    id_var = "ParticipantID",
    order = c("1", "2"),
    cohort_policy = "available",
    comparison_design = "independent group 1 versus group 2",
    comparison_direction = "group 2 - group 1"
  ),
  student = list(
    filename = "student_enaset.RData",
    seed = base_seed + 3L,
    analysis_kind = "ordered cross-sectional performance profile",
    time_var = "PerformanceBand",
    id_var = "Name",
    order = c("Low (60-74)", "Middle (75-89)", "High (90-100)"),
    cohort_policy = "available",
    comparison_design = "independent low versus high performance",
    comparison_direction = "High performance - low performance"
  )
)

load_trusted_ena <- function(path) {
  loaded <- new.env(parent = emptyenv())
  object_names <- load(path, envir = loaded)
  if (length(object_names) != 1L) {
    stop(basename(path), " must contain exactly one top-level object.")
  }
  object <- loaded[[object_names[[1L]]]]
  if (!inherits(object, "ena.set") || is.null(object$points) ||
      is.null(object$rotation.matrix) || is.null(object$line.weights)) {
    stop(basename(path), " is not a complete trusted ENA set.")
  }
  list(object = object, object_name = object_names[[1L]])
}

ena_dimensions <- function(points) {
  dimensions <- names(points)[vapply(
    points, inherits, logical(1L), what = "ena.dimension"
  )]
  if (!length(dimensions)) {
    dimensions <- names(points)[grepl("^(MR|SVD)[0-9]+$", names(points))]
  }
  dimensions
}

prepare_analysis_points <- function(slug, points) {
  points <- as.data.frame(points, stringsAsFactors = FALSE, check.names = FALSE)
  if (slug == "newfrat") {
    points$Week <- as.character(points$Week)
  } else if (slug == "sample") {
    points$groupid <- as.character(points$groupid)
    points$ParticipantID <- paste(points$groupid, points$username, sep = "::")
  } else if (slug == "student") {
    score <- suppressWarnings(as.numeric(as.character(
      points[["Science Performance"]]
    )))
    if (any(!is.finite(score)) || any(score < 60 | score > 100)) {
      stop("student Science Performance must be finite and within 60-100.")
    }
    points$PerformanceBand <- cut(
      score,
      breaks = c(-Inf, 74, 89, Inf),
      labels = c("Low (60-74)", "Middle (75-89)", "High (90-100)"),
      ordered_result = TRUE
    )
    points$PerformanceBand <- as.character(points$PerformanceBand)
  }
  points
}

build_comparison <- function(slug, points, dimensions, spec) {
  if (slug == "newfrat") {
    week <- as.integer(points$Week)
    early <- points[week >= 0L & week <= 6L, , drop = FALSE]
    late <- points[week >= 8L & week <= 14L, , drop = FALSE]
    early$RelativeWeek <- as.integer(early$Week)
    late$RelativeWeek <- as.integer(late$Week) - 8L
    comparison <- suppressWarnings(compare_centroid_paths(
      points_a = early,
      points_b = late,
      time_var = "RelativeWeek",
      id_var = "Name",
      dimensions = dimensions,
      order = 0:6,
      cohort_policy = "complete",
      na_policy = "error",
      distance_space = "selected",
      n_boot = bootstrap_repetitions,
      conf_level = confidence_level,
      seed = spec$seed,
      labels = c("Early weeks 0-6", "Late weeks 8-14")
    ))
    if (!inherits(comparison, "paired_centroid_path_comparison") ||
        any(comparison$n_used != 17L)) {
      stop("newfrat paired comparison did not retain all 17 participants.")
    }
    return(comparison)
  }

  if (slug == "sample") {
    side_a <- points[points$groupid == "1", , drop = FALSE]
    side_b <- points[points$groupid == "2", , drop = FALSE]
    side_a$ComparisonTime <- "Snapshot"
    side_b$ComparisonTime <- "Snapshot"
    comparison <- suppressWarnings(compare_independent_centroid_paths(
      points_a = side_a,
      points_b = side_b,
      time_var = "ComparisonTime",
      id_var = "ParticipantID",
      dimensions = dimensions,
      order = "Snapshot",
      cohort_policy = "complete",
      na_policy = "error",
      distance_space = "selected",
      n_boot = bootstrap_repetitions,
      n_perm = permutation_repetitions,
      conf_level = confidence_level,
      seed = spec$seed,
      labels = c("group 1", "group 2"),
      p_adjust_method = "holm"
    ))
    if (any(comparison$n_a_used != 2L) || any(comparison$n_b_used != 2L)) {
      stop("sample independent comparison must retain 2 + 2 units.")
    }
    return(comparison)
  }

  side_a <- points[points$PerformanceBand == "Low (60-74)", , drop = FALSE]
  side_b <- points[points$PerformanceBand == "High (90-100)", , drop = FALSE]
  side_a$ComparisonTime <- "Snapshot"
  side_b$ComparisonTime <- "Snapshot"
  comparison <- suppressWarnings(compare_independent_centroid_paths(
    points_a = side_a,
    points_b = side_b,
    time_var = "ComparisonTime",
    id_var = "Name",
    dimensions = dimensions,
    order = "Snapshot",
    cohort_policy = "complete",
    na_policy = "error",
    distance_space = "selected",
    n_boot = bootstrap_repetitions,
    n_perm = permutation_repetitions,
    conf_level = confidence_level,
    seed = spec$seed,
    labels = c("Low performance", "High performance"),
    p_adjust_method = "holm"
  ))
  if (any(comparison$n_a_used != 18L) || any(comparison$n_b_used != 13L)) {
    stop("student independent comparison must retain 18 low + 13 high units.")
  }
  comparison
}

write_and_check_csv <- function(data, target, metadata = list()) {
  exported <- .trajectory_export_metadata(data, metadata)
  .trajectory_write_csv(exported, target)
  imported <- utils::read.csv(
    target,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = ""
  )
  if (!identical(names(imported), names(exported)) ||
      nrow(imported) != nrow(exported)) {
    stop(basename(target), " changed its table shape during CSV round trip.")
  }
  for (column in names(data)[vapply(data, is.numeric, logical(1L))]) {
    if (!isTRUE(all.equal(
      as.numeric(data[[column]]), suppressWarnings(as.numeric(imported[[column]])),
      tolerance = 1e-12, check.attributes = FALSE
    ))) {
      stop(basename(target), " changed numeric column ", column, ".")
    }
  }
  for (column in names(data)[vapply(data, is.logical, logical(1L))]) {
    if (!identical(data[[column]], imported[[column]])) {
      stop(basename(target), " changed logical column ", column, ".")
    }
  }
  imported
}

compare_numeric_frames <- function(reference, candidate, context) {
  if (!identical(names(reference), names(candidate)) ||
      nrow(reference) != nrow(candidate)) {
    stop(context, " changed table shape.")
  }
  numeric_columns <- names(reference)[vapply(reference, is.numeric, logical(1L))]
  for (column in numeric_columns) {
    if (!isTRUE(all.equal(
      as.numeric(reference[[column]]), as.numeric(candidate[[column]]),
      tolerance = 1e-12, check.attributes = FALSE
    ))) {
      stop(context, " changed numeric column ", column, ".")
    }
  }
  invisible(TRUE)
}

dataset_manifests <- list()

for (slug in names(dataset_specs)) {
  spec <- dataset_specs[[slug]]
  source_path <- file.path(project_root, "sample_data", spec$filename)
  dataset_output <- file.path(output_root, slug)
  dir.create(dataset_output, recursive = TRUE, showWarnings = FALSE)

  loaded <- load_trusted_ena(source_path)
  ena_set <- loaded$object
  points <- prepare_analysis_points(slug, ena_set$points)
  dimensions_all <- ena_dimensions(ena_set$points)
  if (length(dimensions_all) < 3L) {
    stop(spec$filename, " has fewer than three ENA dimensions.")
  }
  dimensions <- dimensions_all[seq_len(3L)]

  required_columns <- unique(c(spec$time_var, spec$id_var, dimensions))
  if (length(setdiff(required_columns, names(points)))) {
    stop(spec$filename, " is missing required analytical columns.")
  }
  bad_key <- Reduce(`|`, lapply(points[c(spec$time_var, spec$id_var)], function(x) {
    is.na(x) | !nzchar(trimws(as.character(x)))
  }))
  bad_dimension <- Reduce(`|`, lapply(points[dimensions], function(x) {
    is.na(x) | !is.finite(x)
  }))
  duplicate_grain <- duplicated(points[c(spec$time_var, spec$id_var)])
  participant_count <- length(unique(points[[spec$id_var]][!bad_key]))
  period_counts <- table(factor(
    as.character(points[[spec$time_var]]), levels = spec$order
  ))

  path <- suppressWarnings(compute_centroid_path(
    points = points,
    time_var = spec$time_var,
    id_var = spec$id_var,
    dimensions = dimensions,
    order = spec$order,
    cohort_policy = spec$cohort_policy,
    na_policy = "error",
    distance_space = "selected"
  ))
  bootstrap <- suppressWarnings(bootstrap_centroid_path(
    points = points,
    time_var = spec$time_var,
    id_var = spec$id_var,
    dimensions = dimensions,
    order = spec$order,
    cohort_policy = spec$cohort_policy,
    na_policy = "error",
    distance_space = "selected",
    n_boot = bootstrap_repetitions,
    conf_level = confidence_level,
    seed = spec$seed,
    bootstrap_design = "auto"
  ))
  comparison <- build_comparison(slug, points, dimensions, spec)

  plot <- plot_centroid_trajectory_3d(
    bootstrap,
    dimensions = dimensions,
    show_direction = TRUE,
    show_warnings = TRUE,
    axis_titles = dimensions
  )
  built_plot <- plotly_build(plot)
  path_trace_count <- sum(vapply(built_plot$x$data, function(trace) {
    identical(trace$type, "scatter3d") &&
      identical(trace$mode, "lines+markers")
  }, logical(1L)))
  if (path_trace_count != 1L) {
    stop(slug, " 3D plot must contain exactly one path trace.")
  }
  plot_html <- file.path(dataset_output, "trajectory_3d.html")
  htmlwidgets::saveWidget(
    plot,
    file = plot_html,
    selfcontained = FALSE,
    libdir = "trajectory_3d_files",
    title = paste0("3dENA bundled real-data E2E: ", slug)
  )
  writeLines(
    as.character(plotly_json(plot, jsonedit = FALSE, pretty = TRUE)),
    file.path(dataset_output, "trajectory_3d.plotly.json"),
    useBytes = TRUE
  )

  metadata <- list(
    source_file = spec$filename,
    source_sha256 = digest(
      source_path, algo = "sha256", file = TRUE, serialize = FALSE
    ),
    source_object = loaded$object_name,
    analysis_kind = spec$analysis_kind,
    time_var = spec$time_var,
    id_var = spec$id_var,
    dimensions = paste(dimensions, collapse = ";"),
    order = paste(spec$order, collapse = ";"),
    cohort_policy = spec$cohort_policy,
    bootstrap_repetitions = bootstrap_repetitions,
    permutation_repetitions = permutation_repetitions,
    confidence_level = confidence_level,
    seed = spec$seed,
    comparison_design = spec$comparison_design,
    comparison_direction = spec$comparison_direction
  )
  points_imported <- write_and_check_csv(
    points,
    file.path(dataset_output, "analysis_points.csv"),
    metadata
  )
  path_imported <- write_and_check_csv(
    path, file.path(dataset_output, "path.csv"), metadata
  )
  bootstrap_imported <- write_and_check_csv(
    bootstrap, file.path(dataset_output, "bootstrap.csv"), metadata
  )
  comparison_imported <- write_and_check_csv(
    comparison, file.path(dataset_output, "comparison.csv"), metadata
  )

  points_reimported <- prepare_analysis_points(slug, points_imported)
  path_recomputed <- suppressWarnings(compute_centroid_path(
    points = points_reimported,
    time_var = spec$time_var,
    id_var = spec$id_var,
    dimensions = dimensions,
    order = spec$order,
    cohort_policy = spec$cohort_policy,
    na_policy = "error",
    distance_space = "selected"
  ))
  compare_numeric_frames(
    as.data.frame(path), as.data.frame(path_recomputed),
    paste0(slug, " point-CSV-to-path recomputation")
  )
  compare_numeric_frames(
    as.data.frame(path), path_imported[names(path)],
    paste0(slug, " path CSV round trip")
  )
  compare_numeric_frames(
    as.data.frame(bootstrap), bootstrap_imported[names(bootstrap)],
    paste0(slug, " bootstrap CSV round trip")
  )
  compare_numeric_frames(
    as.data.frame(comparison), comparison_imported[names(comparison)],
    paste0(slug, " comparison CSV round trip")
  )

  quality <- data.frame(
    check = c(
      "ena_set_class", "points_rows", "points_columns", "ena_dimensions",
      "selected_key_missing_rows", "selected_dimension_invalid_rows",
      "duplicate_time_id_rows", "analysis_participants", "analysis_periods",
      "minimum_period_count", "maximum_period_count"
    ),
    value = c(
      1L, nrow(points), ncol(points), length(dimensions_all), sum(bad_key),
      sum(bad_dimension), sum(duplicate_grain), participant_count,
      length(spec$order), min(period_counts), max(period_counts)
    ),
    severity = c(
      "pass", "info", "info", "info",
      if (any(bad_key)) "high" else "pass",
      if (any(bad_dimension)) "high" else "pass",
      if (any(duplicate_grain)) "high" else "pass",
      "info", "info", "info", "info"
    ),
    interpretation = c(
      "Trusted native object inherits ena.set and contains points/rotation/network components.",
      "ENA point-table row count.",
      "Analysis point-table columns after documented derived fields.",
      "Available ENA rotation dimensions.",
      "Rows missing the declared time or participant key.",
      "Rows with invalid selected ENA coordinates.",
      "Duplicate rows at the declared time + participant grain.",
      "Distinct IDs under the declared analysis namespace.",
      "Declared ordered path slices.",
      "Smallest source-point count across declared slices.",
      "Largest source-point count across declared slices."
    ),
    stringsAsFactors = FALSE
  )
  .trajectory_write_csv(quality, file.path(dataset_output, "data_quality.csv"))
  .trajectory_write_csv(
    attr(path, "trajectory_warnings", exact = TRUE),
    file.path(dataset_output, "path_diagnostics.csv")
  )
  .trajectory_write_csv(
    attr(bootstrap, "trajectory_warnings", exact = TRUE),
    file.path(dataset_output, "bootstrap_diagnostics.csv")
  )
  .trajectory_write_csv(
    attr(comparison, "trajectory_warnings", exact = TRUE),
    file.path(dataset_output, "comparison_diagnostics.csv")
  )

  comparison_class <- class(comparison)[1L]
  p_columns <- grep("_p_adjusted$", names(comparison), value = TRUE)
  significant_columns <- grep("_significant$", names(comparison), value = TRUE)
  finite_adjusted_tests <- if (length(p_columns)) {
    sum(vapply(comparison[p_columns], function(x) sum(is.finite(x)), integer(1L)))
  } else {
    0L
  }
  significant_tests <- if (length(significant_columns)) {
    sum(vapply(
      comparison[significant_columns],
      function(x) sum(x %in% TRUE, na.rm = TRUE), integer(1L)
    ))
  } else {
    0L
  }
  warnings <- unique(c(
    attr(path, "trajectory_warnings", exact = TRUE)$code,
    attr(bootstrap, "trajectory_warnings", exact = TRUE)$code,
    attr(comparison, "trajectory_warnings", exact = TRUE)$code
  ))

  manifest <- list(
    status = "completed",
    dataset = slug,
    source = list(
      file = spec$filename,
      object = loaded$object_name,
      bytes = unname(file.info(source_path)$size),
      sha256 = metadata$source_sha256,
      trusted_native_import = "pass"
    ),
    ena = list(
      points = nrow(points),
      dimensions = length(dimensions_all),
      selected_dimensions = dimensions,
      rotation_rows = nrow(ena_set$rotation.matrix),
      line_weight_rows = nrow(ena_set$line.weights)
    ),
    analysis = list(
      kind = spec$analysis_kind,
      time_var = spec$time_var,
      id_var = spec$id_var,
      order = spec$order,
      cohort_policy = spec$cohort_policy,
      path_rows = nrow(path),
      participant_count = participant_count
    ),
    bootstrap = list(
      repetitions = bootstrap_repetitions,
      confidence_level = confidence_level,
      seed = spec$seed,
      failed_replicates = attr(bootstrap, "bootstrap_spec")$failed_replicates
    ),
    comparison = list(
      design = spec$comparison_design,
      direction = spec$comparison_direction,
      class = comparison_class,
      rows = nrow(comparison),
      finite_adjusted_tests = finite_adjusted_tests,
      significant_tests = significant_tests
    ),
    plot = list(
      html = "trajectory_3d.html",
      plotly_trace_count = length(built_plot$x$data),
      path_trace_count = path_trace_count
    ),
    csv_roundtrip = list(
      analysis_points_to_path = "pass",
      path = "pass",
      bootstrap = "pass",
      comparison = "pass"
    ),
    diagnostics = warnings,
    caveat = if (slug == "newfrat") {
      "Longitudinal path; the paired comparison aligns early and late seven-week windows and excludes Week 7."
    } else if (slug == "sample") {
      "Ordered group profile, not a longitudinal trajectory; independent comparison has only 2 + 2 units and very low inferential power."
    } else {
      "Cross-sectional performance profile, not a longitudinal trajectory; performance bands are declared as 60-74, 75-89, and 90-100."
    },
    generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  write_json(
    manifest,
    file.path(dataset_output, "validation_manifest.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )
  dataset_manifests[[slug]] <- manifest
}

summary_rows <- do.call(rbind, lapply(dataset_manifests, function(manifest) {
  data.frame(
    dataset = manifest$dataset,
    status = manifest$status,
    source_points = manifest$ena$points,
    ena_dimensions = manifest$ena$dimensions,
    analysis_kind = manifest$analysis$kind,
    path_rows = manifest$analysis$path_rows,
    participants = manifest$analysis$participant_count,
    comparison_class = manifest$comparison$class,
    comparison_rows = manifest$comparison$rows,
    finite_adjusted_tests = manifest$comparison$finite_adjusted_tests,
    significant_tests = manifest$comparison$significant_tests,
    csv_roundtrip = "pass",
    stringsAsFactors = FALSE
  )
}))
.trajectory_write_csv(summary_rows, file.path(output_root, "summary.csv"))
write_json(
  list(
    status = "completed",
    datasets = dataset_manifests,
    generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  ),
  file.path(output_root, "validation_manifest.json"),
  auto_unbox = TRUE,
  pretty = TRUE,
  na = "null"
)
capture.output(sessionInfo(), file = file.path(output_root, "sessionInfo.txt"))
print(summary_rows)
