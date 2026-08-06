library(testthat)

if (!exists("compute_centroid_path", mode = "function")) {
  core_candidates <- c(
    file.path("R", "trajectory_analysis.R"),
    file.path("..", "..", "R", "trajectory_analysis.R"),
    file.path("..", "R", "trajectory_analysis.R")
  )
  core_file <- core_candidates[file.exists(core_candidates)][1L]
  if (is.na(core_file)) stop("Could not locate R/trajectory_analysis.R")
  source(core_file)
}

.trajectory_test_integer64_from_hex <- function(values) {
  bytes <- do.call(c, lapply(values, function(value) {
    positions <- seq.int(1L, 15L, by = 2L)
    as.raw(strtoi(
      substring(value, positions, positions + 1L), base = 16L
    ))
  }))
  structure(
    readBin(
      bytes, what = double(), n = length(values), size = 8L, endian = "big"
    ),
    class = "integer64"
  )
}

test_that("full-space distance always includes selected dimensions", {
  points <- data.frame(
    id = rep(1:2, 2),
    time = rep(1:2, each = 2),
    x = c(0, 0, 3, 3),
    y = c(0, 0, 4, 4),
    z = c(0, 0, 12, 12)
  )

  path <- suppressWarnings(compute_centroid_path(
    points, "time", "id", dimensions = c("x", "y"),
    distance_space = "full", full_dimensions = "z"
  ))

  expect_equal(path$step_distance, c(0, 13))
  expect_identical(
    attr(path, "trajectory_spec")$distance_dimensions,
    c("z", "x", "y")
  )
})

test_that("integer and double analytical keys match semantically", {
  side_a <- data.frame(
    id = rep(c(1L, 2L), 2),
    time = rep(c(1L, 2L), each = 2),
    x = c(0, 10, 2, 12),
    y = 0
  )
  side_b <- data.frame(
    id = rep(c(1, 2), 2),
    time = rep(c(1, 2), each = 2),
    x = side_a$x + 1,
    y = 0
  )

  comparison <- suppressWarnings(compare_centroid_paths(
    side_a, side_b, "time", "id", dimensions = c("x", "y"),
    n_boot = 20, seed = 17
  ))

  expect_identical(.trajectory_value_key(c(1L, 2L)),
                   .trajectory_value_key(c(1, 2)))
  expect_false(identical(.trajectory_value_key(1),
                         .trajectory_value_key("0x1p+0")))
  expect_false(identical(.trajectory_value_key(NA_real_),
                         .trajectory_value_key("<NA>")))
  expect_identical(.trajectory_value_key(-0), .trajectory_value_key(0))
  expect_equal(comparison$n_matched, c(2L, 2L))
  expect_equal(comparison$difference_x, c(1, 1))
  expect_false("no_matched_participants" %in%
                 attr(comparison, "trajectory_warnings")$code)

  side_text <- side_b
  side_text$id <- rep(c("0x1p+0", "0x1p+1"), 2L)
  false_collision <- suppressWarnings(compare_centroid_paths(
    side_a, side_text, "time", "id", dimensions = c("x", "y"),
    n_boot = 2, seed = 19
  ))
  expect_identical(false_collision$n_matched, c(0L, 0L))
  expect_true(all(is.na(false_collision$difference_x)))
})

test_that("integer64 keys remain exact above the double precision boundary", {
  # bit64 stores signed 64-bit integers in the raw bits of a double vector.
  # These two storage values are 2^53 and 2^53 + 1 respectively; constructing
  # them directly keeps this regression runnable even when the optional bit64
  # namespace is not installed in a minimal test image.
  large_ids <- .trajectory_test_integer64_from_hex(
    c("0020000000000000", "0020000000000001")
  )
  scalar_id <- function(index) {
    structure(unclass(large_ids)[index], class = "integer64")
  }

  expect_identical(.trajectory_value_family(large_ids), "integer64")
  expect_false(any(.trajectory_is_missing(large_ids)))
  expect_length(unique(.trajectory_value_key(large_ids)), 2L)
  expect_false(identical(
    .trajectory_value_key(large_ids),
    .trajectory_value_key(c(2^53, 2^53))
  ))
  expect_false(identical(
    .trajectory_group_value_label(scalar_id(1L)),
    .trajectory_group_value_label(scalar_id(2L))
  ))
  expect_identical(
    .trajectory_is_missing(.trajectory_test_integer64_from_hex(
      "8000000000000000"
    )), TRUE
  )
  expect_identical(
    .trajectory_is_missing(.trajectory_test_integer64_from_hex(
      "0000000000000000"
    )), FALSE
  )

  points <- data.frame(
    id = structure(
      unclass(large_ids)[c(1L, 2L, 2L)], class = "integer64"
    ),
    time = 1L,
    x = c(0, 10, 20),
    y = c(0, 10, 20)
  )
  path <- suppressWarnings(compute_centroid_path(
    points, "time", "id", dimensions = c("x", "y"), order = 1L
  ))
  expect_identical(path$n_total, 2L)
  expect_identical(path$n_used, 2L)
  expect_identical(path$n_duplicate_rows, 1L)
  expect_equal(path$centroid_x, 7.5)

  side_a <- data.frame(id = scalar_id(1L), time = 1L, x = 0, y = 0)
  side_b <- data.frame(id = scalar_id(2L), time = 1L, x = 10, y = 10)
  comparison <- suppressWarnings(compare_centroid_paths(
    side_a, side_b, "time", "id", dimensions = c("x", "y"),
    order = 1L, n_boot = 2, seed = 23
  ))
  expect_identical(comparison$n_matched, 0L)
  expect_true(is.na(comparison$difference_x))
  expect_true(
    "no_matched_participants" %in%
      attr(comparison, "trajectory_warnings")$code
  )

  expect_error(
    .trajectory_union_groups(
      data.frame(group = scalar_id(1L)),
      data.frame(group = 2^53),
      "group"
    ),
    "compatible value types"
  )

  if (requireNamespace("bit64", quietly = TRUE)) {
    integer64_time <- bit64::as.integer64(
      c("9007199254740992", "9007199254740993")
    )
    time_path <- suppressWarnings(compute_centroid_path(
      data.frame(id = "p1", time = integer64_time, x = 0:1, y = 0:1),
      "time", "id", dimensions = c("x", "y")
    ))
    expect_identical(time_path$time_order, 1:2)
    expect_equal(time_path$elapsed_interval, c(NA, 1))
  }
})

test_that("integer64 copy and elapsed arithmetic preserve all storage bits", {
  storage_hex <- c(
    "0000000000000000", "0000000000000001",
    "7fefffffffffffff", "7ff0000000000000",
    "7ff0000000000001", "7ff8000000000000",
    "7fffffffffffffff", "8000000000000000",
    "8000000000000001", "fff0000000000000",
    "fff8000000000000", "ffffffffffffffff"
  )
  integer64_values <- .trajectory_test_integer64_from_hex(storage_hex)
  frame <- structure(
    list(
      id = integer64_values,
      time = seq_along(integer64_values),
      x = seq_along(integer64_values),
      y = seq_along(integer64_values)
    ),
    class = "data.frame",
    row.names = .set_row_names(length(integer64_values))
  )
  copied <- .trajectory_copy_frame(frame)

  expect_s3_class(copied$id, "integer64")
  expect_identical(.trajectory_integer64_hex(copied$id), storage_hex)
  expect_identical(
    .trajectory_is_missing(copied$id),
    seq_along(storage_hex) == match("8000000000000000", storage_hex)
  )
  expect_length(unique(.trajectory_value_key(copied$id)), length(storage_hex))

  difference <- function(first, second) {
    .trajectory_integer64_differences(
      .trajectory_test_integer64_from_hex(c(first, second))
    )
  }
  expect_identical(
    difference("0020000000000000", "0020000000000001"), 1
  )
  expect_identical(
    difference("7ffffffffffffffe", "7fffffffffffffff"), 1
  )
  expect_identical(
    difference("8000000000000001", "8000000000000002"), 1
  )
  expect_equal(
    difference("8000000000000001", "7fffffffffffffff"),
    18446744073709551614,
    tolerance = 1e-15
  )
  expect_equal(
    difference("7fffffffffffffff", "8000000000000001"),
    -18446744073709551614,
    tolerance = 1e-15
  )
  expect_identical(
    difference("ffffffffffffffff", "0000000000000001"), 2
  )
  expect_identical(
    difference("0000000000000001", "ffffffffffffffff"), -2
  )
  expect_identical(
    difference("00000000ffffffff", "0000000100000000"), 1
  )
  expect_identical(
    sprintf("%a", difference("49308dcc6fbc22ac", "40e26eec3bd04f4e")),
    "-0x1.09c3dc067d7a7p+59"
  )
  expect_identical(
    sprintf("%a", difference("40e26eec3bd04f4e", "49308dcc6fbc22ac")),
    "0x1.09c3dc067d7a7p+59"
  )
  expect_identical(
    sprintf("%a", difference("fa271583b9bf3739", "f8dd1ce5abdd3457")),
    "-0x1.49f89e0de202ep+56"
  )
  expect_true(is.na(difference(
    "8000000000000000", "0000000000000001"
  )))
  expect_identical(
    .trajectory_integer64_differences(integer64_values[1L]), numeric()
  )
  extreme_elapsed <- .trajectory_elapsed(.trajectory_test_integer64_from_hex(
    c("8000000000000001", "7fffffffffffffff")
  ))
  expect_equal(
    extreme_elapsed$values,
    c(NA_real_, 18446744073709551614),
    tolerance = 1e-15
  )

  expect_error(.trajectory_integer64_hex(1), "Expected a bit64")
  expect_error(
    .trajectory_integer64_hex(structure(1L, class = "integer64")),
    "64-bit storage representation"
  )
  expect_identical(
    .trajectory_integer64_hex(structure(numeric(), class = "integer64")),
    character()
  )
})

test_that("integer64 copy loads subset methods in a genuinely fresh R process", {
  project_root <- if (file.exists(file.path("R", "trajectory_analysis.R"))) {
    normalizePath(".", mustWork = TRUE)
  } else {
    normalizePath(file.path("..", ".."), mustWork = TRUE)
  }
  core_path <- file.path(project_root, "R", "trajectory_analysis.R")
  script <- c(
    sprintf("source(%s, local = .GlobalEnv)", encodeString(core_path, quote = '"')),
    "stopifnot(!\"bit64\" %in% loadedNamespaces())",
    paste0(
      "raw64 <- structure(c(0x1p-1021, ",
      "0x1.0000000000001p-1021, -0), class = \"integer64\")"
    ),
    paste0(
      "frame <- structure(list(id=raw64,time=1:3,x=1:3,y=1:3), ",
      "class=\"data.frame\",row.names=.set_row_names(3L))"
    ),
    "has_bit64 <- nzchar(find.package(\"bit64\", quiet = TRUE))",
    "copy <- tryCatch(.trajectory_copy_frame(frame), error = identity)",
    paste0(
      "if (!has_bit64) { stopifnot(inherits(copy, \"error\"), ",
      "grepl(\"bit64\", conditionMessage(copy), fixed = TRUE)); quit() }"
    ),
    "stopifnot(\"bit64\" %in% loadedNamespaces())",
    "stopifnot(inherits(copy$id, \"integer64\"))",
    paste0(
      "stopifnot(identical(.trajectory_integer64_hex(copy$id), ",
      "c(\"0020000000000000\",\"0020000000000001\",",
      "\"8000000000000000\")))"
    ),
    paste0(
      "stopifnot(identical(.trajectory_is_missing(copy$id), ",
      "c(FALSE,FALSE,TRUE)))"
    )
  )
  output <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "-"), stdout = TRUE, stderr = TRUE, input = script
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
})

test_that("integer64 order resolution loads methods before missing filtering", {
  project_root <- if (file.exists(file.path("R", "trajectory_analysis.R"))) {
    normalizePath(".", mustWork = TRUE)
  } else {
    normalizePath(file.path("..", ".."), mustWork = TRUE)
  }
  core_path <- file.path(project_root, "R", "trajectory_analysis.R")
  script <- c(
    sprintf("source(%s, local = .GlobalEnv)", encodeString(core_path, quote = '"')),
    "stopifnot(!\"bit64\" %in% loadedNamespaces())",
    paste0(
      "raw64 <- structure(c(0x1p-1021, ",
      "0x1.0000000000001p-1021, -0), class = \"integer64\")"
    ),
    "has_bit64 <- nzchar(find.package(\"bit64\", quiet = TRUE))",
    "resolved <- tryCatch(.trajectory_resolve_order(raw64), error = identity)",
    paste0(
      "if (!has_bit64) { stopifnot(inherits(resolved, \"error\"), ",
      "grepl(\"bit64\", conditionMessage(resolved), fixed = TRUE)); quit() }"
    ),
    "stopifnot(\"bit64\" %in% loadedNamespaces())",
    "stopifnot(inherits(resolved$values, \"integer64\"))",
    paste0(
      "stopifnot(identical(.trajectory_integer64_hex(resolved$values), ",
      "c(\"0020000000000000\",\"0020000000000001\")))"
    ),
    "stopifnot(length(unique(resolved$keys)) == 2L)"
  )
  output <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "-"), stdout = TRUE, stderr = TRUE, input = script
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
})

test_that("signed zero uses one semantic key without dropping rows", {
  points <- data.frame(
    id = c("negative", "positive"), time = 1L, group = c(-0, 0),
    x = c(0, 10), y = c(0, 10)
  )

  path <- suppressWarnings(compute_centroid_path(
    points, "time", "id", group_vars = "group",
    dimensions = c("x", "y"), order = 1L
  ))

  expect_equal(nrow(path), 1L)
  expect_identical(path$n_total, 2L)
  expect_identical(path$n_used, 2L)
  expect_equal(path$centroid_x, 5)
  expect_equal(path$centroid_y, 5)
})

test_that("comparison rejects incompatible grouping value families", {
  side_a <- data.frame(id = "a", time = 1, group = 1, x = 0, y = 0)
  side_b <- data.frame(
    id = "b", time = 1, group = "0x1p+0", x = 1, y = 1,
    stringsAsFactors = FALSE
  )

  expect_error(
    compare_independent_centroid_paths(
      side_a, side_b, "time", "id", group_vars = "group",
      dimensions = c("x", "y"), n_boot = 2, n_perm = 2
    ),
    "compatible value types"
  )
})

test_that("adjacent numeric groups keep unique diagnostic and sampling labels", {
  adjacent <- c(1, 1 + .Machine$double.eps)
  points <- data.frame(
    id = rep(c("first", "second"), each = 2L),
    time = rep(1:2, times = 2L),
    group = rep(adjacent, each = 2L),
    x = c(0, 1, 10, 11),
    y = c(0, 1, 20, 21)
  )

  path <- suppressWarnings(compute_centroid_path(
    points, "time", "id", group_vars = "group",
    dimensions = c("x", "y"), order = c(1, 2)
  ))
  diagnostics <- attr(path, "trajectory_warnings")
  scoped_groups <- unique(
    diagnostics$group[diagnostics$code == "one_entity_slice"]
  )
  plan <- .trajectory_bootstrap_sampling_plan(
    points, attr(path, "trajectory_spec"), weights = NULL,
    cohort_policy = "available"
  )

  expect_length(scoped_groups, 2L)
  expect_true(all(grepl("\\[exact=", scoped_groups)))
  expect_length(unique(plan$labels), 2L)
  expect_identical(unname(plan$labels), scoped_groups)
  expect_length(unique(names(plan$pools)), 2L)
  expect_identical(
    .trajectory_group_label("Group", data.frame(Group = "G1")),
    "Group=G1"
  )
  delimited <- data.frame(
    a = c("x, b=y", "x"), b = c("z", "y, b=z"),
    stringsAsFactors = FALSE
  )
  delimited_labels <- vapply(seq_len(nrow(delimited)), function(row) {
    .trajectory_group_label(c("a", "b"), delimited[row, , drop = FALSE])
  }, character(1L))
  expect_length(unique(delimited_labels), 2L)
})

test_that("comparison APIs diagnose non-positive explicit elapsed intervals", {
  side_a <- expand.grid(id = c("p1", "p2"), time = 1:2)
  side_a$x <- as.numeric(side_a$id == "p2") + side_a$time
  side_a$y <- side_a$time
  side_b <- side_a
  side_b$x <- side_b$x + 1

  paired <- suppressWarnings(compare_centroid_paths(
    side_a, side_b, "time", "id", dimensions = c("x", "y"),
    order = c(2, 1), n_boot = 2, seed = 41
  ))
  independent <- suppressWarnings(compare_independent_centroid_paths(
    side_a, side_b, "time", "id", dimensions = c("x", "y"),
    order = c(2, 1), n_boot = 2, n_perm = 2, seed = 43
  ))

  for (comparison in list(paired, independent)) {
    expect_equal(comparison$elapsed_interval, c(NA, -1))
    expect_true(all(is.na(comparison$speed_a)))
    expect_true(all(is.na(comparison$speed_b)))
    diagnostics <- attr(comparison, "trajectory_warnings")
    nonpositive <- diagnostics[
      diagnostics$code == "nonpositive_elapsed_interval", , drop = FALSE
    ]
    expect_equal(nrow(nonpositive), 1L)
    expect_identical(nonpositive$count, 1L)
    expect_match(nonpositive$message, "affected group-interval cell", fixed = TRUE)
  }

  grouped_a <- expand.grid(
    id = c("p1", "p2"), time = 1:2, group = c("A", "B"),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  grouped_a$x <- as.numeric(grouped_a$id == "p2") + grouped_a$time
  grouped_a$y <- grouped_a$time
  grouped_b <- grouped_a
  grouped_b$x <- grouped_b$x + 1
  grouped <- list(
    suppressWarnings(compute_centroid_path(
      grouped_a, "time", "id", group_vars = "group",
      dimensions = c("x", "y"), order = c(2, 1)
    )),
    suppressWarnings(compare_centroid_paths(
      grouped_a, grouped_b, "time", "id", group_vars = "group",
      dimensions = c("x", "y"), order = c(2, 1), n_boot = 2, seed = 47
    )),
    suppressWarnings(compare_independent_centroid_paths(
      grouped_a, grouped_b, "time", "id", group_vars = "group",
      dimensions = c("x", "y"), order = c(2, 1),
      n_boot = 2, n_perm = 2, seed = 49
    ))
  )
  for (result in grouped) {
    diagnostics <- attr(result, "trajectory_warnings")
    nonpositive <- diagnostics[
      diagnostics$code == "nonpositive_elapsed_interval", , drop = FALSE
    ]
    expect_identical(nonpositive$count, 2L)
    expect_match(nonpositive$message, "affected group-interval cell", fixed = TRUE)
  }
})

test_that("bootstrap preserves ID-column weight semantics after cloning", {
  points <- expand.grid(id = 1:4, time = 1:2)
  points$x <- points$id^2 + points$time
  points$y <- points$id * points$time
  points$weight_copy <- points$id

  weighted_by_id <- suppressWarnings(bootstrap_centroid_path(
    points, "time", "id", dimensions = c("x", "y"), weights = "id",
    n_boot = 100, conf_level = 0.5, seed = 29
  ))
  weighted_by_copy <- suppressWarnings(bootstrap_centroid_path(
    points, "time", "id", dimensions = c("x", "y"),
    weights = "weight_copy", n_boot = 100, conf_level = 0.5, seed = 29
  ))

  interval_columns <- grep("_(lower|upper|boot_n)$", names(weighted_by_id),
                           value = TRUE)
  expect_equal(weighted_by_id[interval_columns],
               weighted_by_copy[interval_columns])
  expect_equal(weighted_by_id$centroid_x, weighted_by_copy$centroid_x)
  expect_equal(weighted_by_id$centroid_y, weighted_by_copy$centroid_y)
  expect_identical(attr(weighted_by_id, "trajectory_spec")$weights,
                   "column:id")
})

test_that("paired comparison tolerates different factor level sets", {
  side_a <- data.frame(
    id = rep(c("p1", "p2"), 2),
    time = factor(rep(c("pre", "post"), each = 2),
                  levels = c("pre", "post", "unused-a")),
    group = factor("shared", levels = c("shared", "unused-a")),
    x = c(0, 10, 2, 12),
    y = 0
  )
  side_b <- data.frame(
    id = rep(c("p1", "p2"), 2),
    time = factor(rep(c("pre", "post"), each = 2),
                  levels = c("unused-b", "post", "pre")),
    group = factor("shared", levels = c("unused-b", "shared")),
    x = side_a$x + 3,
    y = 0
  )

  comparison <- suppressWarnings(compare_centroid_paths(
    side_a, side_b, "time", "id", group_vars = "group",
    dimensions = c("x", "y"), order = c("pre", "post"),
    n_boot = 20, seed = 31
  ))

  expect_equal(as.character(comparison$group), c("shared", "shared"))
  expect_equal(comparison$n_matched, c(2L, 2L))
  expect_equal(comparison$difference_x, c(3, 3))
})
