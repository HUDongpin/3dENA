library(testthat)

.class1_roots <- c(".", "../..", "..")
.class1_root <- .class1_roots[file.exists(
  file.path(.class1_roots, "R", "trajectory_analysis.R")
)][1L]
if (is.na(.class1_root)) stop("Could not locate the 3dENA project root.")
.class1_root <- normalizePath(.class1_root, mustWork = TRUE)

source(file.path(.class1_root, "R", "security_utils.R"), local = FALSE)
source(file.path(.class1_root, "R", "app_utils.R"), local = FALSE)
source(file.path(.class1_root, "R", "app_module_load_dataset.R"), local = FALSE)
source(file.path(.class1_root, "R", "trajectory_analysis.R"), local = FALSE)
source(file.path(.class1_root, "R", "trajectory_plot.R"), local = FALSE)

.class1_fixture <- file.path(
  .class1_root, "sample_data", "class1_timepoints_enaset.RData"
)

.class1_character_values <- function(value, depth = 0L) {
  if (depth > 10L || is.function(value) || is.environment(value)) {
    return(character())
  }
  if (is.factor(value)) return(as.character(value))
  if (is.character(value)) return(value)
  if (is.data.frame(value) || is.list(value)) {
    return(unlist(lapply(value, .class1_character_values, depth = depth + 1L),
                  use.names = FALSE))
  }
  character()
}


test_that("Class 1 trusted sample is pseudonymous and trajectory-ready", {
  expect_no_warning({
    ena <- ena3d_read_ena_object(.class1_fixture, source_kind = "bundled")
  })

  expect_s3_class(ena, "ena.set")
  expect_equal(nrow(ena$points), 72L)
  expect_equal(nrow(ena$rotation$nodes), 6L)
  expect_setequal(unique(as.character(ena$points$Group)), c("G1", "G2", "G3", "G6", "G7"))
  expect_identical(
    as.integer(table(factor(ena$points$Period, levels = c("TP1", "TP2", "TP3")))),
    c(24L, 23L, 25L)
  )
  expect_equal(length(unique(as.character(ena$points$Speaker))), 26L)
  expect_true(all(grepl("^G(?:1|2|3|6|7)-S[0-9]{2}$", ena$points$Speaker)))
  expect_setequal(
    unique(as.character(ena$points$Condition)),
    c("GenAI group", "Non-GenAI group")
  )
  expect_identical(ena$`_function.params`$trajectory.time.by, "Period")
  expect_identical(ena$`_function.params`$trajectory.id.by, "Speaker")
  expect_identical(ena$`_function.params`$trajectory.group.by, "Group")

  all_text <- .class1_character_values(ena)
  expect_false(any(grepl("[\u4e00-\u9fff]", all_text, perl = TRUE)))
})


test_that("Class 1 G1 centroids retain the reviewed shared-space values", {
  ena <- ena3d_read_ena_object(.class1_fixture, source_kind = "bundled")
  path <- compute_centroid_path(
    points = as.data.frame(ena$points),
    time_var = "Period",
    id_var = "Speaker",
    group_vars = "Group",
    dimensions = c("SVD1", "SVD2", "SVD3"),
    order = c("TP1", "TP2", "TP3"),
    cohort_policy = "available",
    na_policy = "error",
    distance_space = "selected"
  )
  group1 <- path[path$Group == "G1", , drop = FALSE]
  group1 <- group1[order(group1$time_order), , drop = FALSE]

  expect_identical(group1$n_used, c(5L, 3L, 5L))
  expect_equal(
    unname(as.matrix(group1[c(
      "centroid_SVD1", "centroid_SVD2", "centroid_SVD3"
    )])),
    rbind(
      c(0.639414488, -0.379242553, -0.131081517),
      c(0.341784448, 0.096713762, -0.409657789),
      c(-0.099526444, -0.278023590, -0.135813091)
    ),
    tolerance = 1e-8
  )
})


test_that("Class 1 G1 plot retains the reviewed axis and cone geometry", {
  skip_if_not_installed("plotly")
  ena <- suppressWarnings(ena3d_read_ena_object(
    .class1_fixture, source_kind = "bundled"
  ))
  path <- suppressWarnings(compute_centroid_path(
    points = as.data.frame(ena$points),
    time_var = "Period",
    id_var = "Speaker",
    group_vars = "Group",
    dimensions = c("SVD1", "SVD2", "SVD3"),
    order = c("TP1", "TP2", "TP3"),
    cohort_policy = "available",
    na_policy = "error",
    distance_space = "selected"
  ))
  group1_path <- path[path$Group == "G1", , drop = FALSE]
  group1_points <- as.data.frame(ena$points)
  group1_points <- group1_points[
    group1_points$Group == "G1", , drop = FALSE
  ]
  plot <- plot_centroid_trajectory_3d(
    group1_path,
    dimensions = c("SVD1", "SVD2", "SVD3"),
    group_cols = "Group",
    colors = c(G1 = "#2F2F2F"),
    unit_points = group1_points,
    code_nodes = as.data.frame(ena$rotation$nodes)
  )

  geometry <- attr(plot, "trajectory_axis_geometry")
  expect_equal(
    unname(geometry$lengths),
    c(1.3856488013, 0.9863265351, 1.1610389969),
    tolerance = 1e-9
  )
  expect_equal(
    unname(geometry$lower),
    c(-1.411006638, -1.638845048, -1.314478236),
    tolerance = 1e-9
  )
  expect_equal(
    unname(geometry$upper),
    c(1.485648801, 1.086326535, 1.261038997),
    tolerance = 1e-9
  )

  traces <- plotly::plotly_build(plot)$x$data
  roles <- vapply(traces, function(trace) {
    if (is.list(trace$meta)) {
      as.character(trace$meta$trajectory_role)
    } else {
      NA_character_
    }
  }, character(1L))
  arrow <- traces[[which(roles == "direction_arrows")]]
  expect_identical(arrow$type, "cone")
  expect_identical(arrow$anchor, "center")
  expect_equal(as.numeric(arrow$sizeref), 0.13)
  expect_equal(
    cbind(as.numeric(arrow$x), as.numeric(arrow$y), as.numeric(arrow$z)),
    rbind(
      c(0.454883864, -0.084149637, -0.303798806),
      c(0.068171695, -0.135623396, -0.239874076)
    ),
    tolerance = 1e-8
  )
  expect_identical(sum(roles == "coordinate_axis_shaft", na.rm = TRUE), 3L)
  expect_identical(sum(roles == "coordinate_axis_arrowhead", na.rm = TRUE), 3L)
  expect_identical(sum(roles == "coordinate_axis_label", na.rm = TRUE), 3L)
  expect_identical(sum(roles == "unit_points", na.rm = TRUE), 1L)
  expect_identical(sum(roles == "code_nodes", na.rm = TRUE), 1L)
})
