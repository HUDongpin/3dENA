library(testthat)

if (!exists("plot_network", mode = "function")) {
  network_candidates <- c(
    file.path("R", "build_network.R"),
    file.path("..", "..", "R", "build_network.R"),
    file.path("..", "R", "build_network.R")
  )
  network_file <- network_candidates[file.exists(network_candidates)][1L]
  if (is.na(network_file)) stop("Could not locate R/build_network.R")
  source(network_file)
}

make_network_fixture <- function(node_count) {
  labels <- paste0("N", seq_len(node_count))
  nodes <- cbind(
    MR1 = seq_len(node_count),
    SVD2 = sin(seq_len(node_count)),
    SVD3 = cos(seq_len(node_count))
  )
  rownames(nodes) <- labels
  list(nodes = nodes, adjacency = utils::combn(labels, 2L))
}

network_batch_traces <- function(plot) {
  traces <- plotly::plotly_build(plot)$x$data
  Filter(function(trace) {
    is.list(trace$meta) &&
      identical(trace$meta$ena3d_role, "network_edge_batch")
  }, traces)
}

test_that("invalid user network colors fail clearly and a valid retry succeeds", {
  skip_if_not_installed("scales")
  fixture <- make_network_fixture(3L)
  weights <- c(0.5, -0.25, 0.75)

  expect_error(
    build_network(
      fixture$nodes, weights, adjacency.key = fixture$adjacency,
      colors = c("not-a-real-color", "#224466")
    ),
    "Invalid network `colors` value"
  )
  expect_error(
    build_network(
      fixture$nodes, weights, adjacency.key = fixture$adjacency,
      colors = character()
    ),
    "one or two valid R color values"
  )

  valid <- build_network(
    fixture$nodes, weights, adjacency.key = fixture$adjacency,
    colors = c("#AA3300", "#0033AA")
  )
  expect_length(valid$network.edges.shapes, length(weights))
})

test_that("exchange-style base ena.nodes do not dispatch the broken matrix method", {
  skip_if_not_installed("rENA")
  fixture <- make_network_fixture(3L)
  nodes <- data.frame(
    code = structure(
      rownames(fixture$nodes), class = c("ena.metadata", "character")
    ),
    MR1 = structure(
      fixture$nodes[, "MR1"], class = c("ena.dimension", "numeric")
    ),
    SVD2 = structure(
      fixture$nodes[, "SVD2"], class = c("ena.dimension", "numeric")
    ),
    SVD3 = structure(
      fixture$nodes[, "SVD3"], class = c("ena.dimension", "numeric")
    ),
    check.names = FALSE
  )
  class(nodes) <- c("ena.nodes", "data.frame")

  expect_error(as.matrix(nodes), "invalid argument to unary operator")
  network <- build_network(
    nodes,
    network = c(0.5, -0.25, 0.75),
    adjacency.key = fixture$adjacency
  )
  expect_length(network$network.edges.shapes, 3L)
  expect_equal(nrow(network$nodes), 3L)
})

test_that("dense networks use bounded batched traces with per-edge hover", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("scales")
  fixture <- make_network_fixture(24L)
  edge_count <- ncol(fixture$adjacency)
  weights <- seq(-1, 1, length.out = edge_count)
  weights[seq(7L, edge_count, by = 7L)] <- 0
  expected_edges <- sum(is.finite(weights) & weights != 0)

  network <- build_network(
    fixture$nodes,
    weights,
    adjacency.key = fixture$adjacency,
    colors = c(pos = "#B23A2B", neg = "#174F8A")
  )
  expect_length(network$network.edges.shapes, expected_edges)

  plot <- plot_network(
    plotly::plot_ly(), network,
    legend.include.edges = TRUE,
    line_width = 1,
    max_width_bins = 6L
  )
  traces <- network_batch_traces(plot)

  # Two signs/colors times six finite width bins is a constant trace bound;
  # the old implementation produced one trace for every non-zero edge.
  expect_lte(length(traces), 12L)
  expect_lt(length(traces), expected_edges / 10)
  expect_equal(
    sum(vapply(traces, function(trace) trace$meta$edge_count, numeric(1L))),
    expected_edges
  )
  expect_equal(
    sum(vapply(traces, function(trace) {
      sum(is.finite(suppressWarnings(as.numeric(trace$x)))) / 2
    }, numeric(1L))),
    expected_edges
  )

  hover <- unique(stats::na.omit(unlist(lapply(traces, `[[`, "text"))))
  expect_length(hover, expected_edges)
  expect_true(all(grepl("Edge: ", hover, fixed = TRUE)))
  expect_true(all(grepl("<br>Weight: ", hover, fixed = TRUE)))

  legend_traces <- Filter(function(trace) isTRUE(trace$showlegend), traces)
  expect_equal(length(legend_traces), 2L)
  expect_setequal(
    vapply(legend_traces, `[[`, character(1L), "name"),
    c("Positive edges", "Negative edges")
  )
  expect_equal(
    length(unique(vapply(traces, `[[`, character(1L), "legendgroup"))),
    2L
  )
  expect_true(all(vapply(traces, function(trace) {
    is.finite(as.numeric(trace$line$width)) &&
      as.numeric(trace$line$width) >= 0
  }, logical(1L))))

  hidden_legend <- network_batch_traces(plot_network(
    plotly::plot_ly(), network,
    legend.include.edges = FALSE,
    line_width = 1,
    max_width_bins = 6L
  ))
  expect_false(any(vapply(hidden_legend, function(trace) {
    isTRUE(trace$showlegend)
  }, logical(1L))))
})

test_that("zero-weight edges never create network traces", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("scales")
  fixture <- make_network_fixture(5L)
  network <- build_network(
    fixture$nodes,
    rep(0, ncol(fixture$adjacency)),
    adjacency.key = fixture$adjacency
  )

  expect_length(network$network.edges.shapes, 0L)
  empty_plot <- plotly::plot_ly()
  original_attributes <- empty_plot$x$attrs
  plot <- plot_network(empty_plot, network, legend.include.edges = TRUE)
  expect_identical(plot$x$attrs, original_attributes)
})
