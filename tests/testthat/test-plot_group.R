source('../../R/plot_group.R')

test_that("selected-axis extraction rejects duplicate axes", {
  points <- data.frame(A = c(1, 2), B = c(3, 4))

  expect_error(
    ena3d_selected_axis_frame(points, "A", "A", "B"),
    "three distinct",
    ignore.case = TRUE
  )
})

test_that("confidence.interval.values are correct ", {
  library(plotly)
  test_data = load('./test_data/testing_data.Rdata')
  set <- get(test_data)
  
  # first.gruop.lineweights = as.matrix(set$line.weights$groupid$"1")
  # second.group.lineweights = as.matrix(set$line.weights$groupid$"2")
  # first.group.mean = as.vector(colMeans(first.gruop.lineweights))
  # second.group.mean = as.vector(colMeans(second.group.lineweights))
  # 
  # #points
  # first.group.points = as.matrix(set$points$groupid$`1`)
  # second.group.points = as.matrix(set$points$groupid$`2`)
  
  mplot<-ena_plot_group_3d(points = set$points,ena_plot = plot_ly(), confidence.interval = "box")
  confidence.interval.values = mplot$confidence.interval.values
  #confidence.interval.values in MR1 axis
  expect_equal(confidence.interval.values[1,1],-0.03848337)
  expect_equal(confidence.interval.values[2,1],0.03848337)

  # confidence.interval.values in SVD2 axis
  expect_equal(confidence.interval.values[1,2],-0.05830259 )
  expect_equal(confidence.interval.values[2,2],0.05830259)

  # confidence.interval.values in SVD3 axis
  expect_equal(confidence.interval.values[1,3],-0.04920936 )
  expect_equal(confidence.interval.values[2,3],0.04920936)
})

test_that("Correct coordinates of the conf interval are produced", {
  test_data = load('./test_data/testing_data.Rdata')
  set <- get(test_data)
  
  first.group.points = as.matrix(set$points$condition$`1`)
  mplot<-ena_plot_group_3d(points = first.group.points,ena_plot = plot_ly(), confidence.interval = "box")

  test_boxv1 = data.frame(
         X1 = c(0.006793768, 0.116809038, 0.116809038, 0.006793768,0.006793768),
         X2 = c(-0.08888428, -0.08888428, 0.08888428, 0.08888428,-0.08888428),
         X3 = c(-0.07360344, -0.07360344,-0.07360344, -0.07360344,-0.07360344)
  )
  expect_equal(mplot$boxv1,test_boxv1,tolerance=1e-6)
})

test_that("Confidence.interval.values are correctly produced in non-default axis setting", {
  test_data = load('./test_data/testing_data.Rdata')
  set <- get(test_data)
  
  x= 'SVD5'
  y='SVD6'
  z='MR1'
  
  first.group.points = as.matrix(set$points$condition$`1`)
  mplot<-ena_plot_group_3d(points = first.group.points,
                           ena_plot = plot_ly(), 
                           confidence.interval = "box",
                           x_axis = x,
                           y_axis = y,
                           z_axis = z)
  
  confidence.interval.values = mplot$confidence.interval.values

  # confidence.interval.values in X axis
  expect_equal(confidence.interval.values[1,1],-0.06300071)
  expect_equal(confidence.interval.values[2,1],0.06300071)
  
  # confidence.interval.values in Y axis
  expect_equal(confidence.interval.values[1,2],-0.05442957)
  expect_equal(confidence.interval.values[2,2],0.05442957)
  
  #confidence.interval.values in Z axis
  expect_equal(confidence.interval.values[1,3],0.006793768)
  expect_equal(confidence.interval.values[2,3],0.116809038)
})

test_that("centroid trace uses the selected axes", {
  test_data <- load('./test_data/testing_data.Rdata')
  set <- get(test_data)
  selected_axes <- c('SVD5', 'SVD6', 'MR1')
  points <- as.matrix(set$points$condition$`1`)

  plot <- ena_plot_group_3d(
    points = points,
    ena_plot = plot_ly(),
    x_axis = selected_axes[[1]],
    y_axis = selected_axes[[2]],
    z_axis = selected_axes[[3]]
  )
  trace <- plotly_build(plot)$x$data[[1]]

  expect_equal(
    as.numeric(c(trace$x, trace$y, trace$z)),
    as.numeric(colMeans(points[, selected_axes, drop = FALSE])),
    tolerance = 1e-10
  )
})

test_that("selected axes are isolated before centroid aggregation", {
  points <- data.frame(
    participant = c("A", "B", "C"),
    MR1 = c(100, 200, 300),
    SVD2 = c(-100, -200, -300),
    SVD3 = c(50, 60, 70),
    SVD5 = c(1, 4, 7),
    SVD6 = c(2, 5, 8),
    ALT = c(3, 6, 9),
    check.names = FALSE
  )

  plot <- ena_plot_group_3d(
    points = points,
    ena_plot = plotly::plot_ly(),
    x_axis = "SVD5",
    y_axis = "SVD6",
    z_axis = "ALT"
  )
  trace <- plotly::plotly_build(plot)$x$data[[1L]]

  expect_equal(as.numeric(trace$x), mean(points$SVD5))
  expect_equal(as.numeric(trace$y), mean(points$SVD6))
  expect_equal(as.numeric(trace$z), mean(points$ALT))
})

test_that("an ENA points object retains non-default dimension names", {
  test_data <- load('./test_data/testing_data.Rdata')
  set <- get(test_data)
  axes <- c("SVD5", "SVD6", "MR1")

  plot <- ena_plot_group_3d(
    points = set$points,
    ena_plot = plotly::plot_ly(),
    x_axis = axes[[1L]],
    y_axis = axes[[2L]],
    z_axis = axes[[3L]]
  )
  trace <- plotly::plotly_build(plot)$x$data[[1L]]
  numeric_points <- rENA::remove_meta_data(set$points)

  expect_equal(
    as.numeric(c(trace$x, trace$y, trace$z)),
    as.numeric(colMeans(numeric_points[, axes, drop = FALSE])),
    tolerance = 1e-10
  )
})

test_that("group means use one coherent finite selected-axis cohort", {
  points <- data.frame(
    A = c(1, 3, NA, 100),
    B = c(2, 4, 8, Inf),
    C = c(3, 5, 9, 100)
  )
  plot <- ena_plot_group_3d(
    points = points,
    ena_plot = plotly::plot_ly(),
    x_axis = "A", y_axis = "B", z_axis = "C"
  )
  trace <- plotly::plotly_build(plot)$x$data[[1L]]

  expect_equal(as.numeric(c(trace$x, trace$y, trace$z)), c(2, 3, 4))
  expect_identical(plot$excluded.nonfinite, 2L)
})

test_that("an unplottable group mean is skipped while later groups remain plottable", {
  existing <- data.frame(X = 0, Y = 0, Z = 0)
  plot <- plotly::add_trace(
    plotly::plot_ly(),
    data = existing,
    type = "scatter3d",
    x = ~X,
    y = ~Y,
    z = ~Z,
    name = "existing network"
  )

  no_complete_rows <- data.frame(
    A = c(1, 2, 3),
    B = c(4, 5, 6),
    C = c(NA_real_, NA_real_, NA_real_)
  )
  plot <- ena_plot_group_3d(
    ena_plot = plot,
    points = no_complete_rows,
    confidence.interval = "box",
    x_axis = "A", y_axis = "B", z_axis = "C",
    group_name = "unplottable"
  )

  partly_finite <- data.frame(
    A = c(1, 3, NA_real_, 100),
    B = c(2, 4, 8, Inf),
    C = c(3, 5, 9, 100)
  )
  plot <- ena_plot_group_3d(
    ena_plot = plot,
    points = partly_finite,
    x_axis = "A", y_axis = "B", z_axis = "C",
    group_name = "plottable"
  )

  traces <- plotly::plotly_build(plot)$x$data
  expect_length(traces, 2L)
  expect_identical(traces[[1L]]$name, "existing network")
  expect_identical(traces[[2L]]$name, "plottable")
  expect_equal(
    as.numeric(c(traces[[2L]]$x, traces[[2L]]$y, traces[[2L]]$z)),
    c(2, 3, 4)
  )
})


test_that("group plots reject duplicate selected axes", {
  points <- data.frame(A = 1:3, B = 2:4, C = 3:5)
  expect_error(
    ena_plot_group_3d(
      ena_plot = plotly::plot_ly(), points = points,
      x_axis = "A", y_axis = "A", z_axis = "C"
    ),
    "Three distinct axis names"
  )
})

test_that("constant selected axes suppress an unestimable confidence box", {
  points <- data.frame(A = c(1, 1), B = c(2, 2), C = c(3, 3))
  plot <- ena_plot_group_3d(
    points = points,
    ena_plot = plotly::plot_ly(),
    confidence.interval = "box",
    x_axis = "A", y_axis = "B", z_axis = "C"
  )

  expect_null(plot$confidence.interval.values)
  expect_null(plot$boxv1)
})
