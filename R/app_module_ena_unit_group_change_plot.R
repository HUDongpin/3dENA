ena3d_change_axis_layout <- function(title, showgrid = TRUE,
                                     zeroline = TRUE) {
  ena3d_plotly_axis_layout(
    title,
    showgrid = showgrid,
    zeroline = zeroline
  )
}


ena3d_change_group_values <- function(points, group_var) {
  if (is.null(group_var) || length(group_var) != 1L || !nzchar(group_var) ||
      !group_var %in% names(points)) {
    stop("The selected Change variable is not present in the ENA points.")
  }
  values <- points[[group_var]]
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

ena3d_change_group_points <- function(points, group_var, group_value) {
  if (is.null(group_var) || length(group_var) != 1L || !nzchar(group_var) ||
      !group_var %in% names(points)) {
    stop("The selected Change variable is not present in the ENA points.")
  }
  keep <- ena3d_group_value_match(points[[group_var]], group_value)
  ena3d_subset_rows_preserve_column_types(points, keep)
}

ena3d_change_cache_key <- function(dataset_id, group_var, axes, scale_factor,
                                   line_width, show_grid, show_zeroline,
                                   axis_arrows, show_mean, show_confidence_interval) {
  if (is.null(dataset_id)) dataset_id <- ""
  list(
    dataset_id = as.character(dataset_id),
    group_var = as.character(group_var),
    axes = as.character(axes),
    scale_factor = as.numeric(scale_factor),
    line_width = as.numeric(line_width),
    show_grid = isTRUE(show_grid),
    show_zeroline = isTRUE(show_zeroline),
    axis_arrows = as.logical(axis_arrows),
    show_mean = isTRUE(show_mean),
    show_confidence_interval = isTRUE(show_confidence_interval)
  )
}

ena3d_tag_change_cache <- function(cache, key) {
  attr(cache, "ena3d_change_cache_key") <- key
  cache
}

ena3d_change_cache_is_valid <- function(cache, key) {
  length(cache) > 0L && identical(attr(cache, "ena3d_change_cache_key"), key)
}

ena3d_validate_change_cardinality <- function(values,
                                               max_levels = getOption(
                                                 "ena3d.max_change_levels",
                                                 100L
                                               )) {
  max_levels <- as.integer(max_levels)
  if (!is.finite(max_levels) || max_levels < 1L) {
    stop("The Change cardinality limit must be a positive integer.")
  }
  if (length(values) > max_levels) {
    stop(sprintf(
      paste(
        "The selected Change variable has %d values; the public-app limit is %d.",
        "Choose a lower-cardinality time or condition variable."
      ),
      length(values), max_levels
    ))
  }
  invisible(values)
}

ena3d_change_lru_get <- function(cache, key) {
  if (!is.list(cache)) cache <- list()
  match_index <- which(vapply(cache, function(entry) {
    is.list(entry) && identical(entry$key, key)
  }, logical(1)))
  if (!length(match_index)) {
    return(list(hit = FALSE, plot = NULL, cache = cache))
  }
  index <- match_index[[1L]]
  entry <- cache[[index]]
  # Move the hit to the end so eviction removes the least recently used plot.
  cache <- c(cache[-index], list(entry))
  list(hit = TRUE, plot = entry$plot, cache = cache)
}

ena3d_change_lru_put <- function(cache, key, plot,
                                 max_entries = getOption(
                                   "ena3d.max_change_cache_entries", 8L
                                 )) {
  max_entries <- as.integer(max_entries)
  if (!is.finite(max_entries) || max_entries < 1L) {
    stop("The Change cache limit must be a positive integer.")
  }
  if (!is.list(cache)) cache <- list()
  cache <- Filter(function(entry) {
    !(is.list(entry) && identical(entry$key, key))
  }, cache)
  cache[[length(cache) + 1L]] <- list(key = key, plot = plot)
  if (length(cache) > max_entries) {
    cache <- tail(cache, max_entries)
  }
  cache
}

ena_unit_group_change_plot_output <- function(input,output,session,
                                              rv_data,
                                              state,
                                              scaled_points,
                                              scaled_nodes){
  x_axis <- reactive({
    tilde_var_or_null(input$x)
  })
  y_axis <- reactive({
    tilde_var_or_null(input$y)
  })
  z_axis <- reactive({
    tilde_var_or_null(input$z)
  })
  change_cache_key <- reactive({
    req(input$group_change_var, input$x, input$y, input$z)
    ena3d_change_cache_key(
      dataset_id = rv_data$dataset_id,
      group_var = input$group_change_var,
      axes = c(input$x, input$y, input$z),
      scale_factor = input$scale_factor,
      line_width = input$line_width,
      show_grid = input$show_grid,
      show_zeroline = input$show_zeroline,
      axis_arrows = c(
        x = input$show_x_axis_arrow,
        y = input$show_y_axis_arrow,
        z = input$show_z_axis_arrow
      ),
      show_mean = input$group_change_show_mean,
      show_confidence_interval = input$group_change_show_confidence_interval
    )
  })
  observeEvent(list(input$group_change_var, rv_data$dataset_id), {
    req(rv_data$initialized, state$ena_obj, input$group_change_var)
    values <- ena3d_change_group_values(state$ena_obj$points, input$group_change_var)
    ena3d_validate_change_cardinality(values)
    updateSelectInput(
      session,
      "unit_change",
      choices = as.character(values),
      selected = if (length(values)) as.character(values[[1L]]) else character()
    )
    rv_data$unit_group_change_plots <- list()
  }, ignoreInit = TRUE)
  camera = reactive({
    pos = input$camera_position
    if(pos =='default'){
      camera = list(eye=list(x=1.25, y=1.25, z=1.25))
    }else if(pos =='x_y'){
      camera = list(eye=list(x=0, y=0, z=2.5),up=list(x=0,y=1,z=0))
    }else if(pos == 'x_z'){
      camera = list(eye=list(x=0., y=-2.5, z=0.),up=list(x=0,y=0,z=1))
    }else if(pos =='y_z'){
      camera = list(eye=list(x=2.5, y=0., z=0.),up=list(x=0,y=0,z=1))
    }else if(pos =='y_x'){
      camera = list(eye=list(x=0, y=0, z=-2.5),up=list(x=1,y=0,z=0))
    }else if(pos =='z_x'){
      camera = list(eye=list(x=0, y=2.5, z=0),up=list(x=1,y=0,z=0))
    }else if(pos =='z_y'){
      camera = list(eye=list(x=-2.5, y=0, z=0),up=list(x=0,y=1,z=0))
    }
    # list(eye=camera_eye(),up=list(x=0,y=1,z=0))
  })
  
  validate_confidence_interval <- function(points,x,y,z){
    all(vapply(c(x, y, z), function(axis) {
      values <- points[[axis]]
      values <- values[is.finite(values)]
      length(values) >= 2L && is.finite(stats::sd(values)) && stats::sd(values) > 0
    }, logical(1)))
  }
  add_mean_to_plot <- function(plot,
                               all_points,
                               group_name,
                               group_var,
                               show_mean,
                               show_conf_int,
                               color){
    #browser()
    conf <-'none'
    if(show_conf_int){
      conf <- 'box'
    }
    
    if(show_mean){
      points <- ena3d_change_group_points(all_points, group_var, group_name)
      points <-remove_meta_data(points)
      
      #check confidence interval
      #browser()
      # if(sd(points[,input$x])==0 || sd(points[,input$y])==0||sd(points[,input$z])==0 ||
      #    is.na(sd(points[,input$x])) || is.na(sd(points[,input$y]))||is.na(sd(points[,input$z]))){
      #   conf <- 'none'
      # }
      if(!validate_confidence_interval(points,input$x,input$y,input$z)){
        conf <- 'none'
      }
      plot <- ena_plot_group_3d(plot,points = points,
                                colors=color,
                                confidence.interval=conf,
                                x_axis = input$x,
                                y_axis = input$y,
                                z_axis = input$z)
    }
   
    return(plot)
  }
  add_3d_axis_based_on_user_selection = function(plot){
    if(input$show_x_axis_arrow){
      plot<-add_x_3d_axis(plot)
    }
    if(input$show_y_axis_arrow){
      plot<-add_y_3d_axis(plot)
    }
    if(input$show_z_axis_arrow){
      plot<-add_z_3d_axis(plot)
    }
    
    # plot <- layout(plot,title='X-Y',scene= list(camera=list(eye=list(x=0., y=0., z=-2.5))))
    plot
    
  }
  
  # Build only the currently requested value. Caching complete Plotly widgets
  # for every metadata level made high-cardinality variables unbounded in both
  # latency and per-session memory.
  make_unit_group_change_plot <- function(current_group) {
    withProgress(message = "Making Change plot", value = 0, {
      req(
        ena3d_axes_are_distinct(input$x, input$y, input$z),
        cancelOutput = TRUE
      )
      group_values <- ena3d_change_group_values(
        state$ena_obj$points, input$group_change_var
      )
      ena3d_validate_change_cardinality(group_values)
      if (!as.character(current_group) %in% as.character(group_values)) {
        stop("The selected Change value is no longer available.")
      }
      incProgress(0.15, detail = "Preparing nodes")
      my_nodes <- scaled_nodes()
      text_style <- list(
        family = "sans serif", size = 14, color = toRGB("grey50")
      )
      mplot <- plot_ly(source = "change")
      mplot <- add_trace(
        mplot, data = my_nodes, x = x_axis(), y = y_axis(), z = z_axis(),
        type = "scatter3d", mode = "markers", name = "Codes",
        marker = list(
          color = "rgb(77,77,77)", size = abs(my_nodes$weight),
          line = list(width = 0)
        )
      )
      mplot <- add_text(
        mplot, data = my_nodes, x = x_axis(), y = y_axis(), z = z_axis(),
        text = ~code, textfont = text_style, textposition = "top right"
      )

      incProgress(0.25, detail = "Calculating the selected network")
      c_network <- build_network(
        scaled_nodes(),
        network = get_mean_group_lineweights(
          state$ena_obj, input$group_change_var, current_group
        ),
        adjacency.key = state$ena_obj$rotation$adjacency.key
      )
      mplot <- plot_network(
        mplot, c_network, legend.include.edges = FALSE,
        x_axis = input$x, y_axis = input$y, z_axis = input$z,
        line_width = input$line_width
      )
      mplot <- add_3d_axis_based_on_user_selection(mplot)
      mplot <- add_mean_to_plot(
        mplot, all_points = scaled_points(), group_name = current_group,
        group_var = input$group_change_var,
        show_mean = input$group_change_show_mean,
        show_conf_int = input$group_change_show_confidence_interval,
        color = "red"
      )
      incProgress(0.45, detail = "Finalizing the selected plot")
      plot <- layout(
        mplot,
        title = list(
          text = paste(input$group_change_var, current_group),
          pad = list(t = 50, b = 10, l = 10, r = 10)
        ),
        scene = list(
          camera = camera(),
          uirevision = paste0("change-camera-", input$camera_position),
          xaxis = ena3d_change_axis_layout(
            input$x, input$show_grid, input$show_zeroline
          ),
          yaxis = ena3d_change_axis_layout(
            input$y, input$show_grid, input$show_zeroline
          ),
          zaxis = ena3d_change_axis_layout(
            input$z, input$show_grid, input$show_zeroline
          )
        ),
        showlegend = TRUE
      )
      ena3d_apply_plotly_typography(plot)
    })
  }
  
  "
      When the user change the axies or line width or scale factor,
      redraw the unit group change plot
      "
  observeEvent(list(
    rv_data$dataset_id,
    input$x,
    input$y,
    input$z,
    input$group_change_var,
    input$line_width,
    input$scale_factor,
    input$show_grid,
    input$show_zeroline,
    input$show_x_axis_arrow,
    input$show_y_axis_arrow,
    input$show_z_axis_arrow,
    input$group_change_show_mean,
    input$group_change_show_confidence_interval
  ), {
    if (isTRUE(rv_data$initialized)) {
      rv_data$unit_group_change_plots <- list()
    }
  }, ignoreInit = TRUE)
  
  
  
  "
        The plot in the model->change tab
  "
  output$ena_unit_group_change_plot <- renderPlotly({
    req(rv_data$initialized,cancelOutput = TRUE)
    req(input$x, input$y, input$z, input$group_change_var, input$unit_change)
    current_key <- c(
      change_cache_key(),
      list(
        group_value = as.character(input$unit_change),
        camera_position = as.character(input$camera_position)
      )
    )
    cached <- ena3d_change_lru_get(
      isolate(rv_data$unit_group_change_plots), current_key
    )
    isolate(rv_data$unit_group_change_plots <- cached$cache)
    if (isTRUE(cached$hit)) {
      p <- cached$plot
    } else {
      p <- make_unit_group_change_plot(input$unit_change)
      isolate({
        rv_data$unit_group_change_plots <- ena3d_change_lru_put(
          rv_data$unit_group_change_plots, current_key, p
        )
      })
    }
    validate(need(!is.null(p), "No Change plot is available for the selected value."))
    p
  })
}
