ena3d_overall_group_count <- function(points, group_var) {
  if (!is.data.frame(points) || !is.character(group_var) ||
      length(group_var) != 1L || !group_var %in% names(points)) {
    stop("A valid group column is required for the Overall plot.", call. = FALSE)
  }

  values <- points[[group_var]]
  length(unique(values[!is.na(values)]))
}

ena3d_overall_color_map <- function(group_values, configured_colors = NULL,
                                    fallback_colors = character()) {
  group_values <- unique(as.character(group_values))
  group_values <- group_values[!is.na(group_values) & nzchar(group_values)]
  color_map <- character()
  if ((is.matrix(configured_colors) || is.data.frame(configured_colors)) &&
      all(c("color", "group") %in% colnames(configured_colors))) {
    configured <- as.data.frame(
      configured_colors, stringsAsFactors = FALSE, optional = TRUE
    )
    keep <- !is.na(configured$group) & nzchar(configured$group) &
      !is.na(configured$color) & nzchar(configured$color)
    configured <- configured[keep & !duplicated(configured$group), , drop = FALSE]
    color_map <- stats::setNames(configured$color, configured$group)
  }
  missing_groups <- setdiff(group_values, names(color_map))
  if (length(missing_groups)) {
    if (length(fallback_colors) < length(missing_groups)) {
      fallback_colors <- ena3d_palette(length(missing_groups))
    }
    color_map <- c(
      color_map,
      stats::setNames(
        as.character(fallback_colors[seq_along(missing_groups)]),
        missing_groups
      )
    )
  }
  color_map[group_values]
}


ena3d_prepare_overall_points <- function(points, group_var,
                                         selected_groups = NULL,
                                         hover_var = group_var) {
  if (!is.data.frame(points) || !is.character(group_var) ||
      length(group_var) != 1L || !group_var %in% names(points)) {
    stop("A valid group column is required for the Overall plot.", call. = FALSE)
  }
  if (!is.character(hover_var) || length(hover_var) != 1L ||
      !hover_var %in% names(points)) {
    stop("A valid hover column is required for the Overall plot.", call. = FALSE)
  }

  # Convert before selecting. This deliberately uses base data-frame semantics
  # so a data.table cannot interpret a character column name as a literal value.
  frame <- as.data.frame(points, stringsAsFactors = FALSE, optional = TRUE)
  selected_groups <- unlist(selected_groups, use.names = FALSE)
  group_labels <- ena3d_group_value_labels(frame[[group_var]])
  keep <- if (is.null(selected_groups) || !length(selected_groups)) {
    rep(FALSE, nrow(frame))
  } else {
    ena3d_group_value_match(frame[[group_var]], selected_groups)
  }
  frame <- frame[keep, , drop = FALSE]
  frame[[group_var]] <- group_labels[keep]
  frame$.ena3d_group_hover <- as.character(frame[[hover_var]])
  frame
}


ena3d_add_overall_points_trace <- function(plot, points, group_var,
                                            dimensions, colors,
                                            hover_label = "Unit") {
  if (!is.data.frame(points) || !group_var %in% names(points) ||
      !".ena3d_group_hover" %in% names(points)) {
    stop("Prepared Overall point data are required.", call. = FALSE)
  }
  if (!is.character(dimensions) || length(dimensions) != 3L ||
      !all(dimensions %in% names(points))) {
    stop("Three valid Overall plot dimensions are required.", call. = FALSE)
  }

  groups <- unique(as.character(points[[group_var]]))
  if (!length(groups)) return(plot)

  color_names <- names(colors)
  if (is.null(color_names) || !any(nzchar(color_names))) {
    if (length(colors) < length(groups)) {
      stop("One Overall color is required for every displayed group.", call. = FALSE)
    }
    color_map <- stats::setNames(as.character(colors[seq_along(groups)]), groups)
  } else {
    color_map <- stats::setNames(as.character(colors), as.character(color_names))
    missing_colors <- setdiff(groups, names(color_map))
    if (length(missing_colors)) {
      stop(sprintf(
        "Overall colors are missing for groups: %s",
        paste(missing_colors, collapse = ", ")
      ), call. = FALSE)
    }
  }

  for (group in groups) {
    group_points <- points[
      !is.na(points[[group_var]]) & as.character(points[[group_var]]) == group,
      , drop = FALSE
    ]
    plot <- plotly::add_trace(
      plot,
      data = group_points,
      x = stats::reformulate(dimensions[[1L]]),
      y = stats::reformulate(dimensions[[2L]]),
      z = stats::reformulate(dimensions[[3L]]),
      text = ~.ena3d_group_hover,
      type = "scatter3d",
      mode = "markers",
      name = group,
      legendgroup = group,
      hovertemplate = paste0(
        "X: %{x}<br>Y: %{y}<br>Z: %{z}<br>",
        hover_label, ": %{text}<extra></extra>"
      ),
      marker = list(
        color = unname(color_map[[group]]),
        size = 5,
        line = list(width = 0)
      )
    )
  }
  plot
}


source('./app_utils.R')
ena_overall_plot_output <-  function(input, output, session,
                                        data,
                                        state,
                                        scaled_points,
                                        scaled_nodes,
                                        group_var=NULL,
                                        camera=NULL
) {
  # print('module server go with id')
  # print(id)
  x_axis <- reactive({
    tilde_var_or_null(input$x)
  })
  y_axis <- reactive({
    tilde_var_or_null(input$y)
  })
  z_axis <- reactive({
    tilde_var_or_null(input$z)
  })

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
  get_select_group= reactive({
    if(data$model_tab_clicked ==TRUE){
      input$select_group
    }else{
      data$ena_groups
    }
  })
  
  get_colors = reactive({
    group_values <- unique(ena3d_group_value_labels(
      scaled_points()[[data$ena_groupVar[[1L]]]]
    ))
    ena3d_overall_color_map(
      group_values,
      configured_colors = data$group_colors,
      fallback_colors = state$color_list
    )
  })
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
  generate_plot = reactive({
    
    # req(initialized(),cancelOutput = TRUE)
    main_plot <- plot_ly(source = "overall")
    req(data$initialized,cancelOutput = TRUE)
    req(state$render_overall(),cancelOutput = TRUE)
    req(state$is_app_initialized,cancelOutput = TRUE)
    req(!is.null(state$ena_obj),cancelOutput = TRUE)
    req(input$x, input$y, input$z)
    req(
      ena3d_axes_are_distinct(input$x, input$y, input$z),
      cancelOutput = TRUE
    )
    if(state$render_overall() == FALSE){
      return(NULL)
    }
    
    # Create an empty plot
    #browser()
    # Add the first trace (from points_plot)
    my_points <- scaled_points()
    colname <- data$ena_groupVar[[1L]]
    hover_var <- if (length(data$ena_groupVar) >= 2L &&
                     !is.na(data$ena_groupVar[[2L]]) &&
                     nzchar(data$ena_groupVar[[2L]]) &&
                     data$ena_groupVar[[2L]] %in% names(my_points)) {
      data$ena_groupVar[[2L]]
    } else {
      colname
    }
    
    # Fix the bug of not showing edges when the dataset is loaded and the user hasn't open the model page      
    selected_groups <- get_select_group()
    filter_points <- ena3d_prepare_overall_points(
      my_points, colname, selected_groups, hover_var
    )
    
    
    main_plot <- ena3d_add_overall_points_trace(
      main_plot,
      points = filter_points,
      group_var = colname,
      dimensions = c(input$x, input$y, input$z),
      colors = get_colors(),
      hover_label = hover_var
    )
    
    
    my_nodes <- scaled_nodes()
    # Add the second trace (from nodes_plot)
    main_plot <- add_trace(main_plot, data = my_nodes, x = x_axis(), y = y_axis(), z = z_axis(),
                           type = 'scatter3d', mode = "markers", name = "Codes",
                           marker = list(
                             color ='rgb(77,77,77)',
                             size = abs(my_nodes$weight),
                             line = list(
                               width = 0
                             )
                             #,name = labels[i] #rownames(nodes)[i]
                           ))
    t <- list(
      family = "sans serif",
      size = 14,
      color = toRGB("grey50"))
    
    main_plot <-  add_text(main_plot,data=my_nodes,x = x_axis(), y = y_axis(), z = z_axis(),
                           text = ~code,
                           textfont=t,
                           textposition = "top right")
    
    # Customize the layout and appearance of the combined plot
    main_plot <- layout(main_plot,
                        scene = list(
                          xaxis = ena3d_plotly_axis_layout(input$x, input$show_grid, input$show_zeroline),
                          yaxis = ena3d_plotly_axis_layout(input$y, input$show_grid, input$show_zeroline),
                          zaxis = ena3d_plotly_axis_layout(input$z, input$show_grid, input$show_zeroline)
                        ),
                        showlegend = TRUE)
    if(length(selected_groups) == 0){
      return(main_plot)
    }
    # browser()
    # Generate Edges
    mean_in_groups<-get_mean_group_lineweights_in_groups(state$ena_obj,data$ena_groupVar[1],selected_groups)
    network <- build_network(scaled_nodes(),
                             network=mean_in_groups,
                             adjacency.key=state$ena_obj$rotation$adjacency.key)
    
    main_plot <- plot_network(main_plot,
                              network,
                              legend.include.edges = F,
                              x_axis=input$x,
                              y_axis=input$y,
                              z_axis=input$z,
                              line_width = input$line_width)
    
    # if(!is.null(camera)){
    #   print('set cam')
    #   main_plot %>% layout(scene= list(camera=camera))
    # }
    # camera = list(
    #   eye=list(x=0., y=0., z=2.5)
    # )
    main_plot <- layout(
      main_plot,
      title = "Overall ENA model",
      scene = list(
        camera = camera(),
        uirevision = paste0("overall-camera-", input$camera_position)
      )
    )
    main_plot
  })
  
  # reactive(generate_plot())
  # output$ena_points_plot <- renderPlotly({
  #   print('generate plot ena_points_plot')
  #   # generate_plot()
  #   plot_ly(data.frame(x=c(1,2,3),y=c(1,2,3)))
  # })
  output$ena_overall_plot <- renderPlotly({
    comparison_plot <- generate_plot()

    comparison_plot <- add_3d_axis_based_on_user_selection(comparison_plot)

    comparison_plot <- ena3d_apply_plotly_typography(comparison_plot)

    event_register(comparison_plot, 'plotly_relayout')
    # click_data <- event_data("plotly_click", source = "ena_points_plot")
    #
    # if (!is.null(click_data)) {
    #   print(str(click_data))
    #   # idx <- click_data$pointNumber + 1
    #   # data[idx, "col"] <- "red"
    # }


    comparison_plot
   
  })
  
  # observeEvent(event_data(event = "plotly_relayout",source='plot_correlation'),{
  #   clicked <- event_data(event = "plotly_relayout",
  #                         source = "plot_correlation")
  #   if (!is.null(clicked)) {
  #     print(clicked)
  #   }
  # })
  
}
