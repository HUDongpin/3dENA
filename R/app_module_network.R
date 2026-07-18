ena3d_network_axis_layout <- function(title, showgrid = TRUE,
                                      zeroline = TRUE, nticks = 4L) {
  ena3d_plotly_axis_layout(
    title,
    showgrid = showgrid,
    zeroline = zeroline,
    nticks = nticks
  )
}

ena3d_group_selectors_ready <- function(input_values, selectors) {
  if (!length(selectors)) {
    return(FALSE)
  }

  required_fields <- c(
    "color_selector_id",
    "points_toggle_id",
    "show_mean_btn_id",
    "show_conf_int_btn_id"
  )
  all(vapply(selectors, function(selector) {
    input_ids <- unname(unlist(selector[required_fields], use.names = FALSE))
    length(input_ids) == length(required_fields) &&
      all(nzchar(input_ids)) &&
      all(vapply(input_ids, function(input_id) {
        !is.null(input_values[[input_id]])
      }, logical(1)))
  }, logical(1)))
}


ena3d_network_group_points <- function(points, group_var, group_values) {
  if (!is.data.frame(points) || !is.character(group_var) ||
      length(group_var) != 1L || !group_var %in% names(points)) {
    stop("A valid group column is required for Network points.", call. = FALSE)
  }
  ena3d_subset_rows_preserve_column_types(
    points,
    which(ena3d_group_value_match(points[[group_var]], group_values))
  )
}


ena3d_network_selection_target <- function(selection, group_var) {
  decoded <- ena3d_network_selector_decode(selection)
  if (is.null(decoded) || identical(decoded$type, "none")) {
    return(NULL)
  }
  if (identical(decoded$type, "group")) {
    if (!is.character(group_var) || length(group_var) != 1L ||
        is.na(group_var) || !nzchar(group_var)) {
      stop("A group Network selection requires a valid group column.",
           call. = FALSE)
    }
    return(list(type = "group", variable = group_var, value = decoded$value))
  }
  if (identical(decoded$type, "unit")) {
    return(list(type = "unit", variable = "ENA_UNIT", value = decoded$value))
  }
  NULL
}


source('./app_utils.R')
ena_network_plot_output <-  function(input, output, session,
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
      points <- get_points_with_group(all_points,group_var,group_name)
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
                                z_axis = input$z,
                                group_name=group_name)
    }
    
    return(plot)
  }
  validate_confidence_interval <- function(points,x,y,z){
    all(vapply(c(x, y, z), function(axis) {
      values <- points[[axis]]
      values <- values[is.finite(values)]
      length(values) >= 2L && is.finite(stats::sd(values)) &&
        stats::sd(values) > 0
    }, logical(1)))
  }
  generate_plot = reactive({
    #browser()
    # req(initialized(),cancelOutput = TRUE)
    main_plot <- plot_ly(source = "network")
    req(data$initialized,cancelOutput = TRUE)
    req(state$is_app_initialized,cancelOutput = TRUE)
    req(!is.null(state$ena_obj),cancelOutput = TRUE)
    req(input$x, input$y, input$z)
    req(
      ena3d_axes_are_distinct(input$x, input$y, input$z),
      cancelOutput = TRUE
    )
    if(state$render_network_plot() == FALSE){
      return(NULL)
    }
    if (!ena3d_group_selectors_ready(input, data$group_selectors)) {
      return(NULL)
    }
    # Create an empty plot
    #browser()
    # Add the first trace (from points_plot)
    my_points <- scaled_points()
    colname = data$ena_groupVar[1]
    network_target <- ena3d_network_selection_target(
      input$network_selector, colname
    )
    
    # Fix the bug of not showing edges when the dataset is loaded and the user hasn't open the model page      
    
    selected_groups <- get_select_group()
    filter_points <- ena3d_network_group_points(
      my_points, colname, selected_groups
    )
    filter_points[,colname]<-as.character(as.data.frame(filter_points)[,colname])
    
    n <- length(data$ena_groups)
    for(i in 1:n){
      
      
      selected_group<-data$ena_groups[i]
      #browser()
      group_selector = data$group_selectors[[selected_group]]
      
      
      #browser()
      color <- get_group_color(data$group_colors,'group',selected_group)
      
      points_toggle_id <- isTRUE(input[[group_selector['points_toggle_id']]])
      
      if(points_toggle_id){
        filter_points <- ena3d_network_group_points(
          my_points, colname, selected_group
        )
        
        main_plot <- add_trace(main_plot,
                               data = filter_points,
                               
                               x = x_axis(),
                               y = y_axis(),
                               z = z_axis(),
                               #color = tilde_group_var_or_null(),
                               
                               #text=get_secondary_groups(),
                               # test=get_secondary_groups(),
                               type = 'scatter3d',
                               mode = "markers",
                               text=~ENA_UNIT,
                               # name = "Points",
                               hovertemplate = "X: %{x}<br>Y: %{y}<br>Z: %{z}<br>Unit : %{text}<br>",
                               name = selected_group,
                               marker = list(
                                 size = 5,
                                 line = list(
                                   width = 0
                                 ),
                                 color=color
                                 # ,name = labels[i] #rownames(nodes)[i]
                               ))
      }
      
      is_mean_shown <- isTRUE(input[[group_selector['show_mean_btn_id']]])
      is_conf_int_shown <- isTRUE(input[[group_selector['show_conf_int_btn_id']]])
      #browser()
      main_plot<-add_mean_to_plot(main_plot,
                                  all_points = my_points,
                                  group_name = selected_group,
                                  group_var = colname,
                                  show_mean = is_mean_shown,
                                  show_conf_int = is_conf_int_shown,
                                  color = color)
      
    }
    
    if(!is.null(network_target)){
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
      if(identical(network_target$type, "group")){
        lineweights<-get_mean_group_lineweights_in_groups(
          state$ena_obj, network_target$variable, network_target$value
        )
        color <- get_group_color(
          data$group_colors, 'group', network_target$value
        )
      }else{
        #browser()
        lineweights<-get_mean_group_lineweights_in_groups(
          state$ena_obj, network_target$variable, network_target$value
        )
        DT <- ena3d_network_group_points(
          state$ena_obj$points, network_target$variable, network_target$value
        )
        group <- ena3d_group_value_labels(
          as.data.frame(DT)[[data$ena_groupVar[[1L]]]]
        )
        group <- unique(group[!is.na(group)])
        color <- get_group_color(data$group_colors,'group',head(group, 1L))
      }
      
      #browser()
      network <- build_network(scaled_nodes(),
                               network=lineweights,
                               adjacency.key=state$ena_obj$rotation$adjacency.key,
                               colors=c(color,color))

      main_plot <- plot_network(main_plot,
                                network,
                                legend.include.edges = F,
                                x_axis=input$x,
                                y_axis=input$y,
                                z_axis=input$z,
                                line_width = input$line_width
                                )
    }
    
    # my_nodes <- scaled_nodes()
    # # Add the second trace (from nodes_plot)
    # main_plot <- add_trace(main_plot, data = my_nodes, x = x_axis(), y = y_axis(), z = z_axis(),
    #                        type = 'scatter3d', mode = "markers", name = "Codes",
    #                        marker = list(
    #                          color ='rgb(77,77,77)',
    #                          size = abs(my_nodes$weight),
    #                          line = list(
    #                            width = 0
    #                          )
    #                          #,name = labels[i] #rownames(nodes)[i]
    #                        ))
    # t <- list(
    #   family = "sans serif",
    #   size = 14,
    #   color = toRGB("grey50"))
    # 
    # main_plot <-  add_text(main_plot,data=my_nodes,x = x_axis(), y = y_axis(), z = z_axis(),
    #                        text = ~code,
    #                        textfont=t,
    #                        textposition = "top right")
    # 
    # # Customize the layout and appearance of the combined plot
    # main_plot <- layout(main_plot,
    #                     scene = list(xaxis = list(title = input$x,showgrid=input$show_grid,zeroline=input$show_zeroline),
    #                                  yaxis = list(title = input$y,showgrid=input$show_grid,zeroline=input$show_zeroline),
    #                                  zaxis = list(title = input$z,showgrid=input$show_grid,zeroline=input$show_zeroline)),
    #                     showlegend = TRUE)
    # if(length(selected_groups) == 0){
    #   return(main_plot)
    # }
    # browser()
    # Generate Edges
    # mean_in_groups<-get_mean_group_lineweights_in_groups(state$ena_obj,data$ena_groupVar[1],selected_groups)
    # network <- build_network(scaled_nodes(),
    #                          network=mean_in_groups,
    #                          adjacency.key=state$ena_obj$rotation$adjacency.key)
    # 
    # main_plot <- plot_network(main_plot,
    #                           network,
    #                           legend.include.edges = F,
    #                           x_axis=input$x,
    #                           y_axis=input$y,
    #                           z_axis=input$z,
    #                           line_width = input$line_width)
    
    # if(!is.null(camera)){
    #   print('set cam')
    #   main_plot %>% layout(scene= list(camera=camera))
    # }
    # camera = list(
    #   eye=list(x=0., y=0., z=2.5)
    # )
    #print(camera())
    #browser()
    # Let Plotly inspect every completed trace (points, nodes, edges, means,
    # confidence boxes, and optional axis arrows). A range derived from the raw
    # unscaled ENA object clips traces whenever the display scale is increased.
    axx <- ena3d_network_axis_layout(
      input$x, input$show_grid, input$show_zeroline
    )
    axy <- ena3d_network_axis_layout(
      input$y, input$show_grid, input$show_zeroline
    )
    axz <- ena3d_network_axis_layout(
      input$z, input$show_grid, input$show_zeroline
    )
    #browser()
    network_title <- if (is.null(network_target)) {
      "Network view"
    } else {
      target_label <- if (identical(network_target$type, "group")) {
        "Group"
      } else {
        "Unit"
      }
      paste0("Network (", target_label, "): ", network_target$value)
    }
    main_plot <- layout(
      main_plot,
      title = network_title,
      scene = list(
        camera = camera(), xaxis = axx, yaxis = axy, zaxis = axz,
        uirevision = paste0("network-camera-", input$camera_position)
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
  output$ena_network_plot <- renderPlotly({
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
