ena3d_comparison_axis_layout <- function(title, showgrid = TRUE,
                                         zeroline = TRUE) {
  ena3d_plotly_axis_layout(
    title,
    showgrid = showgrid,
    zeroline = zeroline
  )
}

ena3d_normalize_comparison_color <- function(value, fallback) {
  normalize_one <- function(candidate) {
    if (!is.character(candidate) || length(candidate) != 1L ||
        is.na(candidate) || !nzchar(trimws(candidate))) {
      return(NULL)
    }
    rgba <- tryCatch(
      grDevices::col2rgb(trimws(candidate), alpha = TRUE)[, 1L],
      error = function(...) NULL
    )
    if (is.null(rgba)) return(NULL)
    if (rgba[[4L]] < 255L) {
      return(grDevices::rgb(
        rgba[[1L]], rgba[[2L]], rgba[[3L]], rgba[[4L]],
        maxColorValue = 255
      ))
    }
    grDevices::rgb(
      rgba[[1L]], rgba[[2L]], rgba[[3L]], maxColorValue = 255
    )
  }

  normalized <- normalize_one(value)
  if (!is.null(normalized)) return(normalized)
  normalized <- normalize_one(fallback)
  if (!is.null(normalized)) return(normalized)
  # The module passes compile-time defaults, but retain a final safe value if
  # this helper is reused with a malformed fallback.
  "#000000"
}

ena3d_comparison_selection_server <- function(input, session, groups) {
  selection <- reactiveVal(list(
    valid = FALSE,
    group_1 = character(),
    group_2 = character(),
    choices_1 = character(),
    choices_2 = character(),
    message = "Comparison requires at least two distinct groups."
  ))

  synchronize <- function(prefer, use_user_selection = TRUE) {
    resolved <- ena3d_comparison_selection(
      groups = groups(),
      group_1 = if (isTRUE(use_user_selection)) input$compare_group_1 else NULL,
      group_2 = if (isTRUE(use_user_selection)) input$compare_group_2 else NULL,
      prefer = prefer
    )
    selection(resolved)

    updates <- list(
      compare_group_1 = list(
        choices = resolved$choices_1, selected = resolved$group_1
      ),
      compare_group_2 = list(
        choices = resolved$choices_2, selected = resolved$group_2
      )
    )
    for (input_id in names(updates)) {
      selected <- updates[[input_id]]$selected
      if (isTRUE(use_user_selection)) {
        current <- input[[input_id]]
        if (!identical(as.character(current), as.character(selected))) {
          freezeReactiveValue(input, input_id)
        }
      }
      updateSelectInput(
        session,
        input_id,
        choices = updates[[input_id]]$choices,
        selected = selected
      )
    }
    invisible(resolved)
  }

  # Dataset replacement freezes both Selectize inputs. Resolve the new
  # dataset's defaults without reading those frozen values, then let the two
  # lower-priority observers handle subsequent user choices.
  observeEvent(groups(), synchronize("group_1", use_user_selection = FALSE),
               ignoreInit = FALSE, priority = 1100)
  observeEvent(input$compare_group_1, synchronize("group_1"),
               ignoreInit = TRUE, priority = 1000)
  observeEvent(input$compare_group_2, synchronize("group_2"),
               ignoreInit = TRUE, priority = 1000)

  reactive(selection())
}


source('./app_utils.R')
source('./plot_group.R')
ena_comparison_plot_output <-  function(input, output, session,
                                        data,
                                        state,
                                        scaled_points,
                                        scaled_nodes
                                        ) {
    x_axis <- reactive({
      tilde_var_or_null(input$x)
    })
    y_axis <- reactive({
      tilde_var_or_null(input$y)
    })
    z_axis <- reactive({
      tilde_var_or_null(input$z)
    })
    camera <- reactive({
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
    })
    get_groups = reactive({
      data$ena_groups
    })
    comparison_selection <- ena3d_comparison_selection_server(
      input, session, get_groups
    )
    comparison_colors <- reactive(c(
      group_1 = ena3d_normalize_comparison_color(
        input$comparison_group_1_color, "#BF382A"
      ),
      group_2 = ena3d_normalize_comparison_color(
        input$comparison_group_2_color, "#0C4B8E"
      )
    ))
    output$comparison_status <- renderText({
      comparison_selection()$message
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
      
      plot
    }
    
    add_mean_based_on_user_selection <- function(plot){
      selection <- comparison_selection()
      validate(need(selection$valid, selection$message))
      if(input$compare_group_1_show_confidence_interval){
        group_1_conf = 'box'
      }else{
        group_1_conf = 'none'
      }
      
      if(input$compare_group_2_show_confidence_interval){
        group_2_conf = 'box'
      }else{
        group_2_conf = 'none'
      }
      
      if(input$compare_group_1_show_mean){
        points <- get_points_with_group(
          scaled_points(), data$ena_groupVar[1], selection$group_1
        )
        points <-remove_meta_data(points)
        plot <- ena_plot_group_3d(plot,points = points,
                                  colors=comparison_colors()[["group_1"]],
                                  confidence.interval=group_1_conf,
                                  x_axis = input$x,
                                  y_axis = input$y,
                                  z_axis = input$z)
        
      }
      if(input$compare_group_2_show_mean){
        points = get_points_with_group(
          scaled_points(), data$ena_groupVar[1], selection$group_2
        )
        points <-remove_meta_data(points)
        plot <- ena_plot_group_3d(plot,
                                  points = points,
                                  colors=comparison_colors()[["group_2"]],
                                  confidence.interval=group_2_conf,
                                  x_axis = input$x,
                                  y_axis = input$y,
                                  z_axis = input$z)
        
      }
      return(plot)
    }
    generate_plot = reactive({
      
      main_plot <- plot_ly(source='plot_correlation')
      req(data$initialized,cancelOutput = TRUE)
      req(state$render_comparison(),cancelOutput = TRUE)
      req(state$is_app_initialized,cancelOutput = TRUE)
      req(!is.null(state$ena_obj),cancelOutput = TRUE)
      req(input$x, input$y, input$z)
      req(
        ena3d_axes_are_distinct(input$x, input$y, input$z),
        cancelOutput = TRUE
      )
      selection <- comparison_selection()
      validate(need(selection$valid, selection$message))
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
                          scene = list(xaxis = list(title = input$x,showgrid=input$show_grid,zeroline=input$show_zeroline),
                                       yaxis = list(title = input$y,showgrid=input$show_grid,zeroline=input$show_zeroline),
                                       zaxis = list(title = input$z,showgrid=input$show_grid,zeroline=input$show_zeroline)),
                          showlegend = TRUE)
      g1.mean <- get_mean_group_lineweights(
        state$ena_obj, data$ena_groupVar[1], selection$group_1
      )
      g2.mean <- get_mean_group_lineweights(
        state$ena_obj, data$ena_groupVar[1], selection$group_2
      )
      network <- g1.mean - g2.mean

      
      # Generate Edges
      network <- build_network(scaled_nodes(),
                               network=network,
                               adjacency.key=state$ena_obj$rotation$adjacency.key,
                               colors = c(
                                 pos = comparison_colors()[["group_1"]],
                                 neg = comparison_colors()[["group_2"]]
                               ))

      main_plot <- plot_network(main_plot,
                                network,
                                legend.include.edges = T,
                                x_axis=input$x,
                                y_axis=input$y,
                                z_axis=input$z,
                                line_width = input$line_width)

      comparison_title <- paste(
        selection$group_1, "vs", selection$group_2
      )
      main_plot <- layout(
        main_plot,
        title = comparison_title,
        scene = list(
          camera = camera(),
          uirevision = paste0("comparison-camera-", input$camera_position)
        )
      )
      main_plot
    })
    
    output$ena_points_plot <- renderPlotly({
      selection <- comparison_selection()
      validate(need(selection$valid, selection$message))
      comparison_plot <- generate_plot()

      comparison_plot <- add_3d_axis_based_on_user_selection(comparison_plot)
      comparison_plot <- add_mean_based_on_user_selection(comparison_plot)
      event_register(comparison_plot, 'plotly_relayout')

      # Means, confidence boxes, edges, scaled nodes, and optional axis arrows
      # are all present now. Autorange therefore reflects displayed coordinates
      # rather than the former fixed [-10, 10] window.
      comparison_plot <- layout(
        comparison_plot,
        title = paste(selection$group_1, "vs", selection$group_2),
        scene = list(
          camera = camera(),
          uirevision = paste0("comparison-camera-", input$camera_position),
          xaxis = ena3d_comparison_axis_layout(
            input$x, input$show_grid, input$show_zeroline
          ),
          yaxis = ena3d_comparison_axis_layout(
            input$y, input$show_grid, input$show_zeroline
          ),
          zaxis = ena3d_comparison_axis_layout(
            input$z, input$show_grid, input$show_zeroline
          )
        )
      )

      comparison_plot <- ena3d_apply_plotly_typography(comparison_plot)
      
      comparison_plot
      
    })
}
