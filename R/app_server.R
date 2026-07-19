"
This module is the main app. It handles the loading of the ENA data, and prepares data of nodes and points for rendering.
The logic for rendering the plots is handled in the sub-modules.
"
source('build_network.R')
source('transition.R')
source('app_utils.R')
source('app_module_ena_comparison_plot.R')
source('app_module_ena_unit_group_change_plot.R')
source('ena3d_exchange.R')
source('app_module_load_dataset.R')
source('raw_data_import.R')
source('app_module_upload_data.R')
source('app_module_sample_data.R')
source('app_module_stats.R')
source('app_module_overall_model.R')
source('app_module_network.R')
source('trajectory_analysis.R')
source('trajectory_plot.R')
source('app_module_trajectory.R')
source('app_module_ai_interpretation.R')

ena_app_server <- function(id, state, config, page_active, workspace_section) {
  # Calling the moduleServer function
  moduleServer(
    # Setting the id
    id,
    # Defining the module core mechanism
    function(input, output, session) {
      ns = NS(id)
      # reactive values, served as global variable inside the server
      rv <- reactiveValues(myList = list(),
                           unit_group_change_plots=list(),
                           current_camera=list(x=1.25, y=1.25, z=1.25),
                           ena_groups=list(),
                           ena_groupVar=list(),
                           ena_points_plot_ready=FALSE,
                           initialized=FALSE,
                           model_tab_clicked=FALSE,
                           comparison_plot=list(),
                           reactiveFunctions = list(),
                           group_colors=list(),
                           group_selectors=list(),
                           group_options=list())
      ena_nodes <- function(){
        req(state$ena_obj)
        state$ena_obj$rotation$nodes
      }

      # Obtain the points position after scaling
      scaled_points <- reactive({
        req(state$ena_obj, rv$initialized)
        my_points <- data.table::copy(state$ena_obj$points)
        for(i in ena3d_dimension_names(state$ena_obj)){
          my_points[[i]] <- my_points[[i]] * scale_factor()
        }

        my_points
        
      })

      # Obtain the codes position after scaling
      scaled_nodes <- reactive({
        req(state$ena_obj, rv$initialized)
        node_points <- data.table::copy(ena_nodes())
        for(i in intersect(ena3d_dimension_names(state$ena_obj), names(node_points))){
          node_points[[i]] <- node_points[[i]] * scale_factor()
        }

        node_size_range = c(3,10)
        node_points$weight = rep(0, nrow(node_points))
        if( any(node_points$weight > 0)) {
          node_points$weight = scales::rescale((node_points$weight * (1 / max(abs(node_points$weight)))), node_size_range) # * enaplot$get("multiplier"));
        }
        else {
          node_points$weight = node_size_range[2]
        }

        node_points
      })

      scale_factor<- reactive({
        if (is.null(input$scale_factor)) 1 else input$scale_factor
      })

      axis_selection <- reactiveVal(
        stats::setNames(rep(NA_character_, 3L), c("x", "y", "z"))
      )

      observeEvent(rv$dataset_id, {
        req(state$ena_obj)
        dimensions <- ena3d_dimension_names(state$ena_obj)
        axis_selection(ena3d_normalize_axis_selection(dimensions))
      }, ignoreInit = TRUE, priority = 1100)

      synchronize_axis_selection <- function(changed_axis) {
        req(rv$initialized, state$ena_obj, input$x, input$y, input$z)
        dimensions <- ena3d_dimension_names(state$ena_obj)
        current <- c(x = input$x, y = input$y, z = input$z)
        resolved <- ena3d_resolve_axis_change(
          dimensions = dimensions,
          previous = axis_selection(),
          current = current,
          changed = changed_axis
        )
        axis_selection(resolved)

        for (axis in names(resolved)) {
          if (!identical(as.character(current[[axis]]), resolved[[axis]])) {
            freezeReactiveValue(input, axis)
            updateSelectInput(
              session, axis, choices = dimensions, selected = resolved[[axis]]
            )
          }
        }
        invisible(resolved)
      }

      # Treat choosing an already-used dimension as an axis swap. Plot and
      # Stats observers run at the default priority, so this high-priority
      # guard resolves duplicate selections first.
      observeEvent(input$x, synchronize_axis_selection("x"),
                   ignoreInit = TRUE, priority = 1000)
      observeEvent(input$y, synchronize_axis_selection("y"),
                   ignoreInit = TRUE, priority = 1000)
      observeEvent(input$z, synchronize_axis_selection("z"),
                   ignoreInit = TRUE, priority = 1000)

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

      trajectory_results <- trajectory_server(
        "trajectory",
        ena_obj = reactive({
          rv$dataset_id
          if (isTRUE(rv$initialized)) state$ena_obj else NULL
        }),
        selected_axes = reactive(c(input$x, input$y, input$z)),
        raw_dimensions = reactive({
          if (isTRUE(rv$initialized)) ena3d_dimension_names(state$ena_obj) else character()
        }),
        group_colors = reactive(rv$group_colors),
        camera = camera
      )
      
      # rv$reactiveFunctions['get_group_color']<-function(group_colors,group_col,group_name){
      #   group_colors[which(group_colors[,group_col]==group_name)]
      # }
      
      "
        The plot in the model -> comparsion tab
      "
      ena_comparison_plot_output(input, output, session,
                                 rv,
                                 state,
                                 scaled_points,
                                 scaled_nodes,
                                 rv$current_camera,
                                 )
     
      "
        The plot in the model->change tab
      "
      ena_unit_group_change_plot_output(input,output,session,
                                        rv,
                                        state,
                                        scaled_points,
                                        scaled_nodes
                                        )
      
      ena_overall_plot_output(input, output, session,
                              rv,
                              state,
                              scaled_points,
                              scaled_nodes,
                              rv$current_camera,
      )
      ena_network_plot_output(input, output, session,
                              rv,
                              state,
                              scaled_points,
                              scaled_nodes,
                              rv$current_camera,
      )
      
      
      plot_ids <- c(
        comparison_plot = "ena_points_plot",
        group_change = "ena_unit_group_change_plot",
        overall_model = "ena_overall_plot",
        network = "ena_network_plot",
        trajectory = "ena_trajectory_panel"
      )
      observeEvent(state$active_tab(), {
        active_id <- unname(plot_ids[[state$active_tab()]])
        for (plot_id in unname(plot_ids)) {
          session$sendCustomMessage(
            "ena3d-plot-visibility",
            list(id = session$ns(plot_id), visible = identical(plot_id, active_id))
          )
        }
      }, ignoreInit = FALSE)
      
      upload_data(input,output,session,rv,state,config)
      sample_data_load_and_select(input,output,session,rv,config,state)
      stats_results <- stats_module(input,output,session,rv,config,state)

      ai_settings <- reactive({
        current_workspace <- if (is.function(workspace_section)) {
          workspace_section()
        } else {
          workspace_section
        }
        axes <- c(input$x, input$y, input$z)
        group_var <- if (length(rv$ena_groupVar)) {
          rv$ena_groupVar[[1L]]
        } else {
          NULL
        }
        view <- .ena3d_ai_current_view(current_workspace, state$active_tab())
        if (is.null(view)) return(list(axes = axes))

        switch(
          view,
          overall = list(
            group_var = group_var,
            selected_groups = if (!is.null(input$select_group)) {
              input$select_group
            } else {
              rv$ena_groups
            },
            axes = axes
          ),
          network = {
            target <- ena3d_network_selector_decode(input$network_selector)
            list(
              group_var = group_var,
              selected_groups = if (!is.null(target) &&
                                    identical(target$type, "group")) {
                target$value
              } else {
                character()
              },
              selection_type = if (is.null(target)) "none" else target$type,
              axes = axes
            )
          },
          comparison = list(
            group_var = group_var,
            comparison_groups = c(
              input$compare_group_1, input$compare_group_2
            ),
            axes = axes
          ),
          change = list(
            change_var = input$group_change_var,
            change_values = input$unit_change,
            axes = axes
          ),
          stats = list(
            group_var = group_var,
            comparison_groups = c(input$stats_group1, input$stats_group2),
            stats_design = if (identical(input$stats_design, "within")) {
              "paired"
            } else {
              "unpaired"
            },
            p_adjust_method = input$stats_p_adjust_method,
            alternative = input$stats_paired_alternative,
            test_family = input$stats_test_family,
            axes = axes
          ),
          trajectory = list(axes = axes),
          list(axes = axes)
        )
      })

      ai_interpretation_server(
        "ai_interpretation",
        enabled = reactive(isTRUE(config$ai$available)),
        page_active = page_active,
        workspace_section = workspace_section,
        model_tab = state$active_tab,
        ena_obj = reactive({
          rv$dataset_id
          if (isTRUE(rv$initialized)) state$ena_obj else NULL
        }),
        settings = ai_settings,
        data_version = reactive(rv$dataset_id),
        stats_result = stats_results,
        trajectory_result = trajectory_results$result,
        config = config$ai
      )
      # execute_at_next_input <- function(expr, session = getDefaultReactiveDomain()) {
      #   observeEvent(once = TRUE, reactiveValuesToList(session$input), {
      #     print(reactiveValuesToList(session$input))
      #     force(expr)
      #   }, ignoreInit = TRUE)
      # }
      
      # create checkbox for select group in the model->comparison tab
      # output$group_colors_container <- renderUI({
      #   n = length(rv$ena_groups)
      #   checkboxGroupInput(ns("select_group"), "Choose Group:",
      #                      choiceNames = rv$ena_groups,
      #                      choiceValues = rv$ena_groups,
      #                      selected=rv$ena_groups
      #   )
      # })
      
      
      observeEvent(input$select_group,{
        if(!is.null(input$select_group)){
          rv$model_tab_clicked<-TRUE
        }
      })
      
      # One dynamic observer owns all color inputs. The previous implementation
      # created a new nested observer for every group after every dataset load,
      # so old handlers accumulated for the lifetime of the Shiny session.
      observe({
        selectors <- rv$group_selectors
        if (!length(selectors)) return(invisible(NULL))
        color_values <- lapply(selectors, function(selector) {
          input[[selector[["color_selector_id"]]]]
        })
        colors <- isolate(rv$group_colors)
        changed <- FALSE
        for (i in seq_along(selectors)) {
          value <- color_values[[i]]
          if (is.null(value) || !nzchar(value)) next
          group_name <- selectors[[i]][["group_name"]]
          index <- which(colors[, "group"] == group_name)
          if (length(index) &&
              !identical(as.character(colors[index[[1L]], "color"]), value)) {
            colors[index[[1L]], "color"] <- value
            changed <- TRUE
          }
        }
        if (changed) isolate(rv$group_colors <- colors)
      })
      # observeEvent(
      #   eventExpr = {
      #   
      #     if(length(rv$group_selectors)==0){
      #       input$x
      #     }else{
      #       list_of_selectors<-list()
      #       for(group_selector in rv$group_selectors) {
      #         print(group_selector)
      #         color_selector_id <-group_selector[['color_selector_id']]
      #         list_of_selectors <- append(list_of_selectors, input[[color_selector_id]])
      #       }
      #       print('listen to this')
      #       print(list_of_selectors)
      #       list_of_selectors
      #     }
      #     #input$x
      #   },
      #   handlerExpr = { #Replace with listen to any input with id starting with "button_"
      #     #req(input$changed)
      #     print('click')
      #     #browser()
      #     #print(input$changed)
      #     #print(paste0("Inside observer: ",input[[input$changed]]," was fired"))
      #   },
      #   ignoreInit = T
      # )
    
      
    }
  )
}
