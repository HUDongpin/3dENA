hide_element <- function(element_id){
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session)) {
    session$sendCustomMessage(
      "ena3d-plot-visibility",
      list(id = element_id, visible = FALSE)
    )
  }
}
show_element <- function(element_id){
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session)) {
    session$sendCustomMessage(
      "ena3d-plot-visibility",
      list(id = element_id, visible = TRUE)
    )
  }
}

ena3d_plotly_font <- function(size = 14L, color = "#25282d") {
  list(
    family = paste(
      "Inter, ui-sans-serif, -apple-system, BlinkMacSystemFont,",
      "'Segoe UI', sans-serif"
    ),
    size = as.numeric(size),
    color = as.character(color)
  )
}

ena3d_plotly_axis_layout <- function(title, showgrid = TRUE,
                                     zeroline = TRUE, nticks = NULL,
                                     autorange = TRUE) {
  axis <- list(
    title = list(
      text = as.character(title),
      font = ena3d_plotly_font(16L)
    ),
    tickfont = ena3d_plotly_font(14L),
    showgrid = isTRUE(showgrid),
    zeroline = isTRUE(zeroline),
    autorange = isTRUE(autorange)
  )
  if (!is.null(nticks)) axis$nticks <- as.integer(nticks)
  axis
}

ena3d_apply_plotly_typography <- function(plot) {
  if (is.null(plot)) return(NULL)
  plotly::layout(
    plot,
    font = ena3d_plotly_font(14L),
    legend = list(
      font = ena3d_plotly_font(14L),
      title = list(font = ena3d_plotly_font(14L))
    )
  )
}

ena3d_plotly_empty_state <- function(source, title, message) {
  plotly::plot_ly(source = source) |>
    plotly::layout(
      title = list(text = as.character(title)),
      annotations = list(list(
        text = as.character(message),
        x = 0.5,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = ena3d_plotly_font(16L)
      )),
      scene = list(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        zaxis = list(visible = FALSE)
      ),
      showlegend = FALSE
    )
}

get_ena_group<- function(ena_obj){
  if(is.null(ena_obj$`_function.params`$groups) && is.null(ena_obj$`_function.params`$unit.groups)){
    stop('No group specified in the ena_obj! Either ena_obj$`_function.params`$groups or ena_obj$`_function.params`$unit.groups is null.')
  }
  if(!is.null(ena_obj$`_function.params`$groups)){
    return(ena_obj$`_function.params`$groups)
  }
  return(ena_obj$`_function.params`$unit.groups)
}
get_ena_group_var<- function(ena_obj){
  if(is.null(ena_obj$`_function.params`$groupVar) && is.null(ena_obj$`_function.params`$units.by)){
    stop('No group specified in the ena_obj! Either ena_obj$`_function.params`$groupVar or ena_obj$`_function.params`$unit.by is null.')
  }
  if(!is.null(ena_obj$`_function.params`$groupVar)){
    return(ena_obj$`_function.params`$groupVar)
  }
  return(ena_obj$`_function.params`$units.by)
}


# Return a deterministic, ASCII-only identity key for every scalar in a
# vector. The key preserves exact numeric values (including adjacent doubles),
# exact POSIXct instants, semantic difftime values, storage type, and missing
# values without using a delimiter that can occur in user data.
ena3d_value_identity_keys <- function(values) {
  encode_hex <- function(value) {
    bytes <- charToRaw(enc2utf8(as.character(value)))
    if (!length(bytes)) return("")
    paste(sprintf("%02x", as.integer(bytes)), collapse = "")
  }

  if (inherits(values, "POSIXt")) {
    type <- "datetime"
    plain <- sprintf("%a", as.numeric(values))
  } else if (inherits(values, "Date")) {
    type <- "date"
    plain <- sprintf("%a", as.numeric(values))
  } else if (inherits(values, "difftime")) {
    type <- "duration_seconds"
    plain <- sprintf("%a", as.numeric(values, units = "secs"))
  } else if (is.ordered(values)) {
    type <- "ordered"
    plain <- as.character(values)
  } else if (is.factor(values)) {
    type <- "factor"
    plain <- as.character(values)
  } else if (is.double(values)) {
    type <- "double"
    plain <- sprintf("%a", values)
  } else if (is.integer(values)) {
    type <- "integer"
    plain <- as.character(values)
  } else if (is.logical(values)) {
    type <- "logical"
    plain <- ifelse(values, "true", "false")
  } else if (is.character(values)) {
    type <- "character"
    plain <- enc2utf8(values)
  } else {
    type <- paste(class(values), collapse = "/")
    plain <- enc2utf8(as.character(values))
  }

  missing <- is.na(values)
  if (is.numeric(values) || inherits(values, c("Date", "POSIXt", "difftime"))) {
    missing <- missing | !is.finite(as.numeric(values))
  }
  type_key <- encode_hex(type)
  vapply(seq_along(values), function(index) {
    if (missing[[index]]) return(paste0("ena3d:v1:", type_key, ":n"))
    paste0("ena3d:v1:", type_key, ":v:", encode_hex(plain[[index]]))
  }, character(1L), USE.NAMES = FALSE)
}


# Values emitted by Shiny selectors are strings, while ENA metadata may retain
# richer R classes. Keep ordinary labels unchanged, but disambiguate exact
# numeric values whose default text collides. POSIXct needs special handling
# because two different instants in a daylight-saving fold can share the same
# wall-clock text; add the UTC offset only for ambiguous wall times and retain
# an exact epoch fallback for sub-second collisions.
ena3d_group_value_labels <- function(values) {
  labels <- as.character(values)
  if (!length(values)) return(labels)

  if (!inherits(values, "POSIXt")) {
    is_exact_numeric <- is.double(values) &&
      !inherits(values, "integer64")
    if (!is_exact_numeric) return(labels)

    exact_values <- if (inherits(values, "difftime")) {
      as.numeric(values, units = "secs")
    } else {
      as.numeric(values)
    }
    usable <- !is.na(values) & is.finite(exact_values) & !is.na(labels)
    rows <- which(usable)
    if (!length(rows)) return(labels)

    exact_labels <- sprintf("%a", exact_values)
    exact_labels[usable & exact_values == 0] <- "0x0p+0"
    distinct <- tapply(
      exact_labels[rows],
      labels[rows],
      function(keys) length(unique(keys)),
      simplify = TRUE
    )
    ambiguous <- names(distinct)[distinct > 1L]
    collision <- usable & labels %in% ambiguous
    labels[collision] <- paste0(
      labels[collision], " [exact=", exact_labels[collision], "]"
    )
    return(labels)
  }

  instants <- as.numeric(values)
  usable <- !is.na(values) & is.finite(instants) & !is.na(labels)
  collision_rows <- function(current_labels) {
    rows <- which(usable)
    if (!length(rows)) return(rep(FALSE, length(values)))
    instant_keys <- sprintf("%.17g", instants[rows])
    distinct <- tapply(
      instant_keys,
      current_labels[rows],
      function(keys) length(unique(keys)),
      simplify = TRUE
    )
    ambiguous <- names(distinct)[distinct > 1L]
    usable & current_labels %in% ambiguous
  }

  ambiguous <- collision_rows(labels)
  if (any(ambiguous)) {
    timezone <- attr(values, "tzone", exact = TRUE)
    timezone <- if (is.null(timezone) || !length(timezone) ||
                       is.na(timezone[[1L]])) "" else timezone[[1L]]
    labels[ambiguous] <- format(
      values[ambiguous],
      format = "%Y-%m-%d %H:%M:%OS6 %z",
      tz = timezone,
      usetz = FALSE
    )
  }

  still_ambiguous <- collision_rows(labels)
  if (any(still_ambiguous)) {
    labels[still_ambiguous] <- paste0(
      labels[still_ambiguous],
      " [epoch=", sprintf("%.17g", instants[still_ambiguous]), "]"
    )
  }
  labels
}


ena3d_group_value_match <- function(values, selections) {
  if (is.null(selections) || !length(selections)) {
    return(rep(FALSE, length(values)))
  }
  if (inherits(values, "POSIXt") && inherits(selections, "POSIXt")) {
    return(
      !is.na(values) & is.finite(as.numeric(values)) &
        as.numeric(values) %in% as.numeric(selections)
    )
  }
  !is.na(values) &
    ena3d_group_value_labels(values) %in% as.character(selections)
}


ena3d_axes_are_distinct <- function(x, y = NULL, z = NULL) {
  axes <- if (is.null(y) && is.null(z) && length(x) == 3L) {
    as.character(x)
  } else {
    as.character(c(x, y, z))
  }
  length(axes) == 3L && !anyNA(axes) && all(nzchar(axes)) &&
    length(unique(axes)) == 3L
}


ena3d_axis_selection_vector <- function(selected = NULL) {
  axis_names <- c("x", "y", "z")
  values <- as.character(selected)
  value_names <- names(selected)
  result <- stats::setNames(rep(NA_character_, 3L), axis_names)
  if (!length(values)) return(result)

  if (is.null(value_names)) {
    count <- min(length(values), length(axis_names))
    result[axis_names[seq_len(count)]] <- values[seq_len(count)]
    return(result)
  }

  indices <- match(axis_names, value_names)
  present <- !is.na(indices)
  result[present] <- values[indices[present]]
  result
}


ena3d_normalize_axis_selection <- function(dimensions, selected = NULL,
                                           priority = c("x", "y", "z")) {
  dimensions <- unique(as.character(dimensions))
  dimensions <- dimensions[!is.na(dimensions) & nzchar(dimensions)]
  if (length(dimensions) < 3L) {
    stop("At least three distinct ENA dimensions are required.", call. = FALSE)
  }

  axis_names <- c("x", "y", "z")
  selected <- ena3d_axis_selection_vector(selected)
  priority <- unique(as.character(priority))
  priority <- c(
    priority[priority %in% axis_names],
    setdiff(axis_names, priority)
  )

  resolved <- stats::setNames(rep(NA_character_, 3L), axis_names)
  used <- character()
  for (axis in priority) {
    candidate <- selected[[axis]]
    if (is.na(candidate) || !nzchar(candidate) ||
        !candidate %in% dimensions || candidate %in% used) {
      candidate <- dimensions[!dimensions %in% used][[1L]]
    }
    resolved[[axis]] <- candidate
    used <- c(used, candidate)
  }
  resolved
}


ena3d_resolve_axis_change <- function(dimensions, previous, current, changed) {
  axis_names <- c("x", "y", "z")
  if (!is.character(changed) || length(changed) != 1L ||
      !changed %in% axis_names) {
    stop("`changed` must be one of x, y, or z.", call. = FALSE)
  }

  previous <- tryCatch(
    ena3d_normalize_axis_selection(dimensions, previous),
    error = function(error) NULL
  )
  current <- ena3d_axis_selection_vector(current)
  requested <- current[[changed]]
  dimensions <- unique(as.character(dimensions))

  if (is.null(previous) || is.na(requested) || !nzchar(requested) ||
      !requested %in% dimensions) {
    return(ena3d_normalize_axis_selection(
      dimensions, current, priority = c(changed, setdiff(axis_names, changed))
    ))
  }

  # When an axis takes a dimension currently used by another axis, swap that
  # other axis to the changed axis's former dimension. This keeps all three
  # selectors valid even when a dataset has exactly three dimensions.
  collisions <- setdiff(axis_names[current == requested], changed)
  if (length(collisions)) {
    current[[collisions[[1L]]]] <- previous[[changed]]
  }
  ena3d_normalize_axis_selection(
    dimensions, current, priority = c(changed, setdiff(axis_names, changed))
  )
}


ena3d_subset_rows_preserve_column_types <- function(data, rows) {
  frame <- as.data.frame(data, stringsAsFactors = FALSE, optional = TRUE)
  subset <- frame[rows, , drop = FALSE]
  for (name in names(subset)) {
    original_attributes <- attributes(data[[name]])
    transferable <- setdiff(
      names(original_attributes), c("names", "dim", "dimnames")
    )
    for (attribute_name in transferable) {
      attr(subset[[name]], attribute_name) <-
        original_attributes[[attribute_name]]
    }
  }
  subset
}


get_points_with_group <-function(points,groupVar,group_name){
  ena3d_subset_rows_preserve_column_types(
    points,
    which(ena3d_group_value_match(points[[groupVar]], group_name))
  )
}
tilde_var_or_null = function(var_name){
  if (is.null(var_name)) return(NULL)
  if (!is.character(var_name) || length(var_name) != 1L ||
      is.na(var_name) || !nzchar(var_name)) {
    stop("A plot variable must be one non-empty column name.", call. = FALSE)
  }

  # Build the formula language object directly. Interpolating a name into
  # formula source is not safe for accepted column names containing backticks
  # or backslashes, even when the backtick itself is escaped.
  stats::as.formula(
    call("~", as.name(var_name)),
    env = parent.frame()
  )
}
add_3d_axis = function(plot){
  if (is.null(plot)) return(NULL)
  plot<-add_x_3d_axis(plot)
  plot<-add_y_3d_axis(plot)
  plot<-add_z_3d_axis(plot)
  
  # plot <- layout(plot,title='X-Y',scene= list(camera=list(eye=list(x=0., y=0., z=-2.5))))
  plot
  
  
}
add_x_3d_axis<-function(plot){
  if (is.null(plot)) return(NULL)
  # Create a 3D plot with scatter3d trace for lines
  plot <- plotly::add_trace(
    plot,
      type = "scatter3d",
      mode = "lines+markers",
      x = c(0,1),
      y = c(0,0),
      z = c(0,0),
      line = list(color = "red", width = 2),
      marker = list(size = 1, color = "red")
    )
  
  cone_height <- 1
  cone_center <- c(1, 0, 0)
  
  # Create a 3D plot with cone trace
  plot <- plotly::add_trace(
    plot,
      type = "cone",
      x = cone_center[1],
      y = cone_center[2],
      z = cone_center[3],
      u = list(cone_height),
      v = list(0),
      w = list(0),
      sizemode = "absolute",
      sizeref = 0.2,
      showscale = FALSE,
      colorscale = list(c(0, 'red'), c(1, 'red')),
      anchor = "tail"
    )
  
  
  plot <- plotly::add_text(
    plot,
    x = cone_center[1],
    y = cone_center[2],
    z = cone_center[3]  + 0.1, # Adjust the height of the text above the cone
    text = "X axis",
    textfont = ena3d_plotly_font(14L, "red")
  )
  plot
}
add_y_3d_axis<-function(plot){
  if (is.null(plot)) return(NULL)
  cone_height <- 1
  plot <- plotly::add_trace(
    plot,
      type = "scatter3d",
      mode = "lines+markers",
      x = c(0,0),
      y = c(0,1),
      z = c(0,0),
      line = list(color = "blue", width = 2),
      marker = list(size = 1, color = "blue")
    )
  
  cone_center <- c(0, 1, 0)
  
  # Create a 3D plot with cone trace
  plot <- plotly::add_trace(
    plot,
      type = "cone",
      x = cone_center[1],
      y = cone_center[2],
      z = cone_center[3],
      u = list(0),
      v = list(cone_height),
      w = list(0),
      sizemode = "absolute",
      sizeref = 0.2,
      showscale = FALSE,
      colorscale = list(c(0, 'blue'), c(1, 'blue')),
      anchor = "tail"
    ) 
  plot <- plotly::add_text(
    plot,
    x = cone_center[1],
    y = cone_center[2],
    z = cone_center[3]  + 0.1, # Adjust the height of the text above the cone
    text = "Y axis",
    textfont = ena3d_plotly_font(14L, "blue")
  )
  plot
}
add_z_3d_axis<-function(plot){
  if (is.null(plot)) return(NULL)
  cone_height <- 1
  plot <- plotly::add_trace(
    plot,
      type = "scatter3d",
      mode = "lines+markers",
      x = c(0,0),
      y = c(0,0),
      z = c(0,1),
      line = list(color = "green", width = 2),
      marker = list(size = 1, color = "green")
    )
  
  cone_center <- c(0, 0, 1)
  
  # Create a 3D plot with cone trace
  plot <- plotly::add_trace(
    plot,
      type = "cone",
      x = cone_center[1],
      y = cone_center[2],
      z = cone_center[3],
      u = list(0),
      v = list(0),
      w = list(cone_height),
      sizemode = "absolute",
      sizeref = 0.2,
      showscale = FALSE,
      colorscale = list(c(0, 'green'), c(1, 'green')),
      anchor = "tail"
    ) 
  plot <- plotly::add_text(
    plot,
    x = cone_center[1],
    y = cone_center[2],
    z = cone_center[3]  + 0.1, # Adjust the height of the text above the cone
    text = "Z axis",
    textfont = ena3d_plotly_font(14L, "green")
  )
  plot
}

set_default_axis_range <- function(plot){
  # Retained for backward compatibility. Plotly must derive its range from the
  # completed, display-scaled traces; a fixed window silently clips valid data.
  axis_layout <- list(nticks = 4, autorange = TRUE)
  
  plot <- plot %>%
    plotly::layout(
      scene = list(
        aspectmode = "cube",
        camera = list(eye = list(x=0, y=0, z=2.5),up=list(x=0,y=1,z=0)),
        xaxis=axis_layout,yaxis=axis_layout,zaxis=axis_layout
      )
    )
  return(plot)
}

ena3d_normalize_plot_color <- function(color, fallback = "#808080") {
  valid_color <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) &&
      nzchar(trimws(value)) &&
      !inherits(try(grDevices::col2rgb(trimws(value), alpha = TRUE), silent = TRUE),
                "try-error")
  }

  fallback <- as.character(fallback)[1L]
  if (!valid_color(fallback)) fallback <- "#808080"
  color <- as.character(color)[1L]
  if (!valid_color(color)) return(fallback)
  trimws(color)
}


get_group_color<-function(group_colors,group_col,group_name){
  matches <- which(
    as.character(group_colors[, group_col]) %in% as.character(group_name)
  )
  if (length(matches) == 0L) return("#808080")
  if (is.matrix(group_colors) && "color" %in% colnames(group_colors)) {
    return(ena3d_normalize_plot_color(
      unname(group_colors[matches[1L], "color"])
    ))
  }
  ena3d_normalize_plot_color(unname(group_colors[matches[1L], 1L]))
}

ena3d_palette <- function(n) {
  if (n <= 0L) return(character())
  grDevices::hcl.colors(n, palette = "Dark 3")
}
