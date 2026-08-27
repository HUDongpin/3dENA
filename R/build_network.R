.ena3d_network_colors <- function(colors) {
  if (length(colors) < 1L || length(colors) > 2L ||
      anyNA(colors) || any(!nzchar(as.character(colors)))) {
    stop(
      paste0(
        "`colors` must contain one or two valid R color values: the first ",
        "for positive edges and the second for negative edges."
      ),
      call. = FALSE
    )
  }

  rgb <- tryCatch(
    grDevices::col2rgb(colors),
    error = function(error) {
      stop(
        sprintf(
          paste0(
            "Invalid network `colors` value(s): %s. Use one or two valid R ",
            "color names or hex values."
          ),
          paste(as.character(colors), collapse = ", ")
        ),
        call. = FALSE
      )
    }
  )
  hsv <- grDevices::rgb2hsv(rgb)

  if (ncol(hsv) == 1L) {
    complement <- hsv[, 1L]
    complement[1L] <- (complement[1L] + 0.5) %% 1
    hsv <- cbind(hsv[, 1L], complement)
  }

  hsv <- hsv[, seq_len(2L), drop = FALSE]
  base_colors <- vapply(seq_len(2L), function(index) {
    grDevices::hsv(hsv[1L, index], hsv[2L, index], hsv[3L, index])
  }, character(1L))

  list(hsv = hsv, base_colors = base_colors)
}

.ena3d_network_width_bins <- function(widths, max_bins = 6L) {
  widths <- suppressWarnings(as.numeric(widths))
  if (!length(widths)) return(integer())

  finite <- is.finite(widths) & widths >= 0
  fallback <- if (any(finite)) stats::median(widths[finite]) else 1
  widths[!finite] <- fallback
  unique_widths <- sort(unique(widths), method = "radix")
  if (length(unique_widths) <= max_bins) {
    return(match(widths, unique_widths))
  }

  breaks <- unique(stats::quantile(
    widths,
    probs = seq(0, 1, length.out = max_bins + 1L),
    names = FALSE,
    type = 8L
  ))
  if (length(breaks) < 2L) return(rep.int(1L, length(widths)))
  as.integer(cut(widths, breaks = breaks, include.lowest = TRUE, labels = FALSE))
}

.ena3d_network_html_escape <- function(value) {
  value <- as.character(value)
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  value <- gsub('"', "&quot;", value, fixed = TRUE)
  value
}

.ena3d_network_weight_label <- function(value) {
  value <- suppressWarnings(as.numeric(value)[1L])
  if (!length(value) || !is.finite(value)) return("not available")
  format(signif(value, 7L), trim = TRUE, scientific = FALSE)
}

build_network = function(node.positions,network,edge_type = "line",
                         adjacency.key = NULL,
                         thickness = c(min(abs(network)), max(abs(network))),
                         opacity = thickness,
                         saturation = thickness,
                         scale.range = c(ifelse(min(network)==0, 0, 0.1), 1),
                         colors=c(pos='#BF382A', neg='#0C4B8E'),
                         thin.lines.in.front=T){

  color_info <- .ena3d_network_colors(colors)
  if(choose(nrow(node.positions), 2) != length(network)) {
    stop(paste0("Network vector needs to be of length ", choose(nrow(node.positions), 2)))
  }
  if(is(node.positions, "ena.nodes")) {
    if(is.null(adjacency.key)) {
      adjacency.key <- namesToAdjacencyKey(node.positions$code)
    }
    node.rows <- node.positions$code
  }
  else {
    if(is.matrix(node.positions)) {
      node.positions <- as.data.frame(node.positions)
    }
    if (is.null(adjacency.key)) {
      adjacency.key <- namesToAdjacencyKey(rownames(node.positions))
    }
    node.rows <- rownames(node.positions)
  }
  network.edges.shapes = list();
  # Do not dispatch rENA's as.matrix.ena.nodes() here. Exchange-restored node
  # tables are valid base data.frames, while that method applies unary `-` to
  # the character column name `code` and fails before any plot can render.
  node_frame <- as.data.frame(
    node.positions, stringsAsFactors = FALSE, optional = TRUE
  )
  coordinate_names <- setdiff(names(node_frame), "code")
  non_numeric <- coordinate_names[!vapply(
    node_frame[coordinate_names], is.numeric, logical(1L)
  )]
  if (length(non_numeric)) {
    stop(sprintf(
      "Node coordinate columns must be numeric: %s",
      paste(non_numeric, collapse = ", ")
    ))
  }
  nodes <- as.data.frame(
    node_frame[coordinate_names], check.names = FALSE, optional = TRUE
  )
  network.thickness = abs(network) * 10;

  if (any(abs(network) > 0, na.rm = TRUE)) {
    network.saturation = scales::rescale(x = abs(network), to = scale.range, from = saturation);
    network.opacity = scales::rescale(x = abs(network), to = scale.range, from = opacity);
  } else {
    network.saturation <- rep(0, length(network))
    network.opacity <- rep(0, length(network))
  }
  
  pos.inds = as.numeric(which(network >=0));

  colors.hsv <- color_info$hsv

  mat = as.matrix(adjacency.key);
  kept_edges <- which(is.finite(network) & network != 0)
  for (i in kept_edges) {
    v0 <- nodes[node.rows==mat[1,i], ];
    v1 <- nodes[node.rows==mat[2,i], ];

    if(i %in% pos.inds) {
      color = colors.hsv[,1];
      edge_sign <- "positive"
      base_color <- color_info$base_colors[1L]
    } else {
      color = colors.hsv[,2];
      edge_sign <- "negative"
      base_color <- color_info$base_colors[2L]
    }
    color[2] = network.saturation[i];

    edge_shape = list(
      opacity = network.opacity[i],
      nodes = c(mat[,i]),
      label = paste(mat[1L, i], mat[2L, i], sep = "."),
      weight = as.numeric(network[i]),
      sign = edge_sign,
      base_color = base_color,
      line = list(
        color= hsv(color[1],color[2],color[3]),
        width= abs(network.thickness[i]) * 1,
        dash = edge_type
      ),
      node1 = v0,
      node2 = v1,
      size = as.numeric(abs(network[i]))
    );
    network.edges.shapes[[length(network.edges.shapes) + 1L]] = edge_shape
  };
  if(thin.lines.in.front) {
    network.edges.shapes = network.edges.shapes[rev(order(sapply(network.edges.shapes, "[[", "size")))]
  }
  else {
    network.edges.shapes = network.edges.shapes[order(sapply(network.edges.shapes, "[[", "size"))]
  }

  network_obj=list()
  network_obj$network.edges.shapes <-network.edges.shapes
  return(network_obj)
}



plot_network <- function(ena_plot,
                         network,
                         legend.include.edges=F,
                         x_axis='MR1',
                         y_axis='SVD2',
                         z_axis='SVD3',
                         line_width=5,
                         max_width_bins=6L){
  edges <- network$network.edges.shapes
  if (is.null(edges) || !length(edges)) return(ena_plot)
  if (!is.numeric(line_width) || length(line_width) != 1L ||
      is.na(line_width) || !is.finite(line_width) || line_width < 0) {
    stop("`line_width` must be one finite non-negative number.", call. = FALSE)
  }
  if (!is.numeric(max_width_bins) || length(max_width_bins) != 1L ||
      is.na(max_width_bins) || !is.finite(max_width_bins) ||
      max_width_bins < 1 || max_width_bins != as.integer(max_width_bins)) {
    stop("`max_width_bins` must be one positive integer.", call. = FALSE)
  }
  max_width_bins <- as.integer(max_width_bins)

  edge_records <- lapply(seq_along(edges), function(index) {
    edge <- edges[[index]]
    node1 <- edge$node1
    node2 <- edge$node2
    weight <- suppressWarnings(as.numeric(edge$weight)[1L])
    size <- suppressWarnings(as.numeric(edge$size)[1L])
    # Newly built networks carry the original weight. The size fallback keeps
    # plot_network compatible with older cached network objects.
    should_draw <- if (length(weight) && is.finite(weight)) {
      weight != 0
    } else if (length(size) && is.finite(size)) {
      size != 0
    } else {
      TRUE
    }
    if (!should_draw) return(NULL)

    sign <- if (!is.null(edge$sign) && nzchar(as.character(edge$sign)[1L])) {
      as.character(edge$sign)[1L]
    } else if (length(weight) && is.finite(weight) && weight < 0) {
      "negative"
    } else {
      "positive"
    }
    color <- as.character(edge$line$color)[1L]
    base_color <- if (!is.null(edge$base_color) &&
                      nzchar(as.character(edge$base_color)[1L])) {
      as.character(edge$base_color)[1L]
    } else {
      color
    }
    dash <- if (!is.null(edge$line$dash)) {
      as.character(edge$line$dash)[1L]
    } else {
      "solid"
    }
    width <- suppressWarnings(as.numeric(edge$line$width)[1L]) * line_width
    opacity <- suppressWarnings(as.numeric(edge$opacity)[1L])
    if (!length(width) || !is.finite(width) || width < 0) width <- line_width
    if (!length(opacity) || !is.finite(opacity)) opacity <- 1
    opacity <- min(1, max(0, opacity))
    label <- if (!is.null(edge$label) && nzchar(as.character(edge$label)[1L])) {
      as.character(edge$label)[1L]
    } else {
      paste(edge$nodes[1L], edge$nodes[2L], sep = ".")
    }
    hover <- paste0(
      "Edge: ", .ena3d_network_html_escape(label),
      "<br>Weight: ", .ena3d_network_weight_label(weight)
    )

    list(
      x = c(suppressWarnings(as.numeric(node1[, x_axis])[1L]),
            suppressWarnings(as.numeric(node2[, x_axis])[1L])),
      y = c(suppressWarnings(as.numeric(node1[, y_axis])[1L]),
            suppressWarnings(as.numeric(node2[, y_axis])[1L])),
      z = c(suppressWarnings(as.numeric(node1[, z_axis])[1L]),
            suppressWarnings(as.numeric(node2[, z_axis])[1L])),
      sign = sign,
      color = color,
      base_color = base_color,
      dash = dash,
      width = width,
      opacity = opacity,
      label = label,
      hover = hover
    )
  })
  edge_records <- Filter(Negate(is.null), edge_records)
  if (!length(edge_records)) return(ena_plot)

  widths <- vapply(edge_records, `[[`, numeric(1L), "width")
  width_bins <- .ena3d_network_width_bins(widths, max_width_bins)
  signs <- vapply(edge_records, `[[`, character(1L), "sign")
  base_colors <- vapply(edge_records, `[[`, character(1L), "base_color")
  dashes <- vapply(edge_records, `[[`, character(1L), "dash")
  style_keys <- paste(signs, base_colors, dashes, width_bins, sep = "\r")
  legend_keys <- paste(signs, base_colors, sep = "\r")
  legend_counts <- table(legend_keys)
  shown_legend_keys <- character()

  # build_network orders shapes for the requested thick/thin layering. Stable
  # first-appearance grouping preserves that approximate front-to-back order.
  for (style_key in unique(style_keys)) {
    rows <- which(style_keys == style_key)
    representative_width <- stats::median(widths[rows])
    representative_row <- rows[which.min(abs(widths[rows] - representative_width))]
    representative <- edge_records[[representative_row]]
    opacity <- stats::median(vapply(
      edge_records[rows], `[[`, numeric(1L), "opacity"
    ))
    key <- legend_keys[representative_row]
    show_legend <- isTRUE(legend.include.edges) &&
      !(key %in% shown_legend_keys)
    if (show_legend) shown_legend_keys <- c(shown_legend_keys, key)
    edge_count <- unname(legend_counts[[key]])
    trace_name <- if (edge_count == 1L) {
      representative$label
    } else {
      paste0(
        if (identical(representative$sign, "negative")) "Negative" else "Positive",
        " edges"
      )
    }

    segment_vector <- function(field, separator) {
      unlist(lapply(edge_records[rows], function(record) {
        c(record[[field]], separator)
      }), use.names = FALSE)
    }
    text <- unlist(lapply(edge_records[rows], function(record) {
      c(record$hover, record$hover, NA_character_)
    }), use.names = FALSE)

    ena_plot <- plotly::add_trace(
      ena_plot,
      type = "scatter3d",
      mode = "lines",
      x = segment_vector("x", NA_real_),
      y = segment_vector("y", NA_real_),
      z = segment_vector("z", NA_real_),
      text = text,
      hovertemplate = "%{text}<extra></extra>",
      line = list(
        color = representative$color,
        width = representative_width,
        dash = representative$dash
      ),
      opacity = opacity,
      connectgaps = FALSE,
      legendgroup = paste0("__ena3d_network_", key),
      showlegend = show_legend,
      name = trace_name,
      meta = list(
        ena3d_role = "network_edge_batch",
        edge_sign = representative$sign,
        edge_count = length(rows),
        width_bin = width_bins[representative_row]
      )
    )
  }
  ena_plot
}
