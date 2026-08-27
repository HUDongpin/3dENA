ena3d_site_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = "#f4f1e9",
    fg = "#102a43",
    primary = "#07747a",
    secondary = "#a7442e"
  )
}

ena3d_static_app_nav_item <- function() {
  bslib::nav_item(
    shiny::tags$a(
      class = "nav-link",
      href = "/app",
      `data-value` = "tool",
      "3D ENA"
    )
  )
}

ena3d_static_site_head <- function(config) {
  asset_version <- utils::URLencode(
    paste(config$app_version, config$build_id, sep = "-"),
    reserved = TRUE
  )

  htmltools::tagList(
    shiny::tags$meta(charset = "utf-8"),
    shiny::tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),
    shiny::tags$meta(
      name = "description",
      content = paste(
        "3D ENA is an interactive research environment for exploring",
        "Epistemic Network Analysis in three dimensions."
      )
    ),
    shiny::tags$meta(name = "ena3d-build", content = config$build_id),
    shiny::tags$meta(property = "og:type", content = "website"),
    shiny::tags$meta(
      property = "og:title",
      content = "3D ENA | See connections in three dimensions"
    ),
    shiny::tags$meta(
      property = "og:description",
      content = paste(
        "Explore knowledge structures, group comparisons, and longitudinal",
        "network trajectories."
      )
    ),
    shiny::tags$meta(property = "og:url", content = "https://3dena.com/"),
    shiny::tags$meta(property = "og:image", content = "https://3dena.com/og.png"),
    shiny::tags$meta(name = "twitter:card", content = "summary_large_image"),
    ena3d_analytics_tags(),
    shiny::tags$link(
      rel = "icon",
      type = "image/svg+xml",
      href = "/ena3d-assets/favicon.svg"
    ),
    shiny::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = paste0("/app_shell.css?v=", asset_version)
    ),
    shiny::tags$script(
      defer = NA,
      src = paste0("/site_routes.js?v=", asset_version)
    )
  )
}

ena3d_static_site_ui <- function() {
  bslib::page_navbar(
    bslib::nav_panel(
      title = "Home",
      value = "home",
      ena3d_home_ui()
    ),
    ena3d_static_app_nav_item(),
    bslib::nav_panel(
      title = "PAPERS",
      value = "papers",
      ena3d_papers_ui()
    ),
    bslib::nav_panel(
      title = "TEAM",
      value = "team",
      ena3d_team_ui()
    ),
    bslib::nav_panel(
      title = "ABOUT",
      value = "about",
      ena3d_about_ui()
    ),
    title = ena3d_brand_ui(),
    id = "site_nav",
    selected = "home",
    window_title = "3D ENA | Epistemic Network Analysis",
    collapsible = TRUE,
    fluid = TRUE,
    fillable = FALSE,
    theme = ena3d_site_theme(),
    footer = ena3d_footer_ui()
  )
}

ena3d_static_route_links <- function(html) {
  for (page in names(ena3d_static_site_routes)) {
    route <- unname(ena3d_static_site_routes[[page]])
    html <- ena3d_replace_matches(
      html,
      paste0('<a href="#[^"]+"[^>]*data-value="', page, '">'),
      function(tag) {
        tab_target <- sub(
          '^.*href="(#[^"]+)".*$',
          "\\1",
          tag,
          perl = TRUE
        )
        sub(
          paste0('href="', tab_target, '"'),
          paste0(
            'href="', route, '" ',
            'data-target="', tab_target, '" ',
            'data-bs-target="', tab_target, '"'
          ),
          tag,
          fixed = TRUE
        )
      }
    )
  }
  html
}

ena3d_render_static_site <- function(config, www_dir, inline_assets = FALSE) {
  document <- htmltools::htmlTemplate(
    text_ = paste0(
      "<!DOCTYPE html>",
      '<html lang="en" data-site-mode="static">',
      "<head><!-- HEAD_CONTENT -->{{ head }}</head>",
      "{{ body }}",
      "</html>"
    ),
    head = ena3d_static_site_head(config),
    body = ena3d_static_site_ui(),
    document_ = TRUE
  )
  html <- htmltools::renderDocument(
    document,
    processDep = shiny:::createWebDependency
  )
  html <- ena3d_static_route_links(as.character(html))
  if (isTRUE(inline_assets)) {
    html <- ena3d_inline_ui_assets(html, www_dir)
  }
  as.character(html)
}

ena3d_static_site_handler <- function(content) {
  if (!is.character(content) || length(content) != 1L || is.na(content)) {
    stop("Static site HTML must be one string.", call. = FALSE)
  }

  handler <- local({
    response_content <- content
    function(request) {
      method <- if (is.null(request$REQUEST_METHOD) ||
          !nzchar(request$REQUEST_METHOD)) {
        "GET"
      } else {
        toupper(as.character(request$REQUEST_METHOD)[[1L]])
      }
      if (!method %in% c("GET", "HEAD")) {
        return(shiny:::httpResponse(
          status = 405L,
          content_type = "text/plain; charset=UTF-8",
          content = "Method Not Allowed",
          headers = list(
            Allow = "GET, HEAD",
            "Cache-Control" = "no-store"
          )
        ))
      }
      shiny:::httpResponse(
        status = 200L,
        content = if (identical(method, "HEAD")) "" else response_content,
        headers = list("Cache-Control" = "no-store")
      )
    }
  })
  attr(handler, "http_methods_supported") <- c("GET", "HEAD")
  handler
}
