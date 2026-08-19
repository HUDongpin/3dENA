ena3d_static_site_routes <- c(
  home = "/",
  papers = "/papers",
  team = "/team",
  about = "/about"
)

ena3d_stateful_site_routes <- c(tool = "/app")
ena3d_internal_app_route <- "/__ena3d-app"

ena3d_site_routes <- c(
  home = ena3d_static_site_routes[["home"]],
  tool = ena3d_stateful_site_routes[["tool"]],
  papers = ena3d_static_site_routes[["papers"]],
  team = ena3d_static_site_routes[["team"]],
  about = ena3d_static_site_routes[["about"]]
)

ena3d_normalize_site_path <- function(path) {
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    return("/")
  }

  path <- sub("[?#].*$", "", as.character(path))
  if (!startsWith(path, "/")) {
    path <- paste0("/", path)
  }
  if (!identical(path, "/")) {
    path <- sub("/+$", "", path)
  }
  path
}

ena3d_is_site_path <- function(path) {
  ena3d_normalize_site_path(path) %in% unname(ena3d_site_routes)
}

ena3d_route_requires_session <- function(path) {
  ena3d_normalize_site_path(path) %in% c(
    unname(ena3d_stateful_site_routes),
    ena3d_internal_app_route
  )
}

ena3d_enable_site_routes <- function(app, static_handler) {
  if (!inherits(app, "shiny.appobj")) {
    stop("app must be a Shiny application object.", call. = FALSE)
  }
  if (!is.function(static_handler)) {
    stop("static_handler must be a function.", call. = FALSE)
  }

  original_handler <- app$httpHandler
  app$httpHandler <- function(request) {
    request_path <- if (is.null(request$PATH_INFO)) "/" else request$PATH_INFO
    normalized_path <- ena3d_normalize_site_path(request_path)
    if (ena3d_is_site_path(request_path) && !identical(request_path, normalized_path)) {
      query <- if (!is.null(request$QUERY_STRING) && nzchar(request$QUERY_STRING)) {
        paste0("?", request$QUERY_STRING)
      } else {
        ""
      }
      return(shiny:::httpResponse(
        status = 308L,
        content = "",
        headers = list(
          Location = paste0(normalized_path, query),
          "Cache-Control" = "no-store"
        )
      ))
    }

    if (normalized_path %in% unname(ena3d_static_site_routes)) {
      return(static_handler(request))
    }

    if (ena3d_route_requires_session(request_path)) {
      # Only the analysis workspace owns a Shiny application document and R
      # session. The internal route lets Vercel select that document without
      # changing the public /app URL seen by the browser.
      request$PATH_INFO <- "/"
    }
    original_handler(request)
  }
  app
}
