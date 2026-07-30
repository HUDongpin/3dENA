ena3d_site_routes <- c(
  home = "/",
  tool = "/app",
  papers = "/papers",
  team = "/team",
  about = "/about"
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

ena3d_enable_site_routes <- function(app) {
  if (!inherits(app, "shiny.appobj")) {
    stop("app must be a Shiny application object.", call. = FALSE)
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
    if (ena3d_is_site_path(request_path)) {
      # All public pages share one Shiny application document. Rewriting the
      # exact page path here keeps deep links working outside Vercel too, while
      # leaving assets, health checks, WebSockets, and unknown paths untouched.
      request$PATH_INFO <- "/"
    }
    original_handler(request)
  }
  app
}
