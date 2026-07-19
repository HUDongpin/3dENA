ena3d_asset_mime_type <- function(path) {
  extension <- tolower(tools::file_ext(path))
  switch(
    extension,
    css = "text/css",
    js = "text/javascript",
    svg = "image/svg+xml",
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    gif = "image/gif",
    webp = "image/webp",
    woff = "font/woff",
    woff2 = "font/woff2",
    ttf = "font/ttf",
    eot = "application/vnd.ms-fontobject",
    "application/octet-stream"
  )
}

ena3d_asset_data_uri <- function(path) {
  paste0(
    "data:",
    ena3d_asset_mime_type(path),
    ";base64,",
    base64enc::base64encode(path)
  )
}

ena3d_replace_matches <- function(text, pattern, replacement) {
  locations <- gregexpr(pattern, text, perl = TRUE)[[1L]]
  if (identical(locations[[1L]], -1L)) {
    return(text)
  }

  lengths <- attr(locations, "match.length")
  matches <- regmatches(text, list(locations))[[1L]]
  for (index in rev(seq_along(locations))) {
    start <- locations[[index]]
    end <- start + lengths[[index]] - 1L
    text <- paste0(
      if (start > 1L) substr(text, 1L, start - 1L) else "",
      replacement(matches[[index]]),
      if (end < nchar(text)) substr(text, end + 1L, nchar(text)) else ""
    )
  }
  text
}

ena3d_inline_css_urls <- function(css, css_path) {
  ena3d_replace_matches(css, "url\\([^)]*\\)", function(reference) {
    value <- trimws(sub("^url\\((.*)\\)$", "\\1", reference))
    value <- sub("^[\"']", "", value)
    value <- sub("[\"']$", "", value)
    if (!nzchar(value) || grepl("^(data:|https?:|//|#)", value)) {
      return(reference)
    }

    relative_path <- utils::URLdecode(sub("[?#].*$", "", value))
    asset_path <- file.path(dirname(css_path), relative_path)
    if (!file.exists(asset_path)) {
      return(reference)
    }
    paste0("url(\"", ena3d_asset_data_uri(asset_path), "\")")
  })
}

ena3d_resolve_asset_path <- function(url, resource_paths, www_dir) {
  clean_url <- utils::URLdecode(sub("[?#].*$", "", sub("^/", "", url)))
  pieces <- strsplit(clean_url, "/", fixed = TRUE)[[1L]]
  prefix <- pieces[[1L]]

  if (prefix %in% names(resource_paths) && length(pieces) > 1L) {
    return(file.path(
      as.character(resource_paths[[prefix]]),
      paste(pieces[-1L], collapse = "/")
    ))
  }
  file.path(www_dir, clean_url)
}

ena3d_inline_ui_assets <- function(html, www_dir, max_inline_script = 1024^2) {
  resource_paths <- shiny::resourcePaths()

  html <- ena3d_replace_matches(
    html,
    "<link[^>]+href=\"[^\"]+\"[^>]*/?>",
    function(tag) {
      url <- sub('^.*href="([^"]+)".*$', "\\1", tag)
      path <- ena3d_resolve_asset_path(url, resource_paths, www_dir)
      if (!file.exists(path)) {
        return(tag)
      }

      if (grepl('rel="stylesheet"', tag, fixed = TRUE)) {
        css <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
        css <- ena3d_inline_css_urls(css, path)
        css <- gsub("</style", "<\\/style", css, fixed = TRUE)
        return(paste0('<style data-inline-href="', url, '">', css, "</style>"))
      }

      if (grepl('rel="icon"', tag, fixed = TRUE)) {
        return(gsub(url, ena3d_asset_data_uri(path), tag, fixed = TRUE))
      }
      tag
    }
  )

  html <- ena3d_replace_matches(
    html,
    '<script[^>]+src="[^"]+"[^>]*></script>',
    function(tag) {
      url <- sub('^.*src="([^"]+)".*$', "\\1", tag)
      path <- ena3d_resolve_asset_path(url, resource_paths, www_dir)
      if (!file.exists(path) || file.info(path)$size > max_inline_script) {
        return(tag)
      }

      script <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      script <- gsub("</script", "<\\/script", script, fixed = TRUE)
      opening_tag <- sub("</script>$", "", tag)
      opening_tag <- gsub(paste0(' src="', url, '"'), "", opening_tag, fixed = TRUE)
      paste0(opening_tag, script, "</script>")
    }
  )

  html <- ena3d_replace_matches(
    html,
    '<img[^>]+src="ena3d-assets/[^"]+"[^>]*>',
    function(tag) {
      url <- sub('^.*src="([^"]+)".*$', "\\1", tag)
      path <- ena3d_resolve_asset_path(url, resource_paths, www_dir)
      if (identical(basename(path), "peter-hu-portrait.png")) {
        web_portrait <- file.path(dirname(path), "peter-hu-portrait-web.jpg")
        if (file.exists(web_portrait)) {
          path <- web_portrait
        }
      }
      if (!file.exists(path)) {
        return(tag)
      }
      gsub(url, ena3d_asset_data_uri(path), tag, fixed = TRUE)
    }
  )

  # Bootstrap 5 normally upgrades Shiny's legacy tab markup during its own
  # external-script lifecycle. When those dependencies are embedded, make the
  # equivalent idempotent upgrade before DOMContentLoaded so Shiny's tab input
  # binding reports its initial value and can receive server-side updates.
  tab_markup_upgrade <- paste0(
    "<script>(function(){",
    "document.querySelectorAll('.shiny-tab-input').forEach(function(tablist){",
    "tablist.setAttribute('role','tablist');",
    "tablist.querySelectorAll(':scope > li').forEach(function(item){",
    "var link=item.querySelector(':scope > a[data-toggle=tab],:scope > a[data-bs-toggle=tab]');",
    "if(!link)return;",
    "var active=item.classList.contains('active')||link.classList.contains('active');",
    "item.classList.add('nav-item');item.classList.remove('active');",
    "item.setAttribute('role','presentation');",
    "link.classList.add('nav-link');link.classList.toggle('active',active);",
    "link.setAttribute('role','tab');link.setAttribute('aria-selected',active?'true':'false');",
    "if(active)link.removeAttribute('tabindex');else link.setAttribute('tabindex','-1');",
    "});});",
    "document.querySelectorAll('.tab-pane.active').forEach(function(panel){panel.classList.add('show');});",
    "})();</script>"
  )
  html <- sub("</body>", paste0(tab_markup_upgrade, "</body>"), html, fixed = TRUE)

  html
}

ena3d_render_inline_ui <- function(ui, www_dir) {
  plotly_probe <- plotly::plot_ly(x = 0, y = 0)
  ui <- htmltools::attachDependencies(
    ui,
    plotly_probe$dependencies,
    append = TRUE
  )
  html <- shiny:::renderPage(ui)
  ena3d_inline_ui_assets(html, www_dir)
}

ena3d_register_plotly_resources <- function() {
  dependencies <- plotly::plot_ly(x = 0, y = 0)$dependencies
  dependency <- dependencies[[which(vapply(
    dependencies,
    function(candidate) identical(candidate$name, "plotly-main"),
    logical(1)
  ))[[1L]]]]
  dependency$src$file <- system.file(
    dependency$src$file,
    package = dependency$package
  )
  invisible(shiny:::createWebDependency(dependency))
}
