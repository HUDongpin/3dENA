library(testthat)

.site_route_roots <- c(".", "..", "../..")
.site_route_root <- .site_route_roots[file.exists(
  file.path(.site_route_roots, "R", "site_routes.R")
)][1L]
if (is.na(.site_route_root)) stop("Could not locate the 3D ENA project root.")
.site_route_root <- normalizePath(.site_route_root)

.site_route_env <- new.env(parent = globalenv())
sys.source(
  file.path(.site_route_root, "R", "site_routes.R"),
  envir = .site_route_env
)

test_that("public site pages have stable paths", {
  expect_identical(
    .site_route_env$ena3d_site_routes,
    c(
      home = "/",
      tool = "/app",
      papers = "/papers",
      team = "/team",
      about = "/about"
    )
  )
  expect_identical(
    unname(vapply(
      c("/", "/app/", "/papers///", "team", "about?from=test"),
      .site_route_env$ena3d_normalize_site_path,
      character(1)
    )),
    c("/", "/app", "/papers", "/team", "/about")
  )
  expect_true(.site_route_env$ena3d_is_site_path("/team/"))
  expect_false(.site_route_env$ena3d_is_site_path("/not-a-page"))
})

test_that("static and stateful routes are disjoint and exhaustive", {
  expect_identical(
    .site_route_env$ena3d_static_site_routes,
    c(
      home = "/",
      papers = "/papers",
      team = "/team",
      about = "/about"
    )
  )
  expect_identical(
    .site_route_env$ena3d_stateful_site_routes,
    c(tool = "/app")
  )
  expect_length(intersect(
    unname(.site_route_env$ena3d_static_site_routes),
    unname(.site_route_env$ena3d_stateful_site_routes)
  ), 0L)
  expect_setequal(
    c(
      unname(.site_route_env$ena3d_static_site_routes),
      unname(.site_route_env$ena3d_stateful_site_routes)
    ),
    unname(.site_route_env$ena3d_site_routes)
  )

  for (path in c("/", "/papers/", "team", "/about?from=test")) {
    expect_false(
      .site_route_env$ena3d_route_requires_session(path),
      info = path
    )
  }
  expect_true(.site_route_env$ena3d_route_requires_session("/app"))
  expect_true(.site_route_env$ena3d_route_requires_session("/app/?from=test"))
})

test_that("only the analysis route enters the Shiny handler", {
  shiny_calls <- character()
  static_calls <- character()
  app <- structure(
    list(httpHandler = function(request) {
      shiny_calls <<- c(shiny_calls, request$PATH_INFO)
      request$PATH_INFO
    }),
    class = "shiny.appobj"
  )
  static_handler <- function(request) {
    static_calls <<- c(static_calls, request$PATH_INFO)
    paste0("static:", request$PATH_INFO)
  }
  routed <- .site_route_env$ena3d_enable_site_routes(
    app,
    static_handler = static_handler
  )

  for (path in unname(.site_route_env$ena3d_static_site_routes)) {
    expect_identical(
      routed$httpHandler(list(PATH_INFO = path)),
      paste0("static:", path),
      info = path
    )
  }
  expect_identical(static_calls, unname(.site_route_env$ena3d_static_site_routes))
  expect_length(shiny_calls, 0L)

  redirect <- routed$httpHandler(list(
    PATH_INFO = "/team/",
    QUERY_STRING = "from=shared"
  ))
  expect_s3_class(redirect, "httpResponse")
  expect_identical(redirect$status, 308L)
  expect_identical(redirect$headers$Location, "/team?from=shared")
  expect_identical(routed$httpHandler(list(PATH_INFO = "/app")), "/")
  expect_identical(
    routed$httpHandler(list(PATH_INFO = "/__ena3d-app")),
    "/"
  )
  expect_identical(shiny_calls, c("/", "/"))
  expect_identical(
    routed$httpHandler(list(PATH_INFO = "/ena3d-health/healthz.json")),
    "/ena3d-health/healthz.json"
  )
  expect_identical(
    routed$httpHandler(list(PATH_INFO = "/app/websocket/")),
    "/app/websocket/"
  )
  expect_identical(
    routed$httpHandler(list(PATH_INFO = "/__sockjs__/info")),
    "/__sockjs__/info"
  )
  expect_identical(
    routed$httpHandler(list(PATH_INFO = "/not-a-page")),
    "/not-a-page"
  )
})

test_that("static browser routing uses History API without Shiny", {
  javascript <- paste(
    readLines(
      file.path(.site_route_root, "R", "www", "site_routes.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  for (literal in c(
    'home: "/"',
    'papers: "/papers"',
    'team: "/team"',
    'about: "/about"',
    'window.history.pushState',
    'window.history.replaceState',
    'window.addEventListener("popstate"',
    'nav.addEventListener("shown.bs.tab"',
    'link.setAttribute("data-bs-target", tabTarget)',
    'link.setAttribute("href", route)',
    'aria-current',
    'handleCitationCopy'
  )) {
    expect_match(javascript, literal, fixed = TRUE)
  }
  expect_false(grepl('tool: "/app"', javascript, fixed = TRUE))
  expect_false(grepl("shiny:connected", javascript, fixed = TRUE))
  expect_false(grepl("window.Shiny", javascript, fixed = TRUE))
})

test_that("Vercel selects the stateful document only for /app", {
  skip_if_not_installed("jsonlite")
  config <- jsonlite::read_json(
    file.path(.site_route_root, "vercel.json"),
    simplifyVector = TRUE
  )
  expected <- data.frame(
    source = c("/app", "/papers", "/team", "/about"),
    destination = c("/__ena3d-app", "/", "/", "/"),
    stringsAsFactors = FALSE
  )

  expect_equal(config$rewrites, expected)
  expect_identical(
    config$rewrites$source[config$rewrites$destination == "/__ena3d-app"],
    "/app"
  )
})

test_that("the bounded Vercel preview listens on the platform container port", {
  dockerfile <- paste(
    readLines(
      file.path(.site_route_root, "Dockerfile.vercel"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(dockerfile, "PORT=80", fixed = TRUE)
  expect_match(dockerfile, "EXPOSE 80", fixed = TRUE)
  expect_match(dockerfile, "${PORT:-80}", fixed = TRUE)
  expect_false(grepl("PORT=3838", dockerfile, fixed = TRUE))
  expect_match(
    dockerfile,
    'CMD ["Rscript", "--vanilla", "-e"',
    fixed = TRUE
  )
  expect_match(
    dockerfile,
    'required <- c("shiny", "plotly", "data.table", "R6", "rENA"',
    fixed = TRUE
  )
})

test_that("the Vercel preview resolves platform Git provenance at runtime", {
  dockerfile <- paste(
    readLines(
      file.path(.site_route_root, "Dockerfile.vercel"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  app_source <- paste(
    readLines(file.path(.site_route_root, "R", "app.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(dockerfile, "VERCEL=1", fixed = TRUE)
  expect_false(grepl("ARG VERCEL_GIT_COMMIT_SHA=", dockerfile, fixed = TRUE))
  expect_false(grepl(
    "VERCEL_GIT_COMMIT_SHA=${VERCEL_GIT_COMMIT_SHA}",
    dockerfile,
    fixed = TRUE
  ))
  expect_match(
    dockerfile,
    "RUN VERCEL= VERCEL_GIT_COMMIT_SHA= ENA3D_BUILD_ID=__ENA3D_BUILD_ID__",
    fixed = TRUE
  )
  expect_match(app_source, "config$build_id = ena3d_resolve_build_id()",
               fixed = TRUE)
  expect_match(app_source, "ENA3D_GIT_COMMIT = config$build_id", fixed = TRUE)
})

test_that("the application loads and enables the public router", {
  app_source <- paste(
    readLines(file.path(.site_route_root, "R", "app.R"), warn = FALSE),
    collapse = "\n"
  )
  static_source <- paste(
    readLines(file.path(.site_route_root, "R", "static_site.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(app_source, ".ena3d_source('site_routes.R')", fixed = TRUE)
  expect_match(app_source, ".ena3d_source('static_site.R')", fixed = TRUE)
  expect_false(grepl('"/site_routes.js?v="', app_source, fixed = TRUE))
  expect_match(static_source, 'src = paste0("/site_routes.js?v="', fixed = TRUE)
  expect_match(
    app_source,
    "static_handler = static_site_http_handler",
    fixed = TRUE
  )
})
