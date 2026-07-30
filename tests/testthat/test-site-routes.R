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

test_that("the Shiny handler serves only exact public routes from the app root", {
  app <- structure(
    list(httpHandler = function(request) request$PATH_INFO),
    class = "shiny.appobj"
  )
  routed <- .site_route_env$ena3d_enable_site_routes(app)

  expect_identical(routed$httpHandler(list(PATH_INFO = "/team")), "/")
  redirect <- routed$httpHandler(list(
    PATH_INFO = "/team/",
    QUERY_STRING = "from=shared"
  ))
  expect_s3_class(redirect, "httpResponse")
  expect_identical(redirect$status, 308L)
  expect_identical(redirect$headers$Location, "/team?from=shared")
  expect_identical(routed$httpHandler(list(PATH_INFO = "/app")), "/")
  expect_identical(
    routed$httpHandler(list(PATH_INFO = "/ena3d-health/healthz.json")),
    "/ena3d-health/healthz.json"
  )
  expect_identical(
    routed$httpHandler(list(PATH_INFO = "/not-a-page")),
    "/not-a-page"
  )
})

test_that("browser routing synchronizes Shiny tabs with History API", {
  javascript <- paste(
    readLines(
      file.path(.site_route_root, "R", "www", "site_routes.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  for (literal in c(
    'home: "/"',
    'tool: "/app"',
    'papers: "/papers"',
    'team: "/team"',
    'about: "/about"',
    'window.history.pushState',
    'window.history.replaceState',
    'window.addEventListener("popstate"',
    'document.addEventListener("shiny:connected"',
    'nav.addEventListener("shown.bs.tab"',
    'link.setAttribute("data-bs-target", tabTarget)',
    'link.setAttribute("href", route)',
    'aria-current'
  )) {
    expect_match(javascript, literal, fixed = TRUE)
  }
})

test_that("Vercel rewrites every deep link to the shared application document", {
  skip_if_not_installed("jsonlite")
  config <- jsonlite::read_json(
    file.path(.site_route_root, "vercel.json"),
    simplifyVector = TRUE
  )
  expected <- data.frame(
    source = c("/app", "/papers", "/team", "/about"),
    destination = rep("/", 4L),
    stringsAsFactors = FALSE
  )

  expect_equal(config$rewrites, expected)
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
})

test_that("the application loads and enables the public router", {
  app_source <- paste(
    readLines(file.path(.site_route_root, "R", "app.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(app_source, ".ena3d_source('site_routes.R')", fixed = TRUE)
  expect_match(app_source, 'src = paste0(\n          "/site_routes.js?v="', fixed = TRUE)
  expect_match(app_source, "ena3d_enable_site_routes(ena3d_app)", fixed = TRUE)
})
