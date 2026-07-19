library(testthat)

.production_test_root <- c(".", "../..", "..")
.production_test_root <- .production_test_root[file.exists(
  file.path(.production_test_root, "Dockerfile")
)][1L]
if (is.na(.production_test_root)) stop("Could not locate the project root.")
.production_test_root <- normalizePath(.production_test_root, mustWork = TRUE)

.read_project_file <- function(...) {
  paste(readLines(file.path(.production_test_root, ...), warn = FALSE),
        collapse = "\n")
}


test_that("application shell exposes build provenance and accessible controls", {
  source <- .read_project_file("R", "app.R")

  expect_match(source, "Version ", fixed = TRUE)
  expect_match(source, "config$build_id", fixed = TRUE)
  expect_match(source, "aria-expanded", fixed = TRUE)
  expect_match(source, "aria-controls", fixed = TRUE)
  expect_match(source, "aria-label", fixed = TRUE)
  expect_match(source, "aria-live", fixed = TRUE)
  expect_match(source, "ena3d-fullscreen-status", fixed = TRUE)
  expect_match(source, "setFullscreenStatus", fixed = TRUE)
  expect_match(source, "@media (max-width: 575.98px)", fixed = TRUE)
  expect_false(grepl('style = "height:93vh;"', source, fixed = TRUE))
})


test_that("app.R uses one rooted source path and no duplicate server modules", {
  source <- .read_project_file("R", "app.R")

  expect_match(source, ".ena3d_project_root", fixed = TRUE)
  expect_match(source, ".ena3d_source('app_server.R')", fixed = TRUE)
  expect_false(grepl(
    ".ena3d_source('app_module_ena_comparison_plot.R')",
    source,
    fixed = TRUE
  ))
  expect_match(source, "mustWork = TRUE", fixed = TRUE)
})


test_that("production artifacts pin the runtime and 3dena.com proxy", {
  required <- c(
    "VERSION", "renv.lock", "Dockerfile", "compose.production.yaml",
    "compose.qwen.yaml", "DEPLOYMENT.md", ".env.example", ".gitignore",
    ".dockerignore", ".Rprofile",
    file.path("renv", "activate.R"),
    file.path("docs", "AI_INTERPRETATION.md"),
    file.path("docs", "ENA3D_EXCHANGE_V1.md"),
    file.path("docs", "ena3d-exchange-v1.schema.json"),
    file.path("tools", "convert_trusted_rdata_to_ena3d_json.R"),
    file.path("deploy", "nginx", "3dena.com.conf.example")
  )
  expect_true(all(file.exists(file.path(.production_test_root, required))))

  lock <- jsonlite::read_json(file.path(.production_test_root, "renv.lock"))
  expect_identical(lock$R$Version, "4.4.1")
  expect_identical(lock$Packages$rENA$Version, "0.2.7")
  expect_identical(lock$Packages$shiny$Version, "1.9.1")
  expect_identical(lock$Packages$zip$Version, "2.3.1")
  expect_identical(lock$Packages$readxl$Version, "1.4.3")
  expect_identical(lock$Packages$curl$Version, "6.0.0")

  dockerfile <- .read_project_file("Dockerfile")
  dockerignore <- .read_project_file(".dockerignore")
  compose <- .read_project_file("compose.production.yaml")
  qwen_compose <- .read_project_file("compose.qwen.yaml")
  env_example <- .read_project_file(".env.example")
  nginx <- .read_project_file("deploy", "nginx", "3dena.com.conf.example")
  expect_match(dockerfile, "USER ena3d:ena3d", fixed = TRUE)
  expect_match(dockerfile, "R_LIBS_USER=/opt/renv/library", fixed = TRUE)
  expect_match(
    dockerfile,
    "packagemanager.posit.co/cran/__linux__/jammy/latest",
    fixed = TRUE
  )
  expect_match(dockerfile, 'normalizePath("/opt/renv/library") %in% .libPaths()',
               fixed = TRUE)
  expect_match(dockerfile, "COPY images ./images", fixed = TRUE)
  expect_false(any(trimws(strsplit(dockerignore, "\n", fixed = TRUE)[[1L]]) ==
                   "images"))
  expect_match(dockerfile, "/ena3d-health/healthz.json", fixed = TRUE)
  expect_match(dockerfile, 'requireNamespace("curl", quietly=TRUE)',
               fixed = TRUE)
  expect_match(compose, "read_only: true", fixed = TRUE)
  expect_match(compose, '"127.0.0.1:3838:3838"', fixed = TRUE)
  expect_match(compose, 'ENA3D_MAX_EXCHANGE_FILE_BYTES: "2097152"',
               fixed = TRUE)
  expect_match(compose, 'ENA3D_MAX_RAW_FILE_BYTES: "5242880"',
               fixed = TRUE)
  expect_match(compose, 'ENA3D_AI_ENABLED: "false"', fixed = TRUE)
  expect_false(grepl("DASHSCOPE_API_KEY", compose, fixed = TRUE))
  expect_match(qwen_compose, 'ENA3D_AI_ENABLED: "true"', fixed = TRUE)
  expect_match(
    qwen_compose,
    'ENA3D_QWEN_MODEL: ${ENA3D_QWEN_MODEL:-qwen3.7-max-2026-06-08}',
    fixed = TRUE
  )
  expect_match(qwen_compose, "ENA3D_QWEN_MAX_COMPLETION_TOKENS", fixed = TRUE)
  expect_match(qwen_compose, "ENA3D_QWEN_THINKING_BUDGET", fixed = TRUE)
  expect_false(grepl("ENA3D_QWEN_MAX_TOKENS", qwen_compose, fixed = TRUE))
  expect_match(
    qwen_compose,
    "DASHSCOPE_API_KEY_FILE: /run/secrets/dashscope_api_key",
    fixed = TRUE
  )
  expect_match(qwen_compose, "ENA3D_DASHSCOPE_SECRET_FILE", fixed = TRUE)
  expect_false(grepl("DASHSCOPE_API_KEY:", qwen_compose, fixed = TRUE))
  expect_match(env_example, "ENA3D_AI_ENABLED=false", fixed = TRUE)
  expect_match(
    env_example, "ENA3D_QWEN_MODEL=qwen3.7-max-2026-06-08", fixed = TRUE
  )
  expect_match(
    env_example, "ENA3D_QWEN_MAX_COMPLETION_TOKENS=4096", fixed = TRUE
  )
  expect_match(env_example, "ENA3D_QWEN_THINKING_BUDGET=1536", fixed = TRUE)
  expect_false(grepl("ENA3D_QWEN_MAX_TOKENS", env_example, fixed = TRUE))
  expect_false(grepl("DASHSCOPE_API_KEY=", env_example, fixed = TRUE))
  expect_match(nginx, "server_name 3dena.com;", fixed = TRUE)
  expect_match(nginx, "server_name 3dena.com www.3dena.com;", fixed = TRUE)
  expect_match(nginx, "server_name www.3dena.com;", fixed = TRUE)
  expect_match(nginx, "return 301 https://3dena.com$request_uri;", fixed = TRUE)
  expect_match(nginx, "proxy_set_header Upgrade", fixed = TRUE)
  expect_false(grepl("server_name www.ena3d.org", nginx, fixed = TRUE))
})


test_that("project activation controls a clean R process library", {
  skip_if_not_installed("processx")

  project_library <- tempfile("ena3d-test-library-")
  dir.create(project_library)
  on.exit(unlink(project_library, recursive = TRUE, force = TRUE), add = TRUE)
  copied <- file.copy(
    find.package("zip"),
    project_library,
    recursive = TRUE
  )
  expect_true(copied)

  expression <- paste(
    "expected <- normalizePath(Sys.getenv('RENV_PATHS_LIBRARY'))",
    "stopifnot(identical(.libPaths()[[1L]], expected))",
    "stopifnot(requireNamespace('zip', quietly=TRUE))",
    "stopifnot(startsWith(normalizePath(find.package('zip')), expected))",
    sep = "; "
  )
  result <- processx::run(
    file.path(R.home("bin"), "Rscript"),
    c("--no-site-file", "-e", expression),
    wd = .production_test_root,
    env = c(RENV_PATHS_LIBRARY = project_library),
    error_on_status = FALSE,
    echo = FALSE
  )
  expect_identical(result$status, 0L, info = result$stderr)
})


test_that("bootstrap preserves configured binary repositories", {
  bootstrap <- .read_project_file("renv", "bootstrap.R")
  expect_match(
    bootstrap,
    'Sys.getenv("RENV_CONFIG_REPOS_OVERRIDE"',
    fixed = TRUE
  )
  expect_match(bootstrap, 'getOption("repos")', fixed = TRUE)
  expect_match(bootstrap, 'repos != "@CRAN@"', fixed = TRUE)
  expect_match(bootstrap, 'https://cloud.r-project.org', fixed = TRUE)
})
