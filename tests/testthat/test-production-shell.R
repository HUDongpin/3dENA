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


test_that("application shell retains build provenance without displaying it", {
  source <- .read_project_file("R", "app.R")

  expect_match(source, "config$build_id", fixed = TRUE)
  expect_false(grepl("ena3d-build-id", source, fixed = TRUE))
  expect_false(grepl('" · Build "', source, fixed = TRUE))
  expect_match(source, "aria-expanded", fixed = TRUE)
  expect_match(source, "aria-controls", fixed = TRUE)
  expect_match(source, "aria-label", fixed = TRUE)
  expect_match(source, "aria-live", fixed = TRUE)
  expect_match(source, "ena3d-fullscreen-status", fixed = TRUE)
  expect_match(source, "setFullscreenStatus", fixed = TRUE)
  expect_match(source, "@media (max-width: 575.98px)", fixed = TRUE)
  expect_false(grepl('style = "height:93vh;"', source, fixed = TRUE))
})


test_that("application shell selects analytics by deployment provider", {
  app_source <- .read_project_file("R", "app.R")
  inline_source <- .read_project_file("R", "inline_ui.R")

  expect_match(app_source, "ena3d_analytics_tags()", fixed = TRUE)
  expect_match(inline_source, 'unset = "none"', fixed = TRUE)
  expect_match(inline_source, "none = htmltools::tagList()", fixed = TRUE)
  expect_match(inline_source, "window.va = window.va || function", fixed = TRUE)
  expect_match(
    inline_source,
    "/_vercel/insights/script.js",
    fixed = TRUE
  )
  expect_match(inline_source, "data-analytics-provider", fixed = TRUE)
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
    file.path("deploy", "nginx", "3dena.com.conf.example"),
    file.path("deploy", "shiny-server.conf"),
    file.path("deploy", "ena3d-entrypoint.sh"),
    file.path("deploy", "write-runtime-env.R")
  )
  expect_true(all(file.exists(file.path(.production_test_root, required))))

  lock <- jsonlite::read_json(file.path(.production_test_root, "renv.lock"))
  expect_identical(lock$R$Version, "4.4.1")
  expect_identical(lock$Packages$rENA$Version, "0.2.7")
  expect_identical(lock$Packages$shiny$Version, "1.9.1")
  expect_identical(lock$Packages$zip$Version, "2.3.1")
  expect_identical(lock$Packages$readxl$Version, "1.4.3")
  expect_identical(lock$Packages$curl$Version, "6.0.0")
  expect_identical(lock$Packages$bit$Version, "4.6.0")
  expect_identical(lock$Packages$bit$Source, "Repository")
  expect_identical(lock$Packages$bit64$Version, "4.8.2")
  expect_identical(lock$Packages$bit64$Source, "Repository")

  dockerfile <- .read_project_file("Dockerfile")
  dockerignore <- .read_project_file(".dockerignore")
  compose <- .read_project_file("compose.production.yaml")
  qwen_compose <- .read_project_file("compose.qwen.yaml")
  env_example <- .read_project_file(".env.example")
  nginx <- .read_project_file("deploy", "nginx", "3dena.com.conf.example")
  shiny_server <- .read_project_file("deploy", "shiny-server.conf")
  entrypoint <- .read_project_file("deploy", "ena3d-entrypoint.sh")
  env_writer <- .read_project_file("deploy", "write-runtime-env.R")
  expect_match(dockerfile, "USER ena3d:ena3d", fixed = TRUE)
  expect_match(
    dockerfile,
    paste0(
      "rocker/r-ver:4.4.1@sha256:",
      "f3ef082e63ca36547fcf0c05a0d74255ddda6ca7bd88f1dae5a44ce117fc3804"
    ),
    fixed = TRUE
  )
  expect_match(dockerfile, "SHINY_SERVER_VERSION=1.5.23.1030", fixed = TRUE)
  expect_match(
    dockerfile,
    "ARG UBUNTU_ARCHIVE_MIRROR=https://archive.ubuntu.com",
    fixed = TRUE
  )
  expect_match(
    dockerfile,
    "ARG UBUNTU_SECURITY_MIRROR=https://security.ubuntu.com",
    fixed = TRUE
  )
  expect_match(dockerfile, "Acquire::Retries=5", fixed = TRUE)
  expect_match(
    dockerfile,
    "s#http://archive.ubuntu.com#${UBUNTU_ARCHIVE_MIRROR}#g",
    fixed = TRUE
  )
  expect_match(
    dockerfile,
    "s#http://security.ubuntu.com#${UBUNTU_SECURITY_MIRROR}#g",
    fixed = TRUE
  )
  expect_match(
    dockerfile,
    "4a3d063a06ccd1b6c53eb1d7f4fb59965bced10d1c5c87e8c476b58dd6fd35ee",
    fixed = TRUE
  )
  expect_match(dockerfile, "--retry 10", fixed = TRUE)
  expect_match(dockerfile, "--retry-all-errors", fixed = TRUE)
  expect_match(
    dockerfile,
    "--mount=type=cache,target=/var/cache/ena3d-downloads,sharing=locked",
    fixed = TRUE
  )
  expect_match(dockerfile, "--max-time 300", fixed = TRUE)
  expect_match(dockerfile, "--max-time 120", fixed = TRUE)
  expect_match(dockerfile, "--speed-time 30", fixed = TRUE)
  expect_match(dockerfile, "--speed-limit 1024", fixed = TRUE)
  expect_match(dockerfile, "--continue-at -", fixed = TRUE)
  expect_match(
    dockerfile,
    "RENV_CONFIG_CACHE_SYMLINKS=FALSE",
    fixed = TRUE
  )
  expect_match(
    dockerfile,
    "--mount=type=cache,target=/opt/renv/cache,sharing=locked",
    fixed = TRUE
  )
  expect_match(dockerfile, "for attempt in 1 2 3 4", fixed = TRUE)
  expect_match(dockerfile, "exit \"${restore_status}\"", fixed = TRUE)
  expect_gt(
    regexpr("ARG ENA3D_BUILD_ID=development", dockerfile, fixed = TRUE)[[1L]],
    regexpr("Rscript renv/bootstrap.R", dockerfile, fixed = TRUE)[[1L]]
  )
  expect_match(
    dockerfile,
    "/usr/local/share/ena3d/provenance/build-id",
    fixed = TRUE
  )
  expect_match(
    dockerfile,
    "/usr/local/share/ena3d/provenance/app-version",
    fixed = TRUE
  )
  expect_match(dockerfile, "chmod 0444", fixed = TRUE)
  expect_gt(
    regexpr(
      "ARG SHINY_SERVER_VERSION=1.5.23.1030",
      dockerfile,
      fixed = TRUE
    )[[1L]],
    regexpr("apt-get install", dockerfile, fixed = TRUE)[[1L]]
  )
  expect_match(dockerfile, 'ENTRYPOINT ["/usr/local/bin/ena3d-entrypoint"]',
               fixed = TRUE)
  expect_match(
    dockerfile,
    'CMD ["/usr/bin/shiny-server", "/etc/shiny-server/shiny-server.conf"]',
               fixed = TRUE)
  expect_false(grepl("shiny::runApp", dockerfile, fixed = TRUE))
  expect_false(grepl(
    "SHINY_SERVER_VERSION=${SHINY_SERVER_VERSION}",
    dockerfile,
    fixed = TRUE
  ))
  expect_match(dockerfile, "SHINY_LOG_STDERR=1", fixed = TRUE)
  expect_match(dockerfile, "R_LIBS_USER=/opt/renv/library", fixed = TRUE)
  expect_match(
    dockerfile,
    "ARG RENV_REPOSITORY=https://cloud.r-project.org",
    fixed = TRUE
  )
  expect_match(dockerfile, "RENV_CONFIG_PPM_ENABLED=FALSE", fixed = TRUE)
  expect_match(dockerfile, "ARG RENV_VERSION=1.1.8", fixed = TRUE)
  expect_match(
    dockerfile,
    "141b3a77a9e405eb1b586db7bda43088825b955ceacc1a2de0322a4fcf78ae08",
    fixed = TRUE
  )
  expect_match(dockerfile, 'attempt_library="/opt/renv/restore-library-',
               fixed = TRUE)
  expect_match(dockerfile, 'normalizePath("/opt/renv/library") %in% .libPaths()',
               fixed = TRUE)
  expect_match(dockerfile, "COPY images ./images", fixed = TRUE)
  expect_false(any(trimws(strsplit(dockerignore, "\n", fixed = TRUE)[[1L]]) ==
                   "images"))
  expect_match(dockerfile, "/ena3d-health/healthz.json", fixed = TRUE)
  expect_match(dockerfile, "vapply(required, requireNamespace",
               fixed = TRUE)
  expect_match(dockerfile, '"bit", "bit64"', fixed = TRUE)
  expect_match(compose, "read_only: true", fixed = TRUE)
  expect_match(compose, "platform: linux/amd64", fixed = TRUE)
  expect_match(compose, "stop_grace_period: 30s", fixed = TRUE)
  expect_match(compose, '"127.0.0.1:3838:3838"', fixed = TRUE)
  expect_match(compose, 'ENA3D_RUNTIME_PROFILE: "persistent"', fixed = TRUE)
  expect_false(grepl(
    "(?m)^ {6}ENA3D_BUILD_ID:",
    compose,
    perl = TRUE
  ))
  expect_false(grepl(
    "(?m)^ {6}ENA3D_APP_VERSION:",
    compose,
    perl = TRUE
  ))
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
  expect_match(env_example, "ENA3D_RUNTIME_PROFILE=persistent", fixed = TRUE)
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
  expect_match(nginx, "client_max_body_size 6m;", fixed = TRUE)
  expect_match(nginx, "proxy_read_timeout 86400s;", fixed = TRUE)
  expect_match(nginx, "location ^~ /__sockjs__/", fixed = TRUE)
  expect_identical(
    lengths(regmatches(
      nginx,
      gregexpr(
        "limit_req zone=ena3d_per_ip burst=120 nodelay;",
        nginx,
        fixed = TRUE
      )
    )),
    2L
  )
  expect_true(grepl(
    "location \\^~ /__sockjs__/ \\{[^}]*access_log off;",
    nginx,
    perl = TRUE
  ))
  expect_match(shiny_server, "run_as ena3d;", fixed = TRUE)
  expect_match(shiny_server, "reconnect true;", fixed = TRUE)
  expect_match(shiny_server, "app_idle_timeout 60;", fixed = TRUE)
  expect_match(shiny_server, "preserve_logs false;", fixed = TRUE)
  expect_false(grepl("app_session_timeout", shiny_server, fixed = TRUE))
  expect_match(entrypoint, "never copy DASHSCOPE_API_KEY", fixed = TRUE)
  expect_match(entrypoint, "write-runtime-env.R", fixed = TRUE)
  expect_match(entrypoint, "runtime provenance does not match image provenance",
               fixed = TRUE)
  expect_match(entrypoint, "exit 78", fixed = TRUE)
  expect_lt(
    regexpr("image_build_id=", entrypoint, fixed = TRUE)[[1L]],
    regexpr("write-runtime-env.R", entrypoint, fixed = TRUE)[[1L]]
  )
  expect_match(entrypoint, 'exec "$@"', fixed = TRUE)
  expect_match(env_writer, '"DASHSCOPE_API_KEY_FILE"', fixed = TRUE)
  expect_false(grepl('"SHINY_SERVER_VERSION"', env_writer, fixed = TRUE))
  expect_false(grepl('"DASHSCOPE_API_KEY",', env_writer, fixed = TRUE))
  expect_match(env_writer, "do.call(Sys.setenv, ena3d_runtime_env)",
               fixed = TRUE)
  expect_match(
    env_writer,
    ".libPaths(unique(c(ena3d_library, .libPaths())))",
    fixed = TRUE
  )
  expect_false(grepl(
    "write_renviron_setting DASHSCOPE_API_KEY",
    entrypoint,
    fixed = TRUE
  ))
  expect_false(grepl("server_name www.ena3d.org", nginx, fixed = TRUE))
})


test_that("pre-release gate proves long-lived same-session recovery", {
  required <- c(
    file.path(".github", "workflows", "pre-release-audit.yml"),
    file.path("tools", "playwright_connection_audit", "monitor.js"),
    file.path("tools", "playwright_connection_audit", "ready.js"),
    file.path("tools", "playwright_connection_audit", "roundtrip.js"),
    file.path("tools", "playwright_connection_audit", "arm-interruption.js"),
    file.path("tools", "playwright_connection_audit", "recovery.js"),
    file.path("tools", "playwright_connection_audit", "terminal.js")
  )
  expect_true(all(file.exists(file.path(.production_test_root, required))))

  workflow <- .read_project_file(
    ".github", "workflows", "pre-release-audit.yml"
  )
  monitor <- .read_project_file(
    "tools", "playwright_connection_audit", "monitor.js"
  )
  ready <- .read_project_file(
    "tools", "playwright_connection_audit", "ready.js"
  )
  roundtrip <- .read_project_file(
    "tools", "playwright_connection_audit", "roundtrip.js"
  )
  arm_interruption <- .read_project_file(
    "tools", "playwright_connection_audit", "arm-interruption.js"
  )
  recovery <- .read_project_file(
    "tools", "playwright_connection_audit", "recovery.js"
  )
  terminal <- .read_project_file(
    "tools", "playwright_connection_audit", "terminal.js"
  )

  expect_match(workflow, "seq 0 12", fixed = TRUE)
  expect_match(workflow, 'test "$hold_seconds" -ge 360', fixed = TRUE)
  expect_match(workflow, ".roundTrips == 15", fixed = TRUE)
  expect_match(workflow, "recovery_proxy_outage_seconds: 2", fixed = TRUE)
  expect_match(workflow, "terminal_proxy_outage_seconds", fixed = TRUE)
  expect_match(workflow, "same_session_recovered", fixed = TRUE)
  expect_match(workflow, "native_disconnect_preserved", fixed = TRUE)
  expect_match(workflow, "browser-session-audit.json", fixed = TRUE)
  expect_match(workflow, "forged build ID was accepted", fixed = TRUE)
  expect_match(workflow, "forged app version was accepted", fixed = TRUE)
  expect_match(workflow, 'test "$?" -eq 78', fixed = TRUE)
  expect_match(workflow, "access_log off;", fixed = TRUE)
  expect_false(grepl("pwcli requests", workflow, fixed = TRUE))
  expect_false(grepl("pwcli tracing-start", workflow, fixed = TRUE))
  expect_false(grepl("pwcli video-start", workflow, fixed = TRUE))
  expect_false(grepl("pwcli screenshot", workflow, fixed = TRUE))
  expect_match(workflow, "stop_nginx", fixed = TRUE)
  expect_match(ready, "baselineSessionId", fixed = TRUE)
  expect_match(roundtrip, "proof.sessionId !== state.baselineSessionId", fixed = TRUE)
  expect_match(recovery, "proof.sessionId === state.baselineSessionId", fixed = TRUE)
  expect_match(workflow, "start_nginx", fixed = TRUE)
  expect_match(workflow, "sleep 2", fixed = TRUE)
  expect_match(workflow, "sleep 22", fixed = TRUE)
  expect_false(grepl("network-state-set", workflow, fixed = TRUE))

  expect_match(monitor, 'page.on("websocket"', fixed = TRUE)
  expect_false(grepl("socket.url", monitor, fixed = TRUE))
  expect_match(
    monitor,
    'eventName = "shiny:message.ena3dConnectionAudit"',
    fixed = TRUE
  )
  expect_match(monitor, '"ena3d_connection_probe"', fixed = TRUE)
  expect_match(monitor, "proof.session_id", fixed = TRUE)
  expect_match(ready, "app.$allowReconnect === false", fixed = TRUE)
  expect_match(ready, 'guard.dataset.sessionProof === "ready"',
               fixed = TRUE)
  expect_match(roundtrip, "state.closed !== state.stableClosed",
               fixed = TRUE)
  expect_match(roundtrip, "state.proveServerRoundTrip()", fixed = TRUE)
  expect_false(grepl("state.roundTrips += 1", roundtrip, fixed = TRUE))
  expect_match(arm_interruption, "state.pendingInterruption", fixed = TRUE)
  expect_match(arm_interruption, "openedBefore", fixed = TRUE)
  expect_match(arm_interruption, "closedBefore", fixed = TRUE)
  expect_match(recovery, "state.closed <= closedBefore", fixed = TRUE)
  expect_match(recovery, "state.opened <= openedBefore", fixed = TRUE)
  expect_match(recovery, "state.proveServerRoundTrip()", fixed = TRUE)
  expect_false(grepl("setOffline", recovery, fixed = TRUE))
  expect_match(
    recovery,
    "Connection restored. Your existing analysis session is unchanged.",
    fixed = TRUE
  )
  expect_match(recovery, "postRecoveryRoundTrip: true", fixed = TRUE)
  expect_match(terminal, "state.closed <= closedBefore", fixed = TRUE)
  expect_false(grepl("setOffline", terminal, fixed = TRUE))
  expect_match(terminal, "outageSeconds: 22", fixed = TRUE)
  expect_match(terminal, 'guard.dataset.state === "failed"',
               fixed = TRUE)
  expect_match(terminal, 'getElementById("ss-reload-link")',
               fixed = TRUE)
  expect_match(terminal, "newSessionReplayPrevented: Boolean(",
               fixed = TRUE)
  expect_match(terminal, "allowReconnectDisabled", fixed = TRUE)
  expect_match(terminal, "!terminalState.restoredAnnouncement", fixed = TRUE)
})


test_that("runtime R profile preserves literals and rejects injection", {
  skip_if_not_installed("processx")

  writer <- file.path(
    .production_test_root,
    "deploy",
    "write-runtime-env.R"
  )
  worker_home <- tempfile("ena3d-worker-home-")
  worker_app <- tempfile("ena3d-worker-app-")
  worker_library <- tempfile("ena3d-worker-library-")
  dir.create(worker_home)
  dir.create(worker_app)
  dir.create(worker_library)
  on.exit(
    unlink(
      c(worker_home, worker_app, worker_library),
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )
  target <- file.path(worker_home, ".Rprofile")
  safe_value <- "build-${HOME}-$literal-\"quoted\"-\\path-'"
  result <- processx::run(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", writer, target),
    env = c(
      ENA3D_BUILD_ID = safe_value,
      R_LIBS_USER = worker_library,
      DASHSCOPE_API_KEY = "must-not-cross-the-boundary"
    ),
    error_on_status = FALSE,
    echo = FALSE
  )
  expect_identical(result$status, 0L, info = result$stderr)
  expect_identical(
    as.numeric(file.info(target)$mode),
    as.numeric(as.octmode("600"))
  )
  bridged <- readLines(target, warn = FALSE)
  expect_true(any(grepl("ENA3D_BUILD_ID", bridged, fixed = TRUE)))
  expect_false(any(grepl(
    "must-not-cross-the-boundary",
    bridged,
    fixed = TRUE
  )))

  loaded <- processx::run(
    "/usr/bin/env",
    c(
      "-i",
      paste0("HOME=", worker_home),
      paste0("PATH=", Sys.getenv("PATH")),
      "LANG=C.UTF-8",
      file.path(R.home("bin"), "R"),
      "--no-save",
      "--slave",
      "-e",
      paste(
        'cat(Sys.getenv("ENA3D_BUILD_ID"), "\\n", sep = "")',
        paste0(
          'cat(Sys.getenv("DASHSCOPE_API_KEY", unset = "unset"), ',
          '"\\n", sep = "")'
        ),
        paste0(
          'cat(normalizePath(.libPaths()[[1L]], mustWork = TRUE), ',
          '"\\n", sep = "")'
        ),
        sep = "; "
      )
    ),
    wd = worker_app,
    error_on_status = FALSE,
    echo = FALSE
  )
  expect_identical(loaded$status, 0L, info = loaded$stderr)
  expect_identical(
    loaded$stdout,
    paste0(
      safe_value,
      "\nunset\n",
      normalizePath(worker_library, mustWork = TRUE),
      "\n"
    )
  )

  rejected_target <- tempfile(
    "ena3d-runtime-rejected-",
    fileext = ".Rprofile"
  )
  on.exit(unlink(rejected_target, force = TRUE), add = TRUE)
  rejected <- processx::run(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", writer, rejected_target),
    env = c(ENA3D_BUILD_ID = "unsafe\nexport INJECTED=true"),
    error_on_status = FALSE,
    echo = FALSE
  )
  expect_gt(rejected$status, 0L)
  expect_match(rejected$stderr, "Refusing a line break")
  expect_false(file.exists(rejected_target))
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
