args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(script_arg)) {
  normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(file.path("tests", "e2e", "start-app.R"), mustWork = TRUE)
}

project_root <- normalizePath(
  file.path(dirname(script_path), "..", ".."),
  mustWork = TRUE
)
app_dir <- file.path(project_root, "R")
if (!file.exists(file.path(app_dir, "app.R"))) {
  stop("Could not locate R/app.R from tests/e2e/start-app.R.", call. = FALSE)
}

port_text <- Sys.getenv("E2E_PORT", unset = "3838")
port <- suppressWarnings(as.integer(port_text))
if (is.na(port) || port < 1024L || port > 65535L || as.character(port) != port_text) {
  stop("E2E_PORT must be an integer between 1024 and 65535.", call. = FALSE)
}

setwd(project_root)
shiny::runApp(
  app_dir,
  host = "127.0.0.1",
  port = port,
  launch.browser = FALSE,
  display.mode = "normal"
)
