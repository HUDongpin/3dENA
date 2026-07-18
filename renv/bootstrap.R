project <- normalizePath(
  Sys.getenv("ENA3D_PROJECT_ROOT", unset = getwd()),
  mustWork = TRUE
)
lockfile <- file.path(project, "renv.lock")
library <- Sys.getenv(
  "RENV_PATHS_LIBRARY",
  unset = file.path(project, "renv", "library")
)
activation_paths <- c(
  file.path(project, ".Rprofile"),
  file.path(project, "renv", "activate.R")
)
if (!all(file.exists(activation_paths))) {
  stop("Project activation files are missing.", call. = FALSE)
}
activation_contents <- lapply(activation_paths, function(path) {
  readBin(path, what = "raw", n = file.info(path)$size)
})
restore_project_activation <- function() {
  for (index in seq_along(activation_paths)) {
    writeBin(activation_contents[[index]], activation_paths[[index]])
  }
}
dir.create(library, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(library, .libPaths()))

repos_override <- Sys.getenv("RENV_CONFIG_REPOS_OVERRIDE", unset = "")
repos <- if (nzchar(repos_override)) {
  c(CRAN = repos_override)
} else {
  getOption("repos")
}
usable_repos <- is.character(repos) && length(repos) &&
  any(!is.na(repos) & nzchar(repos) & repos != "@CRAN@")
if (!usable_repos) {
  repos <- c(CRAN = "https://cloud.r-project.org")
}
options(repos = repos)
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", lib = library, repos = repos)
}

renv::consent(provided = TRUE)
restore_error <- tryCatch({
  renv::restore(
    project = project,
    library = library,
    lockfile = lockfile,
    repos = repos,
    clean = TRUE,
    prompt = FALSE
  )
  NULL
}, error = function(error) error)
restore_project_activation()
if (!is.null(restore_error)) {
  stop(restore_error)
}
