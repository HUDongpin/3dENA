project <- normalizePath(
  Sys.getenv("ENA3D_PROJECT_ROOT", unset = getwd()),
  mustWork = TRUE
)
lockfile <- file.path(project, "renv.lock")
library <- Sys.getenv(
  "RENV_PATHS_LIBRARY",
  unset = file.path(project, "renv", "library")
)
dir.create(library, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(library, .libPaths()))

repos <- getOption("repos")
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
renv::restore(
  project = project,
  library = library,
  lockfile = lockfile,
  repos = repos,
  clean = TRUE,
  prompt = FALSE
)
