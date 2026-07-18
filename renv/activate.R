local({
  source_file <- sys.frame(1L)$ofile
  if (is.null(source_file) || !length(source_file)) {
    project <- normalizePath(getwd(), mustWork = TRUE)
  } else {
    project <- normalizePath(
      file.path(dirname(source_file), ".."),
      mustWork = TRUE
    )
  }
  project_library <- Sys.getenv(
    "RENV_PATHS_LIBRARY",
    unset = file.path(project, "renv", "library")
  )
  if (dir.exists(project_library)) {
    .libPaths(c(normalizePath(project_library, mustWork = TRUE), .libPaths()))
  }
})
