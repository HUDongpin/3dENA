script_args <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- normalizePath(
  sub("^--file=", "", script_args[[1L]]),
  mustWork = TRUE
)
project_root <- normalizePath(
  file.path(dirname(script_path), "..", "..", ".."),
  mustWork = TRUE
)

source(file.path(project_root, "R", "security_utils.R"))
source(file.path(project_root, "R", "app_utils.R"))
source(file.path(project_root, "R", "app_module_load_dataset.R"))

sample_environment <- new.env(parent = emptyenv())
object_names <- load(
  file.path(project_root, "sample_data", "sample_enaset.Rdata"),
  envir = sample_environment
)
ena_names <- object_names[vapply(object_names, function(name) {
  inherits(sample_environment[[name]], "ena.set")
}, logical(1))]
stopifnot(length(ena_names) == 1L)
ena_object <- sample_environment[[ena_names[[1L]]]]

# Replace the public demo labels so the committed browser fixture is plainly
# synthetic and contains no person-like identifiers.
synthetic_user <- rep(c("unit_a", "unit_b"), length.out = nrow(ena_object$points))
synthetic_group <- as.character(ena_object$points[["groupid"]])
synthetic_unit <- paste0("group_", synthetic_group, "_", synthetic_user)
for (table_name in c("meta.data", "points", "line.weights")) {
  ena_object[[table_name]][["ENA_UNIT"]] <- synthetic_unit
  ena_object[[table_name]][["username"]] <- synthetic_user
}

output <- file.path(dirname(script_path), "small-valid.ena3d.json")
unlink(output)
result <- ena3d_write_exchange_file(ena_object, output)
message(
  sprintf(
    "Wrote %s (%d bytes, sha256 %s)",
    basename(result$path), result$bytes, result$sha256
  )
)
