file_args <- sub(
  "^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)
)
script_file <- file_args[file.exists(file_args)][1L]
if (is.na(script_file)) {
  stop("Could not locate the converter script.", call. = FALSE)
}
project_root <- normalizePath(
  file.path(dirname(script_file), ".."), mustWork = TRUE
)

arguments <- commandArgs(trailingOnly = TRUE)
usage <- paste(
  "Usage: Rscript tools/convert_trusted_rdata_to_ena3d_json.R",
  "--trusted-native-input input.RData output.ena3d.json"
)
if (length(arguments) != 3L ||
    !identical(arguments[[1L]], "--trusted-native-input")) {
  stop(
    paste(
      usage,
      "This flag confirms that the input is local and trusted. Native R",
      "serialization can execute code while loading; never run this converter",
      "on an untrusted file or inside the public web worker."
    ),
    call. = FALSE
  )
}

input_path <- normalizePath(arguments[[2L]], mustWork = TRUE)
if (!grepl("\\.[Rr][Dd]ata$", input_path)) {
  stop("Trusted converter input must be an .RData file.", call. = FALSE)
}
output_path <- file.path(
  normalizePath(dirname(arguments[[3L]]), mustWork = TRUE),
  basename(arguments[[3L]])
)
if (!grepl("\\.ena3d\\.json$", output_path, ignore.case = TRUE)) {
  stop("Converter output must end in .ena3d.json.", call. = FALSE)
}
checksum_path <- paste0(output_path, ".sha256")
if (file.exists(output_path) || file.exists(checksum_path)) {
  stop("Refusing to overwrite an existing output or checksum.", call. = FALSE)
}

source(file.path(project_root, "R", "security_utils.R"), local = FALSE)
source(file.path(project_root, "R", "app_utils.R"), local = FALSE)
source(file.path(project_root, "R", "ena3d_exchange.R"), local = FALSE)
source(
  file.path(project_root, "R", "app_module_load_dataset.R"), local = FALSE
)

message(
  "Loading a LOCAL TRUSTED native R file. Do not use this tool for public uploads."
)
input_sha256 <- digest::digest(file = input_path, algo = "sha256")
ena_object <- ena3d_read_ena_object(
  input_path,
  source_kind = "trusted_native",
  limits = ena3d_data_limits()
)
result <- ena3d_write_exchange_file(
  ena_object,
  output_path,
  limits = ena3d_data_limits()
)
writeLines(
  paste(result$sha256, basename(result$path)),
  checksum_path,
  useBytes = TRUE
)

cat(
  paste0("trusted_input_sha256=", input_sha256),
  paste0("exchange_sha256=", result$sha256),
  paste0("exchange_bytes=", format(result$bytes, scientific = FALSE)),
  paste0("exchange_path=", result$path),
  paste0("checksum_path=", checksum_path),
  sep = "\n"
)
cat("\n")
