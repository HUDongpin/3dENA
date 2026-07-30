#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop(
    "Usage: Rscript tools/prepare_class1_timepoints_sample.R INPUT.RData OUTPUT.RData",
    call. = FALSE
  )
}

input_path <- normalizePath(args[[1L]], mustWork = TRUE)
output_path <- normalizePath(args[[2L]], mustWork = FALSE)
script_file <- sub(
  "^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)
)[1L]
if (is.na(script_file) || !nzchar(script_file)) {
  stop("Could not locate this preparation script.", call. = FALSE)
}
project_root <- normalizePath(
  file.path(dirname(normalizePath(script_file, mustWork = TRUE)), ".."),
  mustWork = TRUE
)

source(file.path(project_root, "R", "security_utils.R"), local = FALSE)
source(file.path(project_root, "R", "app_utils.R"), local = FALSE)
source(file.path(project_root, "R", "transition.R"), local = FALSE)
source(file.path(project_root, "R", "ena3d_exchange.R"), local = FALSE)
source(file.path(project_root, "R", "app_module_load_dataset.R"), local = FALSE)

source_env <- new.env(parent = emptyenv())
object_names <- load(input_path, envir = source_env)
ena_names <- object_names[vapply(object_names, function(name) {
  inherits(get(name, envir = source_env, inherits = FALSE), "ena.set")
}, logical(1L))]
if (length(ena_names) != 1L) {
  stop("The input must contain exactly one ena.set object.", call. = FALSE)
}
source_set <- unserialize(serialize(
  get(ena_names[[1L]], envir = source_env, inherits = FALSE), NULL
))

metadata <- as.data.frame(source_set$meta.data, stringsAsFactors = FALSE)
required_metadata <- c("ENA_UNIT", "Condition", "Group", "Speaker", "Period")
if (!all(required_metadata %in% names(metadata))) {
  stop("The Class 1 source is missing required trajectory metadata.", call. = FALSE)
}

participants <- unique(metadata[c("Group", "Speaker")])
speaker_groups <- split(as.character(participants$Group), participants$Speaker)
if (any(vapply(speaker_groups, function(groups) {
  length(unique(groups)) != 1L
}, logical(1L)))) {
  stop("A source Speaker label is reused across groups; pseudonymization is ambiguous.",
       call. = FALSE)
}
participants$PseudoSpeaker <- ave(
  seq_len(nrow(participants)),
  participants$Group,
  FUN = function(index) sprintf("S%02d", seq_along(index))
)
participants$PseudoSpeaker <- paste0(
  participants$Group, "-", participants$PseudoSpeaker
)
speaker_map <- stats::setNames(
  participants$PseudoSpeaker, participants$Speaker
)

condition_map <- c(
  "AI" = "GenAI group",
  "Non-AI" = "Non-GenAI group"
)

sanitize_frame <- function(frame) {
  frame <- as.data.frame(frame, stringsAsFactors = FALSE, optional = TRUE)
  required <- c("Condition", "Group", "Speaker", "Period")
  if (!all(required %in% names(frame))) {
    stop("A row-aligned ENA table is missing Class 1 metadata.", call. = FALSE)
  }
  pseudo <- unname(speaker_map[as.character(frame$Speaker)])
  condition <- unname(condition_map[as.character(frame$Condition)])
  if (anyNA(pseudo) || anyNA(condition)) {
    stop("The source contains an unmapped speaker or condition.", call. = FALSE)
  }
  frame$Speaker <- enc2utf8(pseudo)
  frame$Condition <- enc2utf8(condition)
  if ("ENA_UNIT" %in% names(frame)) {
    frame$ENA_UNIT <- enc2utf8(paste(
      frame$Condition, frame$Group, frame$Speaker, frame$Period, sep = "::"
    ))
  }
  frame
}

source_set$meta.data <- sanitize_frame(source_set$meta.data)
source_set$points <- sanitize_frame(source_set$points)
source_set$line.weights <- sanitize_frame(source_set$line.weights)

# Round-trip through the public exchange contract to intentionally discard the
# source model's raw-input caches and retain only the reviewed ENA tables needed
# by the application. This prevents hidden identifiers from entering the public
# sample even if a future rENA object adds more cached fields.
payload <- ena3d_exchange_payload(source_set)
class1_timepoints_ena <- ena3d_exchange_decode(payload)
class1_timepoints_ena$rotation.matrix <- source_set$rotation.matrix
class1_timepoints_ena$`_function.params`$trajectory.time.by <- "Period"
class1_timepoints_ena$`_function.params`$trajectory.id.by <- "Speaker"
class1_timepoints_ena$`_function.params`$trajectory.group.by <- "Group"

ena3d_validate_ena_object(
  class1_timepoints_ena,
  object_name = "Class 1 TP1-TP3 trusted sample"
)
if (nrow(class1_timepoints_ena$points) != 72L ||
    nrow(class1_timepoints_ena$rotation$nodes) != 6L ||
    !identical(sort(unique(as.character(class1_timepoints_ena$points$Period))),
               c("TP1", "TP2", "TP3"))) {
  stop("The prepared sample does not match the reviewed 72-unit Class 1 model.",
       call. = FALSE)
}

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
save(class1_timepoints_ena, file = output_path, compress = "xz", version = 3L)
cat("prepared:", output_path, "\n")
cat("participants:", length(unique(class1_timepoints_ena$points$Speaker)), "\n")
cat("student-period units:", nrow(class1_timepoints_ena$points), "\n")
