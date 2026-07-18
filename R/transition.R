# Mean ENA network summaries used by Overall, Network, Comparison and Change.
# Historical interpolation helpers were removed when observed centroid paths
# became the supported trajectory implementation.
get_mean_group_lineweights <- function(ena_obj,groupVar,group_name){
  line_weights <- ena_obj$line.weights
  rows <- ena3d_group_value_match(line_weights[[groupVar]], group_name)
  if (!any(rows)) {
    stop(sprintf("No line weights found for %s = %s", groupVar, group_name))
  }
  # Strip metadata before row subsetting. Base data.frame subsetting may drop
  # custom `ena.metadata` classes, which would otherwise make character ID
  # columns leak into colMeans() for valid exchange-restored objects.
  numeric_weights <- rENA::remove_meta_data(line_weights)
  as.vector(colMeans(as.matrix(numeric_weights[rows, , drop = FALSE])))

}
get_mean_group_lineweights_in_groups <- function(ena_obj,groupVar,group_names){
  # ena_line_weights = as.data.frame(ena_obj$line.weights)
  # group_lineweights = ena_line_weights[which(ena_line_weights[,groupVar]==group_name),]
  rows <- ena3d_group_value_match(
    ena_obj$line.weights[[groupVar]], group_names
  )
  if (!any(rows)) {
    stop(sprintf("No line weights found for selected %s values.", groupVar))
  }
  numeric_weights <- rENA::remove_meta_data(ena_obj$line.weights)
  as.vector(colMeans(as.matrix(numeric_weights[rows, , drop = FALSE])))

}
