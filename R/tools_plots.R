# This function helps with plotting sampling error in esci graphs
#  The user provides a short/user-friendly name for the style of
#  plotting sampling error, and this maps it onto the specific
#  geom from ggdist
# If no friendly name is passed or the friendly name is not
#  recognized, point_interval is returned, which does not plot sampling error
esci_plot_error_layouts <- function(error_layout = "none") {

  # Mapping of friendly names to ggdist geoms
  error_layouts <- list(
    halfeye = "ggdist::stat_dist_halfeye",
    eye = "ggdist::stat_dist_eye",
    gradient = "ggdist::stat_dist_gradientinterval",
    none = "ggdist::stat_dist_pointinterval"
  )

  # Handle if friendly name not on list
  if(!error_layout %in% names(error_layouts)) {error_layout <- "none"}

  # Return appropriate ggdist geom
  return(error_layouts[[error_layout]])
}


# Same as above, but in this case, maps friendly names for styles of
#  plotting raw data to different geoms in ggplot2, ggbeeswarm, and ggdist
esci_plot_data_layouts <- function(data_layout = "none", data_spread){

  # Mapping of friendly names to geoms for plotting raw data
  data_layouts <- list(
    swarm = "ggbeeswarm::position_beeswarm",
    random = "ggbeeswarm::position_quasirandom",
    none = NULL
  )

  extra_options <- list(
    swarm = paste(", cex = ", data_spread * 4, sep = ""),
    random = paste(", varwidth = TRUE, width = ", data_spread, sep = ""),
    none = NULL
  )

  # Handle if friendly name not on list
  if(!data_layout %in% names(data_layouts)) {data_layout <- "none"}

  res <- list()
  res$call <- data_layouts[[data_layout]]
  res$extras <- extra_options[[data_layout]]

  # Return appropriate ggdist geom
  return(res)
}
