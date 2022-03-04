jamovi_plot_mdiff <- function(
  self,
  estimate,
  image,
  ggtheme,
  theme
) {

  # Basic plot
  notes <- NULL
  args <- list()
  args$estimate <- estimate
  args$effect_size <- "mean"
  args$data_layout <- self$options$data_layout
  args <- jamovi_arg_builder(
    args,
    "data_spread",
    my_value = self$options$data_spread,
    return_value = .25,
    convert_to_number = TRUE,
    lower = .01,
    lower_inclusive = TRUE,
    upper = 2,
    upper_inclusive = TRUE,
    my_value_name = "Data: Spread"
  )
  args$error_layout <- self$options$error_layout
  args <- jamovi_arg_builder(
    args,
    "error_scale",
    self$options$error_scale,
    return_value = 0.25,
    lower = 0,
    lower_inclusive = TRUE,
    upper = 5,
    upper_inclusive = TRUE,
    my_value_name = "Distributions: Width",
    convert_to_number = TRUE
  )
  args <- jamovi_arg_builder(
    args,
    "error_nudge",
    self$options$error_nudge,
    return_value = 0.4,
    lower = 0,
    lower_inclusive = TRUE,
    upper = 5,
    upper_inclusive = TRUE,
    my_value_name = "Distributions: Offset from data",
    convert_to_number = TRUE
  )
  args$difference_axis_units <- self$options$difference_axis_units
  args <- jamovi_arg_builder(
    args,
    "difference_axis_breaks",
    self$options$difference_axis_breaks,
    return_value = 5,
    lower = 1,
    lower_inclusive = TRUE,
    upper = 50,
    upper_inclusive = TRUE,
    my_value_name = "Difference axis: Number of breaks",
    convert_to_number = TRUE
  )
  args$difference_axis_space <- 0.5
  args$simple_contrast_labels <- self$options$simple_contrast_labels
  # Axis breaks
  ymin <- jamovi_sanitize(
    my_value = self$options$ymin,
    return_value = NA,
    na_ok = TRUE,
    convert_to_number = TRUE,
    my_value_name = "Y axis: Axis minimum"
  )
  ymax <- jamovi_sanitize(
    my_value = self$options$ymax,
    return_value = NA,
    na_ok = TRUE,
    convert_to_number = TRUE,
    my_value_name = "Y axis: Axis maximum"
  )

  args <- jamovi_arg_builder(
    args,
    "ybreaks",
    self$options$ybreaks,
    return_value = 5,
    lower = 1,
    lower_inclusive = TRUE,
    upper = 50,
    upper_inclusive = TRUE,
    my_value_name = "Y axis: Number of tick marks",
    convert_to_number = TRUE
  )
  args$ggtheme <- ggtheme[[1]]

  # Store notes from basic plot
  notes <- c(
    notes,
    args$warnings,
    names(ymax), names(ymin)
  )
  args$warnings <- NULL

  args$ylim <- c(ymin, ymax)

  # Do basic plot
  myplot <- do.call(
    what = plot_mdiff,
    args = args
  )

  self$results$estimation_plot_warnings$setState(notes)
  jamovi_set_notes(self$results$estimation_plot_warnings)

  return(myplot)
}
