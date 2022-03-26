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

  # Basic graph options --------------------
  # Axis font sizes
  axis.text.y <- jamovi_sanitize(
    my_value = self$options$axis.text.y,
    return_value = 10,
    na_ok = FALSE,
    convert_to_number = TRUE,
    lower = 1,
    lower_inclusive = TRUE,
    my_value_name = "Y axis: Tick font size"
  )
  axis.title.y <- jamovi_sanitize(
    my_value = self$options$axis.title.y,
    return_value = 12,
    na_ok = FALSE,
    convert_to_number = TRUE,
    lower = 1,
    lower_inclusive = TRUE,
    my_value_name = "Y axis: Label font size"
  )
  axis.text.x <- jamovi_sanitize(
    my_value = self$options$axis.text.x,
    return_value = 10,
    na_ok = FALSE,
    convert_to_number = TRUE,
    lower = 1,
    lower_inclusive = TRUE,
    my_value_name = "X axis: Tick font size"
  )
  axis.title.x <- jamovi_sanitize(
    my_value = self$options$axis.title.x,
    return_value = 10,
    na_ok = FALSE,
    convert_to_number = TRUE,
    lower = 1,
    lower_inclusive = TRUE,
    my_value_name = "X axis: Label font size"
  )


  myplot <- myplot + ggplot2::theme(
    axis.text.y = element_text(size = axis.text.y),
    axis.title.y = element_text(size = axis.title.y),
    axis.text.x = element_text(size = axis.text.x),
    axis.title.x = element_text(size = axis.title.x)
  )


  # Axis labels
  xlab <- jamovi_sanitize(
    my_value = self$options$xlab,
    return_value = NULL,
    na_ok = FALSE,
    my_value_name = "X axis: Title"
  )

  ylab <- jamovi_sanitize(
    my_value = self$options$ylab,
    return_value = NULL,
    na_ok = FALSE,
    my_value_name = "Y axis: Title"
  )

  if (!is.null(xlab)) {
    myplot <- myplot + ggplot2::xlab(xlab)
  }
  if (!is.null(ylab)) {
    myplot <- myplot + ggplot2::ylab(ylab)
  }

  # Aesthetics
  myplot <- myplot + ggplot2::scale_shape_manual(
    values = c(
      "Reference_raw" = self$options$shape_raw_reference,
      "Comparison_raw" = self$options$shape_raw_comparison,
      "Difference_raw" = "triangle filled",
      "Unused_raw" = "diamond filled",
      "Reference_summary" = self$options$shape_summary_reference,
      "Comparison_summary" = self$options$shape_summary_comparison,
      "Difference_summary" = self$options$shape_summary_difference,
      "Unused_summary" = "circle filled"
    )
  )

  myplot <- myplot + ggplot2::scale_color_manual(
    values = c(
      "Reference_raw" = self$options$color_raw_reference,
      "Comparison_raw" = self$options$color_raw_comparison,
      "Difference_raw" = "black",
      "Unused_raw" = "black",
      "Reference_summary" = self$options$color_summary_reference,
      "Comparison_summary" = self$options$color_summary_comparison,
      "Difference_summary" = self$options$color_summary_difference,
      "Unused_summary" = "gray80"
    ),
    aesthetics = c("color", "point_color")
  )

  myplot <- myplot + ggplot2::scale_fill_manual(
    values = c(
      "Reference_raw" = self$options$fill_raw_reference,
      "Comparison_raw" = self$options$fill_raw_comparison,
      "Difference_raw" = "NA",
      "Unused_raw" = "gray80",
      "Reference_summary" = self$options$fill_summary_reference,
      "Comparison_summary" = self$options$fill_summary_comparison,
      "Difference_summary" = self$options$fill_summary_difference,
      "Unused_summary" = "gray80"
    ),
    aesthetics = c("fill", "point_fill")
  )

  myplot <- myplot + ggplot2::discrete_scale(
    c("size", "point_size"),
    "point_size_d",
    function(n) return(c(
      "Reference_raw" = as.integer(self$options$size_raw_reference),
      "Comparison_raw" = as.integer(self$options$size_raw_comparison),
      "Difference_raw" = 2,
      "Unused_raw" = 1,
      "Reference_summary" = as.integer(self$options$size_summary_reference),
      "Comparison_summary" = as.interger(self$options$size_summary_comparison),
      "Difference_summary" = as.integer(self$options$size_summary_difference),
      "Unused_summary" = 3
    ))
  )

  myplot <- myplot + ggplot2::discrete_scale(
    c("alpha", "point_alpha"),
    "point_alpha_d",
    function(n) return(c(
      "Reference_raw" = as.number(self$options$alpha_raw_reference),
      "Comparison_raw" = .as.number(self$options$alpha_raw_comparison),
      "Difference_raw" = .8,
      "Unused_raw" = .5,
      "Reference_summary" = as.number(self$options$alpha_summary_reference),
      "Comparison_summary" = as.number(self$options$alpha_summary_comparison),
      "Difference_summary" = as.number(self$options$alpha_summary_difference),
      "Unused_summary" = 1
    ))
  )


  self$results$estimation_plot_warnings$setState(
    c(
      notes,
      names(xlab),
      names(ylab),
      names(axis.text.y),
      names(axis.title.y),
      names(axis.text.x),
      names(axis.title.x)
    )
  )
  jamovi_set_notes(self$results$estimation_plot_warnings)

  return(myplot)
}
