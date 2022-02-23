#'
#' @export
plot_mdiff <- function(
  estimate,
  effect_size = c("mean", "median"),
  data_layout = c("random", "swarm", "none"),
  data_spread = 0.25,
  error_layout = c("halfeye", "eye", "gradient", "none"),
  error_scale = 0.3,
  error_nudge = 0.35,
  error_normalize = c("groups", "all", "panels"),
  ggtheme = NULL
) {

  # Input checks ---------------------------------------------------------------
  warnings <- NULL

  esci_assert_type(estimate, "is.estimate")
  effect_size <- match.arg(effect_size)
  data_layout <- match.arg(data_layout)
  error_layout <- match.arg(error_layout)
  error_normalize <- match.arg(error_normalize)
  if (is.null(data_spread) | !is.numeric(data_spread) | data_spread < 0) {
    warnings <- c(
      warnings,
      glue::glue(
        "data_spread = {data_spread} but this is invalid; replaced with 0.25"
      )
    )
    data_spread <- 0.25
  }
  if (is.null(error_scale) | !is.numeric(error_scale) | error_scale < 0) {
    warnings <- c(
      warnings,
      glue::glue(
        "error_scale = {error_scale} but this is invalid; replaced with 0.3"
      )
    )
    error_scale = 0.3
  }
  if (is.null(error_nudge) | !is.numeric(error_nudge) | error_nudge < 0) {
    warnings <- c(
      warnings,
      glue::glue(
        "error_nudge = {error_nudge} but this is invalid; replaced with 0.25"
      )
    )
    error_nudge <- 0.25
  }
  if(is.null(ggtheme)) { ggtheme <- ggplot2::theme_classic()}


  # Data prep --------------------------------------
  # Initialization
  conf_level <- estimate$properties$conf_level
  contrast <- estimate$properties$contrast
  reference_groups <- names(contrast[which(contrast < 0)])
  comparison_groups <- names(contrast[which(contrast > 0)])
  plot_raw <- !is.null(estimate$raw_data) & data_layout != "none"
  simple_contrast <- (length(reference_groups) == 1) & (length(comparison_groups) == 1)


  # Raw data
  if (plot_raw) rdata <- estimate$raw_data else rdata <- NULL

  # Group data
  if (effect_size == "mean") {
    gdata <- estimate$es_mean_difference
  } else {
    gdata <- estimate$es_median_difference
    gdata$df <- NA
  }
  gdata$x_value <- gdata$effect
  gdata$y_value <- gdata$effect_size


  # If complex contrast, add overview data
  if (!simple_contrast) {
    overview <- data.frame(
      type = "Unused",
      outcome_variable_name = estimate$overview$outcome_variable_name,
      grouping_variable_name = estimate$overview$grouping_variable_name,
      effect = estimate$overview$grouping_variable_level,
      effect_size = if (effect_size == "mean") estimate$overview$mean else estimate$overview$median,
      LL = if (effect_size == "mean") estimate$overview$mean_LL else estimate$overview$median_LL,
      UL = if (effect_size == "mean") estimate$overview$mean_UL else estimate$overview$median_UL,
      SE = if (effect_size == "mean") estimate$overview$mean_SE else estimate$overview$median_SE,
      df = estimate$overview$df,
      x_value = estimate$overview$grouping_variable_level,
      y_value = if (effect_size == "mean") estimate$overview$mean else estimate$overview$median
    )
  } else {
    overview <- NULL
  }

  myplot <- plot_mdiff_base(
    gdata = gdata,
    conf_level = conf_level,
    contrast = contrast,
    rdata = rdata,
    overview = overview,
    effect_size = effect_size,
    data_layout = data_layout,
    data_spread = data_spread,
    error_layout = error_layout,
    error_scale = error_scale,
    error_nudge = error_nudge,
    error_normalize = error_normalize,
    ggtheme = ggtheme
  )

  # Customize plot -------------------------------
  # Default aesthetics
  myplot <- esci_plot_mdiff_aesthetics(myplot)

  # No legend
  myplot <- myplot + ggplot2::theme(legend.position = "none")


  # Labels -----------------------------
  vnames <- estimate$es_mean_difference$outcome_variable_name[[1]]
  ylab <- glue::glue("{vnames}\n{if (plot_raw) 'Data, ' else ''}{effect_size} and {conf_level*100}% confidence interval")
  xlab <- estimate$es_mean_difference$grouping_variable_name[[1]]


  myplot <- myplot + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)


  # Attach warnings and return    -------------------
  myplot$warnings <- c(myplot$warnings, warnings)

  return(myplot)

}

plot_mdiff_base <- function(
  gdata,
  conf_level,
  contrast,
  rdata = NULL,
  overview = NULL,
  effect_size = c("mean", "median", "r", "P"),
  data_layout = c("random", "swarm", "none"),
  data_spread = 0.25,
  error_layout = c("halfeye", "eye", "gradient", "none"),
  error_scale = 0.3,
  error_nudge = 0.35,
  error_normalize = c("groups", "all", "panels"),
  ggtheme = NULL
) {

  # Input checks ---------------------------------------------------------------
  warnings <- NULL

  # Data prep --------------------------------------
  # Initialization
  reference_groups <- names(contrast[which(contrast < 0)])
  comparison_groups <- names(contrast[which(contrast > 0)])
  simple_contrast <- is.null(overview)

  # Prep raw data
  plot_raw <- !is.null(rdata)
  if (plot_raw) {
    rdata$type <- "Unused"
    rdata[rdata$grouping_variable %in% reference_groups, ]$type <- "Reference"
    rdata[rdata$grouping_variable %in% comparison_groups, ]$type <- "Comparison"
    rdata$type <- paste(rdata$type, "_raw", sep = "")
    nudge <- error_nudge
  } else {
    nudge <- 0
  }

  # Add comparison values to difference row
  comparison_es <- gdata[[2, "y_value"]]
  gdata[3, c("y_value", "LL", "UL")] <- gdata[3, c("y_value", "LL", "UL")]  + comparison_es

  # Wrap effect in () if only 1 level
  if(!simple_contrast) {
    if (length(reference_groups) == 1) gdata[[2, "x_value"]] <- paste("(", gdata[[2, "x_value"]], ")", sep = "")
    if (length(comparison_groups) == 1) gdata[[1, "x_value"]] <- paste("(", gdata[[1, "x_value"]], ")", sep = "")
  }

  # Swap comparison and reference rows for graph
  gdata <- gdata[c(2, 1, 3), ]

  # If complex contrast, add overview data
  if (!simple_contrast) {
    overview$type <- "Unused"
    overview[overview$effect %in% reference_groups, ]$type <- "Reference"
    overview[overview$effect %in% comparison_groups, ]$type <- "Comparison"

    gdata <- rbind(
      overview,
      gdata
    )
  }

  # Set plot order
  gdata$x_value <- factor(gdata$x_value, levels = gdata$x_value)

  # Update types for aesthetic control
  gdata$type <- paste(gdata$type, "_summary", sep = "")


  # Build plot ------------------------------------
  # Base plot
  myplot <- ggplot2::ggplot() + ggtheme

  # Group data
  error_glue <-esci_plot_group_data(effect_size)
  error_call <- esci_plot_error_layouts(error_layout)
  error_expression <- parse(text = glue::glue(error_glue))
  myplot <- try(eval(error_expression))

  # Raw data
  if (plot_raw) {
    raw_expression <- esci_plot_raw_data(myplot, data_layout, data_spread)
    myplot <- try(eval(raw_expression))
  }

  # Attach warnings and return    -------------------
  myplot$warnings <- warnings

  return(myplot)


}


esci_plot_mdiff_aesthetics <- function(myplot) {
  # Customize plot -------------------------------
  # Points
  myplot <- myplot + ggplot2::scale_shape_manual(
    values = c(
      "Reference_raw" = "circle filled",
      "Comparison_raw" = "square filled",
      "Difference_raw" = "triangle filled",
      "Unused_raw" = "diamond filled",
      "Reference_summary" = "square filled",
      "Comparison_summary" = "diamond filled",
      "Difference_summary" = "triangle filled",
      "Unused_summary" = "circle filled"
    )
  )
  myplot <- myplot + ggplot2::scale_color_manual(
    values = c(
      "Reference_raw" = "black",
      "Comparison_raw" = "black",
      "Difference_raw" = "black",
      "Unused_raw" = "black",
      "Reference_summary" = "black",
      "Comparison_summary" = "black",
      "Difference_summary" = "black",
      "Unused_summary" = "gray80"
    ),
    aesthetics = c("color", "point_color")
  )
  myplot <- myplot + ggplot2::scale_fill_manual(
    values = c(
      "Reference_raw" = "#E69F00",
      "Comparison_raw" = "#0072B2",
      "Difference_raw" = "#000000",
      "Unused_raw" = "gray80",
      "Reference_summary" = "#E69F00",
      "Comparison_summary" = "#0072B2",
      "Difference_summary" = "#000000",
      "Unused_summary" = "gray80"
    ),
    aesthetics = c("fill", "point_fill")
  )
  myplot <- myplot + ggplot2::discrete_scale(
    c("size", "point_size"),
    "point_size_d",
    function(n) return(c(
      "Reference_raw" = 2,
      "Comparison_raw" = 2,
      "Difference_raw" = 2,
      "Unused_raw" = 1,
      "Reference_summary" = 3,
      "Comparison_summary" = 3,
      "Difference_summary" = 3,
      "Unused_summary" = 3
    ))
  )
  myplot <- myplot + ggplot2::discrete_scale(
    c("alpha", "point_alpha"),
    "point_alpha_d",
    function(n) return(c(
      "Reference_raw" = .8,
      "Comparison_raw" = .8,
      "Difference_raw" = .8,
      "Unused_raw" = .5,
      "Reference_summary" = 1,
      "Comparison_summary" = 1,
      "Difference_summary" = 1,
      "Unused_summary" = 1
    ))
  )

  # Error bars
  myplot <- myplot + ggplot2::scale_linetype_manual(
    values = c(
      "Reference_summary" = "solid",
      "Comparison_summary" = "solid",
      "Difference_summary" = "solid",
      "Unused_summary" = "dotted"
    )
  )
  myplot <- myplot + ggplot2::scale_color_manual(
    values = c(
      "Reference_summary" = "black",
      "Comparison_summary" = "black",
      "Difference_summary" = "black",
      "Unused_summary" = "gray"
    ),
    aesthetics = "interval_color"
  )
  myplot <- myplot + ggplot2::discrete_scale(
    "interval_alpha",
    "interval_alpha_d",
    function(n) return(c(
      "Reference_summary" = 1,
      "Comparison_summary" = 1,
      "Difference_summary" = 1,
      "Unused_summary" = 1
    ))
  )
  myplot <- myplot + ggplot2::discrete_scale(
    "interval_size",
    "interval_size_d",
    function(n) return(c(
      "Reference_summary" = 2,
      "Comparison_summary" = 2,
      "Difference_summary" = 2,
      "Unused_summary" = 1
    ))
  )

  # Slab
  myplot <- myplot + ggplot2::scale_fill_manual(
    values = c(
      "Reference_summary" = "gray",
      "Comparison_summary" = "gray",
      "Difference_summary" = "gray",
      "Unused_summary" = "gray"
    ),
    aesthetics = "slab_fill"
  )
  myplot <- myplot + ggplot2::discrete_scale(
    "slab_alpha",
    "slab_alpha_d",
    function(n) return(c(
      "Reference_summary" = 1,
      "Comparison_summary" = 1,
      "Difference_summary" = 1,
      "Unused_summary" = 1
    ))
  )

  return(myplot)

}
