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
  gdata$x_label <- gdata$effect
  gdata$y_value <- gdata$effect_size
  gdata$x_label[[3]] <- gsub(" - ", "\n-\n", gdata$x_label[[3]])


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
      x_label = estimate$overview$grouping_variable_level,
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

  if (effect_size == "median")  {
    myplot <- myplot + ggplot2::discrete_scale(
      "interval_size",
      "interval_size_d",
      function(n) return(c("summary" = 1))
    )
  }


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
  one_group <- is.na(gdata$SE[[2]])
  plot_raw <- !is.null(rdata)
  nudge <- 0
  x_nudge <- if (plot_raw) error_nudge/2 else 0
  x_end <- 3

  # Group data --------------------------------
  # Add comparison values to difference row
  comparison_es <- gdata[[2, "y_value"]]
  gdata[3, c("y_value", "LL", "UL")] <- gdata[3, c("y_value", "LL", "UL")]  + comparison_es

  gdata <- gdata[c(2, 1, 3), ]

  # Handle comparisons to a specified reference value
  if (one_group) {
    gdata[is.na(gdata$df), "df"] <- 1
    gdata[is.na(gdata$SE), "SE"] <- .Machine$double.xmin
  }

  # If complex contrast, add overview data -----------------
  gdata_only <- gdata
  if (!simple_contrast) {
    overview$type <- "Unused"
    overview[overview$effect %in% reference_groups, ]$type <- "Reference"
    overview[overview$effect %in% comparison_groups, ]$type <- "Comparison"
    orows <- nrow(overview)
    overview$x_breaks <- seq(from = 1, to = orows, by = 1)
    overview$x_values <- overview$x_breaks + x_nudge
    gdata$x_breaks <- seq(from = orows + 2, to = orows + 4, by = 1)
    gdata$x_value <- gdata$x_breaks + x_nudge
    gdata <- rbind(
      overview,
      gdata
    )
    x_end <- 4 + orows
  } else {
    gdata$x_breaks <- c(1, 2, 3)
    gdata$x_value <- gdata$x_breaks + x_nudge
  }

  # Update types for aesthetic control
  gdata$type <- paste(gdata$type, "_summary", sep = "")


  # Prep raw data ------------------
  if (plot_raw) {
    # Types
    rdata$type <- "Unused"
    if (!one_group) {
      rdata[rdata$grouping_variable %in% reference_groups, ]$type <- "Reference"
    }
    rdata[rdata$grouping_variable %in% comparison_groups, ]$type <- "Comparison"
    rdata$type <- paste(rdata$type, "_raw", sep = "")

    # x_value
    if (simple_contrast) {
      rdata$x_value <- gdata[match(rdata$grouping_variable, gdata$effect), "x_value"] - x_nudge
    } else {
      rdata$x_value <- overview[match(rdata$grouping_variable, overview$effect), "x_value"] - x_nudge
    }

    rdata$y_value <- rdata$outcome_variable
  }

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

  myplot <- myplot + ggplot2::scale_x_continuous(
    breaks = gdata$xbreaks,
    labels = gdata$x_labels
  )

  # Attach warnings and return    -------------------
  myplot$warnings <- warnings

  return(myplot)
}
