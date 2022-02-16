#' @export
plot_magnitude <- function(
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
  esci_assert_type(estimate, "is.estimate")
  effect_size <- match.arg(effect_size)
  data_layout <- match.arg(data_layout)
  error_layout <- match.arg(error_layout)
  error_normalize <- match.arg(error_normalize)
  if (is.null(data_spread) | !is.numeric(data_spread) | data_spread < 0) {
    data_spread <- 0.25
  }
  if (is.null(error_scale) | !is.numeric(error_scale) | error_scale < 0) {
    error_scale = 0.3
  }
  if (is.null(error_nudge) | !is.numeric(error_nudge) | error_nudge < 0) {
    error_nudge <- 0.25
  }
  if(is.null(ggtheme)) { ggtheme <- ggplot2::theme_classic()}


  # Data prep --------------------------------------
  conf_level <- estimate$properties$conf_level


  # Raw data
  plot_raw <- !is.null(estimate$raw_data) & data_layout != "none"
  if (plot_raw) {
    rdata <- estimate$raw_data
    rdata$type <- as.factor("raw")
    nudge <- error_nudge
  } else {
    nudge <- 0
  }

  # Group data
  if (effect_size == "mean") {
    gdata <- estimate$es_mean
    dist_call <- "dist = distributional::dist_student_t(
      df = df,
      mu = effect_size,
      sigma = se
     )"
  } else {
    gdata <- estimate$es_median
    dist_call <- "dist = distributional::dist_normal(
      mu = effect_size,
      sigma = se
     )"
  }
  gdata$type <- as.factor("summary")

  multiple_groups <- (nrow(gdata) > 1)


  # Build plot ------------------------------------
  # Base plot
  myplot <- ggplot2::ggplot() + ggtheme

  # Raw data
  if (plot_raw) {
      raw_glue <-
        "
    myplot <- myplot + ggplot2::geom_point(
      data = rdata,
      ggplot2::aes(
        x = grouping_variable,
        y = outcome_variable,
        color = type,
        shape = type,
        fill = type,
        alpha = type,
        size = type
      ),
      position = {raw_call$call}(
        groupOnX = TRUE,
        {raw_call$extras}
      )
    )
    "
      raw_call <- esci_plot_data_layouts(data_layout, data_spread)
      raw_expression <- parse(text = glue::glue(raw_glue))
      myplot <- try(eval(raw_expression))

  }


  # Group data
  error_glue <-
  "
    myplot <- myplot + {error_call}(
    data = gdata,
    orientation = 'vertical',
    ggplot2::aes(
      x = outcome_variable_name,
      y = effect_size,
      color = type,
      shape = type,
      size = type,
      point_color = type,
      point_fill = type,
      point_size = type,
      point_alpha = type,
      linetype = type,
      interval_color = type,
      interval_size = type,
      interval_alpha = type,
      slab_fill = type,
      slab_alpha = type,
      slab_linetype = type,
      dist = distributional::dist_student_t(
        df = df,
        mu = effect_size,
        sigma = SE
      )
    ),
    scale = {error_scale},
    .width = c({conf_level}),
    normalize = '{error_normalize}',
    position = position_nudge(x = {nudge})
  )
  "
  error_call <- esci_plot_error_layouts(error_layout)
  error_expression <- parse(text = glue::glue(error_glue))
  myplot <- try(eval(error_expression))

  # No legend
  myplot <- myplot + ggplot2::theme(legend.position = "none")


  # Customize plot -------------------------------
  # Points
  myplot <- myplot + ggplot2::scale_shape_manual(
    values = c("raw" = "circle filled", "summary" = "circle filled")
  )
  myplot <- myplot + ggplot2::scale_color_manual(
    values = c("raw" = "black", "summary" = "black"),
    aesthetics = c("color", "point_color")
  )
  myplot <- myplot + ggplot2::scale_fill_manual(
    values = c("raw" = "NA", "summary" = "gray"),
    aesthetics = c("fill", "point_fill")
  )
  myplot <- myplot + ggplot2::discrete_scale(
    c("size", "point_size"),
    "point_size_d",
    function(n) return(c("raw" = 1, "summary" = 3))
  )
  myplot <- myplot + ggplot2::discrete_scale(
    c("alpha", "point_alpha"),
    "point_alpha_d",
    function(n) return(c("raw" = 0.8, "summary" = 1))
  )

  # Error bars
  myplot <- myplot + ggplot2::scale_linetype_manual(
    values = c("summary" = "solid")
  )
  myplot <- myplot + ggplot2::scale_color_manual(
    values = c("summary" = "black"),
    aesthetics = "interval_color"
  )
  myplot <- myplot + ggplot2::discrete_scale(
    "interval_alpha",
    "interval_alpha_d",
    function(n) return(c("summary" = 1))
  )
  myplot <- myplot + ggplot2::discrete_scale(
    "interval_size",
    "interval_size_d",
    function(n) return(c("summary" = 3))
  )

  # Slab
  myplot <- myplot + ggplot2::scale_fill_manual(
    values = c("summary" = "gray"),
    aesthetics = "slab_fill"
  )
  myplot <- myplot + ggplot2::discrete_scale(
    "slab_alpha",
    "slab_alpha_d",
    function(n) return(c("summary" = 1))
  )


  # Labels -----------------------------
  vnames <- paste(estimate$es_mean$outcome_variable_name, collapse = ", ")
  ylab <- glue::glue("{vnames}\n{if (plot_raw) 'Data, ' else ''}{effect_size} and {conf_level*100}% confidence interval")
  xlab <- NULL

  myplot <- myplot + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)



  return(myplot)


}
