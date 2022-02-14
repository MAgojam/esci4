#' @export
plot_magnitude <- function(estimate, plot_what = "mean") {

  conf_level <- estimate$properties$conf_level

  if (plot_what == "mean") {
    gdata <- estimate$es_mean
  } else {
    gdata <- estimate$es_median
  }

  gdata$type <- as.factor("summary")

  plot_raw <- !is.null(estimate$raw_data)
  if (plot_raw) {
    rdata <- estimate$raw_data
    rdata$type <- as.factor("raw")
  }

  myplot <- ggplot2::ggplot()

  myplot <- myplot + ggbeeswarm::geom_beeswarm(
    data = rdata,
    ggplot2::aes(
      x = grouping_variable,
      y = outcome_variable,
      color = type,
      shape = type,
      fill = type,
      alpha = type
    )
  )

  myplot <- myplot + ggdist::stat_dist_halfeye(
    data = gdata,
    orientation = 'vertical',
    ggplot2::aes(
      color = type,
      shape = type,
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
      x = outcome_variable_name,
      y = effect_size,
      dist = distributional::dist_student_t(
        df = df,
        mu = effect_size,
        sigma = SE
      )
    ),
    scale = 0.33,
    .width = c(conf_level),
    position = ggplot2::position_nudge(x = 0.20)
  )


  myplot <- myplot + ggplot2::theme(legend.position = "none")

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
    c("point", "point_size"),
    "point_size_d",
    function(n) return(c("raw" = 3, "summary" = 3))
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


  myplot


  return(myplot)


}
