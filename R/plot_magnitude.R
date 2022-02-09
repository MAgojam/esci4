#' @export
plot_magnitude <- function(estimate, plot_what = "mean") {

  conf_level <- estimate$properties$conf_level

  gdata <- if (plot_what == "mean")
    estimate$es_mean
  else
    estimate$es_median

  myplot <- ggplot2::ggplot()

  myplot <- myplot + ggdist::stat_dist_halfeye(
    data = gdata,
    orientation = 'vertical',
    aes(
      shape = outcome_variable_name,
      point_size = outcome_variable_name,
      point_color = outcome_variable_name,
      point_fill = outcome_variable_name,
      point_alpha = outcome_variable_name,
      linetype = outcome_variable_name,
      interval_color = outcome_variable_name,
      x = outcome_variable_name,
      y = effect_size,
      dist = distributional::dist_student_t(
        df = df,
        mu = effect_size,
        sigma = SE
      )
    ),
    .width = c(conf_level),
  )

  myplot <- myplot + ggplot2::theme(legend.position = "none")

  myplot

  myplot <- myplot + ggplot2::scale_shape_manual(values = c("square filled"))
  myplot <- myplot + ggdist::scale_point_size_discrete(range = c(10, 10))
  myplot <- myplot + ggplot2::scale_color_manual(values = c("red"), aesthetics = "point_color")
  myplot <- myplot + ggplot2::scale_fill_manual(values = c("blue"), aesthetics = "point_fill")
  myplot <- myplot + ggdist::scale_point_alpha_discrete(range = c(1,1))

  myplot <- myplot + ggplot2::scale_linetype_manual(values = c("dashed"))
  myplot <- myplot + ggplot2::scale_color_manual(values = c("yellow"), aesthetics = "interval_color")




  myplot






  # myplot <- myplot + ggplot2::geom_point(
  #   data = gdata,
  #   ggplot2::aes(
  #     x = outcome_variable_name,
  #     y = effect_size,
  #     size = "size",
  #     shape = "shape",
  #     alpha = "alpha",
  #     fill = "fill",
  #     colour = "colour"
  #   )
  # )


  # myplot <- myplot + ggplot2::scale_size_manual(values = c(2))
  # myplot <- myplot + ggplot2::scale_shape_manual(values = c("diamond filled"))
  # myplot <- myplot + ggplot2::scale_fill_manual(values = c("blue"))
  # myplot <- myplot + ggplot2::scale_colour_manual(values = c("green"))
  # myplot <- myplot + ggplot2::scale_alpha_manual(values = c(0.5))



  return(myplot)


}
