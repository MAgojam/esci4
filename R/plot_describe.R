#' Plot a histogram or dotplot of an estimated magnitude with raw data
#'
#' @description
#' `esci_plot_describe` returns a ggplot2 object
#'
#'
#' @param estimate A esci_estimate object with raw data an es_mean
#' @param type histogram or dotplot
#' @param mark_mean should mean be marked?
#' @param mark_median should median be marked?
#' @param mark_sd should mean be marked?
#' @param mark_quartiles should mean be marked?
#' @param mark_z_lines should z lines be marked?
#' @param mark_percentile a percentile (0 to 1) to be marked
#' @param histogram_bins number of bins if a histogram
#' @param ylim 2-length numeric vector
#' @param xlim 2-length numeric vector
#' @param fill_regular color for
#' @param fill_highlighted color for
#' @param color outline color
#' @param ggtheme theme to apply, if any
#'
#' @export
esci_plot_describe <- function(
  estimate,
  type = c("histogram", "dotplot"),
  mark_mean = FALSE,
  mark_median = FALSE,
  mark_sd = FALSE,
  mark_quartiles = FALSE,
  mark_z_lines = FALSE,
  mark_percentile = NULL,
  histogram_bins = 12,
  ylim = c(NA, NA),
  xlim = c(NA, NA),
  fill_regular = "gray45",
  fill_highlighted = "red",
  color = "blue",
  marker_size = 5,
  ggtheme = NULL
) {

  # Input checks ------------------------------------------------------
  esci_assert_type(estimate, "is.estimate")

  if(is.null(estimate$es_mean)) {
    stop("This plot function is for a single quantiative variable; this estimate passed is not the right type.")
  }

  if(is.null(estimate$raw_data)) {
    stop("This plot function requires raw data, but the estimate passed does not have raw data attached.")
  }

  type <- match.arg(type)

  if (is.null(mark_percentile)) {
    draw_percentile <- FALSE
  } else {
    draw_percentile <- TRUE
    esci_assert_type(mark_percentile, "is.numeric")
    esci_assert_range(
      mark_percentile,
      lower = 0,
      upper = 1,
      lower_inclusive = TRUE,
      upper_inclusive = TRUE
    )
  }

  if(is.null(ggtheme)) { ggtheme <- ggplot2::theme_classic()}


  # Prep -------------------------------------------------------------
  # Handle
  rd <- estimate$raw_data
  rd_mean <- estimate$overview$mean[1]
  rd_sd <- estimate$overview$sd[1]
  rd_median <- estimate$overview$median[1]

  # Marking percentile
  fills <- c("TRUE" = fill_highlighted, "FALSE" = fill_regular)
  q_value <- if(draw_percentile)
      quantile(
        x = rd$outcome_variable,
        probs = c(mark_percentile)
      )
    else
      min(rd$outcome_variable) - 1


  # Plot --------------------------------------------------------------
  # Base plot
  myplot <- ggplot2::ggplot(
    data = rd,
    ggplot2::aes(
      x = outcome_variable,
      fill = stat(x < q_value)
    )
  )

  # Theme
  myplot <- myplot + ggtheme

  # Fills for marking percentiles
  myplot <- myplot + ggplot2::scale_fill_manual(values = fills)

  # Histogram or dotplot
  if (type == "histogram") {
    myplot <- myplot + ggdist::stat_histinterval(
      slab_color = color,
      outline_bars = TRUE,
      orientation = "horizontal",
      breaks = histogram_bins,
      interval_alpha = 0,
      point_alpha = 0
    )
  } else {
    myplot <- myplot + ggdist::stat_dotsinterval(
      orientation = "horizontal",
      interval_alpha = 0,
      point_alpha = 0
    )
  }

  # Mark mean
  if (mark_mean) {
    myplot <- myplot + ggplot2::geom_vline(
      xintercept = rd_mean,
      linetype = "dotted"
    )
    myplot <- myplot + ggplot2::geom_point(
      data = data.frame(
        x = rd_mean,
        y = 0
      ),
      ggplot2::aes(
        x = x,
        y = y
      ),
      shape = 24,
      size = marker_size
    )
  }

  # Median
  top <- max(ggplot2::ggplot_build(myplot)$data[[1]]$cdf)

  if (mark_median) {
    myplot <- myplot + ggplot2::geom_point(
      data = data.frame(
        x = rd_median,
        y = top + .2
      ),
      fill = "pink",
      shape = 23,
      size = marker_size,
      ggplot2::aes(
        x = x,
        y = y
      )
    )

    myplot <- myplot + ggplot2::geom_vline(
      xintercept = rd_median,
      color = "black",
      linetype = "dashed"
    )
  }


  if (mark_sd) {
    sd_begin <- rd_mean - rd_sd
    sd_end <- rd_mean - rd_sd
    sd_df <- data.frame(
      x = c(sd_begin, sd_end),
      y = top + .4
    )
    myplot <- myplot + ggplot2::geom_segment(
      color = "black",
      linetype = "solid",
      ggplot2::aes(
        x = sd_begin,
        xend = sd_end,
        y = top + .4,
        yend = top + .4
      )
    )
    myplot <- myplot + ggplot2::geom_point(
      data = sd_df,
      fill = "blue",
      shape = 25,
      size = marker_size,
      ggplot2::aes(
        x = x,
        y = y
      )
    )
  }

  if (mark_z_lines) {
    for (i in -3:3) {
      cz <- rd_mean + (rd_sd * i)
      myplot <- myplot + ggplot2::geom_vline(
        color = "black",
        linetype = "dotted",
        xintercept = cz
      )
    }
    zdata <- data.frame(
      z = c(-3:3),
      x = cz,
      y = top + .5
    )
    zdata$label <- "z ="
    zdata$label <- paste(zdata$label, zdata$z)
    myplot <- myplot + ggplot2::geom_text(
      data = zdata,
      ggplot2::aes(
        x=x,
        y=y,
        label=label
      )
    )

  }


  # Finishing touches ------------------------------------
  myplot <- myplot +  ggplot2::scale_y_continuous(
    limits = ylim,
    expand = c(0,NA)
  )
  myplot <- myplot + ggplot2::scale_x_continuous(
    limits = xlim
  )
  myplot <- myplot + ggplot2::theme(legend.position="none")



  return(myplot)

}
