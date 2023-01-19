
plot_scatter <- function(
    estimate,
    show_line = FALSE,
    show.meanCI = FALSE,
    show_PI = FALSE,
    predict_from_x = NULL,
    ggtheme = ggplot2::theme_classic()
) {

    # Check inputs ---------------------------
  # Needs to be of class estimate
  esci_assert_type(estimate, "is.estimate")

  # Check raw data available
  if (is.null(estimate$raw_data) | nrow(estimate$raw_data) == 0) {
    err_string <- stringr::str_interp(
      "This plot only works for estimates with raw data available."
    )
    stop(err_string)
  }

  plotting_groups <- !is.null(estimate$overview$grouping_variable_name)


  rdata <- estimate$raw_data
  if (!plotting_groups) {
    rdata <- rdata[ , 1:2]
    colnames(rdata) <- c("x", "y")
  }

  # predict_from_x
  if(!is.null(predict_from_x)) {
    if(!is.numeric(predict_from_x)) {
      predict_from_x <- NULL
      warning_string <- stringr::str_interp("'predict_from_x' ignored because it is not numeric, instead it is = ${class(predict_from_x)}.")
      warning(warning_string)
    } else {
      if(predict_from_x > max(rdata$x, na.rm = TRUE)) {
        predict_from_x <- NULL
        warning_string <- stringr::str_interp("'predict_from_x' ignored because it is out of range.  predict_from_x = ${predict_from_x}, but highest y value is ${max(rdata$x, na.rm = TRUE)}.")
        warning(warning_string)
      } else {
        if(predict_from_x < min(rdata$x, na.rm = TRUE)) {
          predict_from_x <- NULL
          warning_string <- stringr::str_interp("'predict_from_x' ignored because it is out of range.  predict_from_x = ${predict_from_x}, but lowest x value is ${min(rdata$x, na.rm = TRUE)}.")
          warning(warning_string)
        }
      }
    }
  }


  # Prep for building graph ---------------------------
  ylab <- estimate$es_r$y_variable_name
  xlab <- estimate$es_r$x_variable_name


  # Build the plot
  myplot <- ggplot2::ggplot() + ggtheme

  if (plotting_groups) {
    myplot <- myplot + ggplot2::geom_point(
      data = rdata,
      ggplot2::aes(
        x = x,
        y = y,
        shape = grouping_variable,
        fill = grouping_variable,
        color = grouping_variable
      )
    )
  } else {
    myplot <- myplot + ggplot2::geom_point(
      data = rdata,
      ggplot2::aes(
        x = x,
        y = y
      )
    )
  }
  myplot <- esci_plot_layers(myplot, "scatter")

  # Style options
  if (!plotting_groups) {
    myplot <- myplot + theme(legend.position="none")
  } else {
    myplot <- myplot + scale_fill_brewer(type = "qual")
    myplot <- myplot + scale_color_brewer(type = "qual")
    myplot <- myplot + guides(color=guide_legend(title=estimate$plot_info$group_name))
    myplot <- myplot + guides(shape=guide_legend(title=estimate$plot_info$group_name))
    myplot <- myplot + guides(fill=guide_legend(title=estimate$plot_info$group_name))
  }
  myplot <- myplot + theme(axis.line = element_line(size = 1, linetype = "solid"))


  if (show_line) {myplot <- myplot + geom_smooth(method='lm', se = show.meanCI)}

  # if (show_PI) {
  #   myplot <- myplot + geom_line(aes(y=lwr), color = "red", linetype = "dashed")
  #   myplot <- myplot + geom_line(aes(y=upr), color = "red", linetype = "dashed")
  # }
  #
  #
  # if(!is.null(predict_from_x)) {
  #   ypr <- predict_from_x*estimate$b+estimate$a
  #   yplabel <- paste("Y' = ", format(ypr, digits = 2))
  #   #    xhalf <- predict_from_x - (predict_from_x - min(s_data$iv) ) /2
  #   #    yhalf <- ypr + ypr*.10
  #   myplot <- myplot + annotate("text", x = min(s_data$iv) - 0.1*xrange, y = ypr, label = yplabel, color = "red")
  #   xlabel <- paste("X =", predict_from_x)
  #   #    xhalf <- predict_from_x + predict_from_x*.10
  #   yhalf <- min(s_data$dv) - (0.1* (max(s_data$dv) - min(s_data$dv)) )
  #   #    yhalf <- ypr - (ypr - min(s_data$dv) )/2
  #   myplot <- myplot + annotate("text", x = predict_from_x, y = yhalf, label = xlabel, color = "red")
  #   pi <- predict(estimate$lm, interval = "prediction", newdata = data.frame(iv = predict_from_x), level = estimate$conf.level)
  #   xlab <- paste(xlab, "\nAt X =", predict_from_x, ": Y' =", format(ypr, digits = 2), ", ", format(estimate$conf.level*100, digits=0), "% PI[", format(pi[1, "lwr"], digits=2), ",", format(pi[1, "upr"], digits=2), "]")
  #   myplot <- myplot + geom_segment(alpha = 0.1, size = 1, color = "red", aes(x = predict_from_x, xend = predict_from_x, y=pi[1, "lwr"], yend = pi[1, "upr"]))
  #   myplot <- myplot + geom_segment(linetype = "dotted", aes(x = predict_from_x, xend = predict_from_x, y = min(s_data$dv), yend = predict_from_x*estimate$b+estimate$a))
  #   myplot <- myplot + geom_segment(linetype = "dotted", aes(x = min(s_data$iv), xend = predict_from_x, y = predict_from_x*estimate$b+estimate$a, yend = predict_from_x*estimate$b+estimate$a))
  #   myplot <- myplot + annotate("point", x = predict_from_x, y = ypr, colour = "red", shape = 23, size=size+1, fill="white")
  # }

  myplot <- myplot + ylab(ylab) + xlab(xlab)


  return(myplot)
}

