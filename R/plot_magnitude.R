#'
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
   } else {
    gdata <- estimate$es_median
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
  if (effect_size == "mean") {

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
      position = ggplot2::position_nudge(x = {nudge})
    )
    "
    error_call <- esci_plot_error_layouts(error_layout)
    error_expression <- parse(text = glue::glue(error_glue))
    myplot <- try(eval(error_expression))
  } else {

    myplot <- myplot + ggplot2::geom_segment(
      data = gdata,
      ggplot2::aes(
        x = outcome_variable_name,
        xend = outcome_variable_name,
        y = LL,
        yend = UL,
        alpha = type,
        size = type,
        linetype = type
      ),
      position = ggplot2::position_nudge(x = nudge)
    )

    myplot <- myplot + ggplot2::geom_point(
      data = gdata,
      ggplot2::aes(
        x = outcome_variable_name,
        y = effect_size,
        color = type,
        shape = type,
        fill = type,
        alpha = type,
        size = type
      ),
      position = ggplot2::position_nudge(x = nudge)
    )

  }

  # Customize plot -------------------------------
  # No legend
  myplot <- myplot + ggplot2::theme(legend.position = "none")

  # Default aesthetics
  myplot <- esci_plot_simple_aesthetics(myplot)

  # Labels -----------------------------
  vnames <- paste(estimate$es_mean$outcome_variable_name, collapse = ", ")
  ylab <- glue::glue("{vnames}\n{if (plot_raw) 'Data, ' else ''}{effect_size} and {conf_level*100}% confidence interval")
  xlab <- NULL

  myplot <- myplot + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)


  # Attach warnings and return    -------------------
  myplot$warnings <- warnings

  return(myplot)


}


#' @export
plot_correlation <- function(
  estimate,
  error_layout = c("halfeye", "eye", "gradient", "none"),
  error_scale = 0.3,
  error_normalize = c("groups", "all", "panels"),
  ggtheme = NULL
) {

  # Input checks ---------------------------------------------------------------
  warnings <- NULL

  esci_assert_type(estimate, "is.estimate")
  error_layout <- match.arg(error_layout)
  error_normalize <- match.arg(error_normalize)
  if (is.null(error_scale) | !is.numeric(error_scale) | error_scale < 0) {
    warnings <- c(
      warnings,
      glue::glue(
        "error_scale = {error_scale} but this is invalid; replaced with 0.3"
      )
    )
    error_scale = 0.3
  }


  # Data prep --------------------------------------
  conf_level <- estimate$properties$conf_level

  gdata <- estimate$es_r
  gdata$type <- as.factor("summary")


  # Build plot ------------------------------------
  # Base plot
  myplot <- ggplot2::ggplot() + ggtheme

  error_glue <-
    "
     myplot <- myplot + {error_call}(
      data = gdata,
      ggplot2::aes(
        y = effect_size,
        x = paste(x_variable_name, '\nvs.\', y_variable_name),
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
        dist = distributional::dist_transformed(
          distributional::dist_normal(
            mu = esci_trans_r_to_z(effect_size),
            sigma = esci_trans_rse_to_sez(n)
          ),
          transform = esci_trans_z_to_r,
          inverse = esci_trans_identity
        )
      ),
      scale = {error_scale},
      .width = c({conf_level}),
      normalize = '{error_normalize}'
    )
    "
  error_call <- esci_plot_error_layouts(error_layout)
  error_expression <- parse(text = glue::glue(error_glue))
  myplot <- try(eval(error_expression))


  # Customize plot ------------------------------
  # No legend
  myplot <- myplot + ggplot2::theme(legend.position = "none")

  # Defualt look
  myplot <- esci_plot_simple_aesthetics(myplot)

  #Labels
  ylab <- glue::glue("Pearson's r and {conf_level*100}% confidence interval")
  xlab <- NULL
  myplot <- myplot + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)

  # Limits
  myplot <- myplot + ylim(-1, 1)


  # Attach warnings and return    -------------------
  myplot$warnings <- warnings

  return(myplot)

}


#' @export
plot_proportion <- function(
  estimate,
  error_layout = c("halfeye", "eye", "gradient", "none"),
  error_scale = 0.3,
  error_normalize = c("groups", "all", "panels"),
  ggtheme = NULL
) {

  # Input checks ---------------------------------------------------------------
  warnings <- NULL

  esci_assert_type(estimate, "is.estimate")
  error_layout <- match.arg(error_layout)
  error_normalize <- match.arg(error_normalize)
  if (is.null(error_scale) | !is.numeric(error_scale) | error_scale < 0) {
    warnings <- c(
      warnings,
      glue::glue(
        "error_scale = {error_scale} but this is invalid; replaced with 0.3"
      )
    )
    error_scale = 0.3
  }


  # Data prep --------------------------------------
  conf_level <- estimate$properties$conf_level

  gdata <- estimate$overview
  gdata$type <- as.factor("summary")


  # Build plot ------------------------------------
  # Base plot
  myplot <- ggplot2::ggplot() + ggtheme

  error_glue <-
    "
     myplot <- myplot + {error_call}(
      data = gdata,
      ggplot2::aes(
        y = P,
        x = outcome_variable_level,
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
        dist = distributional::dist_transformed(
          distributional::dist_normal(
            mu = P,
            sigma = P_SE
          ),
          transform = esci_trans_P,
          inverse = esci_trans_identity
        )
      ),
      scale = {error_scale},
      .width = c({conf_level}),
      normalize = '{error_normalize}'
    )
    "

  error_call <- esci_plot_error_layouts(error_layout)
  error_expression <- parse(text = glue::glue(error_glue))
  myplot <- try(eval(error_expression))

  # Customize ----------------------------
  # No legend
  myplot <- myplot + ggplot2::theme(legend.position = "none")

  # Default look
  myplot <- esci_plot_simple_aesthetics(myplot)

  # Labels
  ylab <- glue::glue("Proportion and {conf_level*100}% confidence interval")
  xlab <- NULL
  myplot <- myplot + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)

  # Limits
  myplot <- myplot + ylim(0, 1)

  return(myplot)

}



esci_plot_simple_aesthetics <- function(myplot) {
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

  return(myplot)
}


esci_trans_r_to_z <- function(r) {
  return ( log((1 + r)/(1 - r))/2 )
}

esci_trans_rse_to_sez <- function(n) {
  return(sqrt(1/((n - 3))))
}

esci_trans_z_to_r <- function(x) {
  return ( (exp(2*x) - 1)/(exp(2*x) + 1) )
}

esci_trans_P <- function(x) {
  x[which(x < 0)] <- 0
  x[which(x > 1)] <- 1
  return(x)
}

esci_trans_identity <- function(x) {
  return(x)
}

rtrans <- function(x) {
  return ( log((1 + x)/(1 - x))/2 )
}

rback <- function(x) {
  return ( (exp(2*x) - 1)/(exp(2*x) + 1) )
}


dist_P <- function(mu = 0, sigma = 1, f, n){
  mu <- vec_cast(mu, double())
  sigma <- vec_cast(sigma, double())
  if(any(sigma[!is.na(sigma)] < 0)){
    abort("Standard deviation of a normal distribution must be non-negative")
  }
  new_dist(mu = mu, sigma = sigma, f = f, n = n, class = "dist_normal")
}


