#'
#' @export
plot_mdiff <- function(
  estimate,
  effect_size = c("mean", "median"),
  data_layout = c("random", "swarm", "none"),
  data_spread = 0.15,
  error_layout = c("halfeye", "eye", "gradient", "none"),
  error_scale = 0.3,
  error_nudge = 0.4,
  error_normalize = c("groups", "all", "panels"),
  difference_axis_units = c("raw", "sd"),
  difference_axis_breaks = 5,
  difference_axis_space = 1,
  simple_contrast_labels = TRUE,
  ylim = c(NA, NA),
  ybreaks = 5,
  ggtheme = NULL
) {

  # Input checks ---------------------------------------------------------------
  warnings <- NULL

  esci_assert_type(estimate, "is.estimate")
  effect_size <- match.arg(effect_size)
  if (effect_size == "median" & is.null(estimate$es_median_difference)) {
    stop("effect_size parameter is 'median' but no median-based effect size available to plot")
  }
  data_layout <- match.arg(data_layout)
  error_layout <- match.arg(error_layout)
  error_normalize <- match.arg(error_normalize)
  difference_axis_units <- match.arg(difference_axis_units)
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
  if (is.null(difference_axis_breaks) | !is.numeric(difference_axis_breaks) | difference_axis_breaks < 1) {
    warnings <- c(
      warnings,
      glue::glue(
        "difference_axis_breaks = {difference_axis_breaks} but this is invalid; replaced with 5"
      )
    )
    difference_axis_breaks = 5
  }
  if(is.null(ggtheme)) { ggtheme <- ggplot2::theme_classic()}


  # Data prep --------------------------------------
  # Initialization
  conf_level <- estimate$properties$conf_level
  contrast <- estimate$properties$contrast
  reference_groups <- names(contrast[which(contrast < 0)])
  comparison_groups <- names(contrast[which(contrast > 0)])
  plot_raw <- !is.null(estimate$raw_data) & data_layout != "none"
  # simple_contrast <- (length(reference_groups) == 1) & (length(comparison_groups) == 1)
  simple_contrast <- (length(contrast) == 2)
  plot_paired <- !is.null(estimate$es_r)
  difference_es_name <- if (difference_axis_units == "sd")
    estimate$es_smd_properties$effect_size_name_html
  else
    if (effect_size == "mean")
      "<i>Mean</i><sub>diff</sub>"
    else
      "<i>Median</i><sub>diff</sub>"

  # Raw data
  if (plot_raw) {
    rdata <- estimate$raw_data
  } else {
    rdata <- NULL
  }

  # Group data
  if (effect_size == "mean") {
    gdata <- estimate$es_mean_difference
  } else {
    gdata <- estimate$es_median_difference
    gdata$df <- NA
  }
  gdata$y_value <- gdata$effect_size
  gdata$x_label <- gdata$effect

  if (simple_contrast) {
    if (simple_contrast_labels) {
      gdata$x_label[[3]] <- "Difference"
    }
  } else {
    if (simple_contrast_labels) {
      gdata$x_label <- c("Reference", "Comparison", "Difference")
    } else {
      gdata$x_label <- gdata$effect
      gdata$x_label <- gsub(" - ", "\n-\n", gdata$x_label)
      gdata$x_label <- gsub(" and ", "\nand\n", gdata$x_label)
    }
  }

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
    plot_paired = plot_paired,
    rdata = rdata,
    overview = overview,
    effect_size = effect_size,
    data_layout = data_layout,
    data_spread = data_spread,
    error_layout = error_layout,
    error_scale = error_scale,
    error_nudge = error_nudge,
    error_normalize = error_normalize,
    difference_axis_units = difference_axis_units,
    difference_axis_breaks = difference_axis_breaks,
    difference_axis_normalizer = estimate$es_smd$denominator[[1]],
    difference_es_name = difference_es_name,
    ylim = ylim,
    ybreaks = ybreaks,
    daxis_space = difference_axis_space,
    ggtheme = ggtheme
  )

  # Customize plot -------------------------------
  # Default aesthetics
  myplot <- esci_plot_mdiff_aesthetics(
    myplot,
    use_ggdist = (effect_size == "mean"),
    plot_paired = plot_paired
  )


  # Labels -----------------------------
  vnames <- if (plot_paired)
    paste(estimate$overview$outcome_variable_name, collapse = " and ", sep = "")
  else
    estimate$es_mean_difference$outcome_variable_name[[1]]
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
  plot_paired,
  rdata = NULL,
  overview = NULL,
  effect_size = c("mean", "median", "r", "P"),
  data_layout = c("random", "swarm", "none"),
  data_spread = 0.25,
  error_layout = c("halfeye", "eye", "gradient", "none"),
  error_scale = 0.3,
  error_nudge = 0.35,
  error_normalize = c("groups", "all", "panels"),
  difference_axis_units = c("raw", "sd"),
  difference_axis_breaks = 5,
  difference_axis_normalizer = 1,
  difference_es_name = "Difference",
  ylim = c(NA, NA),
  ybreaks = 5,
  daxis_space = 1,
  ggtheme = NULL
) {

  # Input checks ---------------------------------------------------------------
  warnings <- NULL
  difference_axis_units <- match.arg(difference_axis_units)

    # Data prep --------------------------------------
  # Initialization
  reference_groups <- names(contrast[which(contrast < 0)])
  comparison_groups <- names(contrast[which(contrast > 0)])
  simple_contrast <- is.null(overview)
  one_group <- is.na(gdata$SE[[2]])
  plot_raw <- !is.null(rdata)
  nudge <- if (plot_raw) error_nudge/2 else 0
  pooled_sd <- difference_axis_normalizer

  # Group data --------------------------------
  # Add comparison values to difference row
  comparison_es <- gdata[[1, "y_value"]]
  reference_es <- gdata[[2, "y_value"]]
  difference_LL <- gdata[[3, "LL"]]
  difference_UL <- gdata[[3, "UL"]]

  gdata[3, c("y_value", "LL", "UL")] <- gdata[3, c("y_value", "LL", "UL")]  + reference_es

  # Reorder comparison data
  gdata <- gdata[c(2, 1, 3), ]

  # Handle comparisons to a specified reference value
  if (one_group) {
    gdata[is.na(gdata$df), "df"] <- 1
    gdata[is.na(gdata$SE), "SE"] <- .Machine$double.xmin
  }
  if (nrow(gdata[gdata$SE <= 0, ]) > 0) {
    gdata[gdata$SE <= 0, ]$SE <- .Machine$double.xmin
  }

  # If complex contrast, add overview data -----------------

  if (!simple_contrast) {
    overview$type <- "Unused"
    overview[overview$effect %in% reference_groups, ]$type <- "Reference"
    overview[overview$effect %in% comparison_groups, ]$type <- "Comparison"
    orows <- nrow(overview)
    overview$x_value <- seq(from = 1, to = orows, by = 1)
    overview$nudge <- nudge

    rlines <- overview[overview$type == "Reference", c("y_value", "x_value", "nudge", "type")]
    clines <- overview[overview$type == "Comparison", c("y_value", "x_value", "nudge", "type") ]



    gdata$x_value <- seq(from = orows + 2, to = orows + 4, by = 1)
    gdata$nudge <- 0

    ref_x <- gdata$x_value[[1]] + gdata$nudge[[1]]
    comp_x <- gdata$x_value[[2]] + gdata$nudge[[2]]
    rlines$xend <- ref_x
    clines$xend <- comp_x
    rlines$yend <- rlines$y_value
    clines$yend <- clines$y_value

    if (nrow(clines) > 1) {
      clines$xend <- clines$xend - 0.5
      clines <- rbind(
        clines,
        data.frame(
          y_value = c(min(clines$yend), gdata$y_value[[2]]),
          x_value = c(comp_x - 0.5, comp_x - 0.5),
          nudge = c(0, 0),
          type = "Comparison",
          xend = c(comp_x - 0.5, comp_x),
          yend = c(max(clines$yend), gdata$y_value[[2]])
        )
      )
    }
    if (nrow(rlines) > 1) {
      rlines$xend <- rlines$xend - 0.5
      rlines <- rbind(
        clines,
        data.frame(
          y_value = c(min(rlines$yend), gdata$y_value[[1]]),
          x_value = c(ref_x - 0.5, ref_x - 0.5),
          nudge = c(0, 0),
          type = "Reference",
          xend = c(ref_x - 0.5, ref_x),
          yend = c(max(rlines$yend), gdata$y_value[[1]])
        )
      )
    }
    rlines$type <- paste(rlines$type, "_summary", sep = "")
    clines$type <- paste(clines$type, "_summary", sep = "")

    gdata <- rbind(
      overview,
      gdata
    )
    x_end <- 4 + orows
  } else {
    gdata$x_value <- c(1, 2, 3)
    if (plot_paired) {
      gdata$nudge <- c(nudge* -2, nudge, nudge)
    } else {
      gdata$nudge <- c(nudge, nudge, 0)
    }
  }

  # Update types for aesthetic control
  gdata$type <- paste(gdata$type, "_summary", sep = "")


  # Prep raw data ------------------
  if (plot_raw) {
    if (plot_paired) {
        rdata <- data.frame(
          grouping_variable = c(
            rep(gdata$effect[[1]], nrow(rdata)),
            rep(gdata$effect[[2]], nrow(rdata)),
            rep(gdata$effect[[3]], nrow(rdata))
          ),
          outcome_variable = c(
            rdata$reference_measure,
            rdata$comparison_measure,
            rdata$comparison_measure - rdata$reference_measure + reference_es
          )
        )
       rdata$type <- "Difference"
    }

    # Types
    if (!plot_paired) rdata$type <- "Unused"
    if (!one_group) {
      rdata[rdata$grouping_variable %in% reference_groups, ]$type <- "Reference"
    }
    rdata[rdata$grouping_variable %in% comparison_groups, ]$type <- "Comparison"


    rdata$type <- paste(rdata$type, "_raw", sep = "")

    # x_value
    if (simple_contrast) {
      rdata$x_value <- gdata[match(rdata$grouping_variable, gdata$effect), "x_value"]
    } else {
      rdata$x_value <- overview[match(rdata$grouping_variable, overview$effect), "x_value"]
    }

    rdata$y_value <- rdata$outcome_variable

  }


  # Difference axis ------------------------------------
  daxis_x <- max(gdata$x_value) + daxis_space

  if ( (difference_UL + reference_es) > reference_es) {
    rawEnd <- difference_UL
    saxisEnd <- ceiling(difference_UL/pooled_sd)
    if (saxisEnd < 1) saxisEnd = 1
  } else {
    rawEnd <- 0
    saxisEnd <- 0
  }

  if ( (difference_LL + reference_es) < reference_es) {
    rawStart <- difference_LL
    saxisStart <- floor(difference_LL/pooled_sd)
    if (saxisStart > -1) saxisStart = -1
  } else {
    rawStart <- 0
    saxisStart <- 0
  }


  if (difference_axis_units == "raw") {
    saxisBreaks <- pretty(c(rawStart,rawEnd), n = difference_axis_breaks)
    if(! 0 %in% saxisBreaks) {
      saxisBreaks <- sort(c(0, saxisBreaks))
    }
    slabels <- esci_scaleFUN(saxisBreaks)
  } else {
    saxisBreaks <- pretty(c(saxisStart,saxisEnd), n = difference_axis_breaks)
    slabels <- esci_scaleFUN(saxisBreaks)
    saxisBreaks <- saxisBreaks * pooled_sd
  }

  saxisStart <- reference_es + (saxisBreaks[1])
  saxisEnd <- reference_es + (saxisBreaks[length(saxisBreaks)])

  daxis_name <- difference_es_name
  # epart <-
  daxis_name <- gsub("<sub>", "[", daxis_name)
  daxis_name <- gsub("</sub>", "]", daxis_name)
  daxis_name <- gsub("<i>", "italic(", daxis_name)
  daxis_name <- gsub("</i>", ")", daxis_name)
  daxis_name <- parse(text = daxis_name)



  # Build plot ------------------------------------
  # Base plot
  myplot <- ggplot2::ggplot() + ggtheme

  if (!simple_contrast) {
    myplot <- myplot + ggplot2::geom_segment(
      data = rbind(rlines, clines),
      aes(
        x = x_value + nudge,
        xend = xend,
        y = y_value,
        yend = yend,
        color = type
      ),
      linetype = "solid"
    )

  }

  # Group data
  error_glue <-esci_plot_group_data(effect_size)
  error_call <- esci_plot_error_layouts(error_layout)
  error_expression <- parse(text = glue::glue(error_glue))
  myplot <- try(eval(error_expression))

  # Reference lines
  myplot <- myplot + ggplot2::geom_segment(
    data = tail(gdata, 3),
    aes(
      x = x_value + nudge,
      xend = daxis_x,
      y = y_value,
      yend = y_value,
    ),
    linetype = "dotted"
  )

  # Raw data
  if (plot_raw) {
    raw_expression <- esci_plot_raw_data(myplot, data_layout, data_spread)
    myplot <- try(eval(raw_expression))


    if (plot_paired) {
      p <- ggplot2::ggplot_build(myplot)
      last_layer <- length(p$data)
      m1 <- p$data[[last_layer]][1:(nrow(rdata)/3), ]
      m2 <- p$data[[last_layer]][((nrow(rdata)/3)+1):(nrow(rdata)*2/3), ]
      colnames(m2) <- paste("m2.", colnames(m2), sep = "")
      line_data <- cbind(m1, m2)

      myplot <- myplot + ggplot2::geom_segment(
        data = line_data,
        ggplot2::aes(
          x = x,
          xend = m2.x,
          y = y,
          yend = m2.y
        ),
        color = "gray",
        alpha = 0.5
      )

      temp <- myplot$layers[[last_layer]]
      myplot$layers[[last_layer]] <- myplot$layers[[last_layer+1]]
      myplot$layers[[last_layer+1]] <- temp

      #myplot <- try(eval(raw_expression))
    }

  }

  # paired measure lines
  if (plot_paired) {
    myplot <- myplot + ggplot2::geom_segment(
      data = NULL,
      ggplot2::aes(
        x = gdata$x_value[[1]] + gdata$nudge[[1]],
        xend = gdata$x_value[[2]] + gdata$nudge[[2]],
        y = gdata$y_value[[1]],
        yend = gdata$y_value[[2]]
      ),
      linetype = "solid",
      colour = "black"
    )
  }


  # Floating axis
  myplot <- myplot + ggplot2::geom_segment(color="black",
                                           linetype="solid",
                                           ggplot2::aes(x=daxis_x,
                                                        xend=daxis_x,
                                                        y=saxisStart,
                                                        yend=saxisEnd
                                           ),
                                           size=1
  )


  # Now define the y-axis
  p <- ggplot2::ggplot_build(myplot)
  lowest <- min(c(gdata$y_value, ylim[[1]]), na.rm = TRUE)
  highest <- max(c(gdata$y_value, ylim[[2]]), na.rm = TRUE)
  for (x in 1:length(p$data)) {
    lowest <- min(c(lowest, p$data[[x]]$y), na.rm = TRUE)
    highest <- max(c(highest, p$data[[x]]$y), na.rm = TRUE)
  }
  if (is.na(ylim[[1]])) {
    switch(
      effect_size,
      mean = {ylim[[1]] <- lowest*.85},
      rdiff = {ylim[[1]] <- min(c(-1, saxisBreaks+reference_es))},
      P = {ylim[[1]] <- min(c(0, saxisBreaks+reference_es))}
    )
  }
  if (is.na(ylim[[2]])) {
    switch(
      effect_size,
      mean = {ylim[[2]] <- highest*1.1},
      rdiff = {ylim[[2]] <- max(c(1, saxisBreaks+reference_es))},
      P = {ylim[[2]] <- max(c(1, saxisBreaks+reference_es))}
    )
  }
  lowest <- min(c(lowest, ylim))
  highest <- max(c(highest, ylim))

  myplot <- myplot + ggplot2::scale_y_continuous(
    limits = ylim,
    n.breaks = ybreaks,
    sec.axis = ggplot2::sec_axis(
      name = daxis_name,
      trans = ~.-reference_es,
      breaks = saxisBreaks,
      labels = slabels
    )
  )


  # Set x axis labels
  mybreaks <- gdata$x_value + gdata$nudge
  if (plot_paired) mybreaks[[1]] <- mybreaks[[1]] + nudge

  myplot <- myplot + ggplot2::scale_x_continuous(
    breaks = mybreaks,
    labels = gdata$x_label
  )


  # No legend and difference axis placement

  label_placement <- (highest-((saxisStart + saxisEnd)/2)) / (highest - lowest)

  myplot <- myplot + ggplot2::theme(
    legend.position = "none",
    axis.line.y.right = ggplot2::element_blank(),
    axis.title.y.right = ggplot2::element_text(hjust = label_placement)
  )


  # And finally, adjust coordinates
  # Set boundaries
  xmin <- min(gdata$x_value)
  xdeduct <- if (plot_paired) 2*nudge else nudge
  xmin <- xmin - xdeduct - 0.25



  myplot <- myplot + ggplot2::coord_cartesian(
    xlim = c(
      xmin,
      daxis_x
    ),
    ylim = ylim,
    expand = FALSE
  )


  # Attach warnings and return    -------------------
  myplot$warnings <- warnings

  return(myplot)
}



#'
#' @export
plot_pdiff <- function(
  estimate,
  error_layout = c("halfeye", "eye", "gradient", "none"),
  error_scale = 0.3,
  error_normalize = c("groups", "all", "panels"),
  simple_contrast_labels = TRUE,
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
  if(is.null(ggtheme)) { ggtheme <- ggplot2::theme_classic()}


  # Data prep --------------------------------------
  # Initialization
  conf_level <- estimate$properties$conf_level
  contrast <- estimate$properties$contrast
  reference_groups <- names(contrast[which(contrast < 0)])
  comparison_groups <- names(contrast[which(contrast > 0)])
  simple_contrast <- (length(reference_groups) == 1) & (length(comparison_groups) == 1)
  plot_paired <- !is.null(estimate$es_phi)
  rdata <- NULL
  effect_size <- "P"
  difference_es_name <- "<i>Proportion</i><sub>diff</sub>"

  gdata <- estimate$es_proportion_difference
  x <- 1
  found <- FALSE
  while ((x <= nrow(estimate$overview)) & (found == FALSE)) {
    clevel <- estimate$overview$outcome_variable_level[[x]]
    if (grepl(clevel, estimate$es_proportion_difference$effect[[1]])) found <- TRUE
    x <- x + 1
  }

  gdata$y_value <- gdata$effect_size
  gdata$x_label <- gsub(paste(" P_", clevel, sep = ""), "", gdata$effect)

  if (simple_contrast) {
    if (simple_contrast_labels) {
      gdata$x_label[[3]] <- "Difference"
    }
  } else {
    if (simple_contrast_labels) {
      gdata$x_label <- c("Reference", "Comparison", "Difference")
    } else {
      gdata$x_label <- gdata$effect
      gdata$x_label <- gsub(" - ", "\n-\n", gdata$x_label)
      gdata$x_label <- gsub(" and ", "\nand\n", gdata$x_label)
    }
  }


  # If complex contrast, add overview data
  if (!simple_contrast) {
    overview <- estimate$overview[estimate$overview$outcome_variable_level == clevel, ]
    overview <- data.frame(
      type = "Unused",
      outcome_variable_name = overview$outcome_variable_name,
      grouping_variable_name = overview$grouping_variable_name,
      effect = overview$grouping_variable_level,
      effect_size = overview$P,
      LL = overview$P_LL,
      UL = overview$P_UL,
      SE = overview$P_SE,
      x_label = overview$grouping_variable_level,
      y_value = overview$P
    )
  } else {
    overview <- NULL
  }

  myplot <- plot_mdiff_base(
    gdata = gdata,
    conf_level = conf_level,
    contrast = contrast,
    plot_paired = plot_paired,
    rdata = rdata,
    overview = overview,
    effect_size = effect_size,
    difference_es_name = difference_es_name,
    error_layout = error_layout,
    error_scale = error_scale,
    error_nudge = 0,
    error_normalize = error_normalize,
    ggtheme = ggtheme
  )

  # Customize plot -------------------------------
  # Default aesthetics
  myplot <- esci_plot_mdiff_aesthetics(
    myplot,
    use_ggdist = FALSE,
    plot_paired = plot_paired
  )


  # Labels -----------------------------
  if (plot_paired) {
    vname <- NULL
    xlab <- NULL
  } else {
    vname <- estimate$es_proportion_difference$outcome_variable_name[[1]]
    xlab <- estimate$es_proportion_difference$grouping_variable_name[[1]]
  }
  ylab <- glue::glue("{vname}\nProportion {clevel} and {conf_level*100}% confidence interval")



  myplot <- myplot + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)


  # Attach warnings and return    -------------------
  myplot$warnings <- c(myplot$warnings, warnings)

  return(myplot)

}



#'
#' @export
plot_rdiff <- function(
  estimate,
  error_layout = c("halfeye", "eye", "gradient", "none"),
  error_scale = 0.3,
  error_normalize = c("groups", "all", "panels"),
  simple_contrast_labels = TRUE,
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
  if(is.null(ggtheme)) { ggtheme <- ggplot2::theme_classic()}


  # Data prep --------------------------------------
  # Initialization
  conf_level <- estimate$properties$conf_level
  contrast <- c(1, -1)
  names(contrast) <- estimate$es_r[1:2, "effect"]
  reference_groups <- names(contrast[which(contrast < 0)])
  comparison_groups <- names(contrast[which(contrast > 0)])
  simple_contrast <- (length(reference_groups) == 1) & (length(comparison_groups) == 1)
  plot_paired <- FALSE
  rdata <- NULL
  effect_size <- "rdiff"
  difference_es_name <- "<i>r</i><sub>diff</sub>"

  gdata <- estimate$es_rdiff
  if (is.null(gdata)) gdata <- estimate$es_r
  gdata$y_value <- gdata$effect_size
  gdata$x_label <- gdata$effect

  if (simple_contrast) {
    if (simple_contrast_labels) {
      gdata$x_label[[3]] <- "Difference"
    }
  } else {
    if (simple_contrast_labels) {
      gdata$x_label <- c("Reference", "Comparison", "Difference")
    } else {
      gdata$x_label <- gdata$effect
      gdata$x_label <- gsub(" - ", "\n-\n", gdata$x_label)
      gdata$x_label <- gsub(" and ", "\nand\n", gdata$x_label)
    }
  }


  # If complex contrast, add overview data
  if (!simple_contrast) {
      # tbd in case this is ever impplemented
  } else {
    overview <- NULL
  }

  myplot <- plot_mdiff_base(
    gdata = gdata,
    conf_level = conf_level,
    contrast = contrast,
    plot_paired = plot_paired,
    rdata = rdata,
    overview = overview,
    effect_size = effect_size,
    difference_es_name = difference_es_name,
    error_layout = error_layout,
    error_scale = error_scale,
    error_nudge = 0,
    error_normalize = error_normalize,
    ggtheme = ggtheme
  )

  # Customize plot -------------------------------
  # Default aesthetics
  myplot <- esci_plot_mdiff_aesthetics(
    myplot,
    use_ggdist = FALSE,
    plot_paired = plot_paired
  )

  # Labels -----------------------------
  vname <- paste(estimate$es_r$x_variable_name[[1]], estimate$es_r$y_variable_name[[1]], sep = " and ")
  ylab <- glue::glue("Correlation between {vname}\nr and {conf_level*100}% confidence interval")
  xlab <- estimate$es_r$grouping_variable[[1]]


  myplot <- myplot + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)


  # Attach warnings and return    -------------------
  myplot$warnings <- c(myplot$warnings, warnings)

  return(myplot)

}
