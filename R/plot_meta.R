plot_meta <- function(
    estimate
) {


  myplot <- ggplot2::ggplot()

  # Raw data prep
  rdata <- estimate$raw_data
  if (is.null(rdata$moderator)) {
    rdata$moderator <- "None"
  }
  rows_raw <- nrow(rdata)
  rdata$line <- -seq(1:rows_raw)


  # Raw data plot
  myplot <- myplot + ggplot2::geom_errorbarh(
    data = rdata,
    ggplot2::aes(
      xmin = LL,
      xmax = UL,
      y = line,
      colour = moderator
    ),
    height = 0
  )

  myplot <- myplot + ggplot2::geom_point(
    data = rdata,
    ggplot2::aes(
      x = effect_size,
      y = line,
      colour = moderator,
      size = weight
    )
  )


  # Group data
  gdata <- estimate$es_meta
  rows_total <- rows_raw + nrow(gdata)
  rows_start <- rows_raw+1
  gdata$line <- -seq(from = rows_start, to = row_total)

  # Plot group data
  myplot <- myplot + ggplot2::geom_errorbarh(
    data = gdata,
    ggplot2::aes(
      xmin = LL,
      xmax = UL,
      y = line,
      colour = moderator_variable_level
    ),
    height = 0
  )

  # Difference data
  dlabel <- NULL
  if (!is.null(estimate$es_meta_difference) & nrow(gdata) == 3) {
    ddata <- estimate$es_meta_difference["Difference", ]
    reference <- estimate$es_meta_difference["Reference", "effect_size"]

    rows_total <- rows_total + 1
    ddata$line <- -rows_total

    myplot <- myplot + ggplot2::geom_errorbarh(
      data = ddata,
      ggplot2::aes(
        xmin = LL,
        xmax = UL,
        y = line,
        colour = moderator_level
      ),
      height = 0
    )

    dlabel <- ddata$moderator_level
  }

  # Clean up
  all_labels <- c(
    rdata$label,
    gdata$moderator_variable_level,
    dlabel
  )
  myplot <- myplot + ggplot2::scale_y_continuous(
    breaks = -seq(1:rows_total),
    labels = all_labels
  )

  myplot <- myplot + ggplot2::ylab(NULL)
  myplot <- myplot + ggplot2::xlab(estimate$properties$effect_size_name)
  myplot <- myplot + ggplot2::theme(legend.position = "none")

  myplot
}
