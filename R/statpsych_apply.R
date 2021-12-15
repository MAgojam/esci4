apply_ci_mdiff <- function(
  myrow,
  assume_equal_variance = FALSE,
  effect_size = c("raw", "smd"),
  conf_level = 0.95
) {

  effect_size <- match.arg(effect_size)

  if (effect_size == "raw") {
    res <- as.data.frame(
      statpsych::ci.mean2(
        alpha = 1 - conf_level,
        m1 = myrow[["comparison_mean"]],
        m2 = myrow[["reference_mean"]],
        sd1 = myrow[["comparison_sd"]],
        sd2 = myrow[["reference_sd"]],
        n1 = myrow[["comparison_n"]],
        n2 = myrow[["reference_n"]]
      )
    )
  } else {
    res <- as.data.frame(
      statpsych::ci.stdmean2(
        alpha = 1 - conf_level,
        m1 = myrow[["comparison_mean"]],
        m2 = myrow[["reference_mean"]],
        sd1 = myrow[["comparison_sd"]],
        sd2 = myrow[["reference_sd"]],
        n1 = myrow[["comparison_n"]],
        n2 = myrow[["reference_n"]]
      )
    )
  }

  res_row <- if(assume_equal_variance) 1 else 2

  res <- c(
    res[res_row, "Estimate"],
    res[res_row, "SE"]^2,
    res[res_row, "LL"],
    res[res_row, "UL"]
  )
  names(res) <- c("yi", "vi", "LL", "UL")

  return(res)
}

