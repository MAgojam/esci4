#' @export
test_mdiff <- function(
    estimate,
    effect_size = c("mean", "median"),
    rope_lower = 0,
    rope_upper = 0,
    rope_units = c("raw", "sd"),
    output_html = FALSE
)

{


  # Input Checks ---------------------
  # This function expects:
  #   estimate should be of class estimate
  #   effect_size = "mean" | "median"
  #   rope_lower <= 0
  #   rope_upper >= 0
  #   If rope_lower and rope_upper = 0, only traditional nil test returned
  #   0 < alpha < 1
  #   rope_units = "sd" | "raw"
  esci_assert_type(estimate, "is.estimate")
  effect_size <- match.arg(effect_size)
  esci_assert_range(
    var = rope_lower,
    upper = 0,
    upper_inclusive = TRUE
  )
  esci_assert_range(
    var = rope_upper,
    lower = 0,
    lower_inclusive = TRUE
  )

  rope_units <- match.arg(rope_units)

  if (effect_size == "median" & is.null(estimate$es_median_difference)) {
    stop("Effect size is median, but estimate object passed doesn't have a median difference table.")
  }

  if (effect_size == "median" & is.null(estimate$es_mean_difference)) {
    stop("Effect size is mean, but estimate object passed doesn't have a mean difference table.")
  }

  if (rope_units == "sd" & is.null(estimate$es_smd)) {
    stop("Units are sd, but estimate object passed doesn't have a standardized-mean difference table.")

  }

  # Prep ------------------------------------------
  etable <- if(effect_size == "mean") "es_mean_difference" else "es_median_difference"

  if (output_html) {
    statistic <- if(effect_size == "mean") "<i>M</i><sub>diff</sub>" else "<i>Mdn</i><sub>diff</sub>"
    parameter <- if(effect_size == "mean") "<i>&mu;</i><sub>diff</sub>" else "<i>&eta;</i><sub>diff</sub>"
    p_symbol <- "<i>p</i>"
  } else {
    statistic <- if(effect_size == "mean") "M_diff" else "Mdn_diff"
    parameter <- if(effect_size == "mean") "\U003BC_diff" else "\U003B7_diff"
    p_symbol <- "p"
  }

  alpha <- 1 - estimate$properties$conf_level
  confidence <- estimate$properties$conf_level*100
  confidence_2alpha <- (1 - 2*alpha)*100

  interval_null <- if (rope_lower != rope_upper) TRUE else FALSE

  # Reduce down to differences
  effect_sizes <- estimate[[etable]][
    estimate[[etable]]$type == "Difference",
  ]


  if (effect_size == "mean") {
    effect_sizes$tcrit <- qt(p = alpha, df = effect_sizes$df, lower.tail = FALSE)
    effect_sizes$ta_LL <- effect_sizes$effect_size - (effect_sizes$SE * effect_sizes$tcrit)
    effect_sizes$ta_UL <- effect_sizes$effect_size + (effect_sizes$SE * effect_sizes$tcrit)
  }

  res <- list()
  res$properties <- list(
    effect_size_name = effect_size,
    alpha = alpha,
    interval_null = interval_null,
    rope_units = rope_units
  )

  # Loop through all effect sizes
  for (my_row in 1:nrow(effect_sizes)) {

    # Get single effect size and contrast to work with
    es <- as.list(effect_sizes[my_row, ])


    #### Set rope limits
    if (rope_units == "sd") {
      sd <- estimate$es_smd[[my_row, "denominator"]]
      this_rope_upper <- rope_upper * sd
      this_rope_lower <- rope_lower * sd
    } else {
      this_rope_upper <- rope_upper
      this_rope_lower <- rope_lower
    }

    test_plot <- list(
      outcome_variable_name = es$outcome_variable_name,
      effect = es$effect,
      effect_size = es$effect_size,
      effect_size_name = effect_size,
      LL = es$LL,
      UL = es$UL,
      ta_LL = es$ta_LL,
      ta_UL = es$ta_UL,
      rope_lower = rope_lower,
      rope_upper = rope_upper
    )


    # Nil hypothesis test ------------------------------
    if (effect_size == "mean") {
      df <- es$df
      t <- es$effect_size / es$SE
      if (is.na(df))
        p <- 2*pnorm(-abs(t))
      else
        p <- 2*pt(-abs(t), df=df)

      significant <- p < alpha
    } else {
      df <- NA
      t <- NA
      p <- NA
      significant <- (0 < es$LL | 0 > es$UL)
    }

    null_hypothesis <- glue::glue("{parameter} = 0")
    null_words <- glue::glue("{parameter} is exactly 0")

    CI <- glue::glue("{confidence}% CI [{format(es$LL)}, {format(es$UL)}])")

    CI_compare <- if (significant)
      "Null is not in the CI"
    else
      "Null is in the CI"

    p_result <- if (significant)
      glue::glue("{p_symbol} < {alpha}")
    else
      glue::glue("{p_symbol} \U002265 {alpha}")

    conclusion <- if (significant)
      glue::glue("{parameter} is not exactly 0")
    else
      glue::glue("0 is a compatible value for {parameter}")


    test_plot$note <- if (significant)
      glue::glue("At \U03B1 = {alpha}, conclude there *is* an effect (effect is not precisely 0)")
    else
      glue::glue("At \U03B1 = {alpha}, not enough data to determine the sign of the effect.")


    nil_result <- list(
      test_type = "Nil Hypothesis Test",
      outcome_variable_name = es$outcome_variable_name,
      effect = es$effect,
      null_hypothesis = null_hypothesis,
      null_words = null_words,
      CI = CI,
      CI_compare = CI_compare,
      t = t,
      df = df,
      p = p,
      p_result = p_result,
      conclusion = conclusion,
      significant = significant
    )


    res$hypothesis_evaluations <- rbind(
      res$hypothesis_evaluations,
      as.data.frame(nil_result)
    )

    if (!(rope_lower == 0 & rope_upper == 0)) {
      # Maximal effect test
      df <- NA
      t <- NA
      p <- NA

      significant <- (es$LL >= rope_upper| es$UL <= rope_lower)
      me_significant <- significant

      null_hypothesis <- glue::glue("{rope_lower} < {parameter} < {rope_upper}")
      null_words <- glue::glue("{parameter} is negligible, between {rope_lower} and {rope_upper}")

      CI <- glue::glue("{confidence}% CI [{format(es$LL)}, {format(es$UL)}]")

      CI_compare <- if (significant)
        "No overlap between null range and CI"
      else
        "At least some overlap between null range and CI"

      p_result <- if (significant)
        glue::glue("{p_symbol} < {alpha}")
      else
        glue::glue("{p_symbol} \U002265 {alpha}")

      conclusion <- if (significant)
        glue::glue("{parameter} is not negligible")
      else
        glue::glue("At least some negligible values are compatible with {parameter}")

      me_result <- list(
        test_type = "Maximal effect test",
        outcome_variable_name = es$outcome_variable_name,
        effect = es$effect,
        null_hypothesis = null_hypothesis,
        null_words = null_words,
        CI = CI,
        CI_compare = CI_compare,
        t = t,
        df = df,
        p = p,
        p_result = p_result,
        conclusion = conclusion,
        significant = significant
      )

      res$hypothesis_evaluations <- rbind(
        res$hypothesis_evaluations,
        as.data.frame(me_result)
      )

      # Equiv effect test
      df <- NA
      t <- NA
      p <- NA

      significant <- (es$ta_LL > rope_lower & es$ta_UL < rope_upper)
      eq_significant <- significant

      null_hypothesis <- glue::glue("{parameter} < {rope_lower} or {parameter} > {rope_upper}")
      null_words <- glue::glue("{parameter} is substantive, outside the range of {rope_lower} to {rope_upper}")

      CI <- glue::glue("{confidence_2alpha}% CI [{format(es$ta_LL)}, {format(es$ta_UL)}]")

      CI_compare <- if (significant)
        "No overlap between null range and CI"
      else
        "At least some overlap between null range and CI"

      p_result <- if (significant)
        glue::glue("{p_symbol} < {alpha}")
      else
        glue::glue("{p_symbol} \U002265 {alpha}")

      conclusion <- if (significant)
        glue::glue("{parameter} is negligible")
      else
        glue::glue("At least some substantive values are compatible with {parameter}")


      eq_result <- list(
        test_type = "Equivalence test",
        outcome_variable_name = es$outcome_variable_name,
        effect = es$effect,
        null_hypothesis = null_hypothesis,
        null_words = null_words,
        CI = CI,
        CI_compare = CI_compare,
        t = t,
        df = df,
        p = p,
        p_result = p_result,
        conclusion = conclusion,
        significant = significant
      )

      res$hypothesis_evaluations <- rbind(
        res$hypothesis_evaluations,
        as.data.frame(eq_result)
      )

      # Finalize overall interpretation
      if (me_significant) {
        test_plot$note <- glue::glue(
          "At \U03B1 = {alpha}, this is a substantive (non-negligible) effect."
        )
      }

      if (eq_significant) {
        test_plot$note <- glue::glue(
          "At \U03B1 = {alpha}, this is negligible effect."
        )
      }

      if (!me_significant & !eq_significant) {
        test_plot$note <- glue::glue(
          "At \U03B1 = {alpha}, there is not enough data to discern if this is a neglgible or substantive effect."
        )
      }

    }  # Finish with interval null tests


    # Add row that will plot test results
    res$test_plot <- rbind(
      res$test_plot,
      as.data.frame(test_plot)
    )

  } # Continue looping through effects


  return(res)

}


#' @export
plot_htest <- function(
    test_result,
    ggtheme = NULL
) {

  if(is.null(ggtheme)) { ggtheme <- ggplot2::theme_classic()}


  test_result$test_plot$effect <- factor(
    test_result$test_plot$effect,
    levels = test_result$test_plot$effect
  )

  if (test_result$properties$rope_units == "sd" & nrow(test_result$test_plot) > 1) {
    # If sd units and multiple variables, we need a different panel for each
    myplot <- ggplot2::ggplot(
      data = test_result$test_plot,
      ggplot2::aes(
        y = 1,
        x = effect_size
      )
    )

    myplot <- myplot + ggplot2::theme(
          axis.text.x = ggplot2::element_blank(), #remove x axis labels
          axis.ticks.x = ggplot2::element_blank(), #remove x axis ticks
          axis.text.y = ggplot2::element_blank(),  #remove y axis labels
          axis.ticks.y = ggplot2::element_blank()  #remove y axis ticks
    )

    myplot <- myplot + ggplot2::facet_grid(
      rows = ggplot2::vars(outcome_variable_name)
    )

  } else {
    # For raw units and/or 1 variable, no need for faceting
    myplot <- ggplot2::ggplot(
      data = test_result$test_plot,
      ggplot2::aes(
        x = effect_size,
        y = outcome_variable_name
      )
    )

  }

  # Apply theme
  myplot <- myplot + ggtheme

  # Apply 2-alpha error bar
  myplot <- myplot + ggplot2::geom_errorbar(
    ggplot2::aes(
      xmin = ta_LL,
      xmax = ta_UL
    ),
    width = 0,
    size = 3
  )

  # Apply 1-alpha error bar
  myplot <- myplot + ggplot2::geom_errorbar(
    ggplot2::aes(
      xmin = LL,
      xmax = UL
    ),
    width = 0,
    size = 1
  )

  # Mark the effect_size
  myplot <- myplot + ggplot2::geom_point(
    shape = "triangle filled",
    size = 4,
    fill = "white"
  )

  # Mark 0 effect
  myplot <- myplot + ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dotted"
  )

  # If testing an interval null, mark the interval
  if (test_result$properties$interval_null) {
      myplot <- myplot + ggplot2::geom_rect(
        ggplot2::aes(
          xmin = rope_lower,
          xmax = rope_upper,
          ymin = -Inf,
          ymax = Inf
        ),
        alpha = 0.07,
        fill = 'black'
      )
  }

  myplot <- myplot + ggplot2::ylab(NULL)

  if (test_result$properties$effect_size_name == "mean") {
    ename <- "Mean difference"
  } else {
    ename <- "Median difference"
  }

  confidence <- 1 - test_result$properties$alpha
  confidence_ta <- 1 - 2*test_result$properties$alpha
  xlab <- glue::glue(
    "{ename} with {confidence*100}% (thin bar) and {confidence_ta*100}% (thick bar) CIs."
  )

  if (test_result$properties$interval_null) {
    xlab <- paste(
      xlab,
      "\n",
      "The gray shaded area represents the null interval."
    )
  }

  myplot <- myplot + ggplot2::xlab(
    xlab
  )

  return(myplot)

}
