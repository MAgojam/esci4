#' @export
test_mdiff <- function(
    estimate,
    effect_size = c("mean", "median"),
    rope_lower = -0.1,
    rope_upper = 0.1,
    rope_units = c("raw", "sd"),
    alpha = 0.05
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
  esci_assert_range(
    var = alpha,
    lower = 0,
    upper = 1,
    lower_inclusive = FALSE,
    upper_inclusive = FALSE
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
  res <- list()

  res$properties <- list(
    rope_lower = rope_lower,
    rope_upper = rope_upper,
    rope_units = rope_units,
    alpha = alpha
  )

  etable <- if(effect_size == "mean") "es_mean_difference" else "es_median_difference"

  esymbol <- if(effect_size == "mean") "M" else "Mdn"

  # Reduce down to differences
  effect_sizes <- estimate[[etable]][
    estimate[[etable]]$type == "Difference",
  ]


  # Loop through all effect sizes
  for (my_row in 1:nrow(effect_sizes)) {

    # Get single effect size and contrast to work with
    es <- as.list(effect_sizes[my_row, ])

    if (is.null(es$outcome_variable_name)) {
      outcome_variable_name <- estimate$properties$outcome_variable_name
    } else {
      outcome_variable_name <- es$outcome_variable_name
    }

    #### Set rope limits
    if (rope_units == "sd") {
      sd <- estimate$es_smd[[my_row, "denominator"]]
      this_rope_upper <- rope_upper * sd
      this_rope_lower <- rope_lower * sd
    } else {
      this_rope_upper <- rope_upper
      this_rope_lower <- rope_lower
    }

    # Nil hypothesis test ------------------------------
    df <- es$df
    t_nil <- es$effect_size / es$SE
    if (is.na(df))
      p_nil <- 2*pnorm(-abs(t_nil))
    else
      p_nil <- 2*pt(-abs(t_nil), df=df)

    if (p_nil < alpha) {
      if (es$effect_size > 0) {
        my_operator <- ">"
      } else {
        my_operator <- "<"
      }
      conclusion_nil <- glue::glue(
        "{esymbol}_diff is {my_operator} 0"
      )
    } else {
      conclusion_nil <- glue::glue(
        "Sign of {esymbol}_diff is ambiguous"
      )
    }

    nil_result <- list(
      outcome_variable_name = outcome_variable_name,
      effect_size_label = es$effect,
      null_hypothesis = glue::glue("{esymbol}_diff is exactly 0"),
      t = t_nil,
      df = df,
      p = p_nil,
      alpha = alpha,
      conclusion = conclusion_nil
    )

    res$hypothesis_evaluations <- rbind(
      res$hypothesis_evaluations,
      as.data.frame(nil_result)
    )

    if (!(rope_lower == 0 & rope_upper == 0)) {
      t_upper <- (es$effect_size - this_rope_upper) / es$SE
      t_lower <- (es$effect_size - this_rope_lower) / es$SE
      if (is.na(df)) {
        eq_p_upper <- pnorm(t_upper, lower.tail=TRUE)
        eq_p_lower <- pnorm(t_lower, lower.tail=FALSE)
      } else {
        eq_p_upper <- pt(t_upper, df, lower.tail=TRUE)
        eq_p_lower <- pt(t_lower, df, lower.tail=FALSE)

      }

      if (eq_p_upper > eq_p_lower) {
        t_report <- t_upper
      } else {
        t_report <- t_lower
      }

      if (eq_p_upper < alpha & eq_p_lower < alpha) {
        conclusion_eq <- glue::glue("All compatible values of {esymbol}_diff are negligble")
      } else {
        conclusion_eq <- glue::glue("Cannot rule out substantive values of {esymbol}_diff")
      }

      eq_result <- list(
        outcome_variable_name = outcome_variable_name,
        effect_size_label = es$effect,
        null_hypothesis = glue::glue("
{esymbol}_diff is substantive,
outside of range {round(this_rope_lower, 2)} to {round(this_rope_upper, 2)}"
        ),
        t = t_report,
        df = df,
        p = max(eq_p_upper, eq_p_lower),
        alpha = alpha,
        conclusion = conclusion_eq
      )

      if (is.na(df)) {
        me_p_upper <- pnorm(t_upper, lower.tail=FALSE)
        me_p_lower <- pnorm(t_lower, lower.tail=TRUE)
      } else {
        me_p_upper <- pt(t_upper, df, lower.tail=FALSE)
        me_p_lower <- pt(t_lower, df, lower.tail=TRUE)
      }

      if (eq_p_upper < eq_p_lower) {
        t_report <- t_upper
      } else {
        t_report <- t_lower
      }


      if (me_p_upper < alpha/2 | me_p_lower < alpha/2) {
        conclusion_me <- glue::glue("All compatible values of {esymbol}_diff are substantive")
      } else {
        conclusion_me <- glue::glue("Cannot rule out neglible values of {esymbol}_diff")
      }

      me_result <- list(
        outcome_variable_name = outcome_variable_name,
        effect_size_label = es$effect,
        null_hypothesis = glue::glue(
          "{esymbol}_diff is neglible,
inside the range {round(this_rope_lower, 2)} to {round(this_rope_upper, 2)}"),
        t = t_report,
        df = df,
        p = min(me_p_upper, me_p_lower),
        alpha = alpha/2,
        conclusion = conclusion_me
      )

      res$hypothesis_evaluations <- rbind(
        res$hypothesis_evaluations,
        as.data.frame(eq_result),
        as.data.frame(me_result)
      )
    } # end of equivalence and minimal effect tests


  } # end of looping through effect sizes

  class(res) <- "esci_test"
  return(res)


}
