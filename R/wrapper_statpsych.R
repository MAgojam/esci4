wrapper_ci.stdmean1 <- function(
  comparison_mean,
  comparison_sd,
  comparison_n,
  reference_mean,
  effect_label,
  conf_level
) {

  # Result from statpsych -------------------------------
  res <- as.data.frame(
    statpsych::ci.stdmean1(
      alpha = 1 - conf_level,
      m = comparison_mean,
      sd = comparison_sd,
      n = comparison_n,
      h = reference_mean
    )
  )

  # Wrangling ----------------------------------------
  # Change order and names
  res <- res[ , c("Estimate", "LL", "UL", "SE")]
  colnames(res) <- c("effect_size", "LL", "UL", "SE_temp")

  # Add additional properties
  res$numerator <- comparison_mean - reference_mean
  res$denominator <- comparison_sd
  res$SE <- res$SE_temp
  res$SE_temp <- NULL
  res$d_biased <- res$effect_size

  # Add effect label
  res <- cbind(
    "effect" = effect_label,
    res
  )


  # Properties ------------------------------
  properties <- list(
    effect_size_name = "d_1",
    effect_size_name_html = "<i>d</i><sub>1.biased</sub>",
    denominator_name = "s_comparison",
    denominator_name_html = "<i>s</i><sub>comparison</sub>",
    bias_corrected = FALSE
  )

  properties$effect_size_category = "difference"
  properties$effect_size_precision = "magnitude"
  properties$conf_level = conf_level
  properties$error_distribution = "norm"


  properties$message <- glue::glue(
    "
This standardized mean difference is called {properties$effect_size_name} because the standardizer used was {properties$denominator_name}. {properties$effect_size_name} {if (properties$bias_corrected) 'has' else 'has *not*'} been corrected for bias. Correction for bias can be important when df < 50.
    "
  )

  properties$message_html <- glue::glue(
    "
This standardized mean difference is called {properties$effect_size_name_html}
because the standardizer used was {properties$denominator_name_html}.<br>
{properties$effect_size_name_html} {if (properties$bias_corrected) 'has' else 'has *not*'}
been corrected for bias.
Correction for bias can be important when <i>df</i> < 50.<br>
    "
  )


  smd <- list(
    es_smd = res,
    es_smd_properties = properties
  )

  return(smd)

}


wrapper_ci.mean.ps <- function(
  comparison_mean,
  comparison_sd,
  reference_mean,
  reference_sd,
  n,
  correlation,
  grouping_variable_levels,
  outcome_variable_name,
  grouping_variable_name,
  conf_level
) {

  effect_label <- paste(
    grouping_variable_levels[1],
    "-",
    grouping_variable_levels[2],
    sep = " "
  )

  es_mean_difference <- as.data.frame(
    statpsych::ci.mean.ps(
      alpha = 1 - conf_level,
      m1 = comparison_mean,
      m2 = reference_mean,
      sd1 = comparison_sd,
      sd2 = reference_sd,
      cor = correlation,
      n = n
    )
  )

  es_mean_difference <- es_mean_difference[ , c("Estimate", "LL", "UL", "SE", "df")]
  colnames(es_mean_difference) <- c("effect_size", "LL", "UL", "SE", "df")

  es_mean_difference <- cbind(
    "type" = "Difference",
    "outcome_variable_name" = outcome_variable_name,
    "grouping_variable_name" = grouping_variable_name,
    "effect" = effect_label,
    es_mean_difference
  )

  comp_mean <- estimate_mdiff_ind_contrast(
    means = c(comparison_mean, reference_mean),
    sds = c(comparison_sd, reference_sd),
    ns = c(n, n),
    contrast = c(1, -1),
    conf_level = conf_level,
    assume_equal_variance = FALSE,
    grouping_variable_levels = grouping_variable_levels,
    outcome_variable_name = outcome_variable_name,
    grouping_variable_name = grouping_variable_name
  )

  es_mean_difference <- rbind(
    comp_mean$es_mean_difference[1, ],
    comp_mean$es_mean_difference[2, ],
    es_mean_difference
  )


  return(es_mean_difference)

}
