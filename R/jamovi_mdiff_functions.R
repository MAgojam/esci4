jamovi_mdiff_initialize <- function(self, grouping_variable = TRUE) {

  # Set some variables for convenience -----------------------
  #   Is analysis from summary data or raw?
  #   Are we evaluating a hypothesis?
  #   Is this a contrast?
  from_raw <- (self$options$switch == "from_raw")

  # Get a handle for each table
  tbl_overview <- NULL
  tbl_es_mean_difference <- NULL
  tbl_es_mean_ratio <- NULL
  tbl_es_smd <- NULL
  tbl_es_median_difference <- NULL
  tbl_es_median_ratio <- NULL
  tbl_es_odds_ratio <- NULL
  try(tbl_overview <- self$results$overview)
  try(tbl_es_mean_difference <- self$results$es_mean_difference)
  try(tbl_es_mean_ratio <- self$results$es_mean_ratio)
  try(tbl_es_smd <- self$results$es_smd)
  try(tbl_es_median_difference <- self$results$es_median_difference)
  try(tbl_es_median_ratio <- self$results$es_median_ratio)
  try(tbl_es_odds_ratio <- self$results$es_odds_ratio)


  # Prep output -------------------------------------------
  # Set CI and MoE columns to reflect confidence level
  conf_level <- jamovi_sanitize(
    my_value = self$options$conf_level,
    return_value = 95,
    na_ok = FALSE,
    convert_to_number = TRUE
  )

  jamovi_set_confidence(tbl_overview, conf_level)
  jamovi_set_confidence(tbl_es_mean_difference, conf_level)
  jamovi_set_confidence(tbl_es_smd, conf_level)
  jamovi_set_confidence(tbl_es_mean_ratio, conf_level)
  jamovi_set_confidence(tbl_es_median_difference, conf_level)
  jamovi_set_confidence(tbl_es_median_ratio, conf_level)


  # Outcomes: 1 if from summary, length of outcome_variables if raw
  outcome_count <- if(from_raw) {
    length(self$options$outcome_variable)
  } else {
    1
  }

  # For now, only 1 contrast can be specified
  contrast_count <- 1

  # How many levels?
  #  For raw, check grouping_variable
  #  For summary, check group_labels
  if (grouping_variable) {
    if (from_raw) {
      level_source <- self$options$grouping_variable
      level_count <- length(levels(as.factor(self$data[, level_source])))
    } else {
      level_source <- try(level_source <- self$options$grouping_variable_levels)
      if (is(level_source, "try-error")) {
        level_count = 2
      } else {
        level_count <- length(levels(as.factor(self$data[, level_source])))
      }
    }
  } else {
    level_count <- 1
  }

  # Rows needed for each table -------------------------------
  overview_rows <- level_count * outcome_count
  mdiff_rows <- contrast_count * outcome_count * 3
  smd_rows <- contrast_count * outcome_count

  jamovi_init_table(tbl_overview, overview_rows)
  jamovi_init_table(tbl_es_mean_difference, mdiff_rows, breaks = 3)
  jamovi_init_table(tbl_es_smd, smd_rows)
  jamovi_init_table(tbl_es_mean_ratio, smd_rows)
  jamovi_init_table(tbl_es_median_difference, mdiff_rows, breaks = 3)
  jamovi_init_table(tbl_es_median_ratio, smd_rows, breaks = 3)


}




