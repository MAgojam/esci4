test_mdiff_2x2_between <- function() {
  # Esci in excel  - independent groups from summary data example ----------

  # Setup
  means <- c(1.5, 1.14, 1.38, 2.22)
  sds <- c(1.38, .96,1.5, 1.68)
  ns <- c(26, 26, 25, 26)
  grouping_variable_A_levels <- c("Evening", "Morning")
  grouping_variable_B_levels <- c("Sleep", "No Sleep")

  # Check - match esci with 95% CI
  estimate <- estimate_mdiff_2x2_between(
    means = means,
    sds = sds,
    ns = ns,
    grouping_variable_A_levels = grouping_variable_A_levels,
    grouping_variable_B_levels = grouping_variable_B_levels,
    grouping_variable_A_name = "Testing Time",
    grouping_variable_B_name = "Rest",
    outcome_variable_name = "False Memory Score",
    assume_equal_variance = TRUE
  )
  estimate$main_effect_A
  estimate$interaction
}
