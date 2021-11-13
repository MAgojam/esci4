test_pdiff_paired <- function() {
  # From summary data -------------------
  estimate_pdiff_paired(
    cases_consistent = 60,
    cases_inconsistent = 50,
    not_cases_consistent = 68,
    not_cases_inconsistent = 22,
    conf_level = 0.95
  )

  estimate_pdiff_paired(
    cases_consistent = 60,
    cases_inconsistent = 50,
    not_cases_consistent = 68,
    not_cases_inconsistent = 22,
    case_label = "Answered True",
    not_case_label = "Answered False",
    grouping_variable_levels = c("9th grade", "12th grade"),
    outcome_variable_name = "Test Response",
    grouping_variable_name = "Grade level",
    conf_level = 0.95
  )


  # From raw data, data frame -------------------
  pre_test <- as.factor(
    sample(
      c("Depressed", "Not Depressed"),
      size = 300,
      replace = TRUE,
      prob = c(0.75, 0.25)
    )
  )
  post_test <- as.factor(
    sample(
      c("Depressed", "Not Depressed"),
      size = 300,
      replace = TRUE,
      prob = c(0.25, 0.75)
    )
  )

  d_treat <- data.frame(
    "before" = pre_test,
    "after" = post_test
  )

  estimate_pdiff_paired(
    data = d_treat,
    reference_measure = "before",
    comparison_measure = "after"
  )


  # More than 2 levels -------------------
  pre_test <- as.factor(
    sample(
      c("Depressed", "Not Depressed", "No Answer"),
      size = 300,
      replace = TRUE,
      prob = c(0.65, 0.25, 0.10)
    )
  )
  post_test <- as.factor(
    sample(
      c("Depressed", "Not Depressed", "No Answer"),
      size = 300,
      replace = TRUE,
      prob = c(0.25, 0.65, 0.10)
    )
  )

  d_treat <- data.frame(
    "before" = pre_test,
    "after" = post_test
  )

  estimate_pdiff_paired(
    data = d_treat,
    reference_measure = "before",
    comparison_measure = "after",
    case_label = "No Answer"
  )


  estimate_pdiff_paired(
    data = d_treat,
    reference_measure = "before",
    comparison_measure = "after",
    case_label = 3
  )


  # With NA values --------------------------------
  pre_test <- as.factor(
    sample(
      c("Depressed", "Not Depressed", "No Answer", NA),
      size = 300,
      replace = TRUE,
      prob = c(0.65, 0.25, 0.05, 0.05)
    )
  )
  post_test <- as.factor(
    sample(
      c("Depressed", "Not Depressed", "No Answer", NA),
      size = 300,
      replace = TRUE,
      prob = c(0.25, 0.65, 0.05, 0.05)
    )
  )

  d_treat <- data.frame(
    "before" = pre_test,
    "after" = post_test
  )

  estimate_pdiff_paired(
    data = d_treat,
    reference_measure = "before",
    comparison_measure = "after",
    case_label = "No Answer"
  )

}
