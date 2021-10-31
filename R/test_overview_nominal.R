test_overview_nominal <- function() {

  # Esci one proportion - 8/22
  overview_nominal(counts = c(8, 22-8))
  # Should yield:
  #outcome_variable_name outcome_variable_level count  n         P      P_LL      P_UL       P_SE
  #1   My Outcome Variable                 Level1     8 22 0.3636364 0.1976126 0.5716182 0.09541133


  overview_nominal(
    counts = c(8, 22-8),
    outcome_variable_levels = c("Depressed", "Not-Depressed"),
    outcome_variable_name = "Depression Status",
    conf_level = 0.99
  )

  dep_status <- as.factor(
    c(
      rep("Depressed", 8),
      rep("NotDepressed", 22-8)
    )
  )

  overview_nominal(
    outcome_variable = dep_status
  )



}
