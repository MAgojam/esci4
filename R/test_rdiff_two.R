test_r <- function() {

  # Summary example from esci, should give:
  #  comparison r 95% CI [.286, .709]
  #  reference r 95% CI [.172, .603]
  #  difference .12 95% CI [-.191, .418]
  estimate_rdiff_two(
    comparison_r = .53,
    comparison_n = 45,
    reference_r = .41,
    reference_n = 59
  )


  estimate_rdiff_two(
    comparison_r = .53,
    comparison_n = 45,
    reference_r = .41,
    reference_n = 59,
    grouping_variable_levels = c("Females", "Males"),
    x_variable_name = "Satisfaction with life",
    y_variable_name = "Body satisfaction",
    grouping_variable_name = "Gender",
    conf_level = .95
  )


  myr2 <- data.frame(
    thex = rnorm(n = 100),
    they = rnorm(n = 100),
    thegroup = as.factor(sample(x = c("Men", "Women"), size = 100, replace = TRUE))
  )

  estimate <- estimate_rdiff_two(
    myr2,
    thex,
    they,
    thegroup
  )

}
