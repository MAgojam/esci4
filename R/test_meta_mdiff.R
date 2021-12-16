test_estimate_meta_mdiff <- function() {

  testd <- data.frame(
    cm= c(
      rnorm(n = 10, mean = 12, sd = 1),
      rnorm(n = 10, mean = 14, sd = 1),
      rnorm(n = 10, mean = 10, sd = 1)
    ),
    rm = rnorm(n = 30, mean = 10, sd = 1),
    csd = abs(rnorm(n = 30, mean = 1, sd = 0.25)),
    rsd = abs(rnorm(n = 30, mean = 1, sd = 0.25)),
    cn = abs(round(rnorm(n=30, mean = 25, sd = 5))),
    rn = abs(round(rnorm(n=30, mean = 25, sd = 5))),
    study_name = paste("Group", seq(1:30), sep = "_"),
    mod = as.factor(
      c(
        rep(x = "Group A", times = 10),
        rep(x = "Group B", times = 10),
        rep(x = "Group C", times = 10)
      )
    )
  )


  estimate <- meta_mdiff(
    data = testd,
    comparison_means = cm,
    comparison_sds = csd,
    comparison_ns = cn,
    reference_means = rm,
    reference_sds = rsd,
    reference_ns = rn,
    moderator = mod,
    labels = study_name
  )

  estimate <- meta_mdiff(
    data = testd,
    comparison_means = cm,
    comparison_sds = csd,
    comparison_ns = cn,
    reference_means = rm,
    reference_sds = rsd,
    reference_ns = rn,
    moderator = mod,
    contrast = c(1, -1/2, -1/2),
    labels = study_name
  )



  esci_test <- data.frame(
    study_name = c("McCabe 1", "McCabe 2", paste("Michael", seq(1:8), sep = " ")),
    nbM = c(2.89, 2.69, 2.90, 2.62, 2.96, 2.93, 2.86, 2.50, 2.41, 2.54),
    nbS = c(0.79, 0.55, 0.58, 0.54, 0.36, 0.60, 0.59, 0.84, 0.78, 0.66),
    nbN = c(28, 26, 98, 42, 24, 184, 274, 58, 34, 99),
    bM = c(3.12, 3.00, 2.86, 2.85, 3.07, 2.89, 2.91, 2.60, 2.74, 2.72),
    bS = c(0.65, 0.54, 0.61, 0.57, 0.55, 0.60, 0.52, 0.83, 0.51, 0.68),
    bN = c(26, 28, 99, 33, 21, 184, 255, 55, 34, 95),
    mod = as.factor(
      c("Simple", "Critique", "Simple","Simple","Simple","Simple","Simple","Critique","Critique","Critique")
    )
  )

  estimate <- meta_mdiff(
    data = esci_test,
    comparison_means = bM,
    comparison_sds = bS,
    comparison_ns = bN,
    reference_means = nbM,
    reference_sds = nbS,
    reference_ns = nbN,
    labels = study_name,
    effect_label = "Brain Photo Rating - No Brain Photo Rating",
    assume_equal_variance = TRUE,
    random_effects = TRUE
  )

  estimate <- meta_mdiff(
    data = esci_test,
    comparison_means = bM,
    comparison_sds = bS,
    comparison_ns = bN,
    reference_means = nbM,
    reference_sds = nbS,
    reference_ns = nbN,
    moderator = mod,
    labels = study_name,
    effect_label = "Brain Photo Rating - No Brain Photo Rating",
    assume_equal_variance = TRUE,
    random_effects = TRUE
  )

  estimate <- meta_mdiff(
    data = esci_test,
    comparison_means = bM,
    comparison_sds = bS,
    comparison_ns = bN,
    reference_means = nbM,
    reference_sds = nbS,
    reference_ns = nbN,
    moderator = mod,
    labels = study_name,
    effect_label = "Brain Photo Rating - No Brain Photo Rating",
    assume_equal_variance = TRUE,
    random_effects = FALSE,
    conf_level = 0.99
  )

  # Currently matches esci example perfectly *except* diamond ratios for
  #  moderator groups
  # In esci, DR calculated from subgroup CIs from overall analysis
  # In this code, DR calculated from re-running FE and RE on subgroup only
  # And this can lead to some small differences in CI widths and therefore DRs
  # And with moderators, FE weights match, but esc-excel not providing
  #  different RE weights

  estimate <- meta_mdiff(
    data = esci_test,
    comparison_means = bM,
    comparison_sds = bS,
    comparison_ns = bN,
    reference_means = nbM,
    reference_sds = nbS,
    reference_ns = nbN,
    moderator = mod,
    contrast = c(-1, 1),
    labels = study_name,
    effect_label = "Brain Photo Rating - No Brain Photo Rating",
    assume_equal_variance = TRUE,
    random_effects = FALSE,
    conf_level = 0.99
  )

  estimate <- meta_mdiff(
    data = esci_test,
    comparison_means = bM,
    comparison_sds = bS,
    comparison_ns = bN,
    reference_means = nbM,
    reference_sds = nbS,
    reference_ns = nbN,
    moderator = mod,
    labels = study_name,
    effect_label = "Brain Photo Rating - No Brain Photo Rating",
    assume_equal_variance = TRUE,
    random_effects = TRUE,
    report_smd = TRUE
  )

}

