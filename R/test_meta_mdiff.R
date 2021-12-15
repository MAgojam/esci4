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
    testd,
    cm,
    csd,
    cn,
    rm,
    rsd,
    rn,
    study_name,
    mod
  )



  esci_test <- data.frame(
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
    assume_equal_variance = TRUE,
    random_effects = FALSE
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
    assume_equal_variance = TRUE,
    random_effects = FALSE
  )

}

