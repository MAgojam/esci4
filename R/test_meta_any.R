test_estimate_meta_any <- function() {

  my_meta <- data.frame(
    labels = paste("Study", seq(from = 1, to = 30, by = 1), sep = ""),
    effect_size = c(
      rnorm(n = 10, mean = 100, sd = 15),
      rnorm(n = 10, mean = 115, sd = 15),
      rnorm(n = 10, mean = 130, sd = 15)
    ),
    variance = (rnorm(n = 30, mean = 9, sd = 15*sqrt(2/24)))^2,
    mymod = as.factor(
      c(
        rep(x = "Normal", times = 10),
        rep(x = "Online", times = 10),
        rep(x = "Biased", times = 10)
      )
    )
  )


  estimate <- meta_any(my_meta, effect_size, variance)

  meta_any(my_meta, "effect_size", "variance")

  meta_any(my_meta, effect_size, variance, labels, mymod)

  meta_any(
    data = my_meta,
    yi = effect_size,
    vi = variance,
    labels = labels,
    moderator = mymod,
    contrast = c(1/2, -1, 1/2),
    effect_label = "Mean IQ",
    moderator_variable_name = "Participant Pool",
  )


  testd <- data.frame(
    cm= c(rnorm(n = 15, mean = 10, sd = 1), rnorm(n = 15, mean = 10, sd = 1)),
    rm = c(rnorm(n = 15, mean = 11, sd = 1), rnorm(n = 15, mean = 10, sd = 1)),
    csd = abs(rnorm(n = 30, mean = 1, sd = 0.25)),
    rsd = abs(rnorm(n = 30, mean = 1, sd = 0.25)),
    cn = abs(round(rnorm(n=30, mean = 25, sd = 5))),
    rn = abs(round(rnorm(n=30, mean = 25, sd = 5))),
    study_name = paste("Group", seq(1:30), sep = "_"),
    mod = as.factor(sample(x = c("Group A", "Group B", "Group C"), size = 30, replace = TRUE))
  )


  meta_mdiff_two(
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


}

