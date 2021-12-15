test_estimate_meta_any <- function() {

  my_meta <- data.frame(
    labels = paste("Study", seq(from = 1, to = 10, by = 1), sep = ""),
    yi = rnorm(n = 10, mean = 100, sd = 15),
    vi = (rnorm(n = 10, mean = 0, sd = 2))^2,
    moderator = as.factor(
      sample(x = c("Level1", "Level2", "Level3"), size = 10, replace = TRUE)
    )
  )


  estimate <- meta_any(
    data = my_meta,
    moderator = TRUE
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


  meta_mdiff(
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

