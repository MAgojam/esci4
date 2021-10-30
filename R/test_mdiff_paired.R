test_mdiff_paired <- function() {

  estimate_mdiff_paired(
    comparison_mean = 12,
    comparison_sd = 2,
    reference_mean = 10,
    reference_sd = 3,
    n = 20,
    correlation = 0.70
  )


  bk_wrapper <- c(
    4	,
    4	,
    3	,
    2	,
    2	,
    5	,
    1	,
    1	,
    3	,
    1	,
    1	,
    2	,
    4	,
    3	,
    1	,
    1	,
    1	,
    3	,
    1	,
    1	,
    1	,
    5	,
    1	,
    4	,
    1	,
    3	,
    2	,
    4	,
    2	,
    1
  )

  wc_wrapper <- c(
    2	,
    3	,
    2	,
    1	,
    1	,
    2	,
    1	,
    1	,
    3	,
    2	,
    1	,
    1	,
    2	,
    4	,
    1	,
    1	,
    4	,
    2	,
    2	,
    1	,
    2	,
    2	,
    1	,
    3	,
    2	,
    2	,
    1	,
    1	,
    2	,
    2

  )

  wrapper <- data.frame(
    "wc_wrapper" = wc_wrapper,
    "bk_wrapper" = bk_wrapper
  )

  estimate_mdiff_paired(
    comparison_measure = wc_wrapper,
    reference_measure = bk_wrapper
  )

  estimate_mdiff_paired(
    data = wrapper,
    comparison_measure = "wc_wrapper",
    reference_measure = "bk_wrapper"
  )

}
