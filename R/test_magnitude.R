test_magnitude <- function() {

  # Check - pen group from pen/latop in esci in excel, summary two
  estimate_magnitude(mean = 6.88, sd = 4.22, n = 48)
  # Should give 6.88 95% CI [5.65464, 8.10536]

  # Check - pen group with 99% CI
  estimate_magnitude(
    mean = 6.88,
    sd = 4.22,
    n = 48,
    outcome_variable_name = "Transcription %",
    conf_level = 0.99
  )
  # Should give 6.88 99% CI [5.24483, 8.51517]



  pen_transcription <- c(
    12.1	,
    6.5	,
    8.1	,
    7.6	,
    12.2	,
    10.8	,
    1	,
    2.9	,
    14.4	,
    8.4	,
    17.7	,
    20.1	,
    2.1	,
    11.1	,
    11.2	,
    10.7	,
    1.9	,
    5.2	,
    9.7	,
    5.2	,
    2.4	,
    7.1	,
    8.7	,
    8	,
    11.3	,
    8.5	,
    9.1	,
    4.5	,
    9.2	,
    13.3	,
    18.3	,
    2.8	,
    5.1	,
    12.4
  )

  # Check - vector
  estimate_magnitude(
    outcome_variable = pen_transcription,
    conf_level = 0.99
  )
  # Should give 8.81176 99% CI [6.5855, 11.038]

  pen_study <- data.frame(
    "transcription" = pen_transcription,
    "other" = rnorm(n = 34, mean = 100, sd = 15)
  )

  # Check data.frame
  estimate_magnitude(pen_study, transcription)
  # Should give 8.81176 95% CI [7.15464, 10.4689]

  # Check jamovi
  estimate_magnitude(pen_study, c("transcription", "other"))

}
