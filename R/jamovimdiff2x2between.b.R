
# This file is a generated template, your changes will not be overwritten

jamovimdiff2x2betweenClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovimdiff2x2betweenClass",
    inherit = jamovimdiff2x2betweenBase,
    private = list(
        .init = function() {
          # Set some variables for convenience -----------------------
          #   Is analysis from summary data or raw?
          #   Are we evaluating a hypothesis?
          #   Is this a contrast?
          from_raw <- (self$options$switch == "from_raw")

          # Get a handle for each table
          tbl_overview <- NULL
          tbl_es_mean_difference <- NULL
          tbl_es_smd <- NULL
          tbl_es_median_difference <- NULL
          tbl_hypothesis_evaluations <- NULL
          # tbl_htest_summary <- NULL
          assume_equal_variance <- NULL
          try(tbl_overview <- self$results$overview)
          try(tbl_es_mean_difference <- self$results$es_mean_difference)
          try(tbl_es_smd <- self$results$es_smd)
          try(tbl_es_median_difference <- self$results$es_median_difference)
          try(tbl_es_median_ratio <- self$results$es_median_ratio)
          try(assume_equal_variance <- self$options$assume_equal_variance)
          try(tbl_point_null <- self$results$point_null)
          try(tbl_interval_null <- self$results$interval_null)
          try(effect_size <- self$options$effect_size)

          if (effect_size == "mean_difference") {

            if (!is.null(tbl_overview) & !is.null(assume_equal_variance)) {
              if (assume_equal_variance) {
                tbl_overview$setNote(
                  key = "overview_table",
                  note = "Variances are assumed equal, so <i>s</i><sub>p</sub> was used to calculate each CI.",
                  init = TRUE
                )
              } else {
                tbl_overview$setNote(
                  key = "overview_table",
                  note = "Variances are not assumed equal, and so the CI was calculated separately for each mean.",
                  init = TRUE
                )

              }
            }
          } else {
            tbl_overview$setNote(
              key = "overview_table",
              note = NULL,
              init = TRUE
            )
          }

          if (!is.null(tbl_es_mean_difference) & !is.null(assume_equal_variance)) {
            if (assume_equal_variance) {
              tbl_es_mean_difference$setNote(
                key = "overview_table",
                note = "Variances are assumed equal, so <i>s</i><sub>p</sub> was used to calculate each CI.",
                init = FALSE
              )
            } else {
              tbl_es_mean_difference$setNote(
                key = "overview_table",
                note = "Variances are not assumed equal, so the Welch method was used to calculate each CI on a difference.",
                init = FALSE
              )

            }
          }

          # Prep output -------------------------------------------
          # Set CI and MoE columns to reflect confidence level
          conf_level <- jamovi_sanitize(
            my_value = self$options$conf_level,
            return_value = 95,
            na_ok = FALSE,
            convert_to_number = TRUE,
            lower = 75,
            lower_inclusive = FALSE,
            upper = 100,
            upper_inclusive = FALSE,
            my_value_name = "Confidence level"
          )

          jamovi_set_confidence(tbl_overview, conf_level)
          jamovi_set_confidence(tbl_es_mean_difference, conf_level)
          jamovi_set_confidence(tbl_es_smd, conf_level)
          jamovi_set_confidence(tbl_es_median_difference, conf_level)

          jamovi_init_table(tbl_overview, 4)
          jamovi_init_table(tbl_es_mean_difference, 15, breaks = 3)
          jamovi_init_table(tbl_es_smd, 5)
          jamovi_init_table(tbl_es_median_difference, 15, breaks = 3)
          jamovi_init_table(tbl_point_null, 1)
          jamovi_init_table(tbl_interval_null, 1)

          width <- jamovi_sanitize(
            my_value = self$options$es_plot_width,
            return_value = 600,
            convert_to_number = TRUE,
            lower = 10,
            lower_inclusive = TRUE,
            upper = 3000,
            upper_inclusive = TRUE
          )
          height <- jamovi_sanitize(
            my_value = self$options$es_plot_height,
            return_value = 400,
            convert_to_number = TRUE,
            lower = 10,
            lower_inclusive = TRUE,
            upper = 4000,
            upper_inclusive = TRUE
          )

          image <- self$results$main_effect_A
          image$setState("Main Effect of A")
          image$setSize(width, height)
          image <- self$results$main_effect_B
          image$setState("Main Effect of B")
          image$setSize(width, height)


        },
        .run = function() {

            from_raw <- (self$options$switch == "from_raw")

            estimate <- jamovi_mdiff_2x2between(
              self = self,
              save_raw_data = FALSE
            )

            # Print any notes that emerged from running the analysis
            jamovi_set_notes(self$results$help)

            # Check to see if the analysis ran
            #  If null, return
            #  If error, return the error
            if(is.null(estimate)) return(TRUE)
            if(is(estimate, "try-error")) stop(estimate[1])

            # Add in MoE
            estimate$es_mean_difference$moe <- (estimate$es_mean_difference$UL - estimate$es_mean_difference$LL)/2
            estimate$overview$moe <- (estimate$overview$mean_UL - estimate$overview$mean_LL)/2

            # Fill tables
            jamovi_estimate_filler(self, estimate, TRUE)

        },
        .estimation_plot = function(image, ggtheme, theme, ...) {

          # Redo analysis
          estimate <- jamovi_mdiff_2x2between(
            self = self,
            save_raw_data = TRUE
          )

          if (is.null(estimate)) return(TRUE)

          # if(!is(estimate, "esci_estimate"))
          #   return(TRUE)
          #

          which_plot <- switch(
            image$state,
            "Main Effect of A" = "main_effect_A",
            "Main Effect of B" = "main_effect_B"
          )

          which_title <- switch(
            image$state,
            "Main Effect of A" = "grouping_variable_A",
            "Main Effect of B" = "grouping_variable_B"
          )

          image$setTitle(
            paste(
              "Main Effect of",
              self$options[[which_title]]
            )
          )

          myplot <- jamovi_plot_mdiff(
            self,
            estimate[[which_plot]],
            image,
            ggtheme,
            theme
          )

          print(myplot)
          TRUE

        })
)



jamovi_mdiff_2x2between <- function(
    self,
    save_raw_data = FALSE
) {


  # Prelim -----------------------------------------------------
  from_raw <- (self$options$switch == "from_raw")
  notes <- c(NULL)


  # Step 1 - Check if analysis basics are defined ---------------
  #  if not, return NULL
  if(from_raw) {
    if (
      is.null(self$options$grouping_variable_A) |
      is.null(self$options$grouping_variable_B) |
      is.null(self$options$outcome_variable)
    ) return(NULL)
  } else {
  }



  # Step 3: Run analysis ------------------------------------------
  # Fill in analysis properties

  # If from summary:
  # get outcome and grouping variable names
  # and set notes if they have been replaced
  if(!from_raw) {

  }

  call <- esci4::estimate_mdiff_2x2_between
  args <- list()
  conf_level <- jamovi_sanitize(
    my_value = self$options$conf_level,
    return_value = 95,
    na_ok = FALSE,
    convert_to_number = TRUE,
    lower = 75,
    lower_inclusive = FALSE,
    upper = 100,
    upper_inclusive = FALSE,
    my_value_name = "Confidence level"
  )
  notes <- c(notes, names(conf_level))
  args$conf_level <- conf_level/100
  args$assume_equal_variance <- self$options$assume_equal_variance

  # Set args for summary and raw data cases
  if (from_raw) {
    # Analysis from raw data
    args$data <- self$data
    args$grouping_variable_A <- self$options$grouping_variable_A
    args$grouping_variable_B <- self$options$grouping_variable_B
    args$outcome_variable <- self$options$outcome_variable

  } else {

  }

  # self$results$debug$setContent(args)
  # self$results$debug$setVisible(TRUE)


  # Do analysis, then post any notes that have emerged
  estimate <- try(do.call(what = call, args = args))

  # For summary data, store in a list based on outcome_variable_name
  if (!is(estimate, "try-error")) {
    estimate <- jamovi_add_htest_mdiff(
      self = self,
      estimate = estimate
    )

    notes <- c(notes, estimate$warnings)
    self$results$help$setState(notes)

  }



  return(estimate)

}
