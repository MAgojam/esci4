
# This file is a generated template, your changes will not be overwritten

jamovimdiffoneClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovimdiffoneClass",
    inherit = jamovimdiffoneBase,
    private = list(
        .init = function() {

          from_raw <- (self$options$switch == "from_raw")

          # Get a handle for each table
          tbl_overview <- self$results$overview
          tbl_es_mean_difference <- self$results$es_mean_difference
          tbl_es_smd <- self$results$es_smd
          tbl_es_median_difference <- self$results$es_median_difference
          tbl_eval <- self$results$htest
          tbl_htest_summary <- self$results$htest_summary

          # Prep output -------------------------------------------
          # Set CI and MoE columns to reflect confidence level
          conf_level <- jamovi_sanitize(
            my_value = self$options$conf_level,
            return_value = 95,
            na_ok = FALSE,
            convert_to_number = TRUE
          )

          jamovi_set_confidence(tbl_overview, conf_level)
          jamovi_set_confidence(tbl_es_mean_difference, conf_level)
          jamovi_set_confidence(tbl_es_smd, conf_level)
          jamovi_set_confidence(tbl_es_median_difference, conf_level)

          # Outcomes: 1 if from summary, length of outcome_variables if raw
          outcome_count <- if(from_raw) {
            length(self$options$outcome_variable)
          } else {
            1
          }

          # For now, only 1 contrast can be specified
          contrast_count <- 1

          level_count <- 1

          eval_base <- if(self$options$null_boundary != 0) {
            3
          } else {
            1
          }


          # Rows needed for each table -------------------------------
          overview_rows <- level_count * outcome_count
          mdiff_rows <- contrast_count * outcome_count * 3
          smd_rows <- contrast_count * outcome_count
          eval_rows <- eval_base * contrast_count * outcome_count

          jamovi_init_table(tbl_overview, overview_rows)
          jamovi_init_table(tbl_es_mean_difference, mdiff_rows, breaks = 3)
          jamovi_init_table(tbl_es_smd, smd_rows)
          jamovi_init_table(tbl_es_median_difference, mdiff_rows, breaks = 3)
          jamovi_init_table(
            tbl_eval,
            eval_rows,
            breaks = if(eval_base == 1) NULL else eval_base
          )
          jamovi_init_table(tbl_htest_summary, outcome_count)

          #
          reference_mean <- jamovi_required_numeric(
            self$options$reference_mean
          )
          diff_vis <- TRUE
          if (is.null(reference_mean) | !is.numeric(reference_mean) | is.na(reference_mean))  {
            diff_vis  <- FALSE
            # tbl_es_mean_difference$setVisible(FALSE)
            # tbl_es_median_difference$setVisible(FALSE)
            # tbl_es_smd$setVisible(FALSE)
          }
          tbl_es_smd$setVisible(self$options$effect_size == "mean_difference" & diff_vis)
          tbl_es_mean_difference$setVisible(self$options$effect_size == "mean_difference" & diff_vis)
          tbl_es_median_difference$setVisible(self$options$effect_size == "median_difference" & diff_vis)

          width <- jamovi_sanitize(
            my_value = self$options$es_plot_width,
            return_value = 200,
            convert_to_number = TRUE,
            lower = 10,
            lower_inclusive = TRUE,
            upper = 2000,
            upper_inclusive = TRUE
          )
          height <- jamovi_sanitize(
            my_value = self$options$es_plot_height,
            return_value = 550,
            convert_to_number = TRUE,
            lower = 10,
            lower_inclusive = TRUE,
            upper = 4000,
            upper_inclusive = TRUE
          )

          self$results$hplot$setSize(width, height)

          keys <- if (from_raw)
            self$options$outcome_variable
          else
            jamovi_sanitize(
              self$options$outcome_variable_name,
              "My outcome variable",
              na_ok = FALSE
            )

          for (my_key in keys) {
            self$results$estimation_plots$addItem(key = my_key)
            image <- self$results$estimation_plots$get(my_key)
            image$setSize(width , height)
          }

        },
        .run = function() {

          from_raw <- (self$options$switch == "from_raw")
          evaluate_h <- self$options$evaluate_hypotheses
          tbl_eval <- self$results$htest
          tbl_htest_summary <- self$results$htest_summary

          estimate <- jamovi_mdiff_one(
            self,
            outcome_variable = NULL,
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

          # Add calculation details
          alpha <- 1 - self$options$conf_level/100
          estimate$es_mean_difference$t_multiplier <- stats::qt(1-alpha/2, estimate$es_mean_difference$df)
          estimate$es_mean_difference$s_component[c(1, 3)] <- estimate$es_smd$denominator[[1]]
          estimate$es_mean_difference$n_component <- estimate$es_mean_difference$moe / estimate$es_mean_difference$t_multiplier / estimate$es_mean_difference$s_component

          # Fill tables
          jamovi_estimate_filler(self, estimate, TRUE)


          if(evaluate_h) {
            # Test results
            effect_size <- if (self$options$effect_size == "mean_difference")
              "mean"
            else
              "median"

            test_results <- test_mdiff(
                estimate,
                effect_size = effect_size,
                rope_lower = self$options$null_boundary*-1,
                rope_upper = self$options$null_boundary,
                rope_units = self$options$rope_units,
                output_html = TRUE
              )

            self$results$debug$setContent(test_results)

            image <- self$results$hplot
            image$setState(test_results)

            # Fill table
            jamovi_table_filler(
             tbl_eval,
             test_results$hypothesis_evaluations
            )
            jamovi_table_filler(
              tbl_htest_summary,
              test_results$test_plot
            )
          }


          # Deal with plots ----------------------------------------
          # Set up array of estimation plots
          keys <- if (from_raw)
            self$options$outcome_variable
          else
            jamovi_sanitize(
              self$options$outcome_variable_name,
              "My outcome variable",
              na_ok = FALSE
            )

          for (my_key in keys) {
            image <- self$results$estimation_plots$get(key=my_key)
            image$setState(my_key)
          }

          if (length(keys) > 1) {
            self$results$estimation_plots$setTitle(
              paste(
                self$results$estimation_plots$title,
                "s",
                sep = ""
              )
            )
          }


        },
        .estimation_plots = function(image, ggtheme, theme, ...) {

          reference_mean <- jamovi_required_numeric(
            self$options$reference_mean
          )
          if (is.null(reference_mean)) return(TRUE)
          if (!is.numeric(reference_mean)) return(TRUE)
          if (is.na(reference_mean)) return(TRUE)

          if (is.null(image$state))
            return(FALSE)

          # Redo analysis
          estimate <- jamovi_mdiff_one(
            self,
            outcome_variable = c(image$state),
            save_raw_data = TRUE
          )

          if(!is(estimate, "esci_estimate"))
            return(TRUE)

          myplot <- jamovi_plot_mdiff(
            self,
            estimate,
            image,
            ggtheme,
            theme
          )

          print(myplot)
          TRUE

        },
        .plot_hplot=function(image, ggtheme, theme, ...) {  # <-- the plot function
          if (is.null(image$state))
            return(FALSE)

          test_data <- image$state

          plot <- jamovi_plot_hdiff(
            self,
            image,
            ggtheme,
            theme
          )

          print(plot)
          TRUE

        }
  )
)


jamovi_mdiff_one <- function(self, outcome_variable = NULL, save_raw_data = FALSE) {

  # Prelim -----------------------------------------------------
  from_raw <- (self$options$switch == "from_raw")
  notes <- c(NULL)


  # Step 1 - Check if analysis basics are defined ---------------
  args <- list()



  args$reference_mean <- jamovi_required_numeric(
    self$options$reference_mean
  )
  if (!is.numeric(args$reference_mean)) {
    notes <- c(
      "For this analysis, please specify a Reference mean (<i>M</i><sub>Referenece</sub>)"
    )
    self$results$help$setState(notes)
    args$reference_mean <- NULL

  }



  if(from_raw) {

    if (is.null(outcome_variable)) {
      if (
        is.null(self$options$outcome_variable) |
        length(self$options$outcome_variable) == 0
      ) return(NULL)
    }

  } else {
    args$comparison_mean <- jamovi_required_numeric(
      self$options$comparison_mean,
      my_value_name = "Mean (<i>M</i>)"
    )
    args$comparison_sd <- jamovi_required_numeric(
      self$options$comparison_sd,
      lower = 0,
      lower_inclusive = FALSE,
      my_value_name = "Standard deviation (<i>s</i>)"
    )
    args$comparison_n <- jamovi_required_numeric(
      self$options$comparison_n,
      integer_required = TRUE,
      lower = 0,
      lower_inclusive = FALSE,
      my_value_name = "Sample size (<i>N</i>)"
    )

    unfilled <- NULL
    for (element in args[which(is.na(args))]) {
      unfilled <- c(unfilled, names(element))
    }

    for (element in args) {
      if (is.character(element)) {
        notes <- c(notes, element)
      }
    }

    if (length(unfilled) > 0) {
      notes <- c(
        paste(
          "For summary data, please specify: ",
          paste0(unfilled, collapse = ", ")
        ),
        notes
      )
      self$results$help$setState(notes)
      return(NULL)
    }

  }


  # Step 2: Get analysis properties-----------------------------
  call <- esci4::estimate_mdiff_one

  args$save_raw_data <- save_raw_data
  args$conf_level <- jamovi_sanitize(
    my_value = self$options$conf_level,
    return_value = 95,
    na_ok = FALSE,
    convert_to_number = TRUE,
    lower = 0,
    lower_inclusive = FALSE,
    upper = 100,
    upper_inclusive = FALSE,
    my_value_name = "Confidence level"
  )/100


  if(from_raw) {
    args$data <- self$data
    if (is.null(outcome_variable)) {
      args$outcome_variable <- unname(self$options$outcome_variable)
    } else {
      args$outcome_variable <- unname(outcome_variable)
      args$outcome_variable_name <- outcome_variable
    }
    for (x in 1:length(args$outcome_variable)) {
      args$data[[args$outcome_variable[[x]]]] <- as.numeric(args$data[[args$outcome_variable[[x]]]])
    }
  } else {
    args$outcome_variable_name <- jamovi_sanitize(
      self$options$outcome_variable_name,
      return_value = "My outcome variable",
      na_ok = FALSE
    )

    for (element in args) {
      notes <- c(notes, names(element))
    }

  }


  #self$results$debug$setContent(args)

  # Do analysis, then post any notes that have emerged
  estimate <- try(do.call(what = call, args = args))

  if (!is(estimate, "try-error")) {
    if (length(estimate$warnings) > 0) {
      notes <- c(notes, estimate$warnings)
    }
  }

  self$results$help$setState(notes)

  return(estimate)


}
