
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


          # Outcomes: 1 if from summary, length of outcome_variables if raw
          outcome_count <- if(from_raw) {
            length(self$options$outcome_variable)
          } else {
            1
          }

          # For now, only 1 contrast can be specified
          contrast_count <- 1

          level_count <- 1

          # Rows needed for each table -------------------------------
          overview_rows <- level_count * outcome_count
          mdiff_rows <- contrast_count * outcome_count * 3
          smd_rows <- contrast_count * outcome_count

          jamovi_init_table(tbl_overview, overview_rows)
          jamovi_init_table(tbl_es_mean_difference, mdiff_rows, breaks = 3)
          jamovi_init_table(tbl_es_smd, smd_rows)


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

          # Fill tables
          jamovi_estimate_filler(self, estimate, TRUE)

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

        },
        .estimation_plots = function(image, ggtheme, theme, ...) {

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

        })
)


jamovi_mdiff_one <- function(self, outcome_variable = NULL, save_raw_data = FALSE) {

  # Prelim -----------------------------------------------------
  from_raw <- (self$options$switch == "from_raw")
  notes <- c(NULL)


  # Step 1 - Check if analysis basics are defined ---------------
  args <- list()


  if(from_raw) {
    args$reference_mean <- jamovi_required_numeric(
      self$options$reference_mean
    )

    if (!is.numeric(args$reference_mean)) {
      notes <- c(
        "For this analysis, please specify a Reference Mean (<i>M</i><sub>Referenece</sub>)"
      )
      self$results$help$setState(notes)
      return(NULL)
    }

    if (is.null(outcome_variable)) {
      if (
        is.null(self$options$outcome_variable) |
        length(self$options$outcome_variable) == 0
      ) return(NULL)
    }


  } else {
    args$comparison_mean <- jamovi_required_numeric(
      self$options$comparison_mean
    )
    args$comparison_sd <- jamovi_required_numeric(
      self$options$comparison_sd,
      lower = 0,
      lower_inclusive = FALSE
    )
    args$comparison_n <- jamovi_required_numeric(
      self$options$comparison_n,
      integer_required = TRUE,
      lower = 0,
      lower_inclusive = FALSE
    )

    args$reference_mean <- jamovi_required_numeric(
      self$options$reference_mean
    )

    unfilled <- names(args[which(is.na(args))])

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
    }

    if (length(notes) > 0) {
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
