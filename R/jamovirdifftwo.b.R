
# This file is a generated template, your changes will not be overwritten

jamovirdifftwoClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovirdifftwoClass",
    inherit = jamovirdifftwoBase,
    private = list(
        .init = function() {
            from_raw <- (self$options$switch == "from_raw")

            # Get a handle for each table
            tbl_overview <- self$results$overview
            tbl_es_r <- self$results$es_r
            tbl_es_r_difference <- self$results$es_r_difference

            tbl_overview$setVisible(from_raw)

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
            jamovi_set_confidence(tbl_es_r, conf_level)
            jamovi_set_confidence(tbl_es_r_difference, conf_level)

        },
        .run = function() {

            estimate <- jamovi_rdiff_two(self)

            # Print any notes that emerged from running the analysis
            jamovi_set_notes(self$results$help)

            # Check to see if the analysis ran
            #  If null, return
            #  If error, return the error
            if(is.null(estimate)) return(TRUE)
            if(is(estimate, "try-error")) stop(estimate[1])

            # Fill in MoE
            estimate$overview$moe <- (estimate$overview$mean_UL - estimate$overview$mean_LL)/2

            # Fill tables
            jamovi_estimate_filler(self, estimate, TRUE)

#
#             self$results$debug$setContent(estimate)
#             self$results$debug$setVisible(TRUE)

        },
        .estimation_plots = function(image, ggtheme, theme, ...) {


          # Redo analysis
          estimate <- jamovi_rdiff_two(self)

          if(!is(estimate, "esci_estimate"))
            return(TRUE)

          notes <- NULL

          #Hyothesis evaluation
          interval_null <- FALSE
          htest <- FALSE
          args <- list()
          graph_call <- "plot_rdiff"

          args$estimate <- estimate
          args$ggtheme <- ggtheme[[1]]

          try(htest <- self$options$evaluate_hypotheses)
          if (htest) {
            args <- jamovi_arg_builder(
              args,
              "null_boundary",
              my_value = self$options$null_boundary,
              return_value = 0,
              convert_to_number = TRUE,
              lower = 0,
              lower_inclusive = TRUE,
              upper = 1,
              upper_inclusive = TRUE,
              my_value_name = "Hypothesis Evaluation: Null range (+/-)"
            )

            args$rope <- c(
              0 - args$null_boundary,
              0 + args$null_boundary
            )

            if (args$rope[[1]] != args$rope[[2]]) {
              interval_null <- TRUE
            }

            notes <- c(
              notes,
              names(args$null_boundary),
              args$warnings
            )
            args$null_boundary <- NULL
            args$warnings <- NULL
          }


          myplot <- do.call(
            what = graph_call,
            args = args
          )


          self$results$estimation_plot_warnings$setState(
            c(
              notes
            )
          )
          jamovi_set_notes(self$results$estimation_plot_warnings)


          print(myplot)
          TRUE

        },
        .scatter_plots = function(image, ggtheme, theme, ...) {

          # Redo analysis
          estimate <- jamovi_rdiff_two(self)

          if(!is(estimate, "esci_estimate"))
            return(TRUE)

          if (is.null(estimate$raw_data)) return(TRUE)

          myplot <- plot_scatter(
            estimate,
            ggtheme = ggtheme[[1]]
          )

          print(myplot)
          TRUE

        })
)


jamovi_rdiff_two <- function(self) {
    # Prelim -----------------------------------------------------
    from_raw <- (self$options$switch == "from_raw")
    notes <- c(NULL)

    # Step 1 - Check if analysis basics are defined ---------------
    args <- list()


    if(from_raw) {
        if (
            is.null(self$options$x) |
            is.null(self$options$y) |
            is.null(self$options$grouping_variable)
        ) return(NULL)

    } else {
        args$comparison_r <- jamovi_required_numeric(
            self$options$comparison_r,
            lower = -1,
            lower_inclusive = TRUE,
            upper = 1,
            upper_inclusive = TRUE
        )
        args$comparison_n <- jamovi_required_numeric(
            self$options$comparison_n,
            integer_required = TRUE,
            lower = 0,
            lower_inclusive = FALSE
        )
        args$reference_r <- jamovi_required_numeric(
            self$options$reference_r,
            lower = -1,
            lower_inclusive = TRUE,
            upper = 1,
            upper_inclusive = TRUE
        )
        args$reference_n <- jamovi_required_numeric(
            self$options$reference_n,
            integer_required = TRUE,
            lower = 0,
            lower_inclusive = FALSE
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
    call <- esci4::estimate_rdiff_two

    args$conf_level <- jamovi_sanitize(
      my_value = self$options$conf_level,
      return_value = 95,
      na_ok = FALSE,
      convert_to_number = TRUE,
      lower = 75,
      lower_inclusive = FALSE,
      upper = 100,
      upper_inclusive = FALSE,
      my_value_name = "Confidence level"
    )/100


    if(from_raw) {
        args$data <- self$data
        args$x <- unname(self$options$x)
        args$y <- unname(self$options$y)
        args$grouping_variable <- unname(self$options$grouping_variable)
    } else {
        args$comparison_level_name <- jamovi_sanitize(
            self$options$comparison_level_name,
            return_value = "Comparison level",
            na_ok = FALSE
        )
        args$reference_level_name <- jamovi_sanitize(
            self$options$reference_level_name,
            return_value = "Reference level",
            na_ok = FALSE
        )
        args$x_variable_name <- jamovi_sanitize(
            self$options$x_variable_name,
            return_value = "X variable",
            na_ok = FALSE
        )
        args$y_variable_name <- jamovi_sanitize(
            self$options$y_variable_name,
            return_value = "Y variable",
            na_ok = FALSE
        )
        args$grouping_variable_name <- jamovi_sanitize(
            self$options$grouping_variable_name,
            return_value = "Grouping variable",
            na_ok = FALSE
        )


        for (element in args) {
            notes <- c(notes, names(element))
        }

        args$grouping_variable_levels <- c(
            args$comparison_level_name,
            args$reference_level_name
        )

        args$comparison_level_name <- NULL
        args$reference_level_name <- NULL

    }



    # Do analysis, then post any notes that have emerged
    estimate <- try(do.call(what = call, args = args))

    if (!is(estimate, "try-error")) {

      estimate <- jamovi_add_htest_rdiff(
        self = self,
        estimate = estimate
      )

      if (length(estimate$warnings) > 0) {
          notes <- c(notes, estimate$warnings)
      }
    }

    self$results$help$setState(notes)


    return(estimate)
}
