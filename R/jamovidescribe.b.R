
# This file is a generated template, your changes will not be overwritten

jamovidescribeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovidescribeClass",
    inherit = jamovidescribeBase,
    private = list(
        .init = function() {
            jamovi_set_confidence(self$results$overview, 95)
        },
        .run = function() {

            # First - are options needed for analysis defined?
            if (is.null(self$options$outcome_variable)) return(TRUE)


            # Yes, we're doing the analysis
            args <- list()
            args$data <- self$data
            args$outcome_variable <- self$options$outcome_variable
            call <- esci4::estimate_magnitude

            estimate <- try(do.call(what = call, args = args))

            if(is.null(estimate)) return(TRUE)
            if(is(estimate, "try-error")) stop(estimate[1])

            jamovi_estimate_filler(self, estimate, TRUE)


        },
        .describe_plot = function(image, ggtheme, theme, ...) {

          # First - are options needed for analysis defined?
          if (is.null(self$options$outcome_variable)) return(TRUE)


          # Yes, we're doing the analysis
          args <- list()
          args$data <- self$data
          args$outcome_variable <- self$options$outcome_variable
          call <- esci4::estimate_magnitude

          estimate <- try(do.call(what = call, args = args))

          if(is.null(estimate)) return(TRUE)
          if(is(estimate, "try-error")) stop(estimate[1])

          mark_percentile <- jamovi_sanitize(
            my_value = self$options$mark_percentile,
            return_value = NULL,
            na_ok = FALSE,
            convert_to_number = TRUE,
            lower = 1,
            lower_inclusive = TRUE,
            upper = 100,
            upper_inclusive = TRUE,
            my_value_name = "Percentile"
          )
          if (!is.null(mark_percentile)) mark_percentile <- mark_percentile/100

          myplot <- plot_describe(
            estimate,
            mark_mean = self$options$mark_mean,
            mark_median = self$options$mark_median,
            mark_sd = self$options$mark_sd,
            mark_quartiles = self$options$mark_quartiles,
            mark_z_lines = self$options$mark_z_lines,
            mark_percentile = mark_percentile
          )

          print(myplot)
          return(TRUE)

        })
)


