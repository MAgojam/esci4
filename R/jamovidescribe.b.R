
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


        })
)
