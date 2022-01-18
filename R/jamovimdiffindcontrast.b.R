
# This file is a generated template, your changes will not be overwritten

jamovimdiffindcontrastClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovimdiffindcontrastClass",
    inherit = jamovimdiffindcontrastBase,
    private = list(
        .init = function() {

            jamovi_mdiff_initialize(self, grouping_variable = TRUE)

        },
        .run = function() {


        estimate <- jamovi_mdiff_contrastindependent(
            self = self,
            outcome_variables = self$options$outcome_variable,
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



        })
)
