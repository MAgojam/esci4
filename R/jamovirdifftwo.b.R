
# This file is a generated template, your changes will not be overwritten

jamovirdifftwoClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovirdifftwoClass",
    inherit = jamovirdifftwoBase,
    private = list(
        .init = function() {
            from_raw <- (self$options$switch == "from_raw")

            # Get a handle for each table
            tbl_overview <- self$results$overview
            tbl_es_proportion_difference <- self$results$es_proportion_difference

            # Prep output -------------------------------------------
            # Set CI and MoE columns to reflect confidence level
            conf_level <- jamovi_sanitize(
                my_value = self$options$conf_level,
                return_value = 95,
                na_ok = FALSE,
                convert_to_number = TRUE
            )

            jamovi_set_confidence(tbl_overview, conf_level)
            jamovi_set_confidence(tbl_es_proportion_difference, conf_level)

        },
        .run = function() {

            estimate <- jamovi_pdiff_two(self)

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

