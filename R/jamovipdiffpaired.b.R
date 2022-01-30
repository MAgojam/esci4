
# This file is a generated template, your changes will not be overwritten

jamovipdiffpairedClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovipdiffpairedClass",
    inherit = jamovipdiffpairedBase,
    private = list(
        .init = function() {
            from_raw <- (self$options$switch == "from_raw")

            # Get a handle for each table
            tbl_overview <- self$results$overview
            tbl_es_proportion_difference <- self$results$es_proportion_difference
            tbl_es_phi <- self$results$es_phi

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
            jamovi_set_confidence(tbl_es_phi, conf_level)

            #overview rows
            overview_rows <- 4
            if (from_raw) {
                overview_rows <- 0
                if (!is.null(self$options$reference_measure)) {
                    overview_rows <- length(levels(as.factor(self$data[, self$options$reference_measure])))
                }
                if (!is.null(self$options$comparison_measure)) {
                    overview_rows <- overview_rows + length(levels(as.factor(self$data[, self$options$reference_measure])))
                }

            }

            jamovi_init_table(tbl_overview, overview_rows)


        },
        .run = function() {

            estimate <- jamovi_pdiff_paired(self)

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


jamovi_pdiff_paired <- function(self) {
    # Prelim -----------------------------------------------------
    from_raw <- (self$options$switch == "from_raw")
    notes <- c(NULL)

    # Step 1 - Check if analysis basics are defined ---------------
    args <- list()


    if(from_raw) {
        if (
            is.null(self$options$comparison_measure) |
            is.null(self$options$reference_measure)
        ) return(NULL)

    } else {
        args$cases_consistent <- jamovi_required_numeric(
            self$options$cases_inconsistent,
            lower = 0,
            lower_inclusive = TRUE,
            integer_required = TRUE
        )
        args$cases_inconsistent <- jamovi_required_numeric(
            self$options$cases_inconsistent,
            integer_required = TRUE,
            lower = 0,
            lower_inclusive = TRUE
        )
        args$not_cases_consistent <- jamovi_required_numeric(
            self$options$not_cases_inconsistent,
            lower = 0,
            lower_inclusive = TRUE,
            integer_required = TRUE
        )
        args$not_cases_inconsistent <- jamovi_required_numeric(
            self$options$not_cases_inconsistent,
            integer_required = TRUE,
            lower = 0,
            lower_inclusive = TRUE
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
    call <- esci4::estimate_pdiff_paired

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
        args$reference_measure <- unname(self$options$reference_measure)
        args$comparison_measure <- unname(self$options$comparison_measure)
    } else {
        args$case_label <- jamovi_sanitize(
            self$options$case_label,
            return_value = "Affected",
            na_ok = FALSE
        )
        args$not_case_label <- jamovi_sanitize(
            self$options$not_case_label,
            return_value = "Not affected",
            na_ok = FALSE
        )
        args$comparison_measure_name <- jamovi_sanitize(
            self$options$comparison_measure_name,
            return_value = "Comparison measure",
            na_ok = FALSE
        )
        args$reference_measure_name <- jamovi_sanitize(
            self$options$reference_measure_name,
            return_value = "Reference measure",
            na_ok = FALSE
        )

        for (element in args) {
            notes <- c(notes, names(element))
        }

    }

    # b <- paste(names(args), args)
    # c <- NULL
    # for (e in args) {
    #     paste(c, class(e))
    # }
    # self$results$debug$setContent(paste(b, c, collapse = ", "))
    # return(NULL)

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
