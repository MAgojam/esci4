
# This file is a generated template, your changes will not be overwritten

jamovipdifftwoClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovipdifftwoClass",
    inherit = jamovipdifftwoBase,
    private = list(
        .init = function() {
            from_raw <- (self$options$switch == "from_raw")

            # Get a handle for each table
            tbl_overview <- self$results$overview
            tbl_es_proportion_difference <- self$results$es_proportion_difference
            tbl_es_odds_ratio <- self$results$es_odds_ratio

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
            jamovi_set_confidence(tbl_es_odds_ratio, conf_level)


            # Outcomes: 1 if from summary, length of outcome_variables if raw
            outcome_count <- if(from_raw) {
                length(self$options$outcome_variable)
            } else {
                1
            }


            # For now, only 1 contrast can be specified
            contrast_count <- 1

            # How many levels?
            #  For raw, check grouping_variable
            #  For summary, check group_labels
            level_count <- 1
            if (from_raw & !is.null(self$options$grouping_variable)) {
                level_source <- self$options$grouping_variable
                level_count <- length(levels(as.factor(self$data[, level_source])))
            }

            #overview rows
            overview_rows <- 2 * level_count
            o <- NULL
            if (from_raw & !is.null(self$options$outcome_variable)) {
                for (myi in 1:length(self$options$outcome_variable)) {
                    current_outcome <- self$options$outcome_variable[[myi]]
                    o <- c(o, length(levels(as.factor(self$data[, current_outcome]))))
                }
                overview_rows <- sum(level_count * o)
            }

            # Rows needed for each table -------------------------------
            mdiff_rows <- contrast_count * outcome_count * 3
            smd_rows <- contrast_count * outcome_count

            jamovi_init_table(tbl_overview, overview_rows)
            jamovi_init_table(tbl_es_proportion_difference, mdiff_rows, breaks = 3)
            jamovi_init_table(tbl_es_odds_ratio, smd_rows)


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


jamovi_pdiff_two <- function(self) {
    # Prelim -----------------------------------------------------
    from_raw <- (self$options$switch == "from_raw")
    notes <- c(NULL)

    # Step 1 - Check if analysis basics are defined ---------------
    args <- list()


    if(from_raw) {
        if (
            is.null(self$options$outcome_variable) |
            is.null(self$options$grouping_variable)
        ) return(NULL)

    } else {
        args$comparison_cases <- jamovi_required_numeric(
            self$options$comparison_cases,
            lower = 0,
            lower_inclusive = TRUE,
            integer_required = TRUE
        )
        args$comparison_n <- jamovi_required_numeric(
            self$options$comparison_n,
            integer_required = TRUE,
            lower = 0,
            lower_inclusive = FALSE
        )

        args$reference_cases <- jamovi_required_numeric(
            self$options$reference_cases,
            lower = 0,
            lower_inclusive = TRUE,
            integer_required = TRUE
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
    call <- esci4::estimate_pdiff_two

    args$count_NA <- self$options$count_NA
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
        args$outcome_variable <- unname(self$options$outcome_variable)
        args$grouping_variable <- unname(self$options$grouping_variable)
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
        args$grouping_variable_level1 <- jamovi_sanitize(
            self$options$grouping_variable_level1,
            return_value = "Comparison Level",
            na_ok = FALSE
        )
        args$grouping_variable_level2 <- jamovi_sanitize(
            self$options$grouping_variable_level2,
            return_value = "Reference Level",
            na_ok = FALSE
        )

        args$outcome_variable_name <- jamovi_sanitize(
            self$options$outcome_variable_name,
            return_value = "My outcome variable",
            na_ok = FALSE
        )
        args$grouping_variable_name <- jamovi_sanitize(
            self$options$grouping_variable_name,
            return_value = "My grouping variable",
            na_ok = FALSE
        )

        for (element in args) {
            notes <- c(notes, names(element))
        }

        args$grouping_variable_levels <- c(
            args$grouping_variable_level1,
            args$grouping_variable_level2
        )

        args$grouping_variable_level1 <- NULL
        args$grouping_variable_level2 <- NULL

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
