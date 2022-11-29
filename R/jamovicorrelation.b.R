
# This file is a generated template, your changes will not be overwritten

jamovicorrelationClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovicorrelationClass",
    inherit = jamovicorrelationBase,
    private = list(
        .init = function() {
            from_raw <- (self$options$switch == "from_raw")

            # Get a handle for each table
            tbl_overview <- self$results$overview
            tbl_es_r <- self$results$es_r

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

        },
        .run = function() {

            estimate <- jamovi_correlation(self)

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


jamovi_correlation <- function(self) {
    # Prelim -----------------------------------------------------
    from_raw <- (self$options$switch == "from_raw")
    notes <- c(NULL)

    # Step 1 - Check if analysis basics are defined ---------------
    args <- list()


    if(from_raw) {
        if (
            is.null(self$options$x) |
            is.null(self$options$y)
        ) return(NULL)

    } else {
        args$r <- jamovi_required_numeric(
            self$options$r,
            lower = -1,
            lower_inclusive = TRUE,
            upper = 1,
            upper_inclusive = TRUE
        )
        args$n <- jamovi_required_numeric(
            self$options$n,
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
    call <- esci4::estimate_correlation

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
    } else {
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
