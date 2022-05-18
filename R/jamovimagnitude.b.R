
# This file is a generated template, your changes will not be overwritten

jamovimagnitudeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovimagnitudeClass",
    inherit = jamovimagnitudeBase,
    private = list(
        .init = function() {
            from_raw <- (self$options$switch == "from_raw")
            try(tbl_overview <- self$results$overview)

            conf_level <- jamovi_sanitize(
                my_value = self$options$conf_level,
                return_value = 95,
                na_ok = FALSE,
                convert_to_number = TRUE
            )

            jamovi_set_confidence(tbl_overview, conf_level)


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
            if (from_raw) {
                level_count <- jamovi_sanitize(
                    my_value = length(self$options$outcome_variable),
                    return_value = 1,
                    convert_to_number = TRUE,
                    lower = 1,
                    lower_inclusive = TRUE
                )
            } else {
                level_count <- 1
            }
            image <- self$results$magnitude_plot
            image$setSize(width * level_count, height)

        },
        .run = function() {

            estimate <- jamovi_magnitude(self, FALSE)


            # Print any notes that emerged from running the analysis
            jamovi_set_notes(self$results$help)

            # Check to see if the analysis ran
            #  If null, return
            #  If error, return the error
            if(is.null(estimate)) return(TRUE)
            if(is(estimate, "try-error")) stop(estimate[1])

            # Fill tables
            jamovi_estimate_filler(self, estimate, TRUE)

        },
        .magnitude_plot = function(image, ggtheme, theme, ...) {

            # Do analysis
            estimate <- jamovi_magnitude(self, TRUE)
            if(is.null(estimate)) return(TRUE)
            if(is(estimate, "try-error")) stop(estimate[1])

            # Basic plot
            notes <- NULL
            args <- list()
            args$estimate <- estimate
            args$data_layout <- self$options$data_layout
            args$effect_size = self$options$effect_size
            args <- jamovi_arg_builder(
                args,
                "data_spread",
                my_value = self$options$data_spread,
                return_value = .25,
                convert_to_number = TRUE,
                lower = .01,
                lower_inclusive = TRUE,
                upper = 2,
                upper_inclusive = TRUE,
                my_value_name = "Data: Spread"
            )
            args$error_layout <- self$options$error_layout
            args <- jamovi_arg_builder(
                args,
                "error_scale",
                self$options$error_scale,
                return_value = 0.25,
                lower = 0,
                lower_inclusive = TRUE,
                upper = 5,
                upper_inclusive = TRUE,
                my_value_name = "Distributions: Width",
                convert_to_number = TRUE
            )
            args <- jamovi_arg_builder(
                args,
                "error_nudge",
                self$options$error_nudge,
                return_value = 0.4,
                lower = 0,
                lower_inclusive = TRUE,
                upper = 5,
                upper_inclusive = TRUE,
                my_value_name = "Distributions: Offset from data",
                convert_to_number = TRUE
            )
            args$ggtheme <- ggtheme[[1]]

            # Store notes from basic plot
            notes <- c(
                notes,
                args$warnings
            )
            args$warnings <- NULL

            # Do basic plot
            myplot <- do.call(
                what = plot_magnitude,
                args = args
            )

            # Basic graph options --------------------
            # Axis font sizes
            axis.text.y <- jamovi_sanitize(
                my_value = self$options$axis.text.y,
                return_value = 10,
                na_ok = FALSE,
                convert_to_number = TRUE,
                lower = 1,
                lower_inclusive = TRUE,
                my_value_name = "Y axis: Tick font size"
            )
            axis.title.y <- jamovi_sanitize(
                my_value = self$options$axis.title.y,
                return_value = 12,
                na_ok = FALSE,
                convert_to_number = TRUE,
                lower = 1,
                lower_inclusive = TRUE,
                my_value_name = "Y axis: Label font size"
            )
            axis.text.x <- jamovi_sanitize(
                my_value = self$options$axis.text.x,
                return_value = 10,
                na_ok = FALSE,
                convert_to_number = TRUE,
                lower = 1,
                lower_inclusive = TRUE,
                my_value_name = "X axis: Tick font size"
            )
            axis.title.x <- jamovi_sanitize(
                my_value = self$options$axis.title.x,
                return_value = 10,
                na_ok = FALSE,
                convert_to_number = TRUE,
                lower = 1,
                lower_inclusive = TRUE,
                my_value_name = "X axis: Label font size"
            )


            myplot <- myplot + ggplot2::theme(
                axis.text.y = element_text(size = axis.text.y),
                axis.title.y = element_text(size = axis.title.y),
                axis.text.x = element_text(size = axis.text.x),
                axis.title.x = element_text(size = axis.title.x)
            )


            # Axis labels
            xlab <- jamovi_sanitize(
                my_value = self$options$xlab,
                return_value = NULL,
                na_ok = FALSE,
                my_value_name = "X axis: Title"
            )

            ylab <- jamovi_sanitize(
                my_value = self$options$ylab,
                return_value = NULL,
                na_ok = FALSE,
                my_value_name = "Y axis: Title"
            )

            if (self$options$xlab != "auto") {
                myplot <- myplot + ggplot2::xlab(xlab)
            }
            if (self$options$ylab != "auto") {
                myplot <- myplot + ggplot2::ylab(ylab)
            }


            # Axis breaks
            ymin <- jamovi_sanitize(
                my_value = self$options$ymin,
                return_value = NULL,
                na_ok = FALSE,
                convert_to_number = TRUE,
                my_value_name = "Y axis: Axis minimum"
            )
            ymax <- jamovi_sanitize(
                my_value = self$options$ymax,
                return_value = NULL,
                na_ok = FALSE,
                convert_to_number = TRUE,
                my_value_name = "Y axis: Axis maximum"
            )
            breaks <- jamovi_sanitize(
                my_value = self$options$breaks,
                return_value = NULL,
                na_ok = FALSE,
                convert_to_number = TRUE,
                my_value_name = "Y axis: Number of tick marks"
            )

            myplot <- myplot + ggplot2::scale_y_continuous(
                limits = c(ymin, ymax),
                n.breaks = breaks
            )


            #aesthetics
            myplot <- myplot + ggplot2::scale_shape_manual(
                values = c(
                    "raw" = self$options$shape_raw,
                    "summary" = self$options$shape_summary
                )
            )

            myplot <- myplot + ggplot2::scale_color_manual(
                values = c(
                    "raw" = self$options$color_raw,
                    "summary" = self$options$color_summary
                ),
                aesthetics = c("color", "point_color")
            )

            myplot <- myplot + ggplot2::scale_fill_manual(
                values = c(
                    "raw" = self$options$fill_raw,
                    "summary" = self$options$fill_summary
                ),
                aesthetics = c("fill", "point_fill")
            )

            myplot <- myplot + ggplot2::discrete_scale(
                c("size", "point_size"),
                "point_size_d",
                function(n) return(c(
                    "raw" = as.numeric(self$options$size_raw),
                    "summary" = as.numeric(self$options$size_summary)
                ))
            )

            myplot <- myplot + ggplot2::discrete_scale(
                c("alpha", "point_alpha"),
                "point_alpha_d",
                function(n) return(c(
                    "raw" = as.numeric(self$options$alpha_raw),
                    "summary" = as.numeric(self$options$alpha_summary)
                ))
            )

            myplot <- myplot + ggplot2::scale_linetype_manual(
                values = c(
                    "summary" = self$options$linetype_summary
                )
            )

            myplot <- myplot + ggplot2::scale_color_manual(
                values = c(
                    "summary" = self$options$color_interval
                ),
                aesthetics = "interval_color"
            )
            myplot <- myplot + ggplot2::discrete_scale(
                "interval_alpha",
                "interval_alpha_d",
                function(n) return(c(
                    "summary" = as.numeric(self$options$alpha_interval)
                ))
            )
            myplot <- myplot + ggplot2::discrete_scale(
                "interval_size",
                "interval_size_d",
                function(n) return(c(
                    "summary" = as.numeric(self$options$size_interval)
                ))
            )

            # Slab
            myplot <- myplot + ggplot2::scale_fill_manual(
                values = c(
                    "summary" = self$options$fill_error
                ),
                aesthetics = "slab_fill"
            )
            myplot <- myplot + ggplot2::discrete_scale(
                "slab_alpha",
                "slab_alpha_d",
                function(n) return(c(
                    "summary" = as.numeric(self$options$alpha_error)
                ))
            )

            notes <- c(
                notes,
                names(axis.text.y),
                names(axis.title.y),
                names(axis.text.x),
                names(axis.title.x),
                names(xlab),
                names(ylab),
                names(ymin),
                names(ymax),
                names(breaks)
            )
            self$results$magnitude_plot_warnings$setState(notes)
            jamovi_set_notes(self$results$magnitude_plot_warnings)


            print(myplot)
            TRUE
        })
)


jamovi_magnitude <- function(self, save_raw_data = FALSE) {

    # Prelim -----------------------------------------------------
    from_raw <- (self$options$switch == "from_raw")
    notes <- c(NULL)

    # Step 1 - Check if analysis basics are defined ---------------
    args <- list()

    if(from_raw) {
        if (
            is.null(self$options$outcome_variable)
        ) return(NULL)
    } else {
        args$mean <- jamovi_required_numeric(
            self$options$mean
        )
        args$sd <- jamovi_required_numeric(
            self$options$sd,
            lower = 0,
            lower_inclusive = FALSE
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
    call <- esci4::estimate_magnitude

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
        args$outcome_variable <- unname(self$options$outcome_variable)
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
