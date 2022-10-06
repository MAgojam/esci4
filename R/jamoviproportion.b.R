
# This file is a generated template, your changes will not be overwritten

jamoviproportionClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamoviproportionClass",
    inherit = jamoviproportionBase,
    private = list(
        .init = function() {
          from_raw <- (self$options$switch == "from_raw")

          # Get a handle for each table
          tbl_overview <- self$results$overview

          # Prep output -------------------------------------------
          # Set CI and MoE columns to reflect confidence level
          conf_level <- jamovi_sanitize(
            my_value = self$options$conf_level,
            return_value = 95,
            na_ok = FALSE,
            convert_to_number = TRUE
          )

          jamovi_set_confidence(tbl_overview, conf_level)


          width <- jamovi_sanitize(
            my_value = self$options$es_plot_width,
            return_value = 300,
            convert_to_number = TRUE,
            lower = 10,
            lower_inclusive = TRUE,
            upper = 2000,
            upper_inclusive = TRUE
          )
          height <- jamovi_sanitize(
            my_value = self$options$es_plot_height,
            return_value = 400,
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

          estimate <- jamovi_proportion(self)

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
          estimate <- jamovi_proportion(self)
          if(is.null(estimate)) return(TRUE)
          if(is(estimate, "try-error")) stop(estimate[1])

          # self$results$debug$setContent(estimate)
          # self$results$debug$setVisible(TRUE)
          # return(TRUE)

          # Basic plot
          divider <- 1
          notes <- NULL
          args <- list()
          args$estimate <- estimate

          #Hyothesis evaluation
          interval_null <- FALSE
          if (self$options$evaluate_hypotheses) {
            args <- jamovi_arg_builder(
              args,
              "null_boundary",
              my_value = self$options$null_boundary,
              return_value = 0,
              convert_to_number = TRUE,
              lower = 0,
              lower_inclusive = TRUE,
              my_value_name = "Hypothesis Evaluation: Null range (+/-)"
            )
            args <- jamovi_arg_builder(
              args,
              "point_null",
              my_value = self$options$null_value,
              return_value = 0,
              convert_to_number = TRUE,
              my_value_name = "Hypothesis Evaluation: Null value"
            )

            multiplier <- 1

            args$rope <- c(
              args$point_null - (args$null_boundary * multiplier),
              args$point_null + (args$null_boundary * multiplier)
            )

            if (args$rope[[1]] != args$rope[[2]]) {
              interval_null <- TRUE
            }

            notes <- c(
              notes,
              names(args$point_null),
              names(args$null_boundary)
            )
            args$point_null <- NULL
            args$null_boundary <- NULL
          }

          args$ggtheme <- ggtheme[[1]]

          width <- jamovi_sanitize(
            my_value = self$options$es_plot_width,
            return_value = 300,
            convert_to_number = TRUE,
            lower = 10,
            lower_inclusive = TRUE,
            upper = 2000,
            upper_inclusive = TRUE,
            my_value_name = "Plot width"
          )
          height <- jamovi_sanitize(
            my_value = self$options$es_plot_height,
            return_value = 400,
            convert_to_number = TRUE,
            lower = 10,
            lower_inclusive = TRUE,
            upper = 4000,
            upper_inclusive = TRUE,
            my_value_name = "Plot height"
          )


          # Store notes from basic plot
          notes <- c(
            notes,
            args$warnings,
            names(width),
            names(height)
          )
          args$warnings <- NULL

          # Do basic plot
          myplot <- do.call(
            what = plot_proportion,
            args = args
          )


          # Basic graph options --------------------
          # Axis font sizes
          axis.text.y <- jamovi_sanitize(
            my_value = self$options$axis.text.y,
            return_value = 14,
            na_ok = FALSE,
            convert_to_number = TRUE,
            lower = 1,
            lower_inclusive = TRUE,
            upper = 97,
            my_value_name = "Y axis: Tick font size"
          )
          axis.title.y <- jamovi_sanitize(
            my_value = self$options$axis.title.y,
            return_value = 15,
            na_ok = FALSE,
            convert_to_number = TRUE,
            lower = 1,
            lower_inclusive = TRUE,
            upper = 97,
            my_value_name = "Y axis: Label font size"
          )
          axis.text.x <- jamovi_sanitize(
            my_value = self$options$axis.text.x,
            return_value = 14,
            na_ok = FALSE,
            convert_to_number = TRUE,
            lower = 1,
            lower_inclusive = TRUE,
            upper = 97,
            my_value_name = "X axis: Tick font size"
          )
          axis.title.x <- jamovi_sanitize(
            my_value = self$options$axis.title.x,
            return_value = 15,
            na_ok = FALSE,
            convert_to_number = TRUE,
            lower = 1,
            lower_inclusive = TRUE,
            upper = 97,
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


          if (!(self$options$xlab %in% c("auto", "Auto", "AUTO"))) {
            myplot <- myplot + ggplot2::xlab(xlab)
          }
          if (!(self$options$ylab %in% c("auto", "Auto", "AUTO"))) {
            myplot <- myplot + ggplot2::ylab(ylab)
          }


          # Axis breaks
          breaks <- jamovi_sanitize(
            my_value = self$options$breaks,
            return_value = NULL,
            na_ok = FALSE,
            lower = 1,
            lower_inclusive = TRUE,
            upper = 200,
            upper_inclusive = TRUE,
            convert_to_number = TRUE,
            my_value_name = "Y axis: Number of tick marks"
          )

          myplot <- myplot + ggplot2::scale_y_continuous(
            limits = c(0, 1),
            n.breaks = breaks
          )


          #aesthetics
          myplot <- myplot + ggplot2::scale_shape_manual(
            values = c(
              "summary" = self$options$shape_summary
            )
          )

          myplot <- myplot + ggplot2::scale_color_manual(
            values = c(
              "summary" = self$options$color_summary
            ),
            aesthetics = c("color", "point_color")
          )

          myplot <- myplot + ggplot2::scale_fill_manual(
            values = c(
              "summary" = self$options$fill_summary
            ),
            aesthetics = c("fill", "point_fill")
          )

          myplot <- myplot + ggplot2::discrete_scale(
            c("size", "point_size"),
            "point_size_d",
            function(n) return(c(
              "summary" = as.numeric(self$options$size_summary)/divider
            ))
          )

          myplot <- myplot + ggplot2::discrete_scale(
            c("alpha", "point_alpha"),
            "point_alpha_d",
            function(n) return(c(
              "summary" = as.numeric(self$options$alpha_summary)
            ))
          )

          myplot <- myplot + ggplot2::scale_linetype_manual(
            values = c(
              "summary" = self$options$linetype_summary
            )
          )


          if (self$options$evaluate_hypotheses) {
            myplot$layers[["null_line"]]$aes_params$colour <- self$options$null_color
            if (interval_null) {
              try(myplot$layers[["null_interval"]]$aes_params$fill <- self$options$null_color)
              try(myplot$layers[["ta_CI"]]$aes_params$size <- as.numeric(self$options$size_interval)/divider+1)
              try(myplot$layers[["ta_CI"]]$aes_params$alpha <- as.numeric(self$options$alpha_interval))
              try(myplot$layers[["ta_CI"]]$aes_params$colour <- self$options$color_interval)
              try(myplot$layers[["ta_CI"]]$aes_params$linetype <- self$options$linetype_summary)

              # if (self$options$effect_size == "median") {
              #  try(myplot$layers[["ta_CI"]]$aes_params$colour <- self$options$color_summary)
              # }

            }
          }


          notes <- c(
            notes,
            names(axis.text.y),
            names(axis.title.y),
            names(axis.text.x),
            names(axis.title.x),
            names(xlab),
            names(ylab),
            names(breaks)
          )
          self$results$magnitude_plot_warnings$setState(notes)
          jamovi_set_notes(self$results$magnitude_plot_warnings)


          print(myplot)
          TRUE
        })
)

jamovi_proportion <- function(self) {
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
    args$comparison_cases <- jamovi_required_numeric(
      self$options$cases,
      lower = 0,
      lower_inclusive = TRUE,
      integer_required = TRUE
    )
    args$comparison_n <- jamovi_required_numeric(
      self$options$observations,
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

    args$cases <- c(args$cases, args$observations - args$cases)
    args$observations <- NULL

  }


  # Step 2: Get analysis properties-----------------------------
  call <- esci4::estimate_pdiff_one

  args$reference_p <- jamovi_sanitize(
    my_value = self$options$null_value,
    return_value = 0,
    convert_to_number = TRUE,
    my_value_name = "Null value"
  )

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
  } else {
    args$case_label <- jamovi_sanitize(
      self$options$case_label,
      return_value = "Affected",
      na_ok = FALSE
    )
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
      estimate <- jamovi_add_htest_pdiff(
        self = self,
        estimate = estimate
      )

      notes <- c(notes, estimate$warnings)
    }
  }

  self$results$help$setState(notes)

  return(estimate)
}
