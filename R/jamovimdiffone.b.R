
# This file is a generated template, your changes will not be overwritten

jamovimdiffoneClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovimdiffoneClass",
    inherit = jamovimdiffoneBase,
    private = list(
        .init = function() {

          from_raw <- (self$options$switch == "from_raw")

          # Get a handle for each table
          tbl_overview <- self$results$overview
          tbl_es_mean_difference <- self$results$es_mean_difference
          tbl_es_smd <- self$results$es_smd
          tbl_es_median_difference <- self$results$es_median_difference

          # Prep output -------------------------------------------
          # Set CI and MoE columns to reflect confidence level
          conf_level <- jamovi_sanitize(
            my_value = self$options$conf_level,
            return_value = 95,
            na_ok = FALSE,
            convert_to_number = TRUE
          )

          jamovi_set_confidence(tbl_overview, conf_level)
          jamovi_set_confidence(tbl_es_mean_difference, conf_level)
          jamovi_set_confidence(tbl_es_smd, conf_level)
          jamovi_set_confidence(tbl_es_median_difference, conf_level)

          # Outcomes: 1 if from summary, length of outcome_variables if raw
          outcome_count <- if(from_raw) {
            length(self$options$outcome_variable)
          } else {
            1
          }

          # For now, only 1 contrast can be specified
          contrast_count <- 1

          level_count <- 1

          # Rows needed for each table -------------------------------
          overview_rows <- level_count * outcome_count
          mdiff_rows <- contrast_count * outcome_count * 3
          smd_rows <- contrast_count * outcome_count

          jamovi_init_table(tbl_overview, overview_rows)
          jamovi_init_table(tbl_es_mean_difference, mdiff_rows, breaks = 3)
          jamovi_init_table(tbl_es_smd, smd_rows)
          jamovi_init_table(tbl_es_median_difference, mdiff_rows, breaks = 3)


          #
          if (self$options$as_difference) {
            self$results$magnitude_plot$setVisible(FALSE)
          }

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

          keys <- if (from_raw)
            self$options$outcome_variable
          else
            jamovi_sanitize(
              self$options$outcome_variable_name,
              "My outcome variable",
              na_ok = FALSE
            )

          for (my_key in keys) {
            self$results$estimation_plots$addItem(key = my_key)
            image <- self$results$estimation_plots$get(my_key)
            image$setSize(width , height)
          }

        },
        .run = function() {

          from_raw <- (self$options$switch == "from_raw")

          estimate <- jamovi_mdiff_one(
            self,
            outcome_variable = NULL,
            save_raw_data = FALSE
          )

          # Print any notes that emerged from running the analysis
          jamovi_set_notes(self$results$help)

          # Check to see if the analysis ran
          #  If null, return
          #  If error, return the error
          if(is.null(estimate)) return(TRUE)
          if(is(estimate, "try-error")) stop(estimate[1])

          # Add in MoE
          estimate$es_mean_difference$moe <- (estimate$es_mean_difference$UL - estimate$es_mean_difference$LL)/2
          estimate$overview$moe <- (estimate$overview$mean_UL - estimate$overview$mean_LL)/2

          # Add calculation details
          alpha <- 1 - self$options$conf_level/100
          estimate$es_mean_difference$t_multiplier <- stats::qt(1-alpha/2, estimate$es_mean_difference$df)
          estimate$es_mean_difference$s_component[c(1, 3)] <- estimate$es_smd$denominator[[1]]
          estimate$es_mean_difference$n_component <- estimate$es_mean_difference$moe / estimate$es_mean_difference$t_multiplier / estimate$es_mean_difference$s_component

          # Fill tables
          alpha <- 1 - as.numeric(self$options$conf_level)/100
          estimate$overview$t_multiplier <- stats::qt(1-alpha/2, estimate$overview$df)
          estimate$overview$s_component <- estimate$overview$sd
          estimate$overview$n_component <- 1/sqrt(estimate$overview$n)


          # Fill tables
          jamovi_estimate_filler(self, estimate, TRUE)

          # Deal with plots ----------------------------------------
          # Set up array of estimation plots
          keys <- if (from_raw)
            self$options$outcome_variable
          else
            jamovi_sanitize(
              self$options$outcome_variable_name,
              "My outcome variable",
              na_ok = FALSE
            )

          for (my_key in keys) {
            image <- self$results$estimation_plots$get(key=my_key)
            image$setState(my_key)
          }

          if (length(keys) > 1) {
            self$results$estimation_plots$setTitle(
              paste(
                self$results$estimation_plots$title,
                "s",
                sep = ""
              )
            )
          }


        },
        .estimation_plots = function(image, ggtheme, theme, ...) {
          if (!self$options$as_difference) return(FALSE)

          reference_mean <- jamovi_required_numeric(
            self$options$reference_mean
          )
          if (is.null(reference_mean)) return(TRUE)
          if (!is.numeric(reference_mean)) return(TRUE)
          if (is.na(reference_mean)) return(TRUE)

          if (is.null(image$state))
            return(FALSE)

          # Redo analysis
          estimate <- jamovi_mdiff_one(
            self,
            outcome_variable = c(image$state),
            save_raw_data = TRUE
          )

          if(!is(estimate, "esci_estimate"))
            return(TRUE)

          myplot <- jamovi_plot_mdiff(
            self,
            estimate,
            image,
            ggtheme,
            theme
          )

          print(myplot)
          TRUE

        },
        .magnitude_plot = function(image, ggtheme, theme, ...) {

          if (self$options$as_difference) return(FALSE)

          # Redo analysis
          estimate <- jamovi_mdiff_one(
            self,
            outcome_variable = c(image$state),
            save_raw_data = TRUE
          )

          if(is.null(estimate)) return(TRUE)
          if(is(estimate, "try-error")) stop(estimate[1])

          # Basic plot
          notes <- NULL
          args <- list()
          args$estimate <- estimate
          args$data_layout <- self$options$data_layout
          if (self$options$effect_size == "mean_difference") {
            args$effect_size = "mean"
          } else {
            args$effect_size = "median"
          }
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
            my_value = self$options$ybreaks,
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
              "raw" = self$options$shape_raw_comparison,
              "summary" = self$options$shape_summary_comparison
            )
          )

          myplot <- myplot + ggplot2::scale_color_manual(
            values = c(
              "raw" = self$options$color_raw_comparison,
              "summary" = self$options$color_summary_comparison
            ),
            aesthetics = c("color", "point_color")
          )

          myplot <- myplot + ggplot2::scale_fill_manual(
            values = c(
              "raw" = self$options$fill_raw_comparison,
              "summary" = self$options$fill_summary_comparison
            ),
            aesthetics = c("fill", "point_fill")
          )

          myplot <- myplot + ggplot2::discrete_scale(
            c("size", "point_size"),
            "point_size_d",
            function(n) return(c(
              "raw" = as.numeric(self$options$size_raw_comparison),
              "summary" = as.numeric(self$options$size_summary_comparison)
            ))
          )

          myplot <- myplot + ggplot2::discrete_scale(
            c("alpha", "point_alpha"),
            "point_alpha_d",
            function(n) return(c(
              "raw" = as.numeric(self$options$alpha_raw_comparison),
              "summary" = as.numeric(self$options$alpha_summary_comparison)
            ))
          )

          myplot <- myplot + ggplot2::scale_linetype_manual(
            values = c(
              "summary" = self$options$linetype_summary_comparison
            )
          )

          myplot <- myplot + ggplot2::scale_color_manual(
            values = c(
              "summary" = self$options$color_interval_comparison
            ),
            aesthetics = "interval_color"
          )
          myplot <- myplot + ggplot2::discrete_scale(
            "interval_alpha",
            "interval_alpha_d",
            function(n) return(c(
              "summary" = as.numeric(self$options$alpha_interval_comparison)
            ))
          )
          myplot <- myplot + ggplot2::discrete_scale(
            "interval_size",
            "interval_size_d",
            function(n) return(c(
              "summary" = as.numeric(self$options$size_interval_comparison)
            ))
          )

          # Slab
          myplot <- myplot + ggplot2::scale_fill_manual(
            values = c(
              "summary" = self$options$fill_error_comparison
            ),
            aesthetics = "slab_fill"
          )
          myplot <- myplot + ggplot2::discrete_scale(
            "slab_alpha",
            "slab_alpha_d",
            function(n) return(c(
              "summary" = as.numeric(self$options$alpha_error_comparison)
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


jamovi_mdiff_one <- function(self, outcome_variable = NULL, save_raw_data = FALSE) {

  # Prelim -----------------------------------------------------
  from_raw <- (self$options$switch == "from_raw")
  as_difference <- self$options$as_difference
  notes <- c(NULL)


  # Step 1 - Check if analysis basics are defined ---------------
  args <- list()


  if (as_difference) {
    args$reference_mean <- jamovi_required_numeric(
      self$options$reference_mean
    )
    if (!is.numeric(args$reference_mean)) {
      notes <- c(
        "For this analysis, please specify a Reference mean (<i>M</i><sub>Referenece</sub>)"
      )
      self$results$help$setState(notes)
      args$reference_mean <- NULL
      # return(NULL)
    }

  }

  if(from_raw) {

    if (is.null(outcome_variable)) {
      if (
        is.null(self$options$outcome_variable) |
        length(self$options$outcome_variable) == 0
      ) return(NULL)
    }

  } else {
    args$comparison_mean <- jamovi_required_numeric(
      self$options$comparison_mean,
      my_value_name = "Comparison <i>M</i>"
    )
    args$comparison_sd <- jamovi_required_numeric(
      self$options$comparison_sd,
      lower = 0,
      lower_inclusive = FALSE,
      my_value_name = "Comparison <i>s</i>"
    )
    args$comparison_n <- jamovi_required_numeric(
      self$options$comparison_n,
      integer_required = TRUE,
      lower = 0,
      lower_inclusive = FALSE,
      my_value_name = "Comparison <i>n</i>"
    )

    # if (as_difference) {
    #   args$reference_mean <- jamovi_required_numeric(
    #     self$options$reference_mean,
    #     my_value_name = "Reference <i>M</i>"
    #   )
    #
    # }

    unfilled <- NULL
    for (element in args[which(is.na(args))]) {
      unfilled <- c(unfilled, names(element))
    }

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
  call <- esci4::estimate_mdiff_one

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
    if (is.null(outcome_variable)) {
      args$outcome_variable <- unname(self$options$outcome_variable)
    } else {
      args$outcome_variable <- unname(outcome_variable)
      args$outcome_variable_name <- outcome_variable
    }
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


  #self$results$debug$setContent(args)

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
