
# This file is a generated template, your changes will not be overwritten

jamovirmetameanClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovirmetameanClass",
    inherit = jamovirmetameanBase,
    private = list(
        .init = function() {

            tbl_raw_data <- self$results$raw_data
            tbl_es_meta <- self$results$es_meta
            tbl_es_meta_difference <- self$results$es_meta_difference

            conf_level <<- jamovi_sanitize(
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

            jamovi_set_confidence(tbl_raw_data, conf_level)
            jamovi_set_confidence(tbl_es_meta, conf_level)


            moderator <- !is.null(self$options$moderator)

            tbl_es_meta_difference$setVisible(moderator)
            tbl_es_meta$getColumn("moderator_variable_name")$setVisible(moderator)
            tbl_es_meta$getColumn("moderator_variable_level")$setVisible(moderator)
            tbl_raw_data$getColumn("moderator")$setVisible(moderator)

            meta_note <- if(self$options$random_effects)
                "Estimate is based on a random effects model."
            else
                "Estimate is based on a fixed effects model."

            if (self$options$reported_effect_size == "smd") {
                meta_note <- paste(
                    meta_note,
                    "  This standardized mean difference has been corrected for sampling bias."
                )
            }

            tbl_es_meta$setNote(
                key = "meta_note",
                note = meta_note
            )


        },
        .run = function() {

            estimate <- jamovi_meta_mean(self)


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
        .estimation_plots = function(image, ggtheme, theme, ...) {

          # Redo analysis
          estimate <- jamovi_meta_mean(self)

          if(!is(estimate, "esci_estimate"))
            return(TRUE)

          myplot <- plot_meta(
            estimate,
            mark_zero = TRUE,
            report_CIs = FALSE,
            meta_diamond_height = .35,
            ggtheme = ggtheme
          )

          if (!is.null(myplot$layers$raw_Reference_point)) {
            myplot$layers$raw_Reference_point$aes_params$shape <- self$options$shape_raw_reference
            myplot$layers$raw_Reference_point$aes_params$colour <- self$options$color_raw_reference
            myplot$layers$raw_Reference_point$aes_params$fill <- self$options$fill_raw_reference
            myplot$layers$raw_Reference_point$aes_params$alpha <- as.numeric(self$options$alpha_raw_reference)

            myplot$layers$raw_Reference_error$aes_params$colour <- self$options$color_interval_reference
            myplot$layers$raw_Reference_error$aes_params$size <- as.numeric(self$options$size_interval_reference)
            myplot$layers$raw_Reference_error$aes_params$alpha <- as.numeric(self$options$alpha_interval_reference)
            myplot$layers$raw_Reference_error$aes_params$linetype <- self$options$linetype_raw_reference
          }
          if (!is.null(myplot$layers$raw_Comparison_point)){
            myplot$layers$raw_Comparison_point$aes_params$shape <- self$options$shape_raw_comparison
            myplot$layers$raw_Comparison_point$aes_params$colour <- self$options$color_raw_comparison
            myplot$layers$raw_Comparison_point$aes_params$fill <- self$options$fill_raw_comparison
            myplot$layers$raw_Comparison_point$aes_params$alpha <- as.numeric(self$options$alpha_raw_comparison)

            myplot$layers$raw_Comparison_error$aes_params$colour <- self$options$color_interval_comparison
            myplot$layers$raw_Comparison_error$aes_params$size <- as.numeric(self$options$size_interval_comparison)
            myplot$layers$raw_Comparison_error$aes_params$alpha <- as.numeric(self$options$alpha_interval_comparison)
            myplot$layers$raw_Comparison_error$aes_params$linetype <- self$options$linetype_raw_comparison
          }
          if (!is.null(myplot$layers$raw_Unused_point)) {
            myplot$layers$raw_Unused_point$aes_params$shape <- self$options$shape_raw_unused
            myplot$layers$raw_Unused_point$aes_params$colour <- self$options$color_raw_unused
            myplot$layers$raw_Unused_point$aes_params$fill <- self$options$fill_raw_unused
            myplot$layers$raw_Unused_point$aes_params$alpha <- as.numeric(self$options$alpha_raw_unused)

            myplot$layers$raw_Unused_error$aes_params$colour <- self$options$color_interval_unused
            myplot$layers$raw_Unused_error$aes_params$size <- as.numeric(self$options$size_interval_unused)
            myplot$layers$raw_Unused_error$aes_params$alpha <- as.numeric(self$options$alpha_interval_unused)
            myplot$layers$raw_Unused_error$aes_params$linetype <- self$options$linetype_raw_unused
          }

          if (!is.null(myplot$layers$group_Overall_diamond)) {
            myplot$layers$group_Overall_diamond$aes_params$colour <- self$options$color_summary_overall
            myplot$layers$group_Overall_diamond$aes_params$fill <- self$options$fill_summary_overall
            myplot$layers$group_Overall_diamond$aes_params$alpha <- as.numeric(self$options$alpha_summary_overall)
          }

          if (!is.null(myplot$layers$group_Comparison_diamond)) {
            myplot$layers$group_Comparison_diamond$aes_params$colour <- self$options$color_summary_comparison
            myplot$layers$group_Comparison_diamond$aes_params$fill <- self$options$fill_summary_comparison
            myplot$layers$group_Comparison_diamond$aes_params$alpha <- as.numeric(self$options$alpha_summary_comparison)
          }

          if (!is.null(myplot$layers$group_Reference_diamond)) {
            myplot$layers$group_Reference_diamond$aes_params$colour <- self$options$color_summary_reference
            myplot$layers$group_Reference_diamond$aes_params$fill <- self$options$fill_summary_reference
            myplot$layers$group_Reference_diamond$aes_params$alpha <- as.numeric(self$options$alpha_summary_reference)
          }

          if (!is.null(myplot$layers$group_Difference_diamond)) {
            myplot$layers$group_Difference_diamond$aes_params$colour <- self$options$color_summary_difference
            myplot$layers$group_Difference_diamond$aes_params$fill <- self$options$fill_summary_difference
            myplot$layers$group_Difference_diamond$aes_params$alpha <- as.numeric(self$options$alpha_summary_difference)
          }

          if (!is.null(myplot$layers$group_Unused_diamond)) {
            myplot$layers$group_Unused_diamond$aes_params$colour <- self$options$color_summary_unused
            myplot$layers$group_Unused_diamond$aes_params$fill <- self$options$fill_summary_unused
            myplot$layers$group_Unused_diamond$aes_params$alpha <- as.numeric(self$options$alpha_summary_unused)
          }

          print(myplot)
          TRUE

        })
)


jamovi_meta_mean <- function(self) {

    # Prelim -----------------------------------------------------
    notes <- c(NULL)

    # Step 1 - Check if analysis basics are defined ---------------
    args <- list()

    if (
        is.null(self$options$means) |
        is.null(self$options$sds) |
        is.null(self$options$ns)
    ) return(NULL)


    # Step 2: Get analysis properties-----------------------------
    call <- esci4::meta_mean

    args$effect_label <- jamovi_sanitize(
        self$options$effect_label,
        return_value = "My effect",
        na_ok = FALSE
    )

    args$reference_mean <- jamovi_sanitize(
        self$options$reference_mean,
        return_value = 0,
        na_ok = FALSE,
        convert_to_number = TRUE
    )

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


    for (element in args) {
        notes <- c(notes, names(element))
    }


    if (!is.null(self$options$moderator)) {
        args$moderator <- self$options$moderator
    }

    if (!is.null(self$options$labels)) {
        args$labels <- self$options$labels
    }

    args$data <- self$data
    args$means <- self$options$means
    args$sds <- self$options$sds
    args$ns <- self$options$ns

    args$reported_effect_size <- self$options$reported_effect_size

    args$random_effects <- self$options$random_effects



    # Do analysis, then post any notes that have emerged
    estimate <- try(do.call(what = call, args = args))
    estimate$raw_data$label <- as.character(estimate$raw_data$label)
    if (!is.null(self$options$moderator)) {
        estimate$raw_data$moderator <- as.character(estimate$raw_data$moderator)
    }

    if (!is(estimate, "try-error")) {
        if (length(estimate$warnings) > 0) {
            notes <- c(notes, estimate$warnings)
        }
    }

    self$results$help$setState(notes)

    return(estimate)
}
