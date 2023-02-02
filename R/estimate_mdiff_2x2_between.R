#' Estimate the magnitude of difference for an independent groups contrast.
#'
#' @description
#' \loadmathjax
#' `estimate_mdiff_2x2_between` returns point estimate and
#' confidence intervals for a 2x2 fully between-subjects design
#'
#'
#' @param data For raw data - a dataframe or tibble
#' @param outcome_variable For raw data - The column name of the outcome
#'   variable, or a vector of numeric data
#' @param grouping_variable_A For raw data - The column name of the grouping
#'   variable, or a vector of group names, only 2 levels allowed
#' @param grouping_variable_B For raw data - The column name of the grouping
#'   variable, or a vector of group names, only 2 levels allowed
#' @param means For summary data - A vector of 4 means: A1B1, A1B2, A2B1, A2B2
#' @param sds For summary data - A vector of 4 standard deviations, same order
#' @param ns For summary data - A vector of 4 sample sizes
#' @param grouping_variable_A_levels For summary data - An optional vector of
#'   2 group labels
#' @param grouping_variable_B_levels For summary data - An optional vector of
#'   2 group labels
#' @param outcome_variable_name Optional friendly name for the outcome variable.
#'   Defaults to 'My outcome variable' or the outcome variable column name if a
#'   data frame is passed.
#' @param grouping_variable_A_name Optional friendly name for the grouping
#'   variable.  Defaults to 'A' or the grouping variable
#'   column name if a data.frame is passed.
#' @param grouping_variable_B_name Optional friendly name for the grouping
#'   variable.  Defaults to 'A' or the grouping variable
#'   column name if a data.frame is passed.
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#' @param assume_equal_variance Defaults to FALSE
#' @param save_raw_data For raw data; defaults to TRUE; set to FALSE to save
#'   memory by not returning raw data in estimate object
#'
#'
#' @return Returnsobject of class esci_estimate
#'
#'
#' @examples
#' # From Raw Data ------------------------------------
#' # Just pass in the data source, grouping column, and outcome column.
#' # You can pass these in by position, skipping the labels:
#'
#' # Note... not sure if PlantGrowth dataset meets assumptions for this analysis
#' estimate_mdiff_2x2_between(
#'
#' )
#'
#' @export
estimate_mdiff_2x2_between <- function(
  data = NULL,
  outcome_variable = NULL,
  grouping_variable_A = NULL,
  grouping_variable_B = NULL,
  means = NULL,
  sds = NULL,
  ns = NULL,
  grouping_variable_A_levels = c("A1", "A2"),
  grouping_variable_B_levels = c("B1", "B2"),
  outcome_variable_name = "My outcome variable",
  grouping_variable_A_name = "A",
  grouping_variable_B_name = "A",
  conf_level = 0.95,
  assume_equal_variance = FALSE,
  save_raw_data = TRUE
) {

  analysis_type <- "Undefined"


  # Check to see if summary data has been passed
  if (!is.null(means)) {

    # Summary data is passed, so check to make sure raw data not included
    if(!is.null(data))  stop(
      "You have passed summary statistics,
      so don't pass the 'data' parameter used for raw data.")
    if(!is.null(grouping_variable_A)) stop(
      "You have passed summary statistics,
      so don't pass the 'grouping_variable_A' parameter used for raw data.")
    if(!is.null(grouping_variable_B)) stop(
      "You have passed summary statistics,
      so don't pass the 'grouping_variable_B' parameter used for raw data.")
    if(!is.null(outcome_variable)) stop(
      "You have passed summary statistics,
      so don't pass the 'outcome_variable' parameter used for raw data.")

    # Looks good, we can pass on to summary data
    analysis_type <- "summary"

  } else {
    # Raw data has been passed, first sure summary data is not passed
    if(!is.null(means))  stop(
      "You have passed raw data,
      so don't pass the 'means' parameter used for summary data.")
    if(!is.null(sds))  stop(
      "You have passed raw data,
      so don't pass the 'sds' parameter used for summary data.")
    if(!is.null(ns))  stop(
      "You have passed raw data,
      so don't pass the 'ns' parameter used for summary data.")
    if(!is.null(grouping_variable_A_levels))  stop(
      "You have passed raw data,
      so don't pass the 'grouping_variable_A_levels' parameter used for summary data.")
    if(!is.null(grouping_variable_B_levels))  stop(
      "You have passed raw data,
      so don't pass the 'grouping_variable_B_levels' parameter used for summary data.")

    # Check grouping_variable -- if it is an unquoted column name
    #  turn it into a string and store back to grouping_variable
    is_column_name <- try(grouping_variable_A, silent = TRUE)
    if(class(is_column_name) == "try-error") {
      grouping_variable_A_enquo <- rlang::enquo(grouping_variable_A)
      grouping_variable_A_enquo_name <- try(
        eval(rlang::as_name(grouping_variable_A_enquo)), silent = TRUE
      )
      if (class(grouping_variable_A_enquo_name) != "try-error") {
        # This only succeeds if the columns were passed unquoted
        # So now replace grouping_variable with a quoted version
        grouping_variable_A <- grouping_variable_A_enquo_name
      }
    }


    is_column_name <- try(grouping_variable_B, silent = TRUE)
    if(class(is_column_name) == "try-error") {
      grouping_variable_B_enquo <- rlang::enquo(grouping_variable_B)
      grouping_variable_B_enquo_name <- try(
        eval(rlang::as_name(grouping_variable_B_enquo)), silent = TRUE
      )
      if (class(grouping_variable_B_enquo_name) != "try-error") {
        # This only succeeds if the columns were passed unquoted
        # So now replace grouping_variable with a quoted version
        grouping_variable_B <- grouping_variable_B_enquo_name
      }
    }

    # Now we have to figure out what type of raw data:
    #   could be tidy column names, string column names, or vectors
    # We check to see if we have a tidy column name by trying to evaluate it
    is_column_name <- try(outcome_variable, silent = TRUE)
    if(class(is_column_name) == "try-error") {
      # Column names have been passed, check if need to be quoted up
      outcome_variable_enquo <- rlang::enquo(outcome_variable)
      outcome_variable_quoname <- try(
        eval(rlang::as_name(outcome_variable_enquo)), silent = TRUE
      )
      if (class(outcome_variable_quoname) != "try-error") {
        # This only succeeds if outcome_variable was passed unquoted
        # Reset outcome_variable to be fully quoted
        outcome_variable <- outcome_variable_quoname
      }

      # Ready to be analyzed as a list of string column names
      analysis_type <- "data.frame"

    } else if (class(outcome_variable) == "numeric") {
      # At this stage, we know that y was not a tidy column name,
      #  so it should be either a vector of raw data (class = numeric)
      #  or a vector of column names passed as strings
      analysis_type <- "vector"
    } else if (class(outcome_variable) == "character") {
      # Ok, must have been string column names
      if (length(outcome_variable) == 1) {
        analysis_type <- "data.frame"
      } else {
        analysis_type <- "jamovi"
      }
    }
  }

  # At this point, we've figured out the type of data passed
  #  so we can dispatch

  # I put all the dispatches here, at the end, to make it easier to
  #   update in case the underlying function parameters change

  if(analysis_type == "data.frame") {
    return(
      estimate_mdiff_2x2_between.data.frame(
        data = data,
        outcome_variable = outcome_variable,
        grouping_variable_A = grouping_variable_A,
        grouping_variable_B = grouping_variable_B,
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance,
        save_raw_data = save_raw_data
      )
    )
  } else if (analysis_type == "summary") {
    return(
      estimate_mdiff_2x2_between.summary(
        means = means,
        sds = sds,
        ns = ns,
        grouping_variable_A_levels = grouping_variable_A_levels,
        grouping_variable_B_levels = grouping_variable_B_levels,
        outcome_variable_name = outcome_variable_name,
        grouping_variable_A_name = grouping_variable_A_name,
        grouping_variable_B_name = grouping_variable_B_name,
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance
      )
    )
  } else if (analysis_type == "vector") {
    if (is.null(grouping_variable_A_name) | grouping_variable_A_name == "A") {
      grouping_variable_A_name <-  deparse(substitute(grouping_variable_A))
    }
    if (is.null(grouping_variable_B_name) | grouping_variable_B_name == "B") {
      grouping_variable_B_name <-  deparse(substitute(grouping_variable_B))
    }
    if (outcome_variable_name == "My outcome variable") {
      outcome_variable_name <- deparse(substitute(outcome_variable))
    }

    return(
      estimate_mdiff_2x2_between.vector(
        grouping_variable_A = grouping_variable_A,
        grouping_variable_B = grouping_variable_B,
        outcome_variable = outcome_variable,
        outcome_variable_name = outcome_variable_name,
        grouping_variable_A_name = grouping_variable_B_name,
        grouping_variable_A_name = grouping_variable_B_name,
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance,
        save_raw_data = save_raw_data
        )
    )
  }

  stop("Something went wrong dispatching this function")

}


# Handles construction of the effect_sizes and standardized_effect_sizes tables
estimate_mdiff_2x2_between.base <- function(
  overview_table,
  grouping_variable_A_levels,
  grouping_variable_B_levels,
  grouping_variable_A_name,
  grouping_variable_B_name,
  outcome_variable_name,
  conf_level = 0.95,
  assume_equal_variance = FALSE
) {

  # To do
  # Check lengths of outcome and grouping variable names

  # Input checks -------------------------
  # This is the base function for generating an estimated contrast
  # It expects:
  # grouping_variable_name - non-zero length character
  # outcome_variable_name - non-zero length character
  # conf_level should be a numeric value > 0 and <1
  # assume_equal_variance should be a logical value, TRUE or FALSE
  # contrast shold be a vector of numerics
  #   If named vector, names must match grouping_variable_levels
  #   Otherwise, vector must be same length as means

  # Should already be checked
  # means, sds, and ns should already be checked to be numerics
  #  of same length without NAs and with all ns > 1
  # grouping_variable_levels should be already be checked to be a vector
  #  of characters without NAs of same length as means

  means <- overview_table$mean
  sds <- overview_table$sd
  ns <- overview_table$n


  # Check variable names
  esci_assert_type(grouping_variable_A_name, "is.character")
  esci_assert_type(grouping_variable_B_name, "is.character")
  esci_assert_type(outcome_variable_name, "is.character")

  # Check conf.level
  esci_assert_type(conf_level, "is.numeric")
  esci_assert_range(
    conf_level,
    lower = 0,
    upper = 1,
    lower_inclusive = FALSE,
    upper_inclusive = FALSE
  )

  # Check assume_equal_variance
  esci_assert_type(assume_equal_variance, "is.logical")


  # Prep the contrast ---------------------
  all_contrasts <- list(
    main_effect_A = c(-1/2, -1/2, 1/2, 1/2),
    main_effect_B = c(-1/2, 1/2, -1/2, 1/2),
    simple_effect_B_at_A1 = c(-1, 1, 0, 0),
    simple_effect_B_at_A2 = c(0, 0, -1, 1),
    interaction = c(1, -1, -1, 1)
  )

  all_grouping_variable_names <- c(
    grouping_variable_A_name,
    grouping_variable_B_name,
    grouping_variable_B_name,
    grouping_variable_B_name,
    "Interaction"
  )

  all_contrast_labels <-list(
    c(grouping_variable_A_levels[[2]], grouping_variable_A_levels[[1]], "Difference"),
    c(grouping_variable_B_levels[[2]], grouping_variable_B_levels[[1]], "Difference"),
    c(grouping_variable_B_levels[[2]], grouping_variable_B_levels[[1]], "Difference"),
    c(grouping_variable_B_levels[[2]], grouping_variable_B_levels[[1]], "Difference"),
    c(
      paste("Simple effect of", grouping_variable_B_name, "at", grouping_variable_A_levels[[1]]),
      paste("Simple effect of", grouping_variable_B_name, "at", grouping_variable_A_levels[[2]]),
      paste("Difference in simple effects")
    )
  )

  names(means) <- c(
    paste(grouping_variable_A_levels[1], grouping_variable_B_levels, sep = " - "),
    paste(grouping_variable_A_levels[2], grouping_variable_B_levels, sep = " - ")
  )


  all_estimates <- list()

  for (x in 1:5) {
    contrast <- all_contrasts[[x]]
    grouping_variable_name <- all_grouping_variable_names[[x]]
    weights <- esci_tool_contrast_fixed(contrast, means)
    contrast_labels <- all_contrast_labels[[x]]

    # We'll estimate the comparison subset, reference subset, and the difference
    if (names(all_contrasts)[[x]] == "interaction") {
      contrasts <- list(
        comparison = all_contrasts$simple_effect_B_at_A2,
        reference = all_contrasts$simple_effect_B_at_A1,
        difference = all_contrasts$interaction
      )
      names(contrasts$comparison) <- names(means)
      names(contrasts$reference) <- names(means)
      names(contrasts$difference) <- names(means)
    } else {
      contrasts <- list(
        comparison = weights,
        reference = weights,
        difference = weights
      )
      # Filter to create comparison and reference only subsets
      contrasts$comparison[which(contrasts$comparison < 0)] <- 0
      contrasts$reference[which(contrasts$reference > 0)] <- 0
      contrasts$reference <- abs(contrasts$reference)
    }


    # Prepare esci_estimate object that will be returned-------------------------
    estimate <- list()
    estimate$properties <- list(
      outcome_variable_name = outcome_variable_name,
      grouping_variable_A_name = grouping_variable_A_name,
      grouping_variable_B_name = grouping_variable_B_name,
      contrast = weights,
      conf_level = conf_level
    )

    estimate$es_mean_difference_properties <- list(
      effect_size_name = "M_Diff",
      effect_size_name_html = "<i>M</i><sub>diff</sub>",
      effect_size_category = "difference",
      effect_size_precision = "magnitude",
      conf_level = conf_level,
      assume_equal_variance = assume_equal_variance,
      error_distribution = "t_dist"
    )
    class(estimate) <- "esci_estimate"


    # Now dispatch the contrast ----------------------------------------------
    es_mean_difference <- NULL
    es_median_difference <- NULL

    for (mycontrast in contrasts) {
      res <- as.data.frame(
        statpsych::ci.lc.mean.bs(
          alpha = 1 - conf_level,
          m = means,
          sd = sds,
          n = ns,
          v = mycontrast
        )
      )

      res <- res[if (assume_equal_variance) 1 else 2, ]

      res_2a <- as.data.frame(
        statpsych::ci.lc.mean.bs(
          alpha = (1 - conf_level)*2,
          m = means,
          sd = sds,
          n = ns,
          v = mycontrast
        )
      )

      res$ta_LL <- res_2a[if (assume_equal_variance) 1 else 2, "LL"]
      res$ta_UL <- res_2a[if (assume_equal_variance) 1 else 2, "UL"]

      es_mean_difference <- rbind(
        es_mean_difference,
        res
      )

      if (!is.null(overview_table$median)) {
        mdn_res <- statpsych::ci.lc.median.bs(
              alpha = 1 - conf_level,
              m = overview_table$median,
              se = overview_table$median_SE,
              v = mycontrast
        )

        mdn_2a <- statpsych::ci.lc.median.bs(
          alpha = (1 - conf_level)*2,
          m = overview_table$median,
          se = overview_table$median_SE,
          v = mycontrast
        )

        mdn_df <- as.data.frame(mdn_res)

        mdn_df$ta_LL <- mdn_2a[1, "LL"]
        mdn_df$ta_UL <- mdn_2a[1, "UL"]


        es_median_difference <- rbind(
          es_median_difference,
          mdn_df
        )

      } # end of processing median

    } # end of loop through 3 effect sizes


    estimate$es_mean_difference <- data.frame(matrix(NA, ncol=1, nrow=3))[-1]
    estimate$es_mean_difference[ , c("effect_size", "LL", "UL", "SE", "df", "ta_LL", "ta_UL")] <-
      es_mean_difference[ , c("Estimate", "LL", "UL", "SE", "df", "ta_LL", "ta_UL")]

    estimate$es_mean_difference <- cbind(
      type = c("Comparison", "Reference", "Difference"),
      outcome_variable_name = outcome_variable_name,
      grouping_variable_name = grouping_variable_name,
      effect = contrast_labels,
      estimate$es_mean_difference
    )

    # Median difference
    if (!is.null(es_median_difference)) {
      estimate$es_median_difference <- data.frame(matrix(NA, ncol=1, nrow=3))[-1]
      estimate$es_median_difference[ , c("effect_size", "LL", "UL", "SE", "ta_LL", "ta_UL")] <-
        es_median_difference[ , c("Estimate", "LL", "UL", "SE", "ta_LL", "ta_UL")]

      estimate$es_median_difference <- cbind(
        type = c("Comparison", "Reference", "Difference"),
        outcome_variable_name = outcome_variable_name,
        grouping_variable_name = grouping_variable_name,
        effect = contrast_labels,
        estimate$es_median_difference
      )

      estimate$es_median_difference_properties <- list(
        effect_size_name = "Mdn_Diff",
        effect_size_name_html = "<i>Mdn</i><sub>diff</sub>",
        effect_size_category = "difference",
        effect_size_precision = "magnitude",
        conf_level = conf_level,
        error_distribution = "norm"
      )
    }


    # SMD ----------------------------------------------------------
    smd_result <- CI_smd_ind_contrast(
      means = means,
      sds = sds,
      ns = ns,
      contrast = weights,
      conf_level = conf_level,
      assume_equal_variance = assume_equal_variance,
      correct_bias = (assume_equal_variance | length(means) == 2)
    )

    estimate$es_smd_properties <- smd_result$properties
    smd_result$properties <- NULL
    smd_result <- list(list(effect = contrast_labels[[3]]), smd_result)

    estimate$es_smd <- as.data.frame(smd_result)

    estimate$es_smd <- cbind(
      outcome_variable_name = outcome_variable_name,
      grouping_variable_name = grouping_variable_name,
      estimate$es_smd
    )

    all_estimates[[names(all_contrasts)[[x]]]] <- estimate

  }


  return(all_estimates)

}


estimate_mdiff_2x2_between.summary <- function(
  means,
  sds,
  ns,
  grouping_variable_A_levels = NULL,
  grouping_variable_B_levels = NULL,
  grouping_variable_A_name = "A",
  grouping_variable_B_name = "B",
  outcome_variable_name = "My outcome variable",
  conf_level = 0.95,
  assume_equal_variance = FALSE
){


  # Input checks      ---------------------------------------------------------
  # This function expects:
  # means - vector of numeric data with 4 elements, no NAs
  # sds  - vector of numeric data of same length as means, no NAs
  # ns - vector of integers >= 2, of same length as means, no NAs
  # grouping_variable_A_levels -
  #   if passed: vector of 2 characters
  #   if not passed: auto-generated (A1, A2) and warning issued
  # grouping_variable_B_levels -
  #   if passed: vector of 2 characters
  #   if not passed: auto-generated (B1, B2) and warning issued

  # The base function will check:
  #  The validity of the contrast
  #  conf_level is >0 and <1
  #  assume_equal_variance is logical
  #  grouping_variable_name - optional, non-zero length character
  #  outcome_variable_name - optional, non-zero length character


  warnings <- c(NULL)

  # Check means
  esci_assert_type(means, "is.numeric")
  row_report <- esci_assert_vector_valid_length(
    means,
    lower = 4,
    lower_inclusive = TRUE,
    upper = 4,
    upper_inclusive = TRUE,
    na.invalid = TRUE
  )
  cells <- row_report$total

  # Check sds
  esci_assert_type(sds, "is.numeric")
  row_report <- esci_assert_vector_valid_length(
    sds,
    lower = cells,
    upper = cells,
    lower_inclusive = TRUE,
    upper_inclusive = TRUE,
    na.invalid = TRUE
  )

  # Check ns
  esci_assert_type(ns, "is.numeric")
  row_report <- esci_assert_vector_valid_length(
    ns,
    lower = cells,
    upper = cells,
    lower_inclusive = TRUE,
    upper_inclusive = TRUE,
    na.invalid = TRUE
  )
  for (n_value in ns) {
    esci_assert_type(n_value, "is.numeric")
    esci_assert_type(n_value, "is.whole.number")
    esci_assert_range(n_value,
                      lower = 2,
                      lower_inclusive = TRUE)
  }

  # Set A labels
  if(!is.null(grouping_variable_A_levels)) {
    esci_assert_type(grouping_variable_A_levels, "is.character")
    row_report <- esci_assert_vector_valid_length(
      grouping_variable_A_levels,
      lower = 2,
      upper = 2,
      lower_inclusive = TRUE,
      upper_inclusive = TRUE,
      na.rm = FALSE
    )
  } else {
    grouping_variable_A_levels <- c("A1", "A2")
    glc <- paste(
      grouping_variable_A_levels,
      collapse = ", "
    )
    msg <- glue::glue("Labels for grouping_variable_A have been auto-generated: {glc}")
    warnings <- c(warnings, msg)
    warning(msg)
  }


  # Set B labels
  if(!is.null(grouping_variable_B_levels)) {
    esci_assert_type(grouping_variable_B_levels, "is.character")
    row_report <- esci_assert_vector_valid_length(
      grouping_variable_B_levels,
      lower = 2,
      upper = 2,
      lower_inclusive = TRUE,
      upper_inclusive = TRUE,
      na.rm = FALSE
    )
  } else {
    grouping_variable_B_levels <- c("B1", "B2")
    glc <- paste(
      grouping_variable_B_levels,
      collapse = ", "
    )
    msg <- glue::glue("Labels for grouping_variable_A have been auto-generated: {glc}")
    warnings <- c(warnings, msg)
    warning(msg)
  }

  olevels <- c(
    paste(grouping_variable_A_levels[1], grouping_variable_B_levels, sep = " - "),
    paste(grouping_variable_A_levels[2], grouping_variable_B_levels, sep = " - ")
  )

  # Overview table-------------------------------------------------------------
  overview <- overview.summary(
    means = means,
    sds = sds,
    ns = ns,
    grouping_variable_levels = olevels,
    grouping_variable_name = grouping_variable_A_name,
    outcome_variable_name = outcome_variable_name,
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance
  )

  #
  # estimate <- estimate_mdiff_ind_contrast.base(
  #   overview_table = overview,
  #   grouping_variable_levels = grouping_variable_levels,
  #   grouping_variable_name = grouping_variable_name,
  #   outcome_variable_name = outcome_variable_name,
  #   contrast = contrast,
  #   conf_level = conf_level,
  #   assume_equal_variance = assume_equal_variance
  # )

  estimate <- estimate_mdiff_2x2_between.base(
    overview_table = overview,
    grouping_variable_A_levels = grouping_variable_A_levels,
    grouping_variable_B_levels = grouping_variable_B_levels,
    grouping_variable_A_name = grouping_variable_A_name,
    grouping_variable_B_name = grouping_variable_B_name,
    outcome_variable_name = outcome_variable_name,
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance
  )

  for (x in 1:length(estimate)) {
    estimate[[x]]$overview <- overview
    estimate[[x]]$properties$data_type <- "summary"
    estimate[[x]]$properties$data_source <- NULL
  }

  estimate$warnings <- c(estimate$warnings, warnings)

  return(estimate)

}


estimate_mdiff_2x2_between.vector <- function(
  grouping_variable,
  outcome_variable,
  contrast = NULL,
  grouping_variable_name = "My grouping variable",
  outcome_variable_name = "My outcome variable",
  conf_level = 0.95,
  assume_equal_variance = FALSE,
  save_raw_data = TRUE
) {

  # To do - improve checking on levels of grouping variable and
  #  rows per level of outcome variable

  # Input checks --------------------------------------------------------------
  # This function expects:
  #   grouping_variable to be a factor:
  #      with >= 2 valid levels
  #  outcome_variable to be a vector of numeric data:
  #      with > 2 valid rows
  #      of same length as x
  #  save_raw_data is a logical, TRUE or FALSE

  # Check grouping variable
  esci_assert_type(grouping_variable, "is.factor")
  grouping_variable_report <- esci_assert_vector_valid_length(
    grouping_variable,
    lower = 2,
    lower_inclusive = FALSE)
  if (length(levels(as.factor(grouping_variable))) < 2) {
    stop("Not enough levels in grouping_variable")
  }

  # Check outcome variable
  esci_assert_type(outcome_variable, "is.numeric")
  if(length(grouping_variable) != length(outcome_variable)) {
    # vectors not of same length!
    msg <- glue::glue("
The grouping_variable and outcome_variable are not the same length
The grouping_variable length is {length(grouping_variable)};
The outcome_variable length is {length(outcome_variable)}.
    ")
    stop(msg)
  }

  # Check save_raw_data
  esci_assert_type(save_raw_data, "is.logical")


  # Do the analysis --------------------------------------------------
  # Create overview -- which will gracefully deal with missing and n=0 or n=1
  all_overview <- overview.vector(
    grouping_variable = grouping_variable,
    outcome_variable = outcome_variable,
    outcome_variable_name = outcome_variable_name,
    grouping_variable_name = grouping_variable_name,
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance
  )

  # From the overview function, get just the valid groups
  no_miss_overview <- all_overview[row.names(all_overview) != "missing", ]
  overview <- no_miss_overview[no_miss_overview$n > 1, ]

  # If no contrast, job is done
  if (is.null(contrast)) {
    estimate <- list()
    estimate$properties <- list(
      outcome_variable_name = outcome_variable_name,
      grouping_variable_name = grouping_variable_name,
      effect_size_category = "Simple",
      effect_size_name = "M",
      conf_level = conf_level,
      assume_equal_variance = assume_equal_variance
    )
    estimate$overview <- all_overview
    class(estimate) <- "esci_estimate"

    # Store raw data -----------------------------------------------
    if (save_raw_data) {
      # Revise all NAs
      if (row.names(overview)[nrow(overview)] == "missing") {
        # na_level was generated in overview to be unique
        na_level <- overview[nrow(overview), "group"]
        levels(grouping_variable) <- c(levels(grouping_variable), na_level)
        grouping_variable[which(is.na(grouping_variable))] <- na_level
      }

      estimate$raw_data <- data.frame(
        grouping_variable = grouping_variable,
        outcome_variable = outcome_variable
      )
    }


    return(estimate)
  }


  # Check the validity of the contrast ---------------------------------------
  ns <- no_miss_overview$n
  groups <- no_miss_overview$grouping_variable_level

  valid_groups <- groups[which(ns > 1)]
  invalid_groups <- groups[which(ns < 2)]
  invalid_ns <- ns[which(ns < 2)]

  if (is.null(names(contrast))) {
    if (length(valid_groups) != length(contrast)) {
      msg <- glue::glue("
Invalid contrast specified.
Contrast length is {length(contrast)}.
But number of valid groups is {length(valid_groups)}.
The contrast passed is: {paste(contrast, collapse = ', ')}.
The valid groups found are: {paste(valid_groups, collapse = ', ')}
Invalid groups are those with n < 2.
{length(invalid_groups)} invalid groups: {paste(invalid_groups, collapse = ', ')}
      ")
      stop(msg)
    }
  } else {
    if (prod(names(contrast) %in% valid_groups) != 1) {
      msg <- glue::glue("
Invalid contrast specified.
Contrast has named value that is not found in the valid groups.
The contrast passed is: {paste(names(contrast), '=', contrast, collapse = ', ')}.
The valid groups are: {paste(valid_groups, collapse = ', ')}
Invalid groups are those with n < 2.
{length(invalid_groups)} invalid groups: {paste(invalid_groups, collapse = ', ')}
      ")
      stop(msg)
    }
  }


  # Dispatch only valid groups to base function
  estimate <-estimate_mdiff_ind_contrast.base(
    overview_table = overview,
    grouping_variable_levels = overview$grouping_variable_level,
    grouping_variable_name = grouping_variable_name,
    outcome_variable_name = outcome_variable_name,
    contrast = contrast,
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance
  )


  if(length(contrast) == 2) {
    comp_level <- names(contrast[contrast > 0])
    ref_level <- names(contrast[contrast < 0])

    if(length(comp_level) > 0 & length(ref_level) > 0) {
      vec_comparison <- outcome_variable[which(grouping_variable == comp_level)]
      vec_reference <- outcome_variable[which(grouping_variable == ref_level)]
      vec_comparison <- vec_comparison[!is.na(vec_comparison)]
      vec_reference <- vec_reference[!is.na(vec_reference)]

      if(length(vec_comparison) > 0 & length(vec_reference) > 0) {
        estimate$es_mean_ratio <- as.data.frame(
            statpsych::ci.ratio.mean2(
              alpha = 1 - conf_level,
              y1 = vec_comparison,
              y2 = vec_reference
            )
        )

        estimate$es_mean_ratio_properties <- list(
          message_html = "
          For more information on this effect size, see Bonett & Price (2020) doi: 10.3102/1076998620934125."
        )


        estimate$es_median_ratio <- as.data.frame(
          statpsych::ci.ratio.median2(
            alpha = 1 - conf_level,
            y1 = vec_comparison,
            y2 = vec_reference
          )
        )

        estimate$es_median_ratio_properties <- list(
          message_html = "
          For more information on this effect size, see Bonett & Price (2020) doi: 10.3102/1076998620934125."
        )


        estimate$es_mean_ratio <- estimate$es_mean_ratio[ , c(3, 4, 5, 1, 2)]
        colnames(estimate$es_mean_ratio) <- c("effect_size", "LL", "UL", "comparison_mean", "reference_mean")

        estimate$es_median_ratio <- estimate$es_median_ratio[ , c(3, 4, 5, 1, 2)]
        colnames(estimate$es_median_ratio) <- c("effect_size", "LL", "UL", "comparison_median", "reference_median")

        clabel <- paste(
          comp_level,
          "/",
          ref_level
        )

        estimate$es_mean_ratio <- cbind(
          outcome_variable_name = outcome_variable_name,
          grouping_variable_name = grouping_variable_name,
          effect = clabel,
          estimate$es_mean_ratio
        )

        estimate$es_median_ratio <- cbind(
          outcome_variable_name = outcome_variable_name,
          grouping_variable_name = grouping_variable_name,
          effect = clabel,
          estimate$es_median_ratio
        )

      }
    }
  }

  estimate$overview <- all_overview
  estimate$properties$data_type <- "vector"
  estimate$properties$data_source <- NULL


  # Raise warnings if needed ------------------------------------------------
  # Warning if all_overview has a row for missing grouping data
  if ("missing" %in% row.names(all_overview)) {
    n_miss <- all_overview[row.names(all_overview) == "missing", "n"]
    n_rows <- all_overview[row.names(all_overview) == "missing", "missing"]
    if (n_miss > 0 & n_rows > 0) {
      msg <-  glue::glue("
There are {n_rows} rows missing the outcome variable and grouping variable and {n_miss} rows missing only the grouping variable ({grouping_variable_name}).
Outcomes with missing grouping variables were **dropped* from the analysis
    ")
      estimate$warnings <- c(estimate$warnings, msg)
      warning(msg)
    }
    if (n_miss > 0 & n_rows == 0) {
      msg <-  glue::glue("
There are {n_miss} missing values in grouping variable {grouping_variable_name}.
Outcomes with missing grouping variables were **dropped* from the analysis
    ")
      estimate$warnings <- c(estimate$warnings, msg)
      warning(msg)
    }
    if (n_rows > 0 & n_miss == 0) {
      msg <-  glue::glue("
There are {n_rows} rows that were missing values for both the grouping variable ({grouping_variable_name}) and the outcome variable.
    ")
      estimate$warnings <- c(estimate$warnings, msg)
      warning(msg)
    }
  }

  if(length(invalid_groups) > 0) {
    for (x in 1:length(invalid_groups)) {
      msg <-  glue::glue("
  The group {invalid_groups[[x]]} had an invalid sample size: {invalid_ns[[x]]}.
  This group was **dropped* from the analysis.
        ")
      estimate$warnings <- c(estimate$warnings, msg)
      warning(msg)

    }
  }

  # Store raw data -----------------------------------------------
  if (save_raw_data) {
    # Revise all NAs
    if (row.names(overview)[nrow(overview)] == "missing") {
      # na_level was generated in overview to be unique
      na_level <- overview[nrow(overview), "group"]
      levels(grouping_variable) <- c(levels(grouping_variable), na_level)
      grouping_variable[which(is.na(grouping_variable))] <- na_level
    }

    estimate$raw_data <- data.frame(
      grouping_variable = grouping_variable,
      outcome_variable = outcome_variable
    )
  }

  return(estimate)
}


estimate_mdiff_2x2_between.data.frame <- function(
  data,
  grouping_variable,
  outcome_variable,
  contrast = NULL,
  conf_level = 0.95,
  assume_equal_variance = FALSE,
  save_raw_data = TRUE
) {


  # Input Checks -------------------------------------------------------------
  # This function expects:
  #   data to be a data frame
  #   grouping_variable to be a factor with more than 2 valid rows
  #   outcome_variable to be a numeric column in data, with more than 2 rows
  esci_assert_type(data, "is.data.frame")
  esci_assert_valid_column_name(data, grouping_variable)
  esci_assert_column_type(data, grouping_variable, "is.factor")
  esci_assert_column_has_valid_rows(
    data,
    grouping_variable,
    lower = 2,
    na.rm = TRUE
  )

  # Validate this outcome variable
  esci_assert_valid_column_name(data, outcome_variable)
  esci_assert_column_type(data, outcome_variable, "is.numeric")
  esci_assert_column_has_valid_rows(
    data,
    outcome_variable,
    lower = 2,
    na.rm = TRUE
  )

  # Now pass along to the .vector version of this function
  estimate <- estimate_mdiff_ind_contrast.vector(
    grouping_variable = data[[grouping_variable]],
    outcome_variable = data[[outcome_variable]],
    grouping_variable_name = grouping_variable,
    outcome_variable_name = outcome_variable,
    contrast = contrast,
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance,
    save_raw_data = save_raw_data
  )

  estimate$properties$data_type <- "data.frame"
  estimate$properties$data_source <- deparse(substitute(data))

  return(estimate)

}

