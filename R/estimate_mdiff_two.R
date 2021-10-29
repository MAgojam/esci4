#' Estimate magnitude of difference between two independent groups.
#'
#' @description
#' \loadmathjax
#' `estimate_mdiff_two` returns effect sizes estimating the magnitude of
#' difference between two groups along a quantitative variable.
#'
#'
#' @param data For raw data - a dataframe or tibble
#' @param outcome_variable For raw data - The column name of the outcome
#'   variable, or a vector of numeric data
#' @param grouping_variable For raw data - The column name of the grouping
#'   variable, or a vector of group names
#' @param comparison_mean For summary data, a numeric
#' @param comparison_sd For summary data, numeric > 0
#' @param comparison_n For summary data, a numeric integer > 0
#' @param reference_mean For summary data, a numeric
#' @param reference_sd For summary data, numeric > 0
#' @param reference_n For summary data, a numeric integer > 0
#' @param grouping_variable_levels For summary data - An optional vector of
#'   2 group labels
#' @param outcome_variable_name Optional friendly name for the outcome variable.
#'   Defaults to 'My outcome variable' or the outcome variable column name if a
#'   data frame is passed.
#' @param grouping_variable_name Optional friendly name for the grouping
#'   variable.  Defaults to 'My grouping variable' or the grouping variable
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
#' estimate_mdiff_two(
#'  PlantGrowth[PlantGrowth$group != 'trt2', ],
#'  weight,
#'  group
#' )
#'
#' @export
estimate_mdiff_two <- function(
  data = NULL,
  outcome_variable = NULL,
  grouping_variable = NULL,
  comparison_mean = NULL,
  comparison_sd = NULL,
  comparison_n = NULL,
  reference_mean = NULL,
  reference_sd = NULL,
  reference_n = NULL,
  grouping_variable_levels = NULL,
  grouping_variable_name = "My grouping variable",
  outcome_variable_name = "My outcome variable",
  conf_level = 0.95,
  assume_equal_variance = FALSE,
  save_raw_data = TRUE
) {

  analysis_type <- "Undefined"

  # Check to see if summary data has been passed
  if (
    !is.null(comparison_mean) |
    !is.null(comparison_sd) |
    !is.null(comparison_n) |
    !is.null(reference_mean) |
    !is.null(reference_sd) |
    !is.null(reference_n)
    ) {

    # Summary data is passed, so check to make sure raw data not included
    if(!is.null(data))  stop(
      "You have passed summary statistics,
      so don't pass the 'data' parameter used for raw data.")
    if(!is.null(grouping_variable)) stop(
      "You have passed summary statistics,
      so don't pass the 'grouping_variable' parameter used for raw data.")
    if(!is.null(outcome_variable)) stop(
      "You have passed summary statistics,
      so don't pass the 'outcome_variable' parameter used for raw data.")

    # Looks good, we can pass on to summary data

    return(
      estimate_mdiff_ind_contrast(
        means = c(comparison_mean, reference_mean),
        sds = c(comparison_sd, reference_sd),
        ns = c(comparison_n, reference_n),
        grouping_variable_levels = grouping_variable_levels,
        grouping_variable_name = grouping_variable_name,
        outcome_variable_name = outcome_variable_name,
        contrast = c(1, -1),
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance
      )
    )

  } else {
    # Raw data has been passed, first sure summary data is not passed
    if(!is.null(comparison_mean))  stop(
      "You have passed raw data,
      so don't pass the 'comparison_mean' parameter used for summary data.")
    if(!is.null(comparison_sd))  stop(
      "You have passed raw data,
      so don't pass the 'comparison_sd' parameter used for summary data.")
    if(!is.null(comparison_n))  stop(
      "You have passed raw data,
      so don't pass the 'comparison_n' parameter used for summary data.")
    if(!is.null(reference_mean))  stop(
      "You have passed raw data,
      so don't pass the 'reference_mean' parameter used for summary data.")
    if(!is.null(reference_sd))  stop(
      "You have passed raw data,
      so don't pass the 'reference_sd' parameter used for summary data.")
    if(!is.null(reference_n))  stop(
      "You have passed raw data,
      so don't pass the 'reference_n' parameter used for summary data.")
    if(!is.null(grouping_variable_levels))  stop(
      "You have passed raw data,
      so don't pass the 'grouping_variable_levels' parameter used for summary data.")

    # Check grouping_variable -- if it is an unquoted column name
    #  turn it into a string and store back to grouping_variable
    is_column_name <- try(grouping_variable, silent = TRUE)
    if(class(is_column_name) == "try-error") {
      grouping_variable_enquo <- rlang::enquo(grouping_variable)
      grouping_variable_enquo_name <- try(
        eval(rlang::as_name(grouping_variable_enquo)), silent = TRUE
      )
      if (class(grouping_variable_enquo_name) != "try-error") {
        # This only succeeds if the columns were passed unquoted
        # So now replace grouping_variable with a quoted version
        grouping_variable <- grouping_variable_enquo_name
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

    if (analysis_type == "vector") {
      grouping_variable_levels <- levels(as.factor(grouping_variable))
    } else {
      grouping_variable_levels <- levels(as.factor(data[[grouping_variable]]))
    }
    if (length(grouping_variable_levels) < 2) {
      stop(
        paste(
          "Not enough levels in grouping_variable; 2 required; only",
          length(grouping_variable_levels),
          "found in the grouping_variable",
          grouping_variable_name,
          sep = " "
        )
      )
    }

    contrast <- c(1, -1)
    names(contrast) <- grouping_variable_levels[1:2]

    estimate <- estimate_mdiff_ind_contrast(
        data = data,
        outcome_variable = outcome_variable,
        grouping_variable = grouping_variable,
        outcome_variable_name = outcome_variable_name,
        grouping_variable_name = grouping_variable_name,
        contrast = contrast,
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance,
        save_raw_data = save_raw_data
    )
    if (length(grouping_variable_levels) > 2) {
      estimate$warnings <- c(
        estimate$warnings,
        paste(
          "The grouping variable (",
          grouping_variable_name,
          ") had",
          length(grouping_variable_levels),
          "levels.  Only the first 2 levels were used for effect-size calculations.",
          sep = " "
        )
      )
    }
    return(estimate)

  }


  stop("Something went wrong dispatching this function")

}


