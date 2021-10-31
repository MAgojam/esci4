#' Calculates descriptive statistics for a numerical variable
#'
#'
#' @description
#' This function caluclated basic descriptive statistics for a categorical/
#' nominal variable. Inputs can be summary data, vectors, or a data frame.
#'
#'
#' @param data - for raw data, a data frame or tibble
#' @param outcome_variable - for raw data, either a vector containing factor
#'   data or the name of a data-frame column containing a factor
#' @param counts For summary data - A vector of 1 or more counts, integers,
#'  > 0
#' @param outcome_variable_levels For summary data - An optional vector of
#'   group labels, same length as counts.  If not passed, auto-generated.
#' @param outcome_variable_name Optional friendly name for the outcome variable.
#'   Defaults to 'My Outcome Variable'.  Ignored if a data-frame is passed,
#'   this argument is ignored.
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#'
#'
#' @return Returns a table of descriptive statistics
#'
#'
#' @examples
#' # From Summary data --------------------------------------
#'
#' overview_nominal(
#'   counts = c(10, 15, 100),
#'   outcome_variable_levels = c(
#'      "First_Years", "Second_Years", "Third_Years"
#'   ),
#'   outcome_variable_name = "% time near target",
#'   assume_equal_variance = TRUE
#' )
#'
#'
#' @importFrom stats aggregate median na.omit quantile rnorm sd
#' @importFrom utils head
#' @importFrom rlang enquo as_name abort
#' @importFrom glue glue
#'
#' @export
overview_nominal <- function(
  data = NULL,
  outcome_variable = NULL,
  counts = NULL,
  outcome_variable_levels = NULL,
  outcome_variable_name = "My Outcome Variable",
  conf_level = 0.95,
  assume_equal_variance = FALSE
) {

  analysis_type <- "Undefined"


  # Check to see if summary data has been passed
  if (!is.null(counts)) {

    # Summary data is passed, so check to make sure raw data not included
    if(!is.null(data))  stop(
      "You have passed summary statistics,
      so don't pass the 'data' parameter used for raw data.")
    if(!is.null(outcome_variable)) stop(
      "You have passed summary statistics,
      so don't pass the 'grouping_variable' parameter used for raw data.")

    # Looks good, we can pass on to summary data
    analysis_type <- "summary"

  } else {
    # Raw data has been passed, first sure summary data is not passed
    if(!is.null(counts))  stop(
      "You have passed raw data,
      so don't pass the 'counts' parameter used for summary data.")
    if(!is.null(outcome_variable_levels))  stop(
      "You have passed raw data,
      so don't pass the 'outcome_variable_levels' parameter used for summary data.")

    if (class(outcome_variable) == "factor") {
      if(!is.null(data)) stop(
        "Outcome variable is a factor,
        so don't pass the 'data' parameter used for data.frames."
      )
      analysis_type <- "vector"
    } else {
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

      } else {
        # Ok, must have been string column names
        if (length(outcome_variable) == 1) {
          analysis_type <- "data.frame"
        } else {
          analysis_type <- "jamovi"
        }
      }

    }

  }

  # At this point, we've figured out the type of data passed
  #  so we can dispatch

  # I put all the dispatches here, at the end, to make it easier to
  #   update in case the underlying function parameters change

  if(analysis_type == "data.frame") {
    return(
      overview_nominal.data.frame(
        data = data,
        outcome_variable = make.names(outcome_variable),
        conf_level = conf_level
      )
    )
  } else if (analysis_type == "jamovi") {
    return(
      overview_nominal.jamovi(
        data = data,
        outcome_variables = outcome_variable,
        conf_level = conf_level
      )
    )
  } else if (analysis_type == "summary") {
    return(
      overview_nominal.summary(
        counts = counts,
        outcome_variable_levels = outcome_variable_levels,
        outcome_variable_name = outcome_variable_name,
        conf_level = conf_level
      )
    )
  } else if (analysis_type == "vector") {
    if (outcome_variable_name == "My Outcome Variable") {
      outcome_variable_name <- deparse(substitute(outcome_variable))
    }
    return(
      overview_nominal.vector(
        outcome_variable = outcome_variable,
        outcome_variable_name = outcome_variable_name,
        conf_level = conf_level
      )
    )
  }

  stop("Something went wrong dispatching this function")

}



# Takes an overview table and fills in mean CI, df, se, and MoE
# If assume_equal_variance, uses a pooled sd and total df for estimating means
# Otherwise, just uses group sd and df for estimating each group mean
# Uses CI_mdiff_contrast_bs as the base function for calculating CIs
overview_nominal.base <- function(
  overview_table,
  conf_level = 0.95
) {

  # Input checks -------------------------
  # This is the base function for generating an estimated contrast
  # It expects:
  # overview_table - has more than 1 row
  # conf_level should be a numeric value > 0 and <1

  # Check conf.level
  esci_assert_type(conf_level, "is.numeric")
  esci_assert_range(
    conf_level,
    lower = 0,
    upper = 1,
    lower_inclusive = FALSE,
    upper_inclusive = FALSE
  )


  # Prep --------------------------------------------
  n_cells <- nrow(overview_table)

  statpsych_row <- 1
  statpsych_cnames <- c("Estimate", "LL", "UL", "SE")
  esci_column_names <- c("P", "P_LL", "P_UL", "P_SE")


  # Analysis ----------------------------------------
  for (x in 1:n_cells) {
    res <- as.data.frame(
      statpsych::ci.prop1(
        alpha = 1 - conf_level,
        f = overview_table$count[x],
        n = overview_table$n[x]
      )
    )

    res$Estimate[1] <- res$Estimate[2]

    overview_table[x, esci_column_names] <- res[statpsych_row, statpsych_cnames]

  }

  return(overview_table)
}


# Produces an overview table from summary data
overview_nominal.summary <- function(
  counts,
  outcome_variable_levels = NULL,
  outcome_variable_name = "My Outcome Variable",
  conf_level = 0.95
) {

  # Input checks      ---------------------------------------------------------
  # This function expects:
  # counts - vector of 2 or more positive integers, no NAs
  # outcome_variable_name -
  # outcome_variable_levels -
  #   if passed: vector of characters same length of counts
  #   if not passed: auto-generated (Group1, Group2, etc.) and warning issued

  # The base function will check:
  #  conf_level is >0 and <1

  # Check counts
  esci_assert_type(counts, "is.numeric")
  row_report <- esci_assert_vector_valid_length(
    counts,
    lower = 2,
    lower_inclusive = TRUE,
    na.invalid = TRUE
  )
  for (count in counts) {
    esci_assert_type(count, "is.whole.number")
    esci_assert_range(
      count,
      lower = 0,
      lower_inclusive = TRUE
    )
  }
  cells <- row_report$total


  # Check outcome_variable_name
  esci_assert_type(outcome_variable_name, "is.character")
  outcome_variable_names <- rep(outcome_variable_name, cells)

  # Check outcome_variable_levels
  if(!is.null(outcome_variable_levels)) {
    esci_assert_type(outcome_variable_levels, "is.character")
    row_report <- esci_assert_vector_valid_length(
      outcome_variable_levels,
      lower = cells,
      upper = cells,
      lower_inclusive = TRUE,
      upper_inclusive = TRUE,
      na.rm = FALSE
    )
    levels_passed <- TRUE
  } else {
    outcome_variable_levels <- paste("Level", c(1:cells), sep = "")
    levels_passed <- FALSE
  }


  overview_table <- data.frame(
    outcome_variable_name = outcome_variable_names,
    outcome_variable_level = outcome_variable_levels,
    count = counts,
    n = sum(counts),
    P = NA,
    P_LL = NA,
    P_UL = NA,
    P_SE = NA
  )


  return(
    overview_nominal.base(
      overview_table = overview_table,
      conf_level = conf_level
    )
  )
}


# Produces an overview table from vector data
overview_nominal.vector <- function(
  outcome_variable,
  outcome_variable_name = NULL,
  conf_level = 0.95
) {


  # Input checks ----------------------------
  if(is.null(outcome_variable_name)) {
    outcome_variable_name <- deparse(substitute(outcome_variable))
  } else {
    # Check that it is a character
    esci_assert_type(outcome_variable_name, "is.character")
  }


  # Setup   ---------------------
  groups <- addNA(levels(as.factor(outcome_variable)))
  outcome_variable_names <- rep(outcome_variable_name, length(groups)+1)


  # Build the overview table ---------------------------------------------------
  overview_table <- data.frame(
    outcome_variable_name = outcome_variable_names,
    outcome_variable_levels = c(groups, "Missing")
  )

  overview_table$counts <- aggregate(
    outcome_variable,
    by = list(addNA(outcome_variable)),
    drop = FALSE,
    FUN = length)[, 2]

  overview_table$n <- sum(overview_table$counts, na.omit(TRUE))

  overview_table$P <- NA
  overview_table$P_LL <- NA
  overview_table$P_UL <- NA
  overview_table$P_z <- NA


  # Cleanup - Deal with invalid rows and missing data rows-----------------
  overview_table <- overview_nominal.base(
    overview_table = overview_table,
    conf_level = conf_level
  )

  return(overview_table)

}

# Overview from a data frame
overview.data.frame <- function(
  data,
  outcome_variable,
  grouping_variable = NULL,
  conf_level = 0.95,
  assume_equal_variance = TRUE
) {

  # Input Checks -------------------------------------------------------------
  # This function expects:
  #   data to be a data frame
  #   grouping_variable to be null a factor with more than 2 valid rows
  #   outcome_variable to be a numeric column in data, with more than 2 rows
  esci_assert_type(data, "is.data.frame")

  if (!is.null(grouping_variable)) {
    esci_assert_valid_column_name(data, grouping_variable)
    esci_assert_column_type(data, grouping_variable, "is.factor")
    esci_assert_column_has_valid_rows(
      data,
      grouping_variable,
      lower = 2,
      na.rm = TRUE
    )
  }

  # Validate this outcome variable
  esci_assert_valid_column_name(data, outcome_variable)
  esci_assert_column_type(data, outcome_variable, "is.numeric")
  esci_assert_column_has_valid_rows(
    data,
    outcome_variable,
    lower = 2,
    na.rm = TRUE
  )

  if (!is.null(grouping_variable)) {
    grouping_variable_name <- grouping_variable
    grouping_variable <- data[[grouping_variable]]
  } else {
    grouping_variable_name <- NULL
  }

  # Now pass along to the .vector version of this function
  overview_table <- overview.vector(
    outcome_variable = data[[outcome_variable]],
    grouping_variable = grouping_variable,
    outcome_variable_name = outcome_variable,
    grouping_variable_name = grouping_variable_name,
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance
  )

  return(overview_table)
}


# Overview from a data frame with a list of outcome variables
overview.jamovi <- function(
  data,
  outcome_variables,
  grouping_variable = NULL,
  conf_level = 0.95,
  assume_equal_variance = TRUE
) {

  res <- NULL

  # Cycle through the list of columns;
  #  for each call estimate_mean_one.character, which handles 1 column
  for (outcome_variable in outcome_variables) {

    # Now pass along to the data_frame version
    res <- rbind(
      res,
      overview.data.frame(
        data = data,
        outcome_variable = outcome_variable,
        grouping_variable = grouping_variable,
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance
      )
    )

  } # Finish cycle through variables

  return(res)
}


wrap_ci_median1 <- function(
  x,
  conf_level,
  na.rm = TRUE,
  drop = FALSE
) {

  if(length(x) < 2) return(c(median(x), NA, NA, NA))

  res <- statpsych::ci.median1(
    alpha = 1 - conf_level,
    y = if(na.rm) x[!is.na(x)] else x
  )

  return(res)
}


wrap_ci_mean1 <- function(
  x,
  conf_level,
  na.rm = TRUE,
  drop = FALSE
) {

  if(length(x) < 2) return(c(mean(x), NA, NA, NA))

  res <- statpsych::ci.mean1(
    alpha = 1 - conf_level,
    m = mean(x, na.rm = na.rm),
    sd = sd(x, na.rm = na.rm),
    n = if(na.rm) length(x[!is.na(x)]) else length(x)
  )

  return(res)
}
