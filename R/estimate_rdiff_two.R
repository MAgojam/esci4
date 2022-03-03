#' Estimate a correlation.
#'
#' @description
#' Returns effect sizes appropriate for estimating the linear relationship
#' between two quantitative variables
#'
#' @param data For raw data - a dataframe or tibble
#' @param x For raw data - The column name of the outcome
#'   variable, or a vector of numeric data
#' @param y For raw data - The column name of the outcome
#'   variable, or a vector of numeric data
#' @param grouping_variable For raw data, a vector that is a factor or the
#'   name of a factor column from data
#' @param comparison_r For summary data, a pearson's r correlation coefficient
#' @param comparison_n For summary data - An integer > 0
#' @param reference_r For summary data, a pearson's r correlation coefficient
#' @param reference_n For summary data - An integer > 0
#' @param grouping_variable_levels For summary data - An optional vector of
#'   2 group labels
#' @param x_variable_name Optional friendly name for the x variable.
#'   Defaults to 'My x variable' or the outcome variable column name if a
#'   data frame is passed.
#' @param y_variable_name Optional friendly name for the y variable.
#'   Defaults to 'My y variable' or the outcome variable column name if a
#'   data frame is passed.
#' @param grouping_variable_name Optional friendly name for the grouping
#'   variable.  Defaults to 'My grouping variable' or the grouping variable
#'   column name if a data.frame is passed.
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
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
#' estimate_correlation(
#'  datasets::anscombe,
#'  x1,
#'  y1
#' )
#'
#' @export
estimate_rdiff_two <- function(
  data = NULL,
  x = NULL,
  y = NULL,
  grouping_variable = NULL,
  comparison_r = NULL,
  comparison_n = NULL,
  reference_r = NULL,
  reference_n = NULL,
  grouping_variable_levels = NULL,
  x_variable_name = "My x variable",
  y_variable_name = "My y variable",
  grouping_variable_name = "My grouping variable",
  conf_level = 0.95,
  save_raw_data = TRUE
) {

  analysis_type <- "Undefined"


  # Check to see if summary data has been passed
  if (!is.null(comparison_r)) {

    # Summary data is passed, so check to make sure raw data not included
    if(!is.null(data))  stop(
      "You have passed summary statistics,
      so don't pass the 'data' parameter used for raw data.")
    if(!is.null(x)) stop(
      "You have passed summary statistics,
      so don't pass the 'x' parameter used for raw data.")
    if(!is.null(y)) stop(
      "You have passed summary statistics,
      so don't pass the 'y' parameter used for raw data.")
    if(!is.null(grouping_variable)) stop(
      "You have passed summary statistics,
      so don't pass the 'grouping_variable' parameter used for raw data.")

    # Looks good, we can pass on to summary data
    analysis_type <- "summary"

  } else {
    # Raw data has been passed, first sure summary data is not passed
    if(!is.null(comparison_r))  stop(
      "You have passed raw data,
      so don't pass the 'comparison_r' parameter used for summary data.")
    if(!is.null(comparison_n))  stop(
      "You have passed raw data,
      so don't pass the 'comparison_n' parameter used for summary data.")
    if(!is.null(reference_r))  stop(
      "You have passed raw data,
      so don't pass the 'comparison_r' parameter used for summary data.")
    if(!is.null(reference_n))  stop(
      "You have passed raw data,
      so don't pass the 'comparison_n' parameter used for summary data.")

    if (is.null(data)) {
      analysis_type <- "vector"
    } else {

      # Check x_variable -- if it is an unquoted column name
      #  turn it into a string and store back to x
      is_char <- try(
        is.character(x), silent = TRUE
      )
      if (class(is_char) == "try-error") {
        # If not a character, must have been quoted
        x_enquo <- rlang::enquo(x)
        x_quoname <- try(
          eval(rlang::as_name(x_enquo)), silent = TRUE
        )
        if (class(x_quoname) != "try-error") {
          # This only succeeds if outcome_variable was passed unquoted
          # Reset outcome_variable to be fully quoted
          x <- x_quoname
          x_variable_name <- x
        } else {
          stop("Could not parse x")
        }
      }


      is_char <- try(
        is.character(y), silent = TRUE
      )
      if (class(is_char) == "try-error") {
        # If not a character, must have been quoted
        y_enquo <- rlang::enquo(y)
        y_quoname <- try(
          eval(rlang::as_name(y_enquo)), silent = TRUE
        )
        if (class(y_quoname) != "try-error") {
          # This only succeeds if y was passed unquoted
          # Reset y to be fully quoted
          y <- y_quoname
          y_variable_name <- y
        } else {
          stop("Could not parse y")
        }
      }

      is_char <- try(
        is.character(grouping_variable), silent = TRUE
      )
      if (class(is_char) == "try-error") {
        # If not a character, must have been quoted
        grouping_variable_enquo <- rlang::enquo(grouping_variable)
        grouping_variable_quoname <- try(
          eval(rlang::as_name(grouping_variable_enquo)), silent = TRUE
        )
        if (class(grouping_variable_quoname) != "try-error") {
          # This only succeeds if outcome_variable was passed unquoted
          # Reset outcome_variable to be fully quoted
          grouping_variable <- grouping_variable_quoname
          grouping_variable_name <- grouping_variable
        } else {
          stop("Could not parse grouping_variable")
        }
      }


      analysis_type <- "data.frame"
    }

  }

  # At this point, we've figured out the type of data passed
  #  so we can dispatch

  # I put all the dispatches here, at the end, to make it easier to
  #   update in case the underlying function parameters change

  if(analysis_type == "data.frame") {
    return(
      estimate_rdiff_two.data.frame(
        data = data,
        x = x,
        y = y,
        grouping_variable = grouping_variable,
        y_variable_name = y_variable_name,
        x_variable_name = x_variable_name,
        grouping_variable_name = grouping_variable_name,
        conf_level = conf_level,
        save_raw_data = save_raw_data
      )
    )
  } else if (analysis_type == "summary") {
    return(
      estimate_rdiff_two.summary(
        comparison_r = comparison_r,
        comparison_n = comparison_n,
        reference_r = reference_r,
        reference_n = reference_n,
        conf_level = conf_level,
        grouping_variable_levels = grouping_variable_levels,
        x_variable_name = x_variable_name,
        y_variable_name = y_variable_name,
        grouping_variable_name = grouping_variable_name
      )
    )
  } else if (analysis_type == "vector") {
    if (is.null(x_variable_name) | x_variable_name == "My x variable") {
      x_variable_name <- deparse(substitute(x))
    }
    if (is.null(y_variable_name) | y_variable_name == "My y variable") {
      y_variable_name <- deparse(substitute(y))
    }
    if (is.null(grouping_variable_name) | grouping_variable_name == "My grouping variable") {
      grouping_variable_name <- deparse(substitute(grouping_variable))
    }
    return(
      estimate_rdiff_two.vector(
        x = x,
        y = y,
        grouping_variable = grouping_variable,
        grouping_variable_name = grouping_variable_name,
        x_variable_name = x_variable_name,
        y_variable_name = y_variable_name,
        conf_level = conf_level,
        save_raw_data = save_raw_data
        )
    )
  }

  stop("Something went wrong dispatching this function")

}



estimate_rdiff_two.summary <- function(
  comparison_r,
  comparison_n,
  reference_r,
  reference_n,
  grouping_variable_levels,
  x_variable_name = "My x variable",
  y_variable_name = "My y variable",
  grouping_variable_name = "My grouping variable",
  conf_level = 0.95
){


  # Input checks      ---------------------------------------------------------
  # This function expects:
  # comparison_r and reference_r - numeric data between -1 and 1
  # comparison_n and reference_n - numeric integer > 1
  # conf_level is >0 and <1
  # x_variable_name - optional, non-zero length character
  # y_variable_name - optional, non-zero length character
  # grouping_variable_name - optional, non-zero length character

  # Check comparison_r
  esci_assert_type(comparison_r, "is.numeric")
  esci_assert_range(
    comparison_r,
    lower = -1,
    lower_inclusive = TRUE,
    upper = 1,
    upper_inclusive = TRUE
  )
  # Check reference_r
  esci_assert_type(reference_r, "is.numeric")
  esci_assert_range(
    reference_r,
    lower = -1,
    lower_inclusive = TRUE,
    upper = 1,
    upper_inclusive = TRUE
  )
  # Check comparison_n
  esci_assert_type(comparison_n, "is.numeric")
  esci_assert_type(comparison_n, "is.whole.number")
  esci_assert_range(
    comparison_n,
    lower = 2,
    lower_inclusive = TRUE
  )
  # Check reference_n
  esci_assert_type(reference_n, "is.numeric")
  esci_assert_type(reference_n, "is.whole.number")
  esci_assert_range(
    comparison_n,
    lower = 2,
    lower_inclusive = TRUE
  )
  # Set group labels
  if(!is.null(grouping_variable_levels)) {
    esci_assert_type(grouping_variable_levels, "is.character")
    row_report <- esci_assert_vector_valid_length(
      grouping_variable_levels,
      lower = 2,
      upper = 2,
      lower_inclusive = TRUE,
      upper_inclusive = TRUE,
      na.rm = FALSE
    )
  } else {
    grouping_variable_levels <- paste(
      "Group",
      seq(from = 1, to = 2, by = 1),
      sep = ""
    )
    glc <- paste(
      grouping_variable_levels,
      collapse = ", "
    )
    msg <- glue::glue("Group labels have been auto-generated: {glc}")
    warnings <- c(warnings, msg)
    warning(msg)
  }
  # Check variable names
  esci_assert_type(x_variable_name, "is.character")
  esci_assert_type(y_variable_name, "is.character")
  esci_assert_type(grouping_variable_name, "is.character")
  # Check conf.level
  esci_assert_type(conf_level, "is.numeric")
  esci_assert_range(
    conf_level,
    lower = 0,
    upper = 1,
    lower_inclusive = FALSE,
    upper_inclusive = FALSE
  )


  # Do analysis ------------------------------------
  estimate <- list()
  class(estimate) <- "esci_estimate"
  estimate$es_r <- wrapper_ci.cor2(
    comparison_r = comparison_r,
    comparison_n = comparison_n,
    reference_r = reference_r,
    reference_n = reference_n,
    conf_level = conf_level,
    grouping_variable_levels = grouping_variable_levels,
    x_variable_name = x_variable_name,
    y_variable_name = y_variable_name,
    grouping_variable_name = grouping_variable_name
  )

  estimate$properties <- list(
    data_type = "Summary",
    data_source = NULL,
    conf_level = conf_level
  )

  return(estimate)

}


estimate_rdiff_two.vector <- function(
  x,
  y,
  grouping_variable,
  x_variable_name = "My x variable",
  y_variable_name = "My y variable",
  grouping_variable_name = "My grouping variable",
  conf_level = 0.95,
  save_raw_data = TRUE
) {

  # Input checks --------------------------------------------------------------
  # This function expects:
  #  outcome_variable to be a vector of numeric data:
  #      with > 2 valid rows
  #  save_raw_data is a logical, TRUE or FALSE

  # Check x variable
  esci_assert_type(x, "is.numeric")
  row_report <- esci_assert_vector_valid_length(
    x,
    lower = 2,
    lower_inclusive = FALSE,
    na.invalid = TRUE
  )
  # Check y variable
  esci_assert_type(y, "is.numeric")
  row_report <- esci_assert_vector_valid_length(
    y,
    lower = 2,
    lower_inclusive = FALSE,
    na.invalid = TRUE
  )
  esci_assert_type(grouping_variable, "is.factor")
  row_report <- esci_assert_vector_valid_length(
    grouping_variable,
    lower = 2,
    lower_inclusive = FALSE,
    na.invalid = TRUE
  )

  # Check that x and y and grouping variable are same length
  if (length(x) != length(y)) {
    stop("x and y variable must be same length")
  }
  if (length(x) != length(grouping_variable)) {
    stop("x, y, and grouping_variable must all be the same length")
  }

  mydata <- data.frame(
    x = x,
    y = y,
    grouping_variable = grouping_variable
  )
  colnames(mydata) <- c(
    x_variable_name,
    y_variable_name,
    grouping_variable_name
  )

  estimate <- estimate_correlation.data.frame(
    data = mydata,
    x = x_variable_name,
    y = y_variable_name,
    grouping_variable = grouping_variable_name,
    x_variable_name = x_variable_name,
    y_variable_name = y_variable_name,
    grouping_variable_name = grouping_variable_name,
    conf_level = conf_level,
    save_raw_data = save_raw_data
  )


  estimate$properties$data_type <- "vector"
  estimate$properties$data_source <- NULL

  return(estimate)
}


estimate_rdiff_two.data.frame <- function(
  data,
  x,
  y,
  grouping_variable,
  y_variable_name = "My y variable",
  x_variable_name = "My x variable",
  grouping_variable_name = "My grouping variable",
  conf_level = conf_level,
  save_raw_data = save_raw_data
) {

  estimate <- list()
  class(estimate) <- "esci_estimate"
  estimate$properties <- list(
    data_type = "data.frame",
    data_source = deparse(substitute(data)),
    conf_level = conf_level
  )

  estimate$overview <- rbind(
    overview(
      data = data,
      outcome_variable = x,
      grouping_variable = grouping_variable,
      outcome_variable_name = x_variable_name,
      grouping_variable_name = grouping_variable_name,
      conf_level = conf_level
    ),
    overview(
      data = data,
      outcome_variable = y,
      grouping_variable = grouping_variable,
      outcome_variable_name = y_variable_name,
      grouping_variable_name = grouping_variable_name,
      conf_level = conf_level
    )
  )

  level1 <- levels(data[[grouping_variable]])[[1]]
  level2 <- levels(data[[grouping_variable]])[[2]]

  data1 <- data[data[[grouping_variable]] == level1, ]
  data2 <- data[data[[grouping_variable]] == level2, ]

  comparison_r <- cor(data1[[x]], data1[[y]], use = "pairwise.complete.obs")
  reference_r <- cor(data2[[x]], data2[[y]], use = "pairwise.complete.obs")
  comparison_n <- crossprod(!is.na(data1[ , c(x, y)]))[1, 2]
  reference_n <- crossprod(!is.na(data2[ , c(x, y)]))[1, 2]

  estimate$es_r <- estimate_rdiff_two.summary(
    comparison_r = comparison_r,
    comparison_n = comparison_n,
    reference_r = reference_r,
    reference_n = reference_n,
    grouping_variable_levels = c(level1, level2),
    x_variable_name = x_variable_name,
    y_variable_name = y_variable_name,
    grouping_variable_name = grouping_variable_name,
    conf_level = conf_level
  )$es_r

  if(save_raw_data) {

  }

  return(estimate)

}

