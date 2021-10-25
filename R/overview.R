# Takes an overview table and fills in mean CI, df, se, and MoE
# If assume_equal_variance, uses a pooled sd and total df for estimating means
# Otherwise, just uses group sd and df for estimating each group mean
# Uses CI_mdiff_contrast_bs as the base function for calculating CIs


overview.base <- function(
  overview_table,
  conf_level = 0.95,
  assume_equal_variance = FALSE
) {

  # Input checks -------------------------
  # This is the base function for generating an estimated contrast
  # It expects:
  # overview_table - has more than 1 row
  # conf_level should be a numeric value > 0 and <1
  # assume_equal_variance should be a logical value, TRUE or FALSE

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


  # Prep --------------------------------------------
  n_means <- nrow(overview_table)
  contrasts <- matrix(data = 0, nrow = n_means, ncol = n_means)
  diag(contrasts) <- 1

  statpsych_row <- if (assume_equal_variance | n_means == 1) 1 else 2
  statpsych_cnames <- c("LL", "UL", "df", "SE")
  esci_column_names <- c("mean_LL", "mean_UL", "df", "mean_SE")


  # Analysis ----------------------------------------
    if (n_means == 1) {
    res <- as.data.frame(
        statpsych::ci.mean1(
        alpha = 1 - conf_level,
        m = overview_table$mean,
        sd = overview_table$sd,
        n = overview_table$n
      )
    )

    res$df <- overview_table$n - 1

    overview_table[1, esci_column_names] <- res[1, statpsych_cnames]

  } else {

    for (x in 1:n_means) {
      res <- statpsych::ci.lc.mean.bs(
        alpha = 1 - conf_level,
        m = overview_table$mean,
        s = overview_table$sd,
        n = overview_table$n,
        v = contrasts[x,]
      )

      overview_table[x, esci_column_names] <- res[statpsych_row, statpsych_cnames]
    }

  }

  return(overview_table)
}


# Produces an overview table from summary data
overview.summary <- function(
  means,
  sds,
  ns,
  outcome_variable_name = "My Outcome Variable",
  grouping_variable_levels = NULL,
  conf_level = 0.95,
  assume_equal_variance = FALSE
) {

  # Input checks      ---------------------------------------------------------
  # This function expects:
  # means - vector of numeric data with >2 elements, no NAs
  # sds  - vector of numeric data >0 of same length as means, no NAs
  # ns - vector of integers >= 2, of same length as means, no NAs
  # outcome_variable_name -
  # grouping_variable_levels -
  #   if passed: vector of characters same length of means
  #   if not passed: auto-generated (Group1, Group2, etc.) and warning issued

  # The base function will check:
  #  conf_level is >0 and <1
  #  assume_equal_variance is logical

  # Check means
  esci_assert_type(means, "is.numeric")
  row_report <- esci_assert_vector_valid_length(
    means,
    lower = 1,
    lower_inclusive = TRUE,
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
  for (sd_value in sds) {
    esci_assert_type(sd_value, "is.numeric")
    esci_assert_range(sd_value,
                      lower = 0,
                      lower_inclusive = TRUE)
  }

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

  # Check outcome_variable_names
  esci_assert_type(outcome_variable_name, "is.character")
  outcome_variable_names <- rep(outcome_variable_name, length(means))

  # Check grouping_variable_levels
  if(!is.null(grouping_variable_levels)) {
    esci_assert_type(grouping_variable_levels, "is.character")
    row_report <- esci_assert_vector_valid_length(
      grouping_variable_levels,
      lower = length(means),
      upper = length(means),
      lower_inclusive = TRUE,
      upper_inclusive = TRUE,
      na.rm = FALSE
    )
  } else {
    grouping_variable_levels <- rep("All", length(means))
  }

  overview_table <- data.frame(
    outcome_variable_name = outcome_variable_names,
    grouping_variable_level = grouping_variable_levels,
    mean = means,
    mean_LL = NA,
    mean_UL = NA,
    sd = sds,
    n = ns,
    df = NA,
    mean_se = NA
  )

  return(
    overview.base(
      overview_table = overview_table,
      conf_level = conf_level,
      assume_equal_variance = assume_equal_variance
    )
  )
}


# Produces an overview table from vector data
overview.vector <- function(
  outcome_variable,
  grouping_variable = NULL,
  outcome_variable_name = NULL,
  conf_level = 0.95,
  assume_equal_variance = TRUE
) {


  # Input checks ----------------------------
  if(is.null(outcome_variable_name)) {
    outcome_variable_name <- deparse(substitute(outcome_variable))
  } else {
    # Check that it is a character
    esci_assert_type(outcome_variable_name, "is.character")
  }


  # If no grouping variable, just summarize whole vector
  if (is.null(grouping_variable)) {
    grouping_variable <- rep("All", length(outcome_variable))
  }

  # Deal with NA values in grouping variable------------------------------------
  grouping_variable_report <- esci_assert_vector_valid_length(
    grouping_variable,
    lower = 2,
    lower_inclusive = FALSE)
  grouping_variable_all <-grouping_variable_report$total
  grouping_variable_valid <- grouping_variable_report$valid
  grouping_variable_missing <- grouping_variable_report$missing


  # Setup   ---------------------
  groups <- levels(addNA(grouping_variable))
  outcome_variable_names <- rep(outcome_variable_name, length(groups))


  # Build the overview table ---------------------------------------------------
  overview_table <- data.frame(
    outcome_variable_name = outcome_variable_names,
    grouping_variable_level = groups
  )

  overview_table[ , c("mean", "mean_SE_temp", "mean_LL", "mean_UL")] <- aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    FUN = wrap_ci_mean1,
    drop = FALSE,
    conf_level = conf_level,
    na.rm = TRUE
  )[, 2]

  # overview_table$mean <- aggregate(
  #   outcome_variable,
  #   by = list(addNA(grouping_variable)),
  #   FUN = mean,
  #   drop = FALSE,
  #   na.rm = TRUE)[, 2]
  #
  # overview_table$mean_LL <- NA
  #
  # overview_table$mean_UL <- NA

  overview_table[ , c("median", "median_SE_temp", "median_LL", "median_UL")] <- aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    FUN = wrap_ci_median1,
    drop = FALSE,
    conf_level = conf_level,
    na.rm = TRUE
  )[, 2]

  overview_table$sd <- aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    FUN = sd,
    drop = FALSE,
    na.rm = TRUE)[, 2]

  overview_table$min <- aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    FUN = min,
    drop = FALSE,
    na.rm = TRUE)[, 2]

  overview_table$max <- aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    FUN = max,
    drop = FALSE,
    na.rm = TRUE)[, 2]

  overview_table[ , c("q1", "q3")] <-aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    FUN = quantile,
    drop = FALSE,
    probs = c(0.25, 0.75),
    na.rm = TRUE)[, 2]

  overview_table$n <- aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    drop = FALSE,
    FUN = length)[, 2]

  overview_table$missing <-aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    drop = FALSE,
    function(x) { sum(is.na(x)) })[, 2]

  overview_table$n <- overview_table$n - overview_table$missing

  overview_table$df <- NA

  overview_table$mean_SE <- overview_table$mean_SE_temp
  overview_table$mean_SE_temp <- NULL

  overview_table$median_SE <- overview_table$median_SE_temp
  overview_table$median_SE_temp <- NULL


  # Cleanup - Deal with invalid rows and missing data rows-----------------

  if (nrow(overview_table[is.na(overview_table$n), ]) > 0) {
    overview_table[is.na(overview_table$n), ]$n <- 0
  }

  na_level <- NULL
  if(overview_table[nrow(overview_table), "n"] == 0) {
    overview_table <- head(overview_table, -1)
  } else {
    na_level <- "Missing"
    while (na_level %in% overview_table$group) {
      na_level <- paste(na_level, "*", sep ="")
    }
    overview_table[nrow(overview_table), "grouping_variable_level"] <- na_level
    row.names(overview_table)[nrow(overview_table)] <- "missing"
  }

  overview_no_miss <- overview_table[row.names(overview_table) != 'missing', ]
  overview_valid <- overview_no_miss[overview_no_miss$n > 1, ]

  overview_valid <- overview.base(
    overview_table = overview_valid,
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance
  )

  overview_all <- rbind(
    overview_valid,
    overview_table[row.names(overview_table) != "missing" & overview_table$n < 2, ],
    overview_table[row.names(overview_table) == "missing", ]
  )

  return(overview_all)

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
    grouping_variable <- data[[grouping_variable]]
  }

  # Now pass along to the .vector version of this function
  overview_table <- overview.vector(
    outcome_variable = data[[outcome_variable]],
    grouping_variable = grouping_variable,
    outcome_variable_name = outcome_variable,
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
