# Takes an overview table and fills in mean CI, df, se, and MoE
# If assume_equal_variance, uses a pooled sd and total df for estimating means
# Otherwise, just uses group sd and df for estimating each group mean
# Uses CI_mdiff_contrast_bs as the base function for calculating CIs


overview.base <- function(
  overview_table,
  conf_level = 0.95,
  assume_equal_variance = FALSE
) {


  n_means <- nrow(overview_table)
  contrasts <- matrix(data = 0, nrow = n_means, ncol = n_means)
  diag(contrasts) <- 1

  statpsych_row_number <- if(assume_equal_variance) 1 else 2
  statpsych_column_names <- c("LL", "UL", "df", "SE")
  esci_column_names <- c("mean_LL", "mean_UL", "df", "mean_SE")

for (x in 1:n_means) {
    res <- statpsych::ci.lc.mean.bs(
      alpha = 1 - conf_level,
      m = overview_table$mean,
      s = overview_table$sd,
      n = overview_table$n,
      v = contrasts[x,]
    )

    overview_table[x, esci_column_names] <- res[statpsych_row_number, statpsych_column_names]

  }

  return(overview_table)
}

# Produces an overview table from summary data
overview.summary <- function(
  means,
  sds,
  ns,
  outcome_variable_names = NULL,
  grouping_variable_levels = NULL,
  conf_level = 0.95,
  assume_equal_variance = FALSE
) {

  # Check outcome_variable_names
  if(!is.null(outcome_variable_names)) {
    esci_assert_type(outcome_variable_names, "is.character")
    row_report <- esci_assert_vector_valid_length(
      outcome_variable_names,
      lower = length(means),
      upper = length(means),
      lower_inclusive = TRUE,
      upper_inclusive = TRUE,
      na.rm = FALSE
    )
  } else {
    outcome_variable_names <- paste("Variable", seq(1:length(means)))
  }

  # Check grouping_variable_levels
  if(!is.null(grouping_variable_levels)) {
    esci_assert_type(group_labels, "is.character")
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
  conf_level = 0.95,
  assume_equal_variance = TRUE
) {

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


  # Calculate overview ---------------------------------------------------------
  groups <- levels(addNA(grouping_variable))

  overview <- overview.table_definition(rows = length(groups))
  overview$group = groups
  overview$m <- aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    FUN = mean,
    drop = FALSE,
    na.rm = TRUE)[, 2]
  overview[ , c("median", "q1", "q3")] <-aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    FUN = quantile,
    drop = FALSE,
    probs = c(0.50, 0.25, 0.75),
    na.rm = TRUE)[, 2]
  overview$s <- aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    FUN = sd,
    drop = FALSE,
    na.rm = TRUE)[, 2]
  overview$min <- aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    FUN = min,
    drop = FALSE,
    na.rm = TRUE)[, 2]
  overview$max <- aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    FUN = max,
    drop = FALSE,
    na.rm = TRUE)[, 2]
  overview$n <- aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    drop = FALSE,
    FUN = length)[, 2]
  overview$missing <-aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    drop = FALSE,
    function(x) { sum(is.na(x)) })[, 2]
  overview$n <- overview$n - overview$missing

  if (nrow(overview[is.na(overview$n), ]) > 0) {
    overview[is.na(overview$n), ]$n <- 0
  }

  na_level <- NULL
  if(overview[nrow(overview), "n"] == 0) {
    overview <- head(overview, -1)
  } else {
    na_level <- "Missing"
    while (na_level %in% overview$group) {
      na_level <- paste(na_level, "*", sep ="")
    }
    overview[nrow(overview), "group"] <- na_level
    row.names(overview)[nrow(overview)] <- "missing"
  }

  overview_no_miss <- overview[row.names(overview) != 'missing', ]
  overview_valid <- overview_no_miss[overview_no_miss$n > 1, ]

  overview_valid <- overview.base(
    overview_table = overview_valid,
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance
  )

  overview_all <- rbind(
    overview_valid,
    overview[row.names(overview) != "missing" & overview$n < 2, ],
    overview[row.names(overview) == "missing", ]
  )

  return(overview_all)

}

overview.list <- function(
  data,
  grouping_variable = NULL,
  outcome_variable,
  conf_level = 0.95,
  assume_equal_variance = FALSE
) {

  res <- NULL

  # Cycle through the list of columns;
  #  for each call estimate_mean_one.character, which handles 1 column
  for (outcome in outcome_variable) {

    outcome_name <- rlang::as_name(outcome)

    # Now pass along to the .vector version of this function
    myres <- overview.vector(
      grouping_variable = data[[grouping_variable]],
      outcome_variable = data[[outcome_name]],
      conf_level = conf_level,
      assume_equal_variance = assume_equal_variance
    )

    res <- rbind(res, cbind(outcome_variable_name = outcome_name, myres))

  } # Finish cycle through variables

  return(res)
}


# Base definition for the overview table
overview.table_definition <- function(rows = 1) {
  overview <- data.frame(
    outcome_variable_name = rep(NA, times = rows),
    grouping_variable_level = rep(NA, times = rows),
    mean = rep(NA, times = rows),
    mean_LL = rep(NA, times = rows),
    mean_UL = rep(NA, times = rows),
    median = rep(NA, times = rows),
    median_LL = rep(NA, times = rows),
    median_UL = rep(NA, times = rows),
    sd = rep(NA, times = rows),
    min = rep(NA, times = rows),
    max = rep(NA, times = rows),
    q1 = rep(NA, times = rows),
    q3 = rep(NA, times = rows),
    n = rep(NA, times = rows),
    missing = rep(NA, times = rows),
    df = rep(NA, times = rows),
    sp = rep(NA, times = rows),
    mean_se = rep(NA, times = rows),
    median_se = rep(NA, times = rows),
  )
  return(overview)
}
