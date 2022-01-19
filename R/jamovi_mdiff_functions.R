jamovi_mdiff_initialize <- function(self, grouping_variable = TRUE) {

  # Set some variables for convenience -----------------------
  #   Is analysis from summary data or raw?
  #   Are we evaluating a hypothesis?
  #   Is this a contrast?
  from_raw <- (self$options$switch == "from_raw")

  # Get a handle for each table
  tbl_overview <- NULL
  tbl_es_mean_difference <- NULL
  tbl_es_mean_ratio <- NULL
  tbl_es_smd <- NULL
  tbl_es_median_difference <- NULL
  tbl_es_median_ratio <- NULL
  try(tbl_overview <- self$results$overview)
  try(tbl_es_mean_difference <- self$results$es_mean_difference)
  try(tbl_es_mean_ratio <- self$results$es_mean_ratio)
  try(tbl_es_smd <- self$results$es_smd)
  try(tbl_es_median_difference <- self$results$es_median_difference)
  try(tbl_es_median_ratio <- self$results$es_median_ratio)


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
  jamovi_set_confidence(tbl_es_mean_ratio, conf_level)
  jamovi_set_confidence(tbl_es_median_difference, conf_level)
  jamovi_set_confidence(tbl_es_median_ratio, conf_level)


  # Outcomes: 1 if from summary, length of outcome_variables if raw
  outcome_count <- if(from_raw) {
    length(self$options$outcome_variable)
  } else {
    1
  }

  # For now, only 1 contrast can be specified
  contrast_count <- 1

  # How many levels?
  #  For raw, check grouping_variable
  #  For summary, check group_labels
  if (grouping_variable) {
    if (from_raw) {
      level_source <- self$options$grouping_variable
      level_count <- length(levels(as.factor(self$data[, level_source])))
    } else {
      level_source <- try(level_source <- self$options$grouping_variable_levels)
      if (is(level_source, "try-error")) {
        level_count = 2
      } else {
        level_count <- length(levels(as.factor(self$data[, level_source])))
      }
    }
  } else {
    level_count <- 1
  }

  # Rows needed for each table -------------------------------
  overview_rows <- level_count * outcome_count
  mdiff_rows <- contrast_count * outcome_count * 3
  smd_rows <- contrast_count * outcome_count

  jamovi_init_table(tbl_overview, overview_rows)
  jamovi_init_table(tbl_es_mean_difference, mdiff_rows, breaks = 3)
  jamovi_init_table(tbl_es_smd, smd_rows)
  jamovi_init_table(tbl_es_mean_ratio, smd_rows)
  jamovi_init_table(tbl_es_median_difference, mdiff_rows, breaks = 3)
  jamovi_init_table(tbl_es_median_ratio, smd_rows, breaks = 3)


}




jamovi_mdiff_contrastindependent <- function(
  self,
  outcome_variables = NULL,
  save_raw_data = FALSE
) {
  # This function will build the analysis and then return
  #   - the estimate (class esci_estimate)
  #   - an error (class try-error)
  #   - or NULL (representing analysis not ready)


  # Prelim -----------------------------------------------------
  from_raw <- (self$options$switch == "from_raw")
  notes <- c(NULL)


  # Step 1 - Check if analysis basics are defined ---------------
  #  if not, return NULL
  if(from_raw) {
    if (
      is.null(self$options$grouping_variable) |
      is.null(outcome_variables) |
      length(outcome_variables) == 0
    ) return(NULL)
  } else {
    if(
      is.null(self$options$means) |
      is.null(self$options$sds) |
      is.null(self$options$ns) |
      is.null(self$options$grouping_variable_levels)
    ) return(NULL)
  }

  # Step 2: Check on the contrast --------------------------------
  clabels <- self$options$comparison_labels
  rlabels <- self$options$reference_labels


  if(from_raw) {
    level_source <- self$options$grouping_variable
    valid_levels <- levels(as.factor(self$data[, level_source]))
    multiplier <- length(self$options$outcome_variable)
  } else {
    level_source <- self$options$grouping_variable_levels
    valid_levels <- self$data[
      which(!is.na(self$data[, self$options$grouping_variable_levels])),
      level_source
    ]
    multiplier <- 1
  }


  # This function checks if the contrast is valid or not
  reference_result <- jamovi_check_contrast(
    labels = rlabels,
    valid_levels = valid_levels,
    level_source = level_source,
    group_type = "Reference",
  )

  # Same, but with comparison labels
  comparison_result <- jamovi_check_contrast(
    labels = clabels,
    valid_levels = valid_levels,
    level_source = level_source,
    group_type = "Comparison",
    sequential = !is.null(reference_result$error_string)
  )

  notes <- c(notes,
             reference_result$error_string,
             comparison_result$error_string
  )

  overlap <- reference_result$label %in% comparison_result$label
  if (length(reference_result$label[overlap]) != 0) {
    next_note <- glue::glue(
      "<b>Error</b>: Reference and comparison groups must be distinct, but
{reference_result$label[overlap]} has been entered in both"
    )
    notes <- c(notes, next_note)
  }


  contrast <- if(length(notes) > 0)
    NULL
  else
    jamovi_create_contrast(
      reference_result$label,
      comparison_result$label
    )

  # Step 3: Run analysis ------------------------------------------
  # Fill in analysis properties

  # If from summary:
  # get outcome and grouping variable names
  # and set notes if they have been replaced
  if(!from_raw) {
    outcome_variable_name <- jamovi_sanitize(
      self$options$outcome_variable_name,
      return_value = "My outcome variable",
      na_ok = FALSE
    )
    grouping_variable_name <- jamovi_sanitize(
      self$options$grouping_variable_name,
      return_value = "My grouping variable",
      na_ok = FALSE
    )
    notes <- c(
      notes,
      names(outcome_variable_name),
      names(grouping_variable_name)
    )
  }

  args <- list()
  conf_level <- jamovi_sanitize(
    my_value = self$options$conf_level,
    return_value = 95,
    na_ok = FALSE,
    convert_to_number = TRUE,
    lower = 0,
    lower_inclusive = FALSE,
    upper = 100,
    upper_inclusive = FALSE,
    my_value_name = "Confidence level"
  )
  notes <- c(notes, names(conf_level))
  args$conf_level <- conf_level/100
  args$assume_equal_variance <- self$options$assume_equal_variance
  args$contrast <- contrast

  # Set args for summary and raw data cases
  if (from_raw) {
    # Analysis from raw data
    args$data <- self$data
    args$grouping_variable <- self$options$grouping_variable
    args$outcome_variable <- outcome_variables
    call <- esci4::estimate_mdiff_ind_contrast
  } else {
    # Analysis from summary data
    group_labels <- self$data[, self$options$grouping_variable_levels]
    valid_rows <- which(!is.na(group_labels))

    if(length(valid_rows) != length(group_labels)) {
      msg <- glue::glue("
There are {length(group_labels) - length(valid_rows)} empty values
in {self$options$grouping_variable_levels}.  Rows with empty group labels have been
**dropped** from the analysis
                    ")
      notes <- c(notes, msg)
    }

    args$means <- self$data[valid_rows, self$options$means]
    args$sds <- self$data[valid_rows, self$options$sds]
    args$ns <- self$data[valid_rows, self$options$ns]
    args$grouping_variable_levels <- as.character(group_labels[valid_rows])
    args$outcome_variable_name <- outcome_variable_name
    args$grouping_variable_name <- grouping_variable_name
    call <- esci4::estimate_mdiff_ind_contrast
  }


  # Do analysis, then post any notes that have emerged
  estimate <- try(do.call(what = call, args = args))

  # For summary data, store in a list based on outcome_variable_name
  if (!is(estimate, "try-error")) {
    notes <- c(notes, estimate$warnings)
    self$results$help$setState(notes)
    if(!from_raw) {
      estimate_list <- list()
      key <- outcome_variable_name
      estimate_list[[key]] <- estimate
      class(estimate_list) <- "esci_estimate"
      estimate <- esci_estimate_consolidate(estimate_list)
    }
  }

  return(estimate)
}


jamovi_mdiff_two <- function(self, save_raw_data = FALSE) {
  # Prelim -----------------------------------------------------
  from_raw <- (self$options$switch == "from_raw")
  notes <- c(NULL)


  # Step 1 - Check if analysis basics are defined ---------------
  args <- list()

  if(from_raw) {
    if (
      is.null(self$options$grouping_variable) |
      is.null(self$options$outcome_variable) |
      length(self$options$outcome_variable) == 0
    ) return(NULL)
  } else {
    args$comparison_mean <- jamovi_required_numeric(
        self$options$comparison_mean
    )
    args$comparison_sd <- jamovi_required_numeric(
      self$options$comparison_sd,
      lower = 0,
      lower_inclusive = FALSE
    )
    args$comparison_n <- jamovi_required_numeric(
      self$options$comparison_n,
      integer_required = TRUE,
      lower = 0,
      lower_inclusive = FALSE
    )

    args$reference_mean <- jamovi_required_numeric(
      self$options$reference_mean
    )
    args$reference_sd <- jamovi_required_numeric(
      self$options$reference_sd,
      lower = 0,
      lower_inclusive = FALSE
    )
    args$reference_n <- jamovi_required_numeric(
      self$options$reference_n,
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
  call <- esci4::estimate_mdiff_two

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
  args$assume_equal_variance <- self$options$assume_equal_variance
  args$switch_comparison_order <- self$options$switch_comparison_order


  if(from_raw) {
    args$data <- self$data
    args$outcome_variable <- unname(self$options$outcome_variable)
    args$grouping_variable <- unname(self$options$grouping_variable)
  } else {
    args$outcome_variable_name <- jamovi_sanitize(
      self$options$outcome_variable_name,
      return_value = "My outcome variable",
      na_ok = FALSE
    )
    args$grouping_variable_name <- jamovi_sanitize(
      self$options$grouping_variable_name,
      return_value = "My grouping variable",
      na_ok = FALSE
    )
    comparison_level_name <- jamovi_sanitize(
      self$options$comparison_level_name,
      return_value = "Comparison level",
      na_ok = FALSE
    )
    reference_level_name <- jamovi_sanitize(
      self$options$reference_level_name,
      return_value = "Reference level",
      na_ok = FALSE
    )
    args$grouping_variable_levels <- c(
      comparison_level_name,
      reference_level_name
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
