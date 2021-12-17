#' Estimate meta-analytic difference in magnitude between two ind. groups
#'
#' @description
#' `meta_mdiff_two` returns
#'
#'
#' @param data A dataframe or tibble
#' @param comparison_means comparison
#' @param comparison_sds comparison
#' @param comparison_n comparison
#' @param reference_means reference
#' @param reference_sds reference
#' @param reference_ns reference
#' @param labels labels
#' @param moderator mod
#' @param contrast contrast
#' @param effect_label el
#' @param report_smd smd
#' @param random_effects re
#' @param assume_equal_variance aev
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#'
#' @return Returnsobject of class esci_estimate
#'
#'
#' @export
meta_mdiff_two <- function(
  data,
  comparison_means,
  comparison_sds,
  comparison_ns,
  reference_means,
  reference_sds,
  reference_ns,
  labels = NULL,
  moderator = NULL,
  contrast = NULL,
  effect_label = "My effect",
  report_smd = FALSE,
  random_effects = TRUE,
  assume_equal_variance = FALSE,
  conf_level = .95
)  {

  # Initialization ---------------------------
  # Create quosures and quonames.
  # Stolen directly from dabestr
  comparison_means_enquo        <-  rlang::enquo(comparison_means)
  comparison_means_quoname      <-  rlang::quo_name(comparison_means_enquo)

  reference_means_enquo        <-  rlang::enquo(reference_means)
  reference_means_quoname      <-  rlang::quo_name(reference_means_enquo)

  comparison_sds_enquo        <-  rlang::enquo(comparison_sds)
  comparison_sds_quoname      <-  rlang::quo_name(comparison_sds_enquo)

  reference_sds_enquo        <-  rlang::enquo(reference_sds)
  reference_sds_quoname      <-  rlang::quo_name(reference_sds_enquo)

  comparison_ns_enquo        <-  rlang::enquo(comparison_ns)
  comparison_ns_quoname      <-  rlang::quo_name(comparison_ns_enquo)

  reference_ns_enquo        <-  rlang::enquo(reference_ns)
  reference_ns_quoname      <-  rlang::quo_name(reference_ns_enquo)

  moderator_enquo        <-  rlang::enquo(moderator)
  moderator_quoname      <-  rlang::quo_name(moderator_enquo)
  if (moderator_quoname == "NULL") moderator_quoname <- NULL

  labels_enquo        <-  rlang::enquo(labels)
  labels_quoname      <-  rlang::quo_name(labels_enquo)
  if (labels_quoname == "NULL") labels_quoname <- NULL

  warnings <- NULL

  # Input checks --------------------------------
  # * data must be a data frame
  #    all rows with an NA a parameter column will be dropped, warning issued
  # * the column comparison_means must exist and be numeric,
  #    with > 1 row after NAs removed
  # * the column reference_means must exist and be numeric,
  #    with > 1 row after NAs removed
  # * the column comparison_sds must exist and be numeric > 0
  #    with > 1 row after NAs removed
  # * the column reference_sds must exist and be numeric > 0
  #    with > 1 row after NAs removed
  # * the column comparison_ns must exist and be numeric integers > 0
  #    with > 1 row after NAs removed
  # * the column reference_ns must exist and be numeric integers > 0
  #    with > 1 row after NAs removed
  # * the column labels is optional, but if passed must exist and
  #    have > 1 row after NAs removed
  # * the column moderator is optional; checks happen in meta_any
  # * contrast should only be passed in moderator is defined; checks in meta_any
  # * effect_label should be a character, checked in meta_any
  # * effect_size_name should be a character, checked in meta_any
  # * moderator_variable_name checked in meta_any
  # * random_effect must be a logical, TRUE or FALSE, checked in meta_any
  # * conf_level must be a numeric >0 and < 1, checked in meta_any

  # Check that data is a data.frame
  esci_assert_type(data, "is.data.frame")
  # reference_means
  esci_assert_valid_column_name(data, reference_means_quoname)
  esci_assert_column_type(data, reference_means_quoname, "is.numeric")
  row_report <- esci_assert_column_has_valid_rows(
    data,
    reference_means_quoname,
    lower = 2,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }
  # comparison_means
  esci_assert_valid_column_name(data, comparison_means_quoname)
  esci_assert_column_type(data, comparison_means_quoname, "is.numeric")
  row_report <- esci_assert_column_has_valid_rows(
    data,
    comparison_means_quoname,
    lower = 2,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }
  # reference_sds
  esci_assert_valid_column_name(data, reference_sds_quoname)
  esci_assert_column_type(data, reference_sds_quoname, "is.numeric")
  if (!all(data[[reference_sds_quoname]] > 0, na.rm = TRUE)) {
    stop(
      glue::glue("
Some sd values in {reference_sds_quoname} are 0 or less.
These are rows {which(data[[reference_sds_quoname]] > 0)}.
      ")
    )
  }
  row_report <- esci_assert_column_has_valid_rows(
    data,
    reference_sds_quoname,
    lower = 2,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }
  # comparison_sds
  esci_assert_valid_column_name(data, comparison_sds_quoname)
  esci_assert_column_type(data, comparison_sds_quoname, "is.numeric")
  if (!all(data[[comparison_sds_quoname]] > 0, na.rm = TRUE)) {
    stop(
      glue::glue("
Some sd values in {comparison_sds_quoname} are 0 or less.
These are rows {which(data[[comparison_sds_quoname]] > 0)}.
      ")
    )
  }
  row_report <- esci_assert_column_has_valid_rows(
    data,
    comparison_sds_quoname,
    lower = 2,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }
  # reference_ns
  esci_assert_valid_column_name(data, reference_ns_quoname)
  esci_assert_column_type(data, reference_ns_quoname, "is.numeric")
  if (!all(data[[reference_ns_quoname]] > 0, na.rm = TRUE)) {
    stop(
      glue::glue("
Some n values in {reference_ns_quoname} are 0 or less.
These are rows {which(data[[reference_ns_quoname]] > 0)}.
      ")
    )
  }
  if (!all(is.whole.number(data[[reference_ns_quoname]]), na.rm = TRUE)) {
    stop(
      glue::glue("
Some n values in {reference_ns_quoname} are not integers.
These are rows {which(!is.whole.number(data[[reference_ns_quoname]]))}.
      ")
    )
  }
  row_report <- esci_assert_column_has_valid_rows(
    data,
    reference_ns_quoname,
    lower = 2,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }
  # comparison_ns
  esci_assert_valid_column_name(data, comparison_ns_quoname)
  esci_assert_column_type(data, comparison_ns_quoname, "is.numeric")
  if (!all(data[[comparison_ns_quoname]] > 0, na.rm = TRUE)) {
    stop(
      glue::glue("
Some sample-size values in {comparison_ns_quoname} are 0 or less.
These are rows {which(data[[comparison_ns_quoname]] > 0)}.
      ")
    )
  }
  if (!all(is.whole.number(data[[comparison_ns_quoname]]), na.rm = TRUE)) {
    stop(
      glue::glue("
Some n values in {comparison_ns_quoname} are not integers.
These are rows {which(!is.whole.number(data[[comparison_ns_quoname]]))}.
      ")
    )
  }
  row_report <- esci_assert_column_has_valid_rows(
    data,
    comparison_ns_quoname,
    lower = 2,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }

  # labels
  if (is.null(labels_quoname)) {
    data$esci_label <- paste("Study", seq(1:nrow(data)))
    labels_quoname <- "esci_label"
  } else {
    esci_assert_valid_column_name(data, labels_quoname)
  }
  # moderator
  moderator <- !is.null(moderator_quoname)
  if (moderator) esci_assert_valid_column_name(data, moderator_quoname)
  # Check options
  esci_assert_type(assume_equal_variance, "is.logical")
  esci_assert_type(report_smd, "is.logical")
  # All other checks happen in meta_any


  # Data prep------------------------------------------
  # vector of passed column names
  just_cols <- c(
    labels_quoname,
    reference_means_quoname,
    reference_sds_quoname,
    reference_ns_quoname,
    comparison_means_quoname,
    comparison_sds_quoname,
    comparison_ns_quoname,
    if (moderator) moderator_quoname
  )

  # vector of cannonical column names
  numeric_cols <- c(
    "reference_mean",
    "reference_sd",
    "reference_n",
    "comparison_mean",
    "comparison_sd",
    "comparison_n"
  )
  col_names <- c(
    "label",
    numeric_cols,
    if (moderator) "moderator"
  )

  # reduce data down to just needed columns with cannonical names
  data <- data[ , just_cols]
  colnames(data) <- col_names


  # Calculations -------------------------------------------------
  # Get yi and vi for raw scores
  if (!report_smd) {
    es_data <- as.data.frame(
      t(
        apply(
          X = data[ , numeric_cols],
          MARGIN = 1,
          FUN = apply_ci_mdiff,
          assume_equal_variance = assume_equal_variance,
          conf_level = conf_level
        )
      )
    )
  } else {
    es_data <- as.data.frame(
      t(
        apply(
          X = data[ , numeric_cols],
          MARGIN = 1,
          FUN = apply_ci_stdmean_two,
          assume_equal_variance = assume_equal_variance,
          correct_bias = TRUE,
          conf_level = conf_level
        )
      )
    )

  }


  res <- meta_any(
    data = cbind(data, es_data),
    yi = "yi",
    vi = "vi",
    moderator = !!if (moderator) "moderator" else NULL,
    labels = "label",
    effect_label = effect_label,
    effect_size_name = if (report_smd) "SMD" else "Mean difference",
    moderator_variable_name = if (moderator) moderator_quoname else "My moderator",
    random_effects = random_effects,
    conf_level = conf_level
  )

  data$label <- NULL
  data$moderator <- NULL
  res$raw_data <- cbind(res$raw_data, es_data[ , c("LL", "UL")], data)

  return(res)
}


