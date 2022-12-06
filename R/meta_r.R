#' Estimate meta-analytic difference in magnitude between two ind. groups
#'
#' @description
#' `meta_r` returns
#'
#'
#' @param data A dataframe or tibble
#' @param rs smd
#' @param ns comparison
#' @param labels labels
#' @param moderator mod
#' @param contrast contrast
#' @param effect_label effect_label
#' @param random_effects re
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#'
#' @return Returnsobject of class esci_estimate
#'
#'
#' @export
meta_r <- function(
  data,
  rs,
  ns,
  labels = NULL,
  moderator = NULL,
  contrast = NULL,
  effect_label = "My effect",
  random_effects = TRUE,
  conf_level = .95
)  {

  # Initialization ---------------------------
  # Create quosures and quonames.
  # Stolen directly from dabestr
  rs_enquo        <-  rlang::enquo(rs)
  rs_quoname      <-  rlang::quo_name(rs_enquo)

  ns_enquo        <-  rlang::enquo(ns)
  ns_quoname      <-  rlang::quo_name(ns_enquo)

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
  # * the column r must exist and be numeric, >= -1 and <= -1
  #    with > 1 row after NAs removed
  # * the column ns must exist and be numeric integers > 0
  #    with > 1 row after NAs removed
  # * the column labels is optional, but if passed must exist and
  #    have > 1 row after NAs removed
  # * the column moderator is optional; checks happen in meta_any
  # * contrast should only be passed in moderator is defined; checks in meta_any
  # * effect_label should be a character, checked in meta_any
  # * conf_level must be a numeric >0 and < 1, checked in meta_any


  # Check that data is a data.frame
  esci_assert_type(data, "is.data.frame")

  # rs
  esci_assert_valid_column_name(data, rs_quoname)
  esci_assert_column_type(data, rs_quoname, "is.numeric")
  row_report <- esci_assert_column_has_valid_rows(
    data,
    rs_quoname,
    lower = 2,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }
  if (!all(data[[rs_quoname]] >= -1, na.rm = TRUE)) {
    stop(
      glue::glue("
Some r values in {rs_quoname} are < -1.
These are rows {paste(which(data[[rs_quoname]] < -1), collapse = ', ')}.
      ")
    )
  }
  if (!all(data[[rs_quoname]] <= 1, na.rm = TRUE)) {
    stop(
      glue::glue("
Some r values in {rs_quoname} are > 1.
These are rows {paste(which(data[[rs_quoname]] > 1), collapse = ', ')}.
      ")
    )
  }

  # ns
  esci_assert_valid_column_name(data, ns_quoname)
  esci_assert_column_type(data, ns_quoname, "is.numeric")
  if (!all(data[[ns_quoname]] > 0, na.rm = TRUE)) {
    stop(
      glue::glue("
Some n values in {ns_quoname} are 0 or less.
These are rows {paste(which(data[[ns_quoname]] <= 0), collapse = ', ')}.
      ")
    )
  }
  if (!all(is.whole.number(data[[ns_quoname]]), na.rm = TRUE)) {
    stop(
      glue::glue("
Some n values in {ns_quoname} are not integers.
These are rows {paste(which(!is.whole.number(data[[ns_quoname]])), collapse = ', ')}.
      ")
    )
  }
  row_report <- esci_assert_column_has_valid_rows(
    data,
    ns_quoname,
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
  row_report <- esci_assert_column_has_valid_rows(
    data,
    labels_quoname,
    lower = 2,
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }

  # moderator
  moderator <- !is.null(moderator_quoname)
  if (moderator) {
    esci_assert_valid_column_name(data, moderator_quoname)
    row_report <- esci_assert_column_has_valid_rows(
      data,
      moderator_quoname,
      lower = 2,
    )
    if (row_report$missing > 0) {
      warnings <- c(warnings, row_report$warning)
      warning(row_report$warning)
      data <- data[-row_report$NA_rows, ]
    }
  }

  # Check options

  # All other checks happen in meta_any:
  # * additional constraints on moderator
  # * contrast
  # * random_effects
  # * conf_level


  # Data prep------------------------------------------

  # vector of passed column names
  just_cols <- c(
    labels_quoname,
    rs_quoname,
    ns_quoname,
    if (moderator) moderator_quoname
  )

  # vector of cannonical column names
  numeric_cols <- c(
    "r",
    "N"
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
  es_data <- as.data.frame(
    t(
      apply(
        X = data[ , numeric_cols],
        MARGIN = 1,
        FUN = apply_ci_cor,
        conf_level = conf_level
      )
    )
  )

  res <- meta_any(
    data = cbind(data, es_data),
    yi = "yi",
    vi = "vi",
    moderator = !!if (moderator) "moderator" else NULL,
    labels = "label",
    effect_label = effect_label,
    effect_size_name = "r",
    moderator_variable_name = if (moderator) moderator_quoname else "My moderator",
    contrast = contrast,
    random_effects = random_effects,
    conf_level = conf_level
  )

  # Clean up -----------------------------
  clear_cols <- c(
    "label",
    "moderator"
  )
  data[ , clear_cols] <- NULL
  res$raw_data <- cbind(res$raw_data, es_data[ , c("LL", "UL")], data)
  res$warnings <- c(res$warnings, warnings)

  # Effect size labels
  res$properties$effect_size_name <- "r"
  res$properties$effect_size_name_html <- "<i>r</i>"
  res$properties$effect_size_name_ggplot <- "*r*"

  return(res)
}


