#' Estimate meta-analytic difference in magnitude between two ind. groups
#'
#' @description
#' `meta_smd_two` returns
#'
#'
#' @param data A dataframe or tibble
#' @param smds smd
#' @param comparison_n comparison
#' @param reference_ns reference
#' @param labels labels
#' @param moderator mod
#' @param contrast contrast
#' @param random_effects re
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#'
#' @return Returnsobject of class esci_estimate
#'
#'
#' @export
meta_smd_two <- function(
  data,
  smds,
  comparison_ns,
  reference_ns,
  labels = NULL,
  moderator = NULL,
  contrast = NULL,
  effect_label = "My effect",
  random_effects = TRUE,
  assume_equal_variance = FALSE,
  correct_bias = TRUE,
  esci_vi = FALSE,
  conf_level = .95
)  {

  # Initialization ---------------------------
  # Create quosures and quonames.
  # Stolen directly from dabestr
  smds_enquo        <-  rlang::enquo(smds)
  smds_quoname      <-  rlang::quo_name(smds_enquo)

  comparison_ns_enquo        <-  rlang::enquo(comparison_ns)
  comparison_ns_quoname      <-  rlang::quo_name(comparison_ns_enquo)

  reference_ns_enquo        <-  rlang::enquo(reference_ns)
  reference_ns_quoname      <-  rlang::quo_name(reference_ns_enquo)

  moderator_enquo        <-  rlang::enquo(moderator)
  moderator_quoname      <-  rlang::quo_name(moderator_enquo)

  labels_enquo        <-  rlang::enquo(labels)
  labels_quoname      <-  rlang::quo_name(labels_enquo)


  # Data prep------------------------------------------

  # If no labels column, create one
  if(is.null(data[[labels_quoname]])) {
    data$esci_label <- paste("Study", seq(1:nrow(data)))
  }

  # vector of passed column names
  just_cols <- c(
    smds_quoname,
    reference_ns_quoname,
    comparison_ns_quoname,
    if (is.null(data[[labels_quoname]])) "esci_label" else labels_quoname
  )

  # vector of cannonical column names
  numeric_cols <- c(
    "smd",
    "reference_n",
    "comparison_n"
  )
  col_names <- c(
    numeric_cols,
    "label"
  )

  # If moderator define, add it to passed and cannonical column names
  moderator <- FALSE
  if(!is.null(data[[moderator_quoname]])) {
    just_cols <- c(just_cols, moderator_quoname)
    col_names <- c(col_names, "moderator")
    moderator <- TRUE
  }

  # reduce data down to just needed columns with cannonical names
  data <- data[ , just_cols]
  colnames(data) <- col_names


  # Calculations -------------------------------------------------
  data$comparison_sd <- rep(1, times = nrow(data))
  data$reference_sd <- rep(1, times = nrow(data))
  data$reference_mean <- rep(0, times = nrow(data))
  data$comparison_mean <- data$smd
  smd_numeric_cols <- c(
    "reference_mean",
    "reference_sd",
    "reference_n",
    "comparison_mean",
    "comparison_sd",
    "comparison_n"
  )

  # Get yi and vi for raw scores
  es_data <- as.data.frame(
    t(
      apply(
        X = data[ , smd_numeric_cols],
        MARGIN = 1,
        FUN = apply_ci_stdmean_two,
        assume_equal_variance = assume_equal_variance,
        correct_bias = correct_bias,
        conf_level = conf_level
      )
    )
  )

  res <- meta_any(
    data = cbind(data, es_data),
    yi = "yi",
    vi = !!if (esci_vi) "vi_alt" else "vi",
    moderator = !!if (moderator) "moderator" else NULL,
    labels = "label",
    effect_label = effect_label,
    effect_size_name = "SMD",
    moderator_variable_name = if (moderator) moderator_quoname else "My moderator",
    contrast = contrast,
    random_effects = random_effects,
    conf_level = conf_level
  )

  clear_cols <- c(
    "label",
    "moderator",
    "comparison_sd",
    "reference_sd",
    "reference_mean",
    "comparison_mean"
  )
  data[ , clear_cols] <- NULL
  res$raw_data <- cbind(res$raw_data, es_data[ , c("LL", "UL")], data)

  return(res)
}


