#' Estimate meta-analytic difference in magnitude between two ind. groups
#'
#' @description
#' `meta_mdiff` returns
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
#' @param random_effects re
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#'
#' @return Returnsobject of class esci_estimate
#'
#'
#' @export
meta_mdiff <- function(
  data,
  comparison_means,
  comparison_sds,
  comparison_ns,
  reference_means,
  reference_sds,
  reference_ns,
  labels,
  moderator = NULL,
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

  labels_enquo        <-  rlang::enquo(labels)
  labels_quoname      <-  rlang::quo_name(labels_enquo)


  # Data prep------------------------------------------

  # If no labels column, create one
  if(is.null(data[[labels_quoname]])) {
    data$esci_label <- paste("Study", seq(1:nrow(data)))
  }

  # vector of passed column names
  just_cols <- c(
    reference_means_quoname,
    reference_sds_quoname,
    reference_ns_quoname,
    comparison_means_quoname,
    comparison_sds_quoname,
    comparison_ns_quoname,
    if (is.null(data[[labels_quoname]])) "esci_label" else labels_quoname
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
  # Get yi and vi for raw scores
  mdiff <- as.data.frame(
    t(
      apply(
        X = data[ , numeric_cols],
        MARGIN = 1,
        FUN = apply_ci_mdiff,
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance,
        effect_size = "raw"
      )
    )
  )

  smd <- as.data.frame(
    t(
      apply(
        X = data[ , numeric_cols],
        MARGIN = 1,
        FUN = apply_ci_mdiff,
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance,
        effect_size = "smd"
      )
    )
  )


  res <- meta_any(
    data = cbind(data, mdiff),
    moderator = moderator,
    effect_size_name = "Mean Difference",
    random_effects = random_effects,
    conf_level = conf_level
  )

  smd_res <- meta_any(
    data = cbind(data, smd),
    moderator = moderator,
    effect_size_name = "Standardized Mean Difference",
    random_effects = random_effects,
    conf_level = conf_level
  )

  res$es_meta_smd <- smd_res$es_meta
  res$es_meta_smd_difference <- smd_res$es_meta_difference

  colnames(smd) <- paste("smd_", colnames(smd))
  colnames(mdiff) <- paste("raw_", colnames(mdiff))
  res$raw_data <- cbind(data, mdiff, smd)

  return(res)
}


