# Check for class estimate
# This function checks if x is an esci_estimate
is.estimate <- function(x)  {
  is(x, "esci_estimate")
}


#' Print an esci_estimate
#'
#' Pretties up the printing of a complex esci_estimate object.
#'
#' @param x - object to print; must be of class esci_estimate
#' @param ... S3 signature for generic plot function.
#' @param verbose - optional logical print all details; defaults to false
#'
#' @exportS3Method print esci_estimate
print.esci_estimate <- function(x, ..., verbose = FALSE) {

  estimate <- x

  # Summary
  summary_text <- if(estimate$properties$data_type == "Summary")
    "Analysis of summary data:\n"
  else
    "Analysis of raw data:\n"
  if(!is.null(estimate$properties$data_source)) {
    summary_text <- paste(
      summary_text,
      "Data frame = ",
      estimate$properties$data_source,
      "\n",
      sep = ""
    )
  }
  if(!is.null(estimate$properties$outcome_variable_name)) {
    summary_text <- paste(
      summary_text,
      "Outcome variable(s) = ",
      paste(estimate$properties$outcome_variable_name, collapse = ", "),
      "\n",
      sep = ""
    )
  }
  if(!is.null(estimate$properties$grouping_variable_name)) {
    summary_text <- paste(
      summary_text,
      "Grouping variable(s) = ",
      estimate$properties$grouping_variable_name,
      "\n",
      sep = ""
    )
  }

  cat(summary_text)


  cat("\n---Overview---\n")
  print(estimate$overview)

  for (tbl in names(estimate)) {
    if (class(estimate[[tbl]]) == "data.frame" & tbl != "overview") {
      cat(paste("\n--", tbl, "--"))
      print(estimate[[tbl]])
    }
  }


  # Note about CI width
  ci_message <- paste(
    "\n\nNote: lower and upper are boundaries of confidence intervals with ",
    estimate$properties$conf_level*100,
    "% expected coverage.",
    sep = ""
  )

  cat(ci_message)
}
