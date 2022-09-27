jamovi_peffect_html <- function(tfix) {

  if (is.null(tfix)) return(NULL)
  if (nrow(tfix) == 0) return(tfix)
  for (x in 1:nrow(tfix)) {
    old_effect <- tfix[x, "outcome_variable_name"]
    last_under <- gregexpr("_", old_effect, fixed=TRUE)[[1]]
    last_under <- last_under[length(last_under)]

    tfix[x, "outcome_variable_name"] <- paste(
      substr(old_effect, 1, last_under -2),
      "<i>P</i><sub>",
      substr(old_effect, last_under + 1, nchar(old_effect)),
      "</sub>",
      sep = ""
    )



  }

  return(tfix)

}

