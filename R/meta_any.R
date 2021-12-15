#' Estimate any meta effect
#'
#' @description
#' `meta_any` returns
#'
#'
#' @param data A dataframe or tibble with columns
#'   * label, LL, UL, yi, vi, moderator and any other columns relevant to that meta-analysis
#' @param moderator logical if moderator has been passed
#' @param effect_size_name defaults to MDiff
#' @param random_effects logical
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#'
#' @return Returnsobject of class esci_estimate
#'
#'
#' @export
meta_any <- function(
  data,
  moderator = FALSE,
  contrast = NULL,
  effect_size_name = "MDiff",
  random_effects = TRUE,
  conf_level = 0.95
) {
  # We've passed in a prepared data table with columns:
  # label, UL, LL, yi, vi, moderator and any other columns
  # relevant to that meta-analysis

  # Now do both fixed and random effects overall meta-analaysis
  FE <- metafor::rma(data = data, yi = yi, vi = vi, method="FE")
  RE <- metafor::rma(data = data, yi = yi, vi = vi, method="DL")

  # Calculate diamond.ratio
  dr_res <- CI_diamond_ratio(RE, FE, data$vi, conf_level = conf_level)

  diamond_ratio <- dr_res$diamond_ratio
  diamond_ratio_LL <- dr_res$LL
  diamond_ratio_UL <- dr_res$UL


  # Extract results table from each
  FEtbl <- meta_to_table(FE, fixed_effects = TRUE, dr_res, label = "Overall")
  REtbl <- meta_to_table(RE, fixed_effects = FALSE, dr_res, label = "Overall")

  # Ok - this gets a bit complicated
  if(moderator) {
    # With -1, we estimate with no intercept, so the parameters reflect each moderator level effect on its own
    FEgroups <- metafor::rma(
      data = data,
      yi = yi,
      vi = vi,
      mods = ~ moderator -1,
      method="FE"
    )
    REgroups <- metafor::rma(
      data = data,
      yi = yi,
      vi = vi,
      mods = ~ moderator -1,
      method="DL"
    )

    # Save tables for each group-based analysis
    FEgtable <- meta_to_table(FEgroups, fixed_effects = TRUE)
    REgtable <- meta_to_table(REgroups, fixed_effects = FALSE)

    # Getting really crazy now--the group-based analyses don't give us I2
    # or the diamond ratio for each level, so we run an additional analyses for each level
    #   and save I2 and its CI for each of these.
    x <- 0
    replabels <- NULL
    for(lev in levels(data$moderator)) {
      x <- x + 1
      REjustl <- metafor::rma(data = data[data$moderator == lev, ], yi = yi, vi = vi, method = "DL")
      FEjustl <- metafor::rma(data = data[data$moderator == lev, ], yi = yi, vi = vi, method = "FE")
      dr_res <- CI_diamond_ratio(REjustl, FEjustl, data[data$moderator == lev, ]$vi, conf_level = conf_level)
      REjustltbl <- meta_to_table(REjustl, fixed_effects = FALSE, dr_res)
      replabels <- c(replabels, lev)
      REgtable[x, "I2"] <- REjustltbl$I2[1]
      REgtable[x, "I2_LL"] <- REjustltbl$I2_LL[1]
      REgtable[x, "I2_UL"] <- REjustltbl$I2_UL[1]
      REgtable[x, "diamond_ratio"] <- dr_res$diamond_ratio
      REgtable[x, "diamond_ratio_LL"] <- dr_res$LL
      REgtable[x, "diamond_ratio_UL"] <- dr_res$UL
      FEgtable[x, "diamond_ratio"] <- dr_res$diamond_ratio
      FEgtable[x, "diamond_ratio_LL"] <- dr_res$LL
      FEgtable[x, "diamond_ratio_UL"] <- dr_res$UL
    }
    REgtable$effect <- replabels
    FEgtable$effect <- replabels

    FEtbl <- rbind(
      FEtbl,
      FEgtable
    )
    REtbl <- rbind(
      REtbl,
      REgtable
    )


    # Now contrasts
    # Check contrast
    if (is.null(contrast)) {
      contrast = rep(x = 0, times = length(FEgroups$b))
      contrast[1] = -1
      contrast[2] = 1
    }

    if (is.null(names(contrast)) & (length(contrast) == length(replabels))) {
      names(contrast) <- replabels
    }

    # Build contrast set: comparison, reference, and difference
    contrasts <- list(
      Comparison = contrast,
      Reference = contrast,
      Difference = contrast
    )
    # Filter to create comparison and reference only subsets
    contrasts$Comparison[which(contrasts$Comparison < 0)] <- 0
    contrasts$Reference[which(contrasts$Reference > 0)] <- 0
    contrasts$Reference <- abs(contrasts$Reference)

    contrast_labels <- esci_tool_contrast_labels(contrast)

    # initialize results tables
    FE_contrast <- NULL
    RE_contrast <- NULL

    for (x in 1:length(contrasts)) {
      FE_anova <- anova(FEgroups, X = contrasts[[x]])
      FE_contrast <- rbind(
        FE_contrast,
        anova_to_table(
          cres = FE_anova,
          type = names(contrasts[x]),
          effect = contrast_labels[[x]],
          conf_level
        )
      )
      RE_anova <- anova(REgroups, X = contrasts[[x]])
      RE_contrast <- rbind(
        RE_contrast,
        anova_to_table(
          cres = RE_anova,
          type = names(contrasts[x]),
          effect = contrast_labels[[x]],
          conf_level
        )
      )
    }

  }


  # Cross-populate RE/FE
  cross_cols <- c(
    "FE_effect_size", "RE_effect_size",
    "FE_CI_width", "RE_CI_width"
  )
  FEtbl <- meta_FE_and_RE(FEtbl, REtbl)
  REtbl <- cbind(REtbl, FEtbl[ , cross_cols])

  # Select which meta-analysis to report
  if(random_effects) {
    es_meta <- REtbl
    es_meta_difference = if (moderator) RE_contrast else NULL
  }  else {
    es_meta <- FEtbl
    es_meta_difference = if (moderator) FE_contrast else NULL
  }


  # Set properties
  properties <- list(
    conf_level = conf_level,
    data_type = "meta"
  )


  # Fix up data just a bit
  data$sample_variance <- data$vi
  data$vi <- NULL
  data$weight <- metafor::weights.rma.uni(if (random_effects)  RE else FE)
  names(data)[names(data) == "yi"] <- "effect_size"

  res <- list(
    properties = properties,
    es_meta = es_meta,
    es_meta_difference = es_meta_difference,
    raw_data = data
  )

  class(res) <- "esci_estimate"

  return(res)
}



meta_to_table <- function(
  meta,
  fixed_effects = TRUE,
  dr_res = NULL,
  label = NULL
) {
  rowcount <- length(meta$b[, 1])

  if(fixed_effects) {
    I2 <- rep(NA, rowcount)
    I2_LL <- rep(NA, rowcount)
    I2_UL <- rep(NA, rowcount)
  } else {
    hetCIs <- metafor::confint.rma.uni(meta)
    I2 <- rep(hetCIs$random["I^2(%)", "estimate"], rowcount)
    I2_LL <- rep(hetCIs$random["I^2(%)", "ci.lb"], rowcount)
    I2_UL <- rep(hetCIs$random["I^2(%)", "ci.ub"], rowcount)
  }

  if(is.null(label)) {
    label <- names(meta$b[ ,1])
  } else {
    label <- rep(label, rowcount)
  }


  if(is.null(dr_res)) {
    dr_res <- list(
      diamond_ratio = NA,
      LL = NA,
      UL = NA
    )
  }

  result_table <- data.frame(
    effect = label,
    effect_size = unname(meta$b[, 1]),
    LL = unname(meta$ci.lb),
    UL = unname(meta$ci.ub),
    SE = unname(meta$se),
    diamond_ratio = dr_res$diamond_ratio,
    diamond_ratio_LL = dr_res$LL,
    diamond_ratio_UL = dr_res$UL,
    I2 = I2,
    I2_LL = I2_LL,
    I2_UL = I2_UL
  )

  return(result_table)
}


meta_FE_and_RE <- function(FEtable, REtable) {
  FEtable$FE_effect_size <- FEtable$effect_size
  FEtable$RE_effect_size <- REtable$effect_size
  FEtable$FE_CI_width <- abs(FEtable$UL - FEtable$LL)
  FEtable$RE_CI_width <- abs(REtable$UL - REtable$LL)

  return(FEtable)

}

anova_to_table <- function(
  cres,
  type,
  effect,
  conf_level
) {
  c_es <- cres$Xb[[1, 1]]
  c_se <- cres$se
  z_crit <- qnorm((1-conf_level)/2, lower.tail = FALSE)
  c_LL <- c_es - (c_se * z_crit)
  c_UL <- c_es + (c_se * z_crit)

  return(
    data.frame(
      type = type,
      effect = effect,
      effect_size = c_es,
      LL = c_LL,
      UL = c_UL,
      SE = c_se
    )
  )
}
