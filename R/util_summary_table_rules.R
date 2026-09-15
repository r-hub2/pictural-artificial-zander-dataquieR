#' Internal helper: apply summary table rules
#'
#' @noRd
util_apply_summary_table_rules <- function(tb, rules) {
  util_stop_if_not(is.data.frame(tb), is.list(rules))

  for (rule in rules) {
    sources <- rule[["sources"]]
    target <- rule[["target"]]
    formatter <- rule[["formatter"]]
    if (!all(sources %in% colnames(tb))) {
      next
    }

    values <- formatter(tb[, sources, drop = FALSE])
    util_stop_if_not(length(values) == nrow(tb))
    if (target %in% colnames(tb)) {
      use_values <- is.na(tb[[target]]) & !is.na(values)
      tb[[target]][use_values] <- values[use_values]
    } else {
      tb[[target]] <- values
    }
    tb[setdiff(sources, target)] <- NULL
  }

  tb
}

#' Internal helper: summary table identity
#'
#' @noRd
util_summary_table_identity <- function(source_columns) {
  source_columns[[1]]
}

#' Internal helper: ssi format mahalanobis threshold
#'
#' @noRd
util_ssi_format_mahalanobis_threshold <- function(source_columns) {
  threshold <- source_columns[[1]]
  ifelse(
    is.na(threshold),
    NA_character_,
    paste0("\u2264 \u03c7\u00b2 quantile (p = ", threshold, ")")
  )
}

#' Internal helper: ssi format n percent
#'
#' @noRd
util_ssi_format_n_percent <- function(source_columns) {
  n <- source_columns[[1]]
  percent <- source_columns[[2]]
  values <- rep(NA_character_, length(n))
  available <- !is.na(n) | !is.na(percent)
  values[available] <- paste0(n[available], " (", percent[available], ")")
  values
}

#' Internal helper: ssi summary table rules
#'
#' @noRd
util_ssi_summary_table_rules <- function() {
  list(
    list(
      sources = "mahalanobis_threshold",
      target = "Admissible",
      formatter = util_ssi_format_mahalanobis_threshold
    ),
    list(
      sources = "Admissible range",
      target = "Admissible",
      formatter = util_summary_table_identity
    ),
    list(
      sources = c("MD_outliers (N)", "MD_outliers (%)"),
      target = "Above range N (%)",
      formatter = util_ssi_format_n_percent
    ),
    list(
      sources = "observational_units_removed",
      target = "Observational units removed",
      formatter = util_summary_table_identity
    )
  )
}
