#' Select percentages that fit inside pie-chart sectors
#'
#' @param values Numeric sector sizes.
#' @param min_fraction Minimum share required for an on-chart label.
#'
#' @return A character vector. Small or invalid sectors receive an empty label;
#'   their values remain available in the legend, hover text, or detailed view.
#' @noRd
util_pie_display_percentages <- function(values, min_fraction = 0.08) {
  util_expect_scalar(min_fraction, check_type = is.numeric)
  util_stop_if_not(min_fraction >= 0, min_fraction <= 1)

  values <- as.numeric(values)
  total <- sum(values, na.rm = TRUE)
  if (!is.finite(total) || total <= 0) {
    return(rep("", length(values)))
  }

  fractions <- values / total
  labels <- scales::percent(fractions)
  labels[!is.finite(fractions) | fractions < min_fraction] <- ""
  labels
}
