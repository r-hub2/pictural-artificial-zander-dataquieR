#' Get `ReportSummaryTable` higher-means direction
#'
#' @param x `ReportSummaryTable` object.
#'
#' @return Higher-means direction.
#' @noRd
util_report_summary_table_higher_means <- function(x) {
  util_attr(x, "higher_means", exact = TRUE)
}

#' Set `ReportSummaryTable` higher-means direction
#'
#' @param x `ReportSummaryTable` object.
#' @param value higher-means direction.
#'
#' @return `ReportSummaryTable` object with updated attribute.
#' @noRd
util_set_report_summary_table_higher_means <- function(x, value) {
  attr(x, "higher_means") <- value
  x
}

#' Get `ReportSummaryTable` flip mode
#'
#' @param x `ReportSummaryTable` object.
#'
#' @return Flip mode.
#' @noRd
util_report_summary_table_flip_mode <- function(x) {
  util_attr(x, "flip_mode", exact = TRUE)
}

#' Set `ReportSummaryTable` flip mode
#'
#' @param x `ReportSummaryTable` object.
#' @param value flip mode.
#'
#' @return `ReportSummaryTable` object with updated attribute.
#' @noRd
util_set_report_summary_table_flip_mode <- function(x, value) {
  attr(x, "flip_mode") <- value
  x
}

#' Get whether a `ReportSummaryTable` is continuous
#'
#' @param x `ReportSummaryTable` object.
#'
#' @return [logical] continuous flag.
#' @noRd
util_report_summary_table_continuous <- function(x) {
  util_attr(x, "continuous", exact = TRUE)
}

#' Set whether a `ReportSummaryTable` is continuous
#'
#' @param x `ReportSummaryTable` object.
#' @param value [logical] continuous flag.
#'
#' @return `ReportSummaryTable` object with updated attribute.
#' @noRd
util_set_report_summary_table_continuous <- function(x, value) {
  attr(x, "continuous") <- value
  x
}

#' Get `ReportSummaryTable` color scale
#'
#' @param x `ReportSummaryTable` object.
#'
#' @return Color scale.
#' @noRd
util_report_summary_table_colscale <- function(x) {
  util_attr(x, "colscale", exact = TRUE)
}

#' Set `ReportSummaryTable` color scale
#'
#' @param x `ReportSummaryTable` object.
#' @param value color scale.
#'
#' @return `ReportSummaryTable` object with updated attribute.
#' @noRd
util_set_report_summary_table_colscale <- function(x, value) {
  attr(x, "colscale") <- value
  x
}

#' Get `ReportSummaryTable` color codes
#'
#' @param x `ReportSummaryTable` object.
#'
#' @return Color codes.
#' @noRd
util_report_summary_table_colcode <- function(x) {
  util_attr(x, "colcode", exact = TRUE)
}

#' Set `ReportSummaryTable` color codes
#'
#' @param x `ReportSummaryTable` object.
#' @param value color codes.
#'
#' @return `ReportSummaryTable` object with updated attribute.
#' @noRd
util_set_report_summary_table_colcode <- function(x, value) {
  attr(x, "colcode") <- value
  x
}

#' Get `ReportSummaryTable` level names
#'
#' @param x `ReportSummaryTable` object.
#'
#' @return Level names.
#' @noRd
util_report_summary_table_level_names <- function(x) {
  util_attr(x, "level_names", exact = TRUE)
}

#' Set `ReportSummaryTable` level names
#'
#' @param x `ReportSummaryTable` object.
#' @param value level names.
#'
#' @return `ReportSummaryTable` object with updated attribute.
#' @noRd
util_set_report_summary_table_level_names <- function(x, value) {
  attr(x, "level_names") <- value
  x
}

#' Get whether a `ReportSummaryTable` uses relative values
#'
#' @param x `ReportSummaryTable` object.
#'
#' @return [logical] relative flag.
#' @noRd
util_report_summary_table_relative <- function(x) {
  util_attr(x, "relative", exact = TRUE)
}

#' Set whether a `ReportSummaryTable` uses relative values
#'
#' @param x `ReportSummaryTable` object.
#' @param value [logical] relative flag.
#'
#' @return `ReportSummaryTable` object with updated attribute.
#' @noRd
util_set_report_summary_table_relative <- function(x, value) {
  attr(x, "relative") <- value
  x
}

#' Get `ReportSummaryTable` variable names
#'
#' @param x `ReportSummaryTable` object.
#'
#' @return [character] variable names.
#' @noRd
util_report_summary_table_var_names <- function(x) {
  util_attr(x, "VAR_NAMES", exact = TRUE)
}

#' Set `ReportSummaryTable` variable names
#'
#' @param x `ReportSummaryTable` object.
#' @param value [character] variable names.
#'
#' @return `ReportSummaryTable` object with updated variable-name attribute.
#' @noRd
util_set_report_summary_table_var_names <- function(x, value) {
  attr(x, "VAR_NAMES") <- value
  x
}

#' Get the label column used by the `Variables` column
#'
#' @param x `ReportSummaryTable` object.
#'
#' @return [character] label column.
#' @noRd
util_report_summary_table_variables_label_col <- function(x) {
  util_attr(x$Variables, "label_col", exact = TRUE)
}

#' Set the label column used by the `Variables` column
#'
#' @param x `ReportSummaryTable` object.
#' @param value [character] label column.
#'
#' @return `ReportSummaryTable` object with updated `Variables` label column.
#' @noRd
util_set_report_summary_table_variables_label_col <- function(x, value) {
  attr(x$Variables, "label_col") <- value
  x
}
