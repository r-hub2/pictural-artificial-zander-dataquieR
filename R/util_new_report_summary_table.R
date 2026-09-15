#' Construct a `ReportSummaryTable`
#'
#' @param x [data.frame] object to mark as a `ReportSummaryTable`.
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data. Used to translate
#'                               variable names, if given.
#' @inheritParams .template_function_developer
#'
#' @return `ReportSummaryTable` object based on `x`.
#' @noRd
util_new_report_summary_table <- function(x, meta_data, label_col) {
  util_expect_data_frame(x)
  if (nrow(x) == 0L && ncol(x) == 0L) {
    x <- data.frame(Variables = character(0), N = integer(0))
  }
  if (!missing(meta_data) && !missing(label_col)) {
    x <- util_validate_report_summary_table(x,
      meta_data = meta_data,
      label_col = label_col
    )
  } else if (!missing(meta_data) || !missing(label_col)) {
    if (!missing(meta_data)) {
      x <- util_validate_report_summary_table(x, meta_data = meta_data)
    } else {
      x <- util_validate_report_summary_table(x, label_col = label_col)
    }
  } else {
    x <- util_validate_report_summary_table(x)
  }
  class(x) <- union("ReportSummaryTable", class(x))
  x
}

#' Test if an object is a `ReportSummaryTable`
#'
#' @param x Object to test.
#'
#' @return [logical] `TRUE`, if `x` inherits from `ReportSummaryTable`.
#' @noRd
util_is_report_summary_table <- function(x) {
  inherits(x, "ReportSummaryTable")
}
