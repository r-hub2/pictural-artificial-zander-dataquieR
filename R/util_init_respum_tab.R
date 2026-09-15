#' Extract all properties of a `ReportSummaryTable`
#'
#' @param x `ReportSummaryTable` object
#'
#' @return [list] with all properties
#'
#' @noRd
util_init_respum_tab <- function(x) {
  my_cols <- c(
    "#7f0000", "#b30000", "#d7301f", "#ef6548", "#fc8d59",
    "#fdbb84", "#fdd49e", "#fee8c8", "#2166AC"
  )

  higher_means <- util_report_summary_table_higher_means(x)
  if (is.null(higher_means)) higher_means <- "worse"
  continuous <- util_report_summary_table_continuous(x)
  if (is.null(continuous)) continuous <- TRUE
  colcode <- util_report_summary_table_colcode(x)
  if (is.null(colcode)) {
    continuous <- TRUE
  }
  level_names <- util_report_summary_table_level_names(x)

  relative <- util_report_summary_table_relative(x)
  if (is.null(relative)) relative <- continuous
  colscale <- util_report_summary_table_colscale(x)
  if (is.null(colscale)) {
    colscale <- my_cols
  }

  if (!is.null(util_report_summary_table_flip_mode(x))) {
    flip_mode <- util_report_summary_table_flip_mode(x)
  }

  if (higher_means != "worse") {
    colscale <- rev(colscale)
  }

  as.list(environment())
}
