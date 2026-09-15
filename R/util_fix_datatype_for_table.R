# nolint start: line_length_linter.
#' Add data type attributes to referred value-code tables
#'
#' This helper is deliberately narrow: it annotates the stable referred-table
#' columns used for missing code and value label rendering.
#'
#' @param x [data.frame] a referred table for which the data type of the
#' columns needs to be defined
#'
#' @family util_functions
#' @concept html
#' @noRd
# nolint end
util_fix_datatype_for_table <- function(x) {
  if (!is.data.frame(x)) {
    util_warning(c(
      "Adding data type attributes to columns not possible,",
      "the object is not a data frame.",
      "Internal issue, sorry, please report to us"
    ))
    return(x)
  }
  column_types <- c(
    stats::setNames(DATA_TYPES$STRING, CODE_VALUE),
    stats::setNames(DATA_TYPES$STRING, CODE_LABEL),
    stats::setNames(DATA_TYPES$INTEGER, CODE_ORDER)
  )
  for (column_name in intersect(names(column_types), colnames(x))) {
    attr(x[[column_name]], DATA_TYPE) <- column_types[[column_name]]
  }
  return(x)
}
