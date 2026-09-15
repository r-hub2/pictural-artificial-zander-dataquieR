#' Anti-join using common columns
#'
#' @description
#' `dplyr::*_join(by = NULL)` joins by all common columns. If there are no
#' common columns, this used to imply `by = character()` and now warns. Keep the
#' old semantics explicit: if there are no common columns, every row in `x`
#' matches every row in a non-empty `y`.
#'
#' @param x,y [data.frame] tables to compare
#'
#' @return a data frame
#'
#' @noRd
util_anti_join_common <- function(x, y) {
  by <- intersect(names(x), names(y))
  if (length(by) > 0) {
    return(suppressMessages(dplyr::anti_join(x, y, by = by)))
  }
  if (nrow(y) > 0) {
    return(x[FALSE, , drop = FALSE])
  }
  x
}
