#' Names of a `dataquieR` report object (v2.0)
#'
#' @param x the result object
#'
#' @return the names
#'
#' @method dimnames dataquieR_resultset2
#' @export
dimnames.dataquieR_resultset2 <- function(x) {
  matrix_list <- util_attr(x, "matrix_list", exact = TRUE)
  row_indices <- util_attr(matrix_list, "row_indices", exact = TRUE)
  col_indices <- util_attr(matrix_list, "col_indices", exact = TRUE)
  list(
    names(sort(row_indices)),
    names(sort(col_indices)),
    resnames(x)
  )
}
