#' Get machine variant for snapshot tests
#'
#' @return [character] the variant
#'
#' @keywords internal
#' @export
prep_get_variant <- function() {
  v <- sub("^([0-9]+[.][0-9]+).*", "R\\1", as.character(getRversion()))
  return(v)
}
