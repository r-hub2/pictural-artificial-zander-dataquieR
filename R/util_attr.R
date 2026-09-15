#' Get an attribute using exact matching by default
#'
#' @param x object whose attribute is requested
#' @param which [character] attribute name
#' @param exact [logical] passed to [attr()]
#'
#' @return the requested attribute value, or `NULL`
#'
#' @noRd
util_attr <- function(x, which, exact = TRUE) {
  base::attr(x, which, exact = exact)
}
