#' @inherit resnames
#'
#' @export
# importFrom dataquieR resnames
resnames.dataquieR_resultset2 <- function(x) {
  rsn <- util_attr(x, "resnames", exact = TRUE)
  if (!is.null(rsn)) {
    return(rsn)
  } else {
    return(sort(unique(unname(unlist(lapply(x, names))))))
  }
}
