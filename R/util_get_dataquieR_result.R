#' Extract Parts of a `dataquieR` Result Object
#'
#' @param x the `dataquieR` result object
#'
#' @param ... arguments passed to the implementation for lists.
#'
#' @return the sub-list of the `dataquieR` result object with all messages
#'         still attached
#'
#' @seealso  [base::Extract]
#' @export
#'
#' @noRd
`[.dataquieR_result` <- function(x, ...) {
  r <- NextMethod()
  attr(r, "error") <- util_attr(x, "error", exact = TRUE)
  attr(r, "message") <- util_attr(x, "message", exact = TRUE)
  attr(r, "warning") <- util_attr(x, "warning", exact = TRUE)
  attr(r, "as_plotly") <- util_attr(x, "as_plotly", exact = TRUE)
  attr(r, "dont_util_adjust_geom_text_for_plotly") <-
    util_attr(x, "dont_util_adjust_geom_text_for_plotly", exact = TRUE)
  attr(r, "function_name") <- util_attr(x, "function_name", exact = TRUE)
  attr(r, "cn") <- util_attr(x, "cn", exact = TRUE)
  attr(r, "call") <- util_attr(x, "call", exact = TRUE)
  attr(r, CHECK_ID) <- util_attr(x, CHECK_ID, exact = TRUE)
  attr(r, CHECK_LABEL) <- util_attr(x, CHECK_LABEL, exact = TRUE)
  class(r) <- unique(c("dataquieR_result", class(r)))
  r
}

#' Extract Elements of a `dataquieR` Result Object
#'
#' @param x the `dataquieR` result object
#'
#' @param ... arguments passed to the implementation for lists.
#'
#' @return the element of the `dataquieR` result object with all messages
#'         still attached
#'
#' @seealso  [base::Extract]
#' @export
#'
#' @noRd
`[[.dataquieR_result` <- function(x, ...) {
  r <- NextMethod()
  if (is.null(r)) {
    r <- list()
    class(r) <- union("dataquieR_NULL", class(r))
  }
  if (!util_is_gg(x)) {
    attr(r, "error") <- util_attr(x, "error", exact = TRUE)
    attr(r, "message") <- util_attr(x, "message", exact = TRUE)
    attr(r, "warning") <- util_attr(x, "warning", exact = TRUE)
    # Do not assign the dataquieR_result class here.
    class(r) <- unique(c("Slot", class(r)))
  }
  r
}

#' Extract elements of a `dataquieR` Result Object
#'
#' @param x the `dataquieR` result object
#'
#' @param ... arguments passed to the implementation for lists.
#'
#' @return the element of the `dataquieR` result object with all messages
#'         still attached
#'
#' @seealso  [base::Extract]
#' @export
#'
#' @noRd
`$.dataquieR_result` <- `[[.dataquieR_result`
