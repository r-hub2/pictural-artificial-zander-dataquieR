#' Escape characters for HTML in a data frame
#'
#' @param x [data.frame] to be escaped
#'
#' @return [data.frame] with html escaped content
#' @noRd
util_df_escape <- function(x) {
  if (identical(util_attr(x, "is_html_escaped", exact = TRUE), TRUE)) {
    return(x)
  }
  util_expect_data_frame(x)
  variable_columns <- intersect(names(x), c("Variables", VAR_NAMES))
  x[] <- lapply(names(x), function(name) {
    y <- x[[name]]
    plain_label <- util_attr(y, "plain_label", exact = TRUE)
    if (is.null(plain_label) && name %in% variable_columns) {
      plain_label <- as.character(y)
    }
    r <- htmltools::htmlEscape(y)
    attr(r, "plain_label") <- plain_label
    # Add the new data type attributes for columns
    attr(r, DATA_TYPE) <- util_attr(y, DATA_TYPE, exact = TRUE)

    r
  })
  attr(x, "is_html_escaped") <- TRUE
  x
}
