#' Prepare a label as part of a title text for `RMD` files
#'
#' @param s the label
#' @param html prepare the label for direct `HTML` output instead of `RMD`
#'
#' @return the escaped label
#' @export
#'
prep_title_escape <- function(s, html = FALSE) {
  # Historical manual title escaping removed here. Inspect commit 6a2a6a8b30
  # before restoring the older character replacement chain.
  if (html) {
    r <- lapply(s, htmltools::pre)
    r <- vapply(s, as.character, FUN.VALUE = character(1))
  } else {
    r <- s
    r <- gsub("`", "", r, fixed = TRUE)
    r <- paste0("`", r, "`")
  }
  r
}
