#' Utility function single quote string
#'
#' This function generates usual single-quotes for each element of the character
#' vector.
#'
#' @param string Character vector
#'
#' @return quoted string
#'
#' @family string_functions
#' @concept process
#' @noRd
util_set_sQuoteString <- function(string) {
  withr::local_options(list(useFancyQuotes = FALSE))
  sQuote(string)
}
