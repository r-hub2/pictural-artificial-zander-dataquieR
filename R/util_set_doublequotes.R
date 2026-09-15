#' Utility function to put strings in quotes
#'
#' This function generates usual double-quotes for each element of the character
#' vector
#'
#' @param string Character vector
#'
#' @return quoted string
#'
#' @family string_functions
#' @concept process
#' @noRd
util_set_dQuoteString <- function(string) {
  withr::local_options(list(useFancyQuotes = FALSE))
  dQuote(string)
}
