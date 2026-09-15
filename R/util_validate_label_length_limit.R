#' Internal helper: util validate label length limit
#'
#' @noRd
.util_validate_label_length_limit <- function(x, argument = NULL,
  option = NULL,
  clamp_to_max = FALSE) {
  util_stop_if_not(length(argument) <= 1L)
  util_stop_if_not(length(option) <= 1L)
  util_stop_if_not(length(clamp_to_max) == 1L)

  option_hint <- paste0(
    "set options(dataquieR.MAX_LABEL_LEN = ..., ",
    "dataquieR.MAX_LONG_LABEL_LEN = ...) to values of at least ",
    .MIN_LABEL_LEN
  )
  if (!is.null(argument)) {
    source_hint <- sprintf("Argument %s", sQuote(argument))
    fix_hint <- sprintf(
      "Pass %s >= %d, or omit it and %s.",
      sQuote(argument), .MIN_LABEL_LEN, option_hint
    )
  } else if (!is.null(option)) {
    source_hint <- sprintf("Option %s", sQuote(option))
    fix_hint <- sprintf("Please %s.", option_hint)
  } else {
    source_hint <- "Label length limit"
    fix_hint <- sprintf("Please %s.", option_hint)
  }

  if (length(x) != 1L || !is.numeric(x) || !is.finite(x) ||
      !util_is_integer(x)) {
    util_error(
      c(
        "%s must be a finite whole number of at least %d.",
        "%s"
      ),
      source_hint, .MIN_LABEL_LEN, fix_hint,
      applicability_problem = TRUE
    )
  }
  if (x < .MIN_LABEL_LEN) {
    util_error(
      c(
        "%s must be at least %d to keep shortened labels",
        "recognizable and unique. %s"
      ),
      source_hint, .MIN_LABEL_LEN, fix_hint,
      applicability_problem = TRUE
    )
  }
  if (clamp_to_max) {
    return(min(.MAX_LABEL_LEN, x))
  }
  x
}
