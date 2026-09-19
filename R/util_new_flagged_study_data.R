#' Construct a `FlaggedStudyData`
#'
#' @param x [data.frame] object to mark as a `FlaggedStudyData`.
#' @inheritParams .template_function_developer
#'
#' @return `x` with the `FlaggedStudyData` class.
#' @noRd
util_new_flagged_study_data <- function(x) {
  util_expect_data_frame(x)
  class(x) <- union("FlaggedStudyData", class(x))
  x
}

#' Test if an object is a `FlaggedStudyData`
#'
#' @param x object to test.
#'
#' @return [logical] `TRUE`, if `x` inherits from `FlaggedStudyData`.
#' @noRd
util_is_flagged_study_data <- function(x) {
  inherits(x, "FlaggedStudyData")
}
