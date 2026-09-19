#' print implementation for the class `FlaggedStudyData`
#'
#' @description
#' Use this function to print results objects of the class
#' `FlaggedStudyData`.
#'
#' @param x `FlaggedStudyData` objects to print
#' @param ... not used, yet
#'
#' @seealso base::print
#' @export
#' @return the printed object
print.FlaggedStudyData <- function(x, ...) {
  class(x) <- setdiff(class(x), "FlaggedStudyData")
  print(x)
}
