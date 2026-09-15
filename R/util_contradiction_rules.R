##### Prefixes:
# cr_vv = variable 1 value <cmp> variable 2 value OR variable 1 available
#                                                   <logic> variable 2 available
# cr_lc = variable 1 in levels <logic> variable 2 <cmp> constant value
# cr_ll = variable 1 in levels <logic> variable 1 in levels
# All functions should carry an description attribute for displaying them

######################################################################
# Detect abnormalities help functions
#
# 2 variables:
#
#    if A != B
#    if A > B
#    if A >= B
#    if A & is.na(B)
#    if A & !(is.na(B))
#    if A & B %in% {set of levels}
#    if A %in% {set of levels} & B >  value
#    if A %in% {set of levels} & B == value
#    if A %in% {set of levels} & B <  value
#    if A %in% {set of levels} & B %in% {set of levels}
#    if A %in% {set of levels} & !(B %in% {set of levels})
#
#############################
# List of generic functions #
#############################
#' Internal helper: A not equal B vv
#'
#' @noRd
A_not_equal_B_vv <- function(study_data, A, B, A_levels, B_levels,
  A_value, B_value) {
  X <- study_data

  grading <- ifelse(X[[A]] != X[[B]], 1, 0)
  return(grading)
}
attr(A_not_equal_B_vv, "description") <- "A \u2260 B"

#' Internal helper: A less than B vv
#'
#' @noRd
A_less_than_B_vv <- function(study_data, A, B, A_levels, B_levels,
  A_value, B_value) {
  X <- study_data
  util_warn_unordered(X[[A]], A)
  util_warn_unordered(X[[B]], B)
  if (is.factor(X[[A]])) {
    X[[A]] <- util_as_numeric(X[[A]])
  }
  if (is.factor(X[[B]])) {
    X[[B]] <- util_as_numeric(X[[B]])
  }

  grading <- ifelse(X[[A]] < X[[B]], 1, 0)
  return(grading)
}
attr(A_less_than_B_vv, "description") <- "A < B"

#' Internal helper: A less equal B vv
#'
#' @noRd
A_less_equal_B_vv <- function(study_data, A, B, A_levels, B_levels,
  A_value, B_value) {
  X <- study_data
  util_warn_unordered(X[[A]], A)
  util_warn_unordered(X[[B]], B)
  if (is.factor(X[[A]])) {
    X[[A]] <- util_as_numeric(X[[A]])
  }
  if (is.factor(X[[B]])) {
    X[[B]] <- util_as_numeric(X[[B]])
  }

  grading <- ifelse(X[[A]] <= X[[B]], 1, 0)
  return(grading)
}
attr(A_less_equal_B_vv, "description") <- "A \u2264 B"

#' Internal helper: A greater than B vv
#'
#' @noRd
A_greater_than_B_vv <- function(study_data, A, B, A_levels, B_levels,
  A_value, B_value) {
  X <- study_data
  util_warn_unordered(X[[A]], A)
  util_warn_unordered(X[[B]], B)
  if (is.factor(X[[A]])) {
    X[[A]] <- util_as_numeric(X[[A]])
  }
  if (is.factor(X[[B]])) {
    X[[B]] <- util_as_numeric(X[[B]])
  }

  grading <- ifelse(X[[A]] > X[[B]], 1, 0)
  return(grading)
}
attr(A_greater_than_B_vv, "description") <- "A > B"

#' Internal helper: A greater equal B vv
#'
#' @noRd
A_greater_equal_B_vv <- function(study_data, A, B, A_levels, B_levels,
  A_value, B_value) {
  X <- study_data
  util_warn_unordered(X[[A]], A)
  util_warn_unordered(X[[B]], B)
  if (is.factor(X[[A]])) {
    X[[A]] <- util_as_numeric(X[[A]])
  }
  if (is.factor(X[[B]])) {
    X[[B]] <- util_as_numeric(X[[B]])
  }

  grading <- ifelse(X[[A]] >= X[[B]], 1, 0)
  return(grading)
}
attr(A_greater_equal_B_vv, "description") <- "A \u2265 B"

#' Internal helper: A present not B vv
#'
#' @noRd
A_present_not_B_vv <- function(study_data, A, B, A_levels, B_levels,
  A_value, B_value) {
  X <- study_data
  grading <- ifelse(!is.na(X[[A]]) & is.na(X[[B]]), 1, 0)
  return(grading)
}
attr(A_present_not_B_vv, "description") <- "\u2203 A \u2227 \u2204 B"

#' Internal helper: A present and B vv
#'
#' @noRd
A_present_and_B_vv <- function(study_data, A, B, A_levels, B_levels,
  A_value, B_value) {
  X <- study_data
  grading <- ifelse(!is.na(X[[A]]) & !(is.na(X[[B]])), 1, 0)
  return(grading)
}
attr(A_present_and_B_vv, "description") <- "\u2203 A \u2227 \u2203 B"

#' Internal helper: A present and B levels vl
#'
#' @noRd
A_present_and_B_levels_vl <- function(study_data, A, B, A_levels, B_levels,
  A_value, B_value) {
  X <- study_data
  grading <- ifelse((!is.na(X[[A]])) & X[[B]] %in% B_levels, 1, 0)
  return(grading)
}
attr(A_present_and_B_levels_vl, "description") <-
  "\u2203 A \u2227 B \u2208 L\u2082"

#' Internal helper: A levels and B levels ll
#'
#' @noRd
A_levels_and_B_levels_ll <- function(study_data, A, B, A_levels, B_levels,
  A_value, B_value) {
  X <- study_data
  grading <- ifelse(X[[A]] %in% A_levels & X[[B]] %in% B_levels, 1, 0)
  return(grading)
}
attr(A_levels_and_B_levels_ll, "description") <-
  "A \u2208 L\u2081 \u2227 B \u2208 L\u2082"

#' Internal helper: A levels and B gt value lc
#'
#' @noRd
A_levels_and_B_gt_value_lc <- function(study_data, A, B, A_levels, B_levels,
  A_value, B_value) {
  X <- study_data
  util_warn_unordered(X[[B]], B)
  if (is.factor(X[[B]])) {
    X[[B]] <- util_as_numeric(X[[B]])
  }
  grading <- ifelse(X[[A]] %in% A_levels & X[[B]] > B_value, 1, 0)
  return(grading)
}
attr(A_levels_and_B_gt_value_lc, "description") <- "A \u2208 L \u2227 B > c"

#' Internal helper: A levels and B lt value lc
#'
#' @noRd
A_levels_and_B_lt_value_lc <- function(study_data, A, B, A_levels, B_levels,
  A_value, B_value) {
  X <- study_data
  util_warn_unordered(X[[B]], B)
  if (is.factor(X[[B]])) {
    X[[B]] <- util_as_numeric(X[[B]])
  }
  grading <- ifelse(X[[A]] %in% A_levels & X[[B]] < B_value, 1, 0)
  return(grading)
}
attr(A_levels_and_B_lt_value_lc, "description") <- "A \u2208 L \u2227 B < c"

#' contradiction_functions
#'
#' Detect abnormalities help functions
#'
#' 2 variables:
#'  - `A_not_equal_B`, if `A != B`
#'  - `A_greater_equal_B`, if `A >= B`
#'  - `A_greater_than_B`, if `A > B`
#'  - `A_less_than_B`, if `A < B`
#'  - `A_less_equal_B`, if `A <= B`
#'  - `A_present_not_B`, if `A & is.na(B)`
#'  - `A_present_and_B`, if `A & !(is.na(B))`
#'  - `A_present_and_B_levels`, if `A & B  %in% {set of levels}`
#'  - `A_levels_and_B_gt_value`, if `A %in% {set of levels} & B >  value`
#'  - `A_levels_and_B_lt_value`, if `A %in% {set of levels} & B <  value`
#'  - `A_levels_and_B_levels`, if
#'                             `A %in% {set of levels} & B %in% {set of levels}`
#'
#'
#' @export
#'
#' @keywords internal
contradiction_functions <- objects(pattern = ".*")
contradiction_functions <- mget(contradiction_functions)
contradiction_functions <- contradiction_functions[
  vapply(contradiction_functions, is.function, TRUE)
]
contradiction_functions <- contradiction_functions[
  vapply(contradiction_functions, function(object) {
    !is.null(util_attr(object, "description", exact = TRUE))
  }, TRUE)
]

#' description of the contradiction functions
#' @export
contradiction_functions_descriptions <- lapply(
  contradiction_functions, util_attr,
  "description"
)

######################################################################
#
#    TBD
#
#    > 2 Variablen
#
#    if B == level & A1 < A2
#    if A == level & sum(!(is.na(B))) > 0
#    if A == level & sum(!(is.na(B))) == 0
#    if A == level & sum(B) == 0
#    if abs(sum(sign(A-B))) == length(A)
#    if A1 %in% {set of levels} & sum(c(B) %in% {set of levels}) > 0
#
