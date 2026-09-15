#' Return the category for a result
#'
#' messages do not cause any category, warnings are `cat3`, errors are `cat5`
#'
#' @param result a `dataquieR_resultset2` result
#' @param aspect an aspect/problem category of results
#'               (error, applicability error)
#' @param ... not used
#'
#' @return a category, see `util_as_cat()`
#'
#' @family summary_functions
#' @noRd
util_get_category_for_result <- function(result, aspect =
    c(
      "applicability", "error",
      "anamat", "indicator_or_descriptor"
    ),
  ...) {
  aspect <- util_match_arg(aspect, several_ok = FALSE)

  ##### preps -----

  ### indicator_or_descriptor -----

  if (aspect %in% c("indicator_or_descriptor")) {
    function_name <- util_attr(result, "function_name", exact = TRUE)
    if (is.null(function_name)) { # if we do not have a result, we cannot tell
      return(util_as_cat(NA))
    }
    if (get(function_name, .indicator_or_descriptor)) {
      return(cat1) # It's a data quality indicator
    } else {
      return(cat3) # It's a descriptor
    }
    # Historical SummaryTable-based indicator/descriptor detection removed
    # here. Inspect with
    # `git show 4560cf0051 -- R/util_get_category_for_result.R`.
  } else { ##### error or applicability or anamat -----
    errors <- util_attr(result, "error", exact = TRUE)
    warnings <- util_attr(result, "warning", exact = TRUE)
    if (length(errors) > 0) { # some error occurred ----
      util_stop_if_not(length(errors) == 1)
      cnd <- errors[[1]]
      applicability_problem <- util_attr(cnd, "applicability_problem",
        exact = TRUE
      )
      if (is.null(applicability_problem) || is.na(applicability_problem)) {
        applicability_problem <- FALSE
      }
      intrinsic_applicability_problem <- util_attr(cnd,
        "intrinsic_applicability_problem",
        exact = TRUE
      )
      if (is.null(intrinsic_applicability_problem) ||
          is.na(intrinsic_applicability_problem)) {
        intrinsic_applicability_problem <- FALSE
      }
      if (aspect == "anamat" && applicability_problem &&
          intrinsic_applicability_problem) {
        return(cat5)
      } else if (aspect == "anamat" && applicability_problem &&
          !intrinsic_applicability_problem) {
        return(cat1)
      } else if (aspect == "anamat" && !applicability_problem) {
        return(cat1)
      } else if (aspect == "applicability" && applicability_problem &&
          !intrinsic_applicability_problem) {
        return(cat5) # applicability error was asked and occurred
      } else if (aspect == "error" && !applicability_problem) {
        return(cat5) # other error was asked and occurred
      }
      return(cat3) # error of the other class (not asked now) occurred
    }
    # If we have warnings, we cannot return, we have to go over all warnings
    # and return cat3, if we find a warning of the aspect's class
    res <- cat1
    if (length(warnings) > 0) { # some warning occurred ----
      for (w in warnings) {
        applicability_problem <- util_attr(w, "applicability_problem",
          exact = TRUE
        )
        if (is.null(applicability_problem) || is.na(applicability_problem)) {
          applicability_problem <- FALSE
        }
        intrinsic_applicability_problem <- util_attr(w,
          "intrinsic_applicability_problem",
          exact = TRUE
        )
        if (is.null(intrinsic_applicability_problem) ||
            is.na(intrinsic_applicability_problem)) {
          intrinsic_applicability_problem <- FALSE
        }
        if (aspect == "anamat" && applicability_problem &&
            intrinsic_applicability_problem) {
          # Intrinsic applicability warnings do not worsen `anamat`.
        } else if (aspect == "anamat" && applicability_problem &&
            !intrinsic_applicability_problem) {
          # Non-intrinsic applicability warnings do not worsen `anamat`.
        } else if (aspect == "anamat" && !applicability_problem) {
          # Non-applicability warnings do not worsen `anamat`.
        } else if (aspect == "applicability") {
          if (applicability_problem && !intrinsic_applicability_problem) {
            return(cat3)
          }
        } else if (aspect == "error") {
          if (!applicability_problem) {
            return(cat3)
          }
        } else { # nocov start
          util_error(
            "internal error in get color for result: %s",
            dQuote(aspect)
          )
        } # nocov end
      }
    }
    return(cat1)
  }
}
