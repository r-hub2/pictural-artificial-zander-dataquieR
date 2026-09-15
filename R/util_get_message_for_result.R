#' Return messages/warnings/notes/error messages for a result
#'
#' @param result a `dataquieR_resultset2` result
#' @param aspect an aspect/problem category of results
#' @param collapse either a lambda function or a separator for combining
#'                 multiple messages for the same result
#' @param ... not used
#'
#' @return hover texts for results with data quality issues,
#'         run-time errors, warnings or notes (aka messages)
#'
#' @family summary_functions
#' @concept reporting
#' @noRd
util_get_message_for_result <- function(result,
  aspect = c(
    "applicability", "error",
    "anamat", "indicator_or_descriptor"
  ),
  collapse = "\n<br />\n", ...) {
  # check if the aspect is an allowed name (robustness)
  aspect <- util_match_arg(aspect, several_ok = FALSE)

  if (aspect == "indicator_or_descriptor") {
    return("")
  }

  if (!inherits(result, "dataquieR_result")) {
    return("No results computed")
  }

  if (!(aspect %in% c("applicability", "anamat"))) {
    expected_a_result <-
      (util_get_category_for_result(result, "applicability") %in% c(cat1, cat2, cat3)) # nolint: line_length_linter.
    expected_a_result <-
      expected_a_result && !is.na(util_get_category_for_result(result, "anamat")) # nolint: line_length_linter.
  } else {
    expected_a_result <- NA
  }
  msgs <- character(0)
  messages <- util_attr(result, "message", exact = TRUE)
  warnings <- util_attr(result, "warning", exact = TRUE)
  errors <- util_attr(result, "error", exact = TRUE)
  if (length(messages) > 0) {
    for (w in messages) {
      applicability_problem <- util_attr(w, "applicability_problem", exact = TRUE) # nolint: line_length_linter.
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
      if (aspect %in% c("applicability", "anamat")) {
        if (aspect == "applicability" &&
            applicability_problem && !intrinsic_applicability_problem) {
          msgs <- c(msgs, paste(
            "<span class=\"dataquieR-message-message\">",
            gsub(
              "\n>.*$", "", # gsub("^.*?: ", "",
              conditionMessage(w)
            ),
            "</span>"
          ))
        } else if ((aspect == "anamat") &&
            applicability_problem && intrinsic_applicability_problem) {
          msgs <- c(msgs, paste(
            "<span class=\"dataquieR-message-message\">",
            gsub(
              "\n>.*$", "", # gsub("^.*?: ", "",
              conditionMessage(w)
            ),
            "</span>"
          ))
        }
      } else {
        if (!applicability_problem && !intrinsic_applicability_problem) {
          msgs <- c(msgs, paste(
            "<span class=\"dataquieR-message-message\">",
            gsub(
              "\n>.*$", "", # gsub("^.*?: ", "",
              conditionMessage(w)
            ),
            "</span>"
          ))
        }
      }
    }
  }
  if (length(warnings) > 0) {
    for (w in warnings) {
      applicability_problem <- util_attr(w, "applicability_problem", exact = TRUE) # nolint: line_length_linter.
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
      if (aspect == "applicability") {
        if (applicability_problem && !intrinsic_applicability_problem) {
          msgs <- c(msgs, paste(
            "<span class=\"dataquieR-warning-message\">",
            gsub(
              "\n>.*$", "", # gsub("^.*?: ", "",
              conditionMessage(w)
            ),
            "</span>"
          ))
        }
      } else if (aspect == "anamat") {
        if (applicability_problem && intrinsic_applicability_problem) {
          msgs <- c(msgs, paste(
            "<span class=\"dataquieR-warning-message\">",
            gsub(
              "\n>.*$", "", # gsub("^.*?: ", "",
              conditionMessage(w)
            ),
            "</span>"
          ))
        }
      } else {
        if (!applicability_problem && !intrinsic_applicability_problem) {
          msgs <- c(msgs, paste(
            "<span class=\"dataquieR-warning-message\">",
            gsub(
              "\n>.*$", "", # gsub("^.*?: ", "",
              conditionMessage(w)
            ),
            "</span>"
          ))
        }
      }
    }
  }
  if (length(errors) > 0) {
    for (w in errors) {
      applicability_problem <- util_attr(w, "applicability_problem", exact = TRUE) # nolint: line_length_linter.
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
      if (aspect %in% c("applicability", "anamat")) {
        if (aspect == "applicability" &&
            applicability_problem && !intrinsic_applicability_problem) {
          msgs <- c(msgs, paste(
            "<span class=\"dataquieR-error-message\">",
            gsub(
              "\n>.*$", "", # gsub("^.*?: ", "",
              conditionMessage(w)
            ),
            "</span>"
          ))
        } else if ((aspect == "anamat") &&
            applicability_problem && intrinsic_applicability_problem) {
          msgs <- c(msgs, paste(
            "<span class=\"dataquieR-error-message\">",
            gsub(
              "\n>.*$", "", # gsub("^.*?: ", "",
              conditionMessage(w)
            ),
            "</span>"
          ))
        }
      } else {
        if (!applicability_problem) {
          msgs <- c(msgs, paste(
            "<span class=\"dataquieR-error-message\">",
            gsub(
              "\n>.*$", "", # gsub("^.*?: ", "",
              conditionMessage(w)
            ),
            "</span>"
          ))
        }
      }
    }
  }

  msgs <- unique(msgs)

  if (is.function(collapse)) {
    collapse(rev(msgs))
  } else {
    paste0(rev(msgs), collapse = collapse)
  }
}
