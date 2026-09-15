#' Creates links to the online documentation website
#'
#' Rendering must be safe in offline environments. Therefore this helper is
#' purely deterministic and never checks whether the website is reachable.
#' Browser-side JavaScript may disable the rendered link while the report viewer
#' is offline.
#'
#' @param fkt_name [character] function name to generate a link for
#' @param target [character] `implementation` or `concept`
#'
#' @return [character] the link
#'
#' @noRd
util_online_ref <- function(fkt_name, target = "implementation") {
  util_expect_scalar(fkt_name, check_type = is.character)
  util_expect_scalar(target, check_type = is.character)
  target <- match.arg(target, c("implementation", "concept"))

  refs <- util_online_ref_candidates(fkt_name)
  refs[[target]]
}

#' @noRd
util_online_ref_candidates <- function(fkt_name) {
  util_expect_scalar(fkt_name, check_type = is.character)

  if (!fkt_name %in% util_all_ind_functions()) {
    util_error(
      paste0(
        "Internal error, sorry, please report: %s must be a known indicator ",
        "or descriptor function name."
      ),
      sQuote("fkt_name")
    )
  }

  fkt_name_parts <- strsplit(fkt_name, "_", fixed = TRUE)[[1]]

  concept_name <- paste(c("VIN", fkt_name_parts), collapse = "_")
  implementation_name <- paste(
    c("VIN", head(fkt_name_parts, 1), "impl", tail(fkt_name_parts, -1)),
    collapse = "_"
  )

  list(
    concept = sprintf(
      "https://dataquality.qihs.uni-greifswald.de/%s.html",
      concept_name
    ),
    implementation = sprintf(
      "https://dataquality.qihs.uni-greifswald.de/%s.html",
      implementation_name
    )
  )
}
