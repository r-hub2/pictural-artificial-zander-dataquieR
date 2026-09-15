#' Get description for an indicator function
#'
#' @param fname the function name
#'
#' @return the description
#'
#' @noRd
util_function_description <- function(fname) {
  # Historical Definition/Explanation/Relevance/Guidance mapping prototype
  # removed here. Inspect with
  # `git show c93e6f7535 -- R/util_function_description.R`.
  int <-
    util_map_labels(fname,
      util_get_concept_info("implementations"),
      to = "Interpretation",
      from = "function_R",
      ifnotfound = ""
    )
  if (is.na(int) || gsub("[^a-z]", "", tolower(trimws(int))) == "na") {
    int <- ""
  }
  desc <- paste(int, sep = "\n")
  if (util_empty(desc)) {
    desc <-
      .manual$descriptions
    if (length(desc) != 1) {
      desc <- sprintf(
        "<i>No description found for <tt>%s</tt></i>",
        dQuote(fname)
      )
    }
  } else {
    if (suppressWarnings(util_ensure_suggested("markdown", err = FALSE))) {
      desc <-
        markdown::markdownToHTML(int, fragment.only = TRUE)
    }
  }
  desc
}
