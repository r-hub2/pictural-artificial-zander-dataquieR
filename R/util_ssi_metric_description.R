#' Get description for an SSI function
#'
#' @param fname the function name
#'
#' @return the description
#'
#' @noRd
util_ssi_metric_description <- function(fname) {
  # Historical Definition/Explanation/Relevance/Guidance mapping prototype
  # removed here. Inspect with
  # `git show c93e6f7535 -- R/util_function_description.R`.
  int <-
    util_map_labels(
      fname,
      util_get_concept_info("ssi"),
      to = "metric_description",
      from = "SSI_METRICS",
      ifnotfound = ""
    )
  if (is.na(int) || gsub("[^a-z]", "", tolower(trimws(int))) == "na") {
    int <- ""
  }
  desc <- paste(int, sep = "\n")
  if (util_empty(desc)) {
    desc <- sprintf(
      "<i>No description found for <tt>%s</tt></i>", dQuote(fname)
    )
  } else {
    if (suppressWarnings(util_ensure_suggested("markdown", err = FALSE))) {
      desc <- markdown::markdownToHTML(int, fragment.only = TRUE)
    }
  }
  desc
}
