#' Support function to scan variable labels for applicability
#'
#' @description
#'
#' Adjust labels in meta_data to be valid variable names in formulas for
#' diverse r functions, such as [`glm`] or [`lme4::lmer`].
#'
#' @details
#'
#' Hint: The following is still true, but the functions should be capable
#' of doing potentially needed fixes on-the-fly automatically, so likely
#' you will not need this function any more.
#'
#' Currently, labels as given by `label_col` arguments in the most functions
#' are directly used in formula, so that they become natural part of the
#' outputs, but different models expect differently strict syntax for such
#' formulas, especially for valid variable names. `prep_clean_labels` removes
#' all potentially inadmissible characters from variable names (no guarantee,
#' that some exotic model still rejects the names, but minimizing the number
#' of exotic characters). However, variable names are modified, may become
#' unreadable or indistinguishable from other variable names. For the latter
#' case, a `stop` call is possible, controlled by the `no_dups` argument.
#'
#' A warning is emitted, if modifications were necessary.
#'
#' @param label_col [character] label attribute to adjust or character vector to
#'                              adjust, depending on `meta_data` argument is
#'                              given or missing.
#' @param item_level [data.frame] metadata data frame: If `label_col` is a label
#'                               attribute to adjust, this is the metadata
#'                               table to process on. If missing, `label_col`
#'                               must be a character vector with values to
#'                               adjust.
#' @param meta_data [data.frame] old name for `item_level`
#' @param no_dups   [logical] disallow duplicates in input or output vectors of
#'                            the function, then, prep_clean_labels would call
#'                            `stop()` on duplicated labels.
#' @param meta_data_v2 [character] path to workbook like metadata file, see
#'                                 [`prep_load_workbook_like_file`] for details.
#'                                 **ALL LOADED DATAFRAMES WILL BE PURGED**,
#'                                 using [`prep_purge_data_frame_cache`],
#'                                 if you specify `meta_data_v2`.
#'
#' @return a data.frame with:
#'  - if `meta_data` is set, a list with:
#'    - modified `meta_data[, label_col]` column
#'  - if `meta_data` is not set, adjusted labels that then were directly given
#'    in label_col
#'
#' @export
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' meta_data1 <- data.frame(
#'   LABEL =
#'     c(
#'       "syst. Blood pressure (mmHg) 1",
#'       "1st heart frequency in MHz",
#'       "body surface (\\u33A1)"
#'     )
#' )
#' print(meta_data1)
#' print(prep_clean_labels(meta_data1$LABEL))
#' meta_data1 <- prep_clean_labels("LABEL", meta_data1)
#' print(meta_data1)
#' }
prep_clean_labels <- function(label_col, item_level = "item_level",
                              no_dups = FALSE, meta_data = item_level,
                              meta_data_v2) {
  util_maybe_load_meta_data_v2()
  if (missing(label_col)) {
    util_error("Need at least on paramter")
  }
  if (is.factor(label_col)) {
    label_col <- as.character(label_col)
  }
  if (!(missing(meta_data) && is.character(meta_data))) {
    util_expect_data_frame(meta_data)
  }
  if (!(missing(item_level) && is.character(item_level))) {
    util_expect_data_frame(item_level)
  }
  util_stop_if_not((is.character(label_col) && missing(meta_data) &&
                      missing(item_level)) ||
              (is.data.frame(meta_data) || is.data.frame(item_level)))
  if (!missing(meta_data)) {
    util_expect_data_frame(meta_data, c(label_col))
    orig_col <- meta_data[[label_col]]
  } else {
    orig_col <- label_col
  }
  if (no_dups && any(duplicated(orig_col))) {
    util_error("Have duplicates in desired variable labels",
               applicability_problem = TRUE)
  }
  adjusted_col <- gsub("[^a-zA-Z0-9_]+", "_", orig_col)
  adjusted_col <- gsub("^[^a-zA-Z]+", "", adjusted_col)
  if (no_dups && any(duplicated(adjusted_col))) {
    util_error(c(
      "Have duplicates in desired variable labels after",
      "adjusting them to be valid variable names"),
      applicability_problem = TRUE)
  }
  if (any(na.omit(orig_col != adjusted_col))) {
    if (!missing(meta_data)) {
      util_message("Adjusted labels in %s to be valid variable names.",
                   dQuote(label_col), applicability_problem = TRUE)
    } else {
      util_message("Adjusted labels to be valid variable names.",
                   applicability_problem = TRUE)
    }
  }
  if (!missing(meta_data)) {
    meta_data[[label_col]] <- adjusted_col
  } else {
    meta_data <- adjusted_col
  }
  meta_data
}
