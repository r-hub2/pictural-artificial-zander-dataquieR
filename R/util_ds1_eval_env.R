#' Create an environment with several alias names for the study data variables
#'
#' generates an environment similar to `as.environment(ds1)`, but makes
#' variables available by their `VAR_NAME`, `LABEL`, and `label_col` - names.
#'
#' @inheritParams .template_function_developer
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#'
#' @family rule_functions
#' @concept process
#' @noRd
util_ds1_eval_env <- function(study_data,
  meta_data = "item_level",
  label_col = LABEL) {
  if (isTRUE(util_attr(study_data, "MAPPED", exact = TRUE))) {
    ds1 <- study_data
  } else {
    prep_prepare_dataframes()
  }
  label_col_from <- util_attr(ds1, "label_col", exact = TRUE)
  label_col_to <- label_col
  res <- ds1
  lct <- setdiff(c(VAR_NAMES, LABEL, LONG_LABEL, label_col_to), label_col_from)
  lct <- intersect(lct, colnames(meta_data))
  for (cur_nm in lct) {
    res[, util_map_labels(colnames(ds1),
        from = label_col_from,
        to = cur_nm,
        meta_data = meta_data
      )] <-
      ds1[, colnames(ds1), FALSE]
  }
  as.environment(res)
}
