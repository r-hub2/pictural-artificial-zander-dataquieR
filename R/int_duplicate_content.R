# nolint start: line_length_linter.
#' Check for duplicated content
#'
#' @description
#' This function tests for duplicates entries in the data set. It is possible to
#' check duplicated entries by study segments or to consider only selected
#' segments.
#'
#' [Indicator]
#'
#' @param level [character] a character vector indicating whether the assessment should be conducted at the study level (level = "dataframe") or at the segment level (level = "segment").
#' @param ... Depending on `level`, passed to either
#'            `util_int_duplicate_content_segment` or
#'            `util_int_duplicate_content_dataframe`
#'
#' @inheritParams .template_function_indicator
#'
#' @return a [list]. Depending on `level`, see
#'   `util_int_duplicate_content_segment` or
#'   `util_int_duplicate_content_dataframe` for a description of the outputs.
#'
#' @export
# nolint end
int_duplicate_content <- function(level = c("dataframe", "segment"),
  study_data,
  item_level = "item_level",
  label_col,
  meta_data = item_level,
  meta_data_v2,
  ...) {
  util_maybe_load_meta_data_v2()
  level <- util_match_arg(level)
  util_int_level_dispatch(
    fname = paste("util", rlang::call_name(rlang::frame_call()), level,
      sep = "_"
    ),
    level = level,
    study_data = if (missing(study_data)) NULL else study_data,
    has_study_data = !missing(study_data),
    item_level = item_level,
    has_item_level = !missing(item_level),
    label_col = if (missing(label_col)) NULL else label_col,
    has_label_col = !missing(label_col),
    meta_data = meta_data,
    has_meta_data = !missing(meta_data),
    include_item_level = FALSE,
    ...
  )
}
