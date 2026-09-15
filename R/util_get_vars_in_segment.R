#' Return all variables in the segment `segment`
#'
#' @param segment [character] the segment as specified in `STUDY_SEGMENT`
#' @param meta_data [data.frame] the metadata
#' @param label_col [character] the metadata attribute used for naming the
#'                              variables
#'
#' @return vector of variable names
#'
#' @family metadata_management
#' @concept metadata_management
#' @noRd


util_get_vars_in_segment <- function(segment, meta_data = "item_level",
  label_col = LABEL) {
  util_expect_scalar(segment, check_type = is.character)
  util_expect_data_frame(meta_data, list(
    STUDY_SEGMENT = is.character,
    VAR_NAMES = is.character
  ))
  if (!(LABEL %in% colnames(meta_data))) {
    meta_data[[LABEL]] <- meta_data[[VAR_NAMES]]
  }
  def <- list(
    STUDY_SEGMENT = is.character,
    VAR_NAMES = is.character,
    LABEL = is.character
  )
  def[[label_col]] <- is.character
  util_expect_data_frame(meta_data, def)
  kss <- meta_data[[STUDY_SEGMENT]]
  # Historical segment-label mapping variants removed in commit 214dd76a7d.
  r <-
    unique(c(
      meta_data[kss == segment, label_col, drop = TRUE]
      # Historical mapped segment matches removed in commit 214dd76a7d.
    ))
  sort(r)
}
