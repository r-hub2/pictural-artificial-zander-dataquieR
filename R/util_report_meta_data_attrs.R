#' Get cross-item metadata stored on a report
#'
#' @param report [dataquieR_resultset2] report object
#'
#' @return a [data.frame] with cross-item-level metadata
#'
#' @noRd
util_report_meta_data_cross_item <- function(report) {
  meta_data_cross_item <- util_attr(report, "meta_data_cross_item")
  if (is.data.frame(meta_data_cross_item)) {
    return(meta_data_cross_item)
  }

  meta_data_cross <- util_attr(report, "meta_data_cross")
  if (is.data.frame(meta_data_cross)) {
    return(meta_data_cross)
  }

  data.frame()
}

#' Get metadata attributes stored on a report
#'
#' @param report [dataquieR_resultset2] report object
#'
#' @return [character] report metadata attribute names
#'
#' @noRd
util_report_meta_data_frames <- function(report) {
  meta_data_frames <- grep("^meta_data", names(attributes(report)),
    value = TRUE
  )
  if ("meta_data_cross_item" %in% meta_data_frames &&
      "meta_data_cross" %in% meta_data_frames) {
    meta_data_frames <- setdiff(meta_data_frames, "meta_data_cross")
  }
  meta_data_frames
}
