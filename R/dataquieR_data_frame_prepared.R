#' Keep prepared study data attributes aligned during subsetting
#'
#' @param x a prepared data frame returned by [prep_prepare_dataframes()]
#' @param i row index
#' @param j column index
#' @param ... further arguments passed to `[.data.frame`
#' @param drop drop dimensions
#'
#' @return the subsetted data frame
#' @noRd
#' @export
`[.dataquieR_data_frame_prepared` <- function(x, i, j, ..., drop = TRUE) {
  raw_study_data <- util_attr(x, "study_data", exact = TRUE)
  has_raw_study_data <- is.data.frame(raw_study_data) &&
    identical(dim(raw_study_data), dim(x))

  y <- NextMethod("[")

  if (!is.data.frame(y)) {
    return(y)
  }

  if (has_raw_study_data) {
    raw_rows <- match(rownames(y), rownames(x))
    raw_cols <- match(names(y), names(raw_study_data))
    if (anyNA(raw_rows)) {
      attr(y, "study_data") <- NULL
    } else if (!anyNA(raw_cols)) {
      attr(y, "study_data") <- raw_study_data[raw_rows, raw_cols,
        drop = FALSE
      ]
    } else if (all(names(y) %in% names(x))) {
      raw_cols <- match(names(y), names(x))
      attr(y, "study_data") <- raw_study_data[raw_rows, raw_cols,
        drop = FALSE
      ]
    } else {
      attr(y, "study_data") <- NULL
    }
  } else {
    attr(y, "study_data") <- NULL
  }

  class(y) <- union("dataquieR_data_frame_prepared", class(y))
  y
}

#' Internal helper: as prepared data frame
#'
#' @noRd
util_as_prepared_data_frame <- function(x) {
  if (!is.data.frame(x)) {
    return(x)
  }
  class(x) <- union("dataquieR_data_frame_prepared", class(x))
  x
}
