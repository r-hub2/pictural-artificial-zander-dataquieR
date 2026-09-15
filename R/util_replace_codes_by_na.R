#' Utility function to replace missing codes by `NA`s
#'
#' Substitute all missing codes in a [data.frame] by `NA`.
#'
#' @param study_data Study data including jump/missing codes as specified in the
#'                   code conventions
#' @param meta_data Metadata as specified in the code conventions
#' @param split_char Character separating missing codes
#' @param sm_code missing code for `NAs`, if they have been
#'                re-coded by `util_combine_missing_lists`
#'
#' Codes are expected to be numeric.
#'
#' @importFrom stats setNames
#' @return a list with a modified data frame and some counts
#'
#' @family missing_functions
#' @concept metadata_management
#' @noRd
util_replace_codes_by_NA <- function(study_data, meta_data = "item_level",
  split_char = SPLIT_CHAR,
  sm_code = NULL) {
  util_expect_scalar(sm_code,
    check_type = util_all_is_integer,
    allow_null = TRUE
  )
  sdf <- study_data
  mdf <- meta_data
  # data_records is edited but not explicitly copied, so we can expect the
  # MAPPED-attribute to be preserved.
  util_expect_data_frame(sdf)
  util_expect_data_frame(mdf)

  label_col <- VAR_NAMES
  if (!is.null(util_attr(sdf, "label_col", exact = TRUE))) {
    label_col <- util_attr(sdf, "label_col", exact = TRUE)
  }

  for (code_name in c(MISSING_LIST, JUMP_LIST)) {
    if (!(code_name %in% names(mdf)) || all(is.na(mdf[[code_name]]) |
          trimws(mdf[[code_name]]) == "")) {
      util_warning(
        c(
          "Metadata does not provide a filled column",
          "called %s for replacing codes with NAs."
        ),
        dQuote(code_name),
        applicability_problem = TRUE
      )
    }
  }

  # apply functions on studydata and create list
  miss <- lapply(setNames(nm = names(sdf)), util_get_code_list,
    MISSING_LIST, split_char,
    mdf = mdf, label_col = label_col, warning_if_no_list = FALSE,
    warning_if_unsuitable_list = FALSE
  )
  jump <- lapply(setNames(nm = names(sdf)), util_get_code_list,
    JUMP_LIST, split_char,
    mdf = mdf, label_col = label_col, warning_if_no_list = FALSE,
    warning_if_unsuitable_list = FALSE
  )

  # Historical missing-code and jump-code counters removed here.
  # Use mapply() and lapply() locally to recompute miss_n and jump_n.

  replace <- function(x, l, sm_code) {
    # if (lubridate::is.timepoint(x) && length(sm_code) && is.numeric(sm_code))
    if ((length(l) == 0 && length(sm_code == 0)) || !length(x)) {
      return(x)
    }
    if (lubridate::is.timepoint(x)) {
      if (!lubridate::is.timepoint(l)) {
        return(x)
      }
      x <- as.POSIXct(round(as.POSIXct(x), units = "secs"))
      x <- lubridate::with_tz(lubridate::force_tz(x))
      l <- as.POSIXct(round(as.POSIXct(l), units = "secs"))
      l <- lubridate::with_tz(lubridate::force_tz(l))
    } else if (lubridate::is.timepoint(l)) {
      return(x)
    }
    if (!is.null(sm_code)) {
      l <- unique(c(l, sm_code))
    }
    x[x %in% l] <- NA
    x
  }
  environment(replace) <- parent.env(environment())

  sdf[] <- mapply(replace,
    x = sdf, l = miss,
    MoreArgs = list(sm_code = sm_code),
    SIMPLIFY = FALSE
  )
  sdf[] <- mapply(replace,
    x = sdf, l = jump,
    MoreArgs = list(sm_code = sm_code),
    SIMPLIFY = FALSE
  )

  attr(sdf, "Codes_to_NA") <- TRUE
  attr(sdf, "MAPPED") <- util_attr(study_data, "MAPPED", exact = TRUE)

  return(sdf)
}
