#' Internal helper: handle complex data types
#'
#' @noRd
util_handle_complex_data_types <- function(meta_data) {
  melt <- function(
    meta_data,
    rows,
    column,
    value,
    force = FALSE,
    tp,
    allow_na = FALSE
  ) {
    util_stop_if_not(is.data.frame(meta_data))
    util_expect_scalar(value, allow_na = allow_na)
    util_expect_scalar(column, check_type = is.character)
    util_expect_scalar(
      rows,
      allow_more_than_one = TRUE,
      min_length = nrow(meta_data),
      max_length = nrow(meta_data),
      check_type = is.logical
    )
    if (force) {
      util_stop_if_not(!missing(tp))
    }
    if (nrow(meta_data) == 0) {
      return(meta_data)
    }
    if (!column %in% colnames(meta_data)) {
      meta_data[[column]] <- NA_character_
    }
    if (force) {
      current_value <- meta_data[[column]]
      differs <- if (is.na(value)) {
        !is.na(current_value)
      } else {
        tolower(trimws(current_value)) != tolower(trimws(value))
      }
      if (
        column != DATA_TYPE &&
          any(
            rows &
              !util_empty(current_value) &
              differs,
            na.rm = TRUE
          )
      ) {
        util_warning(
          c(
            "Overwriting some entries in %s in",
            "the %s, because they have %s set to %s"
          ),
          dQuote(column),
          sQuote("meta_data"),
          sQuote(DATA_TYPE),
          dQuote(tp)
        )
      }
    } else {
      rows <- rows & util_empty(meta_data[[column]])
    }
    meta_data[rows, column] <- as.character(value)
    meta_data
  }

  structured_text_melt <- function(meta_data, txt_format) {
    type <- tolower(trimws(meta_data[[DATA_TYPE]]))
    fmt_rows <- type == txt_format
    if (any(fmt_rows, na.rm = TRUE)) {
      meta_data <- melt(
        meta_data,
        fmt_rows,
        DATA_TYPE,
        DATA_TYPES$STRING,
        force = TRUE,
        type
      )
      meta_data <- melt(
        meta_data,
        fmt_rows,
        SCALE_LEVEL,
        SCALE_LEVELS$`NA`,
        force = TRUE,
        type
      )
      meta_data <- melt(
        meta_data,
        fmt_rows,
        HARD_LIMITS,
        NA,
        force = TRUE,
        type,
        allow_na = TRUE
      )
      meta_data <- melt(meta_data, fmt_rows, EXTENDED_DATA_TYPE, txt_format)
      meta_data <- melt(
        meta_data,
        fmt_rows,
        "STRUCTURED_TEXT_DATA_TYPE",
        txt_format
      )
    }
    return(meta_data)
  }

  if (DATA_TYPE %in% colnames(meta_data)) {
    # Handle non-basic data types
    meta_data <- structured_text_melt(meta_data, "json")
    meta_data <- structured_text_melt(meta_data, "yaml")
    meta_data <- structured_text_melt(meta_data, "xml")

    tp <- tolower(trimws(meta_data[[DATA_TYPE]]))

    count_rows <- tp == "count"
    if (any(count_rows, na.rm = TRUE)) {
      meta_data <- melt(
        meta_data,
        count_rows,
        DATA_TYPE,
        DATA_TYPES$INTEGER,
        force = TRUE,
        tp = tp
      )
      meta_data <- melt(
        meta_data,
        count_rows,
        SCALE_LEVEL,
        SCALE_LEVELS$RATIO
      )
      meta_data <- melt(
        meta_data,
        count_rows,
        HARD_LIMITS,
        "[0; Inf)"
      )
      meta_data <- melt(
        meta_data,
        count_rows,
        EXTENDED_DATA_TYPE,
        "count",
        force = TRUE,
        tp = tp
      )
    }

    unkown <- !tp %in% DATA_TYPES
    if (any(unkown)) {
      meta_data <- .util_fix_data_types(meta_data)
    }
  }

  return(meta_data)
}
