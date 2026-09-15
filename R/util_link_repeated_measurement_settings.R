#' Link result-table references to their metadata tables
#'
#' @param tb [data.frame] report-facing result table.
#'
#' @return [data.frame] with linked reference columns, if applicable.
#' @noRd
util_link_result_references <- function(tb) {
  tb <- util_link_repeated_measurement_settings(tb)
  tb
}

#' Link repeated-measurement result settings to their metadata table
#'
#' @param tb [data.frame] report-facing result table.
#'
#' @return [data.frame] with escaped content and linked setting IDs.
#' @noRd
util_link_repeated_measurement_settings <- function(tb) {
  setting_cols <- intersect(
    c("Repeated-measurement setting", "repeated_measures_metric_setting"),
    names(tb)
  )
  if (length(setting_cols) == 0) {
    return(tb)
  }

  tb <- util_df_escape(tb)
  setting_col <- setting_cols[[1]]
  settings <- as.character(tb[[setting_col]])
  has_setting <- !util_empty(settings)
  if (!any(has_setting)) {
    attr(tb, "is_html_escaped") <- TRUE
    return(tb)
  }

  setting_text <- settings[has_setting]
  href <- paste0(
    prep_link_escape("statistical_settings", html = TRUE),
    ".html?dq_filter_col=SETTING_ID&dq_filter_value=",
    utils::URLencode(setting_text, reserved = TRUE)
  )
  tb[[setting_col]][has_setting] <- vapply(
    mapply(
      href = href,
      text = setting_text,
      SIMPLIFY = FALSE,
      FUN = function(href, text) {
        htmltools::a(href = href, text)
      }
    ),
    as.character,
    FUN.VALUE = character(1)
  )
  attr(tb, "is_html_escaped") <- TRUE
  tb
}
