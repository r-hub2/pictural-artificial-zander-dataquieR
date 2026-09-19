#' Check structured text data for syntax errors
#'
#' `r lifecycle::badge("experimental")`
#'
#' @description
#' This function checks syntax for entries whose `EXTENDED_DATA_TYPE` is JSON,
#' YAML, or XML, using the corresponding format parser.
#'
#' [Indicator]
#'
#' @inheritParams .template_function_indicator
#'
#' @return A [list] with:
#'   - `SummaryTable`: a [data.frame] containing data quality checks for
#'                     "Invalid syntax rate" for each response
#'                     variable in `resp_vars`.
#'   - `SummaryData`: a [data.frame] containing data quality checks for
#'                    “Invalid syntax rate” for a report
##'
#' @export
#'

int_invalid_syntax <- function(
  resp_vars,
  study_data,
  label_col = NULL,
  item_level = "item_level",
  meta_data = item_level,
  meta_data_v2
) {
  util_maybe_load_meta_data_v2()
  prep_prepare_dataframes()

  if (!("STRUCTURED_TEXT_DATA_TYPE" %in% names(meta_data))) {
    return(list(
      SummaryTable = data.frame(Variables = NULL),
      SummaryData = data.frame(Variables = NULL)
    ))
  }

  SUPPORTED_DATA_FORMATS <- c("json", "xml", "yaml")

  if (missing(resp_vars) || length(resp_vars) == 0) {
    resp_vars <- meta_data[
      meta_data[["STRUCTURED_TEXT_DATA_TYPE"]] %in% SUPPORTED_DATA_FORMATS, ,
      drop = FALSE
    ]
    resp_vars <- resp_vars[[label_col]]
  } else {
    # if given the resp_vars, check if they are valid
    resp_var_meta <- meta_data[
      meta_data[[label_col]] %in% resp_vars, ,
      drop = FALSE
    ]
    if (
      !(any(
        resp_var_meta[["STRUCTURED_TEXT_DATA_TYPE"]] %in% SUPPORTED_DATA_FORMATS
      ))
    ) {
      util_error(
        paste0(
          "All specified variables have no ",
          "or an incorrect data type set for ",
          "structured text validation"
        ),
        applicability_problem = TRUE
      )
    }
    if (
      !(all(
        resp_var_meta[["STRUCTURED_TEXT_DATA_TYPE"]] %in% SUPPORTED_DATA_FORMATS
      ))
    ) {
      culprits <- resp_var_meta[
        !(
          resp_var_meta[["STRUCTURED_TEXT_DATA_TYPE"]] %in%
            SUPPORTED_DATA_FORMATS
        ), ,
        drop = FALSE
      ][[label_col]]
      util_warning(
        paste0(
          "Variables ",
          paste(culprits, collapse = ", "),
          "have unsupported data formats",
          "and cannot be processed"
        ),
        applicability_problem = TRUE
      )
      resp_vars <- resp_var_meta[
        resp_var_meta[["STRUCTURED_TEXT_DATA_TYPE"]] %in%
          SUPPORTED_DATA_FORMATS, ,
        drop = FALSE
      ][[label_col]]
    }
  }

  suggested_libs <- function(text_fmt, lib_name) {
    if (
      any(
        meta_data[meta_data[[label_col]] %in% resp_vars, , drop = FALSE][[
          "STRUCTURED_TEXT_DATA_TYPE"
        ]] ==
          text_fmt
      )
    ) {
      util_ensure_suggested(
        lib_name,
        sprintf("To check for syntax errors in %s", text_fmt)
      )
    }
  }

  suggested_libs("json", "jsonlite")
  suggested_libs("xml", "xml2")
  suggested_libs("yaml", "yaml12")

  fn_for_variable <- function(the_var) {
    txt_fmt <- meta_data[meta_data[[label_col]] == the_var, , drop = FALSE][[
      "STRUCTURED_TEXT_DATA_TYPE"
    ]]
    switch(txt_fmt,
      # TODO: what if ds1[1, the_var] == NA?
      json = if (.util_file_or_json(ds1[1, the_var, drop = TRUE]) == "json") {
        jsonlite::parse_json
      } else {
        jsonlite::read_json
      },
      xml = function(...) {
        # read_xml() distinguishes XML text from file paths internally.
        xml2::read_xml(..., options = "PEDANTIC")
      },
      yaml = yaml12::parse_yaml
    )
  }

  result <- vapply(
    resp_vars,
    FUN = function(the_var) {
      parsing_fn <- fn_for_variable(the_var)
      errors <- vapply(
        ds1[[the_var]],
        FUN = function(var_content) {
          if (util_empty(var_content)) {
            return(FALSE)
          }
          tryCatch(
            error = function(x) {
              return(TRUE)
            },
            warning = function(x) {
              return(TRUE)
            },
            {
              result <- parsing_fn(var_content)
              FALSE
            }
          )
        },
        FUN.VALUE = logical(1)
      )
      sum(errors, na.rm = TRUE)
    },
    FUN.VALUE = numeric(1)
  )

  result_pct <- 100 * (result / length(ds1[[resp_vars[1]]]))

  SummaryTable <- data.frame(
    Variables = resp_vars,
    NUM_int_dsc_format = result, # unexpected data set format
    PCT_int_dsc_format = result_pct,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  SummaryData <- data.frame(
    Variables = resp_vars,
    `Number of items with an unexpected data set format` = result,
    `Percentage of items with unexpe` = sprintf("%.1f%%", result_pct),
    stringsAsFactors = FALSE,
    row.names = NULL,
    check.names = FALSE
  )

  return(list(SummaryTable = SummaryTable, SummaryData = SummaryData))
}

#' Distinguish inline JSON from a file path
#'
#' @param the_string A character scalar containing JSON or a path.
#' @return Either `"json"` or `"path"`.
#' @noRd
.util_file_or_json <- function(the_string) {
  util_expect_scalar(the_string, check_type = is.character)
  the_string <- trimws(the_string)
  # Starts with { -> json object
  if (
    startsWith(the_string, "{") || # object
      startsWith(the_string, "[") || # array
      startsWith(the_string, "\"") || # string
      the_string == "true" || # bool
      the_string == "false" || # bool
      grepl(
        r"[^-?\d+(\.\d+)?(e-?\d+)?$]", # number
        the_string,
        perl = TRUE
      )
  ) {
    "json"
  } else {
    "path"
  }
}
