#' Check structured text data for schema nonadherence
#'
#' `r lifecycle::badge("experimental")`
#'
#' @description
#' This function checks adherence to `STRUCTURED_TEXT_SCHEMA` for entries
#' whose `EXTENDED_DATA_TYPE` is JSON or XML. It checks data against the schema
#' or to a subschema pointed to by STRUCTURED_TEXT_SCHEMA_REFERENCE.
#' A reference starts with the schema filename when read from a file
#' (e.g. `my_schema.json#/definitions/MyDefinition`) and with `#` otherwise
#' (e.g. `#/definitions/MyDefinition`).
#'
#' [Indicator]
#'
#' @inheritParams .template_function_indicator
#'
#' @return A [list] with:
#'   - `SummaryTable`: a [data.frame] containing data quality checks for
#'                     a variety of data quality indicators mapped from
#'                     corresponding JSON-schema keywords for each response
#'                     variable in `resp_vars`. In the case of XML, just
#'                     general nonadherence is tested.
#'
#'   - `ReportSummaryTable`: a [data.frame] for the heatmap.
#'
#'   - `SummaryData`: a [data.frame] with more readable column names.
#'
##'
#' @export
#'

con_schema_nonadherence <- function(
  resp_vars,
  study_data,
  label_col = NULL,
  item_level = "item_level",
  meta_data = item_level,
  meta_data_v2,
  segment_level,
  meta_data_segment = "segment_level"
) {
  util_maybe_load_meta_data_v2()
  prep_prepare_dataframes()
  meta_data_segment <- prep_check_meta_data_segment(meta_data_segment)

  if (!("STRUCTURED_TEXT_DATA_TYPE" %in% names(meta_data))) {
    util_error(
      "Data has no structured text data type (json or xml)",
      intrinsic_applicability_problem = TRUE,
      applicability_problem = TRUE
    )
  }
  if (!(STRUCTURED_TEXT_SCHEMA %in% names(meta_data))) {
    util_error(
      "Data has no attached validation schema",
      applicability_problem = TRUE
    )
  }

  SUPPORTED_DATA_FORMATS <- c("json", "xml")
  if (missing(resp_vars) || length(resp_vars) == 0) {
    resp_vars <- meta_data[
      meta_data[["STRUCTURED_TEXT_DATA_TYPE"]] %in% SUPPORTED_DATA_FORMATS, ,
      drop = FALSE
    ]
    resp_vars <- resp_vars[
      !util_empty(resp_vars[[STRUCTURED_TEXT_SCHEMA]]), ,
      drop = FALSE
    ]
    resp_vars <- resp_vars[[label_col]]
  } else {
    # if given the resp_vars, check if they are valid
    # TODO: Maybe enhance and use util_correct_variable_use
    #       though this code will only be needed here (?)
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
        !(resp_var_meta[["STRUCTURED_TEXT_DATA_TYPE"]] %in%
            SUPPORTED_DATA_FORMATS), ,
        drop = FALSE
      ][[label_col]]
      util_message(
        paste0(
          "Variables ",
          paste(culprits, collapse = ", "),
          "have unsupported data formats",
          "and will not be processed"
        ),
        applicability_problem = TRUE
      )
      resp_vars <- resp_var_meta[
        resp_var_meta[["STRUCTURED_TEXT_DATA_TYPE"]] %in%
          SUPPORTED_DATA_FORMATS, ,
        drop = FALSE
      ][[label_col]]
      resp_var_meta <- resp_var_meta[
        resp_var_meta[[label_col]] %in% resp_vars, , drop = FALSE
      ]
    }
    if (all(util_empty(resp_var_meta[[STRUCTURED_TEXT_SCHEMA]]))) {
      util_error(
        paste0(
          "No schema given for any of the",
          "structured text variables"
        ),
        applicability_problem = TRUE
      )
    }
    if (any(util_empty(resp_var_meta[[STRUCTURED_TEXT_SCHEMA]]))) {
      culprits <- resp_var_meta[
        util_empty(resp_var_meta[[STRUCTURED_TEXT_SCHEMA]]), ,
        drop = FALSE
      ][[label_col]]
      util_message(
        paste0(
          "Variables ",
          paste(culprits, collapse = ", "),
          "have no schema specified",
          "and will not be processed"
        ),
        applicability_problem = TRUE
      )
      resp_vars <- resp_var_meta[
        !(util_empty(resp_var_meta[[STRUCTURED_TEXT_SCHEMA]])), ,
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
        sprintf("To check for schema adherence in %s", text_fmt)
      )
    }
  }

  suggested_libs("json", "jsonvalidate")
  suggested_libs("xml", "xml2")

  get_text_data_type <- function(the_var) {
    meta_data[meta_data[[label_col]] == the_var, , drop = FALSE][[
      "STRUCTURED_TEXT_DATA_TYPE"
    ]]
  }

  get_json_schema_reference <- function(the_var) {
    if ("STRUCTURED_TEXT_SCHEMA_REFERENCE" %in% names(meta_data)) {
      ref <- meta_data[meta_data[[label_col]] == the_var, , drop = FALSE][[
        "STRUCTURED_TEXT_SCHEMA_REFERENCE"
      ]]
      if (util_empty(ref)) {
        NULL
      } else {
        ref
      }
    } else {
      NULL
    }
  }

  # AJV error spec: https://ajv.js.org/api.html#validation-errors
  # xml2 errors are less specific and come from the libxml2 source code.

  # jsonvalidate results: attr(res, "errors")[["keyword"]] (is character(n))
  # xml2 results: attr(res, "errors") is a list of error messages

  fn_for_variable <- function(the_var) {
    txt_fmt <- get_text_data_type(the_var)
    switch(txt_fmt,
      json = function(data, schema, cache_env) {
        if (is.null(cache_env$json_validator)) {
          #
          # ref is a reference to some definition in the json
          # schema. Because of a quirky workaround in
          # jsonvalidate, the correct path to some definition
          # starts with the filename.
          # E.g. "my_schema.json#/definitions/Patient"
          #
          ref <- get_json_schema_reference(the_var)
          cache_env$json_validator <-
            jsonvalidate::json_schema$new(
              schema = schema,
              engine = "ajv",
              strict = as.logical(getOption(
                "dataquieR.json.schema.strict.mode",
                dataquieR.json.schema.strict.mode_default
              )),
              reference = ref
            )
        }
        # TODO: throw unrelated errors if possible
        error_strings <- tryCatch(
          error = function(cnd) {
            c("SyntaxErrors")
          },
          {
            res <- cache_env$json_validator$validate(
              json = data,
              verbose = TRUE,
              greedy = TRUE
            )
            util_attr(res, "errors", exact = TRUE)[["keyword"]]
          }
        )
        .util_make_result_table(error_strings, txt_fmt)
      },
      xml = function(data, schema, cache_env) {
        xml_data <- xml2::read_xml(data, options = "PEDANTIC")
        if (is.null(cache_env$xml_schema)) {
          cache_env$xml_schema <- xml2::read_xml(schema, options = "PEDANTIC")
        }
        error_strings <- tryCatch(
          error = function(cnd) {
            if (inherits(cnd, "simpleError") || inherits(cnd, "rlang_error")) {
              # Broken XML. Coarse because of bad error messages
              c("SyntaxErrors")
            } else {
              util_error(cnd)
            }
          },
          {
            res <- xml2::xml_validate(xml_data, cache_env$xml_schema)
            util_attr(res, "errors", exact = TRUE)
          }
        )
        .util_make_result_table(error_strings, txt_fmt)
      }
    )
  }

  schema_for_var <- function(the_var) {
    meta_data[
      meta_data[[label_col]] == the_var,
      STRUCTURED_TEXT_SCHEMA,
      drop = TRUE
    ]
  }

  validation_output <- lapply(
    resp_vars,
    FUN = function(the_var) {
      cache_env <- rlang::env()
      withr::defer(rm(cache_env))
      validating_fn <- fn_for_variable(the_var)
      schema <- schema_for_var(the_var)
      txt_fmt <- get_text_data_type(the_var)
      var_data <- ds1[[the_var]][!util_empty(ds1[[the_var]])]
      validation_data <- lapply(var_data, function(x) {
        return(validating_fn(x, schema, cache_env))
      })
      validation_data
    }
  )

  names(validation_output) <- resp_vars

  indices_with_errors <- lapply(
    validation_output,
    FUN = function(the_output) {
      summed <- lapply(the_output, sum)
      which(summed != 0)
    }
  )

  dqi <- util_get_concept_info("dqi")

  errors_table <- Reduce(
    function(err_df, resp_var) {
      if (!length(indices_with_errors[[resp_var]])) {
        return(err_df)
      }
      study_seg <- meta_data[
        meta_data[[label_col]] == resp_var, ,
        drop = FALSE
      ][[STUDY_SEGMENT]]
      id_var <- meta_data_segment[
        meta_data_segment[[STUDY_SEGMENT]] == study_seg, ,
        drop = FALSE
      ][[SEGMENT_ID_VARS]]
      id_var <- util_map_labels(id_var,
        meta_data = meta_data, to = label_col
      )
      if (grepl("|", id_var, fixed = TRUE)) {
        util_warning(paste0(
          "Study segment SEGMENT_ID_VARS with",
          "pipe not yet supported in con_schema_nonadherence"
        ))
        id_var <- NULL
      }
      tryCatch(
        error = function() {
          util_warning(paste0(
            "Study segment SEGMENT_ID_VARS with",
            "labels other than VAR_NAMES not yet supported in",
            "con_schema_nonadherence"
          ))
          id_var <- NULL
        },
        {
          test <- ds1[[id_var]]
          test <- NULL
        }
      )
      data <- validation_output[[resp_var]][indices_with_errors[[resp_var]]]
      data <- lapply(data, unclass)
      data <- as.data.frame(data, stringsAsFactors = FALSE)
      rn <- rownames(data)
      colnames(data) <- NULL
      data <- data[endsWith(rownames(data), "file_total"), , drop = FALSE]
      rownames(data) <- gsub("_file_total", "", rownames(data), fixed = TRUE)
      rowname_selector <- rownames(data) %in% dqi[["abbreviation"]]
      rownames(data)[rowname_selector] <- vapply(
        rownames(data)[rowname_selector],
        FUN = function(abbr) {
          dqi[dqi[["abbreviation"]] == abbr, , drop = TRUE][["Name"]][[1]]
        },
        FUN.VALUE = character(1)
      )
      data <- rbind(ds1[
        indices_with_errors[[resp_var]], ,
        drop = FALSE
      ][[id_var]], data)
      data <- rbind(indices_with_errors[[resp_var]], data)
      data <- rbind(resp_var, data)
      rownames(data) <- c(
        label_col,
        "Index",
        "ID",
        rownames(data)[4:length(rownames(data))]
      )
      data <- as.data.frame(t(data), stringsAsFactors = FALSE)
      if (!is.null(err_df)) {
        data <- rbind(err_df, data)
      }
      data
    },
    resp_vars,
    init = NULL
  )

  if (is.null(errors_table)) {
    errors_table <- data.frame()
  }
  FlaggedStudyData <- util_new_flagged_study_data(errors_table)
  for (cname in setdiff(colnames(FlaggedStudyData), c("ID", label_col))) {
    attr(FlaggedStudyData[[cname]], DATA_TYPE) <- DATA_TYPES$INTEGER
  }
  if ("ID" %in% colnames(FlaggedStudyData)) {
    attr(FlaggedStudyData[["ID"]], DATA_TYPE) <- DATA_TYPES$STRING
  }
  if (label_col %in% colnames(FlaggedStudyData)) {
    attr(FlaggedStudyData[[label_col]], DATA_TYPE) <- DATA_TYPES$STRING
  }

  result <- vapply(
    resp_vars,
    FUN = function(the_var) {
      txt_fmt <- get_text_data_type(the_var)
      Reduce(
        `+`,
        validation_output[[the_var]],
        init = .util_make_result_table(NULL, txt_fmt)
      )
    },
    FUN.VALUE = double(20)
  )

  result_for <- function(metric) {
    stats::setNames(
      as.numeric(result[metric, , drop = TRUE]),
      colnames(result)
    )
  }

  syntax_cnt <- result_for("syntax error")
  if (any(syntax_cnt != 0)) {
    util_message(
      paste0(
        "Found syntax errors in %s structured text ",
        "files while validating them. Ignoring those."
      ),
      sum(syntax_cnt)
    )
  }

  counts <- vapply(
    resp_vars,
    function(var) {
      l <- length(ds1[[var]])
      l <- l - sum(util_empty(ds1[[var]]))
      l <- l - syntax_cnt[[var]]
      l
    },
    FUN.VALUE = numeric(1)
  )


  mk_percent <- function(x) {
    vapply(
      names(x),
      function(nm) {
        l <- counts[[nm]]
        100 * (x[[nm]] / l)
      },
      FUN.VALUE = numeric(1)
    )
  }

  error_cnt <- result_for("has error")
  error_pct <- mk_percent(error_cnt)

  con_con_contc <- result_for("con_con_contc")
  con_rvv_inum <- result_for("con_rvv_inum")
  con_rvv_icat <- result_for("con_rvv_icat")
  int_sts_countel <- result_for("int_sts_countel")
  int_sts_element <- result_for("int_sts_element")
  int_sts_structure <- result_for("int_sts_structure")
  int_vfe_inhom <- result_for("int_vfe_inhom")
  int_vfe_type <- result_for("int_vfe_type")
  uncategorized <- result_for("uncategorized error")

  repsumtab <- data.frame(
    Variables = resp_vars,
    N = counts,
    `Total errors (Number)` = error_cnt,
    NUM_con_con_contc = con_con_contc,
    NUM_con_rvv_inum = con_rvv_inum,
    NUM_con_rvv_icat = con_rvv_icat,
    NUM_int_sts_countel = int_sts_countel,
    NUM_int_sts_element = int_sts_element,
    NUM_int_sts_structure = int_sts_structure,
    NUM_int_vfe_inhom = int_vfe_inhom,
    NUM_int_vfe_type = int_vfe_type,
    `Uncategorized errors (Number)` = uncategorized,
    stringsAsFactors = FALSE,
    row.names = NULL,
    check.names = FALSE
  )

  colnames(repsumtab) <- util_translate_indicator_metrics(
    colnames(repsumtab),
    ignore_unknown = TRUE
  )

  ReportSummaryTable <- util_new_report_summary_table(
    repsumtab,
    meta_data,
    label_col
  )

  for (cname in setdiff(colnames(ReportSummaryTable), c("Variables"))) {
    attr(ReportSummaryTable[[cname]], DATA_TYPE) <- DATA_TYPES$INTEGER
  }
  attr(ReportSummaryTable[["Variables"]], DATA_TYPE) <- DATA_TYPES$STRING


  SummaryTable <- data.frame(
    Variables = resp_vars,
    NUM_total_errors = error_cnt,
    PCT_total_errors = error_pct,
    NUM_con_con_contc = con_con_contc,
    NUM_con_rvv_inum = con_rvv_inum,
    NUM_con_rvv_icat = con_rvv_icat,
    NUM_int_sts_countel = int_sts_countel,
    NUM_int_sts_element = int_sts_element,
    NUM_int_sts_structure = int_sts_structure,
    NUM_int_vfe_inhom = int_vfe_inhom,
    NUM_int_vfe_type = int_vfe_type,
    NUM_uncategorized = uncategorized,
    PCT_con_con_contc = mk_percent(con_con_contc),
    PCT_con_rvv_inum = mk_percent(con_rvv_inum),
    PCT_int_sts_countel = mk_percent(int_sts_countel),
    PCT_int_sts_element = mk_percent(int_sts_element),
    PCT_int_sts_structure = mk_percent(int_sts_structure),
    PCT_int_vfe_inhom = mk_percent(int_vfe_inhom),
    PCT_int_vfe_type = mk_percent(int_vfe_type),
    PCT_uncategorized = mk_percent(uncategorized),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  for (cname in setdiff(colnames(SummaryTable), c("Variables"))) {
    if (startsWith(cname, "NUM")) {
      attr(SummaryTable[[cname]], DATA_TYPE) <- DATA_TYPES$INTEGER
    } else if (startsWith(cname, "PCT")) {
      attr(SummaryTable[[cname]], DATA_TYPE) <- DATA_TYPES$FLOAT
    }
  }
  attr(SummaryTable[["Variables"]], DATA_TYPE) <- DATA_TYPES$STRING


  SummaryData <- data.frame(
    Variables = resp_vars,
    `Number of items that violate the specified schema` = error_cnt,
    `Percentage of items that violate the specified schema` = sprintf(
      "%.3f%%",
      error_pct
    ),
    `Number of items with logical contradictions` = con_con_contc,
    `Number of items with inadmissible numerical values` = con_rvv_inum,
    `Number of items with inadmissible categorial values` = con_rvv_icat,
    `Number of items with unexpected data element count` = int_sts_countel,
    `Number of items with unexpected data element set` = int_sts_element,
    `Number of items with unexpected data set structure` = int_sts_structure,
    `Number of items with inhomogeneous value formats` = int_vfe_inhom,
    `Number of items with data type mismatch` = int_vfe_type,
    `Average number of uncategorized errors per observation` = uncategorized,
    stringsAsFactors = FALSE,
    row.names = NULL,
    check.names = FALSE
  )

  for (cname in setdiff(colnames(SummaryData), c("Variables"))) {
    if (startsWith(cname, "Number")) {
      attr(SummaryData[[cname]], DATA_TYPE) <- DATA_TYPES$INTEGER
    } else if (startsWith(cname, "Percentage")) {
      attr(SummaryData[[cname]], DATA_TYPE) <- DATA_TYPES$FLOAT
    } else if (startsWith(cname, "Average")) {
      attr(SummaryData[[cname]], DATA_TYPE) <- DATA_TYPES$FLOAT
    }
  }

  attr(SummaryData[["Variables"]], DATA_TYPE) <- DATA_TYPES$STRING


  list(
    SummaryTable = SummaryTable,
    ReportSummaryTable = ReportSummaryTable,
    SummaryData = SummaryData,
    FlaggedStudyData = FlaggedStudyData
  )
}

#
# AJV error spec: https://ajv.js.org/api.html#validation-errors
# xml2 does not provide any specification and is thus unclassified for now.
# Its source code may reveal the error types, but spans many lines of C:
# https://gitlab.gnome.org/GNOME/libxml2/-/blob/master/xmlschemas.c?ref_type=heads # nolint: line_length_linter.
#
# There is no output for if-else errors: AJV cannot sensibly classify them.
#
# con_con_contc     - Logical Contradictions
# con_rvv_inum      - Inadmissible numerical values
# int_sts_countel   - Unexpected data element count
# int_sts_element   - Unexpected data element set
# int_sts_structure - Unexpected data set structure
# int_vfe_inhom     - Inhomogeneous value formats
# int_vfe_type      - Data type mismatch
#
# uncategorized error
#

#' Map a structured-text validation error to a data-quality indicator
#'
#' @param data_type The structured-text format, JSON or XML.
#' @param error_str The validator's error category.
#' @return The corresponding indicator abbreviation or error category.
#' @noRd
.util_classify_validation_error <- function(
  data_type = c("json", "xml"),
  error_str
) {
  data_type <- util_match_arg(data_type)
  switch(data_type,
    json = {
      switch(error_str,
        type = "int_vfe_type", # Data type mismatch
        maxItems = "int_sts_countel", # Unexpected data element count
        minItems = "int_sts_countel", # Unexpected data element count
        maxLength = "int_vfe_inhom", # Inhomogeneous value formats
        minLength = "int_vfe_inhom", # Inhomogeneous value formats
        maxProperties = "int_sts_countel", # Unexpected data element count
        minProperties = "int_sts_countel", # Unexpected data element count
        additionalItems = "int_sts_countel", # Unexpected data element count
        additionalProperties = "int_sts_element", # Unexpected data element set
        dependencies = "con_con_contc", # Logical Contradictions
        # https://www.learnjsonschema.com/2020-12/format-annotation/format/
        # e.g. datetime
        format = "int_vfe_type", # Data type mismatch
        maximum = "con_rvv_inum", # Inadmissible numerical values
        minimum = "con_rvv_inum", # Inadmissible numerical values
        exclusiveMaximum = "con_rvv_inum", # Inadmissible numerical values
        exclusiveMinimum = "con_rvv_inum", # Inadmissible numerical values
        multipleOf = "con_rvv_inum", # Inadmissible numerical values
        pattern = "int_vfe_inhom", # Inhomogeneous value formats  (regex)
        required = "int_sts_structure", # unexpected data set structure
        propertyNames = "int_sts_element", # Unexpected data element set
        enum = "con_rvv_icat", # Inadmissible categorial values
        SyntaxErrors = "syntax error",
        "uncategorized error"
      )
    },
    xml = {
      switch(error_str,
        FoundSyntaxErrors = "syntax error",
        "uncategorized error"
      )
    }
  )
}

#' Summarize structured-text validation errors
#'
#' @param error_strings The validator's error categories.
#' @param txt_fmt The structured-text format, JSON or XML.
#' @return A named table of indicator counts and error status.
#' @noRd
.util_make_result_table <- function(error_strings, txt_fmt) {
  errors <- vapply(
    error_strings,
    FUN = function(v) {
      .util_classify_validation_error(txt_fmt, v)
    },
    FUN.VALUE = character(1)
  )

  errors_table <- table(errors)

  if (length(error_strings) > 0) {
    errors_table[["has error"]] <- 1
  } else {
    errors_table[["has error"]] <- 0
  }

  set_if_empty <- function(x, table, orig) {
    if (!(x %in% names(orig))) {
      table[[x]] <- 0
    } else {
      table[[x]] <- orig[[x]]
    }
    table
  }

  set_per_file_count <- function(x, table) {
    if (x == "syntax error" || x == "has error") {
      return(table)
    }
    if (table[[x]] > 0) {
      table[[paste0(x, "_file_total")]] <- table[[x]]
      table[[x]] <- 1
    } else {
      table[[paste0(x, "_file_total")]] <- 0
    }
    table
  }

  error_categories <- c(
    "con_con_contc",
    "con_rvv_inum",
    "int_sts_countel",
    "int_sts_element",
    "int_sts_structure",
    "int_vfe_inhom",
    "int_vfe_type",
    "con_rvv_icat",
    "uncategorized error",
    "syntax error",
    "has error"
  )

  table <- Reduce(
    f = function(prev, curr) {
      tbl <- set_if_empty(curr, prev, errors_table)
      tbl <- set_per_file_count(curr, tbl)
      tbl
    },
    x = error_categories,
    init = table(double(0))
  )

  table
}
