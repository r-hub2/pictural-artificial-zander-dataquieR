#' Extract variables referenced in square brackets
#'
#' @param text character vector containing expressions with bracketed variables.
#' @param variable_names names that can be referenced in `text`.
#'
#' @return A list of character vectors, one per element of `text`.
#'
#' @noRd
util_report_by_bracket_references <- function(text, variable_names) {
  needles <- paste0("[", variable_names, "]")
  matches <- vapply(
    setNames(needles, nm = variable_names),
    grepl,
    setNames(nm = text),
    fixed = TRUE,
    FUN.VALUE = logical(length = length(text))
  )
  if (is.vector(matches)) {
    matches <- as.matrix(t(matches))
  }

  unname(lapply(as.data.frame(t(matches)), function(has_match) {
    unique(sort(colnames(matches)[has_match]))
  }))
}

#' Complete computation metadata with referenced variables
#'
#' @inheritParams .template_function_developer
#' @param variable_names names that can be referenced in computation rules.
#'
#' @return The completed computed-item metadata.
#'
#' @noRd
util_report_by_complete_computation_variables <- function(
  meta_data_item_computation,
  variable_names
) {
  variable_list <- lapply(
    util_report_by_bracket_references(
      meta_data_item_computation[[COMPUTATION_RULE]],
      variable_names
    ),
    paste0,
    collapse = sprintf(" %s ", SPLIT_CHAR)
  )

  if (!VARIABLE_LIST %in% names(meta_data_item_computation)) {
    meta_data_item_computation[[VARIABLE_LIST]] <-
      rep(NA_character_, nrow(meta_data_item_computation))
  }
  empty_variable_list <- util_empty(meta_data_item_computation[[VARIABLE_LIST]])
  meta_data_item_computation[[VARIABLE_LIST]][empty_variable_list] <-
    variable_list[empty_variable_list]

  meta_data_item_computation
}

#' Resolve subgroup expressions to variable names
#'
#' @inheritParams .template_function_developer
#' @param subgroup character vector containing subgroup expressions.
#' @param variable_names names that can be referenced in subgroup expressions.
#'
#' @return Character vector of variable names.
#'
#' @noRd
util_report_by_subgroup_variables <- function(subgroup,
  variable_names,
  meta_data,
  label_col) {
  if (is.null(subgroup)) {
    return(character(0))
  }

  references <- unlist(
    util_report_by_bracket_references(subgroup, variable_names),
    use.names = FALSE
  )
  variables <- vapply(references, function(variable) {
    if (variable %in% meta_data[[VAR_NAMES]]) {
      return(variable)
    }
    unname(util_map_labels(
      variable,
      meta_data = meta_data,
      from = label_col,
      to = VAR_NAMES
    ))
  }, FUN.VALUE = character(1))

  unname(variables)
}

#' Normalize report-by identifier variables
#'
#' @inheritParams .template_function_developer
#' @param id_vars identifier variables supplied to `dq_report_by()`.
#'
#' @return Character vector of identifier variable names.
#'
#' @noRd
util_report_by_id_vars <- function(id_vars, meta_data, label_col) {
  if (is.null(id_vars)) {
    return(character(0))
  }

  util_expect_scalar(
    arg_name = id_vars,
    allow_more_than_one = TRUE,
    check_type = is.character
  )
  if (length(id_vars) == 1) {
    id_vars <- unname(unlist(util_parse_assignments(
      id_vars,
      split_char = SPLIT_CHAR
    )))
  }

  unname(vapply(id_vars, function(id_var) {
    if (id_var %in% meta_data[[VAR_NAMES]]) {
      return(id_var)
    }

    mapped_id_var <- try(util_map_labels(
      id_var,
      meta_data = meta_data,
      from = label_col,
      to = VAR_NAMES
    ), silent = TRUE)
    if (util_is_try_error(mapped_id_var)) {
      util_error(
        c("The id_vars %s is not present", " in the item_level metadata"),
        dQuote(id_var)
      )
    }
    unname(mapped_id_var)
  }, FUN.VALUE = character(1)))
}

#' Validate segment selection without an explicit segment column
#'
#' @inheritParams .template_function_developer
#' @param segment_column selected metadata column, if any.
#' @param segment_select segments to include.
#' @param segment_exclude segments to exclude.
#'
#' @return The resolved segment column.
#'
#' @noRd
util_report_by_segment_column <- function(meta_data,
  segment_column,
  segment_select,
  segment_exclude) {
  if (!is.null(segment_select) && is.null(segment_column)) {
    if (STUDY_SEGMENT %in% names(meta_data)) {
      segment_column <- STUDY_SEGMENT
      possible_segments <- unique(meta_data[[STUDY_SEGMENT]])
      if (any(!segment_select %in% possible_segments, na.rm = TRUE)) {
        util_error(c(
          "segment_select values are not present in the ",
          "column 'STUDY_SEGMENT' that is assumed to be the ",
          "segment_column when this is not assigned."
        ))
      }
    } else {
      util_error(c(
        "No segment_column provided and no STUDY_SEGMENT ",
        "available in the metadata"
      ))
    }
  }

  if (!is.null(segment_exclude) && is.null(segment_column)) {
    if (STUDY_SEGMENT %in% names(meta_data)) {
      segment_column <- STUDY_SEGMENT
      possible_segments <- unique(meta_data[[STUDY_SEGMENT]])
      if (any(!segment_exclude %in% possible_segments, na.rm = TRUE)) {
        util_error(c(
          "segment_exclude values are not present in the ",
          "column 'STUDY_SEGMENT' that is assumed to be the ",
          "segment_column when this is not assigned."
        ))
      }
    } else {
      util_error(c(
        "No segment_column provided and no STUDY_SEGMENT",
        " available in the metadata"
      ))
    }
  }

  segment_column
}

#' Prepare report-by segment names
#'
#' @inheritParams .template_function_developer
#' @param segment_column selected metadata column.
#' @param segment_select segments to include.
#' @param segment_exclude segments to exclude.
#' @param label_col_provided originally provided label column.
#'
#' @return A list with updated `meta_data` and final `segment_names`.
#'
#' @noRd
util_report_by_segment_names <- function(meta_data,
  segment_column,
  segment_select,
  segment_exclude,
  label_col,
  label_col_provided) {
  .md <- meta_data[[segment_column]]
  i <- ""
  while (any(.md == paste0("na", i), na.rm = TRUE)) {
    if (i == "") {
      i <- 0
    }
    i <- i + 1
  }
  .md[util_empty(.md)] <- paste0("na", i)
  meta_data[[segment_column]] <- .md
  segments <- unique(meta_data[[segment_column]])

  if (label_col_provided != VAR_NAMES &&
      all(segments %in% meta_data[[VAR_NAMES]])) {
    segment_names <- util_map_labels(segments, meta_data, label_col)
  } else {
    segment_names <- segments
  }

  if (!is.null(segment_select)) {
    if (length(segment_select) == 1) {
      segment_select <- unname(unlist(util_parse_assignments(
        segment_select,
        split_char = SPLIT_CHAR
      )))
    }
    if (!any(segment_select %in% segment_names)) {
      if (length(segment_select) > 1) {
        util_error(
          "No segment_column level matches the provided names: %s",
          dQuote(segment_select)
        )
      } else {
        all_segment_names <- segment_names
        segment_names <- segment_names[grepl(segment_select, segment_names)]
        if (length(segment_names) == 0) {
          util_error(
            c(
              "No segment_column level matches the provided name or",
              "pattern: %s"
            ),
            dQuote(segment_select)
          )
          segment_names <- all_segment_names
        }
      }
    } else {
      segment_names <- segment_names[segment_names %in% segment_select]
    }
  }

  if (!is.null(segment_exclude)) {
    if (length(segment_exclude) == 1) {
      segment_exclude <- unname(unlist(util_parse_assignments(
        segment_exclude,
        split_char = SPLIT_CHAR
      )))
    }
    if (!any(segment_exclude %in% segment_names)) {
      if (length(segment_exclude) > 1) {
        util_error(
          "No segment_column level matches the provided names to exclude: %s",
          dQuote(segment_exclude)
        )
      } else {
        unwanted_segments <- segment_names[grepl(
          segment_exclude,
          segment_names
        )]
        segment_names <- setdiff(segment_names, unwanted_segments)
        if (length(segment_names) == 0) {
          util_error(
            c(
              "No segment_column level left after removing ",
              "unwanted segments: %s"
            ),
            dQuote(segment_exclude)
          )
        }
      }
    } else {
      segment_names <- segment_names[!(segment_names %in% segment_exclude)]
      if (length(segment_names) == 0) {
        util_error(
          c(
            "No segment_column level left after removing ",
            "unwanted segments: %s"
          ),
          dQuote(segment_exclude)
        )
      }
    }
  }

  list(meta_data = meta_data, segment_names = segment_names)
}

#' Resolve value labels for the report-by strata variable
#'
#' @inheritParams .template_function_developer
#' @param strata_column study-data variable used for strata.
#'
#' @return A named character vector mapping strata values to value labels.
#'
#' @noRd
util_report_by_expected_strata <- function(meta_data, strata_column) {
  value_labels <- meta_data[
    meta_data[[VAR_NAMES]] == strata_column,
    VALUE_LABELS,
    drop = TRUE
  ]

  if (length(value_labels) == 0 ||
      is.null(value_labels) ||
      anyNA(value_labels)) {
    value_label_table_name <- meta_data[
      meta_data[[VAR_NAMES]] == strata_column,
      VALUE_LABEL_TABLE,
      drop = TRUE
    ]

    value_label_table <- try(
      util_expect_data_frame(value_label_table_name, dont_assign = TRUE),
      silent = TRUE
    )
    if (!is.data.frame(value_label_table)) {
      code_list_table <- try(
        util_expect_data_frame("CODE_LIST_TABLE", dont_assign = TRUE),
        silent = TRUE
      )
      if (!is.data.frame(code_list_table)) {
        util_message(sprintf(
          "No value_label_table_name %s found",
          dQuote(value_label_table_name)
        ))
        value_label_table <- data.frame(
          CODE_VALUE = character(0),
          CODE_LABEL = character(0)
        )
      } else {
        value_label_table <- code_list_table[
          code_list_table[[VALUE_LABEL_TABLE]] == value_label_table_name, ,
          drop = FALSE
        ]
      }
    }
    return(setNames(
      value_label_table[[CODE_LABEL]],
      nm = value_label_table[[CODE_VALUE]]
    ))
  }

  unlist(
    util_parse_assignments(
      value_labels,
      split_char = SPLIT_CHAR,
      split_on_any_split_char = TRUE,
      multi_variate_text = TRUE
    )
  )
}

#' Normalize report-by selection arguments
#'
#' @param x selection or exclusion values.
#'
#' @return A character vector or `NULL`.
#'
#' @noRd
util_report_by_selection_values <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (length(x) == 1) {
    x <- unname(unlist(util_parse_assignments(
      x,
      split_char = SPLIT_CHAR
    )))
  }
  x
}

#' Prepare item-level metadata and response variables per report-by segment
#'
#' @inheritParams .template_function_developer
#' @param segment_names final segment labels.
#' @param segment_column metadata column containing segment labels.
#' @param id_vars identifier variables.
#' @param vars_in_subgroup variables referenced by subgroup rules.
#' @param seg_in_segment segment-level metadata by segment.
#' @param dfr_in_segment dataframe-level metadata by segment.
#' @param cil_in_segment cross-item metadata by segment.
#' @param computed_in_segment computation metadata by segment.
#' @param strata_column optional strata column.
#'
#' @return A list with item metadata, variables, and response variables by
#'   segment.
#'
#' @noRd
util_report_by_segment_items <- function(segment_names,
  meta_data,
  segment_column,
  resp_vars = NULL,
  id_vars,
  vars_in_subgroup,
  label_col,
  seg_in_segment,
  dfr_in_segment,
  cil_in_segment,
  computed_in_segment,
  strata_column = NULL) {
  items <- lapply(
    setNames(segment_names, nm = segment_names),
    function(segment) {
      vars <- meta_data[meta_data[[segment_column]] ==
          segment, VAR_NAMES, drop = TRUE]
      if (!is.null(resp_vars)) {
        vars <- vars[vars %in% resp_vars]
      }
      overview_vars_md <- util_referred_vars(
        resp_vars = vars,
        id_vars = id_vars,
        vars_in_subgroup = vars_in_subgroup,
        label_col = label_col,
        meta_data = meta_data,
        meta_data_segment = seg_in_segment[[segment]],
        meta_data_dataframe = dfr_in_segment[[segment]],
        meta_data_cross_item = cil_in_segment[[segment]],
        meta_data_item_computation =
          computed_in_segment[[segment]],
        strata_column = strata_column
      )
      md_seg <- overview_vars_md$md_complete
      attr(md_seg, "normalized") <- TRUE
      attr(md_seg, "version") <- 2
      response_vars <- character(0)
      if (!is.null(resp_vars)) {
        response_vars <- overview_vars_md$vars_complete[
          overview_vars_md$vars_complete %in% resp_vars
        ]
      }
      list(
        vars = overview_vars_md$vars_complete,
        meta_data = md_seg,
        resp_vars = response_vars
      )
    }
  )

  list(
    vars_in_segment = lapply(items, `[[`, "vars"),
    md_in_segment = lapply(items, `[[`, "meta_data"),
    resp_vars_in_segment = lapply(items, `[[`, "resp_vars")
  )
}

#' Prepare report-by metadata levels
#'
#' @inheritParams .template_function_developer
#' @param study_data_provided whether study data were supplied explicitly.
#' @param study_data_is_data_frame whether supplied study data are a data frame.
#' @param study_data_length number of supplied study-data references.
#' @param input_dir optional directory for dataframe references.
#'
#' @return A named list of normalized metadata levels.
#'
#' @noRd
util_report_by_metadata_levels <- function(meta_data,
  meta_data_cross_item,
  meta_data_segment,
  meta_data_dataframe,
  label_col,
  study_data_provided,
  study_data_is_data_frame,
  study_data_length,
  input_dir) {
  meta_data_cross_item <- util_ensure_cross_item_metadata(
    meta_data_cross_item
  )
  meta_data_cross_item <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item,
    label_col = label_col
  )

  meta_data_segment <- util_ensure_segment_metadata(
    meta_data_segment = meta_data_segment,
    meta_data = meta_data,
    validate_study_segment = TRUE,
    add_segment_id_vars = TRUE
  )

  try(util_expect_data_frame(meta_data_dataframe), silent = TRUE)
  multiple_study_data <- study_data_provided && !study_data_is_data_frame &&
    study_data_length > 1
  if (!is.data.frame(meta_data_dataframe)) {
    util_message(sprintf(
      "No dataframe level metadata %s found",
      dQuote(meta_data_dataframe)
    ))
    meta_data_dataframe <- util_empty_dataframe_metadata()
  } else if (multiple_study_data) {
    util_message(c(
      "When multiple data frames are provided for the",
      " argument 'study data', ",
      "the dataframe_level_metadata information will be ignored"
    ))
    meta_data_dataframe <- util_empty_dataframe_metadata()
  } else {
    util_expect_data_frame(meta_data_dataframe,
      col_names = list(DF_NAME = is.character)
    )
    if (DF_ID_VARS %in% names(meta_data_dataframe)) {
      util_expect_data_frame(meta_data_dataframe,
        col_names = list(DF_ID_VARS = is.character)
      )
    } else {
      meta_data_dataframe[[DF_ID_VARS]] <- NA_character_
    }
    if (DF_CODE %in% names(meta_data_dataframe)) {
      util_expect_data_frame(meta_data_dataframe,
        col_names = list(DF_CODE = is.character)
      )
    } else {
      meta_data_dataframe[[DF_CODE]] <- NA_character_
    }
    if (nrow(meta_data_dataframe) > 0) {
      meta_data_dataframe[[DF_NAME]] <- util_add_input_dir_to_data_frame_refs(
        meta_data_dataframe[[DF_NAME]],
        input_dir = input_dir
      )
    }
  }

  list(
    meta_data_cross_item = meta_data_cross_item,
    meta_data_segment = meta_data_segment,
    meta_data_dataframe = meta_data_dataframe
  )
}
