#' Check whether a data-frame reference is remote
#'
#' @param x character vector of data-frame references.
#'
#' @return A logical vector.
#'
#' @noRd
util_is_remote_data_frame_ref <- function(x) {
  startsWith(tolower(x), "https://") |
    startsWith(tolower(x), "http://") |
    startsWith(tolower(x), "ftp://") |
    startsWith(tolower(x), "ftps://") |
    startsWith(tolower(x), "dbx://")
}

#' Add input_dir to relative data-frame references
#'
#' @param x character vector of data-frame references.
#' @param input_dir optional input directory.
#'
#' @return A character vector with `input_dir` prepended where applicable.
#'
#' @noRd
util_add_input_dir_to_data_frame_refs <- function(x, input_dir = NULL) {
  util_expect_scalar(input_dir,
    check_type = is.character,
    allow_null = TRUE
  )
  if (is.null(input_dir) || !length(x)) {
    return(x)
  }
  input_dir <- sub(paste0(.Platform$file.sep, "$"), "", input_dir)
  has_path <- grepl("[/\\\\]", x)
  is_remote <- util_is_remote_data_frame_ref(x)
  use_input_dir <- !has_path & !is_remote & !util_empty(x)
  x[use_input_dir] <- file.path(input_dir, x[use_input_dir])
  x
}

#' Derive a stable display name for an in-memory study data frame
#'
#' @param study_data_expr expression captured with `substitute(study_data)`.
#'
#' @return A scalar character display name.
#'
#' @noRd
util_report_by_study_data_expr <- function(study_data_expr) {
  if (is.symbol(study_data_expr)) {
    return(as.character(study_data_expr))
  }
  "study_data"
}

#' Resolve dq_report_by study-data argument references
#'
#' @param study_data a data frame or character vector passed as `study_data`.
#' @param study_data_expr expression text used when a data frame was passed.
#' @param input_dir optional input directory for relative character references.
#'
#' @return A character vector with data-frame names or resolved file paths.
#'
#' @noRd
util_report_by_study_data_refs <- function(study_data,
  study_data_expr,
  input_dir = NULL) {
  if (is.data.frame(study_data)) {
    return(study_data_expr)
  }
  if (is.character(study_data) && length(study_data) >= 1) {
    return(util_add_input_dir_to_data_frame_refs(
      study_data,
      input_dir = input_dir
    ))
  }
  util_error(
    c(
      "The provided ",
      "study_data argument of class %s is not supported"
    ),
    util_pretty_vector_string(class(study_data)),
    applicability_problem = TRUE
  )
}

#' Resolve dq_report_by study data and header metadata
#'
#' @param study_data a data frame or character vector passed as `study_data`.
#' @param study_data_expr expression text used when a data frame was passed.
#' @param input_dir optional input directory for relative character references.
#'
#' @return A list with possibly updated `study_data`, `name_of_study_data`,
#'   `dataframe_names`, and `list_sd_columns`.
#'
#' @noRd
util_report_by_collect_study_data <- function(study_data,
  study_data_expr,
  input_dir = NULL) {
  dataframe_names <- util_report_by_study_data_refs(
    study_data = study_data,
    study_data_expr = study_data_expr,
    input_dir = input_dir
  )

  if (is.data.frame(study_data)) {
    util_register_primary_study_data(
      study_data = study_data,
      name_of_study_data = dataframe_names
    )
    list_sd_columns <- list(colnames(study_data))
    names(list_sd_columns) <- dataframe_names
    return(list(
      study_data = study_data,
      name_of_study_data = dataframe_names,
      dataframe_names = dataframe_names,
      list_sd_columns = list_sd_columns
    ))
  }

  if (length(dataframe_names) == 1) {
    list_sd_columns <- list(colnames(prep_get_data_frame(
      dataframe_names,
      keep_types = TRUE
    )))
    names(list_sd_columns) <- dataframe_names
    return(list(
      study_data = dataframe_names,
      name_of_study_data = dataframe_names,
      dataframe_names = dataframe_names,
      list_sd_columns = list_sd_columns
    ))
  }

  list_sd_columns <- lapply(setNames(nm = dataframe_names), function(nm) {
    columns_df <- try(
      prep_get_data_frame(nm,
        column_names_only = TRUE,
        keep_types = TRUE
      ),
      silent = TRUE
    )
    if (util_is_try_error(columns_df)) {
      columns_df <- NULL
    }
    columns_df
  })
  list_sd_columns <- lapply(list_sd_columns, colnames)
  names(list_sd_columns) <- dataframe_names

  list(
    study_data = dataframe_names,
    name_of_study_data = NULL,
    dataframe_names = dataframe_names,
    list_sd_columns = list_sd_columns
  )
}

#' Select dataframe-level metadata for each by-report segment
#'
#' @param segment_names segment labels.
#' @param meta_data item-level metadata.
#' @param segment_column metadata column containing segment labels.
#' @param list_sd_columns named list of available study-data columns.
#' @param dataframe_names resolved data-frame references.
#' @param meta_data_dataframe dataframe-level metadata.
#'
#' @return A named list of dataframe-level metadata, one entry per segment.
#'
#' @noRd
util_report_by_dataframes_by_segment <- function(segment_names,
  meta_data,
  segment_column,
  list_sd_columns,
  dataframe_names,
  meta_data_dataframe) {
  lapply(setNames(segment_names, nm = segment_names), function(segment) {
    vars <- meta_data[meta_data[[segment_column]] == segment, VAR_NAMES, drop = TRUE] # nolint: line_length_linter.
    has_segment_vars <- vapply(
      lapply(list_sd_columns, intersect, vars),
      length,
      FUN.VALUE = integer(1)
    ) > 0
    dataframes_to_use <- dataframe_names[
      dataframe_names %in% names(has_segment_vars)[has_segment_vars]
    ]
    meta_data_dataframe[meta_data_dataframe[[DF_NAME]] %in%
        dataframes_to_use, , drop = FALSE]
  })
}

#' Resolve data-frame names needed by a report-by segment
#'
#' @param vars_in_segment item-level variable names for the segment.
#' @param meta_data item-level metadata.
#' @param list_sd_columns named list of study-data columns, or `NULL`.
#' @param study_data_withcode data-frame-level metadata with `DF_CODE`.
#'
#' @return Character vector of data-frame names.
#'
#' @noRd
util_report_by_segment_dataframe_names <- function(vars_in_segment,
  meta_data,
  list_sd_columns,
  study_data_withcode = NULL) {
  if (is.null(list_sd_columns)) {
    vars_in_seg <- meta_data[meta_data[[VAR_NAMES]] %in% vars_in_segment,
      c(VAR_NAMES, DATAFRAMES),
      drop = FALSE
    ]
    dataframe_codes <- unique(unname(unlist(util_parse_assignments(
      vars_in_seg[[DATAFRAMES]],
      split_char = SPLIT_CHAR,
      multi_variate_text = TRUE
    ))))
    return(study_data_withcode[
      study_data_withcode[[DF_CODE]] %in% dataframe_codes,
      DF_NAME,
      drop = TRUE
    ])
  }

  has_segment_vars <- vapply(
    lapply(list_sd_columns, intersect, vars_in_segment),
    length,
    FUN.VALUE = integer(1)
  ) > 0
  names(has_segment_vars)[has_segment_vars]
}

#' Load data frames needed by a report-by segment
#'
#' @param dataframe_names data-frame references used by the current segment.
#'
#' @return A named list of data frames.
#'
#' @noRd
util_report_by_load_segment_dataframes <- function(dataframe_names) {
  lapply(
    setNames(nm = dataframe_names),
    util_expect_data_frame,
    dont_assign = TRUE
  )
}

#' Select dataframe-level metadata for a report-by segment
#'
#' @param meta_data_dataframe dataframe-level metadata.
#' @param dataframe_names data-frame names used by the current segment.
#' @param segment_name current segment name.
#'
#' @return A named list containing dataframe-level metadata for the segment.
#'
#' @noRd
util_report_by_segment_dataframe_metadata <- function(meta_data_dataframe,
  dataframe_names,
  segment_name) {
  segment_metadata <- list(
    dfr = meta_data_dataframe[
      meta_data_dataframe[[DF_NAME]] %in% dataframe_names, ,
      drop = FALSE
    ]
  )
  names(segment_metadata) <- segment_name
  segment_metadata
}

#' Keep only current report-by segment variables in study data
#'
#' @param study_data data frame to filter.
#' @param vars_in_segment item-level variable names for the segment.
#'
#' @return A data frame with columns from `vars_in_segment`.
#'
#' @noRd
util_report_by_keep_segment_vars <- function(study_data, vars_in_segment) {
  study_data[
    ,
    colnames(study_data) %in% vars_in_segment,
    drop = FALSE
  ]
}

#' Resolve ID variables for merging report-by segment data frames
#'
#' @param dfr_in_segment dataframe-level metadata for the current segment.
#' @param id_vars identifier variables supplied to `dq_report_by()`.
#'
#' @return A unique character vector of ID variable names.
#'
#' @noRd
util_report_by_segment_id_vars <- function(dfr_in_segment, id_vars) {
  if (all(is.na(dfr_in_segment[[DF_ID_VARS]]))) {
    util_warning(
      paste0("Column %s in dataframe_level metadata is empty."),
      sQuote(DF_ID_VARS)
    )
  }

  util_parse_dataframe_level_id_vars(
    unique(c(dfr_in_segment[, DF_ID_VARS, drop = TRUE], id_vars))
  )
}

#' Merge report-by segment data frames
#'
#' @param dataframes named list of data frames for the current segment.
#' @param id_vars identifier variables used for merging.
#'
#' @return A merged data frame.
#'
#' @noRd
util_report_by_merge_segment_dataframes <- function(dataframes, id_vars) {
  vars_before <- unique(unlist(lapply(names(dataframes), function(df_name) {
    colnames(dataframes[[df_name]])
  })))

  if (is.null(id_vars) || length(id_vars) == 0) {
    util_warning(c(
      "Because no id variable is available, merging ",
      "data frames could have created duplicated rows."
    ))
    merged_data <- util_merge_study_data_by_id_vars(dataframes)
  } else {
    merged_data <- util_merge_study_data_by_id_vars(dataframes, id_vars)
  }

  if (length(vars_before) > length(colnames(merged_data))) {
    util_warning(
      "Lost %d variables due to mapping problems. %d variables left.",
      length(vars_before) - length(colnames(merged_data)),
      length(colnames(merged_data)),
      applicability_problem = TRUE
    )
  } else if (length(vars_before) < length(colnames(merged_data))) {
    util_warning(
      sprintf(
        "There are duplicated variables due to merging: %s",
        dQuote(setdiff(colnames(merged_data), vars_before))
      ),
      applicability_problem = TRUE
    )
  }

  merged_data
}

#' Filter report-by segment data frames using item-level data-frame metadata
#'
#' @param dataframes named list of loaded data frames.
#' @param dataframe_names data-frame names to filter.
#' @param dfr_in_segment data-frame-level metadata for the current segment.
#' @param meta_data item-level metadata.
#' @param vars_in_segment item-level variable names for the segment.
#' @param study_data_withcode data-frame-level metadata with `DF_CODE`.
#' @param id_vars identifier variables supplied to `dq_report_by()`.
#'
#' @return A named list of filtered data frames.
#'
#' @noRd
util_report_by_filter_segment_dataframes <- function(dataframes,
  dataframe_names,
  dfr_in_segment,
  meta_data,
  vars_in_segment,
  study_data_withcode,
  id_vars) {
  if (!DF_CODE %in% colnames(dfr_in_segment) ||
      !DATAFRAMES %in% colnames(meta_data)) {
    return(dataframes)
  }

  item_dataframe_vars <- meta_data[
    meta_data[[VAR_NAMES]] %in% vars_in_segment,
    c(VAR_NAMES, DATAFRAMES),
    drop = FALSE
  ]
  expected_vars <- lapply(setNames(nm = dataframe_names), function(df_name) {
    code_df <- study_data_withcode[
      study_data_withcode[[DF_NAME]] %in% df_name,
      DF_CODE,
      drop = TRUE
    ]
    item_dataframe_vars[
      grepl(
        sprintf("\\b(%s)\\b", code_df),
        item_dataframe_vars[[DATAFRAMES]]
      ),
      VAR_NAMES,
      drop = TRUE
    ]
  })

  lapply(setNames(nm = names(expected_vars)), function(df_name) {
    dataframe_vars <- expected_vars[[df_name]]
    dataframe_id_vars <- dfr_in_segment[
      dfr_in_segment[[DF_NAME]] == df_name,
      DF_ID_VARS,
      drop = TRUE
    ]
    dataframe_id_vars <- util_parse_dataframe_level_id_vars(
      unique(c(dataframe_id_vars, id_vars))
    )
    keep_vars <- c(dataframe_vars, dataframe_id_vars)
    dataframes[[df_name]][
      ,
      colnames(dataframes[[df_name]]) %in% keep_vars,
      drop = FALSE
    ]
  })
}

#' Split study-data rows by a report-by strata variable
#'
#' @param study_data data frame to split.
#' @param strata_column optional strata column name.
#'
#' @return A named list of row indices.
#'
#' @noRd
util_report_by_strata_rows <- function(study_data, strata_column) {
  if (is.null(strata_column) ||
      !strata_column %in% colnames(study_data)) {
    return(list(all_observations = seq_len(nrow(study_data))))
  }

  strata_rows <- split(seq_len(nrow(study_data)), study_data[[strata_column]])

  if (anyNA(study_data[[strata_column]])) {
    strata_rows <- c(
      strata_rows,
      setNames(
        list(which(is.na(study_data[[strata_column]]))),
        nm = "NAs_group"
      )
    )
  }

  strata_rows
}

#' Apply a report-by subgroup rule to study data
#'
#' @param study_data data frame to filter.
#' @param meta_data item-level metadata.
#' @param subgroup REDCap-style subgroup rule.
#'
#' @return Filtered `study_data`.
#'
#' @noRd
util_report_by_filter_subgroup <- function(study_data,
  meta_data,
  subgroup) {
  if (is.null(subgroup)) {
    return(study_data)
  }

  nrow_df <- nrow(study_data)
  rule <- util_parse_redcap_rule(subgroup)
  filtered_study_data <- try(
    study_data[util_eval_rule(rule,
        ds1 = study_data,
        meta_data = meta_data
      ), , drop = FALSE],
    silent = TRUE
  )
  if (inherits(filtered_study_data, "try-error")) {
    err <- conditionMessage(util_attr(filtered_study_data, "condition",
        exact = TRUE
      ))
    util_error(
      "The subgroup rule %s was not acceptable: %s",
      dQuote(subgroup), err
    )
  }
  if (nrow(filtered_study_data) == nrow_df) {
    util_warning(
      c(
        "The number of cases did not change after applying the",
        "subgroup filter %s"
      ),
      dQuote(subgroup)
    )
  } else if (nrow(filtered_study_data) == 0 && nrow_df > 0) {
    util_warning(
      c(
        "After using subgroup rule %s, no dataset is left. You may",
        "have provided an impossible condition, e.g., filtering",
        "for a variable equal to %s, but the actual levels would be %s."
      ),
      dQuote(subgroup),
      dQuote("females"),
      dQuote("female")
    )
  }

  filtered_study_data
}

#' Parse dataframe-level ID variable metadata
#'
#' @param x character vector from `DF_ID_VARS`.
#'
#' @return A unique character vector of ID variable names.
#'
#' @noRd
util_parse_dataframe_level_id_vars <- function(x) {
  x <- x[!util_empty(x)]
  if (!length(x)) {
    return(character(0))
  }
  id_vars <- unlist(lapply(
    x,
    util_parse_assignments,
    split_char = SPLIT_CHAR,
    multi_variate_text = TRUE,
    split_on_any_split_char = TRUE
  ))
  unique(unname(id_vars[!util_empty(id_vars)]))
}

#' Merge study data frames by shared ID variables
#'
#' @param dataframes named list of data frames.
#' @param id_vars ID variables to use when present in both data frames.
#'
#' @return Merged data frame.
#'
#' @noRd
util_merge_study_data_by_id_vars <- function(dataframes,
  id_vars = character(0)) {
  util_stop_if_not(is.list(dataframes))
  if (!length(dataframes)) {
    return(NULL)
  }
  dataframes <- dataframes[vapply(dataframes, is.data.frame,
      FUN.VALUE = logical(1)
    )]
  if (length(dataframes) == 1) {
    return(dataframes[[1]])
  }

  if (!length(id_vars)) {
    return(Reduce(function(x, y) {
      merge(x, y, all = TRUE)
    }, dataframes))
  }

  Reduce(function(x, y) {
    partial_merge <- suppressWarnings(merge(
      x,
      y,
      all = TRUE,
      by = intersect(intersect(colnames(x), colnames(y)), id_vars),
      suffixes = c("", "")
    ))
    suppressWarnings(util_fix_merge_dups(partial_merge, FALSE))
  }, dataframes)
}

#' Load data frames listed in dataframe-level metadata
#'
#' @param dataframe_names data-frame references.
#'
#' @return A named list of data frames, or `NULL`.
#'
#' @noRd
util_load_dataframe_level_study_data <- function(dataframe_names) {
  dataframes <- lapply(setNames(dataframe_names, dataframe_names), function(nm) { # nolint: line_length_linter.
    try(prep_get_data_frame(nm, keep_types = TRUE), silent = TRUE)
  })
  ok <- vapply(dataframes, is.data.frame, FUN.VALUE = logical(1))
  if (!all(ok)) {
    util_message(
      c(
        "Could not load all study data frames from dataframe-level metadata.",
        "Failed entries: %s."
      ),
      util_pretty_vector_string(sQuote(names(dataframes)[!ok])),
      applicability_problem = TRUE
    )
    return(NULL)
  }
  dataframes
}

#' Merge study data frames listed in dataframe-level metadata
#'
#' @param meta_data_dataframe checked data-frame-level metadata.
#' @param dataframe_names resolved data-frame references.
#'
#' @return `NULL` or a merged data frame.
#'
#' @noRd
util_merge_dataframe_level_study_data <- function(meta_data_dataframe,
  dataframe_names) {
  id_vars <- util_parse_dataframe_level_id_vars(
    meta_data_dataframe[[DF_ID_VARS]]
  )
  if (!length(id_vars)) {
    util_message(
      c(
        "Found more than one data frame in dataframe-level metadata, but",
        "could not merge them automatically because %s is empty.",
        "Please pass %s explicitly or use %s for multi-data-frame reports."
      ),
      sQuote(DF_ID_VARS),
      sQuote("study_data"),
      sQuote("dq_report_by()"),
      applicability_problem = TRUE
    )
    return(NULL)
  }

  dataframes <- util_load_dataframe_level_study_data(dataframe_names)
  if (is.null(dataframes)) {
    return(NULL)
  }
  id_vars_missing <- vapply(dataframes, function(x) {
    !all(id_vars %in% colnames(x))
  }, FUN.VALUE = logical(1))
  if (any(id_vars_missing)) {
    util_message(
      c(
        "Could not merge study data frames from dataframe-level metadata.",
        "The ID variable(s) %s are not present in all data frames.",
        "Please pass %s explicitly or use %s."
      ),
      util_pretty_vector_string(sQuote(id_vars)),
      sQuote("study_data"),
      sQuote("dq_report_by()"),
      applicability_problem = TRUE
    )
    return(NULL)
  }

  vars_before <- unique(unlist(lapply(dataframes, colnames)))
  merged <- util_merge_study_data_by_id_vars(dataframes, id_vars)
  if (length(vars_before) != length(colnames(merged))) {
    util_message(
      c(
        "Merged study data frames from dataframe-level metadata, but the",
        "merged columns differ from the union of input columns. Please inspect",
        "the result or pass %s explicitly for full control."
      ),
      sQuote("study_data"),
      applicability_problem = TRUE
    )
  }
  merged
}

#' Find study data from data-frame-level metadata
#'
#' This helper resolves a single referenced data frame, prefers an explicitly
#' named or coded `study_data` among multiple data frames, and otherwise only
#' merges multiple data frames if `DF_ID_VARS` provides clear ID variables.
#' More complex multi-data-frame reports remain the responsibility of
#' `dq_report_by()`.
#'
#' @param meta_data_dataframe data-frame-level metadata or registry name.
#' @param input_dir optional input directory for relative `DF_NAME` values.
#'
#' @return `NULL` or a list with `study_data` and `name_of_study_data`.
#'
#' @noRd
util_find_study_data_from_dataframe_level <- function(meta_data_dataframe =
    "dataframe_level",
  input_dir = NULL) {
  meta_data_dataframe <- try(
    prep_check_meta_data_dataframe(meta_data_dataframe),
    silent = TRUE
  )
  if (util_is_try_error(meta_data_dataframe) ||
      !is.data.frame(meta_data_dataframe) ||
      !nrow(meta_data_dataframe)) {
    return(NULL)
  }

  dataframe_names <- unique(meta_data_dataframe[[DF_NAME]])
  dataframe_names <- dataframe_names[!util_empty(dataframe_names)]
  dataframe_names <- util_add_input_dir_to_data_frame_refs(
    dataframe_names,
    input_dir = input_dir
  )
  if (!length(dataframe_names)) {
    return(NULL)
  }
  if (length(dataframe_names) > 1) {
    bare_names <- sub("\\?.*$", "", dataframe_names)
    bare_names <- sub("#.*$", "", bare_names)
    bare_names <- sub("\\.[^.]*$", "", basename(bare_names))
    dataframe_codes <- meta_data_dataframe[[DF_CODE]]
    preferred <- bare_names == "study_data"
    if (length(dataframe_codes) == length(dataframe_names)) {
      preferred <- preferred |
        (!util_empty(dataframe_codes) & dataframe_codes == "study_data")
    }
    if (sum(preferred) == 1) {
      dataframe_names <- dataframe_names[preferred]
    } else {
      merged_study_data <- util_merge_dataframe_level_study_data(
        meta_data_dataframe = meta_data_dataframe,
        dataframe_names = dataframe_names
      )
      if (is.null(merged_study_data)) {
        return(NULL)
      }
      return(list(
        study_data = merged_study_data,
        name_of_study_data = paste(dataframe_names, collapse = ", ")
      ))
    }
  }

  study_data <- try(
    prep_get_data_frame(dataframe_names, keep_types = TRUE),
    silent = TRUE
  )
  if (util_is_try_error(study_data) || !is.data.frame(study_data)) {
    util_message(
      c(
        "Could not load study data %s from dataframe-level metadata.",
        "Please pass %s explicitly or load it into the data-frame cache."
      ),
      dQuote(dataframe_names),
      sQuote("study_data"),
      applicability_problem = TRUE
    )
    return(NULL)
  }

  list(
    study_data = study_data,
    name_of_study_data = dataframe_names
  )
}
