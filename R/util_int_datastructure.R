#' Check dataframe-level record sets for `int_all_datastructure_dataframe()`
#'
#' @noRd
util_int_datastructure_run_subcheck <- function(expr, subcheck, default) {
  tryCatch(
    expr,
    error = function(e) {
      util_warning(
        "Skipping data-structure subcheck %s because it failed: %s",
        dQuote(subcheck),
        conditionMessage(e),
        applicability_problem = TRUE
      )
      default
    }
  )
}

#' Empty dataframe-level result for wrapper fallbacks
#'
#' @noRd
util_int_datastructure_empty_dataframe_result <- function(num_col,
  pct_col,
  data_cols = character()) {
  result <- list(
    DataframeData = as.data.frame(
      stats::setNames(
        replicate(length(data_cols), character(), simplify = FALSE),
        data_cols
      ),
      stringsAsFactors = FALSE
    ),
    DataframeTable = data.frame(
      Level = character(),
      DF_NAME = character(),
      stringsAsFactors = FALSE
    ),
    Other = list()
  )
  util_int_datastructure_add_empty_metrics(result, num_col, pct_col)
}

#' Empty segment-level result for wrapper fallbacks
#'
#' @noRd
util_int_datastructure_empty_segment_result <- function(num_col,
  pct_col,
  data_cols = character()) {
  result <- list(
    SegmentData = as.data.frame(
      stats::setNames(
        replicate(length(data_cols), character(), simplify = FALSE),
        data_cols
      ),
      stringsAsFactors = FALSE
    ),
    SegmentTable = data.frame(
      Segment = character(),
      stringsAsFactors = FALSE
    ),
    Other = list()
  )
  util_int_datastructure_add_empty_metrics(result, num_col, pct_col)
}

#' Add empty metric columns to an empty wrapper result
#'
#' @noRd
util_int_datastructure_add_empty_metrics <- function(x, num_col, pct_col) {
  table_name <- grep("Table$", names(x), value = TRUE)
  x[[table_name]][[num_col]] <- numeric()
  x[[table_name]][[pct_col]] <- numeric()
  x[[table_name]][["GRADING"]] <- numeric()
  x
}

#' Empty dataframe element-set table
#'
#' @noRd
util_int_datastructure_empty_dataframe_element <- function() {
  data.frame(
    Level = character(),
    DF_NAME = character(),
    NUM_int_sts_element = numeric(),
    PCT_int_sts_element = numeric(),
    GRADING = numeric(),
    stringsAsFactors = FALSE
  )
}

#' Check one dataframe element set without dataframe-level assignments
#'
#' @noRd
util_int_datastructure_df_element_set_single <- function(meta_data,
  meta_data_dataframe,
  study_data_list,
  check_type =
    getOption(
      "dataquieR.ELEMENT_MISSMATCH_CHECKTYPE",
      dataquieR.ELEMENT_MISSMATCH_CHECKTYPE_default
    )) {
  util_match_arg(check_type,
    choices = c("none", "exact", "subset_u", "subset_m")
  )

  df_name <- meta_data_dataframe[[DF_NAME]][[1]]
  from_item_level <- meta_data[[VAR_NAMES]]
  from_df <- colnames(study_data_list[[df_name]])
  from_all <- union(from_item_level, from_df)
  not_in_df <- from_all[!(from_all %in% from_df)]
  not_in_il <- from_all[!(from_all %in% from_item_level)]

  details <- character(0)
  if (length(not_in_df) > 0) {
    details["not_in_df"] <- paste(
      "{",
      util_pretty_vector_string(not_in_df, n_max = 5),
      "} \u2209 sd"
    )
  }
  if (length(not_in_il) > 0) {
    details["not_in_il"] <- paste(
      "{",
      util_pretty_vector_string(not_in_il, n_max = 5),
      "} \u2209 md"
    )
  }

  if (check_type == "subset_u") {
    details <- details["not_in_il"]
    num <- length(not_in_il)
    pct <- 100 * num / length(from_df)
  } else if (check_type == "subset_m") {
    details <- details["not_in_df"]
    num <- length(not_in_df)
    pct <- 100 * num / length(from_item_level)
  } else if (check_type == "exact") {
    num <- length(not_in_df) + length(not_in_il)
    pct <- 100 * num / length(from_all)
  } else {
    num <- NA_integer_
    pct <- NA_real_
    details <- NA_character_
  }

  DataframeData <- data.frame(
    DF_NAME = df_name,
    NUM_int_sts_element = num,
    PCT_int_sts_element = round(pct, 2),
    `Affected Elements` = paste(details, collapse = " \u2227 "),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  DataframeData$`Affected Elements`[
    !nzchar(DataframeData$`Affected Elements`)
  ] <- NA_character_

  DataframeTable <- DataframeData[, c(
    "DF_NAME",
    "NUM_int_sts_element",
    "PCT_int_sts_element"
  ), drop = FALSE]

  colnames(DataframeData) <- util_translate_indicator_metrics(
    colnames(DataframeData),
    ignore_unknown = TRUE
  )

  list(
    DataframeTable = DataframeTable,
    DataframeData = DataframeData
  )
}

#' Empty segment element-set table
#'
#' @noRd
util_int_datastructure_empty_segment_element <- function() {
  data.frame(
    Segment = character(),
    NUM_int_sts_element = numeric(),
    PCT_int_sts_element = numeric(),
    GRADING = numeric(),
    stringsAsFactors = FALSE
  )
}

#' Check dataframe-level record sets for `int_all_datastructure_dataframe()`
#'
#' @noRd
util_int_datastructure_df_record_set <- function(meta_data_dataframe,
  study_data_list,
  id_vars_list) {
  meta_data_record_set <- meta_data_dataframe[
    !util_empty(meta_data_dataframe[[DF_RECORD_CHECK]]), ,
    drop = FALSE
  ]
  df_names <- meta_data_record_set[[DF_NAME]]
  if (missing(id_vars_list)) {
    id_vars_list <- util_int_datastructure_id_vars(
      meta_data_record_set,
      DF_NAME,
      DF_ID_VARS
    )
  }
  id_vars_list <- id_vars_list[df_names]
  valid_id_tables <- setNames(meta_data_record_set[[DF_ID_REF_TABLE]], df_names)
  expected_checks <- setNames(meta_data_record_set[[DF_RECORD_CHECK]], df_names)
  empty_record_set <- util_int_datastructure_empty_record_set("Data frame")

  result <- lapply(setNames(nm = df_names), function(current_df) {
    data_current_df <- study_data_list[[current_df]]
    valid_id_table <- util_expect_data_frame(
      valid_id_tables[[current_df]],
      dont_assign = TRUE
    )
    id_vars <- id_vars_list[[current_df]]
    id_vars <- id_vars[!util_empty(id_vars)]

    if (length(id_vars) == 0) {
      util_warning(
        "No %s defined in %s, skipping the check for unexpected record set",
        dQuote("DF_ID_VARS"),
        dQuote("meta_data_studies"),
        applicability_problem = TRUE,
        intrinsic_applicability_problem = TRUE
      )
      return(list(empty_record_set, data.frame()))
    }

    if (length(id_vars) > 1) {
      util_warning(
        c(
          "Check for multiple IDs is not currently supported,",
          "but you could assign value labels to the each variable"
        ),
        applicability_problem = TRUE,
        intrinsic_applicability_problem = TRUE
      )
      return(list(empty_record_set, data.frame()))
    }

    data_ids <- util_remove_empty_rows(data_current_df, id_vars = id_vars)
    metadata_ids <- valid_id_table[!util_empty(valid_id_table)]
    data_values <- data_ids[[id_vars]]
    unexpected_ids <- data_values[!(data_values %in% metadata_ids)]
    match_expected <- expected_checks[[current_df]]

    result_data <- util_int_datastructure_record_set_data(
      level_col = "Data frame",
      level = current_df,
      data_values = data_values,
      metadata_ids = metadata_ids,
      unexpected_ids = unexpected_ids,
      match_expected = match_expected,
      mismatch_denominator = length(metadata_ids)
    )
    other <- util_int_datastructure_unexpected_ids(
      level_col = "Dataframe",
      level = current_df,
      unexpected_ids = unexpected_ids,
      data_values = data_values,
      metadata_ids = metadata_ids
    )

    list(result_data, other)
  })

  data <- if (length(result) == 0) {
    empty_record_set
  } else {
    do.call(rbind.data.frame, lapply(result, `[[`, 1))
  }
  dataframe_table <- if (ncol(data) < 7) {
    data.frame(
      Level = character(0),
      DF_NAME = character(0),
      NUM_int_sts_setrc = numeric(0),
      PCT_int_sts_setrc = numeric(0),
      GRADING = numeric(0),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      Level = "Dataframe",
      DF_NAME = data[, 2, drop = TRUE],
      NUM_int_sts_setrc = data[, 6, drop = TRUE],
      PCT_int_sts_setrc = data[, 7, drop = TRUE],
      GRADING = data$GRADING,
      stringsAsFactors = FALSE
    )
  }

  list(
    DataframeData = data,
    DataframeTable = dataframe_table,
    Other = dplyr::bind_rows(lapply(result, `[[`, 2))
  )
}

#' Check dataframe-level duplicate IDs for `int_all_datastructure_dataframe()`
#'
#' @noRd
util_int_datastructure_df_duplicate_ids <- function(meta_data_dataframe,
  study_data_list,
  id_vars_list) {
  meta_data_dup_ids <- meta_data_dataframe[
    !util_empty(meta_data_dataframe[[DF_UNIQUE_ID]]) &
      !util_empty(meta_data_dataframe[[DF_ID_VARS]]), ,
    drop = FALSE
  ]
  df_names <- meta_data_dup_ids[[DF_NAME]]
  if (missing(id_vars_list)) {
    id_vars_list <- util_int_datastructure_id_vars(
      meta_data_dup_ids,
      DF_NAME,
      DF_ID_VARS
    )
  }
  id_vars_list <- id_vars_list[df_names]
  repetitions <- setNames(meta_data_dup_ids[[DF_UNIQUE_ID]], df_names)

  result <- lapply(setNames(nm = df_names), function(current_df) {
    data_current_df <- study_data_list[[current_df]]
    id_vars <- id_vars_list[[current_df]]
    id_cols <- intersect(id_vars, colnames(data_current_df))

    if (length(id_cols) == 0) {
      util_message(
        c(
          "None of the ID variables (%s) in the metadata are included in",
          "the study data frame %s. Ignore that data frame."
        ),
        util_pretty_vector_string(id_vars),
        dQuote(current_df),
        applicability_problem = TRUE
      )
      return(list(
        util_int_datastructure_missing_duplicate_data("Data frame", current_df),
        NULL
      ))
    }

    dup_info <- util_find_duplicated_rows(
      study_data = data_current_df,
      id_vars = id_vars,
      repeptitions = repetitions[[current_df]]
    )
    n_uniq <- nrow(data_current_df) - sum(dup_info$unexp_reps)

    result_data <- util_int_datastructure_duplicate_data(
      check = "IDs",
      level_col = "Data frame",
      level = current_df,
      n_total = nrow(data_current_df),
      n_unique = n_uniq,
      id_vars = id_cols
    )

    duplicated_rows <- if (result_data[[3]]) dup_info$which else NULL

    list(result_data, duplicated_rows)
  })

  data <- do.call(rbind.data.frame, lapply(result, `[[`, 1))

  list(
    DataframeData = data,
    DataframeTable = util_int_datastructure_duplicate_table(
      data,
      level = "Dataframe",
      source_col = "Data frame",
      level_col = DF_NAME,
      num_col = "NUM_int_sts_dupl_ids",
      pct_col = "PCT_int_sts_dupl_ids"
    ),
    Other = lapply(result, `[[`, 2)
  )
}

# nolint start: line_length_linter.
#' Check segment-level record sets for `int_all_datastructure_segment()`
#'
#' Multiple segment ID variables are not supported here. This preserves the
#' previous behavior: warn about the unsupported case and return an empty result.
#'
#' @noRd
# nolint end
util_int_datastructure_segment_record_set <- function(meta_data_segment,
  study_data,
  meta_data,
  label_col,
  id_vars_list,
  other_column_order =
    "line_first") {
  meta_data_record_set <- meta_data_segment[
    !util_empty(meta_data_segment[[SEGMENT_RECORD_CHECK]]), ,
    drop = FALSE
  ]
  segments <- meta_data_record_set[[STUDY_SEGMENT]]
  valid_id_tables <- setNames(
    meta_data_record_set[[SEGMENT_ID_REF_TABLE]],
    segments
  )
  expected_checks <- setNames(
    meta_data_record_set[[SEGMENT_RECORD_CHECK]],
    segments
  )
  if (missing(id_vars_list)) {
    id_vars_list <- util_int_datastructure_id_vars(
      meta_data_record_set,
      STUDY_SEGMENT,
      SEGMENT_ID_VARS,
      map_meta_data = meta_data,
      label_col = label_col
    )
  }
  id_vars_list <- id_vars_list[segments]
  empty_record_set <- util_int_datastructure_empty_record_set("Segment")

  old_segments <- segments
  segments <- intersect(segments, meta_data[[STUDY_SEGMENT]])

  if (length(old_segments) > length(segments)) {
    util_warning(
      "The segments in the %s do not match the segments in %s, considering only the intersection", # nolint: line_length_linter.
      dQuote("meta_data"),
      dQuote("meta_data_segment"),
      applicability_problem = TRUE,
      intrinsic_applicability_problem = TRUE
    )
  }

  result <- lapply(setNames(nm = segments), function(current_segment) {
    valid_id_table <- util_expect_data_frame(
      valid_id_tables[[current_segment]],
      dont_assign = TRUE
    )
    id_vars <- id_vars_list[[current_segment]]
    id_vars <- id_vars[!util_empty(id_vars)]

    if (length(id_vars) == 0) {
      util_warning(
        "No %s defined in %s, skipping the check for unexpected record set",
        dQuote("SEGMENT_ID_VARS"),
        dQuote("meta_data_segment"),
        applicability_problem = TRUE,
        intrinsic_applicability_problem = TRUE
      )
      return(list(empty_record_set, data.frame()))
    }

    id_vars <- util_int_datastructure_keep_existing_vars(
      id_vars,
      study_data,
      sprintf("ID variables in current segment %s", dQuote(current_segment))
    )
    segment_vars <- util_int_datastructure_keep_existing_vars(
      meta_data[meta_data[[STUDY_SEGMENT]] == current_segment, label_col, drop = TRUE], # nolint: line_length_linter.
      study_data,
      sprintf("Study variables from current segment %s", dQuote(current_segment)) # nolint: line_length_linter.
    )
    data_ids <- util_remove_empty_rows(
      study_data[, c(id_vars, segment_vars), drop = FALSE],
      id_vars = id_vars
    )

    if (current_segment %in% colnames(valid_id_table)) {
      metadata_ids <- valid_id_table[, current_segment, drop = TRUE]
    } else if ("ID" %in% colnames(valid_id_table)) {
      metadata_ids <- valid_id_table[, "ID", drop = TRUE]
    } else {
      metadata_ids <- character(0)
    }
    metadata_ids <- metadata_ids[!util_empty(metadata_ids)]

    if (length(id_vars) > 1) {
      util_warning(
        c(
          "Check for multiple IDs is not currently supported,",
          "but you could assign value labels to the each variable"
        ),
        applicability_problem = TRUE,
        intrinsic_applicability_problem = TRUE
      )
      return(list(empty_record_set, data.frame()))
    }

    data_values <- data_ids[[id_vars]]
    unexpected_ids <- data_values[!(data_values %in% metadata_ids)]
    match_expected <- expected_checks[[current_segment]]
    result_data <- util_int_datastructure_record_set_data(
      level_col = "Segment",
      level = current_segment,
      data_values = data_values,
      metadata_ids = metadata_ids,
      unexpected_ids = unexpected_ids,
      match_expected = match_expected,
      mismatch_denominator = length(data_values) + length(metadata_ids)
    )
    other <- util_int_datastructure_unexpected_ids(
      level_col = "Segment",
      level = current_segment,
      unexpected_ids = unexpected_ids,
      data_values = data_values,
      metadata_ids = metadata_ids
    )
    if (!identical(other_column_order, "unexpected_first")) {
      other <- other[c("Segment", "Line", "UnexpectedID")]
    }

    list(result_data, other)
  })

  data <- do.call(rbind.data.frame, lapply(result, `[[`, 1))

  list(
    SegmentData = data,
    SegmentTable = data.frame(
      Segment = data$Segment,
      NUM_int_sts_setrc = data$`Number of mismatches`,
      PCT_int_sts_setrc = data$`Percentage of mismatches`,
      GRADING = data$GRADING,
      stringsAsFactors = FALSE
    ),
    Other = dplyr::bind_rows(lapply(result, `[[`, 2))
  )
}

#' Build the shared record-set result rows
#'
#' @noRd
util_int_datastructure_record_set_data <- function(level_col,
  level,
  data_values,
  metadata_ids,
  unexpected_ids,
  match_expected,
  mismatch_denominator) {
  match_actual <- util_int_datastructure_match_type(data_values, metadata_ids)
  data <- data.frame(
    check.names = FALSE,
    "Check" = "Record set",
    level,
    "Unexpected records in set?" = length(unexpected_ids) != 0,
    "Number of records in data" = length(data_values),
    "Number of records in metadata" = length(metadata_ids),
    "Number of mismatches" = length(unexpected_ids),
    "Percentage of mismatches" =
      abs(round(100 * length(unexpected_ids) / mismatch_denominator, 3)),
    "Expected match type" = match_expected,
    "Actual match type" = match_actual,
    "GRADING" = ifelse(match_expected[1] == match_actual, 0, 1),
    stringsAsFactors = FALSE
  )
  colnames(data)[2] <- level_col
  data
}

#' Build the shared record-set `Other` rows
#'
#' @noRd
util_int_datastructure_unexpected_ids <- function(level_col,
  level,
  unexpected_ids,
  data_values,
  metadata_ids) {
  mismatch <- !(data_values %in% metadata_ids)
  unexpected_ids <- data_values[mismatch]
  lines <- which(mismatch)
  keep <- !util_empty(unexpected_ids)
  unexpected_ids <- unexpected_ids[keep]
  lines <- lines[keep]
  other <- data.frame(
    level = rep(level, length(unexpected_ids)),
    UnexpectedID = unexpected_ids,
    Line = as.character(lines)
  )
  colnames(other)[1] <- level_col
  other
}

#' Check segment-level duplicate IDs for `int_all_datastructure_segment()`
#'
#' @noRd
util_int_datastructure_segment_duplicate_ids <- function(meta_data_segment,
  study_data,
  meta_data,
  label_col,
  id_vars_list) {
  meta_data_dup_ids <- meta_data_segment[
    !util_empty(meta_data_segment[[SEGMENT_ID_VARS]]) &
      !util_empty(meta_data_segment[[SEGMENT_UNIQUE_ID]]), ,
    drop = FALSE
  ]
  segments <- meta_data_dup_ids[[STUDY_SEGMENT]]
  if (missing(id_vars_list)) {
    id_vars_list <- util_int_datastructure_id_vars(
      meta_data_dup_ids,
      STUDY_SEGMENT,
      SEGMENT_ID_VARS,
      map_meta_data = meta_data,
      label_col = label_col
    )
  }
  id_vars_list <- id_vars_list[segments]
  repetitions <- setNames(meta_data_dup_ids[[SEGMENT_UNIQUE_ID]], segments)
  if (missing(label_col)) {
    label_col <- util_attr(study_data, "label_col", exact = TRUE)
  }

  segments <- intersect(segments, meta_data[[STUDY_SEGMENT]])

  result <- lapply(setNames(nm = segments), function(current_segment) {
    id_vars <- util_find_var_by_meta(
      resp_vars = id_vars_list[[current_segment]],
      meta_data = meta_data,
      label_col = label_col,
      target = label_col,
      ifnotfound = id_vars_list[[current_segment]]
    )
    id_vars <- util_int_datastructure_keep_existing_vars(
      id_vars,
      study_data,
      sprintf("ID variables in current segment %s", dQuote(current_segment))
    )

    if (length(id_vars) == 0) {
      util_warning(
        "The %s (%s) in the %s are not included in the %s",
        "ID variables",
        util_pretty_vector_string(id_vars_list[[current_segment]]),
        "metadata",
        "study data",
        applicability_problem = TRUE
      )
      return(list(
        util_int_datastructure_missing_duplicate_data(
          "Segment",
          current_segment
        ),
        NULL
      ))
    }

    segment_vars <- util_int_datastructure_keep_existing_vars(
      util_get_vars_in_segment(
        segment = current_segment,
        meta_data = meta_data,
        label_col = label_col
      ),
      study_data,
      sprintf("Study variables from current segment %s", dQuote(current_segment)) # nolint: line_length_linter.
    )
    data_reduced <- util_remove_empty_rows(
      study_data[, c(id_vars, segment_vars), drop = FALSE],
      id_vars = id_vars
    )

    dup_info <- util_find_duplicated_rows(
      study_data = data_reduced,
      id_vars = id_vars,
      repeptitions = repetitions[[current_segment]]
    )
    n_uniq <- nrow(data_reduced) - sum(dup_info$unexp_reps)
    n_total <- nrow(data_reduced)

    result_data <- util_int_datastructure_duplicate_data(
      check = "IDs",
      level_col = "Segment",
      level = current_segment,
      n_total = n_total,
      n_unique = n_uniq,
      id_vars = id_vars
    )

    duplicated_rows <- if (result_data[[3]]) dup_info$which else NULL

    list(result_data, duplicated_rows)
  })

  data <- do.call(rbind.data.frame, lapply(result, `[[`, 1))

  list(
    SegmentData = data,
    SegmentTable = util_int_datastructure_duplicate_table(
      data,
      level_col = "Segment",
      num_col = "NUM_int_sts_dupl_ids",
      pct_col = "PCT_int_sts_dupl_ids"
    ),
    Other = lapply(result, `[[`, 2)
  )
}

#' Build the shared duplicate-check result rows
#'
#' @noRd
util_int_datastructure_duplicate_data <- function(check,
  level_col,
  level,
  n_total,
  n_unique,
  id_vars) {
  n_duplicates <- n_total - n_unique
  data <- data.frame(
    check.names = FALSE,
    "Check" = check,
    level,
    "Any duplicates" = n_duplicates > 0,
    "Number of duplicates" = n_duplicates,
    "Percentage of duplicates" = round(100 * n_duplicates / n_total, 3),
    "GRADING" = as.integer(n_duplicates > 0),
    stringsAsFactors = FALSE
  )
  colnames(data)[2] <- level_col

  if (!missing(id_vars)) {
    data[["ID Vars"]] <- prep_deparse_assignments(
      id_vars,
      mode = "string_codes"
    )
  }

  data
}

#' Build duplicate-check rows for missing ID variables
#'
#' @noRd
util_int_datastructure_missing_duplicate_data <- function(level_col, level) {
  data <- data.frame(
    check.names = FALSE,
    "Check" = "IDs",
    level,
    "Any duplicates" = NA,
    "Number of duplicates" = NA_real_,
    "Percentage of duplicates" = NA_real_,
    "GRADING" = NA_real_,
    stringsAsFactors = FALSE
  )
  colnames(data)[2] <- level_col
  data
}

#' Build report-table rows for duplicate checks
#'
#' @noRd
util_int_datastructure_duplicate_table <- function(data,
  level,
  level_col,
  num_col,
  pct_col,
  source_col = level_col) {
  table <- data.frame(
    data[[source_col]],
    data[["Number of duplicates"]],
    data[["Percentage of duplicates"]],
    data[["GRADING"]],
    stringsAsFactors = FALSE
  )
  colnames(table) <- c(level_col, num_col, pct_col, "GRADING")
  if (!missing(level)) {
    table <- cbind(Level = level, table)
  }
  table
}

#' Build duplicate-content data and report-table rows
#'
#' @noRd
util_int_datastructure_duplicate_content_result <- function(data_by_level,
  level_col,
  check,
  num_col,
  pct_col,
  table_level,
  table_level_col =
    level_col,
  source_col =
    level_col) {
  if (length(data_by_level) == 0) {
    return(list(data = setNames(list(), character()), table = data.frame()))
  }

  data <- do.call(rbind.data.frame, Map(
    f = function(level, level_data) {
      util_int_datastructure_duplicate_data(
        check = check,
        level_col = level_col,
        level = level,
        n_total = nrow(level_data),
        n_unique = nrow(unique(level_data))
      )
    },
    level = names(data_by_level),
    level_data = data_by_level
  ))
  table <- util_int_datastructure_duplicate_table(
    data,
    level_col = table_level_col,
    num_col = num_col,
    pct_col = pct_col,
    source_col = source_col
  )
  if (!missing(table_level)) {
    table <- cbind(Level = table_level, table)
  }
  list(data = data, table = table)
}

#' Build an empty record-set table with the historical columns
#'
#' @noRd
util_int_datastructure_empty_record_set <- function(level) {
  data.frame(
    Level = level,
    NUM_int_sts_setrc = 0,
    PCT_int_sts_setrc = 0,
    GRADING = 0,
    stringsAsFactors = FALSE
  )[FALSE, , drop = FALSE]
}

#' Keep metadata rows requesting duplicate-content checks
#'
#' @noRd
util_int_datastructure_unique_rows_metadata <- function(meta_data,
  unique_rows_col,
  filter_metadata) {
  if (!filter_metadata) {
    return(meta_data)
  }

  unique_rows <- trimws(tolower(meta_data[[unique_rows_col]]))
  meta_data[
    !util_empty(meta_data[[unique_rows_col]]) &
      (unique_rows == "no_id" |
          !util_is_na_0_empty_or_false(meta_data[[unique_rows_col]])), ,
    drop = FALSE
  ]
}

#' Check if named cached data frames can be loaded
#'
#' @noRd
util_int_datastructure_can_load_data_frame <- function(data_frame_names,
  keep_types = FALSE) {
  vapply(
    data_frame_names,
    function(data_frame_name) {
      !util_is_try_error(try(
        prep_get_data_frame(
          data_frame_name = data_frame_name,
          keep_types = keep_types
        ),
        silent = TRUE
      ))
    },
    FUN.VALUE = logical(1)
  )
}

#' Parse and optionally map ID variable metadata
#'
#' @noRd
util_int_datastructure_id_vars <- function(meta_data,
  name_col,
  id_col,
  map_meta_data,
  label_col) {
  id_vars_list <- lapply(
    setNames(
      meta_data[[id_col]],
      nm = meta_data[[name_col]]
    ),
    util_parse_assignments,
    multi_variate_text = TRUE
  )
  id_vars_list <- lapply(id_vars_list, unlist, recursive = TRUE)
  if (missing(map_meta_data)) {
    id_vars_list
  } else {
    lapply(
      id_vars_list,
      util_map_labels,
      meta_data = map_meta_data,
      to = label_col
    )
  }
}

#' Build dataframe-level metadata from legacy explicit arguments
#'
#' @noRd
util_int_datastructure_df_metadata <- function(identifier_name_list,
  id_vars_list,
  repetitions,
  unique_rows,
  valid_id_table_list,
  meta_data_record_check_list) {
  if (is.null(names(id_vars_list))) {
    names(id_vars_list) <- identifier_name_list
  }
  data.frame(
    DF_NAME = identifier_name_list,
    DF_ID_VARS = vapply(
      id_vars_list,
      prep_deparse_assignments,
      character(1),
      mode = "string_codes"
    ),
    DF_UNIQUE_ID = if (missing(repetitions)) {
      rep(NA_character_, length(identifier_name_list))
    } else {
      util_int_datastructure_named_arg(repetitions, identifier_name_list)
    },
    DF_UNIQUE_ROWS = if (missing(unique_rows)) {
      rep(NA_character_, length(identifier_name_list))
    } else {
      util_int_datastructure_named_arg(unique_rows, identifier_name_list)
    },
    DF_ID_REF_TABLE = if (missing(valid_id_table_list)) {
      I(rep(list(NA_character_), length(identifier_name_list)))
    } else {
      I(util_int_datastructure_named_arg(
        valid_id_table_list,
        identifier_name_list
      ))
    },
    DF_RECORD_CHECK = if (missing(meta_data_record_check_list)) {
      rep(NA_character_, length(identifier_name_list))
    } else {
      util_int_datastructure_named_arg(
        meta_data_record_check_list,
        identifier_name_list
      )
    },
    stringsAsFactors = FALSE
  )
}

#' Build segment-level metadata from legacy explicit arguments
#'
#' @noRd
util_int_datastructure_segment_metadata <- function(identifier_name_list,
  id_vars_list,
  valid_id_table_list,
  meta_data_record_check_list,
  repetitions,
  unique_rows) {
  if (is.null(names(id_vars_list))) {
    names(id_vars_list) <- identifier_name_list
  }
  data.frame(
    STUDY_SEGMENT = identifier_name_list,
    SEGMENT_ID_VARS = vapply(
      id_vars_list,
      prep_deparse_assignments,
      character(1),
      mode = "string_codes"
    ),
    SEGMENT_ID_REF_TABLE = if (missing(valid_id_table_list)) {
      I(rep(list(NA_character_), length(identifier_name_list)))
    } else {
      I(util_int_datastructure_named_arg(
        valid_id_table_list,
        identifier_name_list
      ))
    },
    SEGMENT_RECORD_CHECK = if (missing(meta_data_record_check_list)) {
      rep(NA_character_, length(identifier_name_list))
    } else {
      util_int_datastructure_named_arg(
        meta_data_record_check_list,
        identifier_name_list
      )
    },
    SEGMENT_UNIQUE_ID = if (missing(repetitions)) {
      rep(NA_character_, length(identifier_name_list))
    } else {
      util_int_datastructure_named_arg(repetitions, identifier_name_list)
    },
    SEGMENT_UNIQUE_ROWS = if (missing(unique_rows)) {
      rep(NA_character_, length(identifier_name_list))
    } else {
      util_int_datastructure_named_arg(unique_rows, identifier_name_list)
    },
    stringsAsFactors = FALSE
  )
}

#' Align legacy explicit-argument vectors/lists to metadata names
#'
#' @noRd
util_int_datastructure_named_arg <- function(x, identifier_name_list) {
  if (is.null(names(x))) {
    names(x) <- identifier_name_list
  }
  unname(x[identifier_name_list])
}

#' Report unsupported mixed legacy-argument modes
#'
#' @noRd
util_int_datastructure_legacy_mode_error <- function(meta_arg,
  explicit_args,
  has_meta_arg) {
  if (has_meta_arg) {
    util_error(
      c(
        "I have %s and one of the following: %s.",
        "This is not supported, please provide",
        "either %s or all of %s."
      ),
      sQuote(meta_arg),
      util_pretty_vector_string(explicit_args),
      sQuote(meta_arg),
      util_pretty_vector_string(explicit_args)
    )
  } else {
    util_error(
      c(
        "I don't have %s and also miss at least",
        "one of the following: %s.",
        "This is not supported, please provide",
        "either %s or all of %s."
      ),
      sQuote(meta_arg),
      util_pretty_vector_string(explicit_args),
      sQuote(meta_arg),
      util_pretty_vector_string(explicit_args)
    )
  }
}

#' Decide whether an internal wrapper uses metadata or explicit arguments
#'
#' @noRd
util_int_datastructure_use_metadata <- function(meta_arg,
  has_meta_arg,
  missing_explicit_args,
  explicit_args) {
  if (all(missing_explicit_args) && has_meta_arg) {
    return(TRUE)
  }

  if (has_meta_arg) {
    util_int_datastructure_legacy_mode_error(
      meta_arg,
      explicit_args,
      has_meta_arg = TRUE
    )
  }

  if (any(missing_explicit_args)) {
    util_int_datastructure_legacy_mode_error(
      meta_arg,
      explicit_args,
      has_meta_arg = FALSE
    )
  }

  FALSE
}

#' Validate duplicate-content row settings
#'
#' @noRd
util_int_datastructure_validate_unique_rows <- function(unique_rows) {
  util_expect_scalar(
    unique_rows,
    allow_more_than_one = TRUE,
    check_type = function(x) {
      all(util_empty(x) | tolower(trimws(x)) %in%
          c("f", "t", "true", "false", "no_id"))
    }
  )
}

#' Add a compact `N (%)` and grading pair to summary data
#'
#' @noRd
util_int_datastructure_add_summary_columns <- function(summary_data,
  source_data,
  label) {
  n_col <- sprintf("%s (Number)", label)
  pct_col <- sprintf("%s (Percentage (0 to 100))", label)
  n_pct_col <- sprintf("%s N (%%)", label)
  grading_col <- sprintf("%s (Grading)", label)

  if (!is.null(source_data[[n_col]])) {
    summary_data[[n_pct_col]] <- util_paste0_with_na(
      source_data[[n_col]],
      " (",
      source_data[[pct_col]],
      ")"
    )
    attr(summary_data[[n_pct_col]], DATA_TYPE) <- DATA_TYPE_NUMBER_PAREN
  }
  if (!is.null(source_data[[grading_col]])) {
    summary_data[[grading_col]] <- source_data[[grading_col]]
  }

  summary_data
}

#' Dispatch dataframe/segment integrity wrappers
#'
#' @noRd
util_int_level_dispatch <- function(fname,
  level,
  study_data,
  has_study_data,
  item_level,
  has_item_level,
  label_col,
  has_label_col,
  meta_data,
  has_meta_data,
  include_item_level,
  ...) {
  cl_l <- list(fname,
    level = level, meta_data = meta_data,
    label_col = label_col, ...
  )
  if (has_study_data) {
    cl_l$study_data <- study_data
  }
  if (include_item_level) {
    cl_l$item_level <- item_level
  }
  if (include_item_level && !has_item_level && has_meta_data) {
    cl_l$item_level <- NULL
  }
  if (!has_label_col) {
    cl_l$label_col <- NULL
  }

  cl_l <- cl_l[names(cl_l) %in% c("", names(formals(fname)))]
  eval(do.call("call", cl_l))
}

#' Classify the match between data IDs and metadata IDs
#'
#' @noRd
util_int_datastructure_match_type <- function(data_ids, metadata_ids) {
  if (all(data_ids %in% metadata_ids) && all(metadata_ids %in% data_ids)) {
    "exact"
  } else if (all(data_ids %in% metadata_ids)) {
    "subset"
  } else if (all(metadata_ids %in% data_ids)) {
    "superset"
  } else {
    "mismatch"
  }
}

#' Keep variables that exist in the study data
#'
#' @noRd
util_int_datastructure_keep_existing_vars <- function(vars,
  study_data,
  context) {
  util_ensure_in(
    vars,
    colnames(study_data),
    err_msg = c(
      context,
      ": Missing %s from the study data,",
      "did you mean %s? I'll remove the missing entries"
    ),
    applicability_problem = TRUE
  )
}

#' Check for duplicated dataframe IDs
#'
#' @noRd
util_int_duplicate_ids_dataframe <- function(level = c("dataframe"),
  id_vars_list,
  identifier_name_list,
  repetitions,
  meta_data_dataframe =
    "dataframe_level",
  ...,
  dataframe_level) {
  util_ck_arg_aliases()
  level <- util_match_arg(level)

  if (missing(id_vars_list) &&
      missing(identifier_name_list) &&
      missing(repetitions) &&
      missing(meta_data_dataframe) &&
      formals()$meta_data_dataframe %in% prep_list_dataframes()) {
    meta_data_dataframe <- force(meta_data_dataframe)
  }

  use_metadata <- util_int_datastructure_use_metadata(
    "meta_data_dataframe",
    !missing(meta_data_dataframe),
    c(
      missing(id_vars_list), missing(identifier_name_list),
      missing(repetitions)
    ),
    c("id_vars_list", "identifier_name_list", "repetitions")
  )

  if (use_metadata) {
    meta_data_dataframe <- prep_check_meta_data_dataframe(meta_data_dataframe)
    meta_data_dataframe <- meta_data_dataframe[
      !util_empty(meta_data_dataframe[[DF_UNIQUE_ID]]) &
        as.numeric(meta_data_dataframe[[DF_UNIQUE_ID]]) > 0 &
        !util_empty(meta_data_dataframe[[DF_ID_VARS]]), ,
      drop = FALSE
    ]
    identifier_name_list <- meta_data_dataframe[[DF_NAME]]
  } else {
    meta_data_dataframe <- util_int_datastructure_df_metadata(
      identifier_name_list = identifier_name_list,
      id_vars_list = id_vars_list,
      repetitions = repetitions
    )
  }

  if (missing(id_vars_list)) {
    id_vars_list <- util_int_datastructure_id_vars(
      meta_data_dataframe,
      DF_NAME,
      DF_ID_VARS
    )
  }
  study_data_list <- lapply(
    setNames(nm = identifier_name_list),
    util_expect_data_frame,
    dont_assign = TRUE
  )

  util_int_datastructure_df_duplicate_ids(
    meta_data_dataframe,
    study_data_list,
    id_vars_list = id_vars_list
  )
}

#' Check for duplicated segment IDs
#'
#' @noRd
util_int_duplicate_ids_segment <- function(level = c("segment"),
  id_vars_list,
  study_segment,
  repetitions,
  study_data,
  meta_data,
  meta_data_segment = "segment_level",
  segment_level) {
  util_ck_arg_aliases()
  level <- util_match_arg(level)

  if (missing(id_vars_list) &&
      missing(study_segment) &&
      missing(repetitions) &&
      missing(meta_data_segment) &&
      formals()$meta_data_segment %in% prep_list_dataframes()) {
    meta_data_segment <- force(meta_data_segment)
  }

  use_metadata <- util_int_datastructure_use_metadata(
    "meta_data_segment",
    !missing(meta_data_segment),
    c(missing(id_vars_list), missing(study_segment), missing(repetitions)),
    c("id_vars_list", "study_segment", "repetitions")
  )

  if (use_metadata) {
    meta_data_segment <- prep_check_meta_data_segment(meta_data_segment)
    meta_data_segment <- meta_data_segment[
      !util_empty(meta_data_segment[[SEGMENT_ID_VARS]]) &
        !util_empty(meta_data_segment[[SEGMENT_UNIQUE_ID]]), ,
      drop = FALSE
    ]
  } else {
    meta_data_segment <- util_int_datastructure_segment_metadata(
      identifier_name_list = study_segment,
      id_vars_list = id_vars_list,
      repetitions = repetitions
    )
  }

  prep_prepare_dataframes(.allow_empty = TRUE)
  label_col <- util_attr(ds1, "label_col", exact = TRUE)
  if (is.null(label_col)) {
    label_col <- VAR_NAMES
  }

  if (missing(id_vars_list)) {
    util_int_datastructure_segment_duplicate_ids(
      meta_data_segment,
      ds1,
      meta_data,
      label_col
    )
  } else {
    util_int_datastructure_segment_duplicate_ids(
      meta_data_segment,
      ds1,
      meta_data,
      label_col,
      id_vars_list = id_vars_list
    )
  }
}

#' Find duplicated ID rows
#'
#' @noRd
util_find_duplicated_rows <- function(study_data,
  id_vars,
  repeptitions = 1) {
  util_expect_data_frame(study_data, col_names = id_vars)
  study_data <- study_data[, id_vars, drop = FALSE]
  empty_sd <- study_data
  empty_sd[] <- lapply(study_data, util_empty)
  study_data[as.matrix(empty_sd)] <- NA
  r <- as.data.frame(table(study_data, useNA = "no"))
  r <- r[r$Freq > repeptitions, , drop = FALSE]
  nsd <- cbind(study_data,
    ..rownr = seq_len(nrow(study_data))
  )
  r$which <- lapply(
    seq_len(nrow(r)),
    function(my_dup_rw) {
      my_dup <- r[my_dup_rw, , drop = FALSE]
      dd <- merge(nsd, my_dup, by = id_vars)
      prep_deparse_assignments(dd$..rownr,
        mode = "string_codes"
      )
    }
  )
  r$unexp_reps <- r$Freq - repeptitions
  r
}

#' Check for duplicated dataframe content
#'
#' @noRd
util_int_duplicate_content_dataframe <- function(level = c("dataframe"),
  identifier_name_list,
  id_vars_list,
  unique_rows,
  meta_data_dataframe =
    "dataframe_level",
  ...,
  dataframe_level) {
  util_ck_arg_aliases()
  level <- util_match_arg(level)

  if (missing(identifier_name_list) &&
      missing(id_vars_list) &&
      missing(meta_data_dataframe) &&
      formals()$meta_data_dataframe %in% prep_list_dataframes()) {
    meta_data_dataframe <- force(meta_data_dataframe)
  }

  use_metadata <- util_int_datastructure_use_metadata(
    "meta_data_dataframe",
    !missing(meta_data_dataframe),
    c(missing(identifier_name_list), missing(id_vars_list)),
    c("identifier_name_list", "id_vars_list")
  )

  if (use_metadata) {
    meta_data_dataframe <- prep_check_meta_data_dataframe(meta_data_dataframe)
    identifier_name_list <- meta_data_dataframe[[DF_NAME]]
  } else {
    if (missing(unique_rows)) {
      unique_rows <- setNames(
        rep(NA_character_, length(identifier_name_list)),
        identifier_name_list
      )
    }
    meta_data_dataframe <- util_int_datastructure_df_metadata(
      identifier_name_list = identifier_name_list,
      id_vars_list = id_vars_list,
      unique_rows = unique_rows
    )
  }

  if (missing(id_vars_list)) {
    id_vars_list <- util_int_datastructure_id_vars(
      meta_data_dataframe,
      DF_NAME,
      DF_ID_VARS
    )
  }
  if (missing(unique_rows)) {
    unique_rows <- meta_data_dataframe[[DF_UNIQUE_ROWS]]
  }

  util_int_datastructure_validate_unique_rows(unique_rows)

  study_data_list <- lapply(
    setNames(nm = identifier_name_list),
    util_expect_data_frame,
    dont_assign = TRUE
  )

  meta_data_dup_rows <- util_int_datastructure_unique_rows_metadata(
    meta_data_dataframe,
    DF_UNIQUE_ROWS,
    filter_metadata = use_metadata
  )
  df_names <- meta_data_dup_rows[[DF_NAME]]
  id_vars_list <- id_vars_list[df_names]
  unique_rows <- setNames(
    tolower(trimws(meta_data_dup_rows[[DF_UNIQUE_ROWS]])),
    df_names
  )
  unique_rows[util_empty(unique_rows)] <- "false"

  duplicate_data_frames <- lapply(
    setNames(nm = df_names),
    function(current_df) {
      data_current_df <- study_data_list[[current_df]]
      if (unique_rows[[current_df]] == "no_id") {
        data_current_df <- data_current_df[
          ,
          setdiff(colnames(data_current_df), id_vars_list[[current_df]]),
          drop = FALSE
        ]
      }
      data_current_df
    }
  )
  result <- util_int_datastructure_duplicate_content_result(
    duplicate_data_frames,
    level_col = "Data frame",
    check = "Duplicates",
    num_col = "NUM_int_sts_dupl_content",
    pct_col = "PCT_int_sts_dupl_content",
    table_level = "Dataframe",
    table_level_col = DF_NAME,
    source_col = "Data frame"
  )

  list(
    DataframeData = result$data,
    DataframeTable = result$table,
    Other = data.frame()
  )
}

#' Check for duplicated segment content
#'
#' @noRd
util_int_duplicate_content_segment <- function(level = c("segment"),
  identifier_name_list,
  id_vars_list,
  unique_rows,
  study_data,
  meta_data,
  meta_data_segment =
    "segment_level",
  segment_level) {
  util_ck_arg_aliases()
  level <- util_match_arg(level)

  if (missing(identifier_name_list) &&
      missing(id_vars_list) &&
      missing(meta_data_segment) &&
      formals()$meta_data_segment %in% prep_list_dataframes()) {
    meta_data_segment <- force(meta_data_segment)
  }

  use_metadata <- util_int_datastructure_use_metadata(
    "meta_data_segment",
    !missing(meta_data_segment),
    c(missing(identifier_name_list), missing(id_vars_list)),
    c("identifier_name_list", "id_vars_list")
  )

  if (use_metadata) {
    meta_data_segment <- prep_check_meta_data_segment(meta_data_segment)
    meta_data_segment <- util_int_datastructure_unique_rows_metadata(
      meta_data_segment,
      SEGMENT_UNIQUE_ROWS,
      filter_metadata = TRUE
    )
  } else {
    if (missing(unique_rows)) {
      unique_rows <- setNames(
        rep(NA_character_, length(identifier_name_list)),
        identifier_name_list
      )
    }
    meta_data_segment <- util_int_datastructure_segment_metadata(
      identifier_name_list = identifier_name_list,
      id_vars_list = id_vars_list,
      unique_rows = unique_rows
    )
  }

  if (missing(unique_rows)) {
    unique_rows <- meta_data_segment[[SEGMENT_UNIQUE_ROWS]]
  }
  util_int_datastructure_validate_unique_rows(unique_rows)

  prep_prepare_dataframes(.allow_empty = TRUE)

  meta_data_dup_rows <- util_int_datastructure_unique_rows_metadata(
    meta_data_segment,
    SEGMENT_UNIQUE_ROWS,
    filter_metadata = use_metadata
  )
  segments <- intersect(
    meta_data_dup_rows[[STUDY_SEGMENT]],
    meta_data[[STUDY_SEGMENT]]
  )
  if (missing(id_vars_list)) {
    id_vars_list <- util_int_datastructure_id_vars(
      meta_data_dup_rows,
      STUDY_SEGMENT,
      SEGMENT_ID_VARS,
      map_meta_data = meta_data,
      label_col = VAR_NAMES
    )
  }
  id_vars_list <- id_vars_list[segments]
  unique_rows <- setNames(
    tolower(trimws(meta_data_dup_rows[[SEGMENT_UNIQUE_ROWS]])),
    meta_data_dup_rows[[STUDY_SEGMENT]]
  )
  unique_rows[util_empty(unique_rows)] <- "false"

  segment_data_list <- lapply(
    setNames(nm = segments),
    function(current_segment) {
      segment_vars <- util_get_vars_in_segment(
        segment = current_segment,
        meta_data = meta_data,
        label_col = VAR_NAMES
      )
      if (unique_rows[[current_segment]] == "no_id") {
        segment_vars <- setdiff(segment_vars, id_vars_list[[current_segment]])
      }
      segment_data <- ds1[
        ,
        intersect(colnames(ds1), segment_vars),
        drop = FALSE
      ]
      util_remove_empty_rows(segment_data)
    }
  )
  result <- util_int_datastructure_duplicate_content_result(
    segment_data_list,
    level_col = "Segment",
    check = "Duplicate records",
    num_col = "NUM_int_sts_dupl_content",
    pct_col = "PCT_int_sts_dupl_content"
  )

  list(
    SegmentData = result$data,
    SegmentTable = result$table,
    Other = data.frame()
  )
}

#' Check for unexpected dataframe record sets
#'
#' @noRd
util_int_unexp_records_set_dataframe <- function(level = c("dataframe"),
  id_vars_list,
  identifier_name_list,
  valid_id_table_list,
  meta_data_record_check_list,
  meta_data_dataframe =
    "dataframe_level",
  ...,
  dataframe_level) {
  util_ck_arg_aliases()
  level <- util_match_arg(level)

  if (missing(id_vars_list) &&
      missing(identifier_name_list) &&
      missing(valid_id_table_list) &&
      missing(meta_data_record_check_list) &&
      missing(meta_data_dataframe) &&
      formals()$meta_data_dataframe %in% prep_list_dataframes()) {
    meta_data_dataframe <- force(meta_data_dataframe)
  }

  use_metadata <- util_int_datastructure_use_metadata(
    "meta_data_dataframe",
    !missing(meta_data_dataframe),
    c(
      missing(id_vars_list), missing(identifier_name_list),
      missing(valid_id_table_list), missing(meta_data_record_check_list)
    ),
    c(
      "id_vars_list", "identifier_name_list", "valid_id_table_list",
      "meta_data_record_check_list"
    )
  )

  if (use_metadata) {
    meta_data_dataframe <- prep_check_meta_data_dataframe(meta_data_dataframe)
    meta_data_dataframe <- meta_data_dataframe[
      util_int_datastructure_can_load_data_frame(
        meta_data_dataframe[[DF_NAME]],
        keep_types = TRUE
      ), ,
      drop = FALSE
    ]
    meta_data_dataframe <- meta_data_dataframe[
      util_int_datastructure_can_load_data_frame(
        meta_data_dataframe[[DF_ID_REF_TABLE]]
      ), ,
      drop = FALSE
    ]
  } else {
    meta_data_dataframe <- util_int_datastructure_df_metadata(
      identifier_name_list = identifier_name_list,
      id_vars_list = id_vars_list,
      valid_id_table_list = valid_id_table_list,
      meta_data_record_check_list = meta_data_record_check_list
    )
  }

  if (missing(identifier_name_list)) {
    identifier_name_list <- meta_data_dataframe[[DF_NAME]]
  }
  if (missing(id_vars_list)) {
    id_vars_list <- util_int_datastructure_id_vars(
      meta_data_dataframe,
      DF_NAME,
      DF_ID_VARS
    )
  }
  if (missing(valid_id_table_list)) {
    valid_id_table_list <- meta_data_dataframe[[DF_ID_REF_TABLE]]
  }
  if (missing(meta_data_record_check_list)) {
    meta_data_record_check_list <- meta_data_dataframe[[DF_RECORD_CHECK]]
  }

  study_data_list <- lapply(
    setNames(nm = identifier_name_list),
    util_expect_data_frame,
    dont_assign = TRUE
  )

  util_int_datastructure_df_record_set(
    meta_data_dataframe,
    study_data_list,
    id_vars_list = id_vars_list
  )
}

#' Check for unexpected segment record sets
#'
#' @noRd
util_int_unexp_records_set_segment <- function(level = c("segment"),
  id_vars_list,
  identifier_name_list,
  valid_id_table_list,
  meta_data_record_check_list,
  study_data,
  label_col,
  meta_data,
  item_level,
  meta_data_segment =
    "segment_level",
  segment_level) {
  util_ck_arg_aliases()
  level <- util_match_arg(level)

  if (missing(id_vars_list) &&
      missing(identifier_name_list) &&
      missing(valid_id_table_list) &&
      missing(meta_data_record_check_list) &&
      missing(meta_data_segment) &&
      formals()$meta_data_segment %in% prep_list_dataframes()) {
    meta_data_segment <- force(meta_data_segment)
  }

  use_metadata <- util_int_datastructure_use_metadata(
    "meta_data_segment",
    !missing(meta_data_segment),
    c(
      missing(id_vars_list), missing(identifier_name_list),
      missing(valid_id_table_list), missing(meta_data_record_check_list)
    ),
    c(
      "id_vars_list", "identifier_name_list", "valid_id_table_list",
      "meta_data_record_check_list"
    )
  )

  if (use_metadata) {
    meta_data_segment <- prep_check_meta_data_segment(meta_data_segment)
  } else {
    meta_data_segment <- util_int_datastructure_segment_metadata(
      identifier_name_list = identifier_name_list,
      id_vars_list = id_vars_list,
      valid_id_table_list = valid_id_table_list,
      meta_data_record_check_list = meta_data_record_check_list
    )
  }

  if (missing(identifier_name_list)) {
    identifier_name_list <- meta_data_segment[[STUDY_SEGMENT]]
  }
  if (missing(id_vars_list)) {
    if (missing(label_col)) {
      label_col <- VAR_NAMES
    }
    id_vars_list <- util_int_datastructure_id_vars(
      meta_data_segment,
      STUDY_SEGMENT,
      SEGMENT_ID_VARS,
      map_meta_data = meta_data,
      label_col = label_col
    )
  }
  if (missing(valid_id_table_list)) {
    valid_id_table_list <- meta_data_segment[[SEGMENT_ID_REF_TABLE]]
  }
  if (missing(meta_data_record_check_list)) {
    meta_data_record_check_list <- meta_data_segment[[SEGMENT_RECORD_CHECK]]
  }

  names(id_vars_list) <- identifier_name_list
  names(meta_data_record_check_list) <- identifier_name_list
  names(valid_id_table_list) <- identifier_name_list

  prep_prepare_dataframes()
  other_column_order <- if (use_metadata) {
    "line_first"
  } else {
    "unexpected_first"
  }
  util_int_datastructure_segment_record_set(
    meta_data_segment,
    ds1,
    meta_data,
    label_col,
    id_vars_list = id_vars_list,
    other_column_order = other_column_order
  )
}
