# nolint start: line_length_linter.
#' Wrapper function to check for studies data structure
#'
#' @description
#' This function tests for unexpected elements and records, as well as duplicated identifiers and content.
#' The unexpected element record check can be conducted by providing the number of expected records or
#' an additional table with the expected records.
#' It is possible to conduct the checks by study segments or to consider only selected
#' segments.
#'
#' [Indicator]
#'
#' @inheritParams .template_function_indicator
#'
#' @return a [list] with
#'   - `DataframeTable`: data frame with selected check results, used for the data quality report.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' out_dataframe <- int_all_datastructure_dataframe(
#'   meta_data_dataframe = "meta_data_dataframe",
#'   meta_data = "ship_meta"
#' )
#' md0 <- prep_get_data_frame("ship_meta")
#' md0
#' md0[[VAR_NAMES]]
#' md0[[VAR_NAMES]][[1]] <- "Id" # is this missmatch reported -- is the data frame
#' # also reported, if nothing is wrong with it
#' out_dataframe <- int_all_datastructure_dataframe(
#'   meta_data_dataframe = "meta_data_dataframe",
#'   meta_data = md0
#' )
#'
#' # This is the "normal" procedure for inside pipeline
#' # but outside this function  checktype is exact by default
#' options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "subset_u")
#' lapply(setNames(nm = prep_get_data_frame("meta_data_dataframe")$DF_NAME),
#'   int_sts_element_dataframe,
#'   meta_data = md0
#' )
#' md0[[VAR_NAMES]][[1]] <-
#'   "id" # is this missmatch reported -- is the data frame also reported,
#' # if nothing is wrong with it
#' lapply(setNames(nm = prep_get_data_frame("meta_data_dataframe")$DF_NAME),
#'   int_sts_element_dataframe,
#'   meta_data = md0
#' )
#' options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "exact")
#' }
#'
# nolint end
int_all_datastructure_dataframe <- function(meta_data_dataframe =
    "dataframe_level",
  item_level = "item_level",
  meta_data = item_level,
  meta_data_v2,
  dataframe_level) {
  util_maybe_load_meta_data_v2()

  util_ck_arg_aliases()

  meta_data_dataframe <- prep_check_meta_data_dataframe(meta_data_dataframe)
  util_expect_data_frame(meta_data)
  prep_check_meta_names(
    meta_data = meta_data,
    level = REQUIRED
  )

  study_data_list <- lapply(
    setNames(nm = meta_data_dataframe[[DF_NAME]]),
    function(dfn) {
      r <- NULL
      try(r <- prep_get_data_frame(dfn, keep_types = TRUE), silent = TRUE)
      if (!is.data.frame(r)) {
        util_warning(
          "Could not load/find data frame %s. Trying it without.",
          dQuote(dfn)
        )
      }
      r
    }
  )

  study_data_list <-
    study_data_list[!vapply(
      study_data_list, is.null,
      FUN.VALUE = logical(1)
    )]

  df_name_ok <- meta_data_dataframe[[DF_NAME]] %in% names(study_data_list)

  if (!all(df_name_ok)) {
    util_warning(
      "Losing %d data frame(s), because they could not be loaded",
      sum(!df_name_ok)
    )
  }

  meta_data_dataframe <- meta_data_dataframe[df_name_ok, , drop = FALSE]

  meta_data_element_count_1 <-
    meta_data_dataframe[
      !util_empty(meta_data_dataframe[[DF_ELEMENT_COUNT]]), ,
      drop = FALSE
    ]

  unexp_element_count_out <- util_int_datastructure_run_subcheck({
    unexp_element_count_out <- withr::with_options(
      list(
        dataquieR.testdebug = TRUE
      ),
      int_unexp_elements(
        identifier_name_list = meta_data_element_count_1[[DF_NAME]],
        data_element_count = meta_data_element_count_1[[DF_ELEMENT_COUNT]]
      )
    )
  }, "dataframe element count",
  util_int_datastructure_empty_dataframe_result(
    "NUM_int_sts_countel",
    "PCT_int_sts_countel"
  ))

  unexp_element_set_out <- util_int_datastructure_run_subcheck({
    meta_data_element_set <- meta_data
    meta_data_element_set[[STUDY_SEGMENT]] <- NULL
    meta_data_dataframe_element_set <- meta_data_dataframe
    if (!DATAFRAMES %in% colnames(meta_data_element_set) &&
        nrow(meta_data_dataframe_element_set) == 1) {
      unexp_element_set_out <-
        util_int_datastructure_df_element_set_single(
          meta_data = meta_data_element_set,
          meta_data_dataframe = meta_data_dataframe_element_set,
          study_data_list = study_data_list
        )
    } else {
      if (!DF_CODE %in% colnames(meta_data_dataframe_element_set)) {
        meta_data_dataframe_element_set[[DF_CODE]] <-
          meta_data_dataframe_element_set[[DF_NAME]]
      }
      unexp_element_set_out <- withr::with_options(list(
        dataquieR.testdebug = TRUE
      ), int_sts_element_dataframe(
        item_level = meta_data_element_set,
        meta_data_dataframe =
          meta_data_dataframe_element_set
      ))
    }

    unexp_element_set_out$DataframeTable$GRADING <-
      ifelse(
        unexp_element_set_out$DataframeTable$NUM_int_sts_element == 0,
        0,
        1
      )
    unexp_element_set_out
  }, "dataframe element set",
  list(
    DataframeTable = util_int_datastructure_empty_dataframe_element(),
    DataframeData = data.frame()
  ))

  meta_data_record_count_1 <-
    meta_data_dataframe[
      !util_empty(meta_data_dataframe[[DF_RECORD_COUNT]]), ,
      drop = FALSE
    ]

  unexp_records_out <- util_int_datastructure_run_subcheck({
    unexp_records_out <- withr::with_options(list(
      dataquieR.testdebug = TRUE
    ), int_unexp_records_dataframe(
      identifier_name_list = meta_data_record_count_1[[DF_NAME]],
      data_record_count = meta_data_record_count_1[[DF_RECORD_COUNT]]
    ))
  }, "dataframe record count",
  util_int_datastructure_empty_dataframe_result(
    "NUM_int_sts_countre",
    "PCT_int_sts_countre"
  ))

  unexp_records_id_out <- util_int_datastructure_run_subcheck({
    unexp_records_id_out <- util_int_datastructure_df_record_set(
      meta_data_dataframe,
      study_data_list
    )
  }, "dataframe record set",
  util_int_datastructure_empty_dataframe_result(
    "NUM_int_sts_setrc",
    "PCT_int_sts_setrc"
  ))


  duplicate_ids_out <- util_int_datastructure_run_subcheck({
    duplicate_ids_out <- util_int_datastructure_df_duplicate_ids(
      meta_data_dataframe,
      study_data_list
    )
  }, "dataframe duplicate IDs",
  util_int_datastructure_empty_dataframe_result(
    "NUM_int_sts_dupl_ids",
    "PCT_int_sts_dupl_ids"
  ))

  duplicates_rows_out <- util_int_datastructure_run_subcheck({
    meta_data_dup_rows <- util_int_datastructure_unique_rows_metadata(
      meta_data_dataframe,
      DF_UNIQUE_ROWS,
      filter_metadata = TRUE
    )
    df_names <- meta_data_dup_rows[[DF_NAME]]
    id_vars_list <- util_int_datastructure_id_vars(
      meta_data_dup_rows,
      DF_NAME,
      DF_ID_VARS
    )[df_names]
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
    duplicates_rows <- util_int_datastructure_duplicate_content_result(
      duplicate_data_frames,
      level_col = "Data frame",
      check = "Duplicates",
      num_col = "NUM_int_sts_dupl_content",
      pct_col = "PCT_int_sts_dupl_content",
      table_level = "Dataframe",
      table_level_col = DF_NAME,
      source_col = "Data frame"
    )
    if (length(duplicate_data_frames) > 0) {
      duplicates_rows_out <- list(
        DataframeData = duplicates_rows$data,
        DataframeTable = duplicates_rows$table
      )
    } else {
      util_int_datastructure_empty_dataframe_result(
        "NUM_int_sts_dupl_content",
        "PCT_int_sts_dupl_content"
      )
    }
  }, "dataframe duplicate rows",
  util_int_datastructure_empty_dataframe_result(
    "NUM_int_sts_dupl_content",
    "PCT_int_sts_dupl_content"
  ))

  result <- list(
    int_sts_countel = unexp_element_count_out$DataframeTable,
    int_sts_element = unexp_element_set_out$DataframeTable,
    int_sts_countre = unexp_records_out$DataframeTable,
    int_sts_setrc = unexp_records_id_out$DataframeTable,
    int_sts_dupl_ids = duplicate_ids_out$DataframeTable,
    int_sts_dupl_row = duplicates_rows_out$DataframeTable
  )

  unexp_element_set_out_data <- unexp_element_set_out$DataframeData

  cn <- colnames(unexp_element_set_out_data)
  if (length(cn) > 0) {
    colnames(unexp_element_set_out_data) <- util_translate_indicator_metrics(
      cn,
      short = FALSE,
      long = TRUE,
      ignore_unknown = TRUE
    )
  }

  result_data <- list(
    int_sts_countel = unexp_element_count_out$DataframeData,
    int_sts_element = unexp_element_set_out_data,
    int_sts_countre = unexp_records_out$DataframeData,
    int_sts_setrc = unexp_records_id_out$DataframeData,
    int_sts_dupl_ids = duplicate_ids_out$DataframeData,
    int_sts_dupl_row = duplicates_rows_out$DataframeData
  )

  for (n in names(result_data)) {
    rownames(result_data[[n]]) <- NULL
  }

  dqi <- util_get_concept_info("dqi")
  dqi <- dqi[!util_empty(dqi$abbreviation) & !util_empty(dqi$Name), , drop = FALSE] # nolint: line_length_linter.

  names(result_data) <-
    util_recode(
      names(result_data),
      dqi,
      "abbreviation",
      "Name",
      names(result_data)
    )
  if (is.data.frame(unexp_records_id_out$Other) &&
      nrow(unexp_records_id_out$Other) > 0) {
    result_data[["Unexpected data record set IDs"]] <-
      unexp_records_id_out$Other
  }

  DataframeTable <- util_merge_data_frame_list(result, "DF_NAME")
  cn <- colnames(DataframeTable)
  if (length(cn) > 0) {
    cn[startsWith(cn, "GRADING.")] <- gsub(
      "^GRADING\\.", "GRADING_",
      cn[startsWith(cn, "GRADING.")]
    )
    colnames(DataframeTable) <- cn
  }
  dataframe_data_1 <- util_make_data_slot_from_table_slot(DataframeTable)
  if ("resp_vars" %in% colnames(DataframeTable)) {
    dataframe_data_1$`Unexp. Variables` <- DataframeTable$resp_vars
  }

  DataframeData <- data.frame(Dataframe = DataframeTable[[DF_NAME]])
  DataframeData <- util_int_datastructure_add_summary_columns(
    DataframeData,
    dataframe_data_1,
    "Unexpected data element count"
  )

  if (!is.null(dataframe_data_1$`Unexp. Variables`)) {
    DataframeData$`Unexp. Variables` <- dataframe_data_1$`Unexp. Variables`
  }

  for (label in c(
    "Unexpected data element set",
    "Unexpected data record count",
    "Unexpected data record set",
    "Duplicates"
  )) {
    DataframeData <- util_int_datastructure_add_summary_columns(
      DataframeData,
      dataframe_data_1,
      label
    )
  }

  rm(dataframe_data_1)

  strings_col <- c(
    "Dataframe",
    "Unexpected data element count N (%)",
    "Unexpected data element set N (%)",
    "Unexpected data record count N (%)",
    "Unexpected data record set N (%)",
    "Duplicates N (%)",
    "Unexp. Variables"
  )
  strings_col <- intersect(strings_col, colnames(DataframeData))

  integers_col <- c(
    "Unexpected data element count (Grading)",
    "Unexpected data element set (Grading)",
    "Unexpected data record count (Grading)",
    "Unexpected data record set (Grading)",
    "Duplicates (Grading)"
  )
  integers_col <- intersect(integers_col, colnames(DataframeData))

  if (length(strings_col) > 0) {
    DataframeData[strings_col] <- lapply(
      DataframeData[strings_col],
      function(x) {
        attr(x, DATA_TYPE) <-
          DATA_TYPES$STRING
        x
      }
    )
  }
  if (length(integers_col) > 0) {
    DataframeData[integers_col] <- lapply(
      DataframeData[integers_col],
      function(x) {
        attr(x, DATA_TYPE) <-
          DATA_TYPES$INTEGER
        x
      }
    )
  }

  return(list(
    DataframeTable = DataframeTable,
    DataframeData = DataframeData,
    DataframeDataList = result_data
  ))
}
