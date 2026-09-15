# nolint start: line_length_linter.
#' Wrapper function to check for segment data structure
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
#' @param meta_data_segment [data.frame] the data frame that contains the metadata for the segment level, mandatory
#'
#' @return a [list] with
#'   - `SegmentTable`: data frame with selected check results, used for the data quality report.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' out_segment <- int_all_datastructure_segment(
#'   meta_data_segment = "meta_data_segment",
#'   study_data = "ship",
#'   meta_data = "ship_meta"
#' )
#'
#' study_data <- cars
#' meta_data <- dataquieR::prep_create_meta(
#'   VAR_NAMES = c("speedx", "distx"),
#'   DATA_TYPE = c("integer", "integer"), MISSING_LIST = "|", JUMP_LIST = "|",
#'   STUDY_SEGMENT = c("Intro", "Ex")
#' )
#'
#' out_segment <- int_all_datastructure_segment(
#'   meta_data_segment = "meta_data_segment",
#'   study_data = study_data,
#'   meta_data = meta_data
#' )
#' }
# nolint end
int_all_datastructure_segment <- function(study_data,
  label_col,
  item_level = "item_level",
  meta_data = item_level,
  meta_data_v2,
  segment_level,
  meta_data_segment = "segment_level") {
  util_maybe_load_meta_data_v2()
  util_ck_arg_aliases()

  prep_prepare_dataframes(.allow_empty = TRUE)
  if (missing(label_col) || is.null(label_col)) {
    label_col <- util_attr(ds1, "label_col", exact = TRUE)
  }
  if (is.null(label_col)) {
    label_col <- VAR_NAMES
  }
  if (!(STUDY_SEGMENT %in% colnames(meta_data))) {
    meta_data[[STUDY_SEGMENT]] <- "ALL"
  }
  meta_data_segment <- prep_check_meta_data_segment(meta_data_segment)
  id_vars_list <- util_int_datastructure_id_vars(
    meta_data_segment,
    STUDY_SEGMENT,
    SEGMENT_ID_VARS,
    map_meta_data = meta_data,
    label_col = label_col
  )

  meta_data_record_count_0 <-
    meta_data_segment[!util_empty(meta_data_segment[[SEGMENT_RECORD_COUNT]]), ,
      drop = FALSE
    ]

  unexp_records_out <- util_int_datastructure_run_subcheck({
    unexp_records_out <- withr::with_options(list(
      dataquieR.testdebug = TRUE
    ), int_unexp_records_segment(
      study_segment = meta_data_record_count_0[[STUDY_SEGMENT]],
      data_record_count = meta_data_record_count_0[[SEGMENT_RECORD_COUNT]],
      study_data = study_data, meta_data = meta_data, label_col = label_col
    ))
  }, "segment record count",
  util_int_datastructure_empty_segment_result(
    "NUM_int_sts_countre",
    "PCT_int_sts_countre"
  ))


  unexp_records_id_out <- util_int_datastructure_run_subcheck({
    meta_data_record_set <- meta_data_segment[
      !util_empty(meta_data_segment[[SEGMENT_RECORD_CHECK]]), ,
      drop = FALSE
    ]
    unexp_records_id_out <- util_int_unexp_records_set_segment(
      id_vars_list =
        id_vars_list[meta_data_record_set[[STUDY_SEGMENT]]],
      identifier_name_list = meta_data_record_set[[STUDY_SEGMENT]],
      valid_id_table_list = meta_data_record_set[[SEGMENT_ID_REF_TABLE]],
      meta_data_record_check_list =
        meta_data_record_set[[SEGMENT_RECORD_CHECK]],
      study_data = study_data,
      label_col = label_col,
      meta_data = meta_data
    )
  }, "segment record set",
  util_int_datastructure_empty_segment_result(
    "NUM_int_sts_setrc",
    "PCT_int_sts_setrc"
  ))


  meta_data_dup_ids <- meta_data_segment[
    !util_empty(meta_data_segment[[SEGMENT_ID_VARS]]), ,
    drop = FALSE
  ]
  duplicate_ids_out <- util_int_datastructure_run_subcheck({
    duplicate_ids_out <- withr::with_options(
      list(dataquieR.testdebug = TRUE),
      int_duplicate_ids(
        level = "segment",
        id_vars_list = id_vars_list[meta_data_dup_ids[[STUDY_SEGMENT]]],
        study_segment = meta_data_dup_ids[[STUDY_SEGMENT]],
        repetitions = meta_data_dup_ids[[SEGMENT_UNIQUE_ID]],
        study_data = study_data,
        meta_data = meta_data,
        label_col = label_col
      )
    )
  }, "segment duplicate IDs",
  util_int_datastructure_empty_segment_result(
    "NUM_int_sts_dupl_ids",
    "PCT_int_sts_dupl_ids"
  ))
  if (is.null(duplicate_ids_out)) {
    duplicate_ids_out <- list(
      SegmentData = setNames(list(), character()),
      SegmentTable = data.frame(),
      Other = list()
    )
  }


  meta_data_dup_rows <- util_int_datastructure_unique_rows_metadata(
    meta_data_segment,
    SEGMENT_UNIQUE_ROWS,
    filter_metadata = TRUE
  )
  duplicate_rows_out <- util_int_datastructure_run_subcheck({
    duplicate_rows_out <- withr::with_options(list(dataquieR.testdebug = TRUE), { # nolint: line_length_linter.
      segments <- intersect(
        meta_data_dup_rows[[STUDY_SEGMENT]],
        meta_data[[STUDY_SEGMENT]]
      )
      id_vars_list <- util_int_datastructure_id_vars(
        meta_data_dup_rows,
        STUDY_SEGMENT,
        SEGMENT_ID_VARS,
        map_meta_data = meta_data,
        label_col = VAR_NAMES
      )[segments]
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
            segment_vars <- setdiff(
              segment_vars,
              id_vars_list[[current_segment]]
            )
          }
          segment_data <- study_data[
            ,
            intersect(colnames(study_data), segment_vars),
            drop = FALSE
          ]
          util_remove_empty_rows(segment_data)
        }
      )
      duplicate_rows <- util_int_datastructure_duplicate_content_result(
        segment_data_list,
        level_col = "Segment",
        check = "Duplicate records",
        num_col = "NUM_int_sts_dupl_content",
        pct_col = "PCT_int_sts_dupl_content"
      )
      if (nrow(meta_data_dup_rows) > 0) {
        list(
          SegmentData = duplicate_rows$data,
          SegmentTable = duplicate_rows$table
        )
      } else {
        NULL
      }
    })
  }, "segment duplicate rows",
  util_int_datastructure_empty_segment_result(
    "NUM_int_sts_dupl_content",
    "PCT_int_sts_dupl_content"
  ))
  if (is.null(duplicate_rows_out) && nrow(meta_data_dup_rows) > 0) {
    duplicate_rows_out <- list(
      SegmentData = setNames(list(), character()),
      SegmentTable = data.frame(),
      Other = list()
    )
  }

  out_int_sts_element <- util_int_datastructure_run_subcheck({
    out_int_sts_element <-
      withr::with_options(list(
        dataquieR.testdebug = TRUE
      ), int_sts_element_segment(
        study_data = study_data,
        label_col = label_col,
        meta_data = meta_data
      ))

    out_int_sts_element$SegmentTable$GRADING <-
      ifelse(out_int_sts_element$SegmentTable$NUM_int_sts_element == 0, 0, 1)
    rownames(out_int_sts_element$SegmentTable) <- NULL
    out_int_sts_element
  }, "segment element set",
  list(
    SegmentTable = util_int_datastructure_empty_segment_element(),
    SegmentData = data.frame()
  ))

  result <- list(
    int_sts_countre = unexp_records_out$SegmentTable,
    int_sts_setrc = unexp_records_id_out$SegmentTable,
    int_sts_dupl_ids = duplicate_ids_out$SegmentTable,
    int_sts_dupl_content = duplicate_rows_out$SegmentTable,
    int_sts_element = out_int_sts_element$SegmentTable
  )

  result <- result[vapply(result,
    FUN.VALUE = logical(1),
    FUN = function(df) {
      !!prod(dim(df))
    }
  )]

  out_int_sts_element_data <- out_int_sts_element$SegmentData

  cn <- colnames(out_int_sts_element_data)
  if (length(cn) > 0) {
    colnames(out_int_sts_element_data) <- util_translate_indicator_metrics(
      cn,
      short = FALSE,
      long = TRUE,
      ignore_unknown = TRUE
    )
  }

  result_data <- list(
    int_sts_countre = unexp_records_out$SegmentData,
    int_sts_setrc = unexp_records_id_out$SegmentData,
    int_sts_dupl_ids = duplicate_ids_out$SegmentData,
    int_sts_dupl_content = duplicate_rows_out$SegmentData,
    int_sts_element = out_int_sts_element_data
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

  SegmentTable <- util_merge_data_frame_list(result, "Segment")
  cn <- colnames(SegmentTable)
  cn[startsWith(cn, "GRADING.")] <- gsub(
    "^GRADING\\.", "GRADING_",
    cn[startsWith(cn, "GRADING.")]
  )
  colnames(SegmentTable) <- cn

  SegmentData1 <- util_make_data_slot_from_table_slot(SegmentTable)
  if ("resp_vars" %in% colnames(SegmentTable)) {
    SegmentData1$`Unexp. Variables` <- SegmentTable$resp_vars
  }

  SegmentData <- data.frame(Segment = SegmentData1$Segment)
  for (label in c(
    "Unexpected data record count",
    "Unexpected data record set",
    "Duplicates"
  )) {
    SegmentData <- util_int_datastructure_add_summary_columns(
      SegmentData,
      SegmentData1,
      label
    )
  }

  if (!is.null(SegmentData1$`Unexp. Variables`)) {
    SegmentData$`Unexp. Variables` <- SegmentData1$`Unexp. Variables`
  }

  SegmentData <- util_int_datastructure_add_summary_columns(
    SegmentData,
    SegmentData1,
    "Unexpected data element set"
  )

  rm(SegmentData1)

  strings_col <- c(
    "Segment",
    "Unexpected data record count N (%)",
    "Unexpected data record set N (%)",
    "Duplicates N (%)",
    "Unexp. Variables",
    "Unexpected data element set N (%)"
  )
  strings_col <- intersect(strings_col, colnames(SegmentData))

  integers_col <- c(
    "Unexpected data record count (Grading)",
    "Unexpected data record set (Grading)",
    "Duplicates (Grading)",
    "Unexpected data element set (Grading)"
  )
  integers_col <- intersect(integers_col, colnames(SegmentData))

  if (length(strings_col) > 0) {
    SegmentData[strings_col] <- lapply(
      SegmentData[strings_col],
      function(x) {
        attr(x, DATA_TYPE) <- DATA_TYPES$STRING
        x
      }
    )
  }
  if (length(integers_col) > 0) {
    SegmentData[integers_col] <- lapply(
      SegmentData[integers_col],
      function(x) {
        attr(x, DATA_TYPE) <- DATA_TYPES$INTEGER
        x
      }
    )
  }

  return(list(
    SegmentTable = SegmentTable,
    SegmentData = SegmentData,
    SegmentDataList = result_data
  ))
}
