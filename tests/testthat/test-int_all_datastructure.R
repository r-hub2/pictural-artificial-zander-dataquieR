test_that("dataframe datastructure checks report expected simple counts", {
  skip_on_cran()
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  df_a <- data.frame(
    id = c("A001", "A002", "A002", "A004", "A999"),
    age = c(21L, 22L, 22L, 24L, 25L),
    score = c(10, 11, 11, 12, 13),
    stringsAsFactors = FALSE
  )
  df_b <- data.frame(
    pid = c("B001", "B002", "B003", "B004"),
    visit = c(1L, 1L, 2L, 2L),
    value = c(5, 5, 7, 8),
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(df_a = df_a, df_b = df_b, append = FALSE)

  id_vars <- list(df_a = "id", df_b = "pid")
  reference_ids <- list(
    df_a = data.frame(
      id = c("A001", "A002", "A004"),
      stringsAsFactors = FALSE
    ),
    df_b = data.frame(
      pid = c("B001", "B002", "B003", "B004", "B005"),
      stringsAsFactors = FALSE
    )
  )
  meta_data <- prep_create_meta(
    VAR_NAMES = c("id", "age", "score", "pid", "visit", "value"),
    DATA_TYPE = c(
      "string", "integer", "float",
      "string", "integer", "float"
    ),
    MISSING_LIST = "|",
    JUMP_LIST = "|",
    DATAFRAMES = c("df_a", "df_a", "df_a", "df_b", "df_b", "df_b")
  )
  meta_data_dataframe <- data.frame(
    DF_NAME = c("df_a", "df_b"),
    DF_ELEMENT_COUNT = c(3, 3),
    DF_RECORD_COUNT = c(5, 4),
    DF_ID_VARS = c("id", "pid"),
    DF_RECORD_CHECK = c("exact", "subset"),
    DF_UNIQUE_ID = c(1, 1),
    DF_UNIQUE_ROWS = c("false", "no_id"),
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(ref_df_a = reference_ids$df_a,
    ref_df_b = reference_ids$df_b,
    append = TRUE)
  meta_data_dataframe[[DF_ID_REF_TABLE]] <- c("ref_df_a", "ref_df_b")

  duplicate_ids <- suppressWarnings(int_duplicate_ids(
    level = "dataframe",
    identifier_name_list = names(id_vars),
    id_vars_list = id_vars,
    repetitions = c(df_a = 1, df_b = 1)
  ))
  duplicate_content <- suppressWarnings(int_duplicate_content(
    level = "dataframe",
    identifier_name_list = names(id_vars),
    id_vars_list = id_vars,
    unique_rows = c(df_a = "false", df_b = "no_id")
  ))
  record_set <- int_unexp_records_set(
    level = "dataframe",
    identifier_name_list = names(id_vars),
    id_vars_list = id_vars,
    valid_id_table_list = reference_ids,
    meta_data_record_check_list = c(df_a = "exact", df_b = "subset")
  )
  all_checks <- int_all_datastructure_dataframe(
    meta_data_dataframe = meta_data_dataframe,
    meta_data = meta_data
  )

  expect_equal(duplicate_ids$DataframeTable$NUM_int_sts_dupl_ids, c(1, 0))
  expect_equal(duplicate_ids$DataframeTable$PCT_int_sts_dupl_ids, c(20, 0))
  expect_equal(duplicate_ids$DataframeTable$GRADING, c(1, 0))

  expect_equal(
    duplicate_content$DataframeTable$NUM_int_sts_dupl_content,
    c(1, 1)
  )
  expect_equal(
    duplicate_content$DataframeTable$PCT_int_sts_dupl_content,
    c(20, 25)
  )
  expect_equal(duplicate_content$DataframeTable$GRADING, c(1, 1))

  expect_equal(record_set$DataframeTable$NUM_int_sts_setrc, c(1, 0))
  expect_equal(record_set$DataframeTable$PCT_int_sts_setrc, c(33.333, 0))
  expect_equal(record_set$DataframeTable$GRADING, c(1, 0))
  expect_equal(record_set$Other$UnexpectedID, "A999")
  expect_true("Unexpected data record set IDs" %in%
      names(all_checks$DataframeDataList))
  expect_equal(
    all_checks$DataframeDataList$`Unexpected data record set IDs`$UnexpectedID,
    "A999"
  )

  expect_identical(
    attr(all_checks$DataframeData$Dataframe, DATA_TYPE, exact = TRUE),
    DATA_TYPES$STRING
  )
  expect_identical(
    attr(all_checks$DataframeData$`Unexpected data record count (Grading)`,
      DATA_TYPE,
      exact = TRUE
    ),
    DATA_TYPES$INTEGER
  )
  expect_identical(
    attr(all_checks$DataframeData$`Duplicates N (%)`,
      DATA_TYPE,
      exact = TRUE
    ),
    DATA_TYPES$STRING
  )
})

test_that("dataframe duplicate-id helper reports missing id columns", {
  skip_on_cran()

  meta_data_dataframe <- data.frame(
    DF_NAME = "df_a",
    DF_UNIQUE_ID = 1,
    DF_ID_VARS = "missing_id",
    stringsAsFactors = FALSE
  )
  study_data_list <- list(
    df_a = data.frame(value = 1:2)
  )
  captured <- new.env(parent = emptyenv())
  captured$messages <- character(0)

  withr::local_options(dataquieR.testthat_expect_message_active = TRUE)
  result <- withCallingHandlers(
    util_int_datastructure_df_duplicate_ids(
      meta_data_dataframe,
      study_data_list
    ),
    message = function(cond) {
      captured$messages <- c(captured$messages, conditionMessage(cond))
      invokeRestart("muffleMessage")
    }
  )

  expect_true(any(grepl("None of the ID variables", captured$messages,
        fixed = TRUE)))
  expect_equal(result$DataframeTable$DF_NAME, "df_a")
  expect_true(is.na(result$DataframeTable$NUM_int_sts_dupl_ids))
  expect_true(is.na(result$DataframeTable$PCT_int_sts_dupl_ids))
  expect_true(is.na(result$DataframeTable$GRADING))
  expect_null(result$Other$df_a)
})

test_that("dataframe wrapper preserves failed subcheck diagnostics", {
  skip_on_cran()
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  prep_add_data_frames(
    df_a = data.frame(id = c("A001", "A002"), value = 1:2),
    append = FALSE
  )
  meta_data <- prep_create_meta(
    VAR_NAMES = c("id", "value"),
    DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$INTEGER),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR
  )
  meta_data_dataframe <- data.frame(
    DF_NAME = "df_a",
    DF_ELEMENT_COUNT = NA_character_,
    DF_RECORD_COUNT = "2",
    DF_ID_VARS = "id",
    DF_RECORD_CHECK = "exact",
    DF_UNIQUE_ID = "1",
    DF_UNIQUE_ROWS = NA_character_,
    stringsAsFactors = FALSE
  )
  meta_data_dataframe[[DF_ID_REF_TABLE]] <- "not a data frame"

  warnings <- character(0)
  result <- withCallingHandlers(
    int_all_datastructure_dataframe(
      meta_data_dataframe = meta_data_dataframe,
      meta_data = meta_data
    ),
    warning = function(cond) {
      warnings <<- c(warnings, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(grepl("dataframe record set", warnings, fixed = TRUE)))
  expect_s3_class(result$DataframeTable, "data.frame")
  expect_s3_class(result$DataframeData, "data.frame")
})

test_that("dataframe wrapper preserves element-set details", {
  skip_on_cran()
  withr::local_options(
    viewer = function(...) NULL,
    dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "exact"
  )

  data_file <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(a = 1:2, b = 3:4, extra = 5:6),
    data_file,
    row.names = FALSE
  )
  ref_file <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(a = 1:2),
    ref_file,
    row.names = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("a", "b", "missing_md"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )
  meta_data_dataframe <- data.frame(
    DF_NAME = data_file,
    DF_CODE = "df_code",
    DF_ELEMENT_COUNT = "3",
    DF_RECORD_COUNT = "2",
    DF_ID_VARS = "a",
    DF_RECORD_CHECK = "exact",
    DF_UNIQUE_ID = "1",
    DF_UNIQUE_ROWS = NA_character_,
    stringsAsFactors = FALSE
  )
  meta_data_dataframe[[DF_ID_REF_TABLE]] <- ref_file

  result <- suppressMessages(suppressWarnings(
    int_all_datastructure_dataframe(
      meta_data_dataframe = meta_data_dataframe,
      meta_data = meta_data
    )
  ))

  element_details <-
    result$DataframeDataList$`Unexpected data element set`
  expect_match(element_details$`Affected Elements`, "missing_md.+extra")
  expect_equal(
    names(result),
    c("DataframeTable", "DataframeData", "DataframeDataList")
  )
})

test_that("segment datastructure checks report expected simple counts", {
  skip_on_cran()
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    person_id = c("P001", "P002", "P002", "P004", "P999"),
    intro_a = c(1L, 2L, 2L, 4L, 5L),
    lab_a = c(10, 11, 11, 13, 14),
    lab_b = c(20, 21, 21, 23, 24),
    stringsAsFactors = FALSE
  )
  meta_data <- prep_create_meta(
    VAR_NAMES = c("person_id", "intro_a", "lab_a", "lab_b"),
    DATA_TYPE = c("string", "integer", "float", "float"),
    MISSING_LIST = "|",
    JUMP_LIST = "|",
    STUDY_SEGMENT = c("INTRO", "INTRO", "LAB", "LAB")
  )
  reference_ids <- list(
    INTRO = data.frame(
      ID = c("P001", "P002", "P004"),
      stringsAsFactors = FALSE
    ),
    LAB = data.frame(
      ID = c("P001", "P002", "P003", "P004"),
      stringsAsFactors = FALSE
    )
  )
  meta_data_segment <- data.frame(
    STUDY_SEGMENT = c("INTRO", "LAB"),
    SEGMENT_ID_VARS = c("person_id", "person_id"),
    SEGMENT_RECORD_COUNT = c(5, 5),
    SEGMENT_RECORD_CHECK = c("exact", "subset"),
    SEGMENT_UNIQUE_ID = c(1, 1),
    SEGMENT_UNIQUE_ROWS = c("false", "no_id"),
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(ref_intro = reference_ids$INTRO,
    ref_lab = reference_ids$LAB,
    append = FALSE)
  meta_data_segment[[SEGMENT_ID_REF_TABLE]] <- c("ref_intro", "ref_lab")

  all_checks <- int_all_datastructure_segment(
    study_data = study_data,
    meta_data = meta_data,
    meta_data_segment = meta_data_segment,
    label_col = VAR_NAMES
  )
  record_set <- int_unexp_records_set(
    level = "segment",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    identifier_name_list = names(reference_ids),
    id_vars_list = list(INTRO = "person_id", LAB = "person_id"),
    valid_id_table_list = reference_ids,
    meta_data_record_check_list = c(INTRO = "exact", LAB = "subset")
  )
  duplicate_ids <- suppressWarnings(int_duplicate_ids(
    level = "segment",
    study_data = study_data,
    meta_data = meta_data,
    study_segment = names(reference_ids),
    id_vars_list = list(INTRO = "person_id", LAB = "person_id"),
    repetitions = c(INTRO = 1, LAB = 1)
  ))
  duplicate_content <- suppressWarnings(int_duplicate_content(
    level = "segment",
    study_data = study_data,
    meta_data = meta_data,
    identifier_name_list = names(reference_ids),
    id_vars_list = list(INTRO = "person_id", LAB = "person_id"),
    unique_rows = c(INTRO = "false", LAB = "no_id")
  ))

  expect_equal(all_checks$SegmentTable$NUM_int_sts_countre, c(0, 0))
  expect_equal(all_checks$SegmentTable$NUM_int_sts_dupl_ids, c(1, 1))
  expect_equal(all_checks$SegmentTable$PCT_int_sts_dupl_ids, c(20, 20))
  expect_equal(all_checks$SegmentTable$GRADING_int_sts_dupl_ids, c(1, 1))
  expect_equal(all_checks$SegmentTable$NUM_int_sts_dupl_content, c(NA, 1))
  expect_equal(all_checks$SegmentTable$PCT_int_sts_dupl_content, c(NA, 20))
  expect_equal(all_checks$SegmentTable$GRADING_int_sts_dupl_content, c(NA, 1))
  expect_equal(all_checks$SegmentTable$NUM_int_sts_element, c(0, 0))

  expect_equal(record_set$SegmentTable$NUM_int_sts_setrc, c(1, 1))
  expect_equal(record_set$SegmentTable$PCT_int_sts_setrc, c(12.5, 11.111))
  expect_equal(record_set$SegmentTable$GRADING, c(1, 1))
  expect_equal(record_set$Other$UnexpectedID, c("P999", "P999"))
  expect_equal(record_set$Other$Line, c("5", "5"))
  expect_true("Unexpected data record set IDs" %in%
      names(all_checks$SegmentDataList))
  expect_equal(
    all_checks$SegmentDataList$`Unexpected data record set IDs`$UnexpectedID,
    c("P999", "P999")
  )

  expect_equal(duplicate_ids$SegmentTable$NUM_int_sts_dupl_ids, c(1, 1))
  expect_equal(duplicate_ids$SegmentTable$PCT_int_sts_dupl_ids, c(20, 20))
  expect_equal(duplicate_ids$SegmentTable$GRADING, c(1, 1))

  expect_equal(
    duplicate_content$SegmentTable$NUM_int_sts_dupl_content,
    c(1, 1)
  )
  expect_equal(
    duplicate_content$SegmentTable$PCT_int_sts_dupl_content,
    c(20, 20)
  )
  expect_equal(duplicate_content$SegmentTable$GRADING, c(1, 1))
})

test_that("segment wrapper preserves failed subcheck diagnostics", {
  skip_on_cran()

  study_data <- data.frame(
    person_id = c("P001", "P002"),
    intro_a = c(1L, 2L),
    stringsAsFactors = FALSE
  )
  meta_data <- prep_create_meta(
    VAR_NAMES = c("person_id", "intro_a"),
    DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$INTEGER),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    STUDY_SEGMENT = c("INTRO", "INTRO")
  )
  meta_data_segment <- data.frame(
    STUDY_SEGMENT = "INTRO",
    SEGMENT_ID_VARS = "person_id",
    SEGMENT_RECORD_COUNT = NA_character_,
    SEGMENT_RECORD_CHECK = "exact",
    SEGMENT_UNIQUE_ID = NA_character_,
    SEGMENT_UNIQUE_ROWS = NA_character_,
    stringsAsFactors = FALSE
  )
  meta_data_segment[[SEGMENT_ID_REF_TABLE]] <- "not a data frame"

  warnings <- character(0)
  result <- withCallingHandlers(
    int_all_datastructure_segment(
      study_data = study_data,
      meta_data = meta_data,
      meta_data_segment = meta_data_segment,
      label_col = VAR_NAMES
    ),
    warning = function(cond) {
      warnings <<- c(warnings, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(grepl("segment record set", warnings, fixed = TRUE)))
  expect_s3_class(result$SegmentTable, "data.frame")
  expect_s3_class(result$SegmentData, "data.frame")
})
