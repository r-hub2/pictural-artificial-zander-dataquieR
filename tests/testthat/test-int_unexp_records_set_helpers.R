test_that("unexpected dataframe record-set helper reports mismatching IDs", {
  skip_on_cran()
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  prep_add_data_frames(df_a = data.frame(
    id = c("A", "B", "X"),
    value = 1:3,
    stringsAsFactors = FALSE
  ))

  result <- util_int_unexp_records_set_dataframe(
    identifier_name_list = "df_a",
    id_vars_list = list(df_a = "id"),
    valid_id_table_list = list(df_a = data.frame(
      id = c("A", "B"),
      stringsAsFactors = FALSE
    )),
    meta_data_record_check_list = c(df_a = "exact")
  )

  expect_equal(result$DataframeTable[[DF_NAME]], "df_a")
  expect_equal(result$DataframeTable$NUM_int_sts_setrc, 1)
  expect_equal(result$DataframeTable$PCT_int_sts_setrc, 50)
  expect_equal(result$DataframeTable$GRADING, 1)
  expect_identical(result$Other$UnexpectedID, "X")
  expect_identical(result$Other$Line, "3")
})

test_that("unexpected record-set details ignore empty unexpected IDs", {
  skip_on_cran()
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  prep_add_data_frames(df_a = data.frame(
    id = c("A", "", "B", NA, "X"),
    value = 1:5,
    stringsAsFactors = FALSE
  ))

  result <- util_int_unexp_records_set_dataframe(
    identifier_name_list = "df_a",
    id_vars_list = list(df_a = "id"),
    valid_id_table_list = list(df_a = data.frame(
      id = c("A", "B"),
      stringsAsFactors = FALSE
    )),
    meta_data_record_check_list = c(df_a = "exact")
  )

  expect_identical(result$Other$UnexpectedID, "X")
  expect_identical(result$Other$Line, "5")
})

test_that(
  "unexpected dataframe record-set helper derives arguments from metadata",
  {
    skip_on_cran()
    prep_purge_data_frame_cache()
    withr::defer(prep_purge_data_frame_cache())

    prep_add_data_frames(
      df_a = data.frame(id = c("A", "B", "X"), value = 1:3),
      ref_ids = data.frame(id = c("A", "B")),
      append = FALSE
    )
    meta_data_dataframe <- data.frame(
      DF_NAME = "df_a",
      DF_ID_VARS = "id",
      DF_ID_REF_TABLE = "ref_ids",
      DF_RECORD_CHECK = "exact",
      stringsAsFactors = FALSE
    )

    result <- suppressMessages(util_int_unexp_records_set_dataframe(
      meta_data_dataframe = meta_data_dataframe
    ))

    expect_identical(result$DataframeTable[[DF_NAME]], "df_a")
    expect_identical(result$Other$UnexpectedID, "X")
  }
)

test_that("unexpected segment record-set helper reports mismatching IDs", {
  skip_on_cran()

  study_data <- data.frame(
    id = c("A", "B", "X"),
    value = 1:3,
    stringsAsFactors = FALSE
  )
  meta_data <- prep_create_meta(
    VAR_NAMES = c("id", "value"),
    DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$NOMINAL),
    MISSING_LIST = "|",
    JUMP_LIST = "|",
    STUDY_SEGMENT = c("SEG", "SEG")
  )

  result <- util_int_unexp_records_set_segment(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    identifier_name_list = "SEG",
    id_vars_list = list(SEG = "id"),
    valid_id_table_list = list(SEG = data.frame(
      ID = c("A", "B"),
      stringsAsFactors = FALSE
    )),
    meta_data_record_check_list = c(SEG = "exact")
  )

  expect_equal(result$SegmentTable$Segment, "SEG")
  expect_equal(result$SegmentTable$NUM_int_sts_setrc, 1)
  expect_equal(result$SegmentTable$PCT_int_sts_setrc, 20)
  expect_equal(result$SegmentTable$GRADING, 1)
  expect_identical(result$Other$UnexpectedID, "X")
  expect_identical(result$Other$Line, "3")
})
