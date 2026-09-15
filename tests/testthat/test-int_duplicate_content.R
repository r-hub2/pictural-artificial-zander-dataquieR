skip_on_cran()

test_that("duplicate content dataframe checks can ignore ID variables", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  prep_add_data_frames(df_a = data.frame(
    id = c(1, 2, 3),
    x = c("a", "a", "a"),
    y = c(1, 1, 1),
    stringsAsFactors = FALSE
  ))

  with_id <- util_int_duplicate_content_dataframe(
    identifier_name_list = "df_a",
    id_vars_list = list(df_a = "id"),
    unique_rows = c(df_a = "true")
  )
  without_id <- util_int_duplicate_content_dataframe(
    identifier_name_list = "df_a",
    id_vars_list = list(df_a = "id"),
    unique_rows = c(df_a = "no_id")
  )

  expect_equal(with_id$DataframeTable$NUM_int_sts_dupl_content, 0)
  expect_equal(with_id$DataframeTable$GRADING, 0)
  expect_equal(without_id$DataframeTable$NUM_int_sts_dupl_content, 2)
  expect_equal(without_id$DataframeTable$PCT_int_sts_dupl_content, 66.667)
  expect_equal(without_id$DataframeTable$GRADING, 1)
})

test_that("duplicate content dataframe checks use dataframe metadata", {
  skip_on_cran()
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  prep_add_data_frames(
    df_full = data.frame(
      id = c(1, 2, 3),
      x = c("a", "a", "a"),
      stringsAsFactors = FALSE
    ),
    df_skip = data.frame(
      id = c(1, 1),
      x = c("b", "b"),
      stringsAsFactors = FALSE
    ),
    df_no_id = data.frame(
      id = c(1, 2, 3),
      x = c("c", "c", "c"),
      stringsAsFactors = FALSE
    )
  )

  meta_data_dataframe <- data.frame(
    DF_NAME = c("df_full", "df_skip", "df_no_id"),
    DF_ID_VARS = c("id", "id", "id"),
    DF_UNIQUE_ROWS = c("true", "false", "no_id"),
    stringsAsFactors = FALSE
  )

  result <- util_int_duplicate_content_dataframe(
    meta_data_dataframe = meta_data_dataframe
  )

  expect_identical(result$DataframeTable[[DF_NAME]], c("df_full", "df_no_id"))
  expect_equal(result$DataframeTable$NUM_int_sts_dupl_content, c(0, 2))
  expect_equal(result$DataframeTable$PCT_int_sts_dupl_content, c(0, 66.667))
})

test_that("duplicate content segment checks can ignore segment ID variables", {
  study_data <- data.frame(
    id = c(1, 2, 3),
    x = c("a", "a", "a"),
    y = c(1, 1, 1),
    z = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("id", "x", "y", "z"),
    LABEL = c("id", "x", "y", "z"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$STRING,
      DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    STUDY_SEGMENT = c("s1", "s1", "s1", "s2"),
    MISSING_LIST = "999 = missing",
    JUMP_LIST = "888 = jump",
    stringsAsFactors = FALSE
  )

  with_id <- util_int_duplicate_content_segment(
    identifier_name_list = "s1",
    id_vars_list = list(s1 = "id"),
    unique_rows = c(s1 = "true"),
    study_data = study_data,
    meta_data = meta_data
  )
  without_id <- util_int_duplicate_content_segment(
    identifier_name_list = "s1",
    id_vars_list = list(s1 = "id"),
    unique_rows = c(s1 = "no_id"),
    study_data = study_data,
    meta_data = meta_data
  )

  expect_equal(with_id$SegmentTable$NUM_int_sts_dupl_content, 0)
  expect_equal(with_id$SegmentTable$GRADING, 0)
  expect_equal(without_id$SegmentTable$NUM_int_sts_dupl_content, 2)
  expect_equal(without_id$SegmentTable$PCT_int_sts_dupl_content, 66.667)
  expect_equal(without_id$SegmentTable$GRADING, 1)
})

test_that("duplicate content segment checks use segment metadata", {
  skip_on_cran()

  study_data <- data.frame(
    id = c(1, 2, 3),
    full = c("a", "a", "b"),
    skip = c("s", "s", "s"),
    no_id = c("n", "n", "n"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("id", "full", "skip", "no_id"),
    LABEL = c("id", "full", "skip", "no_id"),
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    STUDY_SEGMENT = c("FULL", "FULL", "SKIP", "NO_ID"),
    MISSING_LIST = "|",
    JUMP_LIST = "|",
    stringsAsFactors = FALSE
  )
  meta_data_segment <- data.frame(
    STUDY_SEGMENT = c("FULL", "SKIP", "NO_ID"),
    SEGMENT_ID_VARS = c("id", "id", "id"),
    SEGMENT_UNIQUE_ROWS = c("true", "false", "no_id"),
    stringsAsFactors = FALSE
  )

  result <- util_int_duplicate_content_segment(
    study_data = study_data,
    meta_data = meta_data,
    meta_data_segment = meta_data_segment
  )

  expect_identical(result$SegmentTable$Segment, c("FULL", "NO_ID"))
  expect_equal(result$SegmentTable$NUM_int_sts_dupl_content, c(0, 2))
  expect_equal(result$SegmentTable$PCT_int_sts_dupl_content, c(0, 66.667))
})
