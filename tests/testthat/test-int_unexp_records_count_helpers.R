test_that("unexpected dataframe record-count helper reports count mismatches", {
  skip_on_cran()
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  prep_add_data_frames(
    df_a = data.frame(id = c("A", "B", "C"), stringsAsFactors = FALSE),
    df_b = data.frame(id = c("A", "B"), stringsAsFactors = FALSE)
  )

  result <- int_unexp_records_dataframe(
    identifier_name_list = c("df_a", "df_b"),
    data_record_count = c(2, 2)
  )

  expect_equal(as.vector(result$DataframeTable[[DF_NAME]]), c("df_a", "df_b"))
  expect_equal(as.vector(result$DataframeTable$NUM_int_sts_countre), c(1, 0))
  expect_equal(as.vector(result$DataframeTable$PCT_int_sts_countre), c(50, 0))
  expect_equal(as.vector(result$DataframeTable$GRADING), c(1, 0))
  expect_identical(
    util_attr(result$DataframeData$`Number of records in data`,
      DATA_TYPE, exact = TRUE),
    DATA_TYPES$INTEGER
  )
})

test_that("unexpected record-count helpers require complete mappings", {
  skip_on_cran()

  expect_error(
    int_unexp_records_dataframe(identifier_name_list = "df_a"),
    "I don't have"
  )
  expect_error(
    int_unexp_records_dataframe(
      identifier_name_list = c("df_a", "df_b"),
      data_record_count = 2
    ),
    "same length"
  )
  expect_error(
    int_unexp_records_segment(study_segment = "INTRO"),
    "I don't have"
  )
  expect_error(
    int_unexp_records_dataframe(
      identifier_name_list = "df_a",
      data_record_count = 1,
      meta_data_dataframe = data.frame(
        DF_NAME = "df_a",
        DF_RECORD_COUNT = 1
      )
    ),
    "This is not supported"
  )
  expect_error(
    int_unexp_records_segment(
      study_segment = "INTRO",
      data_record_count = 1,
      meta_data_segment = data.frame(
        STUDY_SEGMENT = "INTRO",
        SEGMENT_RECORD_COUNT = 1
      )
    ),
    "This is not supported"
  )
})

test_that("unexpected record-count helpers derive mappings from metadata", {
  skip_on_cran()
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  prep_add_data_frames(df_a = data.frame(id = 1:3))
  dataframe_result <- suppressMessages(int_unexp_records_dataframe(
    meta_data_dataframe = data.frame(
      DF_NAME = "df_a",
      DF_RECORD_COUNT = 2,
      stringsAsFactors = FALSE
    )
  ))
  expect_identical(
    as.vector(dataframe_result$DataframeTable$NUM_int_sts_countre),
    1
  )
  expect_identical(
    as.vector(dataframe_result$DataframeTable$PCT_int_sts_countre),
    50
  )

  study_data <- data.frame(
    intro = c(1L, NA_integer_, 3L),
    intro_2 = c(1L, NA_integer_, 3L)
  )
  meta_data <- prep_create_meta(
    VAR_NAMES = c("intro", "intro_2"),
    DATA_TYPE = rep(DATA_TYPES$INTEGER, 2),
    SCALE_LEVEL = rep(SCALE_LEVELS$NOMINAL, 2),
    MISSING_LIST = "|",
    JUMP_LIST = "|",
    STUDY_SEGMENT = "INTRO"
  )
  segment_result <- suppressMessages(int_unexp_records_segment(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    meta_data_segment = data.frame(
      STUDY_SEGMENT = "INTRO",
      SEGMENT_RECORD_COUNT = 1,
      stringsAsFactors = FALSE
    )
  ))
  expect_identical(
    as.vector(segment_result$SegmentTable$NUM_int_sts_countre),
    1
  )
  expect_identical(
    as.vector(segment_result$SegmentTable$PCT_int_sts_countre),
    100
  )
})

test_that("unexpected segment record-count helper reports non-empty rows", {
  skip_on_cran()

  study_data <- data.frame(
    intro = c(1L, NA_integer_, 3L),
    intro_2 = c(1L, NA_integer_, 3L),
    lab = c(10L, 11L, NA_integer_),
    lab_2 = c(10L, 11L, NA_integer_),
    stringsAsFactors = FALSE
  )
  meta_data <- prep_create_meta(
    VAR_NAMES = c("intro", "intro_2", "lab", "lab_2"),
    DATA_TYPE = rep(DATA_TYPES$INTEGER, 4),
    SCALE_LEVEL = rep(SCALE_LEVELS$NOMINAL, 4),
    MISSING_LIST = "|",
    JUMP_LIST = "|",
    STUDY_SEGMENT = c("INTRO", "INTRO", "LAB", "LAB")
  )

  result <- int_unexp_records_segment(
    study_segment = c("INTRO", "LAB"),
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    data_record_count = c(2, 3)
  )

  expect_equal(as.vector(result$SegmentTable$Segment), c("INTRO", "LAB"))
  expect_equal(as.vector(result$SegmentTable$NUM_int_sts_countre), c(0, 1))
  expect_equal(as.vector(result$SegmentTable$PCT_int_sts_countre), c(0, 33.333))
  expect_equal(as.vector(result$SegmentTable$GRADING), c(0, 1))

  expect_message(
    filtered <- int_unexp_records_segment(
      study_segment = c("INTRO", "UNKNOWN"),
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES,
      data_record_count = c(2, 99)
    ),
    "considering only the intersection"
  )
  expect_identical(as.vector(filtered$SegmentTable$Segment), "INTRO")
  expect_identical(as.vector(filtered$SegmentTable$GRADING), 0)
})
