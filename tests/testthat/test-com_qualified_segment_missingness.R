test_that("com_qualified_segment_missingness handles missing segment list", {
  skip_on_cran()

  study_data <- data.frame(part = c(1, 2, 3))
  meta_data <- data.frame(
    VAR_NAMES = "part",
    LABEL = "Participation",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    VARIABLE_ROLE = VARIABLE_ROLES$PRIMARY,
    MISSING_LIST = NA_character_,
    JUMP_LIST = NA_character_,
    MISSING_LIST_TABLE = NA_character_,
    HARD_LIMITS = NA_character_,
    stringsAsFactors = FALSE
  )
  meta_data_segment <- data.frame(
    STUDY_SEGMENT = "Segment A",
    SEGMENT_PART_VARS = "part",
    stringsAsFactors = FALSE
  )

  warning <- expect_warning(
    com_qualified_segment_missingness(
      study_data = study_data,
      meta_data = meta_data,
      meta_data_segment = meta_data_segment
    ),
    regexp = "No missing-match-table.+part.+Segment A"
  )

  expect_s3_class(warning, "dataquieR.applicability_problem")
  expect_match(
    conditionMessage(warning),
    "No missing-match-table.+part.+Segment A"
  )
})

test_that("com_qualified_segment_missingness computes local AAPOR rates", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  missing_table <- data.frame(
    CODE_VALUE = 1:5,
    CODE_LABEL = as.character(1:5),
    CODE_INTERPRET = c("I", "P", "PL", "R", "BO"),
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(part_missing = missing_table)
  study_data <- data.frame(part = c(1L, 1L, 2L, 3L, 4L, 5L))
  meta_data <- data.frame(
    VAR_NAMES = "part",
    LABEL = "Participation",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    VARIABLE_ROLE = VARIABLE_ROLES$PRIMARY,
    MISSING_LIST = NA_character_,
    JUMP_LIST = NA_character_,
    MISSING_LIST_TABLE = "part_missing",
    HARD_LIMITS = NA_character_,
    stringsAsFactors = FALSE
  )
  meta_data_segment <- data.frame(
    STUDY_SEGMENT = "Segment A",
    SEGMENT_PART_VARS = "part",
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(suppressWarnings(
    com_qualified_segment_missingness(
      study_data = study_data,
      meta_data = meta_data,
      meta_data_segment = meta_data_segment,
      expected_observations = "ALL"
    )
  ))

  expect_equal(as.numeric(result$SegmentTable$PCT_com_qum_nonresp), 50)
  expect_equal(as.numeric(result$SegmentTable$PCT_com_qum_refusal), 100 / 3)
  expect_equal(as.integer(result$SegmentTable$N), 6L)
  expect_equal(as.integer(result$SegmentTable$N2), 6L)
  expect_equal(
    as.character(
      result$SegmentData[["Non-response rate (Percentage (0 to 100))"]]
    ),
    "50%"
  )
  expect_equal(
    as.character(
      result$SegmentData[["Refusal rate (Percentage (0 to 100))"]]
    ),
    "33.33%"
  )
})

test_that("com_qualified_segment_missingness no meta", {
  skip_on_cran() # slow
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx") # nolint: line_length_linter.
  expect_warning(
    expect_warning(
      expect_warning(
        expect_warning(
          expect_warning(
            res <- com_qualified_segment_missingness(
              study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
              meta_data = "item_level",
              meta_data_segment = "segment_level"
            ),
            regexp = "Missing or doubled.+SEGMENT_PART.+segm.+for.+"
          ),
          regexp = "Missing or doubled.+SEGMENT_PART.+segm.+for.+"
        ),
        regexp = "Missing or doubled.+SEGMENT_PART.+segm.+for.+"
      ),
      regexp = "Missing or doubled.+SEGMENT_PART.+segm.+for.+"
    ),
    regexp = "Missing or doubled.+SEGMENT_PART.+segm.+for.+"
  )
  expect_type(res, "list")
  expect_s3_class(res$SegmentTable, "data.frame")
  expect_s3_class(res$SegmentData, "data.frame")
  expect_length(res$SegmentData, 0)
  expect_length(res$SegmentTable, 0)
})

test_that("com_qualified_segment_missingness with meta", {
  skip_on_cran() # slow
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship_meta_v2.xlsx") # nolint: line_length_linter.
  res <- com_qualified_segment_missingness(
    study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS", # nolint: line_length_linter.
    meta_data = "item_level",
    meta_data_segment = "segment_level"
  )
  expect_type(res, "list")
  expect_s3_class(res$SegmentTable, "data.frame")
  expect_s3_class(res$SegmentData, "data.frame")
  expect_equal(nrow(res$SegmentData), 4)
  expect_equal(nrow(res$SegmentTable), 4)
  expect_true(any(res$SegmentTable[, 2:ncol(res$SegmentTable)] > 0))
  expect_true(any(res$SegmentData[, 2:ncol(res$SegmentData)] > 0))
})
