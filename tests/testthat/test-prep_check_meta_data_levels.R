test_that(
  "prep_check_meta_data_dataframe fills defaults and drops empty keys",
  {
    skip_on_cran()

    meta_data_dataframe <- data.frame(
      DF_NAME = c("study", ""),
      DF_CODE = c("study_code", "blank_code"),
      stringsAsFactors = FALSE
    )

    expect_message(
      checked <- prep_check_meta_data_dataframe(meta_data_dataframe),
      "Removing 1 rows"
    )

    expect_equal(checked[[DF_NAME]], "study")
    expect_true(all(c(
      DF_ELEMENT_COUNT,
      DF_RECORD_COUNT,
      DF_ID_REF_TABLE,
      DF_RECORD_CHECK,
      DF_ID_VARS,
      DF_UNIQUE_ID,
      DF_UNIQUE_ROWS,
      GRADING_RULESET
    ) %in% names(checked)))
    expect_true(is.na(checked[[DF_RECORD_COUNT]]))
    expect_true(is.na(checked[[DF_ID_REF_TABLE]]))
    expect_identical(checked[[GRADING_RULESET]], "0")
  }
)

test_that("prep_check_meta_data_dataframe rejects duplicated names and codes", {
  skip_on_cran()

  duplicated_names <- data.frame(
    DF_NAME = c("study", "study"),
    DF_CODE = c("a", "b"),
    stringsAsFactors = FALSE
  )
  expect_error(
    prep_check_meta_data_dataframe(duplicated_names),
    "Found duplicated dataframes"
  )

  duplicated_codes <- data.frame(
    DF_NAME = c("study_a", "study_b"),
    DF_CODE = c("same", "same"),
    stringsAsFactors = FALSE
  )
  expect_error(
    prep_check_meta_data_dataframe(duplicated_codes),
    "Found duplicated dataframe codes"
  )
})

test_that("prep_check_meta_data_segment normalizes legacy ID table columns", {
  skip_on_cran()

  meta_data_segment <- data.frame(
    STUDY_SEGMENT = c("baseline", ""),
    SEGMENT_ID_TABLE = c("baseline_ids", "drop_ids"),
    stringsAsFactors = FALSE
  )

  expect_message(
    expect_message(
      checked <- prep_check_meta_data_segment(meta_data_segment),
      "SEGMENT_ID_TABLE"
    ),
    "Removing 1 rows"
  )

  expect_equal(checked[[STUDY_SEGMENT]], "baseline")
  expect_false("SEGMENT_ID_TABLE" %in% names(checked))
  expect_equal(checked[[SEGMENT_ID_REF_TABLE]], "baseline_ids")
  expect_true(all(c(
    SEGMENT_RECORD_COUNT,
    SEGMENT_RECORD_CHECK,
    SEGMENT_ID_VARS,
    SEGMENT_PART_VARS,
    SEGMENT_UNIQUE_ROWS,
    SEGMENT_UNIQUE_ID,
    GRADING_RULESET
  ) %in% names(checked)))
  expect_equal(checked[[SEGMENT_UNIQUE_ID]], 1L)
  expect_identical(checked[[GRADING_RULESET]], "0")
})

test_that("entity-level grading rulesets are normalized and retained", {
  skip_on_cran()

  segment_level <- data.frame(
    STUDY_SEGMENT = c("baseline", "follow-up"),
    GRADING_RULESET = c(2, NA),
    stringsAsFactors = FALSE
  )
  checked_segments <- prep_check_meta_data_segment(segment_level)
  expect_identical(checked_segments[[GRADING_RULESET]], c("2", "0"))

  dataframe_level <- data.frame(
    DF_NAME = c("baseline.csv", "follow-up.csv"),
    GRADING_RULESET = c(" custom ", ""),
    stringsAsFactors = FALSE
  )
  checked_dataframes <- prep_check_meta_data_dataframe(dataframe_level)
  expect_identical(
    checked_dataframes[[GRADING_RULESET]],
    c("custom", "0")
  )
})
