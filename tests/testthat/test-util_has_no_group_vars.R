test_that("util_has_no_group_vars works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")

  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx") # nolint: line_length_linter.

  expect_equal(util_has_no_group_vars("SBP_0"), FALSE)
  expect_equal(util_has_no_group_vars("DBP_0"), FALSE)
  expect_error(util_has_no_group_vars(NA))
  expect_equal(util_has_no_group_vars("SEX_0"), TRUE)
})

test_that("util_has_no_group_vars uses local group metadata", {
  skip_on_cran()
  meta_data <- data.frame(
    VAR_NAMES = c("blood_pressure", "sex", "age"),
    LABEL = c("Blood pressure", "Sex", "Age"),
    GROUP_VAR_OBSERVER = c("observer", "", NA_character_),
    GROUP_VAR_DEVICE = c("", NA_character_, ""),
    stringsAsFactors = FALSE
  )

  expect_false(util_has_no_group_vars(
    "blood_pressure",
    meta_data = meta_data
  ))
  expect_true(util_has_no_group_vars("sex", meta_data = meta_data))
  expect_true(
    util_has_no_group_vars(
      "Age",
      meta_data = meta_data,
      label_col = LABEL
    )
  )
})
