test_that("acc_cat_distributions works", {
  skip_on_cran() # slow, errors obvious
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx") # nolint: line_length_linter.
  r <- acc_cat_distributions(
    study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
    resp_vars = "EDUCATION_1",
    group_vars = "USR_SOCDEM_0",
    label_col = LABEL
  )
  skip_if_not_installed("vdiffr")
  skip_on_cran()
  expect_doppelganger2(
    "acc_cat_distributions EDUCATION_1",
    r$SummaryPlot
  )
})
