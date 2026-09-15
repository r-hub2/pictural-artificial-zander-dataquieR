test_that("varcomp works", {
  skip_on_cran() # slow and covered by CI
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  meta_data <-
    prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data
    )

  # EATING_PREFS_0 is the nominal outcome in the metadata.
  res1 <- acc_varcomp(
    resp_vars = "EATING_PREFS_0",
    group_vars = "CENTER_0",
    study_data = study_data, meta_data = meta_data, label_col = LABEL
  )

  expect_false(inherits(res1, "try-error"))

  # ordinal outcome
  meta_data[[SCALE_LEVEL]][meta_data[[LABEL]] == "MEAT_CONS_0"] <- SCALE_LEVELS$ORDINAL # nolint: line_length_linter.
  res2 <- acc_varcomp(
    resp_vars = "MEAT_CONS_0",
    group_vars = "USR_SOCDEM_0",
    study_data = study_data, meta_data = meta_data, label_col = LABEL
  )

  expect_false(inherits(res2, "try-error"))

  skip_if_not_installed("ordinal")

  # N_INJURIES_0 is the integer outcome in the metadata.
  res3 <- acc_varcomp(
    resp_vars = "N_INJURIES_0",
    group_vars = "USR_SOCDEM_0",
    study_data = study_data, meta_data = meta_data, label_col = LABEL
  )

  expect_false(inherits(res3, "try-error"))

  # float
  res4 <- acc_varcomp(
    resp_vars = "CRP_0",
    group_vars = "CENTER_0",
    study_data = study_data, meta_data = meta_data, label_col = LABEL
  )

  expect_false(inherits(res4, "try-error"))
})
