test_that("util_ds1_eval_env works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  md <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  sd <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  md <- md[md$VAR_NAMES %in% colnames(sd), , FALSE]
  sd <- sd[, intersect(md$VAR_NAMES, colnames(sd)), FALSE]
  md$xx <- abbreviate(md$LABEL)
  e <- util_ds1_eval_env(sd, md,
    label_col = "xx"
  )
  expect_equal(
    sort(ls(e)),
    sort(unique(c(
      md$VAR_NAMES,
      md$LABEL,
      md$LONG_LABEL,
      md$xx
    )))
  )
})

test_that("util_ds1_eval_env exposes local label aliases", {
  skip_on_cran()

  study_data <- data.frame(
    blood_pressure = c(120, 130),
    sex = c(1, 2)
  )
  attr(study_data, "MAPPED") <- TRUE
  attr(study_data, "label_col") <- VAR_NAMES
  meta_data <- data.frame(
    VAR_NAMES = c("blood_pressure", "sex"),
    LABEL = c("Blood pressure", "Sex"),
    LONG_LABEL = c("Systolic blood pressure", "Biological sex"),
    SHORT = c("BP", "SEX"),
    stringsAsFactors = FALSE
  )

  env <- util_ds1_eval_env(study_data,
    meta_data = meta_data,
    label_col = "SHORT"
  )

  expect_equal(env$blood_pressure, study_data$blood_pressure)
  expect_equal(env$`Blood pressure`, study_data$blood_pressure)
  expect_equal(env$`Systolic blood pressure`, study_data$blood_pressure)
  expect_equal(env$BP, study_data$blood_pressure)
  expect_equal(env$SEX, study_data$sex)
})
