test_that("util_seg_table counts codes for all expected observations", {
  skip_on_cran()

  ds2 <- data.frame(
    sex = c(1, 2, 1, NA),
    visit = c("yes", "no", "yes", "yes")
  )
  study_data <- data.frame(
    sex = ds2$sex,
    visit = ds2$visit
  )
  meta_data <- data.frame(
    VAR_NAMES = c("sex", "visit"),
    LABEL = c("Sex", "Visit"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$STRING),
    SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$NOMINAL)
  )

  tables <- util_seg_table(
    ds2,
    study_data,
    meta_data,
    expected_observations = "ALL"
  )

  expect_named(tables, c("sex", "visit"))
  expect_equal(
    tables$sex,
    data.frame(CODES = c("1", "2"), Freq = c(2L, 1L))
  )
  expect_equal(
    tables$visit,
    data.frame(CODES = c("no", "yes"), Freq = c(1L, 3L))
  )
})

test_that("util_seg_table filters to segment-expected observations", {
  skip_on_cran()

  ds2 <- data.frame(score = c("low", "high", "low", "high"))
  study_data <- data.frame(
    score = ds2$score,
    part_score = c(1, 0, 1, NA)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("part_score", "score"),
    LABEL = c("Score participation", "Score"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$STRING),
    SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$NOMINAL),
    PART_VAR = c("part_score", "part_score")
  )

  tables <- util_seg_table(
    ds2,
    study_data,
    meta_data,
    expected_observations = "SEGMENT"
  )

  expect_equal(
    tables$score,
    data.frame(CODES = "low", Freq = 2L)
  )
})
