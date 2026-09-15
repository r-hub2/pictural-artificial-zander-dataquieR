test_that("acc_mahalanobis_ratio summarizes a precomputed ratio variable", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    ID = seq_len(5),
    Q1 = seq_len(5),
    Q2 = 2:6,
    Q3 = 3:7,
    MD_RATIO = c(0.1, 1.2, NA, 0.8, 1.5)
  )
  item_level <- data.frame(
    VAR_NAMES = c("ID", paste0("Q", 1:3), "MD_RATIO"),
    LABEL = c("ID", paste("Question", 1:3), "MD ratio"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, rep(DATA_TYPES$INTEGER, 3),
      DATA_TYPES$FLOAT),
    SCALE_LEVEL = c(SCALE_LEVELS$ORDINAL, rep(SCALE_LEVELS$ORDINAL, 3),
      SCALE_LEVELS$RATIO),
    VARIABLE_ROLE = c(VARIABLE_ROLES$INTRO, rep(VARIABLE_ROLES$PRIMARY, 4)),
    COMPUTED_VARIABLE_ROLE = c(NA, rep(NA, 3),
      COMPUTED_VARIABLE_ROLES$MAHALANOBIS_RATIO),
    CHECK_ID = c(NA, rep(NA, 3), "mh1"),
    stringsAsFactors = FALSE
  )
  cross_item_level <- data.frame(
    VARIABLE_LIST = "Q1 | Q2 | Q3",
    CHECK_ID = "mh1",
    CHECK_LABEL = "Mahalanobis",
    CONTRADICTION_TERM = NA_character_,
    CONTRADICTION_TYPE = NA_character_,
    MULTIVARIATE_OUTLIER_CHECKTYPE = NA_character_,
    N_RULES = NA_integer_,
    MAHALANOBIS_THRESHOLD = "true",
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(item_level, cross_item_level)

  result <- suppressWarnings(suppressMessages(acc_mahalanobis_ratio(
    resp_vars = "MD_RATIO",
    study_data = study_data,
    meta_data = "item_level",
    meta_data_cross_item = "cross_item_level"
  )))

  expect_equal(result$SummaryTable$NUM_ssc_mah, 2)
  expect_equal(result$SummaryTable$PCT_ssc_mah, 50)
  expect_equal(as.numeric(result$SummaryData$N), 4)
  expect_equal(as.numeric(result$SummaryData$observational_units_removed), 1)
  expect_equal(as.numeric(result$SummaryData$mahalanobis_threshold),
    dataquieR.MAHALANOBIS_THRESHOLD_default)
  expect_equal(util_attr(result$SummaryData$N, DATA_TYPE, exact = TRUE),
    DATA_TYPES$INTEGER)
  expect_equal(
    util_attr(result$SummaryData$mahalanobis_threshold, DATA_TYPE,
      exact = TRUE),
    DATA_TYPES$FLOAT
  )
  expect_equal(as.vector(result$FlaggedStudyData$MD_outliers),
    c(0, 1, NA, 0, 1))
})

test_that("acc_mahalanobis_ratio accepts an explicit numeric threshold", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    Q1 = c(1, 2, 3),
    Q2 = c(2, 3, 4),
    MD_RATIO = c(0.2, 1.1, 1.3)
  )
  item_level <- data.frame(
    VAR_NAMES = c("Q1", "Q2", "MD_RATIO"),
    LABEL = c("Question 1", "Question 2", "MD ratio"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER, DATA_TYPES$FLOAT),
    SCALE_LEVEL = c(SCALE_LEVELS$ORDINAL, SCALE_LEVELS$ORDINAL,
      SCALE_LEVELS$RATIO),
    VARIABLE_ROLE = VARIABLE_ROLES$PRIMARY,
    COMPUTED_VARIABLE_ROLE = c(NA, NA,
      COMPUTED_VARIABLE_ROLES$MAHALANOBIS_RATIO),
    CHECK_ID = c(NA, NA, "mh_numeric"),
    stringsAsFactors = FALSE
  )
  cross_item_level <- data.frame(
    VARIABLE_LIST = "Q1 | Q2",
    CHECK_ID = "mh_numeric",
    CHECK_LABEL = "Mahalanobis",
    CONTRADICTION_TERM = NA_character_,
    CONTRADICTION_TYPE = NA_character_,
    MULTIVARIATE_OUTLIER_CHECKTYPE = NA_character_,
    N_RULES = NA_integer_,
    MAHALANOBIS_THRESHOLD = "0.9",
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(item_level, cross_item_level)

  result <- suppressWarnings(suppressMessages(acc_mahalanobis_ratio(
    resp_vars = "MD_RATIO",
    study_data = study_data,
    meta_data = "item_level",
    meta_data_cross_item = "cross_item_level"
  )))

  expect_identical(
    as.numeric(result$SummaryData$mahalanobis_threshold),
    0.9
  )
  expect_identical(
    as.integer(result$SummaryData$observational_units_removed),
    0L
  )
  expect_identical(result$SummaryTable$NUM_ssc_mah, 2L)
})
