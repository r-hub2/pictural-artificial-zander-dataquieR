test_that("des_summary_categorical keeps categorical summary columns", {
  skip_on_cran()

  study_data <- data.frame(
    cat = c("a", "b", "a", NA),
    num = c(1, 2, 3, 4)
  )
  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = names(study_data),
    DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(
      SCALE_LEVELS$NOMINAL,
      SCALE_LEVELS$RATIO
    ),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(des_summary_categorical(
    resp_vars = c("cat", "num"),
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  ))

  expect_true(all(c("SummaryData", "SummaryTable") %in% names(result)))
  expect_true(util_attr(result$SummaryData, "is_html_escaped", exact = TRUE))
  expect_true(util_attr(result$SummaryTable, "is_html_escaped", exact = TRUE))
  expect_true(all(result$SummaryData$Variables %in% "cat"))
  expect_false("Mean" %in% names(result$SummaryData))
  expect_true("Frequency table" %in% names(result$SummaryData))
})

test_that("des_summary_continuous keeps continuous summary columns", {
  skip_on_cran()

  study_data <- data.frame(
    cat = c("a", "b", "a", NA),
    int = c(1L, 2L, 3L, NA),
    ratio = c(1, 2, 4, 8)
  )
  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = names(study_data),
    DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$INTEGER, DATA_TYPES$FLOAT),
    SCALE_LEVEL = c(
      SCALE_LEVELS$NOMINAL,
      SCALE_LEVELS$INTERVAL,
      SCALE_LEVELS$RATIO
    ),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(des_summary_continuous(
    resp_vars = c("cat", "int", "ratio"),
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  ))

  expect_true(all(c("SummaryData", "SummaryTable") %in% names(result)))
  expect_true(util_attr(result$SummaryData, "is_html_escaped", exact = TRUE))
  expect_true(util_attr(result$SummaryTable, "is_html_escaped", exact = TRUE))
  expect_true(all(result$SummaryData$Variables %in% c("int", "ratio")))
  expect_true("Mean" %in% names(result$SummaryData))
  expect_false("Frequency table" %in% names(result$SummaryData))
})

test_that("des_summary wrappers infer response variables by scale level", {
  skip_on_cran()

  study_data <- data.frame(
    cat = c("a", "b", "a", NA),
    ord = ordered(c("low", "high", "low", "high")),
    ratio = c(1, 2, 4, 8),
    interval = c(10L, 11L, 12L, NA_integer_)
  )
  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = names(study_data),
    DATA_TYPE = c(
      DATA_TYPES$STRING,
      DATA_TYPES$STRING,
      DATA_TYPES$FLOAT,
      DATA_TYPES$INTEGER
    ),
    SCALE_LEVEL = c(
      SCALE_LEVELS$NOMINAL,
      SCALE_LEVELS$ORDINAL,
      SCALE_LEVELS$RATIO,
      SCALE_LEVELS$INTERVAL
    ),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  categorical <- suppressWarnings(des_summary_categorical(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  ))
  continuous <- suppressWarnings(des_summary_continuous(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  ))

  expect_setequal(categorical$SummaryData$Variables, c("cat", "ord"))
  expect_setequal(continuous$SummaryData$Variables, c("ratio", "interval"))
  expect_true("Frequency table" %in% names(categorical$SummaryData))
  expect_true("Mean" %in% names(continuous$SummaryData))
})

test_that("des_summary wrappers recover from unusable metadata", {
  skip_on_cran()

  categorical_data <- data.frame(
    cat = c("a", "b", "a"),
    stringsAsFactors = FALSE
  )
  categorical_meta <- data.frame(
    VAR_NAMES = "unknown",
    LABEL = "unknown",
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  expect_message(
    categorical <- suppressWarnings(des_summary_categorical(
      study_data = categorical_data,
      meta_data = categorical_meta,
      label_col = VAR_NAMES
    )),
    "amending guessed item-level metadata"
  )

  expect_identical(as.character(categorical$SummaryData$Variables), "cat")
  expect_equal(categorical$SummaryData$Mode, "a")

  continuous_data <- data.frame(
    ratio = c(1.5, 2.5, 4.5)
  )
  continuous_meta <- data.frame(
    VAR_NAMES = "unknown",
    LABEL = "unknown",
    DATA_TYPE = DATA_TYPES$FLOAT,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  expect_message(
    continuous <- suppressWarnings(des_summary_continuous(
      study_data = continuous_data,
      meta_data = continuous_meta,
      label_col = VAR_NAMES
    )),
    "amending guessed item-level metadata"
  )

  expect_identical(as.character(continuous$SummaryData$Variables), "ratio")
  expect_equal(continuous$SummaryData$Mean, "2.83")
})
