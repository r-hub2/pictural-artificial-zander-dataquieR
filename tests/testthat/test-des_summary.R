test_that("test_create_descriptive_summ", {
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  target <- withr::local_tempdir("testdessummary")

  sd1 <- head(
    prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE), # nolint: line_length_linter.
    20
  )

  expect_message2(
    des_summary(
      study_data = sd1,
      meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx" # nolint: line_length_linter.
    )
  )

  sd1 <- sd1[, 4:7]
  desc1 <- des_summary(study_data = sd1)
  expect_equal(
    sum(as.numeric(desc1$SummaryData$Mean)),
    297.9
  )

  desc2 <- des_summary(
    resp_vars = c("v00003", "v00004"),
    study_data = sd1
  )
  expect_equal(
    sum(as.numeric(desc2$SummaryData$Mean)),
    171.4
  )
  expect_equal(
    sum(as.numeric(desc2$SummaryData$SD)),
    10.93
  )
  expect_equal(
    sum(as.numeric(desc2$SummaryData$CV)),
    13.27
  )
  expect_equal(
    sum(as.numeric(desc2$SummaryData$Kurtosis)),
    -2.5740
  )
  expect_equal(
    sum(as.numeric(desc2$SummaryData$Median)),
    171
  )
})

test_that("des_summary summarizes mixed local scale levels without fixtures", {
  skip_on_cran()
  withr::local_timezone("UTC")

  study_data <- data.frame(
    num = c(1, 2, 3, NA),
    ord = c(1L, 2L, 2L, 1L),
    cat = c("a", "b", "a", NA),
    tm = as.POSIXct(c(
      "2020-01-01 10:00:00",
      "2020-01-01 11:00:00",
      NA,
      "2020-01-01 12:00:00"
    ), tz = "UTC")
  )
  meta_data <- data.frame(
    VAR_NAMES = c("num", "ord", "cat", "tm"),
    DATA_TYPE = c(
      DATA_TYPES$FLOAT,
      DATA_TYPES$INTEGER,
      DATA_TYPES$STRING,
      DATA_TYPES$DATETIME
    ),
    SCALE_LEVEL = c(
      SCALE_LEVELS$RATIO,
      SCALE_LEVELS$ORDINAL,
      SCALE_LEVELS$NOMINAL,
      SCALE_LEVELS$INTERVAL
    ),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    VARIABLE_ROLE = "primary",
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(suppressWarnings(des_summary(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  )))
  summary_data <- as.data.frame(result$SummaryData)

  expect_type(result, "list")
  expect_s3_class(result$SummaryTable, "data.frame")
  expect_setequal(summary_data$Variables, c("num", "ord", "cat", "tm"))
  expect_equal(
    as.numeric(summary_data$Mean[summary_data$Variables == "num"]),
    2
  )
  expect_identical(
    summary_data$Median[summary_data$Variables == "ord"],
    "1"
  )
  expect_identical(
    summary_data$Mode[summary_data$Variables == "cat"],
    "a"
  )
  expect_identical(
    summary_data$Missing[summary_data$Variables == "tm"],
    "1 (25%)"
  )
})

test_that("des_summary leaves numeric descriptors empty for nominal data", {
  skip_on_cran()

  study_data <- data.frame(
    grp = c("a", "b", "a", NA_character_),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = "grp",
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    VARIABLE_ROLE = "primary",
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(suppressWarnings(des_summary(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  )))
  summary_data <- as.data.frame(result$SummaryData)

  expect_identical(as.character(summary_data$Variables), "grp")
  expect_identical(summary_data$Mode, "a")
  expect_identical(summary_data$Mean, "")
  expect_identical(summary_data$SD, "")
  expect_identical(summary_data$`IQR (Quartiles)`, "")
  expect_identical(summary_data$`Range (Min - Max)`, "")
  expect_identical(summary_data$Missing, "1 (25%)")
})

test_that("des_summary estimates metadata when none are supplied", {
  skip_on_cran()

  study_data <- data.frame(
    num = c(1L, 2L, NA_integer_, 4L),
    cat = c("x", "y", "x", NA_character_),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(suppressWarnings(des_summary(
    study_data = study_data
  )))
  summary_data <- as.data.frame(result$SummaryData)

  expect_type(result, "list")
  expect_s3_class(result$SummaryTable, "data.frame")
  expect_setequal(summary_data$Variables, c("num", "cat"))
  expect_identical(
    summary_data$Type[summary_data$Variables == "num"],
    "nominal, integer"
  )
  expect_identical(
    summary_data$Mode[summary_data$Variables == "cat"],
    "x"
  )
  expect_identical(
    summary_data$Missing[summary_data$Variables == "num"],
    "1 (25%)"
  )
  expect_match(
    summary_data$`Frequency table`[summary_data$Variables == "num"],
    "'4'",
    fixed = TRUE
  )
})

test_that("des_summary wrappers recover from prepared-metadata errors", {
  skip_on_cran()

  study_data <- data.frame(
    num = c(1, 2, 3),
    cat = c("a", "b", "a"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("num", "cat"),
    DATA_TYPE = c(DATA_TYPES$FLOAT, DATA_TYPES$STRING),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    VARIABLE_ROLE = "primary",
    stringsAsFactors = FALSE
  )

  assign("ds1", study_data, envir = .GlobalEnv)
  withr::defer(rm(ds1, envir = .GlobalEnv))
  prepare_calls <- new.env(parent = emptyenv())
  prepare_calls$n <- 0L
  fail_once_then_prepare <- function(...) {
    assign("n", get("n", prepare_calls) + 1L, prepare_calls)
    if (get("n", prepare_calls) == 1L) {
      stop("broken metadata")
    }
    invisible(NULL)
  }

  testthat::local_mocked_bindings(
    prep_study2meta = function(...) {
      meta_data
    },
    prep_prepare_dataframes = fail_once_then_prepare,
    des_summary = function(...) {
      list(
        SummaryData = data.frame(
          Variables = "x",
          stringsAsFactors = FALSE
        ),
        SummaryTable = data.frame(
          Variables = "x",
          stringsAsFactors = FALSE
        )
      )
    }
  )

  expect_warning(
    continuous <- suppressMessages(des_summary_continuous(
      resp_vars = "num",
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES
    )),
    "meta_data.*broken metadata"
  )
  expect_type(continuous, "list")
  expect_true(is.data.frame(continuous$SummaryData))
  expect_gt(get("n", prepare_calls), 1L)

  prepare_calls$n <- 0L
  expect_warning(
    categorical <- suppressMessages(des_summary_categorical(
      resp_vars = "cat",
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES
    )),
    "meta_data.*broken metadata"
  )
  expect_type(categorical, "list")
  expect_true(is.data.frame(categorical$SummaryData))
  expect_gt(get("n", prepare_calls), 1L)
})

test_that("des_summary orders metadata and summarizes local time-only data", {
  skip_on_cran()
  skip_if_not_installed("hms")

  study_data <- data.frame(
    num = c(2, 4, 6, 8),
    cat = c("x", "y", "x", NA_character_),
    stringsAsFactors = FALSE
  )
  study_data$clock <- hms::hms(hours = c(8, 9, 10, 11))
  meta_data <- data.frame(
    VAR_NAMES = c("num", "cat", "clock"),
    DATA_TYPE = c(
      DATA_TYPES$FLOAT,
      DATA_TYPES$STRING,
      DATA_TYPES$TIME
    ),
    SCALE_LEVEL = c(
      SCALE_LEVELS$RATIO,
      SCALE_LEVELS$NOMINAL,
      SCALE_LEVELS$INTERVAL
    ),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    VARIABLE_ROLE = "primary",
    VARIABLE_ORDER = c(3, 1, 2),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(suppressWarnings(des_summary(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  )))
  summary_data <- as.data.frame(result$SummaryData)

  expect_identical(
    as.character(summary_data$Variables),
    c("cat", "clock", "num")
  )
  expect_identical(
    summary_data$Mean[summary_data$Variables == "clock"],
    "34200 secs"
  )
  expect_identical(
    summary_data$Median[summary_data$Variables == "clock"],
    "34200 secs"
  )
  expect_identical(
    summary_data$SD[summary_data$Variables == "clock"],
    "1.29 hours"
  )
  expect_match(
    summary_data$`IQR (Quartiles)`[summary_data$Variables == "clock"],
    "Q1 = 08:45:00",
    fixed = TRUE
  )
  expect_identical(
    summary_data$`Range (Min - Max)`[summary_data$Variables == "clock"],
    "3 hours (08:00:00 - 11:00:00)"
  )
})

test_that("test_create_descriptive_summ_to", {
  skip_on_cran() # slow
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
    keep_types = TRUE
  )

  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx") # nolint: line_length_linter.
  meta_data <- prep_get_data_frame("item_level")

  set.seed(12345)
  day_course <- unlist(lapply(
    0:23,
    function(h) {
      list(
        hms::hms(
          hours = h,
          minutes = 3
        ),
        hms::hms(
          hours = h,
          minutes = 13
        ),
        hms::hms(
          hours = h,
          minutes = 24
        ),
        hms::hms(
          hours = h,
          minutes = 30
        ),
        hms::hms(
          hours = h,
          minutes = 50
        )
      )
    }
  ), recursive = FALSE)
  probs <-
    rep(c(.7, .2, .05, .04, 0.01), 24)
  times <- sample(
    x = day_course,
    prob = probs,
    size = nrow(study_data),
    replace = TRUE
  )
  study_data$v02000 <- times
  meta_data <- util_rbind(
    meta_data,
    data.frame(
      stringsAsFactors = FALSE,
      VAR_NAMES = "v02000",
      LABEL = "ADMIS_TM_0",
      DATA_TYPE = DATA_TYPES$TIME,
      SCALE_LEVEL = SCALE_LEVELS$INTERVAL,
      VALUE_LABELS = NA_character_,
      STANDARDIZED_VOCABULARY_TABLE = NA_character_,
      MISSING_LIST_TABLE = NA_character_,
      HARD_LIMITS = "[09:00:00;18:00:00]",
      DETECTION_LIMITS = NA_character_,
      SOFT_LIMITS = NA_character_,
      DISTRIBUTION = NA_character_,
      DECIMALS = NA_character_,
      DATA_ENTRY_TYPE = NA_character_,
      GROUP_VAR_OBSERVER = NA_character_,
      GROUP_VAR_DEVICE = NA_character_,
      TIME_VAR = NA_character_,
      STUDY_SEGMENT = "STUDY",
      PART_VAR = "PART_STUDY",
      VARIABLE_ROLE = "intro",
      VARIABLE_ORDER = "54",
      LONG_LABEL = "Admission time",
      ELEMENT_HOMOGENITY_CHECKTYPE = NA_character_,
      UNIVARIATE_OUTLIER_CHECKTYPE = NA_character_,
      N_RULES = "4",
      LOCATION_METRIC = NA_character_,
      LOCATION_RANGE = NA_character_,
      PROPORTION_RANGE = NA_character_,
      REPEATED_MEASURES_VARS = NA_character_,
      CO_VARS = NA_character_
    )
  )

  r <-
    des_summary(
      resp_vars = "ADMIS_TM_0",
      study_data = study_data,
      label_col = LABEL,
      meta_data = meta_data
    )
})
