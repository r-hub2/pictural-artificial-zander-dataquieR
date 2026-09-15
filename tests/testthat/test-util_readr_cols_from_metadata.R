skip_on_cran()

test_that("util_adjust_data_type2==util_adjust_data_type2", {
  skip_on_cran() # depends on locales

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  # Use testthat::local_reproducible_output() when debugging locally.
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  require_english_locale_and_berlin_tz()
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
    keep_types = TRUE
  )
  meta_data <-
    prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data
    )

  ds1_old <- withr::with_options(
    list(dataquieR.old_type_adjust = "TRUE"),
    prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data)
  )
  ds1_new <- withr::with_options(
    list(dataquieR.old_type_adjust = "FALSE"),
    prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data)
  )

  attr(ds1_old, "dataquieR_preparation_signature") <- NULL
  attr(ds1_new, "dataquieR_preparation_signature") <- NULL
  expect_equal(ds1_new, ds1_old)
})

test_that("util_adjust_data_type2==util_adjust_data_type2_FCT", {
  skip_on_cran() # depends on locales

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  # Use testthat::local_reproducible_output() when debugging locally.
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  require_english_locale_and_berlin_tz()
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
    keep_types = TRUE
  )
  meta_data <-
    prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data
    )
  study_data$v00014 <- as.factor(study_data$v00014)

  ds1_old <- withr::with_options(
    list(dataquieR.old_type_adjust = "TRUE"),
    prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data)
  )
  ds1_new <- withr::with_options(
    list(dataquieR.old_type_adjust = "FALSE"),
    prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data)
  )

  attr(ds1_old, "dataquieR_preparation_signature") <- NULL
  attr(ds1_new, "dataquieR_preparation_signature") <- NULL
  expect_equal(ds1_new, ds1_old)
})

test_that("util_adjust_data_type2 reports introduced missings per value", {
  study_data <- data.frame(
    id = 1:6,
    x = c("1", "bad_a", "3", "bad_b", "5", "bad_c"),
    y = c("1.5", "2.5", "bad_y", "4.5", "5.5", "6.5"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("id", "x", "y"),
    DATA_TYPE = c("integer", "integer", "float"),
    stringsAsFactors = FALSE
  )

  collect_type_adjust_messages <- function(type_adjust_parallel) {
    messages <- new.env(parent = emptyenv())
    messages$values <- character(0)
    withr::local_options(
      dataquieR.type_adjust_parallel = type_adjust_parallel
    )
    withCallingHandlers(
      util_adjust_data_type2(study_data, meta_data),
      message = function(m) {
        msg <- conditionMessage(m)
        if (grepl("Data type transformation", msg, fixed = TRUE)) {
          messages$values <- c(messages$values, msg)
        }
        invokeRestart("muffleMessage")
      }
    )
    messages$values
  }

  has_count <- function(messages, variable, count) {
    any(
      grepl(variable, messages, fixed = TRUE) &
        grepl(sprintf("introduced %d additional", count),
          messages,
          fixed = TRUE
        )
    )
  }

  parallel_messages <- collect_type_adjust_messages(TRUE)
  serial_messages <- collect_type_adjust_messages(FALSE)

  expect_true(has_count(parallel_messages, "x", 3))
  expect_true(has_count(parallel_messages, "y", 1))
  expect_true(has_count(serial_messages, "x", 3))
  expect_true(has_count(serial_messages, "y", 1))
})

test_that("util_readr_cols_from_metadata maps known data types", {
  meta_data <- data.frame(
    VAR_NAMES = c("int", "str", "flt", "dtm", "tim"),
    DATA_TYPE = c(
      DATA_TYPES$INTEGER,
      DATA_TYPES$STRING,
      DATA_TYPES$FLOAT,
      DATA_TYPES$DATETIME,
      DATA_TYPES$TIME
    )
  )

  col_types <- util_readr_cols_from_metadata(meta_data)

  expect_named(col_types, meta_data$VAR_NAMES)
  expect_s3_class(col_types$int, "collector_double")
  expect_s3_class(col_types$str, "collector_character")
  expect_s3_class(col_types$flt, "collector_double")
  expect_s3_class(col_types$dtm, "collector_datetime")
  expect_s3_class(col_types$tim, "collector_time")
})

test_that("util_readr_cols_from_metadata falls back for unknown data types", {
  meta_data <- data.frame(
    VAR_NAMES = c("known", "unknown"),
    DATA_TYPE = c(DATA_TYPES$STRING, "not_a_dataquieR_type")
  )

  expect_warning(
    col_types <- util_readr_cols_from_metadata(meta_data),
    "falling back"
  )

  expect_s3_class(col_types$known, "collector_character")
  expect_s3_class(col_types$unknown, "collector_character")
})

test_that("util_adjust_data_type2 aggregates introduced-missing warnings", {
  study_data <- data.frame(
    x = rep("not an integer", 100),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = "x",
    DATA_TYPE = "integer",
    stringsAsFactors = FALSE
  )

  messages <- character(0)
  withCallingHandlers(
    invisible(util_adjust_data_type2(study_data, meta_data)),
    message = function(m) {
      msg <- conditionMessage(m)
      if (grepl("Data type transformation", msg, fixed = TRUE)) {
        messages <<- c(messages, msg)
      }
      invokeRestart("muffleMessage")
    }
  )

  expect_length(messages, 1)
  expect_true(grepl("introduced 100 additional", messages, fixed = TRUE))
  expect_equal(
    util_type_adjust_warning_counts(vars = rep("x", 100)),
    c(x = 100L)
  )
  expect_equal(
    util_type_adjust_warning_counts(
      counts = util_type_adjust_warning_counts(vars = "x"),
      vars = "x"
    ),
    c(x = 2L)
  )
  expect_equal(
    util_type_adjust_warning_counts(counts = c(x = 2L), vars = NA_character_),
    c(x = 2L)
  )
})

test_that("util_adjust_integer_column preserves integer adjustment behavior", {
  expect_identical(util_adjust_integer_column(1:3), c(1, 2, 3))
  expect_identical(
    util_adjust_integer_column(c(1.2, 2.8, NA_real_)),
    c(1, 2, NA_real_)
  )
  expect_identical(util_adjust_float_column(1:3), c(1, 2, 3))
  expect_identical(
    util_adjust_float_column(c(1.2, 2.8, NA_real_)),
    c(1.2, 2.8, NA_real_)
  )
})

test_that(
  "util_adjust_data_type2 skips full conversion for numeric matching types",
  {
    study_data <- data.frame(
      id = 1:3,
      score = c(1.5, 2.5, NA_real_),
      integer_score = 1:3,
      label = c("a", "b", NA_character_),
      stringsAsFactors = FALSE
    )
    meta_data <- data.frame(
      VAR_NAMES = c("id", "score", "integer_score", "label"),
      DATA_TYPE = c(" INTEGER ", " FLOAT ", "float", "string"),
      stringsAsFactors = FALSE
    )

    skip_cols <- util_skip_readr_type_convert_cols(study_data, meta_data)

    expect_true(skip_cols[["id"]])
    expect_true(skip_cols[["score"]])
    expect_true(skip_cols[["integer_score"]])
    expect_false(skip_cols[["label"]])
    expect_false(util_can_skip_readr_type_convert(study_data, meta_data))

    adjusted <- util_adjust_data_type2(study_data, meta_data)

    expect_true(isTRUE(attr(adjusted, "Data_type_matches", exact = TRUE)))
    expect_type(adjusted$id, "double")
    expect_type(adjusted$integer_score, "double")
    expect_equal(adjusted$id, c(1, 2, 3))
    expect_equal(adjusted$score, study_data$score)
    expect_equal(adjusted$integer_score, c(1, 2, 3))
    expect_equal(adjusted$label, study_data$label)
  }
)

test_that("util_adjust_data_type2 uses the pipeline parallel conversion path", {
  skip_on_cran()

  study_data <- data.frame(
    x = c("1", "bad", "3"),
    y = c("4.5", "5.5", "bad"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("x", "y"),
    DATA_TYPE = c("integer", "float"),
    stringsAsFactors = FALSE
  )

  old_called_in_pipeline <- .dq2_globs$.called_in_pipeline
  .dq2_globs$.called_in_pipeline <- TRUE
  on.exit(.dq2_globs$.called_in_pipeline <- old_called_in_pipeline, add = TRUE)

  cores <- 2L
  calls <- new.env(parent = emptyenv())
  calls$started <- FALSE
  calls$cluster_calls <- 0L

  testthat::local_mocked_bindings(
    util_par_lapply_lb = function(X, fun, ..., cl = NULL) {
      expect_identical(cl, "mock-cluster")
      lapply(X, fun, ...)
    },
    .package = "dataquieR"
  )
  testthat::local_mocked_bindings(
    makePSOCKcluster = function(cpus) {
      expect_identical(cpus, cores)
      calls$started <- TRUE
      "mock-cluster"
    },
    clusterCall = function(cl, fun, ...) {
      expect_identical(cl, "mock-cluster")
      calls$cluster_calls <- calls$cluster_calls + 1L
      list(invisible(NULL))
    },
    stopCluster = function(cl) {
      expect_identical(cl, "mock-cluster")
      invisible(NULL)
    },
    .package = "parallel"
  )
  withr::local_options(dataquieR.type_adjust_parallel = TRUE)

  messages <- new.env(parent = emptyenv())
  messages$values <- character(0)
  adjusted <- withCallingHandlers(
    util_adjust_data_type2(study_data, meta_data),
    message = function(m) {
      msg <- conditionMessage(m)
      if (grepl("Data type transformation", msg, fixed = TRUE)) {
        messages$values <- c(messages$values, msg)
      }
      invokeRestart("muffleMessage")
    }
  )

  expect_true(calls$started)
  expect_equal(calls$cluster_calls, 2L)
  expect_true(isTRUE(attr(adjusted, "Data_type_matches", exact = TRUE)))
  expect_equal(adjusted$x, c(1, NA, 3))
  expect_equal(adjusted$y, c(4.5, 5.5, NA))
  expect_true(any(grepl("x", messages$values, fixed = TRUE)))
  expect_true(any(grepl("y", messages$values, fixed = TRUE)))
})

test_that("util_adjust_data_type2 keeps full conversion for string metadata", {
  study_data <- data.frame(
    date_like_string = c("2020-01-01", "2020-01-02", "2020-01-03"),
    float_like_string = c("1.23", "4.56", "7.89"),
    integer_like_string = c("1", "2", "3"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    DATA_TYPE = rep(DATA_TYPES$STRING, length(study_data)),
    stringsAsFactors = FALSE
  )

  skip_cols <- util_skip_readr_type_convert_cols(study_data, meta_data)

  expect_false(skip_cols[["date_like_string"]])
  expect_false(skip_cols[["float_like_string"]])
  expect_false(skip_cols[["integer_like_string"]])
  expect_false(util_can_skip_readr_type_convert(study_data, meta_data))
  expect_equal(
    prep_dq_data_type_of(study_data$date_like_string, guess_character = TRUE),
    DATA_TYPES$DATETIME
  )
  expect_equal(
    prep_dq_data_type_of(study_data$float_like_string, guess_character = TRUE),
    DATA_TYPES$FLOAT
  )
  expect_equal(
    prep_dq_data_type_of(
      study_data$integer_like_string, guess_character = TRUE
    ),
    DATA_TYPES$INTEGER
  )
})

test_that("util_adjust_data_type2 keeps full conversion for non-simple types", {
  meta_data <- data.frame(
    VAR_NAMES = c(
      "id", "date", "group", "date_from_string",
      "date_from_whole_double"
    ),
    DATA_TYPE = c("integer", "datetime", "string", "datetime", "datetime"),
    stringsAsFactors = FALSE
  )
  study_data <- data.frame(
    id = 1:3,
    date = as.POSIXct("2020-01-01", tz = "UTC") + 1:3,
    group = factor(c("a", "b", "c")),
    date_from_string = c("2020-01-01", "2020-01-02", "2020-01-03"),
    date_from_whole_double = c(20200101, 20200102, 20200103)
  )

  expect_false(util_can_skip_readr_type_convert(study_data, meta_data))
  expect_false(
    util_skip_readr_type_convert_cols(
      study_data, meta_data
    )[["date_from_string"]]
  )
  expect_false(
    util_skip_readr_type_convert_cols(
      study_data,
      meta_data
    )[["date_from_whole_double"]]
  )
})

test_that("util_adjust_data_type2 normalizes short datetime years", {
  skip_on_cran()

  study_data <- data.frame(
    date = c("1-02-03", "12-02-03", "123-02-03", "2020-02-03"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = "date",
    DATA_TYPE = DATA_TYPES$DATETIME,
    stringsAsFactors = FALSE
  )

  adjusted <- util_adjust_data_type2(study_data, meta_data)
  adjusted_parts <- as.POSIXlt(adjusted$date)

  expect_s3_class(adjusted$date, "POSIXct")
  expect_equal(adjusted_parts$year + 1900, c(1, 12, 123, 2020))
  expect_equal(adjusted_parts$mon + 1, rep(2, 4))
  expect_equal(adjusted_parts$mday, rep(3, 4))
  expect_equal(format(adjusted$date, "%m-%d"), rep("02-03", 4))
  expect_true(isTRUE(attr(adjusted, "Data_type_matches", exact = TRUE)))
})

test_that("util_adjust_data_type2 handles missing study data column names", {
  study_data <- data.frame(
    x = c("1", "2", "3"),
    stringsAsFactors = FALSE
  )
  names(study_data)[[1]] <- NA_character_
  meta_data <- data.frame(
    VAR_NAMES = "x",
    DATA_TYPE = DATA_TYPES$INTEGER,
    stringsAsFactors = FALSE
  )

  adjusted <- util_adjust_data_type2(study_data, meta_data)

  expect_true(is.na(names(adjusted)[[1]]))
  expect_equal(adjusted[[1]], c(1, 2, 3))
})

test_that("util_adjust_data_type2 avoids generated-name prefix collisions", {
  study_data <- data.frame(
    reserved = c("9", "10"),
    unnamed = c("1", "2"),
    stringsAsFactors = FALSE
  )
  names(study_data) <- c("#1", "")
  meta_data <- data.frame(
    VAR_NAMES = c("#1", "x"),
    DATA_TYPE = rep(DATA_TYPES$INTEGER, 2),
    stringsAsFactors = FALSE
  )

  adjusted <- util_adjust_data_type2(study_data, meta_data)

  expect_equal(names(adjusted), c("#1", ""))
  expect_equal(adjusted[[1]], c(9, 10))
  expect_equal(adjusted[[2]], c(1, 2))
})

test_that("util_adjust_data_type2 skips matching columns despite unknown columns", { # nolint: line_length_linter.
  study_data <- data.frame(
    id = 1:3,
    score = c("1.5", "2.5", "3.5"),
    extra = c("1", "2", "3"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("id", "score"),
    DATA_TYPE = c("integer", "float"),
    stringsAsFactors = FALSE
  )

  skip_cols <- util_skip_readr_type_convert_cols(study_data, meta_data)
  col_types <- util_readr_cols_for_study_data(
    names(study_data)[!skip_cols],
    util_readr_cols_from_metadata(meta_data)
  )

  expect_true(skip_cols[["id"]])
  expect_false(skip_cols[["score"]])
  expect_false(skip_cols[["extra"]])
  expect_false(util_can_skip_readr_type_convert(study_data, meta_data))
  expect_named(col_types, c("score", "extra"))
  expect_s3_class(col_types[["extra"]], "collector_guess")

  adjusted <- util_adjust_data_type2(study_data, meta_data)

  expect_equal(adjusted$id, study_data$id)
  expect_type(adjusted$score, "double")
  expect_equal(adjusted$score, c(1.5, 2.5, 3.5))
  expect_equal(adjusted$extra, c(1, 2, 3))
})

test_that("util_adjust_data_type2 keeps declared matches and old factor mode", {
  already_matched <- data.frame(x = 1:3)
  attr(already_matched, "Data_type_matches") <- TRUE
  meta_data <- data.frame(
    VAR_NAMES = "x",
    DATA_TYPE = DATA_TYPES$INTEGER,
    stringsAsFactors = FALSE
  )

  expect_identical(
    util_adjust_data_type2(already_matched, meta_data),
    already_matched
  )

  factor_data <- data.frame(x = factor(c("a", "b", "a")))
  factor_meta <- data.frame(
    VAR_NAMES = "x",
    DATA_TYPE = DATA_TYPES$STRING,
    stringsAsFactors = FALSE
  )

  skip_cols <- util_skip_readr_type_convert_cols(factor_data, factor_meta)
  expect_false(skip_cols[["x"]])
  adjusted <- withr::with_options(
    list(dataquieR.old_factor_handling = TRUE),
    util_adjust_data_type2(factor_data, factor_meta)
  )

  expect_equal(adjusted$x, c("1", "2", "1"))
  expect_true(isTRUE(attr(adjusted, "Data_type_matches", exact = TRUE)))
})
