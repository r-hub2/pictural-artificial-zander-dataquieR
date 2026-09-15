skip_on_cran()

test_that("acc_end_digits works with 2 args", {
  skip_on_cran() # slow, errors obvious
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  meta_data <-
    prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data
    )
  expect_error(
    res1 <-
      acc_end_digits(study_data = study_data, meta_data = meta_data),
    regexp = "Argument resp_vars is NULL",
    perl = TRUE
  )
  expect_message2(
    res1 <-
      acc_end_digits(
        resp_vars = "v00014", study_data = study_data,
        meta_data = meta_data
      ),
    regexp =
      sprintf(
        "(%s)",
        paste(
          "Due to missing values in v00014 301",
          "observations were deleted."
        )
      ),
    perl = TRUE
  )

  expect_true(all(c("SummaryTable", "SummaryPlot") %in% names(res1)))
  expect_equal(
    nrow(res1$SummaryTable),
    1
  )
  expect_equal(
    ncol(res1$SummaryTable),
    2
  )
  expect_equal(
    suppressWarnings(sum(
      as.numeric(
        as.matrix(res1$SummaryTable)
      ),
      na.rm = TRUE
    )), 1
  )
})

test_that("acc_end_digits works with label_col", {
  skip_on_cran() # slow, errors obvious
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  meta_data <-
    prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data
    )
  expect_message2(
    res1 <-
      acc_end_digits(
        resp_vars = "CRP_0", study_data = study_data,
        meta_data = meta_data, label_col = LABEL
      ),
    regexp =
      sprintf(
        "(%s)",
        paste(
          "Due to missing values in CRP_0",
          "301 observations were deleted."
        )
      ),
    perl = TRUE
  )
  expect_true(all(c("SummaryTable", "SummaryPlot") %in% names(res1)))
  expect_equal(
    nrow(res1$SummaryTable),
    1
  )
  expect_equal(
    ncol(res1$SummaryTable),
    2
  )
  expect_equal(
    suppressWarnings(sum(
      as.numeric(
        as.matrix(res1$SummaryTable)
      ),
      na.rm = TRUE
    )), 1
  )
})

test_that("acc_end_digits works image check", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("vdiffr")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  meta_data <-
    prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data
    )
  expect_message2(
    res1 <-
      acc_end_digits(
        resp_vars = "CRP_0",
        study_data = study_data, meta_data = meta_data,
        label_col = LABEL
      ),
    regexp =
      sprintf(
        "(%s)",
        paste(
          "Due to missing values in CRP_0",
          "301 observations were deleted."
        )
      ),
    perl = TRUE
  )
  skip_on_cran()
  expect_doppelganger2(
    "enddigits plot for CRP_0 ok",
    res1$SummaryPlot
  )
})

test_that("acc_end_digits is robust", {
  skip_on_cran() # slow, errors obvious
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  meta_data <-
    prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data
    )
  sd0 <- study_data
  sd0$v00014[[2]] <- Inf
  expect_error(
    res1 <-
      acc_end_digits(
        resp_vars = "CRP_0",
        study_data = sd0, meta_data = meta_data,
        label_col = LABEL
      ),
    regexp =
      sprintf(
        "(%s)",
        paste("Values in .+resp_vars.+ must not contain infinite data")
      ),
    perl = TRUE
  )
  sd0 <- study_data
  md0 <- meta_data
  md0[[DECIMALS]] <- NULL
  expect_error(
    res1 <-
      acc_end_digits(
        resp_vars = "CRP_0",
        study_data = sd0, meta_data = md0,
        label_col = LABEL
      ),
    regexp =
      sprintf(
        "(%s)",
        paste(
          "The number of digits following the",
          "decimal point must be prespecified."
        )
      ),
    perl = TRUE
  )
  sd0 <- study_data
  sd0$v00010[[2]] <- sd0$v00010[[2]] + 0.5
  md0 <- meta_data
  expect_error(
    res1 <-
      acc_end_digits(
        resp_vars = "ARM_CUFF_0",
        study_data = sd0, meta_data = md0,
        label_col = LABEL
      ),
    regexp =
      paste(
        "Argument .+resp_vars.+: Variable .+ARM_CUFF_0.+ .nominal. does",
        "not have an allowed scale level"
      ),
    perl = TRUE
  )

  sd0 <- study_data
  sd0$v00014 <- c(rep(1:9, 3000 / 9), 1:3)
  sd0$v00014 <- sd0$v00014 / 1000
  sd0$CRP_0_x_last <- sd0$v00014
  sd0$CRP_0_x_last1 <- sd0$v00014
  md0 <- prep_add_to_meta(
    VAR_NAMES = "CRP_0_x_last",
    DATA_TYPE = DATA_TYPES$STRING,
    LABEL = "xxx",
    VALUE_LABELS = NA,
    meta_data
  )
  md0 <- prep_add_to_meta(
    VAR_NAMES = "CRP_0_x_last1",
    DATA_TYPE = DATA_TYPES$STRING,
    LABEL = "yyy",
    VALUE_LABELS = NA,
    md0
  )
  res1 <-
    acc_end_digits(
      resp_vars = "CRP_0",
      study_data = sd0, meta_data = md0,
      label_col = LABEL
    )

  sd0 <- study_data
  sd0$v00014 <- c(rep(1:9, 3000 / 9), 1:3)
  sd0$v00014 <- sd0$v00014 / 1000
  sd0$CRP_0_x_last <- sd0$v00014
  sd0$CRP_0_x_last1 <- sd0$v00014
  md0 <- prep_add_to_meta(
    VAR_NAMES = "CRP_0_x_last",
    DATA_TYPE = DATA_TYPES$STRING,
    LABEL = "xxx",
    VALUE_LABELS = NA,
    meta_data
  )
  md0 <- prep_add_to_meta(
    VAR_NAMES = "CRP_0_x_last1",
    DATA_TYPE = DATA_TYPES$STRING,
    LABEL = "yyy",
    VALUE_LABELS = NA,
    md0
  )
  md0$DISTRIBUTION <- NULL
  res1 <-
    acc_end_digits(
      resp_vars = "CRP_0",
      study_data = sd0, meta_data = md0,
      label_col = LABEL
    )
})

test_that("acc_end_digits rejects unrequested pipeline checks", {
  skip_on_cran()

  study_data <- data.frame(x = c(10, 11, 12, 13, 14, 15))
  meta_data <- data.frame(
    VAR_NAMES = "x",
    LABEL = "x",
    DATA_TYPE = DATA_TYPES$FLOAT,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    DECIMALS = 1L,
    END_DIGIT_CHECK = NA_character_,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  with_pipeline(expect_error(
    acc_end_digits(
      resp_vars = "x",
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES
    ),
    "No end digit check requested"
  ))
})

test_that("acc_end_digits supports integer-valued float variables", {
  study_data <- data.frame(x = c(10, 11, 12, 13, 14, 15))
  meta_data <- data.frame(
    VAR_NAMES = "x",
    LABEL = "x",
    DATA_TYPE = DATA_TYPES$FLOAT,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    DECIMALS = 1L,
    END_DIGIT_CHECK = "1",
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  expect_message2(
    result <- acc_end_digits(
      resp_vars = "x",
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES
    ),
    "x.+type integer"
  )
  expect_true(all(c("SummaryTable", "SummaryPlot") %in% names(result)))
})

test_that("acc_end_digits falls back to integer checks for whole floats", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  study_data <- data.frame(x = c(10, 11, 12, 13, 14, 15))
  meta_data <- data.frame(
    VAR_NAMES = "x",
    LABEL = "x",
    DATA_TYPE = DATA_TYPES$FLOAT,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    DECIMALS = 1L,
    END_DIGIT_CHECK = "1",
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )
  captured <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    acc_shape_or_scale = function(resp_vars, study_data, meta_data, ...) {
      captured$study_data <- study_data
      captured$meta_data <- meta_data
      list(
        SummaryTable = data.frame(Variables = "x_x_last", ok = 1),
        SummaryPlot = ggplot2::ggplot()
      )
    }
  )

  result <- suppressMessages(acc_end_digits(
    resp_vars = "x",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  ))

  expect_identical(captured$study_data$x, c(0, 1, 2, 3, 4, 5))
  expect_identical(captured$meta_data[[DATA_TYPE]], DATA_TYPES$INTEGER)
  expect_identical(captured$meta_data[[DISTRIBUTION]], DISTRIBUTIONS$UNIFORM)
  expect_identical(result$SummaryTable$Variables, "x")
})

test_that("acc_end_digits forwards shape-or-scale errors", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  study_data <- data.frame(x = c(0.1, 0.2, 0.3))
  meta_data <- data.frame(
    VAR_NAMES = "x",
    LABEL = "x",
    DATA_TYPE = DATA_TYPES$FLOAT,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    DECIMALS = 1L,
    END_DIGIT_CHECK = "1",
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    acc_shape_or_scale = function(...) {
      util_attach_attr(
        list(
          SummaryTable = data.frame(Variables = "x_x_last"),
          SummaryPlot = ggplot2::ggplot()
        ),
        error = list(simpleError("shape-or-scale failed"))
      )
    }
  )

  expect_error(
    suppressMessages(acc_end_digits(
      resp_vars = "x",
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES
    )),
    "shape-or-scale failed",
    fixed = TRUE
  )
})
