test_that("acc_distributions_ecdf works", {
  skip_on_cran() # slow and tested elsewhere
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("colorspace")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship_meta_v2.xlsx") # nolint: line_length_linter.
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship_meta_v2.xlsx|item_level") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS", keep_types = TRUE) # nolint: line_length_linter.
  meta_data <-
    prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data
    )

  t1 <- acc_distributions_ecdf(
    group_vars = "obs_soma",
    study_data = study_data,
    meta_data = meta_data
  )

  expect_equal(length(names(t1$SummaryPlotList)), 13)

  expect_false(
    inherits(try(ggplot_build(t1$SummaryPlot$exdate)), "try-error")
  )
  expect_false(
    inherits(try(ggplot_build(t1$SummaryPlot$cholesterol)), "try-error")
  )
})

test_that("acc_distributions_ecdf handles local grouped response data", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) FALSE
  )

  study_data <- data.frame(
    value = seq_len(20),
    group = rep(LETTERS[1:5], each = 4),
    stringsAsFactors = FALSE
  )
  meta_data <- prep_create_meta(
    VAR_NAMES = c("value", "group"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$STRING),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR
  )

  expect_message(
    result <- acc_distributions_ecdf(
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES,
      group_vars = "group",
      n_group_max = 2,
      n_obs_per_group_min = 1
    ),
    "All variables with interval or ratio scale"
  )

  expect_named(result$SummaryPlotList, "value")
  built <- ggplot2::ggplot_build(result$SummaryPlotList$value)
  expect_true("other" %in% built$plot$data$group)
})

test_that("acc_distributions_ecdf reports missing suitable responses", {
  skip_on_cran()

  study_data <- data.frame(
    group = rep(c("A", "B"), each = 5),
    stringsAsFactors = FALSE
  )
  meta_data <- prep_create_meta(
    VAR_NAMES = "group",
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR
  )

  expect_error(
    expect_message(
      acc_distributions_ecdf(
        study_data = study_data,
        meta_data = meta_data,
        label_col = VAR_NAMES,
        group_vars = "group"
      ),
      "All variables with interval or ratio scale"
    ),
    "No suitable variables"
  )
})

test_that("acc_distributions_ecdf removes grouping variables from responses", {
  skip_on_cran()

  testthat::local_mocked_bindings(
    util_correct_variable_use = function(...) TRUE,
    util_ensure_suggested = function(...) FALSE
  )

  study_data <- data.frame(
    value = seq_len(20),
    group = rep(c("A", "B"), each = 10),
    stringsAsFactors = FALSE
  )
  meta_data <- prep_create_meta(
    VAR_NAMES = c("value", "group"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$STRING),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR
  )

  expect_warning(
    result <- acc_distributions_ecdf(
      resp_vars = c("value", "group"),
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES,
      group_vars = "group"
    ),
    "Removed grouping variable from response variables",
    fixed = TRUE
  )

  expect_named(result$SummaryPlotList, "value")
})

test_that("acc_distributions_ecdf reports when group filters remove all data", {
  skip_on_cran()

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) FALSE
  )

  study_data <- data.frame(
    value = seq_len(20),
    group = rep(LETTERS[1:5], each = 4),
    stringsAsFactors = FALSE
  )
  meta_data <- prep_create_meta(
    VAR_NAMES = c("value", "group"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$STRING),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR
  )

  expect_error(
    suppressMessages(acc_distributions_ecdf(
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES,
      group_vars = "group",
      n_obs_per_group_min = 5
    )),
    "No data left after data preparation"
  )
})
