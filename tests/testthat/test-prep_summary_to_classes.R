test_that("prep_summary_to_classes rejects unclassed summary-like lists", {
  skip_on_cran()

  malformed_summary <- list(
    Data = list(),
    Table = data.frame(),
    meta_data = data.frame()
  )

  expect_error(
    suppressWarnings(prep_summary_to_classes(malformed_summary)),
    "must be returned by",
    fixed = TRUE
  )
})

test_that("prep_summary_to_classes keeps metric-free summary rows", {
  skip_on_cran()

  report_summary <- structure(
    list(
      Data = list(),
      Table = data.frame(
        VAR_NAMES = c("x", "y"),
        STUDY_SEGMENT = c("baseline", "follow-up"),
        stringsAsFactors = FALSE
      ),
      meta_data = data.frame(VAR_NAMES = c("x", "y"))
    ),
    class = "dq_report2_summary"
  )

  classes <- suppressWarnings(prep_summary_to_classes(report_summary))

  expect_s3_class(classes, "dq_report2_summaryclasses")
  expect_equal(
    as.data.frame(classes),
    report_summary$Table
  )
})

test_that("prep_summary_to_classes keeps variable-group identifiers", {
  skip_on_cran()

  report_summary <- structure(
    list(
      Data = list(`con_test.PCT_metric` = 75),
      Table = data.frame(
        VAR_NAMES = "group-a",
        STUDY_SEGMENT = "baseline",
        CHECK_ID = "group-a",
        `con_test.PCT_metric` = 75,
        check.names = FALSE
      ),
      meta_data = data.frame(
        VAR_NAMES = "group-a",
        CHECK_ID = "group-a"
      )
    ),
    class = "dq_report2_summary"
  )

  testthat::local_mocked_bindings(
    util_get_thresholds = function(indicator_metric, meta_data) {
      expect_identical(indicator_metric, "PCT_metric")
      setNames(list(c(`1` = "[0; 100]")), meta_data[[VAR_NAMES]])
    }
  )

  classes <- suppressWarnings(prep_summary_to_classes(report_summary))

  expect_identical(classes[[CHECK_ID]], "group-a")
})

test_that("prep_summary_to_classes accepts metrics without call names", {
  skip_on_cran()

  report_summary <- structure(
    list(
      Data = list(PCT_metric = 75),
      Table = data.frame(
        VAR_NAMES = "x",
        STUDY_SEGMENT = "baseline",
        PCT_metric = 75,
        stringsAsFactors = FALSE
      ),
      meta_data = data.frame(VAR_NAMES = "x")
    ),
    class = "dq_report2_summary"
  )

  testthat::local_mocked_bindings(
    util_get_thresholds = function(indicator_metric, meta_data) {
      expect_identical(indicator_metric, "PCT_metric")
      list(x = c(`1` = "[0; 50)", `2` = "[50; 100]"))
    }
  )

  classes <- suppressWarnings(prep_summary_to_classes(report_summary))

  expect_equal(classes$class, "2")
  expect_equal(classes$indicator_metric, "PCT_metric")
  expect_equal(classes$call_names, "")
})

test_that("prep_summary_to_classes warns about invalid dotted result names", {
  skip_on_cran()

  summary_table <- data.frame(
    VAR_NAMES = "x",
    STUDY_SEGMENT = "baseline",
    check.names = FALSE
  )
  summary_table[["call.metric.extra"]] <- 1
  report_summary <- structure(
    list(
      Data = list(`call.metric.extra` = 1),
      Table = summary_table,
      meta_data = data.frame(VAR_NAMES = "x")
    ),
    class = "dq_report2_summary"
  )
  observed_warnings <- new.env(parent = emptyenv())
  observed_warnings$messages <- character()

  testthat::local_mocked_bindings(
    util_get_thresholds = function(indicator_metric, meta_data) {
      expect_identical(indicator_metric, "extra")
      setNames(
        rep(list(c(`1` = "[0; 1]")), nrow(meta_data)),
        meta_data[[VAR_NAMES]]
      )
    }
  )

  classes <- withCallingHandlers(
    prep_summary_to_classes(report_summary),
    warning = function(wrn) {
      msg <- conditionMessage(wrn)
      if (!grepl("deprecated", msg, fixed = TRUE)) {
        observed_warnings$messages <- c(observed_warnings$messages, msg)
      }
      invokeRestart("muffleWarning")
    }
  )

  expect_s3_class(classes, "dq_report2_summaryclasses")
  expect_true(any(grepl(
    "more\\s+than one \\.",
    observed_warnings$messages,
    perl = TRUE
  )))
  expect_equal(classes$call_names, "")
  expect_equal(classes$indicator_metric, "")
})
