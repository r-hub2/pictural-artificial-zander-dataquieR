test_that("util_metrics_to_classes classifies metrics and messages", {
  skip_on_cran()

  rs_table_long <- data.frame(
    VAR_NAMES = c("x", "y", "x", "y"),
    STUDY_SEGMENT = "baseline",
    call_names = "call_a",
    function_name = "fn_a",
    indicator_metric = c(
      "PCT_metric",
      "PCT_metric",
      "MSG_note",
      "MSG_note"
    ),
    value = c(75, 25, NA, NA),
    values_raw = c("75", "25", "note-x", "note-y"),
    n_classes = NA_integer_,
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(VAR_NAMES = c("x", "y"))

  testthat::local_mocked_bindings(
    util_get_thresholds = function(indicator_metric, meta_data) {
      if (identical(indicator_metric, "PCT_metric")) {
        list(
          x = c(`1` = "[0; 50)", `2` = "[50; 100]"),
          y = c(`1` = "[0; 50)", `2` = "[50; 100]")
        )
      } else {
        setNames(
          rep(list(setNames(character(0), character(0))), nrow(meta_data)),
          meta_data[[VAR_NAMES]]
        )
      }
    }
  )

  classes <- util_metrics_to_classes(rs_table_long, meta_data)

  expect_s3_class(classes, "dq_report2_summaryclasses")
  pct_rows <- classes[classes$indicator_metric == "PCT_metric", ]
  msg_rows <- classes[classes$indicator_metric == "MSG_note", ]

  expect_equal(pct_rows$class, c("2", "1"))
  expect_equal(msg_rows$class, c("note-x", "note-y"))
})

test_that("util_metrics_to_classes normalizes logical raw metric values", {
  skip_on_cran()

  rs_table_long <- data.frame(
    VAR_NAMES = c("x", "y", "z"),
    STUDY_SEGMENT = "baseline",
    call_names = "call_a",
    function_name = "fn_a",
    indicator_metric = "PCT_flag",
    value = c(NA, NA, NA),
    values_raw = c("TRUE", "FALSE", NA),
    n_classes = NA_integer_,
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(VAR_NAMES = c("x", "y", "z"))

  testthat::local_mocked_bindings(
    util_get_thresholds = function(indicator_metric, meta_data) {
      expect_identical(indicator_metric, "PCT_flag")
      setNames(
        rep(list(c(`1` = "[0; 0]", `2` = "[1; 1]")), nrow(meta_data)),
        meta_data[[VAR_NAMES]]
      )
    }
  )

  classes <- util_metrics_to_classes(rs_table_long, meta_data)

  expect_equal(unname(as.character(classes$class)), c("2", "1", NA))
})

test_that("util_metrics_to_classes ignores missing metric rows", {
  skip_on_cran()

  rs_table_long <- data.frame(
    VAR_NAMES = c("x", "y", "z"),
    STUDY_SEGMENT = "baseline",
    call_names = "call_a",
    function_name = "fn_a",
    indicator_metric = c("PCT_flag", "PCT_flag", NA),
    value = c(NA, NA, NA),
    values_raw = c("TRUE", "FALSE", NA),
    n_classes = NA_integer_,
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(VAR_NAMES = c("x", "y", "z"))

  testthat::local_mocked_bindings(
    util_get_thresholds = function(indicator_metric, meta_data) {
      expect_identical(indicator_metric, "PCT_flag")
      setNames(
        rep(list(c(`1` = "[0; 0]", `2` = "[1; 1]")), nrow(meta_data)),
        meta_data[[VAR_NAMES]]
      )
    }
  )

  classes <- util_metrics_to_classes(rs_table_long, meta_data)

  expect_false(any(is.na(classes$indicator_metric)))
  expect_equal(unname(as.character(classes$class)), c("2", "1"))
})

test_that("util_metrics_to_classes fills missing numeric metric rows", {
  skip_on_cran()

  rs_table_long <- data.frame(
    VAR_NAMES = c("x", "y", "x"),
    STUDY_SEGMENT = "baseline",
    call_names = "call_a",
    function_name = "fn_a",
    indicator_metric = c("PCT_metric", "PCT_metric", "NUM_metric"),
    value = c(75, 25, 1),
    values_raw = c("75", "25", "1"),
    n_classes = NA_integer_,
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(VAR_NAMES = c("x", "y"))

  testthat::local_mocked_bindings(
    util_get_thresholds = function(indicator_metric, meta_data) {
      if (identical(indicator_metric, "PCT_metric")) {
        intervals <- c(`1` = "[0; 50)", `2` = "[50; 100]")
      } else {
        intervals <- c(`1` = "[0; 10]")
      }
      setNames(
        rep(list(intervals), nrow(meta_data)),
        meta_data[[VAR_NAMES]]
      )
    }
  )

  classes <- util_metrics_to_classes(rs_table_long, meta_data)
  completed_row <- classes[
    classes[[VAR_NAMES]] == "y" &
      classes$indicator_metric == "NUM_metric",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(completed_row), 1L)
  expect_true(is.na(completed_row$class))
  expect_true(is.na(completed_row$value))
  expect_true(is.na(completed_row$values_raw))
})

test_that("util_metrics_to_classes chooses worst overlapping class", {
  skip_on_cran()

  rs_table_long <- data.frame(
    VAR_NAMES = "x",
    STUDY_SEGMENT = "baseline",
    call_names = "call_a",
    function_name = "fn_a",
    indicator_metric = "PCT_metric",
    value = 50,
    values_raw = "50",
    n_classes = NA_integer_,
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(VAR_NAMES = "x")

  testthat::local_mocked_bindings(
    util_get_thresholds = function(indicator_metric, meta_data) {
      expect_identical(indicator_metric, "PCT_metric")
      list(x = c(`1` = "[0; 100]", `2` = "[50; 100]"))
    }
  )

  expect_warning(
    classes <- util_metrics_to_classes(rs_table_long, meta_data),
    "overlapping intervals"
  )

  expect_equal(unname(as.character(classes$class)), "2")
  expect_equal(unname(as.integer(classes$n_classes)), 2L)
})
