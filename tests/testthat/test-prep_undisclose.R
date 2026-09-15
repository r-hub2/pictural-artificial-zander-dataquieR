skip_on_cran()

test_that("util_undisclose handles simple non-disclosing objects", {
  expect_identical(util_undisclose(1:3), 1:3)

  data <- data.frame(x = 1:2)
  expect_identical(util_undisclose(data), data)
  expect_identical(util_undisclose(list(data = data, value = "ok")),
    list(data = data, value = "ok"))

  expect_error(util_undisclose(mean), "object of class")
})

test_that("util_undisclose delegates non-standard result subclasses", {
  skip_on_cran()

  result <- structure(
    list(value = "not a standard dataquieR result"),
    class = c("custom_result", "dataquieR_result", "list")
  )

  undisclosed <- util_undisclose(result)

  expect_s3_class(undisclosed, "custom_result")
  expect_s3_class(undisclosed$value, "Slot")
  expect_identical(as.character(undisclosed$value),
    "not a standard dataquieR result")
})

test_that("util_undisclose removes redundant plotly slot from result objects", {
  result <- structure(
    list(
      PlotlyPlot = "disclosing plotly payload",
      SummaryPlot = "already rendered plot"
    ),
    class = c("dataquieR_result", "list")
  )

  undisclosed <- util_undisclose(result)

  expect_s3_class(undisclosed, "dataquieR_result")
  expect_false("PlotlyPlot" %in% names(undisclosed))
  expect_equal(as.character(undisclosed$SummaryPlot), "already rendered plot")
})

test_that("util_undisclose removes plotly-only slots when conversion fails", {
  result <- structure(
    list(
      PlotlyPlot = structure(list(), class = "plotly")
    ),
    class = c("dataquieR_result", "list")
  )

  testthat::local_mocked_bindings(
    util_plotly2svg_object = function(...) {
      stop("no static plotly conversion")
    }
  )

  expect_warning(
    undisclosed <- util_undisclose(result),
    "Could not convert a plotly"
  )

  expect_s3_class(undisclosed, "dataquieR_result")
  expect_false("PlotlyPlot" %in% names(undisclosed))
  expect_false("SummaryPlot" %in% names(undisclosed))
})

test_that("util_undisclose converts plotly-only slots when possible", {
  result <- structure(
    list(
      PlotlyPlot = structure(list(widget = TRUE), class = "plotly")
    ),
    class = c("dataquieR_result", "list")
  )

  testthat::local_mocked_bindings(
    util_fix_sizing_hints = function(dqr, x) {
      attr(dqr, "sizing_hints") <- list(width = 100)
      list(dqr = dqr, x = x)
    },
    util_plotly2svg_object = function(x, sizing_hints = NULL) {
      data.frame(
        converted = isTRUE(x$widget),
        width = sizing_hints$width
      )
    }
  )

  undisclosed <- util_undisclose(result)

  expect_s3_class(undisclosed, "dataquieR_result")
  expect_false("PlotlyPlot" %in% names(undisclosed))
  expect_s3_class(undisclosed$SummaryPlot, "Slot")
  expect_equal(as.data.frame(undisclosed$SummaryPlot), data.frame(
    converted = TRUE,
    width = 100
  ))
})

test_that("util_undisclose removes attached non-standard metadata tables", {
  result <- structure(
    list(
      value = "ok",
      FlaggedStudyData = data.frame(secret = "do not disclose")
    ),
    class = c("dataquieR_result", "list")
  )
  report <- structure(
    list(
      simple_result = result,
      ModifiedStudyData = data.frame(secret = "do not disclose"),
      USER_TABLE = data.frame(
        id = 1,
        study_data = I(list(data.frame(secret = "do not disclose")))
      ),
      CODE_LIST_TABLE = data.frame(code = 1, label = "expected")
    ),
    class = "dataquieR_resultset2"
  )
  attr(report, "referred_tables") <- list(
    CODE_LIST_TABLE = data.frame(code = 1, label = "expected")
  )

  undisclosed <- util_undisclose(report)

  expect_false("USER_TABLE" %in% names(undisclosed))
  expect_false("ModifiedStudyData" %in% names(undisclosed))
  expect_true(CODE_LIST_TABLE %in% names(undisclosed))
  expect_false("FlaggedStudyData" %in% names(undisclosed$simple_result))
  expect_equal(as.character(undisclosed$simple_result$value), "ok")
})

test_that("util_undisclose uses a private cluster for multi-core reports", {
  report <- structure(
    list(simple_result = "ok"),
    class = "dataquieR_resultset2"
  )
  attr(report, "referred_tables") <- list()

  seen <- new.env(parent = emptyenv())
  seen$cluster_calls <- character(0)
  testthat::local_mocked_bindings(
    util_par_lapply_lb = function(cl, x, fun, ...) {
      seen$cluster <- cl
      lapply(x, fun, ...)
    }
  )
  testthat::with_mocked_bindings(
    .package = "parallel",
    makePSOCKcluster = function(ncores) {
      structure(list(cores = ncores), class = "mock_cluster")
    },
    clusterCall = function(cl, fun, ...) {
      seen$cluster_calls <- c(seen$cluster_calls, as.character(list(...)[[1]]))
      list(TRUE)
    },
    stopCluster = function(cl) {
      seen$stopped <- identical(cl, seen$cluster)
      invisible(NULL)
    },
    undisclosed <- util_undisclose(report, cores = 2L)
  )

  expect_s3_class(undisclosed, "dataquieR_resultset2")
  expect_identical(seen$cluster$cores, 2L)
  expect_true(all(c("dataquieR", "hms") %in% seen$cluster_calls))
})

test_that("util_undisclose keeps already undisclosed SVG proxies", {
  svg <- tempfile(fileext = ".svg")
  writeLines(
    c(
      "<svg xmlns='http://www.w3.org/2000/svg' width='1' height='1'>",
      "<rect width='1' height='1' fill='black'/>",
      "</svg>"
    ),
    svg
  )
  proxy <- util_svg_plot_proxy(svg)

  expect_true(util_is_svg_object(proxy))
  expect_identical(util_undisclose(proxy), proxy)
})

test_that("util_undisclose recurses through Slot-compatible lists", {
  slot <- structure(
    list(
      nested = structure(
        list(
          value = "ok",
          NestedStudyData = data.frame(secret = "do not disclose")
        ),
        class = c("dataquieR_result", "list")
      )
    ),
    class = c("Slot", "list")
  )

  undisclosed <- util_undisclose(slot)

  expect_s3_class(undisclosed, "Slot")
  expect_false("NestedStudyData" %in% names(undisclosed$nested))
  expect_equal(as.character(undisclosed$nested$value), "ok")
})

test_that("prep_undisclose rejects unsupported object classes", {
  expect_error(prep_undisclose(data.frame(x = 1)), "results or reports")
})

test_that("prep_undisclose handles minimal local result objects", {
  skip_on_cran()

  result <- structure(
    list(
      SummaryData = data.frame(value = 1),
      FlaggedStudyData = data.frame(secret = "do not disclose")
    ),
    class = c("dataquieR_result", "list")
  )

  expect_message(
    undisclosed <- prep_undisclose(result),
    "without any warranty"
  )

  expect_s3_class(undisclosed, "dataquieR_result")
  expect_true("SummaryData" %in% names(undisclosed))
  expect_false("FlaggedStudyData" %in% names(undisclosed))
  expect_equal(undisclosed$SummaryData$value, 1)
})

test_that("prep_undisclose accepts explicit serial core count", {
  skip_on_cran()

  result <- structure(
    list(
      SummaryData = data.frame(value = 1),
      FlaggedStudyData = data.frame(secret = "do not disclose")
    ),
    class = c("dataquieR_result", "list")
  )

  expect_message(
    undisclosed <- prep_undisclose(result, cores = 1L),
    "without any warranty"
  )

  expect_s3_class(undisclosed, "dataquieR_result")
  expect_true("SummaryData" %in% names(undisclosed))
  expect_false("FlaggedStudyData" %in% names(undisclosed))
})

test_that("prep_undisclose works", {
  skip_on_cran() # slow, parallel, ...
  skip_if_not_installed("stringdist")

  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx") # nolint: line_length_linter.
  study_data <- head(prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE), 100) # nolint: line_length_linter.
  meta_data <- prep_get_data_frame("item_level")

  mlt <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx| missing_table") # nolint: line_length_linter.

  prep_purge_data_frame_cache()

  prep_add_data_frames(`missing_table` = mlt)

  invisible(testthat::capture_output_lines(gc(full = TRUE, verbose = FALSE)))

  sd0 <- study_data[, 1:5]
  sd0$v00012 <- study_data$v00012
  md0 <- subset(meta_data, VAR_NAMES %in% colnames(sd0))
  md0$PART_VAR <- NULL

  # Drop MISSING_LIST_TABLE locally when inspecting reduced metadata.

  # don't include huge reports as RData in the package
  # Suppress warnings since we do not test dq_report2
  # here in the first place
  report <- dq_report2(sd0, md0,
    resp_vars = c(
      "v00000", "v00001", "v00002",
      "v00003", "v00004", "v00012"
    ),
    filter_indicator_functions =
      c(
        "distrib",
        "^acc_univariate_outlier$"
      ),
    cores = NULL,
    dimensions = # for speed, omit Accuracy
      c(
        "Integrity",
        "Completeness",
        "Consistency",
        "Accuracy"
      )
  )

  expect_equal(nrow(report$acc_univariate_outlier.SBP_0$SummaryPlotList$SBP_0$data), 88) # nolint: line_length_linter.
  report_x <- prep_undisclose(report)
  expect_equal(length(report_x$acc_univariate_outlier.SBP_0$SummaryPlotList$SBP_0$data), 0) # nolint: line_length_linter.
})
