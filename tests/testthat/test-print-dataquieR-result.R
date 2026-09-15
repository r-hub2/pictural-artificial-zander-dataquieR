test_that("print.dataquieR_result handles stored conditions and slots", {
  skip_on_cran()

  with_message <- structure(
    list(SummaryTable = data.frame(a = 1)),
    class = c("dataquieR_result", "list"),
    message = list(simpleMessage("stored message"))
  )
  expect_message(
    invisible(print.dataquieR_result(with_message, view = FALSE)),
    "stored message"
  )

  with_warning <- structure(
    list(SummaryTable = data.frame(a = 1)),
    class = c("dataquieR_result", "list"),
    warning = list(simpleWarning("stored warning"))
  )
  expect_warning(
    invisible(print.dataquieR_result(with_warning, view = FALSE)),
    "stored warning"
  )

  with_error <- structure(
    list(),
    class = c("dataquieR_result", "dataquieR_NULL"),
    error = list(simpleError("stored error"))
  )
  printed_error <- capture.output(
    error_result <- print.dataquieR_result(with_error, view = FALSE),
    type = "message"
  )
  expect_null(error_result)
  expect_match(printed_error, "stored error")

  result <- structure(
    list(Other = "slot value"),
    class = c("dataquieR_result", "list")
  )
  expect_identical(
    print.dataquieR_result(result, slot = "Other", view = FALSE),
    "slot value"
  )
  expect_error(
    print.dataquieR_result(result, slot = "Missing", view = FALSE),
    "Cannot find Missing in result"
  )
})

test_that("print.dataquieR_result handles empty results", {
  skip_on_cran()

  empty_result <- structure(
    list(),
    class = c("dataquieR_result", "empty", "list")
  )

  expect_null(print.dataquieR_result(empty_result, view = FALSE))
})

test_that("util_dataquieR_result classifies allowed table and data slots", {
  skip_on_cran()

  report_summary <- util_new_report_summary_table(data.frame(
    Variables = "var1",
    N = 1L,
    metric = 0
  ))

  result <- util_dataquieR_result(list(
    ResultTable = data.frame(ResultName = "res1"),
    DataframeTable = data.frame(DF_NAME = "df1"),
    SegmentTable = data.frame(Segment = "seg1"),
    SummaryTable = data.frame(Variables = "var1"),
    ReportSummaryTable = report_summary,
    VariableGroupTable = data.frame(VARIABLE_LIST = "var1 | var2"),
    SummaryData = data.frame(value = 1),
    ModifiedStudyData = data.frame(var1 = 1),
    Other = "note"
  ))

  expect_s3_class(result, "dataquieR_result")
  expect_s3_class(result, "master_result")
  expect_s3_class(result$SummaryTable, "TableSlot")
  expect_s3_class(result$ReportSummaryTable, "ReportSummaryTable")
  expect_s3_class(result$SummaryData, "DataSlot")
  expect_s3_class(result$ModifiedStudyData, "StudyDataSlot")
  expect_s3_class(result$Other, "Other")

  other_data <- util_dataquieR_result(list(
    OtherData = data.frame(value = 1)
  ))
  expect_s3_class(other_data$OtherData, "DataSlot")

  other_table <- util_dataquieR_result(list(
    OtherTable = data.frame(value = 1)
  ))
  expect_s3_class(other_table$OtherTable, "TableSlot")

  expect_error(
    util_dataquieR_result(list(DataframeTable = data.frame(name = "df1")))
  )
})

test_that("util_dataquieR_result validates plot and nested data slots", {
  skip_on_cran()

  result <- util_dataquieR_result(list(
    PlotlyPlot = structure(list(), class = "plotly"),
    SummaryPlot = structure(list(), class = "gg"),
    SummaryPlotList = list(structure(list(), class = "gg")),
    DataTypePlotList = list(structure(list(), class = "gg")),
    DataframeDataList = list(data.frame(value = 1)),
    SegmentDataList = list(data.frame(value = 1)),
    VariableGroupPlotList = list(structure(list(), class = "gg")),
    ResultData = data.frame(value = 1),
    OtherData = data.frame(value = 1),
    OtherTable = data.frame(value = 1)
  ))

  expect_s3_class(result, "dataquieR_result")
  expect_s3_class(result$OtherData, "DataSlot")
  expect_s3_class(result$OtherTable, "TableSlot")

  expect_error(
    util_dataquieR_result(list(DataframeDataList = data.frame(value = 1)))
  )
  expect_error(
    util_dataquieR_result(list(SegmentDataList = data.frame(value = 1)))
  )

  expect_error(
    util_dataquieR_result(list(ResultTable = data.frame(value = 1)))
  )
  expect_error(
    util_dataquieR_result(list(SegmentTable = data.frame(value = 1)))
  )
  expect_error(
    util_dataquieR_result(list(SummaryTable = data.frame(value = 1)))
  )
  expect_error(
    util_dataquieR_result(list(VariableGroupTable = data.frame(value = 1)))
  )
  expect_error(
    util_dataquieR_result(list(UnknownSlot = data.frame(value = 1)))
  )
})

test_that("util_dataquieR_result rejects malformed plot and list slots", {
  skip_on_cran()

  expect_error(
    util_dataquieR_result(list(PlotlyPlot = structure(list(), class = "gg")))
  )
  expect_error(
    util_dataquieR_result(list(SummaryPlot = "not a plot"))
  )
  expect_error(
    util_dataquieR_result(list(SummaryPlotList = "not a list"))
  )
  expect_error(
    util_dataquieR_result(list(SummaryPlotList = list("not a plot")))
  )
  expect_error(
    util_dataquieR_result(list(DataTypePlotList = "not a list"))
  )
  expect_error(
    util_dataquieR_result(list(DataTypePlotList = list("not a plot")))
  )
  expect_error(
    util_dataquieR_result(list(VariableGroupPlotList = "not a list"))
  )
  expect_error(
    util_dataquieR_result(list(VariableGroupPlotList = list("not a plot")))
  )
  expect_error(
    util_dataquieR_result(list(DataframeDataList = "not a list"))
  )
  expect_error(
    util_dataquieR_result(list(SegmentDataList = "not a list"))
  )
})

test_that("slot print methods return converted objects for view false", {
  skip_on_cran()
  skip_if_not_installed("htmltools")
  skip_if_not_installed("tibble")

  table_slot <- structure(
    data.frame(Variables = "var1", value = 1),
    class = c("TableSlot", "data.frame")
  )
  data_slot <- structure(
    data.frame(value = 1),
    class = c("DataSlot", "data.frame")
  )
  study_slot <- structure(
    data.frame(value = 1),
    class = c("StudyDataSlot", "data.frame")
  )

  testthat::local_mocked_bindings(
    util_make_data_slot_from_table_slot = function(x) {
      data.frame(converted = x$value)
    },
    util_html_table = function(x, ...) {
      htmltools::span(class = "mock-table", paste(names(x), collapse = "|"))
    }
  )

  expect_s3_class(print.StudyDataSlot(study_slot, view = FALSE), "tbl_df")
  expect_s3_class(print.DataSlot(data_slot, view = FALSE), "shiny.tag")
  expect_s3_class(print.TableSlot(table_slot, view = FALSE), "shiny.tag")
})

test_that("print.Slot replays stored conditions and returns cleaned values", {
  skip_on_cran()

  other_slot <- structure(
    "note",
    class = c("Slot", "Other", "character")
  )
  expect_output(print(other_slot), "note")

  message_slot <- structure(
    1,
    class = c("Slot", "numeric"),
    message = list(simpleMessage("slot message"))
  )
  expect_message(
    message_result <- print(message_slot, view = FALSE),
    "slot message"
  )
  expect_equal(unclass(message_result), 1)
  expect_false(inherits(message_result, "Slot"))

  warning_slot <- structure(
    1,
    class = c("Slot", "numeric"),
    warning = list(simpleWarning("slot warning"))
  )
  expect_warning(
    warning_result <- print(warning_slot, view = FALSE),
    "slot warning"
  )
  expect_equal(unclass(warning_result), 1)
  expect_false(inherits(warning_result, "Slot"))

  error_slot <- structure(
    1,
    class = c("Slot", "numeric"),
    error = list(simpleError("slot error"))
  )
  printed_error <- capture.output(
    error_result <- print(error_slot, view = FALSE),
    type = "message"
  )
  expect_equal(unclass(error_result), 1)
  expect_false(inherits(error_result, "Slot"))
  expect_true(any(grepl("slot error", printed_error)))
})
