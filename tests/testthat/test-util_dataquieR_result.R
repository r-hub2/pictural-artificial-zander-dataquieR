test_that("util_dataquieR_result marks allowed slot classes", {
  skip_on_cran()

  result <- util_dataquieR_result(list(
    SummaryTable = data.frame(Variables = "v1"),
    SummaryData = data.frame(value = 1),
    ModifiedStudyData = data.frame(v1 = 1),
    Other = list(note = "kept")
  ))

  expect_s3_class(result, "dataquieR_result")
  expect_s3_class(result, "master_result")
  expect_s3_class(result$SummaryTable, "TableSlot")
  expect_s3_class(result$SummaryData, "DataSlot")
  expect_s3_class(result$ModifiedStudyData, "StudyDataSlot")
  expect_s3_class(result$Other, "Other")
})

test_that("util_dataquieR_result validates slot names and required columns", {
  skip_on_cran()

  expect_error(
    util_dataquieR_result(list(UnexpectedSlot = data.frame())),
    "unexpected result"
  )

  expect_error(
    util_dataquieR_result(list(SummaryTable = data.frame(value = 1))),
    "Variables"
  )

  expect_error(
    util_dataquieR_result(list(ResultTable = data.frame(value = 1))),
    "r\\$ResultTable"
  )

  expect_error(
    util_dataquieR_result(list(DataframeTable = data.frame(value = 1))),
    "r\\$DataframeTable"
  )

  expect_error(
    util_dataquieR_result(list(SegmentTable = data.frame(value = 1))),
    "r\\$SegmentTable"
  )

  expect_error(
    util_dataquieR_result(list(VariableGroupTable = data.frame(value = 1))),
    "r\\$VariableGroupTable"
  )

  report_summary <- structure(
    data.frame(Variables = "v1"),
    class = c("ReportSummaryTable", "data.frame")
  )
  expect_error(
    util_dataquieR_result(list(ReportSummaryTable = report_summary)),
    "r\\$ReportSummaryTable"
  )
})

test_that("util_dataquieR_result accepts list-backed data and plot slots", {
  skip_on_cran()

  result <- util_dataquieR_result(list(
    DataframeDataList = list(df_a = data.frame(x = 1)),
    SegmentDataList = list(seg_a = data.frame(x = 1)),
    DataTypePlotList = list(),
    VariableGroupPlotList = list(),
    ResultData = data.frame(value = 1),
    OtherTable = data.frame(note = "ok")
  ))

  expect_s3_class(result, "dataquieR_result")
  expect_s3_class(result, "master_result")
  expect_equal(names(result$DataframeDataList), "df_a")
  expect_equal(names(result$SegmentDataList), "seg_a")
})

test_that("dataquieR_result extractors preserve result metadata", {
  skip_on_cran()

  result <- util_dataquieR_result(list(
    SummaryTable = data.frame(Variables = "v1"),
    Other = list(note = "kept")
  ))
  attr(result, "message") <- list(simpleMessage("stored message"))
  attr(result, "warning") <- list(simpleWarning("stored warning"))
  attr(result, "function_name") <- "coverage_helper"

  subset_result <- result["SummaryTable"]
  expect_s3_class(subset_result, "dataquieR_result")
  expect_identical(
    util_attr(subset_result, "function_name", exact = TRUE),
    "coverage_helper"
  )
  expect_length(util_attr(subset_result, "message", exact = TRUE), 1L)

  slot <- result[["Other"]]
  expect_s3_class(slot, "Slot")
  expect_identical(slot$note, "kept")
  expect_length(util_attr(slot, "warning", exact = TRUE), 1L)

  missing_slot <- result[["DoesNotExist"]]
  expect_s3_class(missing_slot, "dataquieR_NULL")
  expect_s3_class(missing_slot, "Slot")
})

test_that("util_dataquieR_result rejects bare data frames in list slots", {
  skip_on_cran()

  expect_error(util_dataquieR_result(list(
    DataframeDataList = data.frame(value = 1)
  )))
  expect_error(util_dataquieR_result(list(
    SegmentDataList = data.frame(value = 1)
  )))
})
