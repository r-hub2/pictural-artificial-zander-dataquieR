test_that("util_undisclose drops redundant plotly and study-data slots", {
  skip_on_cran()

  result <- list(
    PlotlyPlot = list(plotly = TRUE),
    SummaryPlot = data.frame(x = 1L),
    ModifiedStudyData = data.frame(id = 1L),
    SummaryTable = data.frame(Variables = "x")
  )
  class(result) <- c("dataquieR_result", "master_result", "list")

  undisclosed <- util_undisclose(result)

  expect_false("PlotlyPlot" %in% names(undisclosed))
  expect_false("ModifiedStudyData" %in% names(undisclosed))
  expect_s3_class(undisclosed$SummaryPlot, "Slot")
  expect_s3_class(undisclosed$SummaryTable, "Slot")
  expect_identical(as.data.frame(undisclosed$SummaryPlot), data.frame(x = 1L))
  expect_identical(
    as.data.frame(undisclosed$SummaryTable),
    data.frame(Variables = "x")
  )
})
