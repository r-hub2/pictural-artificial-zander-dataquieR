skip_on_cran()

dataquieR_result_fixture <- function() {
  structure(
    list(
      SummaryTable = data.frame(a = 1L),
      SummaryData = list(value = 2L)
    ),
    class = c("dataquieR_result", "list"),
    error = "error message",
    message = "regular message",
    warning = "warning message",
    as_plotly = TRUE,
    dont_util_adjust_geom_text_for_plotly = FALSE,
    function_name = "mock_indicator",
    cn = "mock_column",
    call = quote(mock_indicator()),
    CHECK_ID = "group-1",
    CHECK_LABEL = "Group one"
  )
}

test_that("single-bracket extraction keeps dataquieR result attributes", {
  skip_on_cran()

  result <- dataquieR_result_fixture()

  extracted <- result["SummaryTable"]

  expect_s3_class(extracted, "dataquieR_result")
  expect_named(extracted, "SummaryTable")
  expect_identical(attr(extracted, "error", exact = TRUE), "error message")
  expect_identical(attr(extracted, "message", exact = TRUE), "regular message")
  expect_identical(attr(extracted, "warning", exact = TRUE), "warning message")
  expect_true(attr(extracted, "as_plotly", exact = TRUE))
  expect_false(
    attr(extracted, "dont_util_adjust_geom_text_for_plotly", exact = TRUE)
  )
  expect_identical(
    attr(extracted, "function_name", exact = TRUE),
    "mock_indicator"
  )
  expect_identical(attr(extracted, "cn", exact = TRUE), "mock_column")
  expect_identical(
    attr(extracted, "call", exact = TRUE),
    quote(mock_indicator())
  )
  expect_identical(attr(extracted, CHECK_ID, exact = TRUE), "group-1")
  expect_identical(attr(extracted, CHECK_LABEL, exact = TRUE), "Group one")
})

test_that("double-bracket extraction returns a slot with messages", {
  skip_on_cran()

  result <- dataquieR_result_fixture()

  extracted <- result[["SummaryTable"]]

  expect_s3_class(extracted, "Slot")
  expect_false(inherits(extracted, "dataquieR_result"))
  expect_identical(extracted$a, 1L)
  expect_identical(rownames(extracted), "1")
  expect_identical(attr(extracted, "error", exact = TRUE), "error message")
  expect_identical(attr(extracted, "message", exact = TRUE), "regular message")
  expect_identical(attr(extracted, "warning", exact = TRUE), "warning message")
})

test_that("dollar extraction dispatches like double-bracket extraction", {
  skip_on_cran()

  result <- dataquieR_result_fixture()

  extracted <- result$SummaryData

  expect_s3_class(extracted, "Slot")
  expect_identical(extracted$value, 2L)
  expect_identical(attr(extracted, "warning", exact = TRUE), "warning message")
})

test_that("missing result slots return a dataquieR NULL object", {
  skip_on_cran()

  result <- dataquieR_result_fixture()

  extracted <- result[["DoesNotExist"]]

  expect_s3_class(extracted, "dataquieR_NULL")
  expect_s3_class(extracted, "Slot")
  expect_length(extracted, 0L)
  expect_identical(attr(extracted, "error", exact = TRUE), "error message")

  dollar_extracted <- result$DoesNotExist
  expect_s3_class(dollar_extracted, "dataquieR_NULL")
  expect_s3_class(dollar_extracted, "Slot")
  expect_length(dollar_extracted, 0L)
  expect_identical(
    attr(dollar_extracted, "message", exact = TRUE),
    "regular message"
  )
})

test_that("dataquieR_result extraction supports condition-object metadata", {
  result <- structure(
    list(SummaryData = data.frame(value = 1), Other = "kept"),
    error = list(simpleError("boom")),
    message = list(simpleMessage("note")),
    warning = list(simpleWarning("careful")),
    as_plotly = TRUE,
    dont_util_adjust_geom_text_for_plotly = TRUE,
    function_name = "acc_example",
    cn = "acc_example.value",
    call = quote(acc_example(value)),
    class = c("dataquieR_result", "list")
  )

  subset <- result["SummaryData"]
  slot <- result[["SummaryData"]]
  missing_slot <- result[["MissingSlot"]]

  expect_s3_class(subset, "dataquieR_result")
  expect_named(subset, "SummaryData")
  expect_s3_class(attr(subset, "error", exact = TRUE)[[1]], "simpleError")
  expect_s3_class(attr(subset, "message", exact = TRUE)[[1]], "simpleMessage")
  expect_s3_class(attr(subset, "warning", exact = TRUE)[[1]], "simpleWarning")
  expect_true(attr(subset, "as_plotly", exact = TRUE))
  expect_true(attr(
    subset,
    "dont_util_adjust_geom_text_for_plotly",
    exact = TRUE
  ))
  expect_identical(attr(subset, "function_name", exact = TRUE), "acc_example")
  expect_identical(attr(subset, "cn", exact = TRUE), "acc_example.value")
  expect_identical(
    attr(subset, "call", exact = TRUE),
    quote(acc_example(value))
  )

  expect_s3_class(slot, "Slot")
  expect_s3_class(attr(slot, "error", exact = TRUE)[[1]], "simpleError")
  expect_s3_class(attr(slot, "message", exact = TRUE)[[1]], "simpleMessage")
  expect_s3_class(attr(slot, "warning", exact = TRUE)[[1]], "simpleWarning")
  expect_s3_class(result$SummaryData, "Slot")

  expect_s3_class(missing_slot, "dataquieR_NULL")
  expect_s3_class(missing_slot, "Slot")
  expect_length(missing_slot, 0L)
})
