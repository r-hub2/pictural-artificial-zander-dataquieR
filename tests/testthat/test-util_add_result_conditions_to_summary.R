test_that("result conditions use variable-group summary identifiers", {
  skip_on_cran()

  result <- list()
  class(result) <- "dataquieR_result"
  result_call <- quote(example_check(items))
  attr(result_call, VAR_NAMES) <- c("item_1", "item_2")
  attr(result_call, STUDY_SEGMENT) <- "SEGMENT"
  attr(result, "call") <- result_call
  attr(result, "cn") <- "example_check"
  summary <- data.frame(
    VAR_NAMES = c("group_a", "group_b"),
    STUDY_SEGMENT = "SEGMENT",
    call_names = "example_check",
    value = 1,
    values_raw = 1,
    function_name = "example_check",
    indicator_metric = "metric",
    CHECK_ID = c("check-a", "check-b"),
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    util_get_category_for_result = function(...) 1L,
    util_get_message_for_result = function(...) "A result message"
  )
  actual <- util_add_result_conditions_to_summary(
    result = result,
    summary = summary,
    function_name = "example_check"
  )

  condition_rows <- actual[startsWith(actual$indicator_metric, "MSG_"), ]
  expect_setequal(condition_rows$VAR_NAMES, c("group_a", "group_b"))
  expect_true(all(condition_rows$value == "A result message"))
  expect_false(any(condition_rows$VAR_NAMES %in% c("item_1", "item_2")))
  expect_setequal(condition_rows[[CHECK_ID]], c("check-a", "check-b"))
})
