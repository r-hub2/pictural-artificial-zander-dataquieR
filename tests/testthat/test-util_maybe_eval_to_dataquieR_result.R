test_that(
  "util_maybe_eval_to_dataquieR_result honors explicit bypass options",
  {
    skip_on_cran()

    withr::local_options(dataquieR.dontwrapresults = TRUE)
    expect_identical(util_maybe_eval_to_dataquieR_result(1 + 1), 2)

    withr::local_options(
      dataquieR.dontwrapresults = FALSE,
      dataquieR.testdebug = TRUE
    )
    expect_identical(
      util_maybe_eval_to_dataquieR_result({
        value <- 2
        value + 3
      }),
      5
    )
  }
)

test_that("util_maybe_eval_to_dataquieR_result wraps caller results", {
  skip_on_cran()

  testthat::local_mocked_bindings(
    util_testthat_blocks_interactive_wrapper = function() FALSE
  )

  f <- function(resp_vars = "x") {
    util_maybe_eval_to_dataquieR_result(
      list(SummaryTable = data.frame(Variables = resp_vars, value = 1))
    )
  }

  wrapped <- f("v1")

  expect_s3_class(wrapped, "dataquieR_result")
  expect_identical(attr(wrapped, "function_name", exact = TRUE), "f")
  expect_identical(attr(wrapped, "cn", exact = TRUE), "f")
  expect_identical(
    attr(attr(wrapped, "call", exact = TRUE), "entity_name", exact = TRUE),
    "v1"
  )
  expect_s3_class(wrapped$SummaryTable, "TableSlot")
  expect_identical(wrapped$SummaryTable$Variables, "v1")

  g <- function() {
    util_maybe_eval_to_dataquieR_result(list())
  }

  all_result <- g()

  expect_s3_class(all_result, "dataquieR_result")
  expect_identical(attr(all_result, "function_name", exact = TRUE), "g")
  expect_null(
    attr(attr(all_result, "call", exact = TRUE), "entity_name", exact = TRUE)
  )
})
