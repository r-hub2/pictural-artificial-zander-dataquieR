test_that("util_combine_res drops wrapped dataquieR_NULL results", {
  skip_on_cran()

  tab <- data.frame(Variables = "visible", value = 1)
  result <- list(SummaryData = tab)
  class(result) <- c("dataquieR_result", class(result))
  attr(result, "cn") <- "con_attention_check_items"
  attr(result, "call") <- quote(con_attention_check_items()$SummaryData)

  null_result <- list()
  class(null_result) <- c("dataquieR_NULL", class(null_result))
  attr(null_result, "error") <- list(simpleError("not applicable"))
  wrapped_null <- list(null_result)

  combined <- util_combine_res(list(
    con_attention_check_items.visible = result,
    con_attention_check_items.empty = wrapped_null
  ))

  expect_named(combined, "con_attention_check_items")
  expect_equal(
    unname(as.data.frame(combined$con_attention_check_items$SummaryData)),
    tab,
    ignore_attr = TRUE
  )
})

test_that("slot subsetting preserves dataquieR_NULL before combining", {
  skip_on_cran()

  tab <- data.frame(Variables = "visible", value = 1)
  result <- list(SummaryData = tab)
  class(result) <- c("dataquieR_result", class(result))
  attr(result, "cn") <- "con_limit_deviations"
  attr(result, "call") <- quote(con_limit_deviations()$SummaryData)

  null_result <- list()
  class(null_result) <- c("dataquieR_result", "dataquieR_NULL", "list")
  attr(null_result, "cn") <- "con_limit_deviations"
  attr(null_result, "call") <- quote(con_limit_deviations()$SummaryData)
  attr(null_result, "error") <- list(simpleError("not applicable"))

  slot_results <- list(
    con_limit_deviations.visible =
      util_subset_result_slot_for_combine(result, "SummaryData"),
    con_limit_deviations.empty =
      util_subset_result_slot_for_combine(null_result, "SummaryData")
  )

  expect_s3_class(
    slot_results$con_limit_deviations.empty,
    "dataquieR_NULL"
  )
  combined <- expect_warning(util_combine_res(slot_results), NA)

  expect_equal(
    unname(as.data.frame(combined$con_limit_deviations$SummaryData)),
    tab,
    ignore_attr = TRUE
  )
})
