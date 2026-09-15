skip_on_cran()

test_that("variable-group identity is added consistently", {
  result <- list(
    VariableGroupTable = data.frame(value = 1:2),
    VariableGroupData = data.frame(value = 3:4),
    SummaryTable = data.frame(value = 5)
  )

  result <- util_add_variable_group_identity(
    result,
    check_id = "check_1",
    check_label = "Check one"
  )

  expect_identical(util_attr(result, CHECK_ID, exact = TRUE), "check_1")
  expect_identical(util_attr(result, CHECK_LABEL, exact = TRUE), "Check one")
  expect_identical(result$VariableGroupTable[[CHECK_ID]], rep("check_1", 2))
  expect_identical(
    result$VariableGroupData[[CHECK_LABEL]],
    rep("Check one", 2)
  )
  expect_false(CHECK_ID %in% colnames(result$SummaryTable))
})

test_that("empty variable-group identities leave results unchanged", {
  result <- list(VariableGroupTable = data.frame(value = 1))
  expect_identical(
    util_add_variable_group_identity(result, NA_character_, NA_character_),
    result
  )
})
