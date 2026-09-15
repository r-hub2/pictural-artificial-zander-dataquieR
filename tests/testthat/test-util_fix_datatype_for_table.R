test_that("util_fix_datatype_for_table annotates known table columns", {
  skip_on_cran()

  table <- data.frame(
    CODE_VALUE = "1",
    CODE_LABEL = "one",
    CODE_ORDER = 1L,
    check.names = FALSE
  )

  fixed <- util_fix_datatype_for_table(table)

  expect_equal(util_attr(fixed[[CODE_VALUE]], DATA_TYPE, exact = TRUE),
    DATA_TYPES$STRING)
  expect_equal(util_attr(fixed[[CODE_LABEL]], DATA_TYPE, exact = TRUE),
    DATA_TYPES$STRING)
  expect_equal(util_attr(fixed[[CODE_ORDER]], DATA_TYPE, exact = TRUE),
    DATA_TYPES$INTEGER)
})

test_that("util_fix_datatype_for_table drives render alignment metadata", {
  skip_on_cran()

  table <- data.frame(
    CODE_VALUE = "1",
    CODE_LABEL = "one",
    CODE_ORDER = "10",
    check.names = FALSE
  )

  fixed <- util_fix_datatype_for_table(table)
  alignments <- util_get_datatables_alignments(fixed)

  expect_identical(
    vapply(alignments, `[[`, character(1), "className"),
    c("dt-left", "dt-left", "dt-right")
  )
})

test_that("util_fix_datatype_for_table returns non-dataframes unchanged", {
  skip_on_cran()

  expect_warning(
    fixed <- util_fix_datatype_for_table("not a table"),
    "not a data frame"
  )
  expect_identical(fixed, "not a table")
})
