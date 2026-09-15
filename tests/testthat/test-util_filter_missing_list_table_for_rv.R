test_that("util_filter_missing_list_table_for_rv keeps unrestricted tables", {
  skip_on_cran()

  missing_table <- data.frame(
    CODE_VALUE = c("999", "998"),
    CODE_LABEL = c("missing", "unknown")
  )

  expect_identical(
    util_filter_missing_list_table_for_rv(missing_table, rv = "age"),
    missing_table
  )
})

test_that(
  "util_filter_missing_list_table_for_rv combines specific and unspecific rows",
  {
    skip_on_cran()

    missing_table <- data.frame(
      resp_vars = c("age label", "AGE", "height", "", NA_character_),
      CODE_VALUE = c("997", "998", "996", "999", "995"),
      CODE_LABEL = c("label-specific", "name-specific", "other",
        "global", "global-na")
    )

    filtered <- util_filter_missing_list_table_for_rv(
      missing_table,
      rv = "age label",
      rv2 = "AGE"
    )

    expect_equal(
      filtered$CODE_LABEL,
      c("label-specific", "name-specific", "global", "global-na")
    )
  }
)
