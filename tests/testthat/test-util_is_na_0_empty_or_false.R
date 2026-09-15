test_that("util_is_na_0_empty_or_false recognizes common falsish values", {
  skip_on_cran()

  util_purge_falsish_value_cache()

  x <- c(NA, "", " ", "-", "false", "FALSE", "F", "f", "0", "0.0", "1", "yes")
  expect_identical(
    util_is_na_0_empty_or_false(x),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )

  # Repeating the same value should use the cached result and remain identical.
  expect_identical(
    util_is_na_0_empty_or_false(x),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )

  util_purge_falsish_value_cache()
  expect_identical(length(ls(.falsish_value_cache, all.names = TRUE)), 0L)
})

test_that(
  "util_is_na_0_empty_or_false handles hms values through character formatting",
  {
    skip_on_cran()
    skip_if_not_installed("hms")

    util_purge_falsish_value_cache()

    expect_identical(
      util_is_na_0_empty_or_false(hms::hms(seconds = c(0, 1, NA))),
      c(FALSE, FALSE, TRUE)
    )
  }
)
