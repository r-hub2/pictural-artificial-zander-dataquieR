test_that("util_recode maps values through a table", {
  skip_on_cran()

  mapping_table <- data.frame(
    old = c("a", "b"),
    new = c("A", "B"),
    stringsAsFactors = FALSE
  )

  expect_equal(
    util_recode(c("a", "b", "c", NA), mapping_table, "old", "new",
      default = "?"),
    c("A", "B", "?", NA)
  )
})

test_that("util_recode supports per-value defaults", {
  skip_on_cran()

  mapping_table <- data.frame(
    old = c("a", "b"),
    new = c("A", "B"),
    stringsAsFactors = FALSE
  )

  expect_equal(
    util_recode(c("a", "c", "d"), mapping_table, "old", "new",
      default = c("x", "y", "z")),
    c("A", "y", "z")
  )
})

test_that("util_recode validates mapping table columns and defaults", {
  skip_on_cran()

  mapping_table <- data.frame(
    old = "a",
    new = "A",
    stringsAsFactors = FALSE
  )

  expect_error(
    util_recode("a", mapping_table, "missing", "new"),
    "missing"
  )
  expect_error(
    util_recode(c("a", "b"), mapping_table, "old", "new",
      default = "too_short"[FALSE]),
    "length in \\[2:2\\]"
  )
})
