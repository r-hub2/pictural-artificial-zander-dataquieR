test_that("util_conversion_stable checks conversion loss by data type", {
  skip_on_cran()

  expect_equal(
    util_conversion_stable(c("1", "2.5", ""), DATA_TYPES$INTEGER),
    c(TRUE, FALSE, TRUE)
  )
  expect_equal(
    util_conversion_stable(c("1", "2.5", ""), DATA_TYPES$FLOAT),
    c(TRUE, TRUE, TRUE)
  )
  expect_equal(
    util_conversion_stable(c("2020-01-01", "bad", ""), DATA_TYPES$DATETIME),
    c(TRUE, FALSE, TRUE)
  )
  expect_equal(
    util_conversion_stable(
      c("2012-11-29 12:00:00 CET", "2012-11-29 12:00:00 CET asdf"),
      DATA_TYPES$DATETIME
    ),
    c(TRUE, FALSE)
  )
  expect_equal(
    util_conversion_stable(c("01:02:03", "bad", ""), DATA_TYPES$TIME),
    c(TRUE, FALSE, TRUE)
  )
  expect_equal(
    util_conversion_stable(c("text", NA), DATA_TYPES$STRING),
    c(TRUE, TRUE)
  )
  expect_equal(
    util_conversion_stable(c("1", "x"), DATA_TYPES$INTEGER,
      return_percentages = TRUE),
    50
  )
})

test_that("util_conversion_stable handles logical-origin integers", {
  skip_on_cran()

  logical_integer <- DATA_TYPES$INTEGER
  attr(logical_integer, "orig_type") <- "logical"

  expect_equal(
    util_conversion_stable(c("TRUE", "FALSE", "1", ""), logical_integer),
    c(TRUE, TRUE, FALSE, TRUE)
  )
})
