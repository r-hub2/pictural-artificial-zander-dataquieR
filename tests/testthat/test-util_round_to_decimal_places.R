skip_on_cran()

test_that("util_round_to_decimal_places formats ordinary numeric values", {
  expect_identical(
    util_round_to_decimal_places(c(1.2345, 12.345, 0, NA, Inf), digits = 3),
    c("1.23", "12.30", "0.00", "NA", "Inf")
  )
  expect_identical(
    util_round_to_decimal_places(numeric(0)),
    character(0)
  )
})

test_that("util_round_to_decimal_places switches to scientific notation", {
  expect_identical(
    util_round_to_decimal_places(c(0.00012, 12), digits = 2),
    c("1.20e-04", "1.20e+01")
  )
  expect_identical(
    util_round_to_decimal_places(c(10000, -2), digits = 2),
    c("1.00e+04", "-2.00e+00")
  )
})

test_that("util_round_to_decimal_places validates inputs", {
  expect_error(
    util_round_to_decimal_places("1"),
    "Argument x must be numeric",
    fixed = TRUE
  )
  expect_error(
    util_round_to_decimal_places(1, digits = c(2, 3)),
    "Need exactly one element in argument digits",
    fixed = TRUE
  )
})
