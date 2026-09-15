test_that("util_as_numeric_with_unit parses and prints values with units", {
  skip_on_cran()
  skip_if_not_installed("units") # 'units' is Suggests
  skip_if_not_installed("xml2")  # required by units

  value <- suppressMessages(util_as_numeric_with_unit("2 kg"))

  expect_s3_class(value, "numeric_with_unit")
  expect_equal(as.numeric(value), 2)
  expect_equal(util_attr(value, "unit", exact = TRUE), "kg")
  expect_output(print(value), "2 \\[kg\\]")
  expect_error(print.numeric_with_unit(2), "Inadmissible call")
})

test_that("util_as_numeric_with_unit reports parser failures clearly", {
  skip_on_cran()

  testthat::local_mocked_bindings(
    util_unit2baseunit = function(...) stop("unit failure")
  )

  expect_error(
    util_as_numeric_with_unit("1 kg"),
    "Could not parse number with unit"
  )
})

test_that("numeric_with_unit arithmetic preserves or drops units explicitly", {
  skip_on_cran()
  skip_if_not_installed("units") # 'units' is Suggests
  skip_if_not_installed("xml2")  # required by units

  weight <- suppressMessages(util_as_numeric_with_unit("2 kg"))
  more_weight <- suppressMessages(util_as_numeric_with_unit("3 kg"))
  grams <- suppressMessages(util_as_numeric_with_unit("500 g"))

  sum_weight <- weight + more_weight
  expect_s3_class(sum_weight, "numeric_with_unit")
  expect_equal(as.numeric(sum_weight), 5)
  expect_equal(util_attr(sum_weight, "unit", exact = TRUE), "kg")

  ratio <- weight / suppressMessages(util_as_numeric_with_unit("1 kg"))
  expect_false(inherits(ratio, "numeric_with_unit"))
  expect_equal(ratio, 2)

  scaled <- weight * 2
  expect_s3_class(scaled, "numeric_with_unit")
  expect_equal(as.numeric(scaled), 4)
  expect_equal(util_attr(scaled, "unit", exact = TRUE), "kg")

  expect_warning(
    squared <- weight^2,
    "derived units not yet fully supported"
  )
  expect_s3_class(squared, "numeric_with_unit")
  expect_equal(util_attr(squared, "unit", exact = TRUE), "kg^2")

  expect_error(weight + grams, "Cannot calclate kg \\+ g")
})

test_that("numeric_with_unit covers remaining arithmetic branches", {
  skip_on_cran()
  skip_if_not_installed("units") # 'units' is Suggests
  skip_if_not_installed("xml2")  # required by units

  weight <- suppressMessages(util_as_numeric_with_unit("5 kg"))
  smaller_weight <- suppressMessages(util_as_numeric_with_unit("2 kg"))
  distance <- suppressMessages(util_as_numeric_with_unit("2 m"))

  difference <- weight - smaller_weight
  expect_s3_class(difference, "numeric_with_unit")
  expect_equal(as.numeric(difference), 3)
  expect_equal(util_attr(difference, "unit", exact = TRUE), "kg")

  remainder <- weight %% smaller_weight
  expect_s3_class(remainder, "numeric_with_unit")
  expect_equal(as.numeric(remainder), 1)
  expect_equal(util_attr(remainder, "unit", exact = TRUE), "kg")

  integer_ratio <- weight %/% smaller_weight
  expect_false(inherits(integer_ratio, "numeric_with_unit"))
  expect_equal(integer_ratio, 2)

  expect_warning(
    derived <- weight * smaller_weight,
    "derived units not yet fully supported"
  )
  expect_s3_class(derived, "numeric_with_unit")
  expect_equal(as.numeric(derived), 10)
  expect_equal(util_attr(derived, "unit", exact = TRUE), "kg*kg")

  expect_error(weight * distance, "Cannot calclate kg \\* m")
  expect_error(weight ^ distance, "Cannot calclate kg \\^ m")
})

test_that("numeric_with_unit normalizes prefixes and count-like units", {
  skip_on_cran()
  skip_if_not_installed("units") # 'units' is Suggests
  skip_if_not_installed("xml2")  # required by units

  distance <- suppressMessages(util_as_numeric_with_unit("2 km"))
  expect_s3_class(distance, "numeric_with_unit")
  expect_equal(as.numeric(distance), 2000)
  expect_equal(util_attr(distance, "unit", exact = TRUE), "m")

  proportion <- suppressMessages(util_as_numeric_with_unit("50 %"))
  expect_s3_class(proportion, "numeric_with_unit")
  expect_equal(proportion + 1, 1.5)
  expect_equal(1 + proportion, 1.5)
  expect_equal(proportion * 2, 1)
  expect_equal(proportion / 2, 0.25)

  unitless <- structure(3, class = "numeric_with_unit")
  expect_equal(as.numeric(unitless * 2), 6)
  expect_equal(util_attr(unitless * 2, "unit", exact = TRUE), "")
  expect_equal(as.numeric(2 * unitless), 6)
  expect_equal(util_attr(2 * unitless, "unit", exact = TRUE), "")
  expect_equal(unitless + 2, 5)
})

test_that("numeric_with_unit handles one-sided unit arithmetic", {
  skip_on_cran()
  skip_if_not_installed("units") # 'units' is Suggests
  skip_if_not_installed("xml2")  # required by units

  distance <- suppressMessages(util_as_numeric_with_unit("2 m"))

  doubled <- 2 * distance
  expect_s3_class(doubled, "numeric_with_unit")
  expect_equal(as.numeric(doubled), 4)
  expect_equal(util_attr(doubled, "unit", exact = TRUE), "m")

  inverse <- 10 / distance
  expect_s3_class(inverse, "numeric_with_unit")
  expect_equal(as.numeric(inverse), 5)
  expect_equal(util_attr(inverse, "unit", exact = TRUE), "m")

  expect_error(distance + 2, "Cannot calclate m \\+")
  expect_error(2 + distance, "Cannot calclate \\+ m")
})
