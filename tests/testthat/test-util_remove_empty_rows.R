test_that("util_remove_empty_rows works", {
  skip_on_cran()

  result <- util_remove_empty_rows(cars)

  # Check that the result has the correct structure
  expect_s3_class(result, "data.frame")

  # Check that the result has the correct values when there are no empty rows
  expect_identical(result, cars)

  # Check that the result has the correct number of rows when there are empty rows # nolint: line_length_linter.
  # Set a seed for reproducibility
  set.seed(123)

  # Generate random indices to replace with empty string
  n_empty_strings <- 10
  indices_to_replace1 <- sample(seq_along(cars$dist), size = n_empty_strings)

  # Replace values at selected indices with empty string
  cars_na <- cars
  cars_na$dist[indices_to_replace1] <- ""
  cars_na$speed[indices_to_replace1] <- ""

  result_na <- util_remove_empty_rows(cars_na)

  expect_equal(nrow(result_na), nrow(cars) - n_empty_strings)
})

test_that(
  "util_remove_empty_rows ignores id variables when detecting empty rows",
  {
    skip_on_cran()

    x <- data.frame(
      id = c("a", "b", "c", "d"),
      value = c("", "kept", NA, " "),
      note = c(NA, "", "also kept", " "),
      stringsAsFactors = FALSE
    )

    result <- util_remove_empty_rows(x, id_vars = "id")

    expect_equal(result$id, c("b", "c"))
    expect_equal(result$value, c("kept", NA))
    expect_equal(result$note, c("", "also kept"))
  }
)

test_that("util_remove_empty_rows validates inputs", {
  skip_on_cran()

  expect_error(
    util_remove_empty_rows(data.frame(id = "a"), id_vars = 1),
    "must be character"
  )
  expect_error(
    util_remove_empty_rows(list(id = "a")),
    "data frame"
  )
})
