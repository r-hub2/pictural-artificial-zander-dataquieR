test_that("util_remove_na_records works", {
  skip_on_cran()

  result <- util_remove_na_records(cars)

  # Check that the result has the correct structure
  expect_s3_class(result, "data.frame")

  # Check that the result has the correct values when there are no NAs
  expect_identical(result, cars)

  # Check that the result shows a message when NAs where removed
  # Set a seed for reproducibility
  set.seed(123)

  # Generate random indices to replace with NA
  indices_to_replace1 <- sample(seq_along(cars$dist), size = 10)
  indices_to_replace2 <- sample(seq_along(cars$dist), size = 10)

  # Replace values at selected indices with NA
  cars_na <- cars
  cars_na$dist[indices_to_replace1] <- NA
  cars_na$speed[indices_to_replace2] <- NA


  # Find how many NAs are left
  # Use rowSums(is.na(...)) locally to count rows with remaining NA values.

  expect_message2(result_remove_na <- util_remove_na_records(cars_na),
    regexp = ". observations because of NAs in some of the following columns.",
    perl = TRUE
  )
})

test_that("util_remove_na_records keeps known study-data attributes", {
  skip_on_cran()

  study_data <- data.frame(
    id = 1:3,
    value = c(10, NA, 30),
    keep = c(NA, 2, 3)
  )
  attr(study_data, "study_data_name") <- "study_data"
  attr(study_data, "df_code") <- "df1"

  result <- suppressMessages(util_remove_na_records(study_data, "value"))

  expect_equal(result$id, c(1L, 3L))
  expect_equal(result$keep, c(NA_real_, 3))
  expect_identical(attr(result, "study_data_name"), "study_data")
  expect_identical(attr(result, "df_code"), "df1")
})
