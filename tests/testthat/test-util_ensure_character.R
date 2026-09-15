test_that("util_ensure_character works", {
  skip_on_cran()
  # Check that the result is a character
  expect_type(util_ensure_character(666), "character")

  # Check that the result is as expected
  expect_equal(util_ensure_character(c(666, "abc", pi)), c("666", "abc", "3.14159265358979")) # nolint: line_length_linter.

  # Check error messages
  ID <- c("111", "222", "333", "444")
  age <- c(23, 41, NA, "7a")
  df <- data.frame(ID, age)

  expect_error(util_ensure_character(df, TRUE), ".*was not possible for all of its values.*") # nolint: line_length_linter.
  expect_error(util_ensure_character(df, TRUE, "My error message"), "My error message") # nolint: line_length_linter.
})

test_that("util_ensure_character warns on lossy conversions", {
  skip_on_cran()

  df <- data.frame(ID = c("111", "222"), Age = c(23, NA))

  expect_warning(
    expect_identical(
      util_ensure_character(df),
      c("c(\"111\", \"222\")", "c(23, NA)")
    ),
    "conversion of"
  )

  expect_warning(
    expect_identical(
      util_ensure_character(df,
        error = FALSE,
        error_msg = "Could not convert %s", "df"
      ),
      c("c(\"111\", \"222\")", "c(23, NA)")
    ),
    "Could not convert"
  )
})

test_that("util_ensure_character validates diagnostic arguments", {
  skip_on_cran()

  expect_error(
    util_ensure_character(1, error = c(TRUE, FALSE)),
    "Need exactly one element"
  )
  expect_error(
    util_ensure_character(1, error_msg = 1),
    "must be character"
  )
})
