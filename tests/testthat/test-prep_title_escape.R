test_that("prep_title_escape works", {
  skip_on_cran()
  # HTML escaping branch.
  expected_resut <- c("Hello" = "Hello", "world!" = "world!")
  expect_equal(prep_title_escape(c("Hello", "world!"), html = TRUE), expected_resut) # nolint: line_length_linter.

  # Markdown-style escaping branch.
  expect_equal(prep_title_escape("Hello world!", html = FALSE), "`Hello world!`") # nolint: line_length_linter.
  expect_equal(prep_title_escape(c("String1", "String2"), html = FALSE), c("`String1`", "`String2`")) # nolint: line_length_linter.
  expect_equal(prep_title_escape("Test `String`", html = FALSE), "`Test String`") # nolint: line_length_linter.
})
