test_that("util_get_vars_in_segment returns sorted labels for a segment", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("id", "b", "a", "other"),
    LABEL = c("Identifier", "Bee", "Aye", "Other"),
    STUDY_SEGMENT = c("s1", "s1", "s1", "s2"),
    stringsAsFactors = FALSE
  )

  expect_equal(
    util_get_vars_in_segment("s1", meta_data = meta_data),
    c("Aye", "Bee", "Identifier")
  )
  expect_equal(
    util_get_vars_in_segment("s1", meta_data = meta_data,
      label_col = VAR_NAMES),
    c("a", "b", "id")
  )
})

test_that("util_get_vars_in_segment falls back to variable names as labels", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("x", "y"),
    STUDY_SEGMENT = c("s1", "s2"),
    stringsAsFactors = FALSE
  )

  expect_equal(util_get_vars_in_segment("s1", meta_data = meta_data), "x")
})

test_that("util_get_vars_in_segment validates metadata shape", {
  skip_on_cran()

  expect_error(
    util_get_vars_in_segment("s1", meta_data = data.frame(VAR_NAMES = "x")),
    STUDY_SEGMENT
  )
})
