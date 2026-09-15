test_that("prep_deparse_assignments handles labels and bare codes", {
  skip_on_cran()

  expect_equal(
    prep_deparse_assignments(codes = c(1, 2), labels = c("no", "yes")),
    "1 = no | 2 = yes"
  )
  expect_equal(
    prep_deparse_assignments(codes = c(1, 2)),
    "1 | 2"
  )
  expect_equal(
    prep_deparse_assignments(codes = c(1, 2), labels = character(0)),
    "1 | 2"
  )
})

test_that("prep_deparse_assignments validates and normalizes edge inputs", {
  skip_on_cran()

  expect_error(
    prep_deparse_assignments(codes = c(1, 2), labels = "one"),
    "same length"
  )
  expect_error(
    prep_deparse_assignments(codes = c("a", "b")),
    "finite numeric or date/time"
  )
  expect_equal(
    prep_deparse_assignments(codes = c("a", "b"), mode = "string_codes"),
    "a | b"
  )
  expect_equal(
    prep_deparse_assignments(codes = list(1, 2), labels = list("one", "two")),
    "1 = one | 2 = two"
  )
  expect_message(
    result <- prep_deparse_assignments(codes = 1, labels = "a | b"),
    "Removed seperator characters"
  )
  expect_equal(result, "1 = a  b")
  expect_equal(prep_deparse_assignments(codes = numeric(0)), SPLIT_CHAR)
})
