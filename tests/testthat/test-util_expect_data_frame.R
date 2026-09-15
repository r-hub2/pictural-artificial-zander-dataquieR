skip_on_cran()

test_that("util_expect_data_frame explains basic column type mismatches", {
  x <- data.frame(a = 1, stringsAsFactors = FALSE)

  expect_error(
    util_expect_data_frame(x, list(a = is.character)),
    "Column 'a' in 'x' must be characters",
    fixed = TRUE
  )
})

test_that("util_expect_data_frame reports unresolved data frame names", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  x <- "definitely_missing_data_frame"

  expect_error(
    util_expect_data_frame(x),
    "No data frame found for argument 'x': \"definitely_missing_data_frame\"",
    fixed = TRUE
  )
})

test_that("util_expect_data_frame can report user-facing argument names", {
  env_args <- list(meta_data = "definitely_missing_metadata")

  expect_error(
    util_expect_data_frame(env_args$meta_data, arg_name = "meta_data"),
    "No data frame found for argument 'meta_data': \"definitely_missing_metadata\"", # nolint: line_length_linter.
    fixed = TRUE
  )
})

test_that("util_expect_data_frame can require non-empty data frames", {
  x <- data.frame()

  expect_error(
    util_expect_data_frame(x, min_rows = 1),
    "Need at least 1 rows in 'x', got 0",
    fixed = TRUE
  )

  expect_error(
    util_expect_data_frame(
      x,
      min_rows = 1,
      empty_error_message = "No rows available"
    ),
    "No rows available",
    fixed = TRUE
  )
})

test_that("util_expect_data_frame amends empty frames with required columns", {
  skip_on_cran()

  x <- data.frame(existing = character())
  result <- util_expect_data_frame(x, c("existing", "added"))

  expect_identical(names(result), c("existing", "added"))
  expect_identical(names(x), c("existing", "added"))

  y <- data.frame(existing = character())
  util_expect_data_frame(y, c("existing", "added"),
    dont_assign = TRUE)
  expect_identical(names(y), "existing")
})

test_that("util_expect_data_frame converts columns when possible", {
  skip_on_cran()

  x <- data.frame(a = "1", stringsAsFactors = FALSE)
  result <- util_expect_data_frame(
    x,
    col_names = list(a = is.numeric),
    convert_if_possible = list(a = as.numeric)
  )

  expect_equal(result$a, 1)
  expect_equal(x$a, 1)
})

test_that("util_expect_data_frame uses custom mismatch messages", {
  skip_on_cran()

  x <- data.frame(a = 1, stringsAsFactors = FALSE)

  expect_error(
    util_expect_data_frame(
      x,
      col_names = list(a = is.character),
      custom_errors = list(a = "Column a has the wrong semantic type")
    ),
    "wrong semantic type"
  )
})

test_that("util_expect_data_frame validates named list controls", {
  skip_on_cran()

  x <- data.frame(a = 1, stringsAsFactors = FALSE)

  expect_error(
    util_expect_data_frame(x, col_names = list(is.numeric)),
    "col_names"
  )
  expect_error(
    util_expect_data_frame(x, col_names = "a",
      convert_if_possible = list(as.numeric)),
    "convert_if_possible"
  )
  expect_error(
    util_expect_data_frame(x, col_names = "a",
      convert_if_possible = TRUE),
    "convert_if_possible"
  )
})

test_that("util_expect_data_frame reports conversion and predicate details", {
  skip_on_cran()

  must_be_flag <- function(x) identical(x, "flag")
  attr(must_be_flag, "error_msg") <- "be the literal flag"

  x <- data.frame(a = "other", stringsAsFactors = FALSE)
  expect_error(
    util_expect_data_frame(x, col_names = list(a = must_be_flag)),
    "be the literal flag"
  )

  x <- data.frame(a = c("1", "bad"), stringsAsFactors = FALSE)
  warning_env <- new.env(parent = emptyenv())
  warning_env$messages <- character(0)
  expect_error(
    withCallingHandlers(
      util_expect_data_frame(
        x,
        col_names = list(a = is.numeric),
        convert_if_possible = list(a = as.numeric)
      ),
      warning = function(w) {
        warning_env$messages <- c(warning_env$messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    "Column 'a' in 'x' must be numeric",
    fixed = TRUE
  )
  expect_true(any(grepl("could not convert the whole column",
        warning_env$messages, fixed = TRUE)))
})

test_that("util_expect_data_frame rejects malformed predicate returns", {
  skip_on_cran()

  x <- data.frame(a = 1, stringsAsFactors = FALSE)
  x_two_rows <- data.frame(a = 1:2, stringsAsFactors = FALSE)
  vector_predicate <- function(x) c(TRUE, FALSE)
  missing_predicate <- function(x) NA

  expect_error(
    util_expect_data_frame(x, col_names = list(a = vector_predicate)),
    "lambda 'a' did not return",
    fixed = TRUE
  )
  expect_error(
    util_expect_data_frame(x_two_rows, col_names = list(a = missing_predicate)),
    "lambda 'a' did not return",
    fixed = TRUE
  )
})

test_that("util_expect_data_frame validates control arguments", {
  skip_on_cran()

  x <- data.frame(a = 1, stringsAsFactors = FALSE)

  expect_error(
    util_expect_data_frame(x, keep_types = NA),
    "keep_types"
  )
  expect_error(
    util_expect_data_frame(x, keep_encoding_errors = NA),
    "keep_encoding_errors"
  )
  expect_error(
    util_expect_data_frame(x, min_rows = -1),
    "min_rows"
  )
  expect_error(
    util_expect_data_frame(x, col_names = list(a = is.character),
      custom_errors = "wrong"),
    "custom_errors"
  )
})

test_that(
  "util_expect_predicate_message names common predicates and fallbacks",
  {
    skip_on_cran()

    expect_identical(util_expect_predicate_message(is.character),
      "be characters")
    expect_identical(util_expect_predicate_message(is.integer),
      "be integer numbers")
    expect_identical(util_expect_predicate_message(is.double),
      "be floating point")
    expect_identical(util_expect_predicate_message(is.numeric),
      "be numeric")
    expect_identical(util_expect_predicate_message(is.logical),
      "be logical")

    must_be_id <- function(x) all(grepl("^ID", x))
    expect_match(
      util_expect_predicate_message(must_be_id),
      "match the predicate"
    )
  }
)
