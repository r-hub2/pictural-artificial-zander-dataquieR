skip_on_cran()

test_that("util_expect_scalar converts and reassigns scalar arguments", {
  f <- function(x) {
    util_expect_scalar(
      x,
      check_type = is.integer,
      convert_if_possible = as.integer
    )
    x
  }

  expect_identical(f(2), 2L)
})

test_that("util_expect_scalar can validate without assignment", {
  f <- function(x) {
    util_expect_scalar(
      x,
      check_type = is.integer,
      convert_if_possible = as.integer,
      dont_assign = TRUE
    )
    x
  }

  expect_identical(f(2), 2)
})

test_that("util_expect_scalar reports controlled developer and user errors", {
  f <- function(x) util_expect_scalar(x, check_type = is.character)

  expect_error(f(NULL), "Argument x is NULL")
  expect_error(f(character()), "Need exactly one element")
  expect_error(f(c("a", "b")), "Need exactly one element")

  expect_error(
    {
      x <- matrix(1:4, nrow = 2)
      util_expect_scalar(x, allow_more_than_one = TRUE,
        check_type = is.numeric)
    },
    "object with 2 dimensions"
  )

  expect_error(
    {
      x <- "a"
      util_expect_scalar(x, check_type = "not a function")
    },
    "Need a lambda function"
  )

  expect_error(
    {
      x <- "a"
      util_expect_scalar(x, check_type = function(.) c(TRUE, FALSE))
    },
    "function returned"
  )

  expect_error(
    {
      x <- "a"
      util_expect_scalar(x, check_type = function(.) NA)
    },
    "function returned"
  )
})

test_that("util_expect_scalar handles conversion warnings around NAs", {
  f_replace_na <- function(x) {
    util_expect_scalar(
      x,
      allow_more_than_one = TRUE,
      allow_na = TRUE,
      check_type = is.integer,
      convert_if_possible = function(.) c(1L, NA_integer_),
      conversion_may_replace_NA = TRUE
    )
  }

  expect_error(
    expect_warning(f_replace_na(c(NA, 2)), "conversion introduced NAs"),
    "must be integer"
  )

  f_lossy_conversion <- function(x) {
    util_expect_scalar(
      x,
      allow_more_than_one = TRUE,
      allow_na = TRUE,
      check_type = is.integer,
      convert_if_possible = function(.) c(1L, NA_integer_)
    )
  }

  expect_error(
    expect_warning(f_lossy_conversion(c("1", "bad")), "could not convert"),
    "must be integer"
  )
})

test_that("util_expect_scalar validates length ranges and custom messages", {
  f <- function(x) {
    util_expect_scalar(
      x,
      allow_more_than_one = TRUE,
      min_length = 2,
      max_length = 3,
      check_type = is.character
    )
  }

  expect_error(f("a"), "must have a length")
  expect_equal(f(c("a", "b")), c("a", "b"))

  expect_error(
    {
      x <- 1
      util_expect_scalar(x, check_type = is.character,
        error_message = "custom scalar failure")
    },
    "custom scalar failure"
  )
})

test_that("util_expect_scalar validates optional guardrails", {
  skip_on_cran()

  f_null <- function(x = NULL) {
    util_expect_scalar(x, allow_null = TRUE)
  }
  expect_null(f_null())

  f_empty_vector <- function(x) {
    util_expect_scalar(x, allow_more_than_one = TRUE)
  }
  expect_error(
    f_empty_vector(character()),
    "Need at least one element"
  )

  expect_error(
    {
      x <- 1
      util_expect_scalar(x, dont_assign = NA)
    },
    "dont_assign"
  )

  expect_error(
    {
      x <- 1
      util_expect_scalar(
        x,
        check_type = is.character,
        convert_if_possible = "as.character"
      )
    },
    "convert_if_possible"
  )
})

test_that("util_expect_scalar covers remaining scalar validation branches", {
  skip_on_cran()

  f_missing <- function(x) {
    util_expect_scalar(x, allow_null = TRUE)
  }
  expect_warning(
    expect_null(f_missing()),
    "Missing argument"
  )

  expect_error(
    {
      x <- NA
      util_expect_scalar(x, allow_na = FALSE)
    },
    "must not contain NAs"
  )

  expect_error(
    {
      x <- "a"
      util_expect_scalar(x, min_length = "two")
    },
    "Need numeric min_length"
  )
  expect_error(
    {
      x <- "a"
      util_expect_scalar(x, min_length = c(1, 2))
    },
    "Need numeric min_length"
  )

  expect_error(
    {
      x <- "a"
      util_expect_scalar(x, max_length = "two")
    },
    "Need numeric max_length"
  )
  expect_error(
    {
      x <- "a"
      util_expect_scalar(x, max_length = c(1, 2))
    },
    "Need numeric max_length"
  )

  is_known <- function(x) identical(x, "known")
  attr(is_known, "error_msg") <- "be a known scalar"
  expect_error(
    {
      x <- "unknown"
      util_expect_scalar(x, check_type = is_known)
    },
    "be a known scalar"
  )

  f_converted_with_na <- function(x) {
    util_expect_scalar(
      x,
      allow_more_than_one = TRUE,
      allow_na = TRUE,
      check_type = is.integer,
      convert_if_possible = as.integer,
      conversion_may_replace_NA = TRUE
    )
    x
  }
  expect_identical(f_converted_with_na(c("1", NA)), c(1L, NA_integer_))
})

test_that("util_expect_scalar reports developer misuse guardrails", {
  skip_on_cran()

  expect_error(
    util_expect_scalar(c("x", "y")),
    "argument arg_name must be of length 1"
  )
  expect_error(
    util_expect_scalar("not_in_parent"),
    "Unknown function argument"
  )
  expect_error(
    {
      x <- 1L
      util_expect_scalar(x, check_type = is.double)
    },
    "floating point"
  )
})
