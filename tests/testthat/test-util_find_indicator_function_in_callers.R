test_that("util_find_indicator_function_in_callers finds indicator formals", {
  skip_on_cran()

  lookup <- function() {
    util_find_indicator_function_in_callers("resp_vars")
  }
  acc_shape_or_scale <- function(resp_vars) {
    lookup()
  }

  expect_identical(acc_shape_or_scale(c("height", "weight")),
    c("height", "weight")
  )
})

test_that(
  "util_find_indicator_function_in_callers returns NULL outside indicators",
  {
    skip_on_cran()

    lookup <- function() {
      util_find_indicator_function_in_callers("resp_vars")
    }

    expect_null(lookup())
  }
)

test_that("util_find_indicator_function_in_callers handles missing formals", {
  skip_on_cran()

  lookup <- function() {
    util_find_indicator_function_in_callers("resp_vars")
  }
  acc_shape_or_scale <- function(resp_vars) {
    lookup()
  }

  expect_null(acc_shape_or_scale())
})

test_that("util_find_indicator_function_in_callers forwards real errors", {
  skip_on_cran()

  lookup <- function() {
    util_find_indicator_function_in_callers("resp_vars")
  }
  acc_shape_or_scale <- function(resp_vars) {
    lookup()
  }

  expect_error(acc_shape_or_scale(stop("boom")), "boom")
})

test_that(
  "english-language wrapper evaluates expressions with LC_ALL branches",
  {
    skip_on_cran()

    withr::local_envvar(c(LC_ALL = "C"))
    expect_identical(
      util_with_english_language_if_possible("kept"),
      "kept"
    )

    withr::local_envvar(c(LC_ALL = NA))
    expect_identical(
      util_with_english_language_if_possible({
        paste("still", "evaluated")
      }),
      "still evaluated"
    )
  }
)
