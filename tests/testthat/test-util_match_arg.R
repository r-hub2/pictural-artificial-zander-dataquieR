test_that("util_match_arg resolves defaults and validates choices", {
  skip_on_cran()

  wrapper <- function(arg = c("alpha", "beta"),
    several_ok = FALSE,
    error = TRUE) {
    util_match_arg(arg, several_ok = several_ok, error = error)
  }

  expect_equal(wrapper(), "alpha")
  expect_equal(wrapper("beta"), "beta")
  expect_equal(wrapper(c("alpha", "beta"), several_ok = TRUE),
    c("alpha", "beta"))

  expect_error(
    wrapper(c("alpha", "beta")),
    "needs exactly one entry"
  )
  expect_error(
    wrapper("alp"),
    "did you mean"
  )
  expect_warning(
    cleaned <- wrapper("alp", error = FALSE),
    "did you mean"
  )
  expect_identical(cleaned, character(0))

  expect_error(
    util_match_arg(),
    "needs the argument"
  )
})

test_that("util_match_arg rejects empty inferred choices", {
  skip_on_cran()

  wrapper <- function(arg = character()) {
    util_match_arg(arg)
  }

  expect_error(
    wrapper(),
    "does not provide any choice"
  )
})
