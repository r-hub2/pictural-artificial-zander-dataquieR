test_that("util_call_with_only_existing_formals drops unsupported arguments", {
  skip_on_cran()

  f <- function(a, b = 2) {
    a + b
  }

  expect_equal(
    util_call_with_only_existing_formals(f, a = 1, b = 3, ignored = 100),
    4
  )
})

test_that("util_call_with_only_existing_formals preserves dots-capable calls", {
  skip_on_cran()

  f <- function(a, ...) {
    list(a = a, dots = list(...))
  }

  result <- util_call_with_only_existing_formals(
    f,
    a = "kept",
    extra = "forwarded"
  )

  expect_equal(result$a, "kept")
  expect_equal(result$dots$extra, "forwarded")
})

test_that("util_call_with_only_existing_formals evaluates in caller frame", {
  skip_on_cran()

  wrapper <- function() {
    local_value <- "visible"
    util_call_with_only_existing_formals(function() local_value,
      ignored = "dropped")
  }

  expect_equal(wrapper(), "visible")
})
