test_that("transient URL import errors are retried", {
  skip_on_cran()
  withr::local_options(list(
    dataquieR.url_import_attempts = 3,
    dataquieR.url_import_retry_delay = 0
  ))
  state <- new.env(parent = emptyenv())
  state$calls <- 0L
  import <- function() {
    state$calls <- state$calls + 1L
    if (state$calls < 3) {
      return(try(stop("Proxy CONNECT aborted due to timeout"), silent = TRUE))
    }
    data.frame(value = 1)
  }

  result <- suppressMessages(.util_rio_import_with_retry(
    "https://example.org/data.csv",
    import
  ))

  expect_s3_class(result, "data.frame")
  expect_equal(state$calls, 3L)
})

test_that("non-transient and local import errors are not retried", {
  skip_on_cran()
  withr::local_options(list(
    dataquieR.url_import_attempts = 3,
    dataquieR.url_import_retry_delay = 0
  ))
  state <- new.env(parent = emptyenv())
  state$calls <- 0L
  import <- function() {
    state$calls <- state$calls + 1L
    try(stop("invalid file format"), silent = TRUE)
  }

  remote_result <- .util_rio_import_with_retry(
    "https://example.org/data.csv",
    import
  )
  local_result <- .util_rio_import_with_retry("data.csv", import)

  expect_true(util_is_try_error(remote_result))
  expect_true(util_is_try_error(local_result))
  expect_equal(state$calls, 2L)
})

test_that("transient URL import errors stop after the configured attempts", {
  skip_on_cran()
  withr::local_options(list(
    dataquieR.url_import_attempts = 3,
    dataquieR.url_import_retry_delay = 0
  ))
  state <- new.env(parent = emptyenv())
  state$calls <- 0L
  import <- function() {
    state$calls <- state$calls + 1L
    try(stop("Timeout was reached"), silent = TRUE)
  }

  result <- suppressMessages(.util_rio_import_with_retry(
    "https://example.org/data.csv",
    import
  ))

  expect_true(util_is_try_error(result))
  expect_equal(state$calls, 3L)
})

test_that("URL import retry normalizes invalid retry configuration", {
  skip_on_cran()
  withr::local_options(list(
    dataquieR.url_import_attempts = c(2L, 3L),
    dataquieR.url_import_retry_delay = -1
  ))
  state <- new.env(parent = emptyenv())
  state$calls <- 0L
  import <- function() {
    state$calls <- state$calls + 1L
    try(stop("HTTP error 503"), silent = TRUE)
  }

  result <- suppressWarnings(.util_rio_import_with_retry(
    "https://example.org/data.csv",
    import
  ))

  expect_true(util_is_try_error(result))
  expect_equal(state$calls, 1L)
})

test_that("URL import retry normalizes scalar text retry options", {
  skip_on_cran()
  withr::local_options(list(
    dataquieR.url_import_attempts = "invalid",
    dataquieR.url_import_retry_delay = "invalid"
  ))
  state <- new.env(parent = emptyenv())
  state$calls <- 0L
  import <- function() {
    state$calls <- state$calls + 1L
    try(stop("HTTP error 503"), silent = TRUE)
  }

  result <- suppressWarnings(.util_rio_import_with_retry(
    "https://example.org/data.csv",
    import
  ))

  expect_true(util_is_try_error(result))
  expect_equal(state$calls, 1L)
})

test_that("transient URL import error detection handles plain try-errors", {
  skip_on_cran()

  plain_timeout <- structure("Timeout while reading", class = "try-error")
  plain_other <- structure("permanent parse error", class = "try-error")

  expect_true(.util_is_transient_url_import_error(plain_timeout))
  expect_false(.util_is_transient_url_import_error(plain_other))
  expect_false(.util_is_transient_url_import_error(data.frame(x = 1)))
})
