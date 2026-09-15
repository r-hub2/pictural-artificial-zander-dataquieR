test_that("classic parallel helper chunks calls and preserves result names", {
  skip_on_cran()

  calls <- list(
    first = quote(first_indicator()),
    second = quote(second_indicator()),
    third = quote(third_indicator())
  )
  progress_state <- new.env(parent = emptyenv())
  progress_state$values <- numeric()

  testthat::local_mocked_bindings(
    util_parallel_get_options = function() {
      list(settings = list(mode = "local"))
    },
    util_suppress_graphics = function(expr) expr,
    .package = "dataquieR"
  )

  result <- util_parallel_classic(
    all_calls = calls,
    worker = function(expression, nm, function_name, my_call, ...) {
      list(
        expression = as.character(expression[[1]]),
        nm = nm,
        function_name = function_name,
        my_call = as.character(my_call[[1]])
      )
    },
    n_nodes = 2,
    progress = function(value) {
      progress_state$values <- c(progress_state$values, value)
    },
    debug_parallel = FALSE,
    my_storr_object = NULL
  )

  expect_type(result, "list")
  expect_identical(
    unname(vapply(result, `[[`, character(1), "expression")),
    c("first_indicator", "second_indicator", "third_indicator")
  )
  expect_identical(names(result), names(calls))
  expect_identical(
    unname(vapply(result, `[[`, character(1), "nm")),
    names(calls)
  )
  expect_identical(
    unname(vapply(result, `[[`, character(1), "function_name")),
    c("first_indicator", "second_indicator", "third_indicator")
  )
  expect_identical(
    unname(vapply(result, `[[`, character(1), "my_call")),
    c("first_indicator", "second_indicator", "third_indicator")
  )
  expect_identical(progress_state$values, c(50, 100))
})

test_that("classic parallel helper keeps batchtools calls in one row", {
  skip_on_cran()

  calls <- list(
    first = quote(first_indicator()),
    second = quote(second_indicator())
  )
  progress_state <- new.env(parent = emptyenv())
  progress_state$values <- numeric()

  testthat::local_mocked_bindings(
    util_parallel_get_options = function() {
      list(settings = list(mode = "batchtools"))
    },
    util_suppress_graphics = function(expr) expr,
    .package = "dataquieR"
  )

  result <- util_parallel_classic(
    all_calls = calls,
    worker = function(expression, nm, function_name, my_call, ...) {
      list(
        expression = as.character(expression[[1]]),
        nm = nm,
        function_name = function_name,
        my_call = as.character(my_call[[1]])
      )
    },
    n_nodes = 4,
    progress = function(value) {
      progress_state$values <- c(progress_state$values, value)
    },
    debug_parallel = FALSE,
    my_storr_object = NULL
  )

  expect_identical(names(result), names(calls))
  expect_identical(progress_state$values, 100)
})
