test_that("prep_pmap works", {
  skip_on_cran() # deprecated
  df <- data.frame(
    x = c("apple", "banana", "cherry"),
    pattern = c("p", "n", "h"),
    replacement = c("P", "N", "H"),
    stringsAsFactors = FALSE
  )
  expect_equal(
    prep_pmap(df, gsub),
    list(
      "aPPle",
      "baNaNa",
      "cHerry"
    )
  )
  expect_error(
    prep_pmap(df, 42),
    regexp = "Argument .+\\.f.+ should be a function.",
    perl = TRUE
  )
})

test_that("prep_pmap warns for nested parallel setup", {
  skip_on_cran() # deprecated helper

  calls <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    util_parallel_status = function() "running",
    util_parallel_start = function(...) {
      calls$started <- list(...)
      invisible(NULL)
    },
    util_parallel_stop = function() {
      calls$stopped <- TRUE
      invisible(NULL)
    },
    util_parallel_map = function(x, fun, more.args, simplify, use.names,
      show.info, impute.error) {
      list(fun(x[[1]], more.args$suffix))
    }
  )

  expect_warning(
    result <- prep_pmap(
      data.frame(x = "A", stringsAsFactors = FALSE),
      function(x, suffix) paste0(x, suffix),
      suffix = "1",
      cores = 0
    ),
    "encapsulated"
  )

  expect_equal(result, list("A1"))
  expect_equal(calls$started$mode, "local")
})
