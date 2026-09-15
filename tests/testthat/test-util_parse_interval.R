test_that("util_parse_interval parses numeric intervals and fallbacks", {
  skip_on_cran()

  keys <- c("[1;2]", "(1;2)", "[1;)", "[2;1]")
  old_values <- lapply(keys, function(key) {
    if (exists(key, .interval_cache, inherits = FALSE)) {
      get(key, .interval_cache, inherits = FALSE)
    } else {
      NULL
    }
  })
  names(old_values) <- keys
  on.exit({
    for (key in keys) {
      if (exists(key, .interval_cache, inherits = FALSE)) {
        rm(list = key, envir = .interval_cache)
      }
      if (!is.null(old_values[[key]])) {
        assign(key, old_values[[key]], envir = .interval_cache)
      }
    }
  }, add = TRUE)

  closed <- util_parse_interval("[1;2]")
  expect_s3_class(closed, "interval")
  expect_identical(closed$inc_l, TRUE)
  expect_identical(closed$low, 1)
  expect_identical(closed$upp, 2)
  expect_identical(closed$inc_u, TRUE)

  open <- util_parse_interval("(1;2)")
  expect_identical(open$inc_l, FALSE)
  expect_identical(open$inc_u, FALSE)

  open_upper <- util_parse_interval("[1;)")
  expect_identical(open_upper$low, 1)
  expect_identical(open_upper$upp, Inf)

  expect_true(is.na(util_parse_interval("[2;1]")))
  expect_true(is.na(util_parse_interval("")))
  expect_true(exists("[1;2]", .interval_cache, inherits = FALSE))
})

test_that("util_parse_interval uses cached interval results", {
  skip_on_cran()

  key <- "[9;10]"
  had_cached_value <- exists(key, .interval_cache, inherits = FALSE)
  if (had_cached_value) {
    cached_value <- get(key, .interval_cache, inherits = FALSE)
  }
  on.exit({
    if (exists(key, .interval_cache, inherits = FALSE)) {
      rm(list = key, envir = .interval_cache)
    }
    if (had_cached_value) {
      assign(key, cached_value, envir = .interval_cache)
    }
  }, add = TRUE)

  cached <- structure(
    list(inc_l = FALSE, low = 9, upp = 10, inc_u = TRUE),
    class = "interval"
  )
  assign(key, cached, envir = .interval_cache)

  expect_identical(util_parse_interval(key), cached)
})
