test_that("util_as_numeric works", {
  skip_on_cran()
  a <- dataquieR:::util_as_numeric(1:10,
    warn = TRUE
  )
  b <- dataquieR:::util_as_numeric(as.character(1:10),
    warn = TRUE
  )
  c <- dataquieR:::util_as_numeric(
    factor(1:10,
      levels = 10:1,
      labels = as.character(1:10)
    ),
    warn = TRUE
  )
  expect_warning(
    d <- dataquieR:::util_as_numeric(
      factor(1:10,
        levels = 10:1,
        labels =
          paste("Grade", as.character(1:10))
      ),
      warn = TRUE
    ),
    regexp = paste(
      "Could not convert .+Grade 10.+, .+Grade 9.+, .+Grade 8.+,",
      ".+Grade 7.+, .+Grade 6.+, ... to numeric values"
    ),
    perl = TRUE
  )

  e <- dataquieR:::util_as_numeric(
    ordered(1:10,
      levels = 10:1,
      labels = as.character(1:10)
    ),
    warn = TRUE
  )
  expect_warning(
    f <- dataquieR:::util_as_numeric("not-a-number",
      warn = "Custom conversion warning for %s"
    ),
    regexp = "Custom conversion warning"
  )

  expect_equal(a, 1:10)
  expect_equal(b, 1:10)
  expect_equal(c, 10:1)
  expect_equal(d, rep(NA_real_, 10))
  expect_equal(e, 10:1)
  expect_true(is.na(f))
})

test_that("util_as_numeric treats missing and blank input as non-errors", {
  skip_on_cran()

  expect_warning(
    converted <- dataquieR:::util_as_numeric(c(NA, "", "  ", "3")),
    NA
  )

  expect_identical(converted, c(NA_real_, NA_real_, NA_real_, 3))
})
