test_that("util_empty works", {
  skip_on_cran()
  expect_identical(util_empty(c()), logical(0))
  expect_identical(util_empty(c()), logical(0))
  expect_identical(util_empty(c("", "k")), c(TRUE, FALSE))
  expect_identical(util_empty(c("", "k", NA)), c(TRUE, FALSE, TRUE))
  expect_identical(util_empty(NA), TRUE)
  expect_identical(util_empty(Inf), FALSE)
  expect_identical(util_empty(NaN), TRUE)
  expect_identical(util_empty(0), FALSE)
  expect_identical(util_empty(list("", 1, " ")), c(TRUE, FALSE, TRUE))
  expect_identical(suppressWarnings(util_empty(new.env())), logical(0))
})

test_that("util_empty falls back to elementwise trimming", {
  skip_on_cran()

  assign(
    "as.character.util_empty_whole_trim_failure",
    function(x, ...) {
      stop("whole trim fails")
    },
    envir = globalenv()
  )
  withr::defer(
    rm("as.character.util_empty_whole_trim_failure", envir = globalenv())
  )

  x <- structure(list(" ", "x"), class = "util_empty_whole_trim_failure")

  expect_identical(util_empty(x), c(TRUE, FALSE))
})

test_that("util_empty keeps non-trimmable elements non-empty", {
  skip_on_cran()

  assign(
    "as.character.util_empty_trim_failure",
    function(x, ...) {
      stop("element trim fails")
    },
    envir = globalenv()
  )
  withr::defer(
    rm("as.character.util_empty_trim_failure", envir = globalenv())
  )

  x <- list(
    structure("x", class = "util_empty_trim_failure"),
    " ",
    NA_character_
  )

  expect_identical(suppressWarnings(util_empty(x)), c(FALSE, TRUE, TRUE))
})
