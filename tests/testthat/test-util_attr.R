test_that("util_attr reads attributes exactly by default", {
  x <- structure(1, study_data = "partial", study = "exact")

  expect_equal(util_attr(x, "study"), "exact")
  expect_null(util_attr(x, "study_d"))
})

test_that("util_attr still allows explicit partial matching", {
  x <- structure(1, study_data = "partial")

  expect_null(util_attr(x, "study"))
  expect_equal(util_attr(x, "study", exact = FALSE), "partial")
})

test_that("util_attr has exact TRUE as a visible default", {
  expect_true(identical(formals(util_attr)$exact, TRUE))
})
