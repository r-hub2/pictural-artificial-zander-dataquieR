skip_on_cran()

test_that("util_all_is_integer summarizes integer-like vectors", {
  expect_true(util_all_is_integer(c(1, 2, 3)))
  expect_false(util_all_is_integer(c(1, 2.5)))
  expect_false(util_all_is_integer(NA))
})

test_that("util_is_integer handles numeric and non-numeric boundaries", {
  skip_on_cran()

  expect_equal(
    util_is_integer(c(1, 1 + .Machine$double.eps, 1.25, NaN, NA_real_)),
    c(TRUE, TRUE, FALSE, FALSE, TRUE)
  )
  expect_equal(util_is_integer(c("1", "2")), c(FALSE, FALSE))
  expect_equal(util_is_integer(list(1, 2)), c(FALSE, FALSE))
})

test_that("prep_get_variant returns the active R major-minor variant", {
  expect_identical(
    prep_get_variant(),
    paste0("R", getRversion()[, 1:2])
  )
})

test_that("util_get_labels_grading_class maps grading categories to labels", {
  labels <- util_get_labels_grading_class()

  expect_named(labels, as.character(seq_along(labels)))
  expect_true(all(nzchar(labels)))

  testthat::local_mocked_bindings(
    util_get_ruleset_formats = function(...) {
      data.frame(
        category = c(1L, 2L),
        label = c("Low", "High")
      )
    }
  )
  expect_identical(
    util_get_labels_grading_class(),
    c(`1` = "Low", `2` = "High")
  )
})
