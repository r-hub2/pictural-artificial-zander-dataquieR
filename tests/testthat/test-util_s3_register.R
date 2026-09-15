skip_on_cran()

test_that("util_s3_register validates generic strings and missing packages", {
  expect_error(
    util_s3_register("not-a-qualified-generic", "dq_test_class", identity),
    "pkg::generic"
  )

  expect_false(util_s3_register(
    "definitely_missing_package_for_dataquieR_tests::print",
    "dq_test_class",
    identity
  ))
})

test_that("util_s3_register registers available-package methods", {
  cls <- paste0("dq_test_s3_register_", Sys.getpid())
  method <- function(object, ...) "registered"

  expect_true(util_s3_register("stats::predict", cls, method))
  expect_equal(stats::predict(structure(list(), class = cls)), "registered")
})
