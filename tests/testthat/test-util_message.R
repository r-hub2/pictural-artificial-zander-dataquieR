test_that("util_message signals attributed message conditions", {
  skip_on_cran()

  withr::local_options(dataquieR.testthat_expect_message_active = TRUE)

  msg <- expect_message(
    util_message(
      "message for %s",
      "coverage",
      applicability_problem = TRUE,
      intrinsic_applicability_problem = FALSE,
      additional_classes = "coverage_message",
      varname = "x"
    ),
    "message for coverage"
  )

  expect_s3_class(msg, "coverage_message")
  expect_s3_class(msg, dataquieR.applicability_problem)
  expect_true(util_attr(msg, "applicability_problem", exact = TRUE))
  expect_false(util_attr(msg, "intrinsic_applicability_problem", exact = TRUE))
  expect_equal(util_attr(msg, "varname", exact = TRUE), "x")
})
