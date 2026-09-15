test_that("util_stop_if_not reports labelled internal assertion failures", {
  skip_on_cran()

  expect_null(util_stop_if_not(TRUE))

  expect_error(
    util_stop_if_not(FALSE),
    "Internal error: FALSE is not TRUE",
    fixed = TRUE
  )

  expect_error(
    util_stop_if_not(FALSE, label = "custom check"),
    "Internal error: custom check",
    fixed = TRUE
  )

  expect_error(
    util_stop_if_not(FALSE, label = "custom check", label_only = FALSE),
    "Internal error: custom check: FALSE is not TRUE",
    fixed = TRUE
  )
})

test_that("util_stop_if_not validates its own label arguments", {
  skip_on_cran()

  expect_error(
    util_stop_if_not(TRUE, label = 1),
    "Argument label must be characters"
  )

  expect_error(
    util_stop_if_not(TRUE, label_only = "yes"),
    "Argument label_only must be logical"
  )
})
