test_that("util_suppress_warnings muffles selected warning classes only", {
  skip_on_cran()

  custom_warning <- structure(
    list(message = "custom warning", call = NULL),
    class = c("custom_warning", "warning", "condition")
  )

  expect_silent(
    util_suppress_warnings(
      rlang::cnd_signal(custom_warning),
      classes = "custom_warning"
    )
  )

  expect_warning(
    util_suppress_warnings(
      warning("ordinary warning", call. = FALSE),
      classes = "custom_warning"
    ),
    "ordinary warning"
  )
})

test_that("util_suppress_graphics returns values and closes its device", {
  skip_on_cran()

  before <- grDevices::dev.list()

  expect_identical(
    util_suppress_graphics({
      plot.new()
      "done"
    }),
    "done"
  )

  expect_identical(grDevices::dev.list(), before)
})

test_that("util_hide_file_windows is a no-op outside Windows", {
  skip_on_cran()
  skip_if(.Platform$OS.type == "windows")

  file <- tempfile()
  writeLines("visible", file)

  expect_null(util_hide_file_windows(file))
  expect_true(file.exists(file))
})
