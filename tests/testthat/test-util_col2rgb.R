skip_on_cran()

test_that("util_col2rgb converts named, hex, and numeric colors", {
  expect_identical(
    util_col2rgb(c("red", "#112233", "255 0 0", "0 128 255 64")),
    c("#ff0000ff", "#112233ff", "#ff0000", "#0080ff40")
  )
})

test_that("util_col2rgb replaces invalid colors with black", {
  expect_warning(
    named <- util_col2rgb(c("not-a-color", "blue")),
    "No known colors"
  )
  expect_identical(named, c("#000000ff", "#0000ffff"))

  expect_warning(
    numeric <- util_col2rgb(c("999 bad", "1 2")),
    "No known colors"
  )
  expect_identical(numeric, c("#000000", "#000000"))
})
