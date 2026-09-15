skip_on_cran()

test_that("util_get_fg_color chooses readable black or white text", {
  expect_identical(
    util_get_fg_color(c("#ffffff", "#000000", "128 128 128")),
    c("#000000", "#ffffff", "#ffffff")
  )
})

test_that("util_get_colors returns named hex colors from grading formats", {
  colors <- util_get_colors()

  expect_named(colors, as.character(seq_along(colors)))
  expect_true(all(grepl("^#[0-9a-fA-F]{6}$", unname(colors))))
})

test_that("util_get_colors falls back to gray without color column", {
  testthat::local_mocked_bindings(
    util_get_ruleset_formats = function(...) {
      data.frame(category = c(1L, 2L))
    }
  )

  expect_identical(
    util_get_colors(),
    c(`1` = "#808080", `2` = "#808080")
  )
})
