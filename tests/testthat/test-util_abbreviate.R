test_that("util_abbreviate shortens indicator-like names", {
  skip_on_cran()

  expect_equal(
    util_abbreviate(c("acc_univariate_outlier", "custom_long_name")),
    c("aUniOut", "customLonNam")
  )
})

test_that("util_abbreviate keeps abbreviations unique for different names", {
  skip_on_cran()

  out <- util_abbreviate(c(
    "acc_univariate_outlier",
    "acc_universal_output",
    "acc_univariate_outlier"
  ))

  expect_equal(out, c("aUniOut", paste0("aUniOut", intToUtf8(176)),
      "aUniOut"))
  expect_identical(duplicated(out), c(FALSE, FALSE, TRUE))
})

test_that("util_abbreviate accepts empty and missing character inputs", {
  skip_on_cran()

  expect_identical(util_abbreviate(character(0)), character(0))
  expect_identical(util_abbreviate(NA_character_), "NA")
  expect_identical(util_abbreviate(c("des_summary", NA_character_)),
    c("dSum", "NA"))
})
