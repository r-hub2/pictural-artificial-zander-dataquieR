test_that("util_cast_off converts tibbles and preserves known ds1 attributes", {
  skip_on_cran()
  skip_if_not_installed("tibble")

  df <- tibble::tibble(a = 1:2)
  attr(df, "label_col") <- VAR_NAMES
  attr(df, "MAPPED") <- TRUE

  cast <- util_cast_off(df, .dont_cast_off_cols = TRUE)

  expect_s3_class(cast, "data.frame")
  expect_false(inherits(cast, "tbl_df"))
  expect_identical(cast$a, 1:2)
  expect_identical(attr(cast, "label_col", exact = TRUE), VAR_NAMES)
  expect_true(attr(cast, "MAPPED", exact = TRUE))
})

test_that(
  "util_cast_off preserves known ds1 attributes for empty data frames",
  {
    skip_on_cran()

    df <- data.frame(row.names = 1:2)
    attr(df, "normalized") <- TRUE
    attr(df, "version") <- "test"

    cast <- util_cast_off(df)

    expect_s3_class(cast, "data.frame")
    expect_identical(dim(cast), c(2L, 0L))
    expect_true(attr(cast, "normalized", exact = TRUE))
    expect_identical(attr(cast, "version", exact = TRUE), "test")
  }
)

test_that("util_cast_off normalizes ordinary columns and hms list columns", {
  skip_on_cran()
  skip_if_not_installed("hms")

  df <- data.frame(
    group = factor(c("a", "b")),
    score = c("1", "2"),
    stringsAsFactors = FALSE
  )
  attr(df$group, "label") <- "temporary label"

  cast <- util_cast_off(df)

  expect_s3_class(cast$group, "factor")
  expect_identical(attr(cast$group, "label", exact = TRUE), "temporary label")
  expect_identical(cast$score, c("1", "2"))

  with_hms <- data.frame(id = 1:2)
  with_hms$t <- I(list(hms::hms(seconds = 1), hms::hms(seconds = 2)))

  cast_hms <- util_cast_off(with_hms)

  expect_s3_class(cast_hms$t, "hms")
  expect_identical(as.numeric(cast_hms$t), c(1, 2))
})
