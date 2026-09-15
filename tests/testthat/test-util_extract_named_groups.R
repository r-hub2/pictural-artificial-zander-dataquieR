skip_on_cran()

test_that("util_extract_named_groups returns capture columns for each input", {
  result <- util_extract_named_groups(
    "^(?<row>[0-9]+),(?<col>[A-Z]+)$",
    c("12,AB", "not a coordinate")
  )

  expect_s3_class(result, "data.frame")
  expect_identical(names(result), c("row", "col"))
  expect_identical(result$row, c("12", NA_character_))
  expect_identical(result$col, c("AB", NA_character_))
})

test_that("util_extract_named_groups handles a single input string", {
  result <- util_extract_named_groups(
    "^item-(?<id>[0-9]+)$",
    "item-12"
  )

  expect_s3_class(result, "data.frame")
  expect_identical(result$id, "12")
})

test_that("util_extract_named_groups keeps optional groups as missing", {
  result <- util_extract_named_groups(
    "^(?<prefix>[A-Z]+)(-(?<suffix>[0-9]+))?$",
    c("ABC", "ABC-123")
  )

  expect_s3_class(result, "data.frame")
  expect_identical(result$prefix, c("ABC", "ABC"))
  expect_identical(result$suffix, c("", "123"))
})

test_that(
  "util_extract_named_groups returns an empty data frame without groups",
  {
    result <- util_extract_named_groups("plain text", "plain text")

    expect_s3_class(result, "data.frame")
    expect_identical(dim(result), c(0L, 0L))
  }
)
