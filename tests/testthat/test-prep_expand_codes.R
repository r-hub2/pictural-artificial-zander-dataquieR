test_that("prep_expand_codes fills automatic labels within code classes", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("a", "b"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = c("-1 = refused", "-1"),
    JUMP_LIST = c(SPLIT_CHAR, SPLIT_CHAR),
    stringsAsFactors = FALSE
  )

  expect_message(
    expanded <- prep_expand_codes(meta_data),
    "Expand label"
  )

  expect_identical(expanded[[MISSING_LIST]], c(
    "-1 = refused",
    "-1 = refused"
  ))
  expect_identical(expanded[[JUMP_LIST]], c(SPLIT_CHAR, SPLIT_CHAR))
})

test_that("prep_expand_codes can mix missing and jump labels", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("a", "b"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = c("-1 = refused", SPLIT_CHAR),
    JUMP_LIST = c(SPLIT_CHAR, "-1"),
    stringsAsFactors = FALSE
  )

  separate <- prep_expand_codes(meta_data)
  expect_message(
    mixed <- prep_expand_codes(meta_data, mix_jumps_and_missings = TRUE),
    "Expand label"
  )

  expect_identical(separate[[JUMP_LIST]], c(SPLIT_CHAR, "-1 = JUMP -1"))
  expect_identical(mixed[[JUMP_LIST]], c(SPLIT_CHAR, "-1 = refused"))
})
