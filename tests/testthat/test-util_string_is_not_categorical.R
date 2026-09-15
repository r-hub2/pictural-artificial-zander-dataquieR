skip_on_cran()

test_that("util_string_is_not_categorical handles empty and invalid input", {
  expect_true(util_string_is_not_categorical(c("", NA_character_)))

  expect_warning(
    result <- util_string_is_not_categorical(1:3),
    "Wrong use"
  )
  expect_false(result)
})

test_that("util_string_is_not_categorical distinguishes labels from text", {
  expect_false(util_string_is_not_categorical(rep(c("yes", "no"), 20)))

  expect_true(util_string_is_not_categorical(
    c("short", paste(rep("longtext", 20), collapse = ""))
  ))

  expect_true(util_string_is_not_categorical(
    c("{\"a\": 1, \"b\": 2}", "<root><value>x</value></root>")
  ))

  expect_true(util_string_is_not_categorical(sprintf("unique_%02d", 1:25)))
})
