skip_on_cran()

test_that("explicit drop audit finds only multidimensional omissions", {
  fixture <- tempfile(fileext = ".R")
  writeLines(c(
    "one_dimensional <- x[i]",
    "list_element <- x[[i]]",
    "missing_drop <- x[i, j]",
    "kept_dataframe <- x[i, j, drop = FALSE]",
    "intentional_vector <- x[i, j, drop = TRUE]",
    "report_subset <- report[i, j, as_raw = TRUE]",
    "replacement_subset[, j] <- value"
  ), fixture)

  findings <- find_missing_drop_arguments(fixture)

  expect_equal(nrow(findings), 1L)
  expect_equal(findings$line, 3L)
  expect_identical(findings$code, "x[i, j]")
})
