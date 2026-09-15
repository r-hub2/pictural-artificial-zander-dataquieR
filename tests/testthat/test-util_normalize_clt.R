test_that("util_normalize_clt distributes code-list tables by code class", {
  skip_on_cran()

  cache <- new.env(parent = emptyenv())
  result <- with_dataframe_environment(quote({
    value_codes <- data.frame(
      code_value = "1",
      code_class = "VALUE",
      stringsAsFactors = FALSE
    )
    missing_codes <- data.frame(
      code_value = "-1",
      code_class = "MISSING",
      stringsAsFactors = FALSE
    )
    mixed_codes <- data.frame(
      code_value = c("1", "-1"),
      code_class = c("VALUE", "MISSING"),
      stringsAsFactors = FALSE
    )
    colnames(value_codes) <- c(CODE_VALUE, CODE_CLASS)
    colnames(missing_codes) <- c(CODE_VALUE, CODE_CLASS)
    colnames(mixed_codes) <- c(CODE_VALUE, CODE_CLASS)
    prep_add_data_frames(value_codes, missing_codes, mixed_codes)

    meta_data <- data.frame(
      var_names = c("a", "b", "c", "d"),
      code_list_table = c("value_codes", "missing_codes", "mixed_codes", NA),
      value_label_table = c("old_vlt", NA, NA, NA),
      missing_list_table = c(NA, "old_mlt", NA, NA),
      stringsAsFactors = FALSE
    )
    colnames(meta_data) <- c(VAR_NAMES, CODE_LIST_TABLE, VALUE_LABEL_TABLE,
      MISSING_LIST_TABLE)

    suppressMessages(util_normalize_clt(meta_data))
  }), env = cache)

  expect_false(CODE_LIST_TABLE %in% names(result))
  expect_equal(result[[VALUE_LABEL_TABLE]], c("value_codes", NA, "mixed_codes",
      NA))
  expect_equal(result[[MISSING_LIST_TABLE]], c(NA, "missing_codes",
      "mixed_codes", NA))
})

test_that(
  "util_normalize_clt leaves metadata without code-list tables unchanged",
  {
    skip_on_cran()

    meta_data <- data.frame(var_names = "x", stringsAsFactors = FALSE)
    colnames(meta_data) <- VAR_NAMES

    expect_identical(util_normalize_clt(meta_data), meta_data)
  }
)

test_that("util_normalize_clt ignores empty and unavailable code-list tables", {
  skip_on_cran()

  cache <- new.env(parent = emptyenv())
  result <- with_dataframe_environment(quote({
    classless_codes <- data.frame(
      code_value = "1",
      code_label = "yes",
      stringsAsFactors = FALSE
    )
    colnames(classless_codes) <- c(CODE_VALUE, CODE_LABEL)
    prep_add_data_frames(classless_codes)

    meta_data <- data.frame(
      var_names = c("empty", "missing", "classless"),
      code_list_table = c("", "unknown_codes", "classless_codes"),
      value_label_table = NA_character_,
      missing_list_table = NA_character_,
      stringsAsFactors = FALSE
    )
    colnames(meta_data) <- c(
      VAR_NAMES,
      CODE_LIST_TABLE,
      VALUE_LABEL_TABLE,
      MISSING_LIST_TABLE
    )

    util_normalize_clt(meta_data)
  }), env = cache)

  expect_false(CODE_LIST_TABLE %in% names(result))
  expect_equal(result[[VALUE_LABEL_TABLE]], c(
    NA,
    NA,
    "classless_codes"
  ))
  expect_equal(result[[MISSING_LIST_TABLE]], rep(NA_character_, 3L))
})
