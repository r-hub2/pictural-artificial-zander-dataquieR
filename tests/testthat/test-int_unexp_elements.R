test_that("int_unexp_elements compares local dataframe element counts", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())
  prep_add_data_frames(
    df_ok = data.frame(a = 1, b = 2),
    df_extra = data.frame(a = 1, b = 2, c = 3)
  )

  result <- int_unexp_elements(
    identifier_name_list = c("df_ok", "df_extra"),
    data_element_count = c(2, 2)
  )

  expect_equal(
    unname(result$DataframeData[["Unexpected elements"]]),
    c(FALSE, TRUE),
    ignore_attr = TRUE
  )
  expect_equal(
    unname(result$DataframeData[["Number of elements in data"]]),
    c(2, 3),
    ignore_attr = TRUE
  )
  expect_equal(
    unname(result$DataframeData[["Number of elements in metadata"]]),
    c(2, 2),
    ignore_attr = TRUE
  )
  expect_equal(
    unname(result$DataframeData[["Number of mismatches"]]),
    c(0, 1),
    ignore_attr = TRUE
  )
  expect_equal(
    unname(result$DataframeData[["Percentage of mismatches"]]),
    c(0, 50),
    ignore_attr = TRUE
  )
  expect_equal(
    unname(result$DataframeTable$GRADING),
    c(0, 1),
    ignore_attr = TRUE
  )
  expect_equal(
    attr(result$DataframeTable$PCT_int_sts_countel, DATA_TYPE, exact = TRUE),
    DATA_TYPES$FLOAT
  )
})

test_that("int_unexp_elements derives mappings from dataframe metadata", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())
  prep_add_data_frames(
    df_ok = data.frame(a = 1, b = 2),
    df_skip = data.frame(a = 1)
  )
  dataframe_level <- data.frame(
    DF_NAME = c("df_ok", "missing_df", "df_skip"),
    DF_ELEMENT_COUNT = c(2L, 99L, NA_integer_),
    stringsAsFactors = FALSE
  )

  result <- int_unexp_elements(meta_data_dataframe = dataframe_level)

  expect_equal(result$DataframeData[["Data frame"]],
    "df_ok",
    ignore_attr = TRUE
  )
  expect_false(result$DataframeData[["Unexpected elements"]])
  expect_equal(result$DataframeTable$NUM_int_sts_countel,
    0,
    ignore_attr = TRUE
  )
})

test_that("int_unexp_elements rejects ambiguous or incomplete mappings", {
  skip_on_cran()

  expect_error(
    int_unexp_elements(
      identifier_name_list = "df_ok",
      data_element_count = 2,
      meta_data_dataframe = data.frame()
    ),
    "I have .meta_data_dataframe."
  )

  expect_error(
    int_unexp_elements(identifier_name_list = "df_ok"),
    "I don't have .meta_data_dataframe."
  )

  expect_error(
    int_unexp_elements(
      identifier_name_list = c("df_a", "df_b"),
      data_element_count = 1
    ),
    "should have the same length"
  )
})
