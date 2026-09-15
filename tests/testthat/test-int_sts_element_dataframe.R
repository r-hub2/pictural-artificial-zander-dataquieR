test_that("test-int_sts_element_dataframe works", {
  skip_on_cran() # online, fragile
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship_meta_v2.xlsx") # nolint: line_length_linter.
  sd0 <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS", keep_types = TRUE) # nolint: line_length_linter.
  md0 <- prep_get_data_frame("item_level")
  dl0 <- prep_get_data_frame("dataframe_level")
  dl0$DF_NAME <- "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS" # nolint: line_length_linter.
  md0 <- md0[!(md0$VAR_NAMES %in% c("sex")), , FALSE] # remove a variable
  ised <- int_sts_element_dataframe(item_level = md0, meta_data_dataframe = dl0)
  expect_snapshot_value(ised, style = "deparse")
})

test_that("int_sts_element_dataframe compares local dataframe membership", {
  skip_on_cran()
  withr::local_options(viewer = function(...) NULL)

  data_file <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(a = 1:2, b = 3:4, extra = 5:6),
    data_file,
    row.names = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("a", "b", "missing_md"),
    DATAFRAMES = "df_code",
    DATA_TYPE = DATA_TYPES$INTEGER,
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )
  meta_data_dataframe <- data.frame(
    DF_NAME = data_file,
    DF_CODE = "df_code",
    stringsAsFactors = FALSE
  )

  result_for <- function(check_type) {
    suppressMessages(suppressWarnings(int_sts_element_dataframe(
      meta_data = meta_data,
      meta_data_dataframe = meta_data_dataframe,
      check_type = check_type
    )))
  }

  unchecked <- result_for("none")
  expect_true(is.na(unchecked$DataframeTable$NUM_int_sts_element))
  expect_true(is.na(unchecked$DataframeTable$PCT_int_sts_element))

  exact <- result_for("exact")
  expect_equal(as.integer(exact$DataframeTable$NUM_int_sts_element), 2L)
  expect_equal(as.numeric(exact$DataframeTable$PCT_int_sts_element), 50)
  expect_match(
    exact$DataframeData$`Affected Elements`,
    "missing_md.+extra"
  )

  subset_u <- result_for("subset_u")
  expect_equal(as.integer(subset_u$DataframeTable$NUM_int_sts_element), 1L)
  expect_match(subset_u$DataframeData$`Affected Elements`, "extra")
  expect_match(subset_u$DataframeData$`Affected Elements`, "md")

  subset_m <- result_for("subset_m")
  expect_equal(as.integer(subset_m$DataframeTable$NUM_int_sts_element), 1L)
  expect_match(subset_m$DataframeData$`Affected Elements`, "missing_md")
  expect_match(subset_m$DataframeData$`Affected Elements`, "sd")
})
