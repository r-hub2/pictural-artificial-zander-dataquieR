test_that("util_get_encoding assumes UTF-8 when stringi is unavailable", {
  skip_on_cran()

  study_data <- data.frame(v_without_meta = "abc", stringsAsFactors = FALSE)
  meta_data <- data.frame(
    LABEL = "v_without_meta",
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    util_stringi_available = function() FALSE
  )

  expect_warning(
    expect_warning(
      result <- util_get_encoding(
        resp_vars = "v_without_meta",
        study_data = study_data,
        label_col = LABEL,
        meta_data = meta_data,
        meta_data_dataframe = data.frame()
      ),
      "guessing from data"
    ),
    "Cannot guess encodings without"
  )

  expect_identical(unname(result["v_without_meta"]), "UTF-8")
})

test_that(
  "util_get_encoding uses item-level metadata and re-guesses unknown values",
  {
    skip_on_cran()

    study_data <- data.frame(
      v_utf8 = "abc",
      v_bad = "def",
      stringsAsFactors = FALSE
    )
    meta_data <- data.frame(
      LABEL = c("v_utf8", "v_bad"),
      ENCODING = c(" utf-8 ", "definitely-not-an-encoding"),
      stringsAsFactors = FALSE
    )

    expect_warning(
      expect_warning(
        result <- util_get_encoding(
          resp_vars = c("v_utf8", "v_bad"),
          study_data = study_data,
          label_col = LABEL,
          meta_data = meta_data,
          meta_data_dataframe = data.frame()
        ),
        "unkown encodings"
      ),
      "guessing from data"
    )

    expect_identical(unname(result["v_utf8"]), "UTF-8")
    expect_true(unname(result["v_bad"]) %in% c("ASCII", "UTF-8"))
  }
)

test_that("util_get_encoding uses dataframe-level metadata", {
  skip_on_cran()

  study_data <- data.frame(v_from_df = "abc", stringsAsFactors = FALSE)
  meta_data <- data.frame(
    LABEL = "v_from_df",
    DATAFRAMES = "present = df1",
    stringsAsFactors = FALSE
  )
  meta_data_dataframe <- data.frame(
    DF_CODE = "df1",
    ENCODING = " ascii ",
    stringsAsFactors = FALSE
  )

  result <- util_get_encoding(
    resp_vars = "v_from_df",
    study_data = study_data,
    label_col = LABEL,
    meta_data = meta_data,
    meta_data_dataframe = meta_data_dataframe
  )

  expect_identical(unname(result["v_from_df"]), "ASCII")
})

test_that("util_get_encoding warns on multiple dataframe-level encodings", {
  skip_on_cran()

  study_data <- data.frame(v_from_two_dfs = "abc", stringsAsFactors = FALSE)
  meta_data <- data.frame(
    LABEL = "v_from_two_dfs",
    DATAFRAMES = "present = df1 | also_present = df2",
    stringsAsFactors = FALSE
  )
  meta_data_dataframe <- data.frame(
    DF_CODE = c("df1", "df2"),
    ENCODING = c("UTF-8", "ASCII"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- util_get_encoding(
      resp_vars = "v_from_two_dfs",
      study_data = study_data,
      label_col = LABEL,
      meta_data = meta_data,
      meta_data_dataframe = meta_data_dataframe
    ),
    "Found more than one encodings"
  )

  expect_identical(unname(result["v_from_two_dfs"]), "UTF-8")
})

test_that("util_get_encoding re-guesses unknown dataframe-level encodings", {
  skip_on_cran()

  study_data <- data.frame(v_from_df = "abc", stringsAsFactors = FALSE)
  meta_data <- data.frame(
    LABEL = "v_from_df",
    DATAFRAMES = "present = df1",
    stringsAsFactors = FALSE
  )
  meta_data_dataframe <- data.frame(
    DF_CODE = "df1",
    ENCODING = "definitely-not-an-encoding",
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    prep_guess_encoding = function(x) {
      data.frame(encoding = "UTF-8", stringsAsFactors = FALSE)
    }
  )

  expect_warning(
    expect_warning(
      result <- util_get_encoding(
        resp_vars = "v_from_df",
        study_data = study_data,
        label_col = LABEL,
        meta_data = meta_data,
        meta_data_dataframe = meta_data_dataframe
      ),
      "unkown encodings"
    ),
    "guessing from data"
  )

  expect_identical(unname(result["v_from_df"]), "UTF-8")
})

test_that("util_get_encoding guesses when dataframe encoding is empty", {
  skip_on_cran()

  study_data <- data.frame(v_from_df = "abc", stringsAsFactors = FALSE)
  meta_data <- data.frame(
    LABEL = "v_from_df",
    DATAFRAMES = "present = df1",
    stringsAsFactors = FALSE
  )
  meta_data_dataframe <- data.frame(
    DF_CODE = "df1",
    ENCODING = "",
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    prep_guess_encoding = function(x) {
      data.frame(encoding = "UTF-8", stringsAsFactors = FALSE)
    }
  )

  expect_warning(
    result <- util_get_encoding(
      resp_vars = "v_from_df",
      study_data = study_data,
      label_col = LABEL,
      meta_data = meta_data,
      meta_data_dataframe = meta_data_dataframe
    ),
    "guessing from data"
  )

  expect_identical(unname(result["v_from_df"]), "UTF-8")
})
