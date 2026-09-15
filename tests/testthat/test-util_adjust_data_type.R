test_that(
  "legacy util_adjust_data_type converts unstable and logical columns",
  {
    skip_on_cran()

    old_type_adjust <- getOption("dataquieR.old_type_adjust")
    on.exit(options(dataquieR.old_type_adjust = old_type_adjust), add = TRUE)
    options(dataquieR.old_type_adjust = TRUE)

    meta_data <- data.frame(
      var_names = c("num", "all_na_int", "all_na_float"),
      data_type = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER, DATA_TYPES$FLOAT),
      stringsAsFactors = FALSE
    )
    colnames(meta_data) <- c(VAR_NAMES, DATA_TYPE)

    study_data <- data.frame(
      num = c("1", "bad"),
      all_na_int = c(NA, NA),
      all_na_float = c(NA, NA),
      stringsAsFactors = FALSE
    )
    study_data$all_na_int <- as.logical(study_data$all_na_int)
    study_data$all_na_float <- as.logical(study_data$all_na_float)

    expect_message(
      adjusted <- util_adjust_data_type(
        study_data,
        meta_data,
        relevant_vars_for_warnings = "num"
      ),
      "introduced 1 additional missing"
    )

    expect_true(isTRUE(attr(adjusted, "Data_type_matches", exact = TRUE)))
    expect_equal(adjusted$num, c(1, NA))
    expect_type(adjusted$all_na_int, "integer")
    expect_type(adjusted$all_na_float, "double")
    expect_true(all(is.na(adjusted$all_na_int)))
    expect_true(all(is.na(adjusted$all_na_float)))
  }
)

test_that("legacy util_adjust_data_type skips already adjusted data", {
  skip_on_cran()

  old_type_adjust <- getOption("dataquieR.old_type_adjust")
  on.exit(options(dataquieR.old_type_adjust = old_type_adjust), add = TRUE)
  options(dataquieR.old_type_adjust = TRUE)

  meta_data <- data.frame(
    var_names = "x",
    data_type = DATA_TYPES$INTEGER,
    stringsAsFactors = FALSE
  )
  colnames(meta_data) <- c(VAR_NAMES, DATA_TYPE)

  study_data <- data.frame(x = "not converted", stringsAsFactors = FALSE)
  attr(study_data, "Data_type_matches") <- TRUE

  expect_identical(util_adjust_data_type(study_data, meta_data), study_data)
})
