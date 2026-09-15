test_that("util_compare_meta_with_study works", { #####
  skip_on_cran()
  study_data <- cars
  meta_data <- prep_create_meta(
    VAR_NAMES = c("speed", "dist"),
    LABEL = c("Speed", "Stopping distance"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = ""
  )

  expect_equal(
    util_compare_meta_with_study(study_data, meta_data,
      label_col = VAR_NAMES
    ),
    c(speed = 1, dist = 1)
  )

  study_data$speed <- study_data$speed + rnorm(nrow(study_data))
  expect_equal(
    util_compare_meta_with_study(study_data, meta_data,
      label_col = VAR_NAMES
    ),
    c(speed = 0, dist = 1)
  )

  study_data <- cars
  meta_data <- prep_create_meta(
    VAR_NAMES = c("speed", "dist"),
    LABEL = c("Speed", "Stopping distance"),
    DATA_TYPE = c(DATA_TYPES$FLOAT, DATA_TYPES$FLOAT),
    MISSING_LIST = ""
  )
  expect_equal(
    util_compare_meta_with_study(study_data, meta_data,
      label_col = VAR_NAMES
    ),
    c(speed = 1, dist = 1)
  )
  study_data$speed <- study_data$speed + rnorm(nrow(study_data))
  expect_equal(
    util_compare_meta_with_study(study_data, meta_data,
      label_col = VAR_NAMES
    ),
    c(speed = 1, dist = 1)
  )

  # note, that other data types are tested with the function
  # util_check_data_type, that util_compare_meta_with_study bases on
})

test_that("util_compare_meta_with_study normalizes empty comparison names", {
  skip_on_cran()

  study_data <- data.frame(value = c(1L, 2L))
  names(study_data) <- ""
  meta_data <- data.frame(
    VAR_NAMES = "",
    DATA_TYPE = DATA_TYPES$INTEGER,
    stringsAsFactors = FALSE
  )

  expect_warning(
    expect_warning(
      result <- util_compare_meta_with_study(study_data, meta_data,
        label_col = VAR_NAMES
      ),
      "Found columns w/o names"
    ),
    "Found empty labels"
  )
  expect_identical(result, c(v1 = 1L))
})

test_that("util_compare_meta_with_study rejects unstable percentages", {
  skip_on_cran()

  study_data <- data.frame(value = c(1L, 2L))
  meta_data <- data.frame(
    VAR_NAMES = "value",
    DATA_TYPE = DATA_TYPES$INTEGER,
    stringsAsFactors = FALSE
  )

  expect_error(
    util_compare_meta_with_study(study_data, meta_data,
      label_col = VAR_NAMES,
      return_percentages = TRUE
    ),
    "check_conversion_stable"
  )
})

test_that("util_compare_meta_with_study returns conversion percentages", {
  skip_on_cran()

  study_data <- data.frame(value = c("1", "2", "bad"))
  meta_data <- data.frame(
    VAR_NAMES = "value",
    DATA_TYPE = DATA_TYPES$INTEGER,
    stringsAsFactors = FALSE
  )

  result <- util_compare_meta_with_study(study_data, meta_data,
    label_col = VAR_NAMES,
    check_convertible = TRUE,
    return_percentages = TRUE,
    check_conversion_stable = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, "value")
  expect_equal(result$value, c(0, 200 / 3, 0, 100 / 3))
  expect_identical(rownames(result), c(
    "match",
    "convertible_mismatch_stable",
    "convertible_mismatch_unstable",
    "nonconvertible_mismatch"
  ))
  expect_length(attr(result, "which_vec", exact = TRUE)$value, 3L)
})
