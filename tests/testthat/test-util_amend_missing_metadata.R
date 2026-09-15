test_that("util_amend_missing_metadata keeps complete metadata unchanged", {
  skip_on_cran()

  study_data <- data.frame(a = 1:2)
  meta_data <- data.frame(
    VAR_NAMES = "a",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    LABEL = "a"
  )

  expect_identical(
    util_amend_missing_metadata(study_data, meta_data),
    meta_data
  )
})

test_that("util_amend_missing_metadata appends guessed rows for missing vars", {
  skip_on_cran()

  study_data <- data.frame(a = 1:2, b = c("x", "y"))
  meta_data <- data.frame(
    VAR_NAMES = "a",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    LABEL = "a"
  )

  expect_message(
    amended <- util_amend_missing_metadata(
      study_data,
      meta_data,
      level = VARATT_REQUIRE_LEVELS$REQUIRED,
      guess_missing_codes = FALSE
    ),
    "Missing"
  )

  expect_true(all(c("a", "b") %in% amended[[VAR_NAMES]]))
  expect_equal(
    amended[amended[[VAR_NAMES]] == "b", DATA_TYPE, drop = TRUE],
    DATA_TYPES$STRING
  )
  expect_equal(
    amended[amended[[VAR_NAMES]] == "a", LABEL, drop = TRUE],
    "a"
  )
})

test_that("util_amend_missing_metadata replaces invalid metadata", {
  skip_on_cran()

  study_data <- data.frame(a = 1:2, b = c("x", "y"))

  expect_message(
    amended <- util_amend_missing_metadata(
      study_data,
      meta_data = "item_level",
      level = VARATT_REQUIRE_LEVELS$REQUIRED,
      guess_missing_codes = FALSE
    ),
    "Missing"
  )

  expect_s3_class(amended, "data.frame")
  expect_setequal(amended[[VAR_NAMES]], c("a", "b"))
  expect_equal(
    amended[amended[[VAR_NAMES]] == "a", DATA_TYPE, drop = TRUE],
    DATA_TYPES$INTEGER
  )
  expect_equal(
    amended[amended[[VAR_NAMES]] == "b", DATA_TYPE, drop = TRUE],
    DATA_TYPES$STRING
  )
})
