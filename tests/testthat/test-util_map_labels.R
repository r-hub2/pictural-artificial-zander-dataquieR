test_that("util_map_labels works", {
  skip_on_cran()
  meta_data <- prep_create_meta(
    VAR_NAMES = c("ID", "SEX", "AGE", "DOE"),
    LABEL = c("Pseudo-ID", "Gender", "Age", "Examination Date"),
    MISSING_LIST = "",
    DATA_TYPE = c(
      DATA_TYPES$INTEGER, DATA_TYPES$INTEGER, DATA_TYPES$INTEGER,
      DATA_TYPES$DATETIME
    )
  )
  expect_equal(
    util_map_labels(c("AGE", "DOE"), meta_data),
    c(AGE = "Age", DOE = "Examination Date")
  )
  expect_error(util_map_labels(c("NOT_AVAIL", "AGE"), meta_data),
    regexp = "value for .+NOT_AVAIL.+ not found", perl = TRUE
  )
  expect_equal(
    util_map_labels(c("AGE", "NOT_AVAIL"), meta_data,
      ifnotfound = NA_character_
    ),
    c(AGE = "Age", NOT_AVAIL = NA_character_)
  )
  expect_equal(
    util_map_labels(c("AGE", "NOT_AVAIL"), meta_data,
      ifnotfound = 42
    ),
    c(AGE = "Age", NOT_AVAIL = 42)
  )
})

test_that("util_map_labels uses position-specific fallbacks for empty input", {
  skip_on_cran()
  meta_data <- data.frame(
    VAR_NAMES = c("id", "", NA_character_),
    LABEL = c("Identifier", "Blank metadata name", "Missing metadata name"),
    stringsAsFactors = FALSE
  )

  expect_equal(
    util_map_labels(
      c("id", "", "unknown"),
      meta_data = meta_data,
      ifnotfound = c("fallback-id", "fallback-empty", "fallback-unknown")
    ),
    setNames(
      c("Identifier", "fallback-empty", "fallback-unknown"),
      c("id", NA_character_, "unknown")
    )
  )
})

test_that("util_map_labels avoids replacement-name collisions", {
  skip_on_cran()
  meta_data <- data.frame(
    VAR_NAMES = c("id", ""),
    LABEL = c("Identifier", "Blank metadata name"),
    stringsAsFactors = FALSE
  )

  mapped <- util_map_labels(
    c("id", "..1"),
    meta_data = meta_data,
    ifnotfound = c("fallback-id", "fallback-dot")
  )

  expect_equal(mapped, c(id = "Identifier", "..1" = "fallback-dot"))
})

test_that("util_map_labels warns for ambiguous metadata mappings", {
  skip_on_cran()
  meta_data <- data.frame(
    VAR_NAMES = c("id", "id"),
    LABEL = c("First label", "Second label"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    mapped <- util_map_labels(
      "id",
      meta_data = meta_data,
      ifnotfound = "fallback",
      warn_ambiguous = TRUE
    ),
    "There are several entries 'id'"
  )

  expect_equal(mapped, c(id = "Second label"))
})
