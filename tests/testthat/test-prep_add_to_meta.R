test_that("prep_add_to_meta works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  md <- prep_add_to_meta(
    VAR_NAMES = c("X", "Y"),
    LABEL = c("x", "y"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT),
    VALUE_LABELS = c("1 = female | 2 = male", NA),
    LONG_LABEL = c("Ix", "Ypsilon"),
    test = 3:4,
    meta_data = meta_data
  )
  expect_false("test" %in% colnames(md))
  expect_equal(nrow(md), 55)
  new <- md[
    md$VAR_NAMES %in% c("X", "Y"),
    c("VAR_NAMES", "LABEL", "DATA_TYPE", "VALUE_LABELS", "LONG_LABEL")
  ]
  expect_identical(
    new,
    structure(
      list(
        VAR_NAMES = c("X", "Y"), LABEL = c("x", "y"),
        DATA_TYPE = c("integer", "float"),
        VALUE_LABELS = c("1 = female | 2 = male", NA),
        LONG_LABEL = c("Ix", "Ypsilon")
      ),
      row.names = 54:55, class = "data.frame"
    )
  )
})

test_that("prep_add_to_meta handles local metadata rows", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = "x",
    DATA_TYPE = DATA_TYPES$INTEGER,
    LABEL = "x",
    VALUE_LABELS = "",
    MISSING_LIST_TABLE = NA_character_,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    PART_VAR = "",
    stringsAsFactors = FALSE
  )

  cache <- new.env(parent = emptyenv())
  amended <- with_dataframe_environment(quote({
    prep_add_data_frames(item_level = meta_data)
    prep_add_to_meta(
      VAR_NAMES = "new",
      DATA_TYPE = DATA_TYPES$INTEGER,
      LABEL = "new",
      VALUE_LABELS = "1 = yes",
      meta_data = meta_data,
      PART_VAR = "x",
      UNUSED_COLUMN = "ignored"
    )
  }), env = cache)

  expect_equal(amended$VAR_NAMES, c("x", "new"))
  expect_equal(amended$PART_VAR, c("", "x"))
  expect_equal(amended$VALUE_LABELS, c("", "1 = yes"))
  expect_false("UNUSED_COLUMN" %in% names(amended))
})

test_that("prep_add_to_meta rejects missing participation references", {
  skip_on_cran()

  cache <- new.env(parent = emptyenv())
  meta_data <- data.frame(
    VAR_NAMES = "x",
    DATA_TYPE = DATA_TYPES$INTEGER,
    LABEL = "x",
    VALUE_LABELS = "",
    MISSING_LIST_TABLE = NA_character_,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    PART_VAR = "",
    stringsAsFactors = FALSE
  )

  expect_error(
    with_dataframe_environment(quote({
      prep_add_data_frames(item_level = meta_data)
      prep_add_to_meta(
        VAR_NAMES = "new",
        DATA_TYPE = DATA_TYPES$INTEGER,
        LABEL = "new",
        VALUE_LABELS = "",
        meta_data = meta_data,
        PART_VAR = "missing_part"
      )
    }), env = cache),
    "referred variables does not exist"
  )
})
