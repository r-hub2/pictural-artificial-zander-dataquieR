test_that("prepared data frames keep raw study data attributes aligned", {
  study_data <- data.frame(a = 1:4, b = 5:8)
  meta_data <- data.frame(
    VAR_NAMES = c("a", "b"),
    LABEL = c("a", "b"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER)
  )

  ds1 <- prep_prepare_dataframes(
    .study_data = study_data,
    .meta_data = meta_data,
    .label_col = LABEL,
    .replace_missings = FALSE,
    .adjust_data_type = FALSE,
    .amend_scale_level = FALSE
  )

  expect_s3_class(ds1, "dataquieR_data_frame_prepared")
  expect_s3_class(ds1, "data.frame")

  ds1_subset <- ds1[1:2, ]
  expect_s3_class(ds1_subset, "dataquieR_data_frame_prepared")
  expect_identical(
    dim(util_attr(ds1_subset, "study_data", exact = TRUE)),
    dim(ds1_subset)
  )
  expect_equal(util_attr(ds1_subset, "study_data", exact = TRUE), study_data[1:2, ], # nolint: line_length_linter.
    ignore_attr = TRUE
  )

  ds1_head <- head(ds1, 3)
  expect_s3_class(ds1_head, "dataquieR_data_frame_prepared")
  expect_identical(
    dim(util_attr(ds1_head, "study_data", exact = TRUE)),
    dim(ds1_head)
  )
  expect_equal(util_attr(ds1_head, "study_data", exact = TRUE), study_data[1:3, ], # nolint: line_length_linter.
    ignore_attr = TRUE
  )

  ds1_filtered <- subset(ds1, a <= 2)
  expect_s3_class(ds1_filtered, "dataquieR_data_frame_prepared")
  expect_identical(
    dim(util_attr(ds1_filtered, "study_data", exact = TRUE)),
    dim(ds1_filtered)
  )
  expect_equal(util_attr(ds1_filtered, "study_data", exact = TRUE), study_data[1:2, ], # nolint: line_length_linter.
    ignore_attr = TRUE
  )

  expect_equal(ds1[, "a"], study_data$a)
})

test_that("prepared data frame subsetting selects study_data by visible rows and names", { # nolint: line_length_linter.
  ds1 <- data.frame(v00001 = c(1, 2, 1), v00007 = c(1, 1, 2))
  ds1 <- util_as_prepared_data_frame(ds1)
  attr(ds1, "MAPPED") <- TRUE
  attr(ds1, "label_col") <- VAR_NAMES
  attr(ds1, "study_data") <- data.frame(
    v00007 = c(1, 1, 2),
    v00001 = c(1, 2, 1)
  )

  ds2 <- subset(ds1, v00007 == 1, c("v00001", "v00007"))

  expect_equal(colnames(ds2), c("v00001", "v00007"))
  expect_equal(
    colnames(util_attr(ds2, "study_data", exact = TRUE)),
    c("v00001", "v00007")
  )
  expect_equal(util_attr(ds2, "study_data", exact = TRUE), data.frame(
    v00001 = c(1, 2),
    v00007 = c(1, 1),
    row.names = c(1L, 2L)
  ), ignore_attr = TRUE)
})

test_that("prepared data frame subsetting reads raw study_data attribute exactly", { # nolint: line_length_linter.
  ds1 <- data.frame(v00001 = c(1, 2, 1), v00007 = c(1, 1, 2))
  ds1 <- util_as_prepared_data_frame(ds1)
  attr(ds1, "study_data_backup") <- data.frame(
    v00001 = c(99, 99, 99),
    v00007 = c(99, 99, 99)
  )

  expect_true(is.data.frame(attributes(ds1)[["study_data_backup"]]))
  expect_null(util_attr(ds1, "study_data"))

  ds2 <- ds1[1:2, , drop = FALSE]

  expect_null(util_attr(ds2, "study_data", exact = TRUE))
})

test_that("prepared data frame coercion and subsetting drop invalid raw data", {
  expect_identical(
    util_as_prepared_data_frame("not a data frame"),
    "not a data frame"
  )

  ds1 <- data.frame(v00001 = c(1, 2, 3), v00007 = c(3, 2, 1))
  ds1 <- util_as_prepared_data_frame(ds1)
  attr(ds1, "study_data") <- data.frame(v00001 = c(1, 2))

  ds2 <- ds1[1:2, , drop = FALSE]
  expect_s3_class(ds2, "dataquieR_data_frame_prepared")
  expect_null(util_attr(ds2, "study_data", exact = TRUE))

  ds3 <- ds1[, "v00001"]
  expect_equal(ds3, ds1$v00001)
})

test_that("prepared data frame subsetting drops raw data for unknown rows", {
  ds1 <- data.frame(v00001 = c(1, 2, 3), v00007 = c(3, 2, 1))
  ds1 <- util_as_prepared_data_frame(ds1)
  attr(ds1, "study_data") <- data.frame(
    v00001 = c(1, 2, 3),
    v00007 = c(3, 2, 1)
  )

  ds2 <- ds1[c(1, NA), , drop = FALSE]

  expect_s3_class(ds2, "dataquieR_data_frame_prepared")
  expect_equal(rownames(ds2), c("1", "NA"))
  expect_null(util_attr(ds2, "study_data", exact = TRUE))
})
