test_that("com_item_missingness exposes specified-reason metrics", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    item = c(1L, 998L, 999L, NA_integer_, 2L)
  )
  meta_data <- data.frame(
    variable = "item",
    label = "Item",
    type = DATA_TYPES$INTEGER,
    scale = SCALE_LEVELS$RATIO,
    missing = "998 = specified missing",
    jump = "999 = not applicable"
  )
  names(meta_data) <- c(
    VAR_NAMES, LABEL, DATA_TYPE, SCALE_LEVEL, MISSING_LIST, JUMP_LIST
  )

  result <- suppressMessages(com_item_missingness(
    resp_vars = "item",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    expected_observations = "ALL",
    threshold_value = 100,
    suppressWarnings = TRUE
  ))

  expect_equal(result$SummaryTable$NUM_com_qum_spec, 2L,
    ignore_attr = TRUE
  )
  expect_equal(result$SummaryTable$PCT_com_qum_spec, 50,
    ignore_attr = TRUE
  )
  expect_equal(result$SummaryTable$`Sysmiss N`, 1L, ignore_attr = TRUE)
  expect_equal(result$SummaryTable$NUM_int_vfe_missunc, 1L,
    ignore_attr = TRUE
  )
  expect_equal(result$SummaryTable$PCT_int_vfe_missunc, 20,
    ignore_attr = TRUE
  )
  expect_identical(
    attr(result$SummaryTable$NUM_int_vfe_missunc, DATA_TYPE, exact = TRUE),
    DATA_TYPES$INTEGER
  )
  expect_identical(
    attr(result$SummaryTable$PCT_int_vfe_missunc, DATA_TYPE, exact = TRUE),
    DATA_TYPES$FLOAT
  )
  expect_identical(
    attr(result$SummaryTable$NUM_com_qum_spec, DATA_TYPE, exact = TRUE),
    DATA_TYPES$INTEGER
  )
  expect_identical(
    attr(result$SummaryTable$PCT_com_qum_spec, DATA_TYPE, exact = TRUE),
    DATA_TYPES$FLOAT
  )

  metrics <- util_extract_indicator_metrics(result$SummaryTable)
  expect_true(all(c(
    "NUM_int_vfe_missunc", "PCT_int_vfe_missunc",
    "NUM_com_qum_spec", "PCT_com_qum_spec"
  ) %in% names(metrics)))
  expect_equal(metrics$NUM_com_qum_spec, 2L, ignore_attr = TRUE)
  expect_equal(metrics$PCT_com_qum_spec, 50, ignore_attr = TRUE)
  count_label <- "Missing due to specified reason (Number)"
  percentage_label <-
    "Missing due to specified reason (Percentage (0 to 100))"
  expect_equal(result$SummaryData[[count_label]], 2L, ignore_attr = TRUE)
  expect_equal(
    result$SummaryData[[percentage_label]],
    "50%",
    ignore_attr = TRUE
  )
  expect_identical(
    attr(result$SummaryData[[count_label]], DATA_TYPE, exact = TRUE),
    DATA_TYPES$INTEGER
  )
  expect_identical(
    attr(result$SummaryData[[percentage_label]], DATA_TYPE, exact = TRUE),
    DATA_TYPES$FLOAT
  )
})

test_that("specified-reason percentage is undefined without analyzable cases", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(item = NA_integer_)
  meta_data <- data.frame(
    variable = "item",
    label = "Item",
    type = DATA_TYPES$INTEGER,
    scale = SCALE_LEVELS$RATIO,
    missing = "998 = specified missing",
    jump = "999 = not applicable"
  )
  names(meta_data) <- c(
    VAR_NAMES, LABEL, DATA_TYPE, SCALE_LEVEL, MISSING_LIST, JUMP_LIST
  )

  result <- suppressMessages(com_item_missingness(
    resp_vars = "item",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    expected_observations = "ALL",
    threshold_value = 100,
    suppressWarnings = TRUE
  ))

  expect_equal(result$SummaryTable$NUM_com_qum_spec, 0L,
    ignore_attr = TRUE
  )
  expect_true(is.na(result$SummaryTable$PCT_com_qum_spec))
  expect_false(is.nan(result$SummaryTable$PCT_com_qum_spec))
  percentage_label <-
    "Missing due to specified reason (Percentage (0 to 100))"
  expect_true(is.na(result$SummaryData[[percentage_label]]))
})
