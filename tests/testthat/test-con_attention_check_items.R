test_that("con_attention_check_items summarizes attention check mismatches", {
  skip_on_cran()

  study_data <- data.frame(
    scaleA1 = c(1, 2, 1, NA),
    scaleA2 = c(2, 2, 2, 1),
    scaleA3 = c(1, 1, 2, 1),
    SUM_ATTENTION_CHECK_ITEMS_attention_page = c(0, 0, 1, 2)
  )
  meta_data <- prep_study2meta(study_data)
  meta_data[[ITEM_TYPE]] <- c("BOGUS: 1|2", "INSTRUCTED: 2", "BOGUS: 1", NA)
  meta_data[[JUMP_LIST]] <- SPLIT_CHAR
  meta_data[[COMPUTED_VARIABLE_ROLE]] <- c(
    NA,
    NA,
    NA,
    COMPUTED_VARIABLE_ROLES$SUM_ATTENTION_CHECK_ITEMS
  )
  meta_data[[CHECK_ID]] <- c(NA, NA, NA, "attention_page")
  meta_data[[DATA_TYPE]][4] <- DATA_TYPES$FLOAT
  meta_data[[SCALE_LEVEL]][4] <- SCALE_LEVELS$RATIO

  meta_data_cross_item <- data.frame(
    VARIABLE_LIST = "scaleA1 | scaleA2 | scaleA3",
    CHECK_LABEL = "attention_page",
    CHECK_ID = "attention_page",
    SUM_ATTENTION_CHECK_ITEMS = "[0;1]"
  )

  result <- con_attention_check_items(
    study_data = study_data,
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item,
    resp_vars = "SUM_ATTENTION_CHECK_ITEMS_attention_page",
    label_col = VAR_NAMES
  )
  result <- unclass(result)

  expect_equal(
    as.vector(result$SummaryData$`Check items (N)`),
    3
  )
  expect_equal(
    as.vector(result$SummaryData$`Cases with incorrect responses N (%)`),
    "1 (25%)"
  )
  expect_equal(
    result$SummaryTable$NUM_ssc_sumattit,
    1
  )
  expect_equal(
    attr(result$SummaryData$Variables, DATA_TYPE),
    DATA_TYPES$STRING
  )
})

test_that("con_attention_check_items handles open interval bounds", {
  skip_on_cran()

  study_data <- data.frame(
    scaleA1 = c(1, 2, 1, NA),
    scaleA2 = c(2, 2, 2, 1),
    scaleA3 = c(1, 1, 2, 1),
    SUM_ATTENTION_CHECK_ITEMS_attention_page = c(0, 0, 1, 2)
  )
  meta_data <- prep_study2meta(study_data)
  meta_data[[ITEM_TYPE]] <- c("BOGUS: 1|2", "INSTRUCTED: 2", "BOGUS: 1", NA)
  meta_data[[JUMP_LIST]] <- SPLIT_CHAR
  meta_data[[COMPUTED_VARIABLE_ROLE]] <- c(
    NA,
    NA,
    NA,
    COMPUTED_VARIABLE_ROLES$SUM_ATTENTION_CHECK_ITEMS
  )
  meta_data[[CHECK_ID]] <- c(NA, NA, NA, "attention_page")
  meta_data[[DATA_TYPE]][4] <- DATA_TYPES$FLOAT
  meta_data[[SCALE_LEVEL]][4] <- SCALE_LEVELS$RATIO

  meta_data_cross_item <- data.frame(
    VARIABLE_LIST = "scaleA1 | scaleA2 | scaleA3",
    CHECK_LABEL = "attention_page",
    CHECK_ID = "attention_page",
    SUM_ATTENTION_CHECK_ITEMS = "(0;1)"
  )

  result <- con_attention_check_items(
    study_data = study_data,
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item,
    resp_vars = "SUM_ATTENTION_CHECK_ITEMS_attention_page"
  )

  expect_equal(
    unclass(result)$SummaryTable$NUM_ssc_sumattit,
    4
  )
})

test_that("con_attention_check_items rejects invalid intervals", {
  skip_on_cran()

  study_data <- data.frame(
    scaleA1 = c(1, 2),
    SUM_ATTENTION_CHECK_ITEMS_attention_page = c(0, 1)
  )
  meta_data <- prep_study2meta(study_data)
  meta_data[[ITEM_TYPE]] <- c("BOGUS: 1|2", NA)
  meta_data[[JUMP_LIST]] <- SPLIT_CHAR
  meta_data[[COMPUTED_VARIABLE_ROLE]] <- c(
    NA,
    COMPUTED_VARIABLE_ROLES$SUM_ATTENTION_CHECK_ITEMS
  )
  meta_data[[CHECK_ID]] <- c(NA, "attention_page")
  meta_data[[DATA_TYPE]][2] <- DATA_TYPES$FLOAT
  meta_data[[SCALE_LEVEL]][2] <- SCALE_LEVELS$RATIO

  meta_data_cross_item <- data.frame(
    VARIABLE_LIST = "scaleA1",
    CHECK_LABEL = "attention_page",
    CHECK_ID = "attention_page",
    SUM_ATTENTION_CHECK_ITEMS = "bad interval"
  )

  expect_warning(
    expect_error(
      con_attention_check_items(
        study_data = study_data,
        meta_data = meta_data,
        meta_data_cross_item = meta_data_cross_item,
        resp_vars = "SUM_ATTENTION_CHECK_ITEMS_attention_page"
      ),
      "Invalid interval"
    ),
    "Parser error in REDCap interval"
  )
})

test_that("con_attention_check_items rejects missing cross-item metadata", {
  skip_on_cran()

  study_data <- data.frame(
    scaleA1 = c(1, 2),
    SUM_ATTENTION_CHECK_ITEMS_attention_page = c(0, 1)
  )
  meta_data <- prep_study2meta(study_data)
  meta_data[[ITEM_TYPE]] <- c("BOGUS: 1|2", NA)
  meta_data[[JUMP_LIST]] <- SPLIT_CHAR
  meta_data[[COMPUTED_VARIABLE_ROLE]] <- c(
    NA,
    COMPUTED_VARIABLE_ROLES$SUM_ATTENTION_CHECK_ITEMS
  )
  meta_data[[CHECK_ID]] <- c(NA, "attention_page")
  meta_data[[DATA_TYPE]][2] <- DATA_TYPES$FLOAT
  meta_data[[SCALE_LEVEL]][2] <- SCALE_LEVELS$RATIO

  expect_error(
    con_attention_check_items(
      study_data = study_data,
      meta_data = meta_data,
      meta_data_cross_item = "cross-item_level",
      resp_vars = "SUM_ATTENTION_CHECK_ITEMS_attention_page"
    )
  )
})
