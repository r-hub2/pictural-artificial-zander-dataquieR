test_that("com_qualified_item_missingness handles local AAPOR tables", {
  skip_on_cran()

  study_data <- data.frame(
    item = c(1, 2, 3, 4, 99, NA),
    stringsAsFactors = FALSE
  )
  missing_table <- data.frame(
    CODE_VALUE = c(1, 2, 3, 4, 99),
    CODE_LABEL = c(
      "Interview",
      "Refusal",
      "Break-off",
      "Unknown other",
      "Participation"
    ),
    CODE_INTERPRET = c("I", "R", "BO", "UO", "P"),
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(missing_table = missing_table)
  meta_data <- data.frame(
    VAR_NAMES = "item",
    LABEL = "item",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    MISSING_LIST_TABLE = "missing_table",
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- suppressMessages(com_qualified_item_missingness(
      resp_vars = "item",
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES,
      expected_observations = "ALL"
    )),
    "impossible on item level"
  )

  expect_named(result, c("SummaryTable", "SummaryData"))
  expect_equal(as.numeric(result$SummaryTable$P), 0)
  expect_equal(as.numeric(result$SummaryTable$UO), 2)
  expect_equal(as.numeric(result$SummaryTable$PCT_com_qum_refusal), 40)
  expect_equal(as.character(result$SummaryData[[2]]), "80%")
  expect_equal(as.character(result$SummaryData[[3]]), "40%")
  expect_true(all(vapply(
    result$SummaryData[-1],
    is.character,
    FUN.VALUE = logical(1)
  )))
})

test_that("com_qualified_item_missingness works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx") # nolint: line_length_linter.

  r1 <- com_qualified_item_missingness(
    resp_vars = "SBP_0",
    study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
    label_col = LABEL
  )
  expect_identical(dim(r1$SummaryTable), c(1L, 15L))
  suppressWarnings(expect_warning(
    r2 <- com_qualified_item_missingness(
      study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
      label_col = LABEL
    ),
    regexp = "No missing-match-table"
  ))
  expect_identical(dim(r2$SummaryTable), c(36L, 15L))

  mt <- prep_get_data_frame("missing_table")
  mt <- mt[!(mt$CODE_INTERPRET %in% c("I", "P", "PL")), , FALSE] # for qual.segment.missingness, this would trigger warnings # nolint: line_length_linter.
  prep_add_data_frames(missing_table = mt)
  r3 <- com_qualified_item_missingness(
    resp_vars = "SBP_0",
    study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
    label_col = LABEL
  )

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx") # nolint: line_length_linter.
  mt <- prep_get_data_frame("missing_table")
  mt <- mt[!(mt$CODE_INTERPRET %in% c("R", "BO")), , FALSE]
  prep_add_data_frames(missing_table = mt)
  expect_warning(
    r4 <- com_qualified_item_missingness(
      resp_vars = "SBP_0",
      study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
      label_col = LABEL
    ),
    regexp = ".*Nonresponse Rate 1.*"
  )
})

# Use devtools::test_coverage_active_file() locally for coverage debugging.
