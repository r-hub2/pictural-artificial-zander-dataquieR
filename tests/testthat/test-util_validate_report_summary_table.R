skip_on_cran()

test_that(
  "util_validate_report_summary_table fixes duplicate variable labels",
  {
    table <- data.frame(
      Variables = c("x", "x"),
      N = c(10, 20),
      Metric = c(1, 2)
    )

    validated <- util_validate_report_summary_table(table)

    expect_identical(validated$Variables, c("x (1)", "x (2)"))
    expect_false(any(duplicated(validated$Variables)))
    expect_identical(validated$N, c(10, 20))
    expect_identical(validated$Metric, c(1, 2))
  }
)

test_that("util_validate_report_summary_table uses variable-name attributes", {
  table <- data.frame(
    Variables = c("x", "x"),
    N = c(10, 20),
    Metric = c(1, 2)
  )
  table <- util_set_report_summary_table_var_names(table, c("v1", "v2"))

  validated <- util_validate_report_summary_table(table)

  expect_identical(validated$Variables, c("x (v1)", "x (v2)"))
  expect_identical(util_report_summary_table_var_names(validated),
    c("v1", "v2"))
})

test_that("util_validate_report_summary_table fixes residual duplicates", {
  table <- data.frame(
    Variables = c("x", "x"),
    N = c(10, 20),
    Metric = c(1, 2)
  )
  table <- util_set_report_summary_table_var_names(table, c("v", "v"))

  validated <- util_validate_report_summary_table(table)

  expect_false(any(duplicated(validated$Variables)))
  expect_identical(validated$Variables, c("x (v) 1", "x (v) 2"))
})

test_that("util_validate_report_summary_table keeps variable label column", {
  table <- data.frame(
    Variables = c("x", "y"),
    N = c(10, 20),
    Metric = c(1, 2)
  )
  table <- util_set_report_summary_table_variables_label_col(table, LABEL)

  validated <- util_validate_report_summary_table(table)

  expect_type(validated$Variables, "character")
  expect_identical(util_report_summary_table_variables_label_col(validated),
    LABEL)
})

test_that("util_validate_report_summary_table rejects invalid input shape", {
  expect_error(
    util_validate_report_summary_table(data.frame(Variables = "x", Metric = 1)),
    "Missing columns"
  )
  expect_error(
    util_validate_report_summary_table(data.frame(
      Variables = "x",
      N = 1,
      Metric = "not numeric"
    )),
    "numeric columns only"
  )
  expect_error(
    util_validate_report_summary_table(data.frame(
      Variables = NA_character_,
      N = 1,
      Metric = 1
    )),
    "w/o NAs"
  )
})

test_that(
  "util_validate_report_summary_table needs complete metadata context",
  {
    table <- data.frame(Variables = "x", N = 1, Metric = 1)

    expect_error(
      util_validate_report_summary_table(
        table,
        meta_data = data.frame(VAR_NAMES = "x")
      ),
      "Need to have either"
    )
    expect_error(
      util_validate_report_summary_table(table, label_col = VAR_NAMES),
      "Need to have either"
    )
  }
)

test_that(
  "util_validate_report_summary_table maps variables through metadata",
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = c("v1", "v2"),
      LABEL = c("Long label one", "Long label two"),
      DATA_TYPE = DATA_TYPES$INTEGER,
      SCALE_LEVEL = SCALE_LEVELS$RATIO,
      stringsAsFactors = FALSE
    )
    table <- data.frame(
      Variables = c("v1", "v2"),
      N = c(1, 2),
      Metric = c(0.1, 0.2)
    )

    validated <- util_validate_report_summary_table(
      table,
      meta_data = meta_data,
      label_col = LABEL
    )

    expect_equal(as.vector(validated$Variables),
      c("Long label one", "Long label two"))
    expect_identical(attr(validated$Variables, "label_col", exact = TRUE),
      as.character(LABEL))
    expect_equal(validated$N, c(1, 2))
    expect_equal(validated$Metric, c(0.1, 0.2))

    factor_table <- table
    factor_table$Variables <- factor(factor_table$Variables)

    validated_factor <- util_validate_report_summary_table(
      factor_table,
      meta_data = meta_data,
      label_col = LABEL
    )

    expect_s3_class(validated_factor$Variables, "factor")
    expect_equal(levels(validated_factor$Variables),
      c("Long label one", "Long label two"))
    expect_equal(as.character(validated_factor$Variables),
      c("Long label one", "Long label two"))
  }
)
