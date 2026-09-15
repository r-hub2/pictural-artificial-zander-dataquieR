test_that("util_prep_proportion_check prepares global and category ranges", {
  skip_on_cran()

  ds1 <- data.frame(
    x = c(0L, 1L, 1L, 0L),
    y = factor(c("No", "Yes", "Yes", "No"), levels = c("No", "Yes"))
  )
  attr(ds1, "apply_fact_md_inadm") <- TRUE
  attr(ds1, "label_col") <- VAR_NAMES

  meta_data <- data.frame(
    VAR_NAMES = c("x", "y"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$STRING),
    MISSING_LIST = "",
    PROPORTION_RANGE = c("[0.2;0.8]", "No = [0;0.5] | Yes = [0.5;1]"),
    VALUE_LABEL_TABLE = NA_character_,
    stringsAsFactors = FALSE
  )

  global <- util_prep_proportion_check("x", meta_data, ds1,
    report_problems = "message")
  expect_named(global$Range$x, c("0", "1"))
  expect_s3_class(global$Range$x[["0"]], "interval")
  expect_equal(global$Range$x[["0"]]$low, 0.2)
  expect_equal(global$Range$x[["1"]]$upp, 0.8)

  per_category <- util_prep_proportion_check("y", meta_data, ds1,
    report_problems = "message")
  expect_named(per_category$Range$y, c("No", "Yes"))
  expect_equal(per_category$Range$y[["No"]]$upp, 0.5)
  expect_equal(per_category$Range$y[["Yes"]]$low, 0.5)
})

test_that(
  "util_prep_proportion_check reports invalid inputs and missing ranges",
  {
    skip_on_cran()

    ds1 <- data.frame(x = c(0L, 1L))
    attr(ds1, "label_col") <- VAR_NAMES

    meta_data <- data.frame(
      VAR_NAMES = "x",
      DATA_TYPE = DATA_TYPES$INTEGER,
      MISSING_LIST = "",
      PROPORTION_RANGE = "",
      stringsAsFactors = FALSE
    )

    expect_error(
      util_prep_proportion_check("x", meta_data, ds1,
        report_problems = "message"),
      "proprotion check called with bare study data"
    )

    attr(ds1, "apply_fact_md_inadm") <- TRUE
    expect_warning(
      expect_message(
        missing_range <- util_prep_proportion_check("x", meta_data, ds1,
          report_problems = "message"),
        "metadata for a proportion check is missing"
      ),
      "could not be interpreted as an interval"
    )
    expect_true(all(is.na(missing_range$Range$x)))

    meta_data[[PROPORTION_RANGE]] <- "not an interval"
    expect_warning(
      expect_warning(
        invalid_range <- util_prep_proportion_check("x", meta_data, ds1,
          report_problems = "message"),
        "Parser error in REDCap interval"
      ),
      "could not be interpreted as an interval"
    )
    expect_true(all(is.na(invalid_range$Range$x)))

    meta_data[[VALUE_LABELS]] <- "0 = No | 1 = Yes"
    expect_error(
      util_prep_proportion_check("x", meta_data, ds1,
        report_problems = "message"),
      "VALUE_LABELS in util_prep_proportion_check"
    )
  }
)
