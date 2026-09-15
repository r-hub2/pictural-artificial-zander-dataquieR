test_that("acc_cat_distributions reports missing categorical applicability", {
  skip_on_cran()

  study_data <- data.frame(num = c(1, 2, 3))
  meta_data <- data.frame(
    VAR_NAMES = "num",
    LABEL = "num",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  expect_error(
    suppressMessages(acc_cat_distributions(
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES
    )),
    "No suitable variables were defined"
  )
})

test_that(
  paste0(
    "acc_cat_distributions returns an empty plot when only ",
    "group variable remains"
  ),
  {
    skip_on_cran()

    study_data <- data.frame(group = c("a", "b", "a"))
    meta_data <- data.frame(
      VAR_NAMES = "group",
      LABEL = "group",
      DATA_TYPE = DATA_TYPES$STRING,
      SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
      MISSING_LIST = SPLIT_CHAR,
      JUMP_LIST = SPLIT_CHAR,
      stringsAsFactors = FALSE
    )

    captured <- new.env(parent = emptyenv())
    captured$warnings <- character(0)
    result <- withCallingHandlers(
      suppressMessages(acc_cat_distributions(
        resp_vars = "group",
        group_vars = "group",
        study_data = study_data,
        meta_data = meta_data,
        label_col = VAR_NAMES
      )),
      warning = function(w) {
        captured$warnings <- c(captured$warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

    expect_true(any(grepl("Removed grouping variable", captured$warnings)))
    expect_true(any(grepl("No variables left to analyse", captured$warnings)))
    expect_named(result, "SummaryPlot")
    expect_s3_class(result$SummaryPlot, "ggplot")
  }
)
