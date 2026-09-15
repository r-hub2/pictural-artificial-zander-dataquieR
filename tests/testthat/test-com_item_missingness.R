test_that("com_item_missingness works", {
  skip_on_cran() # slow and errors will be obvious.

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  legacy_summary_sum <- function(summary_table) {
    new_metrics <- c(
      "NUM_int_vfe_missunc", "PCT_int_vfe_missunc",
      "NUM_com_qum_spec", "PCT_com_qum_spec"
    )
    legacy_metrics <- setdiff(colnames(summary_table), new_metrics)
    legacy_summary <- summary_table[, legacy_metrics, drop = FALSE]
    sum(as.numeric(as.matrix(legacy_summary)), na.rm = TRUE)
  }

  md0 <- meta_data
  md0$STUDY_SEGMENT <- NULL
  expect_message2(invisible(
    com_item_missingness(
      resp_vars = "v00001",
      study_data = study_data,
      item_level = md0,
      suppressWarnings = TRUE
    )
  ))

  item_missingness_result <- com_item_missingness(
    resp_vars = "v00001",
    study_data = study_data,
    item_level = md0,
    suppressWarnings = TRUE,
    threshold_value = 100,
    include_sysmiss = FALSE,
    drop_levels = TRUE,
    assume_consistent_codes = TRUE,
    expand_codes = TRUE,
    show_causes = TRUE,
    expected_observations =
      "ALL"
  )
  expect_equal(item_missingness_result$SummaryTable$GRADING, FALSE,
    ignore_attr = TRUE
  )
  expect_identical(
    attr(
      item_missingness_result$SummaryTable$GRADING,
      DATA_TYPE
    ),
    DATA_TYPE_LOGICAL
  )

  skip_on_cran() # too many tests make things slow too.

  expect_error(invisible(
    com_item_missingness(study_data, meta_data,
      suppressWarnings = TRUE,
      include_sysmiss = 1:10
    )
  ), regexp = "Need exactly one element in argument include_sysmiss")

  w <- capture_warnings(invisible(
    com_item_missingness(
      resp_vars = "v00001",
      study_data = study_data,
      item_level = meta_data,
      suppressWarnings = TRUE,
      include_sysmiss = TRUE
    )
  ))
  w <- gsub("(\n|^|\r)+>.*$", "", w)
  expect_false(any(grepl("include_sysmiss", w)))

  w <- capture_warnings(invisible(
    com_item_missingness(
      resp_vars = "v00001",
      study_data = study_data,
      item_level = meta_data,
      suppressWarnings = TRUE,
      include_sysmiss = FALSE
    )
  ))
  w <- gsub("(\n|^|\r)+>.*$", "", w)
  expect_false(any(grepl("include_sysmiss", w)))

  expect_message2(
    invisible(
      com_item_missingness(
        resp_vars = "v00001",
        study_data = study_data,
        item_level = meta_data,
        suppressWarnings = TRUE
      )
    ),
    regexp =
      paste(
        "The mandatory argument threshold_value was",
        "not defined and is set to the default of 90%."
      )
  )

  expect_error(invisible(
    com_item_missingness(
      resp_vars = "v00001",
      study_data = study_data,
      item_level = meta_data, suppressWarnings = NA
    )
  ), regexp = "Argument suppressWarnings must not contain NAs")

  expect_error(
    invisible(
      com_item_missingness(
        study_data = study_data,
        item_level = meta_data, suppressWarnings = "xx"
      )
    ),
    regexp =
      paste("Argument suppressWarnings must be logical")
  )

  expect_error(invisible(
    com_item_missingness(
      resp_vars = "v00001",
      study_data = study_data,
      item_level = meta_data, suppressWarnings = 1:2
    )
  ), regexp = "Need exactly one element in argument suppressWarnings, got 2")

  w <- capture_warnings(invisible(
    com_item_missingness(
      resp_vars = "v00001",
      study_data = study_data,
      item_level = meta_data, suppressWarnings = TRUE
    )
  ))
  w <- gsub("(\n|^|\r)+>.*$", "", w)
  expect_false(any(grepl("Setting suppressWarnings to its default FALSE", w)))

  expect_error(invisible(
    com_item_missingness(
      study_data = study_data,
      item_level = meta_data, suppressWarnings = c(
        TRUE,
        FALSE
      )
    )
  ), regexp = "Need exactly one element in argument suppressWarnings, got 2")


  w <- capture_warnings(invisible(
    com_item_missingness(
      resp_vars = "v00001",
      study_data = study_data,
      item_level = meta_data, suppressWarnings = c(TRUE)
    )
  ))
  w <- gsub("(\n|^|\r)+>.*$", "", w)
  expect_false(any(grepl(
    "Setting suppressWarnings to its default FALSE",
    w
  )))


  expect_error(
    invisible(
      com_item_missingness(
        resp_vars = "v00001",
        study_data = study_data,
        item_level = meta_data, suppressWarnings = TRUE,
        cause_label_df = 42, threshold_value = .9
      )
    ),
    regexp =
      paste(".+cause_label_df.+ is not a data frame.")
  )

  expect_message2(invisible(
    com_item_missingness(
      resp_vars = "v00001",
      study_data = study_data,
      item_level = meta_data, suppressWarnings = TRUE,
      threshold_value = "XX"
    )
  ), regexp = paste(
    "Could not convert threshold_value .+XX.+ to a number.",
    "Set to default value 90%."
  ), perl = TRUE)

  cause_label_df <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|missing_table") # nolint: line_length_linter.

  cause_label_df$CODE_VALUE[16] <-
    cause_label_df$CODE_VALUE[15]
  suppressMessages(suppressWarnings(expect_warning(
    invisible(
      com_item_missingness(
        resp_vars = "v00001",
        study_data = study_data,
        item_level = meta_data, suppressWarnings = FALSE,
        cause_label_df = cause_label_df
      )
    ),
    regexp =
      "code.* with more than one meaning"
  )))

  suppressMessages(expect_conditions(
    {
      i1 <- com_item_missingness(
        study_data = study_data,
        item_level = meta_data,
      )
    },
    regexps =
      c(
        paste(
          "There are \\d+ meassurements",
          "of .+",
          "for participants not being",
          "part of one of the segments .+"
        ),
        paste(
          "There are \\d+ meassurements",
          "of .+",
          "for participants not being",
          "part of one of the segments .+"
        ),
        paste(
          "There are \\d+ meassurements",
          "of .+",
          "for participants not being",
          "part of one of the segments .+"
        ),
        paste(
          "There are \\d+ meassurements",
          "of .+",
          "for participants not being",
          "part of one of the segments .+"
        )
      ),
    classes = c("warning")
  ))
  expect_lt(suppressWarnings(
    abs(
      legacy_summary_sum(i1$SummaryTable) - 479149.9
    )
  ), 50)

  code_labels <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|missing_table") # nolint: line_length_linter.
  suppressMessages(expect_conditions(
    {
      i2 <- com_item_missingness(
        study_data = study_data,
        item_level = meta_data,
        cause_label_df = code_labels
      )
    },
    regexps =
      c(
        paste(".*more than one meaning.*"),
        paste(
          "Combining .+ and assignments in .+ in",
          ".+ is discouraged. This may cause",
          "errors."
        ),
        paste(
          "There are \\d+ meassurements",
          "of .+",
          "for participants not being",
          "part of one of the segments .+"
        ),
        paste(
          "There are \\d+ meassurements",
          "of .+",
          "for participants not being",
          "part of one of the segments .+"
        ),
        paste(
          "There are \\d+ meassurements",
          "of .+",
          "for participants not being",
          "part of one of the segments .+"
        ),
        paste(
          "There are \\d+ meassurements",
          "of .+",
          "for participants not being",
          "part of one of the segments .+"
        ),
        paste(
          "The argument .+cause_label_df.+ has been",
          "deprecated. It",
          "will be in a future",
          "version be removed."
        )
      ),
    classes = c("warning")
  ))
  expect_lt(suppressWarnings(
    abs(
      legacy_summary_sum(i2$SummaryTable) - 479149.9
    )
  ), 50)

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  expect_doppelganger2(
    "item missingness no labels",
    i1$SummaryPlot
  )
  expect_doppelganger2(
    "item missingness w labels",
    i2$SummaryPlot
  )
})
