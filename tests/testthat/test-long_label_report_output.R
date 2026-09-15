test_that("dq_report2 prints long special-character labels to safe HTML files", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_not_installed("DT")
  skip_if_not_installed("stringdist")
  skip_if_not_installed("markdown")

  withr::local_options(dataquieR.MAX_LABEL_LEN = 60)

  study_data <- data.frame(
    v0001 = c(1, 1, NA, 2, 2, 1),
    v0002 = c(10, 12, 11, NA, 13, 14),
    v0003 = c(2, NA, 4, 3, 2, 4),
    stringsAsFactors = FALSE
  )
  meta_data <- prep_create_meta(
    VAR_NAMES = c("v0001", "v0002", "v0003"),
    LABEL = c(
      paste(
        .dq_unicode_label_samples()[[1]],
        "Größe µg/m³ Δ-baseline: Haben Sie in den letzten 12 Monaten $income / Einkommen > 0 EUR angegeben?" # nolint: line_length_linter.
      ),
      paste(
        .dq_unicode_label_samples()[[3]],
        "Wurde der Wert <5> in C:\\temp\\ecrf dokumentiert oder spaeter korrigiert?" # nolint: line_length_linter.
      ),
      paste(
        .dq_unicode_label_samples()[[11]],
        "Wie lautet die historische oder lokale Bezeichnung in diesem Interview?" # nolint: line_length_linter.
      )
    ),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(
      SCALE_LEVELS$NOMINAL, SCALE_LEVELS$RATIO,
      SCALE_LEVELS$RATIO
    ),
    VALUE_LABELS = c("1 = ja | 2 = nein", NA_character_, NA_character_),
    MISSING_LIST_TABLE = NA_character_,
    MISSING_LIST = SPLIT_CHAR,
    level = VARATT_REQUIRE_LEVELS$REQUIRED,
    character.only = TRUE
  )

  report <- suppressWarningsMatching(
    dq_report2(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      dimensions = "Completeness",
      filter_indicator_functions = "^com_item_missingness$",
      filter_result_slots = "^SummaryTable$",
      cores = NULL
    ),
    "Some variables have labels with more than 60 characters"
  )

  out <- tempfile("dq-long-labels-")
  dir.create(out)
  expect_error(
    suppressMessages(suppressWarnings(print(report,
          dir = out, view = FALSE,
          force_overwrite = TRUE
        ))),
    NA
  )

  html_files <- list.files(out, pattern = "\\.html$", recursive = TRUE)
  expect_true(length(html_files) > 0)
  for (file in basename(html_files)) {
    for (chr in c(
      "<", ">", ":", "\"", "\\", "?", "*", "`", "$",
      "&", "@", "#", ";", "|"
    )) {
      expect_false(grepl(chr, file, fixed = TRUE))
    }
  }
})
