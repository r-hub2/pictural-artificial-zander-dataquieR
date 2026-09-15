skip_on_cran()

test_that("prep_clean_labels works", {
  skip_on_cran()
  meta_data1 <- data.frame(
    LABEL =
      c(
        "syst. Blood pressure (mmHg) 1",
        "1st heart frequency in MHz",
        "body surface (\\u33A1)"
      )
  )
  expect_message2(
    expect_equal(
      prep_clean_labels(meta_data1$LABEL),
      c(
        "syst_Blood_pressure_mmHg_1", "st_heart_frequency_in_MHz",
        "body_surface_u33A1_"
      )
    ),
    regexp = "Adjusted labels to be valid variable names."
  )
  expect_message2(
    expect_equal(
      prep_clean_labels("LABEL", meta_data1),
      structure(
        list(LABEL = c(
          "syst_Blood_pressure_mmHg_1",
          "st_heart_frequency_in_MHz",
          "body_surface_u33A1_"
        )),
        row.names = c(NA, -3L), class = "data.frame"
      )
    ),
    regexp = "Adjusted labels in .{1,4}LABEL.{1,4} to be valid variable names."
  )
  meta_data2 <- data.frame(
    LABEL =
      c(
        "syst. Blood pressure (mmHg) 1",
        "syst. Blood pressure  mmHg! 1",
        "body surface (\\u33A1)"
      )
  )
  expect_error(print(prep_clean_labels(meta_data2$LABEL, no_dups = TRUE)),
    regexp = "Have duplicates in desired variable labels"
  )
  expect_error(
    prep_clean_labels(c("same_label", "same_label"), no_dups = TRUE),
    regexp = "Have duplicates in desired variable labels"
  )
  expect_message2(
    expect_equal(
      prep_clean_labels(meta_data2$LABEL, no_dups = FALSE),
      c(
        "syst_Blood_pressure_mmHg_1", "syst_Blood_pressure_mmHg_1",
        "body_surface_u33A1_"
      )
    ),
    regexp = "Adjusted labels to be valid variable names."
  )
})

test_that("prep_clean_labels handles formula-hostile label characters", {
  weird_labels <- c(
    "`Q1`: income$ per month?",
    "Path C:\\temp\\file / visit <1>",
    "blood pressure [mmHg] (syst.)",
    "a | b & c \"quoted\"",
    "delta Größe µg/m³ - Δ-baseline",
    "street Straße – café “quoted”"
  )

  expect_message2(
    clean_labels <- prep_clean_labels(weird_labels, no_dups = TRUE),
    regexp = "Adjusted labels to be valid variable names."
  )

  expect_false(any(grepl("[^a-zA-Z0-9_]", clean_labels)))
  expect_false(any(grepl("^[^a-zA-Z]", clean_labels)))
  expect_equal(length(unique(clean_labels)), length(clean_labels))

  dat <- data.frame(
    setNames(
      c(
        list(
          seq_len(8),
          seq(2, 16, by = 2),
          rep(c(0, 1), 4),
          rep(c(1, 2), each = 4)
        ),
        rep(list(rep(c(0, 1), 4)), length(clean_labels) - 4L)
      ),
      clean_labels
    ),
    check.names = FALSE
  )
  fmla <- as.formula(paste0(
    clean_labels[[1]], " ~ ",
    paste(clean_labels[-1], collapse = " + ")
  ))

  expect_silent(stats::lm(fmla, data = dat))
})

test_that("prep_clean_labels validates missing and factor label input", {
  expect_error(
    prep_clean_labels(),
    "Need at least"
  )

  factor_labels <- factor(c("1 bad label", "second.label"))
  expect_message2(
    cleaned <- prep_clean_labels(factor_labels),
    regexp = "Adjusted labels to be valid variable names."
  )

  expect_identical(cleaned, c("bad_label", "second_label"))
})
