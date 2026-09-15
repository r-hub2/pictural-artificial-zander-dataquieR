test_that("util_map_all works", {
  skip_on_cran()

  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  md <- prep_create_meta(
    VAR_NAMES = letters,
    DATA_TYPE = DATA_TYPES$FLOAT,
    LABEL = LETTERS,
    MISSING_LIST = "",
    nums = seq_along(letters)
  )
  sd <- as.data.frame(lapply(setNames(nm = letters), paste, 1:10),
    stringsAsFactors = FALSE
  )
  mapped <- util_map_all(
    label_col = LABEL,
    study_data = sd,
    meta_data = md
  )

  expect_equal(colnames(mapped$df), LETTERS)

  mapped <- util_map_all(
    label_col = "nums",
    study_data = sd,
    meta_data = md
  )

  expect_error(
    util_map_all(
      label_col = c("nums", "kkk"),
      study_data = sd,
      meta_data = md
    ),
    regexp =
      paste(
        "label_col must be exactly 1 metadata attribute,",
        "neither a vector nor NULL."
      )
  )

  expect_error(
    util_map_all(
      label_col = c(),
      study_data = sd,
      meta_data = md
    ),
    regexp =
      paste(
        "label_col must be exactly 1 metadata attribute,",
        "neither a vector nor NULL."
      )
  )

  expect_error(util_map_all(
    label_col = "NO NO NO NO NO",
    study_data = sd,
    meta_data = md
  ), regexp = paste(
    "label_col .+NO NO NO NO NO.+ not found in metadata.",
    "Did you mean .+VAR_NAMES.+"
  ))

  expect_error(util_map_all(
    label_col = "speed",
    study_data = sd,
    meta_data = cars
  ), regexp = paste(".*VAR_NAMES not found in metadata."))

  expect_error(
    util_map_all(
      label_col = "DATA_TYPE",
      study_data = sd,
      meta_data = md
    ),
    regexp =
      paste(
        "The following .+DATA_TYPE.+ are duplicated in the",
        "metadata and cannot be used as label therefore: .+float.+"
      )
  )

  mdx <- md
  mdx$VAR_NAMES[[5]] <- mdx$VAR_NAMES[[1]]
  expect_error(
    util_map_all(
      label_col = LABEL,
      study_data = sd,
      meta_data = mdx
    ),
    regexp =
      paste(
        "The following variable names are duplicated in the",
        "metadata and cannot be used as label therefore: .+a.+"
      )
  )

  mdx <- rbind(
    md,
    md[1:2, , drop = FALSE]
  )
  mdx$VAR_NAMES[(nrow(md) + 1):nrow(mdx)] <- "extra"
  mdx$LABEL[(nrow(md) + 1):nrow(mdx)] <- c("Extra 1", "Extra 2")
  mapped_irrelevant_dups <- util_map_all(
    label_col = LABEL,
    study_data = sd,
    meta_data = mdx
  )
  expect_equal(colnames(mapped_irrelevant_dups$df), LETTERS)

  sd_with_extra <- sd
  sd_with_extra$extra <- seq_len(nrow(sd_with_extra))
  mapped_irrelevant_study_dups <- util_map_all(
    label_col = LABEL,
    study_data = sd_with_extra,
    meta_data = mdx,
    relevant_var_names = letters
  )
  expect_equal(colnames(mapped_irrelevant_study_dups$df), LETTERS)

  mdx <- rbind(
    md,
    md[1:2, , drop = FALSE]
  )
  mdx$VAR_NAMES[(nrow(md) + 1):nrow(mdx)] <- c("extra_1", "extra_2")
  mdx$LABEL[(nrow(md) + 1):nrow(mdx)] <- "Extra"
  mapped_irrelevant_dups <- util_map_all(
    label_col = LABEL,
    study_data = sd,
    meta_data = mdx
  )
  expect_equal(colnames(mapped_irrelevant_dups$df), LETTERS)

  mdx <- md
  mdx$VAR_NAMES[[5]] <- NA
  expect_error(util_map_all(
    label_col = LABEL,
    study_data = sd,
    meta_data = mdx
  ), regexp = paste(
    "For the following variables, some variable names are",
    "missing in the metadata: Variable No. #5"
  ))

  mdx <- md
  mdx$LABEL[[5]] <- NA
  expect_error(util_map_all(
    label_col = LABEL,
    study_data = sd,
    meta_data = mdx
  ), regexp = paste(
    "For the following variables, some .+LABEL.+ are missing",
    "in the metadata and cannot be used as label therefore:",
    "Variable No. #5"
  ))

  mdx <- md
  mdx$VAR_NAMES[[1]] <- "xxx"
  mdx$VAR_NAMES[[4]] <- "yyy"

  withr::with_options(
    list(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "exact"),
    expect_warning(
      expect_warning(
        expect_message2(
          expect_message2(
            invisible(capture.output(util_map_all(
              label_col = LABEL,
              study_data = sd,
              meta_data = mdx
            ))),
            regexp = paste(
              "Did not find any metadata for the following",
              "variables from the study data: .+a.+, .+d.+"
            ),
            perl = TRUE
          ),
          regexp = paste(
            "Found metadata for the following variables not",
            "found in the study data: .+xxx.+, .+yyy.+"
          ),
          perl = TRUE
        ),
        regexp = paste(
          "Lost 7.7% of the study data because",
          "of missing/not assignable metadata"
        ),
        perl = TRUE
      ),
      regexp = paste(
        "Lost 7.7% of the metadata because of",
        "missing/not assignable study data"
      ),
      perl = TRUE
    )
  )

  mdx <- md
  mdx$LABEL[[5]] <- "    "
  expect_error(util_map_all(
    label_col = LABEL,
    study_data = sd,
    meta_data = mdx
  ), regexp = paste(
    "Mapping of metadata on study data yielded invalid",
    "variable labels:"
  ))

  expect_equal(colnames(mapped$df), as.character(seq_along(letters)))
})

test_that("util_map_all validates empty metadata and relevant-name inputs", {
  skip_on_cran()

  md <- prep_create_meta(
    VAR_NAMES = letters[1:3],
    DATA_TYPE = DATA_TYPES$FLOAT,
    LABEL = LETTERS[1:3],
    MISSING_LIST = ""
  )
  sd <- data.frame(a = 1:2, b = 3:4, c = 5:6)

  expect_error(
    util_map_all(label_col = LABEL, study_data = sd, meta_data = md[0, ]),
    "meta_data.+empty"
  )

  expect_error(
    util_map_all(
      label_col = LABEL,
      study_data = sd,
      meta_data = md,
      relevant_var_names = 1:2
    ),
    "relevant_var_names.+character vector or NULL"
  )
})

test_that("util_map_all rejects duplicated labels after mapping", {
  skip_on_cran()

  md <- prep_create_meta(
    VAR_NAMES = c("a", "b"),
    DATA_TYPE = DATA_TYPES$FLOAT,
    LABEL = c("A", "B"),
    MISSING_LIST = ""
  )
  sd <- data.frame(a = 1:2, b = 3:4)

  testthat::local_mocked_bindings(
    util_map_labels = function(...) {
      c("same", "same")
    }
  )

  expect_error(
    util_map_all(label_col = LABEL, study_data = sd, meta_data = md),
    "duplicated variable labels"
  )
})

test_that("util_map_all can suppress one side of mismatch reporting", {
  skip_on_cran()

  md <- prep_create_meta(
    VAR_NAMES = c("a", "metadata_only"),
    DATA_TYPE = DATA_TYPES$FLOAT,
    LABEL = c("A", "Metadata only"),
    MISSING_LIST = ""
  )
  sd <- data.frame(a = 1:2, study_only = 3:4)

  subset_m_warnings <- capture_warnings(
    withr::with_options(
      list(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "subset_m"),
      util_map_all(label_col = LABEL, study_data = sd, meta_data = md)
    )
  )
  expect_true(any(grepl("Lost 50% of the metadata", subset_m_warnings)))
  expect_false(any(grepl("Lost 50% of the study data", subset_m_warnings)))

  subset_u_warnings <- capture_warnings(
    withr::with_options(
      list(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "subset_u"),
      util_map_all(label_col = LABEL, study_data = sd, meta_data = md)
    )
  )
  expect_true(any(grepl("Lost 50% of the study data", subset_u_warnings)))
  expect_false(any(grepl("Lost 50% of the metadata", subset_u_warnings)))
})

test_that("util_map_all suppresses mismatch reporting in pipeline mode", {
  skip_on_cran()

  md <- prep_create_meta(
    VAR_NAMES = c("a", "metadata_only"),
    DATA_TYPE = DATA_TYPES$FLOAT,
    LABEL = c("A", "Metadata only"),
    MISSING_LIST = ""
  )
  sd <- data.frame(a = 1:2, study_only = 3:4)

  old_called_in_pipeline <- .dq2_globs$.called_in_pipeline
  .dq2_globs$.called_in_pipeline <- TRUE
  withr::defer(.dq2_globs$.called_in_pipeline <- old_called_in_pipeline)

  expect_silent(
    mapped <- util_map_all(label_col = LABEL, study_data = sd, meta_data = md)
  )
  expect_named(mapped$df, "A")
  expect_identical(mapped$df$A, sd$a)
})
