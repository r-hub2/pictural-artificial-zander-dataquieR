test_that("pro_applicability_matrix works", {
  skip_on_cran() # deprecated

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data
    )
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]

  for (max_vars_per_plot in list(
    1:10, -1, -Inf, NA, NaN, complex(real = 1), "A", letters
  )) {
    expect_error(
      appmatrix <- pro_applicability_matrix(
        study_data = study_data,
        meta_data = meta_data,
        label_col = LABEL,
        max_vars_per_plot =
          max_vars_per_plot
      ),
      regexp =
        paste(
          "max_vars_per_plot must be one strictly positive",
          "non-complex integer value, may be Inf."
        ),
      perl = TRUE
    )
  }

  for (max_vars_per_plot in list(
    20, Inf
  )) {
    expect_silent(
      appmatrix <- pro_applicability_matrix(
        study_data = study_data,
        meta_data = meta_data,
        label_col = LABEL,
        max_vars_per_plot =
          max_vars_per_plot
      )
    )
  }

  md0 <- meta_data
  md0$DATA_TYPE <- NULL
  expect_error(
    appmatrix <- pro_applicability_matrix(
      study_data = study_data,
      meta_data = md0,
      label_col = LABEL,
      max_vars_per_plot =
        max_vars_per_plot
    ),
    regexp =
      paste("Missing columns .+DATA_TYPE.+ from .+meta_data.+"),
    perl = TRUE
  )

  md0 <- meta_data
  md0$DATA_TYPE[[2]] <- NA
  expect_warning(
    appmatrix <- pro_applicability_matrix(
      study_data = study_data,
      meta_data = md0,
      label_col = LABEL,
      max_vars_per_plot =
        max_vars_per_plot
    ),
    regexp =
      paste("yielding .+v00001 = string.+"),
    perl = TRUE
  )

  md0 <- meta_data
  md0$DATA_TYPE[[2]] <- "Ordinal"
  expect_warning(
    appmatrix <- pro_applicability_matrix(
      study_data = study_data,
      meta_data = md0,
      label_col = LABEL,
      max_vars_per_plot =
        max_vars_per_plot
    ),
    regexp =
      paste("yielding .+v00001 = string.+"),
    perl = TRUE
  )

  expect_silent(
    appmatrix <- pro_applicability_matrix(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      split_segments = TRUE
    )
  )

  md0 <- meta_data
  if (KEY_STUDY_SEGMENT %in% names(md0)) {
    md0[[KEY_STUDY_SEGMENT]][[2]] <- NA
  }
  if (STUDY_SEGMENT %in% names(md0)) {
    md0[[STUDY_SEGMENT]][[2]] <- NA
  }
  expect_message2(
    appmatrix <- pro_applicability_matrix(
      study_data = study_data,
      meta_data = md0,
      label_col = LABEL,
      split_segments = TRUE
    ),
    regexp =
      paste(
        "Some .+STUDY_SEGMENT.+ are NA.",
        "Will assign those to an artificial segment .+SEGMENT.+"
      ),
    perl = TRUE
  )

  expect_message2(
    appmatrix <- pro_applicability_matrix(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      split_segments = TRUE,
      max_vars_per_plot = 2
    ),
    regexp =
      paste(
        ".*Will split segemnt",
        ".+ arbitrarily avoiding too large figures"
      ),
    perl = TRUE
  )


  md0 <- meta_data
  md0$KEY_STUDY_SEGMENT <- NULL
  md0$STUDY_SEGMENT <- NULL
  expect_message2(
    appmatrix <- pro_applicability_matrix(
      study_data = study_data,
      meta_data = md0,
      label_col = LABEL,
      split_segments = TRUE
    ),
    regexp = paste(
      "Stratification for STUDY_SEGMENT is not",
      "possible due to missing metadata. Will split arbitrarily",
      "avoiding too large figures"
    ),
    perl = TRUE
  )

  appmatrix <- pro_applicability_matrix(
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL
  )

  expect_length(appmatrix$ApplicabilityPlotList, 5)
  expect_lt(abs(suppressWarnings(sum(
    na.rm = TRUE,
    as.numeric(as.matrix(appmatrix$SummaryTable))
  )) - 3149), 5)

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  expect_doppelganger2(
    "appmatrix plot ok",
    appmatrix$ApplicabilityPlot
  )
  expect_doppelganger2(
    "appmatrix segment v10000",
    appmatrix$ApplicabilityPlotList$v10000
  )
})

test_that("pro_applicability_matrix handles local segment splitting", {
  skip_on_cran()

  study_data <- data.frame(
    v1 = 1L,
    v2 = 2L,
    v3 = 3L,
    v4 = 4L
  )
  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = names(study_data),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "",
    JUMP_LIST = "",
    VARIABLE_ROLE = "primary",
    STUDY_SEGMENT = "large",
    stringsAsFactors = FALSE
  )

  expect_message(
    result <- pro_applicability_matrix(
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES,
      split_segments = TRUE,
      max_vars_per_plot = 2
    ),
    "Will split segemnt"
  )

  expect_named(result$ApplicabilityPlotList, c("large#1", "large#2"))
  expect_equal(dim(result$SummaryTable), c(4L, 22L))
  expect_true(all(result$SummaryTable$int_datatype_matrix == 3))
  expect_true(all(result$SummaryTable$int_all_datastructure_dataframe == 4))
  expect_true(all(result$SummaryTable$int_all_datastructure_segment == 4))
})
