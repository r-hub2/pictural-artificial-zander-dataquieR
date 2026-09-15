skip_on_cran()

test_that("dq_report2 guards caller-owned clusters before computation", {
  fake_cl <- structure(list(), class = "cluster")

  testthat::local_mocked_bindings(
    util_guard_rstudio_user_cluster = function(cores, advanced_options) {
      expect_identical(cores, fake_cl)
      expect_identical(advanced_options, list())
      stop("guarded before computation", call. = FALSE)
    }
  )

  expect_error(dq_report2(cores = fake_cl), "guarded before computation")
})

test_that("dq_report2 uses study data from dataframe-level metadata", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  input_dir <- tempfile()
  dir.create(input_dir)
  study_data <- data.frame(id = 1:3, x = c(1L, NA_integer_, 3L))
  study_data_file <- file.path(input_dir, "study_data.csv")
  utils::write.csv(study_data, study_data_file, row.names = FALSE)

  item_level <- data.frame(
    VAR_NAMES = c("id", "x"),
    LABEL = c("ID", "X"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$METRIC),
    MISSING_LIST = c("|", "|"),
    JUMP_LIST = c("|", "|")
  )
  dataframe_level <- data.frame(
    DF_NAME = study_data_file,
    DF_ID_VARS = "id",
    stringsAsFactors = FALSE
  )

  prep_add_data_frames(
    item_level = item_level,
    dataframe_level = dataframe_level
  )

  expect_message(
    report <- suppressWarnings(dq_report2(
      meta_data = "item_level",
      meta_data_dataframe = "dataframe_level",
      label_col = LABEL,
      resp_vars = "x",
      dimensions = "Completeness",
      filter_indicator_functions = "^com_item_missingness$",
      filter_result_slots = "^SummaryTable$",
      cores = 1
    )),
    "dataframe level metadata"
  )

  expect_s3_class(report, "dataquieR_resultset2")
  expect_equal(
    prep_get_data_frame("study_data"),
    study_data,
    ignore_attr = TRUE
  )
})

test_that("dq_report2 works", {
  skip_if_not_installed("DT")
  skip_if_not_installed("stringdist")
  skip_if_not_installed("markdown")
  skip_on_cran() # slow, parallel, ...

  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx") # nolint: line_length_linter.

  study_data <- head(prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE), 100) # nolint: line_length_linter.
  meta_data <- prep_get_data_frame("item_level")

  mlt <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx| missing_table") # nolint: line_length_linter.

  prep_purge_data_frame_cache()

  prep_add_data_frames(`missing_table` = mlt)

  invisible(testthat::capture_output_lines(gc(full = TRUE, verbose = FALSE)))

  sd0 <- study_data[, 1:5]
  sd0$v00012 <- study_data$v00012
  md0 <- subset(meta_data, VAR_NAMES %in% colnames(sd0))
  md0$PART_VAR <- NULL

  # The test intentionally keeps missing-list-table metadata in md0.

  # don't include huge reports as RData in the package
  # Suppress warnings since we do not test dq_report2
  # here in the first place
  report <- dq_report2(sd0, md0,
    resp_vars = c(
      "v00000", "v00001", "v00002",
      "v00003", "v00004", "v00012"
    ),
    filter_indicator_functions =
      c(
        "^com_item_missingness$",
        "^acc_varcomp$"
      ),
    filter_result_slots =
      c("^SummaryTable$"),
    cores = NULL,
    dimensions = # for speed, omit Accuracy
      c(
        "Integrity",
        "Completeness",
        "Consistency",
        "Accuracy"
      )
  )

  sts <- report[, "com_item_missingness", "SummaryTable"]

  expect_equal(sum(sts$SummaryTable$GRADING), 1)

  expect_silent(invisible(summary(report)))

  r <- report
  r$acc_varcomp_observer.SBP_0$SummaryTable <- NULL
  expect_silent(summary(r))

  r <- report
  r$acc_varcomp_observer.SBP_0$SummaryTable <-
    r$acc_varcomp_observer.SBP_0$SummaryTable[FALSE, , FALSE]
  expect_silent(summary(r))

  expect_error(
    report <-
      suppressWarnings(suppressMessages(dq_report2(sd0, md0,
            cores = NULL,
            dimensions = 42
          ))),
    regexp =
      sprintf(
        "The argument %s must be character or NULL",
        sQuote("dimensions")
      ),
    perl = TRUE
  )


  expect_warning(
    report <-
      (dq_report2(sd0, md0,
        resp_vars = c(
          "v00000", "v00001", "v00002",
          "v00003", "v00004", "v00012"
        ),
        filter_indicator_functions =
        c(
          "^com_item_missingness$",
          "^acc_varcomp$"
        ),
        filter_result_slots =
        c("^SummaryTable$"),
        cores = NULL,
        dimensions = c("invalid"),
      )),
    regexp =
      paste(
        "(?ms)Missing",
        ".+invalid.+from.+Completeness.+Consis.+Accuracy.+Integrity.+did",
        "you mean.+Integrity.+"
      ),
    perl = TRUE
  )

  # The second report call also keeps missing-list-table metadata in md0.

  expect_silent(
    report <-
      suppressMessages(dq_report2(sd0, md0,
        resp_vars = c(
          "v00000", "v00001", "v00002",
          "v00003", "v00004", "v00012"
        ),
        filter_indicator_functions =
          c(
            "^com_item_missingness$",
            "^acc_varcomp$"
          ),
        filter_result_slots =
          c("^SummaryTable$"),
        cores = NULL,
        strata_attribute = "GROUP_VAR_XXX",
        dimensions = c("Completeness"),
      ))
  )

  report <- suppressWarnings(dq_report2(sd0, md0,
    cores = NULL,
    label_col = LABEL,
    dimensions = # for speed, omit Accuracy
      c(
        "Completeness",
        "Consistency",
        "Accuracy"
      ),
    resp_vars = c("SBP_0", "SEX_0"),
    filter_indicator_functions =
      c(
        "^com_item_missingness$",
        "^acc_distributions_loc$",
        "^acc_margins$"
      ),
    filter_result_slots =
      c("^SummaryTable$"),
    specific_args = list(
      acc_margins =
      list(min_obs_in_subgroup = 40),
      acc_distributions_loc =
      list(flip_mode = "flip"),
      com_item_missingness = list(
        label_col = LONG_LABEL
      )
    )
  ))

  expect_equal(
    util_attr(report$acc_margins_observer.SBP_0, "call",
      exact = TRUE
    )[["min_obs_in_subgroup"]],
    40
  )

  expect_equal(
    util_attr(report$acc_distributions_loc.SBP_0, "call",
      exact = TRUE
    )[["flip_mode"]],
    "flip"
  )

  expect_null(
    util_attr(report$acc_distributions_loc_ecdf_observer.SBP_0,
      "call",
      exact = TRUE
    )[["flip_mode"]]
  )

  expect_equal(
    util_attr(report$com_item_missingness.SBP_0, "call",
      exact = TRUE
    )[["label_col"]],
    LABEL
  ) # this should not be overwritable

  report <- suppressWarnings(dq_report2(sd0, md0,
    resp_vars = c("SBP_0", "SEX_0"),
    filter_indicator_functions =
      c(
        "^acc_distributions_loc_ecdf$",
        "^acc_distributions_loc$"
      ),
    cores = NULL,
    flip_mode = "flip",
    dimensions = # for speed, omit Accuracy
      c(
        "Completeness",
        "Consistency",
        "Accuracy"
      )
  ))

  expect_equal(
    util_attr(report$acc_distributions_loc.SBP_0, "call",
      exact = TRUE
    )$resp_vars,
    "SBP_0"
  ) # resp_vars cannot be overwritten, but SEX_0 is not a pimary output

  expect_equal(
    util_attr(report$acc_distributions_loc.SBP_0, "call",
      exact = TRUE
    )$flip_mode,
    "flip"
  )

  md1 <- md0
  md1$LABEL <- c(
    "CENTER_0",
    "",
    "CENTER_0 DUPLICATE", # will become a duplicated label
    "CENTER_0", # direct duplication of the first label
    "Have you been physically vigorously active in the past 12 hours ('physically vigorously active' means at least 30 minutes of jogging or fast cycling, digging up your garden, carrying heavy objects weighing more than 10 kg for a long time, or similar physical activities)?", # very long label # nolint: line_length_linter.
    "Hybpvaitp1hpvamal3mojofcduygchowmt1kfaltospa"
  ) # legacy abbreviation-like label
  md1$VAR_NAMES[2] <- "yOvCzPY60JRjmrYb16Tsd6qMymal4B5Skw9rZ5PHSCtaBqOVglAKcguPkQhakampFJcC8xqLbZJs7kZUdKH804pbOmM5ORPVabrkEkVkiWbakWiixZ99NRYF6BP8SRxzNYY2tED7DjmhMUwk0t674RjH828jq9zoTJgDxYP6nEdHBxhmXJh0ClCPjGsi1q" # very long variable name that should get caught and not be used as label as it is # nolint: line_length_linter.
  colnames(sd0)[2] <- md1$VAR_NAMES[2]

  suppressWarningsMatching(
    expect_warning(
      expect_warning(
        report <- dq_report2(sd0, md1,
          label_col = LABEL,
          filter_indicator_functions = "int_datatype_matrix",
          cores = NULL,
          dimensions =
            c(
              "Integrity",
              "Consistency"
            )
        ),
        regexp = ".*Labels are required to create a report.*"
      ),
      regexp = ".*Unique labels are required to create a report.*"
    ),
    c(
      "Some variables have labels with more than 60 characters in .+LABEL.+",
      "Unique labels are required",
      ".*duplicated in the metadata and cannot be used as label.*"
    )
  )
  suppressWarningsMatching(
    expect_warning(
      report <- dq_report2(sd0, md1,
        label_col = VAR_NAMES,
        filter_indicator_functions = "int_datatype_matrix",
        cores = NULL,
        dimensions =
          c(
            "Integrity",
            "Consistency"
          )
      ),
      regexp = ".*This will cause suboptimal outputs and possibly.*"
    ),
    c(
      "Some variables have no label in .+LABEL", "Unique labels are required",
      "more than 60 characters"
    )
  )

  md1$VAR_NAMES[2] <- "" # this will be considered different
  colnames(sd0)[2] <- "" # because both are missing, and NA maybe unequal
  # from NA

  suppressWarningsMatching(
    expect_warning(
      expect_warning(
        expect_warning(
          report <- dq_report2(sd0, md1,
            label_col = LABEL,
            filter_indicator_functions = "int_datatype_matrix",
            cores = NULL,
            dimensions =
              c(
                "Integrity",
                "Consistency"
              )
          ),
          regexp = "Need.+VAR_NAMES.+in.*meta_data",
          perl = TRUE
        ),
        regexp = "Some variables have duplicated labels in .+LABEL.+",
        perl = TRUE
      ),
      regexp =
        "Some variables have labels with more than 60 characters in .+LABEL.+",
      perl = TRUE
    ),
    c(
      "(Some variables have duplicated labels in .+LABEL.+|Need.+VAR_NAMES.+in.*meta_data|Some variables have labels with more than 60 characters in .+LABEL.+|Lost 16.7% of the study data because of missing/not assignable metadata|.+dummy names|Some variables have no label in .+LABEL)", # nolint: line_length_linter.
      ".*duplicated in the metadata and cannot be used as label.*"
    )
  )

  suppressWarningsMatching(
    report <- dq_report2(sd0, md1,
      label_col = VAR_NAMES,
      filter_indicator_functions = "int_datatype_matrix",
      cores = NULL,
      dimensions =
        c(
          "Integrity",
          "Consistency"
        )
    ),
    "(Unique labels are required|Some variables have labels with more than 60 characters in .+LABEL.+|Lost 16.7% of the study data because of missing/not assignable metadata|Need.+VAR_NAMES.+discard.+|.+dummy names|Some variables have no label in .+LABEL)" # nolint: line_length_linter.
  )

  # warning message that there is no metadata
  prep_purge_data_frame_cache()
  study_data <- head(
    prep_get_data_frame(
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData" # nolint: line_length_linter.
    ),
    50
  )

  expect_warning(
    dq_report2(
      study_data = study_data,
      filter_indicator_functions = "int_datatype_matrix",
      cores = NULL,
      dimensions = "Integrity"
    ),
    regexp = "NO ITEM LEVEL METADATA.+PLEASE CONSIDER PASSING.+"
  )


  md1 <-
    prep_get_data_frame(
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx| item_level" # nolint: line_length_linter.
    )
  md1 <- md1[, !colnames(md1) %in% c("VARIABLE_ROLE", "MISSING_LIST_TABLE")]


  # message about no assigned variable roles
  suppressWarningsMatching(
    dq_report2(
      study_data = study_data,
      meta_data = md1,
      filter_indicator_functions = "int_datatype_matrix",
      cores = NULL,
      dimensions = "Integrity"
    ),
    "Metadata does not provide.+for replacing codes with NAs."
  )

  md1 <-
    prep_get_data_frame(
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx| item_level" # nolint: line_length_linter.
    )
  md1 <- md1[, !colnames(md1) %in% "MISSING_LIST_TABLE"]
  md1[1, VARIABLE_ROLE] <- "non-existing_role"


  # message about non existing role (ignore warnings about missing JUMP_LIST and
  # MISSING_LIST)
  suppressWarningsMatching(
    dq_report2(
      study_data = study_data,
      meta_data = md1,
      filter_indicator_functions = "int_datatype_matrix",
      cores = NULL,
      dimensions = "Integrity"
    ),
    "Metadata does not provide.+for replacing codes with NAs."
  )


  # message of variable not found in the metadata
  md1 <- md1[, colnames(md1) %in% c(
    "VAR_NAMES", "LABEL", "DATA_TYPE",
    "SCALE_LEVEL", "VALUE_LABELS"
  )]
  md1 <- rbind(md1, data.frame(
    VAR_NAMES = "no_real_var_name",
    LABEL = "no_var_name",
    DATA_TYPE = "integer",
    SCALE_LEVEL = "nominal",
    VALUE_LABELS = NA_character_
  ))

  expect_message2(dq_report2(
    study_data = study_data,
    meta_data = md1,
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL,
    dimensions = "Integrity"
  ))
})

test_that("dq_report2 leaves statistical settings to report generation", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  prep_add_data_frames(
    statistical_settings = data.frame(
      SETTING_ID = "rm_default",
      METHOD = "rmse",
      stringsAsFactors = FALSE
    )
  )

  study_data <- data.frame(x = c(1, 2, 3))
  meta_data <- data.frame(
    VAR_NAMES = "x",
    LABEL = "x",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  report <- suppressWarnings(suppressMessages(dq_report2(
    study_data = study_data,
    item_level = meta_data,
    dimensions = "Completeness",
    filter_indicator_functions = "^com_item_missingness$",
    filter_result_slots = "^SummaryTable$",
    cores = NULL
  )))

  referred_tables <- util_attr(report, "referred_tables", exact = TRUE)
  expect_false("statistical_settings" %in% names(referred_tables))
  expect_equal(
    prep_get_data_frame("statistical_settings"),
    data.frame(
      SETTING_ID = "rm_default",
      METHOD = "rmse",
      stringsAsFactors = FALSE
    )
  )
})

test_that("anti-join helper keeps cross-join semantics explicit", {
  x <- data.frame(a = 1:2)
  y <- data.frame(b = 1)
  expect_equal(util_anti_join_common(x, y), x[FALSE, , drop = FALSE])
  expect_equal(util_anti_join_common(x, y[FALSE, , drop = FALSE]), x)
})

test_that("no issues with parallel running reports", {
  skip_if_not_installed("DT")
  skip_if_not_installed("stringdist")
  skip_if_not_installed("markdown")
  skip_on_cran() # needs maybe too many cores
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")

  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx") # nolint: line_length_linter.

  study_data <- head(prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE), 100) # nolint: line_length_linter.
  meta_data <- prep_get_data_frame("item_level")

  mlt <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx| missing_table") # nolint: line_length_linter.

  prep_purge_data_frame_cache()

  prep_add_data_frames(`missing_table` = mlt)

  invisible(testthat::capture_output_lines(gc(full = TRUE, verbose = FALSE)))

  sd0 <- study_data[, 1:5]
  sd0$v00012 <- study_data$v00012
  md0 <- subset(meta_data, VAR_NAMES %in% colnames(sd0))
  md0$PART_VAR <- NULL

  r <- with_interactive_dataquieR_wrapper(
    withr::with_options( # otherwise, cores = 2 is disallowed
      list(
        dataquieR.tmp_no_load_all = TRUE, # even in testthat do not run load_all in this specific test, it would hide possible problems in the default code branch not being executed under load_all in util_evalute_calls # nolint: line_length_linter.
        dataquieR.dt_adjust = FALSE
      ),
      { # speed up
        dq_report2(sd0, md0,
          resp_vars = c(
            "v00000", "v00001", "v00002",
            "v00003", "v00004", "v00012"
          ),
          filter_indicator_functions =
            c(
              "^com_item_missingness$",
              "^acc_varcomp$"
            ),
          filter_result_slots =
            c("^SummaryTable$"),
          cores = 1,
          mode = "queue",
          dimensions = # for speed, omit Accuracy
            c(
              "Integrity",
              "Completeness",
              "Consistency",
              "Accuracy"
            )
        )
      }
    )
  )

  res_is_null <- vapply(r,
    inherits, "dataquieR_NULL",
    FUN.VALUE = logical(1)
  )

  res_is_iap <-
    any(vapply(r,
      function(rres) {
        if (length(util_attr(rres, "error", exact = TRUE)) > 1) {
          fail("We should not have > 1 error per result")
        }
        if (length(util_attr(rres, "error", exact = TRUE)) == 0) {
          FALSE
        } else {
          vapply(util_attr(rres, "error", exact = TRUE),
            inherits, dataquieR.intrinsic_applicability_problem,
            FUN.VALUE = logical(1)
          )
        }
      },
      FUN.VALUE = logical(1)
    ))

  res_is_ap <-
    any(vapply(r,
      function(rres) {
        if (length(util_attr(rres, "error", exact = TRUE)) > 1) {
          fail("We should not have > 1 error per result")
        }
        if (length(util_attr(rres, "error", exact = TRUE)) == 0) {
          FALSE
        } else {
          vapply(util_attr(rres, "error", exact = TRUE),
            inherits, dataquieR.applicability_problem,
            FUN.VALUE = logical(1)
          )
        }
      },
      FUN.VALUE = logical(1)
    ))

  expect_all_false(res_is_null & !res_is_ap & !res_is_iap) # should not have unknown errors in the report # nolint: line_length_linter.
})

test_that("dq_report2 sees cached missing-code rules", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    sex = c(1L, 2L, 1L),
    pregnant = c(NA_integer_, NA_integer_, 5L)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("sex", "pregnant"),
    LABEL = c("sex", "pregnant"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$METRIC),
    MISSING_LIST = c("|", "|"),
    JUMP_LIST = c("|", "|")
  )
  rules <- data.frame(
    resp_vars = "pregnant",
    CODE_CLASS = "JUMP",
    CODE_LABEL = "not applicable in males",
    CODE_VALUE = "9999",
    RULE = "[sex]=1"
  )

  prep_add_data_frames(
    data_frame_list = setNames(list(rules), MISSING_CODE_RULES)
  )

  report <- suppressWarnings(suppressMessages(dq_report2(
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    resp_vars = "pregnant",
    dimensions = "Completeness",
    filter_indicator_functions = "^com_item_missingness$",
    filter_result_slots = "^SummaryTable$",
    cores = 1
  )))

  expect_s3_class(report, "dataquieR_resultset2")
  expect_equal(
    prep_get_data_frame("study_data")$pregnant,
    c(9999L, NA_integer_, 5L)
  )
})

# Historical local grouping-variable report fixture removed here. Inspect
# commit ec5f672cee before restoring the large-integer grouping setup.
