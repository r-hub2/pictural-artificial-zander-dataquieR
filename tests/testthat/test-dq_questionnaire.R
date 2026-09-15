test_that("dq_questionnaire evaluates SSI calls without dq_report2", {
  skip_on_cran()

  testthat::local_mocked_bindings(
    dq_report2 = function(...) {
      stop("dq_questionnaire must not call dq_report2")
    }
  )

  questionnaire <- suppressWarnings(dq_questionnaire(
    study_data = data.frame(Q1 = c(1L, 1L, 2L), Q2 = c(1L, 2L, 2L)),
    meta_data = data.frame(
      VAR_NAMES = c("Q1", "Q2"),
      LABEL = c("Question 1", "Question 2"),
      LONG_LABEL = c("Question 1", "Question 2"),
      DATA_TYPE = DATA_TYPES$INTEGER,
      SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
      VARIABLE_ROLE = VARIABLE_ROLES$PROCESS,
      stringsAsFactors = FALSE
    ),
    meta_data_cross_item = data.frame(
      VARIABLE_LIST = "Q1 | Q2",
      CHECK_ID = "long",
      CHECK_LABEL = "Long strings",
      MAXIMUM_LONG_STRING = "[;2)",
      stringsAsFactors = FALSE
    )
  ))

  expect_s3_class(questionnaire, "dataquieR_result")
  expect_identical(
    attr(questionnaire, "function_name", exact = TRUE),
    "con_ssi_range_check"
  )
  expect_gt(length(questionnaire), 0)
  expect_length(attr(questionnaire, "error", exact = TRUE), 0)
  expect_identical(
    attr(questionnaire, "dq_questionnaire_result", exact = TRUE),
    TRUE
  )
})

test_that("questionnaire evaluates prepared SSI inputs as pipeline calls", {
  skip_on_cran()

  call <- quote(con_ssi_range_check(resp_vars = "Maximum Long String"))
  attr(call, VAR_NAMES) <- "MAXIMUM_LONG_STRING_all_questionnaire"
  calls <- list(`con_ssi_range_check.Maximum Long String` = call)

  state <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    util_eval_to_dataquieR_result = function(..., called_in_pipeline) {
      state$called_in_pipeline <- called_in_pipeline
      util_compress(structure(
        list(SummaryTable = data.frame(Variables = "Maximum Long String")),
        function_name = "con_ssi_range_check",
        class = c("dataquieR_result", "master_result")
      ))
    }
  )

  results <- util_evaluate_questionnaire_calls(
    all_calls = calls,
    study_data = data.frame(Q1 = 1),
    meta_data = data.frame(
      VAR_NAMES = "MAXIMUM_LONG_STRING_all_questionnaire",
      LABEL = "Maximum Long String",
      CHECK_ID = "all_questionnaire",
      COMPUTED_VARIABLE_ROLE =
        COMPUTED_VARIABLE_ROLES$MAXIMUM_LONG_STRING
    ),
    label_col = LABEL,
    meta_data_segment = data.frame(),
    meta_data_dataframe = data.frame(),
    meta_data_cross_item = data.frame(
      CHECK_ID = "all_questionnaire",
      CHECK_LABEL = "all_questionnaire"
    ),
    filter_result_slots = "^Summary"
  )

  expect_true(state$called_in_pipeline)
  expect_false(inherits(results[[1]], "compressed"))
  expect_s3_class(results[[1]], "dataquieR_result")
  expect_named(results[[1]], "SummaryTable")
})

test_that("questionnaire results follow group and SSI menu order", {
  skip_on_cran()

  make_call <- function(variable_name) {
    call <- quote(con_ssi_range_check(resp_vars = "scale"))
    attr(call, VAR_NAMES) <- variable_name
    call
  }
  all_calls <- lapply(c(
    "MISS_RESP_Page1",
    "TOTRESPT_all_questionnaire",
    "MAHALANOBIS_RATIO_all_questionnaire",
    "MISS_RESP_all_questionnaire",
    "MAXIMUM_LONG_STRING_all_questionnaire",
    "MAXIMUM_LONG_STRING_Page1",
    "unknown"
  ), make_call)
  names(all_calls) <- paste0("result_", seq_along(all_calls))
  meta_data <- data.frame(
    VAR_NAMES = c(
      "MISS_RESP_Page1",
      "TOTRESPT_all_questionnaire",
      "MAHALANOBIS_RATIO_all_questionnaire",
      "MISS_RESP_all_questionnaire",
      "MAXIMUM_LONG_STRING_all_questionnaire",
      "MAXIMUM_LONG_STRING_Page1"
    ),
    CHECK_ID = c(
      "Page1",
      rep("all_questionnaire", 4),
      "Page1"
    ),
    COMPUTED_VARIABLE_ROLE = c(
      COMPUTED_VARIABLE_ROLES$MISS_RESP,
      COMPUTED_VARIABLE_ROLES$TOTRESPT,
      COMPUTED_VARIABLE_ROLES$MAHALANOBIS_RATIO,
      COMPUTED_VARIABLE_ROLES$MISS_RESP,
      COMPUTED_VARIABLE_ROLES$MAXIMUM_LONG_STRING,
      COMPUTED_VARIABLE_ROLES$MAXIMUM_LONG_STRING
    ),
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    CHECK_ID = c("all_questionnaire", "Page1")
  )

  expect_identical(
    util_questionnaire_call_order(
      all_calls,
      meta_data = meta_data,
      meta_data_cross_item = meta_data_cross_item
    ),
    c(4L, 5L, 3L, 2L, 1L, 6L, 7L)
  )
  expect_identical(
    util_questionnaire_call_order(
      all_calls[1],
      meta_data = meta_data,
      meta_data_cross_item = meta_data_cross_item
    ),
    1L
  )
  expect_identical(
    util_questionnaire_call_order(
      all_calls,
      meta_data = data.frame(),
      meta_data_cross_item = meta_data_cross_item
    ),
    seq_along(all_calls)
  )
})

test_that("questionnaire combines compatible results within groups", {
  skip_on_cran()

  variable_names <- c(
    "MISS_RESP_all_questionnaire",
    "MAXIMUM_LONG_STRING_all_questionnaire",
    "MAHALANOBIS_RATIO_all_questionnaire",
    "MISS_RESP_Page1",
    "MAXIMUM_LONG_STRING_Page1"
  )
  functions <- c(
    "con_ssi_range_check",
    "con_ssi_range_check",
    "acc_mahalanobis_ratio",
    "con_ssi_range_check",
    "con_ssi_range_check"
  )
  make_call <- function(variable_name, function_name) {
    call <- call(function_name, resp_vars = variable_name)
    attr(call, VAR_NAMES) <- variable_name
    attr(call, GRADING_RULESET) <- "0"
    attr(call, "entity_name") <- variable_name
    call
  }
  all_calls <- Map(make_call, variable_names, functions)
  names(all_calls) <- paste0(functions, ".", variable_names)
  results <- Map(function(function_name, call, row) {
    structure(
      list(SummaryTable = data.frame(Variables = row, value = row)),
      function_name = function_name,
      call = call,
      class = c("dataquieR_result", "master_result")
    )
  }, functions, all_calls, seq_along(all_calls))
  names(results) <- names(all_calls)
  for (index in seq_along(results)) {
    attr(results[[index]], "r_summary") <- data.frame(source = index)
  }
  meta_data <- data.frame(
    VAR_NAMES = variable_names,
    CHECK_ID = c(rep("all_questionnaire", 3), rep("Page1", 2)),
    COMPUTED_VARIABLE_ROLE = c(
      COMPUTED_VARIABLE_ROLES$MISS_RESP,
      COMPUTED_VARIABLE_ROLES$MAXIMUM_LONG_STRING,
      COMPUTED_VARIABLE_ROLES$MAHALANOBIS_RATIO,
      COMPUTED_VARIABLE_ROLES$MISS_RESP,
      COMPUTED_VARIABLE_ROLES$MAXIMUM_LONG_STRING
    ),
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    CHECK_ID = c("all_questionnaire", "Page1"),
    CHECK_LABEL = c("All questionnaire", "Page 1"),
    stringsAsFactors = FALSE
  )

  combined <- util_combine_questionnaire_results(
    results = results,
    all_calls = all_calls,
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item
  )

  expect_identical(length(combined), 3L)
  expect_identical(
    names(combined),
    c(
      "con_ssi_range_check.All questionnaire",
      names(results)[[3]],
      "con_ssi_range_check.Page 1"
    )
  )
  expect_identical(
    as.vector(combined[[1]]$SummaryTable$value),
    c(1L, 2L)
  )
  expect_identical(
    as.vector(combined[[3]]$SummaryTable$value),
    c(4L, 5L)
  )
  expect_identical(
    as.integer(
      util_attr(combined[[1]], "r_summary", exact = TRUE)[["source"]]
    ),
    1:2
  )
  expect_identical(
    util_attr(
      combined[[1]],
      "dq_questionnaire_grading_meta_data",
      exact = TRUE
    )[[VAR_NAMES]],
    variable_names[1:2]
  )
  expect_match(
    util_attr(combined[[1]], "dq_result_title", exact = TRUE),
    "All questionnaire: Missing responses, Maximum Long String",
    fixed = TRUE
  )
  expect_identical(
    util_attr(combined, "dq_questionnaire_grouped", exact = TRUE),
    TRUE
  )

  questionnaire <- util_questionnaire_results(combined)
  expect_identical(
    names(util_attr(questionnaire, "dq_result_list", exact = TRUE)),
    names(combined)
  )
})

test_that("dq_questionnaire uses cached study data and empty SSI results", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(Q1 = c(1L, 1L, 2L), stringsAsFactors = FALSE)
  meta_data <- data.frame(
    VAR_NAMES = "Q1",
    LABEL = "Question 1",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(study_data = study_data)

  questionnaire <- suppressMessages(dq_questionnaire(
    meta_data = meta_data,
    meta_data_cross_item = data.frame(),
    dt_adjust = FALSE
  ))
  expect_equal(questionnaire, list())

  questionnaire_from_named_cache <- suppressMessages(dq_questionnaire(
    study_data = "study_data",
    meta_data = meta_data,
    meta_data_cross_item = data.frame(),
    dt_adjust = FALSE
  ))
  expect_equal(questionnaire_from_named_cache, list())
})

test_that("dq_questionnaire uses dataframe-level study data fallback", {
  skip_on_cran()

  # Assumption: questionnaire metadata may identify the study data indirectly
  # via dataframe-level metadata when no study_data argument is provided.
  meta_data <- data.frame(
    VAR_NAMES = "Q1",
    LABEL = "Question 1",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    util_find_study_data_from_dataframe_level = function(meta_data_dataframe) {
      list(
        name_of_study_data = "from_dataframe_level",
        study_data = data.frame(Q1 = c(1L, 2L), stringsAsFactors = FALSE)
      )
    }
  )

  questionnaire <- suppressMessages(dq_questionnaire(
    meta_data = meta_data,
    meta_data_dataframe = data.frame(DF_NAME = "from_dataframe_level"),
    meta_data_cross_item = data.frame(),
    dt_adjust = FALSE
  ))

  expect_equal(questionnaire, list())
})

test_that("dq_questionnaire accepts workbook metadata entrypoint", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = "Q1",
    LABEL = "Question 1",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    prep_load_workbook_like_file = function(file, ...) {
      prep_add_data_frames(data_frame_list = list(
        item_level = meta_data,
        `cross-item_level` = data.frame()
      ))
    }
  )

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  questionnaire <- suppressMessages(dq_questionnaire(
    study_data = data.frame(Q1 = c(1L, 2L), stringsAsFactors = FALSE),
    meta_data_v2 = "metadata.xlsx",
    name_of_study_data = "study",
    dt_adjust = FALSE
  ))

  expect_equal(questionnaire, list())
})

test_that("questionnaire calls only include SSI mapped functions", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("Q1", "Q2", "Long"),
    LABEL = c("Question 1", "Question 2", "Long strings"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(
      SCALE_LEVELS$ORDINAL,
      SCALE_LEVELS$ORDINAL,
      SCALE_LEVELS$RATIO
    ),
    COMPUTED_VARIABLE_ROLE = c(
      NA_character_,
      NA_character_,
      COMPUTED_VARIABLE_ROLES$MAXIMUM_LONG_STRING
    ),
    CHECK_ID = c(NA_character_, NA_character_, "long"),
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    VARIABLE_LIST = "Q1 | Q2",
    CHECK_ID = "long",
    CHECK_LABEL = "Long strings",
    MAXIMUM_LONG_STRING = "[;2)",
    stringsAsFactors = FALSE
  )

  calls <- util_generate_questionnaire_calls(
    meta_data = meta_data,
    label_col = LABEL,
    meta_data_segment = data.frame(),
    meta_data_dataframe = data.frame(),
    meta_data_cross_item = meta_data_cross_item,
    specific_args = list(),
    arg_overrides = list(),
    resp_vars = meta_data[[LABEL]]
  )

  expect_true(length(calls) > 0)
  expect_setequal(
    unique(vapply(calls, function(call) as.character(call[[1]]), "")),
    "con_ssi_range_check"
  )
})

test_that("questionnaire handles missing inputs and empty SSI calls", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  expect_error(
    dq_questionnaire(
      meta_data = data.frame(
        VAR_NAMES = "Q1",
        LABEL = "Question 1",
        DATA_TYPE = DATA_TYPES$INTEGER,
        SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
        stringsAsFactors = FALSE
      ),
      meta_data_cross_item = data.frame()
    ),
    "Missing .study_data."
  )

  empty_calls <- util_generate_questionnaire_calls(
    meta_data = data.frame(
      VAR_NAMES = "Q1",
      LABEL = "Question 1",
      DATA_TYPE = DATA_TYPES$INTEGER,
      SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
      stringsAsFactors = FALSE
    ),
    label_col = LABEL,
    meta_data_segment = data.frame(),
    meta_data_dataframe = data.frame(),
    meta_data_cross_item = data.frame(),
    specific_args = list(),
    arg_overrides = list(),
    resp_vars = "Question 1"
  )

  expect_length(empty_calls, 0)
  expect_equal(
    util_evaluate_questionnaire_calls(
      all_calls = list(),
      study_data = data.frame(Q1 = 1),
      meta_data = data.frame(),
      label_col = LABEL,
      meta_data_segment = data.frame(),
      meta_data_dataframe = data.frame(),
      meta_data_cross_item = data.frame(),
      filter_result_slots = character()
    ),
    list()
  )
})

test_that("questionnaire input preparation filters variables", {
  skip_on_cran()

  withr::local_options(list(dataquieR.precomputeStudyData = FALSE))

  study_data <- data.frame(
    Q1 = c(1L, 2L, 3L),
    Q2 = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("Q1", "Q2", "Q3"),
    LABEL = c("Question 1", "Question 2", "Question 3"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    VARIABLE_LIST = "Q1 | Q2",
    CHECK_ID = "check",
    CHECK_LABEL = "Check label",
    MAXIMUM_LONG_STRING = "[;2)",
    stringsAsFactors = FALSE
  )

  prepared <- suppressWarnings(suppressMessages(
    util_prepare_questionnaire_inputs(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      meta_data_segment = data.frame(),
      meta_data_dataframe = "missing_dataframe_level",
      meta_data_cross_item = meta_data_cross_item,
      meta_data_item_computation = "missing_item_computation_level",
      resp_vars = c("Question 1", "Question 2", "Missing label"),
      ignore_empty_vars = FALSE,
      name_of_study_data = "study"
    )
  ))

  expect_identical(prepared$resp_vars, c("Question 1", "Question 2"))
  expect_identical(
    prepared$meta_data_dataframe[[DF_NAME]],
    "study"
  )
  expect_true(util_attr(prepared$meta_data_cross_item, "normalized",
      exact = TRUE
    ))

  prepared_without_empty <- suppressWarnings(suppressMessages(
    util_prepare_questionnaire_inputs(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      meta_data_segment = data.frame(),
      meta_data_dataframe = data.frame(DF_NAME = "study"),
      meta_data_cross_item = data.frame(),
      meta_data_item_computation = data.frame(),
      resp_vars = c("Question 1", "Question 2"),
      ignore_empty_vars = TRUE,
      name_of_study_data = "study"
    )
  ))
  expect_identical(prepared_without_empty$resp_vars, "Question 1")

  prepared_all_vars <- suppressWarnings(suppressMessages(
    util_prepare_questionnaire_inputs(
      study_data = data.frame(Q1 = c(1L, 2L), stringsAsFactors = FALSE),
      meta_data = meta_data[meta_data[[VAR_NAMES]] != "Q2", , drop = FALSE],
      label_col = LABEL,
      meta_data_segment = data.frame(),
      meta_data_dataframe = data.frame(DF_NAME = "study"),
      meta_data_cross_item = data.frame(),
      meta_data_item_computation = data.frame(),
      resp_vars = "Question 1",
      ignore_empty_vars = "auto",
      name_of_study_data = "study"
    )
  ))
  expect_identical(as.character(prepared_all_vars$resp_vars), "Question 1")

  withr::local_options(list(
    dataquieR.study_data_colnames_case_sensitive = FALSE
  ))
  prepared_case_insensitive <- suppressWarnings(suppressMessages(
    util_prepare_questionnaire_inputs(
      study_data = data.frame(q1 = c(1L, 2L), stringsAsFactors = FALSE),
      meta_data = meta_data[1, , drop = FALSE],
      label_col = LABEL,
      meta_data_segment = data.frame(),
      meta_data_dataframe = data.frame(DF_NAME = "study"),
      meta_data_cross_item = data.frame(),
      meta_data_item_computation = data.frame(),
      resp_vars = character(0),
      ignore_empty_vars = "auto",
      name_of_study_data = "study"
    )
  ))
  expect_identical(colnames(prepared_case_insensitive$study_data), "Q1")

  withr::local_options(list(
    dataquieR.study_data_colnames_case_sensitive = TRUE,
    dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "subset_u"
  ))
  prepared_subset <- suppressWarnings(suppressMessages(
    util_prepare_questionnaire_inputs(
      study_data = data.frame(Q1 = c(1L, 2L), stringsAsFactors = FALSE),
      meta_data = meta_data,
      label_col = LABEL,
      meta_data_segment = data.frame(),
      meta_data_dataframe = data.frame(DF_NAME = "study"),
      meta_data_cross_item = data.frame(),
      meta_data_item_computation = data.frame(),
      resp_vars = character(0),
      ignore_empty_vars = "auto",
      name_of_study_data = "study"
    )
  ))
  expect_identical(as.character(prepared_subset$meta_data[[VAR_NAMES]]), "Q1")
})

test_that("questionnaire input preparation guesses item metadata", {
  skip_on_cran()

  # Assumption: if item metadata names do not match the case-sensitive study
  # data columns, input preparation may fall back to guessed item metadata.
  withr::local_options(list(
    dataquieR.study_data_colnames_case_sensitive = TRUE
  ))

  prepared <- suppressWarnings(suppressMessages(
    util_prepare_questionnaire_inputs(
      study_data = data.frame(q1 = c(1L, 2L), stringsAsFactors = FALSE),
      meta_data = data.frame(
        VAR_NAMES = "Q1",
        LABEL = "Question 1",
        DATA_TYPE = DATA_TYPES$INTEGER,
        SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
        stringsAsFactors = FALSE
      ),
      label_col = LABEL,
      meta_data_segment = data.frame(),
      meta_data_dataframe = data.frame(DF_NAME = "study"),
      meta_data_cross_item = data.frame(),
      meta_data_item_computation = data.frame(),
      resp_vars = character(0),
      ignore_empty_vars = "auto",
      name_of_study_data = "study"
    )
  ))

  expect_identical(colnames(prepared$study_data), "q1")
  expect_identical(as.character(prepared$resp_vars), "q1")
  expect_identical(as.character(prepared$meta_data[[VAR_NAMES]]), "q1")
})

test_that("questionnaire input keeps empty variables when requested", {
  skip_on_cran()

  # Assumption: explicitly setting ignore_empty_vars to FALSE keeps empty
  # response variables, while metadata rows missing from study_data are removed.
  withr::local_options(list(
    dataquieR.precomputeStudyData = TRUE
  ))

  populated <- new.env(parent = emptyenv())
  populated$called <- FALSE
  testthat::local_mocked_bindings(
    util_populate_study_data_cache = function(...) {
      populated$called <- TRUE
      invisible(NULL)
    }
  )

  prepared <- suppressMessages(util_prepare_questionnaire_inputs(
    study_data = data.frame(
      Q1 = c(1L, 2L),
      Q3 = c(NA, NA),
      stringsAsFactors = FALSE
    ),
    meta_data = data.frame(
      VAR_NAMES = c("Q1", "Q2", "Q3"),
      LABEL = c("Question 1", "Question 2", "Question 3"),
      DATA_TYPE = DATA_TYPES$INTEGER,
      SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
      stringsAsFactors = FALSE
    ),
    label_col = LABEL,
    meta_data_segment = data.frame(),
    meta_data_dataframe = data.frame(DF_NAME = "study"),
    meta_data_cross_item = data.frame(),
    meta_data_item_computation = data.frame(),
    resp_vars = character(0),
    ignore_empty_vars = FALSE,
    name_of_study_data = "study"
  ))

  expect_true(populated$called)
  expect_setequal(
    as.character(prepared$resp_vars),
    c("Question 1", "Question 3")
  )
  expect_setequal(as.character(prepared$meta_data[[VAR_NAMES]]), c("Q1", "Q3"))
})

test_that("questionnaire result extraction preserves empty and multiple sets", {
  skip_on_cran()

  expect_equal(util_questionnaire_results(list()), list())

  result1 <- structure(list(SummaryTable = data.frame(x = 1)),
    function_name = "con_limit_deviations",
    class = c("dataquieR_result", "master_result")
  )
  result2 <- structure(list(SummaryTable = data.frame(x = 2)),
    function_name = "con_limit_deviations",
    class = c("dataquieR_result", "master_result")
  )
  results <- list(first = result1, second = result2)

  questionnaire <- util_questionnaire_results(results)

  expect_s3_class(questionnaire, "master_result")
  expect_equal(as.vector(questionnaire$SummaryTable$x), c(1, 2))
  single <- util_questionnaire_results(list(only = result1))
  expect_identical(
    attr(single, "dq_questionnaire_result", exact = TRUE),
    TRUE
  )
  attr(single, "dq_questionnaire_result") <- NULL
  attr(single, "dq_result_anchor") <- NULL
  expect_identical(single, result1)

  attr(result2, "function_name") <- "acc_mahalanobis_ratio"
  mixed <- util_questionnaire_results(list(first = result1, second = result2))

  expect_s3_class(mixed, "master_result")
  expect_identical(
    names(attr(mixed, "dq_result_list", exact = TRUE)),
    c("first", "second")
  )
})

test_that("sectioned master results use readable section titles", {
  skip_on_cran()

  result1 <- structure(list(SummaryTable = data.frame(x = 1)),
    function_name = "con_limit_deviations",
    class = c("dataquieR_result", "master_result")
  )
  result2 <- structure(list(SummaryTable = data.frame(x = 2)),
    function_name = "acc_shape_or_scale",
    class = c("dataquieR_result", "master_result")
  )
  result3 <- structure(list(SummaryTable = data.frame(x = 3)),
    function_name = "acc_margins",
    call = structure(quote(acc_margins(resp_vars = "BMI")),
      entity_name = "Body mass index"
    ),
    class = c("dataquieR_result", "master_result")
  )

  result_list <- list(
    "con_limit_deviations.Invalid/missing Resp.: Missing response share" =
      result1,
    "acc_shape_or_scale.BMI" = result2,
    "acc_margins.BMI" = result3
  )

  sectioned <- util_sectioned_master_result_from_result_list(result_list)

  expect_identical(
    attr(sectioned, "dq_result_titles", exact = TRUE),
    c(
      "Range violations",
      "Unexpected distribution shape",
      "Distribution across"
    )
  )
  expect_identical(names(attr(sectioned, "dq_result_list", exact = TRUE)),
    names(result_list)
  )
})

test_that("sectioned master results disambiguate repeated titles", {
  skip_on_cran()

  result1 <- structure(list(SummaryTable = data.frame(x = 1)),
    function_name = "des_summary",
    class = c("dataquieR_result", "master_result")
  )
  result2 <- structure(list(SummaryTable = data.frame(x = 2)),
    function_name = "acc_shape_or_scale",
    class = c("dataquieR_result", "master_result")
  )
  result3 <- structure(list(SummaryTable = data.frame(x = 3)),
    function_name = "com_item_missingness",
    class = c("dataquieR_result", "master_result")
  )

  result_list <- list(
    "des_summary.Body mass index" = result1,
    "acc_shape_or_scale.Body mass index" = result2,
    "com_item_missingness.Symptom score" = result3
  )

  sectioned <- util_sectioned_master_result_from_result_list(result_list)

  expect_identical(
    attr(sectioned, "dq_result_titles", exact = TRUE),
    c(
      "Descriptive statistics",
      "Unexpected distribution shape",
      "Missing values (Item-level)"
    )
  )
})

test_that("questionnaire section titles use SSI group context", {
  skip_on_cran()

  result1 <- structure(list(SummaryTable = data.frame(x = 1)),
    function_name = "con_limit_deviations",
    dq_result_title =
      "Response completeness scale: Invalid or missing responses",
    class = c("dataquieR_result", "master_result")
  )
  result2 <- structure(list(SummaryTable = data.frame(x = 2)),
    function_name = "con_limit_deviations",
    dq_result_title =
      "Straightlining scale: Maximum Long String",
    class = c("dataquieR_result", "master_result")
  )

  sectioned <- util_sectioned_master_result_from_result_list(
    list(miss = result1, straight = result2),
    title_mode = "ssi"
  )

  expect_identical(
    attr(sectioned, "dq_result_titles", exact = TRUE),
    c(
      "Response completeness scale: Invalid or missing responses",
      "Straightlining scale: Maximum Long String"
    )
  )
})

test_that("questionnaire SSI titles use metadata and metric fallbacks", {
  skip_on_cran()

  meta_data_cross_item <- data.frame(
    CHECK_ID = c("scale", "check", "acro", "plain"),
    SCALE_NAME = c("Scale name", NA, NA, NA),
    CHECK_LABEL = c("Scale check", "Check label", NA, NA),
    SCALE_ACRONYM = c("SC", "CH", "AC", NA),
    stringsAsFactors = FALSE
  )

  expect_identical(
    util_questionnaire_group_title("scale", meta_data_cross_item),
    "Scale check (Scale name -- SC)"
  )
  expect_identical(
    util_questionnaire_group_title("check", meta_data_cross_item),
    "Check label (CH)"
  )
  expect_identical(
    util_questionnaire_group_title("acro", meta_data_cross_item),
    "AC"
  )
  expect_identical(
    util_questionnaire_group_title("plain", meta_data_cross_item),
    "plain"
  )
  expect_true(is.na(util_questionnaire_group_title(NA_character_,
        meta_data_cross_item
      )))
  expect_true(is.na(util_questionnaire_group_title("missing",
        meta_data_cross_item
      )))
  expect_true(is.na(util_questionnaire_metric_title(NA_character_)))
  expect_true(is.na(util_questionnaire_metric_title("not-a-role")))

  testthat::local_mocked_bindings(
    util_get_concept_info = function(concept, ..., drop = FALSE) {
      requested <- as.list(substitute(list(...)))[[3]]
      if (identical(requested, "result_caption")) {
        return(NA_character_)
      }
      if (identical(requested, "menu_label")) {
        return("Menu fallback")
      }
      data.frame()
    }
  )
  expect_identical(
    util_questionnaire_metric_title(
      COMPUTED_VARIABLE_ROLES$MAXIMUM_LONG_STRING
    ),
    "Menu fallback"
  )

  testthat::local_mocked_bindings(
    util_get_concept_info = function(...) NA_character_
  )
  expect_true(is.na(util_questionnaire_metric_title(
    COMPUTED_VARIABLE_ROLES$MAXIMUM_LONG_STRING
  )))
})

test_that("questionnaire result titles combine group and metric context", {
  skip_on_cran()

  # Assumption: SSI standalone titles should name the questionnaire group first
  # and the SSI metric second, without exposing technical function aliases.
  meta_data <- data.frame(
    VAR_NAMES = "MISS_RESP",
    LABEL = "Missing response share",
    CHECK_ID = "miss",
    COMPUTED_VARIABLE_ROLE = COMPUTED_VARIABLE_ROLES$MISS_RESP,
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    CHECK_ID = "miss",
    SCALE_NAME = "Response completeness scale",
    CHECK_LABEL = "Invalid or missing responses",
    stringsAsFactors = FALSE
  )
  call <- quote(con_limit_deviations(resp_vars = "Missing response share"))

  expect_identical(
    util_questionnaire_result_title(
      call = call,
      meta_data = meta_data,
      label_col = LABEL,
      meta_data_cross_item = meta_data_cross_item
    ),
    paste0(
      "Invalid or missing responses (Response completeness scale): ",
      "Missing responses"
    )
  )

  attr(call, VAR_NAMES) <- "MISS_RESP"
  meta_data_cross_item[[SCALE_NAME]] <- NA_character_
  expect_identical(
    util_questionnaire_result_title(
      call = call,
      meta_data = meta_data,
      label_col = LABEL,
      meta_data_cross_item = meta_data_cross_item
    ),
    "Invalid or missing responses: Missing responses"
  )

  expect_true(is.na(util_questionnaire_result_title(
    call = quote(con_limit_deviations(resp_vars = c("a", "b"))),
    meta_data = meta_data,
    label_col = LABEL,
    meta_data_cross_item = meta_data_cross_item
  )))

  meta_data[[CHECK_ID]] <- NA_character_
  expect_identical(
    util_questionnaire_result_title(
      call = call,
      meta_data = meta_data,
      label_col = LABEL,
      meta_data_cross_item = meta_data_cross_item
    ),
    "Missing responses"
  )

  meta_data[[CHECK_ID]] <- "miss"
  meta_data[[COMPUTED_VARIABLE_ROLE]] <- NA_character_
  expect_identical(
    util_questionnaire_result_title(
      call = call,
      meta_data = meta_data,
      label_col = LABEL,
      meta_data_cross_item = meta_data_cross_item
    ),
    "Invalid or missing responses"
  )

  missing_resp_var <- quote(con_limit_deviations(resp_vars = missing_object))
  expect_true(is.na(util_questionnaire_result_title(
    call = missing_resp_var,
    meta_data = meta_data,
    label_col = LABEL,
    meta_data_cross_item = meta_data_cross_item
  )))
})

test_that("questionnaire titles handle empty group metadata fields", {
  skip_on_cran()

  # Assumption: cross-item group titles prefer check labels and add available
  # scale details; empty DQ_OBS placeholders count as missing.
  meta_data_cross_item <- data.frame(
    CHECK_ID = c("scale1", "scale2", "scale3"),
    SCALE_NAME = c("|", NA_character_, ""),
    CHECK_LABEL = c("Explicit check label", "|", ""),
    SCALE_ACRONYM = c("ACR1", "ACR2", ""),
    stringsAsFactors = FALSE
  )

  expect_identical(
    util_questionnaire_group_title("scale1", meta_data_cross_item),
    "Explicit check label (ACR1)"
  )
  expect_identical(
    util_questionnaire_group_title("scale2", meta_data_cross_item),
    "ACR2"
  )
  expect_identical(
    util_questionnaire_group_title("scale3", meta_data_cross_item),
    "scale3"
  )
  expect_true(is.na(util_questionnaire_group_title(
    "missing",
    meta_data_cross_item
  )))
  expect_true(is.na(util_questionnaire_group_title(
    NA_character_,
    meta_data_cross_item
  )))
})

test_that("master result section helpers cover fallback titles", {
  skip_on_cran()

  # Assumption: invalid or unmergeable result lists are kept as sectioned master
  # results so print.dataquieR_result can still render every individual result.
  expect_null(util_master_result_from_result_list(list()))
  expect_null(util_master_result_from_result_list(list(plain = list())))

  error_result <- structure(list(SummaryTable = data.frame(x = 1)),
    function_name = "con_limit_deviations",
    error = list(simpleError("broken")),
    class = c("dataquieR_result", "master_result")
  )
  sectioned <- util_master_result_from_result_list(list(error = error_result))
  expect_s3_class(sectioned, "master_result")
  expect_identical(
    attr(sectioned, "dq_result_titles", exact = TRUE),
    "Range violations"
  )

  no_common <- util_master_result_from_result_list(list(
    first = structure(list(SummaryTable = data.frame(x = 1)),
      function_name = "con_limit_deviations",
      class = c("dataquieR_result", "master_result")
    ),
    second = structure(list(Result = data.frame(x = 2)),
      function_name = "con_limit_deviations",
      class = c("dataquieR_result", "master_result")
    )
  ))
  expect_s3_class(no_common, "master_result")
  expect_identical(
    attr(no_common, "dq_result_titles", exact = TRUE),
    c("Range violations: first", "Range violations: second")
  )

  duplicate <- util_sectioned_master_result_from_result_list(list(
    "des_summary.Body mass index" = structure(list(SummaryTable = data.frame(
      x = 1
    )),
    function_name = "des_summary",
    class = c("dataquieR_result", "master_result")
    ),
    "des_summary.Height" = structure(list(SummaryTable = data.frame(x = 2)),
      function_name = "des_summary",
      class = c("dataquieR_result", "master_result")
    )
  ))
  expect_identical(
    attr(duplicate, "dq_result_titles", exact = TRUE),
    c(
      "Descriptive statistics: Body mass index",
      "Descriptive statistics: Height"
    )
  )

  ssi_titles <- util_sectioned_master_result_from_result_list(list(
    "con_limit_deviations.MISS_RESP" = structure(list(
      SummaryTable = data.frame(x = 1)
    ),
    function_name = "con_limit_deviations",
    dq_result_title = "Response completeness scale: Missing responses",
    class = c("dataquieR_result", "master_result")
    )
  ), title_mode = "ssi")
  expect_identical(
    attr(ssi_titles, "dq_result_titles", exact = TRUE),
    "Response completeness scale: Missing responses"
  )

  testthat::local_mocked_bindings(
    util_master_result_from_result_list = function(...) NULL
  )
  sectioned_questionnaire <- util_questionnaire_results(list(
    first = structure(list(SummaryTable = data.frame(x = 1)),
      function_name = "con_limit_deviations",
      class = c("dataquieR_result", "master_result")
    ),
    second = structure(list(SummaryTable = data.frame(x = 2)),
      function_name = "con_limit_deviations",
      class = c("dataquieR_result", "master_result")
    )
  ))
  expect_s3_class(sectioned_questionnaire, "master_result")

  expect_identical(
    util_disambiguate_result_titles(
      list(
        first = structure(list(),
          dq_result_title = "Duplicate",
          class = c("dataquieR_result", "master_result")
        ),
        second = structure(list(),
          dq_result_title = "Other detail",
          class = c("dataquieR_result", "master_result")
        )
      ),
      c("Duplicate", "Duplicate")
    ),
    c("Duplicate: first", "Duplicate: Other detail")
  )

  entity_result <- structure(list(SummaryTable = data.frame(x = 1)),
    function_name = "unknown_function",
    call = structure(quote(unknown_function(resp_vars = "BMI")),
      entity_name = "Body mass index"
    ),
    class = c("dataquieR_result", "master_result")
  )
  expect_identical(
    util_result_entity_title(entity_result, "unknown_function.BMI"),
    "Body mass index"
  )
  attr(entity_result, "call") <- quote(unknown_function(resp_vars = "BMI"))
  expect_identical(
    util_result_entity_title(entity_result, "unknown_function.BMI"),
    "BMI"
  )

  # Assumption: DQ_OBS policy uses empty strings or "|" for intentionally empty
  # captions; "|" must not be treated as a display title.
  expect_true(util_result_caption_empty("|"))
  expect_true(util_result_caption_empty(" | "))
  expect_true(util_result_caption_empty(""))
  expect_true(util_result_caption_empty(NA_character_))
  expect_false(util_result_caption_empty("Readable caption"))

  testthat::local_mocked_bindings(
    util_map_labels = function(...) "Metadata caption"
  )
  expect_identical(
    util_function_caption_from_metadata(
      "des_summary",
      c("menu_title_report", "dq_report2_short_title")
    ),
    "Metadata caption"
  )

  # Assumption: empty DQ_OBS/manual caption fields should fall back to the
  # metadata caption before a technical function name is shown.
  testthat::local_mocked_bindings(
    util_alias2caption = function(...) "|",
    util_function_caption_from_metadata = function(...) "Metadata caption"
  )
  expect_identical(
    util_result_function_caption("unknown_function_suffix"),
    "Metadata caption"
  )

  testthat::local_mocked_bindings(
    util_alias2caption = function(...) "|",
    util_function_caption_from_metadata = function(...) NA_character_
  )
  expect_identical(
    util_result_function_caption("unknown_function_suffix"),
    "unknown_function_suffix"
  )
  expect_identical(
    util_function_name_from_alias("des_summary.BMI"),
    "des_summary"
  )
  expect_true(is.na(util_function_caption_from_metadata(
    NA_character_,
    "menu_title_report"
  )))
})

test_that("master result names use call entity context", {
  skip_on_cran()

  # Assumption: master results keep all individual result tables, while result
  # titles may use the entity context carried by each individual call.
  entity_result <- structure(list(SummaryTable = data.frame(x = 1)),
    function_name = "con_limit_deviations",
    call = structure(quote(con_limit_deviations(resp_vars = "Q1")),
      entity_name = "Scale score"
    ),
    class = c("dataquieR_result", "master_result")
  )
  resp_var_result <- structure(list(SummaryTable = data.frame(x = 2)),
    function_name = "con_limit_deviations",
    call = quote(con_limit_deviations(resp_vars = "Q2")),
    class = c("dataquieR_result", "master_result")
  )

  combined <- util_master_result_from_result_list(list(
    entity = entity_result,
    resp_var = resp_var_result
  ))

  expect_s3_class(combined, "master_result")
  expect_identical(as.vector(combined$SummaryTable$x), c(1, 2))
})
