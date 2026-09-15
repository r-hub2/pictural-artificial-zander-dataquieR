local_entity_grading_segment_result <- function() {
  segment_table <- data.frame(
    Segment = c("a", "b"),
    PCT_com_qum_nonresp = c(5, 25),
    stringsAsFactors = FALSE
  )
  segment_data <- data.frame(
    Segment = c("a", "b"),
    `Non-response rate (Percentage (0 to 100))` = c("5%", "25%"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  list(SegmentTable = segment_table, SegmentData = segment_data)
}

local_attach_entity_grading <- function(result =
    local_entity_grading_segment_result()) {
  evaluation_env <- rlang::env(
    meta_data_segment = data.frame(
      STUDY_SEGMENT = c("a", "b"),
      GRADING_RULESET = c("strict", "0"),
      stringsAsFactors = FALSE
    )
  )
  util_attach_entity_grading_context(
    result,
    env = evaluation_env,
    function_name = "com_qualified_segment_missingness"
  )
}

local_entity_grading_report <- function(cores = NULL) {
  cache <- new.env(parent = emptyenv())
  with_dataframe_environment(quote({
    study_data <- data.frame(part = c(1L, 1L, 2L, 3L, 4L, 5L))
    meta_data <- data.frame(
      VAR_NAMES = "part",
      LABEL = "part",
      DATA_TYPE = DATA_TYPES$INTEGER,
      SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
      VARIABLE_ROLE = VARIABLE_ROLES$PRIMARY,
      MISSING_LIST_TABLE = "aapor_codes",
      MISSING_LIST = NA_character_,
      JUMP_LIST = NA_character_,
      HARD_LIMITS = NA_character_,
      STUDY_SEGMENT = "SegA",
      stringsAsFactors = FALSE
    )
    meta_data_segment <- data.frame(
      STUDY_SEGMENT = "SegA",
      SEGMENT_PART_VARS = "part",
      GRADING_RULESET = "0",
      stringsAsFactors = FALSE
    )
    aapor_codes <- data.frame(
      CODE_VALUE = as.character(1:5),
      CODE_LABEL = as.character(1:5),
      CODE_INTERPRET = c("I", "P", "PL", "R", "BO"),
      stringsAsFactors = FALSE
    )
    prep_add_data_frames(aapor_codes = aapor_codes)
    suppressWarnings(suppressMessages(dq_report2(
      study_data = study_data,
      meta_data = meta_data,
      meta_data_segment = meta_data_segment,
      label_col = VAR_NAMES,
      dimensions = "Completeness",
      filter_indicator_functions =
        "^com_qualified_segment_missingness$",
      cores = cores
    )))
  }), env = cache)
}

test_that("entity grading context retains only render inputs", {
  skip_on_cran()

  attached <- local_attach_entity_grading()
  for (slot in c("SegmentTable", "SegmentData")) {
    context <- util_attr(
      attached[[slot]],
      "entity_grading_context",
      exact = TRUE
    )
    expect_identical(context$entity, "SEGMENT")
    expect_identical(context$entity_names, c("a", "b"))
    expect_identical(context$grading_rule_sets, c("strict", "0"))
    expect_identical(context$indicator_metrics, "PCT_com_qum_nonresp")
    expect_false(any(c("class", "color", "colors") %in% names(context)))
  }
  expect_identical(
    attached$SegmentTable$PCT_com_qum_nonresp,
    c(5, 25)
  )
  expect_false(any(grepl(
    "data-grading",
    as.character(attached$SegmentData[[2L]]),
    fixed = TRUE
  )))
})

test_that("entity grading ignores incomplete non-gradeable results", {
  skip_on_cran()

  expect_identical(
    util_attach_entity_grading_context(
      list(SegmentTable = data.frame()),
      rlang::env(),
      "test_indicator"
    )$SegmentTable,
    data.frame()
  )
  no_metadata <- list(SegmentTable = data.frame(Segment = "a"))
  expect_null(util_attr(
    util_attach_entity_grading_context(
      no_metadata,
      rlang::env(),
      "test_indicator"
    )$SegmentTable,
    "entity_grading_context",
    exact = TRUE
  ))
  no_metric <- util_attach_entity_grading_context(
    no_metadata,
    rlang::env(meta_data_segment = data.frame(STUDY_SEGMENT = "a")),
    "test_indicator"
  )
  expect_null(util_attr(
    no_metric$SegmentTable,
    "entity_grading_context",
    exact = TRUE
  ))

  plain <- data.frame(Segment = "a", value = 1)
  expect_identical(util_apply_entity_grading_for_render(plain), plain)
  context <- util_attr(
    local_attach_entity_grading()$SegmentData,
    "entity_grading_context",
    exact = TRUE
  )
  no_id <- data.frame(value = 1)
  attr(no_id, "entity_grading_context") <- context
  expect_identical(util_apply_entity_grading_for_render(no_id), no_id)
  no_match <- data.frame(Segment = "other", value = 1)
  attr(no_match, "entity_grading_context") <- context
  expect_identical(util_apply_entity_grading_for_render(no_match), no_match)
  no_target <- data.frame(Segment = "a", value = 1)
  attr(no_target, "entity_grading_context") <- context
  expect_identical(util_apply_entity_grading_for_render(no_target), no_target)

  testthat::local_mocked_bindings(
    util_metrics_to_classes = function(rs_table_long, ...) {
      rs_table_long$class <- NA_integer_
      rs_table_long
    }
  )
  unclassified <- local_attach_entity_grading()$SegmentData
  expect_identical(
    util_apply_entity_grading_for_render(unclassified),
    unclassified
  )
})

test_that("entity classes and colors are resolved only for rendering", {
  skip_on_cran()

  attached <- local_attach_entity_grading()
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  withr::local_options(dataquieR.test_entity_classes = c(1, 2))
  testthat::local_mocked_bindings(
    util_metrics_to_classes = function(rs_table_long, meta_data, entity) {
      calls$n <- calls$n + 1L
      expect_identical(entity, "SEGMENT")
      expect_identical(
        meta_data[[GRADING_RULESET]],
        c("strict", "0")
      )
      rs_table_long$class <- getOption("dataquieR.test_entity_classes")
      rs_table_long
    }
  )

  expect_identical(calls$n, 0L)
  first <- util_apply_entity_grading_for_render(attached$SegmentData)
  expect_identical(calls$n, 1L)
  expect_match(first[[2L]][[1L]], 'data-grading="Ok"', fixed = TRUE)
  expect_match(first[[2L]][[2L]], 'data-grading="Unclear"', fixed = TRUE)
  first_args <- util_attr(first, "entity_grading_args", exact = TRUE)
  expect_identical(
    first_args$grading_cols,
    "Non-response rate (Percentage (0 to 100))"
  )
  expect_identical(first_args$grading_colors, util_get_colors())

  options(dataquieR.test_entity_classes = c(5, 4))
  second <- util_apply_entity_grading_for_render(attached$SegmentData)
  expect_identical(calls$n, 2L)
  expect_match(second[[2L]][[1L]], 'data-grading="Critical"', fixed = TRUE)
  expect_match(second[[2L]][[2L]], 'data-grading="Important"', fixed = TRUE)
  expect_false(any(grepl(
    "data-grading",
    as.character(attached$SegmentData[[2L]]),
    fixed = TRUE
  )))
})

test_that("dataframe grading supports compact N percent displays", {
  skip_on_cran()

  dataframe_table <- data.frame(
    DF_NAME = c("first", "second"),
    NUM_com_qum_nonresp = c(1, 2),
    PCT_com_qum_nonresp = c(10, 20),
    stringsAsFactors = FALSE
  )
  dataframe_data <- data.frame(
    Dataframe = c("first", "second"),
    `Non-response rate N (%)` = c("1 (10%)", "2 (20%)"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  evaluation_env <- rlang::env(
    meta_data_dataframe = data.frame(
      DF_NAME = c("first", "second"),
      GRADING_RULESET = c("0", "strict"),
      stringsAsFactors = FALSE
    )
  )
  attached <- util_attach_entity_grading_context(
    list(
      DataframeTable = dataframe_table,
      DataframeData = dataframe_data
    ),
    env = evaluation_env,
    function_name = "test_dataframe_indicator"
  )
  context <- util_attr(
    attached$DataframeData,
    "entity_grading_context",
    exact = TRUE
  )
  expect_false(any(c("class", "color") %in% names(context)))

  seen <- new.env(parent = emptyenv())
  seen$metrics <- character()
  testthat::local_mocked_bindings(
    util_metrics_to_classes = function(rs_table_long, meta_data, entity) {
      seen$metrics <- c(seen$metrics, unique(rs_table_long$indicator_metric))
      expect_identical(entity, "DATAFRAME")
      rs_table_long$class <- c(2, 3)
      rs_table_long
    }
  )
  rendered <- util_apply_entity_grading_for_render(attached$DataframeData)

  expect_identical(seen$metrics, "PCT_com_qum_nonresp")
  expect_match(rendered[[2L]][[1L]], 'data-grading="Unclear"', fixed = TRUE)
  expect_match(rendered[[2L]][[2L]], 'data-grading="Moderate"', fixed = TRUE)
  expect_identical(
    util_attr(rendered, "entity_grading_args", exact = TRUE)$grading_cols,
    "Non-response rate N (%)"
  )
})

test_that("entity grading rejects ambiguous same-level rulesets", {
  skip_on_cran()

  metadata <- data.frame(
    STUDY_SEGMENT = c("a", "a"),
    GRADING_RULESET = c("first", "second"),
    stringsAsFactors = FALSE
  )
  expect_error(
    util_entity_grading_rulesets("a", metadata, STUDY_SEGMENT),
    "More than one.+GRADING_RULESET"
  )
  expect_identical(
    util_entity_grading_rulesets(
      c("a", "missing"),
      metadata[1L, , drop = FALSE],
      STUDY_SEGMENT
    ),
    c("first", "0")
  )
})

test_that("indicator evaluation attaches entity grading to report slots", {
  skip_on_cran()

  evaluation_env <- rlang::env(
    meta_data_segment = data.frame(
      STUDY_SEGMENT = c("a", "b"),
      GRADING_RULESET = c("strict", "0"),
      stringsAsFactors = FALSE
    )
  )
  result <- util_eval_to_dataquieR_result(
    expression = quote(local_entity_grading_segment_result()),
    env = evaluation_env,
    filter_result_slots = character(),
    nm = "com_qualified_segment_missingness.[ALL]",
    function_name = "com_qualified_segment_missingness",
    called_in_pipeline = FALSE
  )

  expect_s3_class(result, "dataquieR_result")
  expect_type(
    util_attr(
      result$SegmentTable,
      "entity_grading_context",
      exact = TRUE
    ),
    "list"
  )
  expect_false(any(grepl(
    "data-grading",
    as.character(result$SegmentData[[2L]]),
    fixed = TRUE
  )))
})

test_that("dq_report2 retains segment grading for render-time use", {
  skip_on_cran()

  report <- local_entity_grading_report()

  result <- report[["com_qualified_segment_missingness.[ALL]"]]
  context <- util_attr(
    result$SegmentData,
    "entity_grading_context",
    exact = TRUE
  )
  expect_identical(context$entity, "SEGMENT")
  expect_identical(context$grading_rule_sets, "0")
  expect_false(any(grepl(
    "data-grading",
    as.character(result$SegmentData[[2L]]),
    fixed = TRUE
  )))
  rendered <- util_apply_entity_grading_for_render(result$SegmentData)
  expect_match(rendered[[2L]], "data-grading", fixed = TRUE)
})

test_that("socket report workers can attach entity grading context", {
  skip_on_cran()
  skip_if_not_installed("parallel")

  probe <- try(parallel::makePSOCKcluster(1L), silent = TRUE)
  if (util_is_try_error(probe)) {
    skip("A local PSOCK worker is unavailable")
  }
  parallel::stopCluster(probe)

  report <- withr::with_options(
    list(dataquieR.tmp_no_load_all = TRUE),
    local_entity_grading_report(cores = 1L)
  )
  result <- report[["com_qualified_segment_missingness.[ALL]"]]
  expect_s3_class(result, "dataquieR_result")
  expect_false(inherits(result, "dataquieR_error"))
  context <- util_attr(
    result$SegmentData,
    "entity_grading_context",
    exact = TRUE
  )
  expect_identical(context$entity, "SEGMENT")
  expect_identical(context$grading_rule_sets, "0")
})

test_that("TableSlot printing applies attached entity grading", {
  skip_on_cran()

  attached <- local_attach_entity_grading()
  table <- attached$SegmentTable
  class(table) <- c("TableSlot", class(table))
  seen <- new.env(parent = emptyenv())
  seen$args <- NULL
  seen$table <- NULL
  testthat::local_mocked_bindings(
    util_html_table = function(tb, additional_init_args, ...) {
      seen$table <- tb
      seen$args <- additional_init_args
      htmltools::div("rendered")
    },
    util_metrics_to_classes = function(rs_table_long, ...) {
      rs_table_long$class <- c(1, 2)
      rs_table_long
    }
  )

  expect_no_error(print.TableSlot(table, view = FALSE))
  expect_match(seen$table[[2L]][[1L]], "data-grading", fixed = TRUE)
  expect_identical(
    seen$args$grading_cols,
    "Non-response rate (Percentage (0 to 100))"
  )
})
