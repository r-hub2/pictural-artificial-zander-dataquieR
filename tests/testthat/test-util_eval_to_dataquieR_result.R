test_that("util_eval_to_dataquieR_result records empty and condition results", {
  skip_on_cran()

  empty_result <- util_eval_to_dataquieR_result(
    quote(list()),
    filter_result_slots = character(),
    nm = "call.alpha",
    function_name = "dummy_fun",
    called_in_pipeline = FALSE
  )

  expect_s3_class(empty_result, "dataquieR_result")
  expect_s3_class(empty_result, "dataquieR_NULL")
  expect_identical(names(empty_result), NULL)
  expect_identical(attr(empty_result, "cn", exact = TRUE), "call")
  expect_identical(
    attr(empty_result, "function_name", exact = TRUE),
    "dummy_fun"
  )
  expect_length(attr(empty_result, "error", exact = TRUE), 0)

  condition_result <- util_eval_to_dataquieR_result(
    quote({
      warning("careful")
      message("noted")
      stop("boom")
    }),
    filter_result_slots = character(),
    nm = "call.beta",
    function_name = "dummy_fun",
    called_in_pipeline = FALSE
  )

  expect_s3_class(condition_result, "dataquieR_NULL")
  expect_equal(
    vapply(
      attr(condition_result, "error", exact = TRUE),
      conditionMessage,
      character(1)
    ),
    "boom"
  )
  expect_equal(
    vapply(
      attr(condition_result, "warning", exact = TRUE),
      conditionMessage,
      character(1)
    ),
    "careful"
  )
  expect_match(
    vapply(
      attr(condition_result, "message", exact = TRUE),
      conditionMessage,
      character(1)
    ),
    "^noted"
  )
})

test_that(
  "util_eval_to_dataquieR_result filters result slots before validation",
  {
    skip_on_cran()

    result <- util_eval_to_dataquieR_result(
      quote(list(
        SummaryTable = data.frame(Variables = "v1", metric = 1),
        Other = "drop"
      )),
      filter_result_slots = "^SummaryTable$",
      nm = "call.gamma",
      function_name = "dummy_fun",
      called_in_pipeline = FALSE
    )

    expect_s3_class(result, "dataquieR_result")
    expect_identical(names(result), "SummaryTable")
    expect_s3_class(result$SummaryTable, "TableSlot")
    expect_equal(result$SummaryTable$Variables, "v1")
    expect_equal(result$SummaryTable$metric, 1)
    expect_length(attr(result, "error", exact = TRUE), 0)
  }
)

test_that("generated variable-group context reaches result tables", {
  skip_on_cran()

  group_call <- quote(dummy_group_function())
  attr(group_call, CHECK_ID) <- "group-1"
  attr(group_call, CHECK_LABEL) <- "Repeated measurements"
  result <- util_eval_to_dataquieR_result(
    quote(list(
      VariableGroupTable = data.frame(metric = 1),
      VariableGroupData = data.frame(value = 2),
      OtherTable = data.frame(detail = 3)
    )),
    filter_result_slots = character(),
    nm = "dummy_group_function.group-1",
    function_name = "dummy_group_function",
    my_call = group_call,
    called_in_pipeline = FALSE
  )

  expect_identical(attr(result, CHECK_ID, exact = TRUE), "group-1")
  expect_identical(
    attr(result, CHECK_LABEL, exact = TRUE),
    "Repeated measurements"
  )
  for (component in c(
    "VariableGroupTable", "VariableGroupData", "OtherTable"
  )) {
    expect_identical(result[[component]][[CHECK_ID]], "group-1")
    expect_identical(
      result[[component]][[CHECK_LABEL]],
      "Repeated measurements"
    )
  }
})

test_that("util_eval_to_dataquieR_result falls back for unnamed expressions", {
  skip_on_cran()

  result <- util_eval_to_dataquieR_result(
    quote(1),
    filter_result_slots = character(),
    nm = "call.delta",
    called_in_pipeline = FALSE
  )

  expect_s3_class(result, "dataquieR_result")
  expect_identical(
    attr(result, "function_name", exact = TRUE),
    "unknown function"
  )
  expect_s3_class(
    attr(result, "error", exact = TRUE),
    "dataquieR_invalid_result_error"
  )
  expect_s3_class(attr(result, "error", exact = TRUE)[[1]], "rlang_error")
})

test_that("util_eval_to_dataquieR_result stores and resumes storr results", {
  skip_on_cran()
  skip_if_not_installed("storr")

  backend <- prep_create_storr_factory(
    namespace = "Test",
    db_dir = withr::local_tempdir()
  )()
  my_call <- quote(des_summary(v1))
  attr(my_call, VAR_NAMES) <- "v1"
  attr(my_call, STUDY_SEGMENT) <- "all_observations"

  stored <- util_eval_to_dataquieR_result(
    quote(list(SummaryTable = data.frame(Variables = "v1", metric = 1))),
    filter_result_slots = character(),
    nm = "des_summary.v1",
    function_name = "des_summary",
    my_call = my_call,
    my_storr_object = backend,
    called_in_pipeline = TRUE
  )

  expect_true(is.na(stored))
  expect_true(backend$exists("des_summary.v1"))
  raw_result <- backend$get("des_summary.v1")
  expect_type(raw_result, "raw")
  expect_named(util_decompress(raw_result), "SummaryTable")
  status_value <- backend$get(
    "des_summary.v1",
    namespace = util_get_storr_stat_namespace(backend)
  )
  summary_value <- backend$get(
    "des_summary.v1",
    namespace = util_get_storr_summ_namespace(backend)
  )
  expect_true(status_value)
  expect_s3_class(summary_value, "data.frame")

  eval_env <- new.env(parent = emptyenv())
  eval_env$calls <- 0L
  resumed <- util_eval_to_dataquieR_result(
    quote({
      calls <- calls + 1L
      list(SummaryTable = data.frame(Variables = "v1", metric = 2))
    }),
    env = eval_env,
    filter_result_slots = character(),
    nm = "des_summary.v1",
    function_name = "des_summary",
    my_call = my_call,
    my_storr_object = backend,
    called_in_pipeline = TRUE,
    checkpoint_resumed = TRUE
  )

  expect_true(is.na(resumed))
  expect_identical(eval_env$calls, 0L)
})
