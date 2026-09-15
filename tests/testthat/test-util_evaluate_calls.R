test_that("util_evaluate_calls handles empty call lists", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = character(),
    DATA_TYPE = character(),
    stringsAsFactors = FALSE
  )

  expect_message(
    result <- suppressWarnings(util_evaluate_calls(
      all_calls = list(),
      study_data = data.frame(),
      meta_data = meta_data,
      label_col = VAR_NAMES,
      meta_data_segment = data.frame(),
      meta_data_dataframe = data.frame(),
      meta_data_cross_item = data.frame(),
      resp_vars = character(),
      filter_result_slots = character(),
      cores = NULL,
      debug_parallel = FALSE,
      mode = "default",
      mode_args = list("unnamed"),
      checkpoint_resumed = FALSE,
      dt_adjust = FALSE
    )),
    "mode_args"
  )

  expect_s3_class(result, "dataquieR_resultset2")
  expect_s3_class(result, "square_results")
  expect_length(result, 0)
  expect_null(attr(result, "all_calls", exact = TRUE))
})

test_that("util_evaluate_calls accepts named mode args for empty call lists", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = character(),
    DATA_TYPE = character(),
    stringsAsFactors = FALSE
  )

  expect_no_message(
    result <- suppressWarnings(util_evaluate_calls(
      all_calls = list(),
      study_data = data.frame(),
      meta_data = meta_data,
      label_col = VAR_NAMES,
      meta_data_segment = data.frame(),
      meta_data_dataframe = data.frame(),
      meta_data_cross_item = data.frame(),
      resp_vars = character(),
      filter_result_slots = character(),
      cores = NULL,
      debug_parallel = FALSE,
      mode = "default",
      mode_args = list(step = 1),
      checkpoint_resumed = FALSE,
      dt_adjust = FALSE,
      content_file = "content.html"
    ))
  )

  expect_s3_class(result, "dataquieR_resultset2")
  expect_s3_class(result, "square_results")
  expect_length(result, 0)
})

test_that("util_evaluate_calls queue setup clamps invalid core counts", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = character(),
    DATA_TYPE = character(),
    stringsAsFactors = FALSE
  )
  seen <- new.env(parent = emptyenv())
  seen$n_nodes <- NA_integer_
  run_with_cores <- function(cores = 99L) {
    util_evaluate_calls(
      all_calls = list(),
      study_data = data.frame(),
      meta_data = meta_data,
      label_col = VAR_NAMES,
      meta_data_segment = data.frame(),
      meta_data_dataframe = data.frame(),
      meta_data_cross_item = data.frame(),
      resp_vars = character(),
      filter_result_slots = character(),
      cores = cores,
      debug_parallel = TRUE,
      mode = "queue",
      mode_args = list(),
      checkpoint_resumed = FALSE,
      dt_adjust = FALSE
    )
  }
  run_without_cores <- function() {
    util_evaluate_calls(
      all_calls = list(),
      study_data = data.frame(),
      meta_data = meta_data,
      label_col = VAR_NAMES,
      meta_data_segment = data.frame(),
      meta_data_dataframe = data.frame(),
      meta_data_cross_item = data.frame(),
      resp_vars = character(),
      filter_result_slots = character(),
      debug_parallel = TRUE,
      mode = "queue",
      mode_args = list(),
      checkpoint_resumed = FALSE,
      dt_adjust = FALSE
    )
  }

  testthat::local_mocked_bindings(
    util_detect_cores = function() 2L,
    util_queue_cluster_setup = function(
      n_nodes,
      progress,
      debug_parallel,
      my_storr_object
    ) {
      seen$n_nodes <- n_nodes
      seen$debug_parallel <- debug_parallel
      seen$has_storr <- !is.null(my_storr_object)
      list(
        workerEval = function(...) stop("unused workerEval"),
        export = function(...) stop("unused export"),
        compute_report = function(...) stop("unused compute_report")
      )
    }
  )

  expect_no_message(
    result <- suppressWarnings(run_with_cores()),
  )

  expect_identical(seen$n_nodes, 2L)
  expect_true(seen$debug_parallel)
  expect_false(seen$has_storr)
  expect_s3_class(result, "dataquieR_resultset2")
  expect_length(result, 0)

  seen$n_nodes <- NA_integer_
  expect_no_message(
    result_missing_cores <- suppressWarnings(run_without_cores()),
  )
  expect_identical(seen$n_nodes, 2L)
  expect_s3_class(result_missing_cores, "dataquieR_resultset2")
  expect_length(result_missing_cores, 0)
})

test_that("util_evaluate_calls queue mode computes local calls", {
  skip_on_cran()

  v1 <- 1:3
  des_summary <- function(v1) {
    list(
      SummaryTable = data.frame(Variables = "v1", metric = length(v1)),
      SummaryData = data.frame(Variables = "v1", metric = length(v1))
    )
  }
  my_call <- quote(des_summary(v1))
  attr(my_call, VAR_NAMES) <- "v1"
  attr(my_call, STUDY_SEGMENT) <- "all_observations"
  all_calls <- list(des_summary.v1 = my_call)
  attr(all_calls, "cn") <- "des_summary"
  attr(all_calls, "rn") <- "v1"

  meta_data <- data.frame(
    VAR_NAMES = "v1",
    DATA_TYPE = DATA_TYPES$INTEGER,
    stringsAsFactors = FALSE
  )
  seen <- new.env(parent = emptyenv())
  seen$step <- NA_integer_
  run_with_queue <- function(cores = 1L) {
    util_evaluate_calls(
      all_calls = all_calls,
      study_data = data.frame(v1 = 1:3),
      meta_data = meta_data,
      label_col = VAR_NAMES,
      meta_data_segment = data.frame(),
      meta_data_dataframe = data.frame(),
      meta_data_cross_item = data.frame(),
      resp_vars = "v1",
      filter_result_slots = character(),
      cores = cores,
      debug_parallel = FALSE,
      mode = "queue",
      mode_args = list(step = "bad"),
      checkpoint_resumed = FALSE,
      dt_adjust = FALSE
    )
  }

  testthat::local_mocked_bindings(
    util_detect_cores = function() 1L,
    util_queue_cluster_setup = function(...) {
      list(
        workerEval = function(fun, args = list()) {
          if ("expr" %in% names(args)) {
            return(list(TRUE))
          }
          list(do.call(fun, args))
        },
        export = function(...) invisible(NULL),
        compute_report = function(all_calls, worker, step) {
          seen$step <- step
          seen$worker_has_group_identity <- exists(
            "util_add_variable_group_identity",
            envir = environment(worker),
            inherits = FALSE
          )
          seen$worker_has_entity_grading <- exists(
            "util_attach_entity_grading_context",
            envir = environment(worker),
            inherits = FALSE
          )
          seen$worker_has_entity_rulesets <- exists(
            "util_entity_grading_rulesets",
            envir = environment(worker),
            inherits = FALSE
          )
          seen$entity_grading_uses_worker_env <- identical(
            environment(get(
              "util_attach_entity_grading_context",
              envir = environment(worker),
              inherits = FALSE
            )),
            environment(worker)
          )
          setNames(
            Map(
              function(call, nm) {
                worker(
                  expression = call,
                  nm = nm,
                  function_name = "des_summary",
                  my_call = call
                )
              },
              all_calls,
              names(all_calls)
            ),
            names(all_calls)
          )
        }
      )
    }
  )

  expect_message(
    result <- suppressWarnings(run_with_queue()),
    "falling back to default"
  )

  expect_identical(seen$step, 6)
  expect_true(seen$worker_has_group_identity)
  expect_true(seen$worker_has_entity_grading)
  expect_true(seen$worker_has_entity_rulesets)
  expect_true(seen$entity_grading_uses_worker_env)
  expect_s3_class(result, "dataquieR_resultset2")
  expect_s3_class(result$des_summary.v1, "dataquieR_result")
  expect_equal(result$des_summary.v1$SummaryTable$metric, 3)
  expect_named(
    attr(attr(result, "matrix_list", exact = TRUE), "row_indices"),
    "v1"
  )
})

test_that("util_evaluate_calls finalizes a minimal in-memory resultset", {
  skip_on_cran()

  my_call <- quote(des_summary(v1))
  attr(my_call, VAR_NAMES) <- "v1"
  attr(my_call, STUDY_SEGMENT) <- "all_observations"
  all_calls <- list(des_summary.v1 = my_call)
  attr(all_calls, "cn") <- "des_summary"
  attr(all_calls, "rn") <- "v1"

  meta_data <- data.frame(
    VAR_NAMES = "v1",
    DATA_TYPE = "integer",
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(suppressMessages(util_evaluate_calls(
    all_calls = all_calls,
    study_data = data.frame(v1 = 1:3),
    meta_data = meta_data,
    label_col = VAR_NAMES,
    meta_data_segment = data.frame(),
    meta_data_dataframe = data.frame(),
    meta_data_cross_item = data.frame(),
    resp_vars = "v1",
    filter_result_slots = character(),
    cores = NULL,
    debug_parallel = FALSE,
    mode = "default",
    mode_args = list(),
    checkpoint_resumed = FALSE,
    dt_adjust = FALSE
  )))

  expect_s3_class(result, "dataquieR_resultset2")
  expect_named(result, "des_summary.v1")
  expect_false(attr(result, "dt_adjust", exact = TRUE))
  expect_named(attr(result, "referred_tables", exact = TRUE), character())
  expect_named(
    attr(attr(result, "matrix_list", exact = TRUE), "row_indices"),
    "v1"
  )
})

test_that("util_evaluate_calls applies non-disclosure during finalization", {
  skip_on_cran()

  my_call <- quote(des_summary(v1))
  attr(my_call, VAR_NAMES) <- "v1"
  attr(my_call, STUDY_SEGMENT) <- "all_observations"
  all_calls <- list(des_summary.v1 = my_call)
  attr(all_calls, "cn") <- "des_summary"
  attr(all_calls, "rn") <- "v1"

  meta_data <- data.frame(
    VAR_NAMES = "v1",
    DATA_TYPE = "integer",
    stringsAsFactors = FALSE
  )
  withr::local_options(dataquieR.non_disclosure = TRUE)

  testthat::local_mocked_bindings(
    util_undisclose = function(x) {
      attr(x, "undisclosed_for_test") <- TRUE
      x
    },
    .package = "dataquieR"
  )

  result <- suppressWarnings(suppressMessages(util_evaluate_calls(
    all_calls = all_calls,
    study_data = data.frame(v1 = 1:3),
    meta_data = meta_data,
    label_col = VAR_NAMES,
    meta_data_segment = data.frame(),
    meta_data_dataframe = data.frame(),
    meta_data_cross_item = data.frame(),
    resp_vars = "v1",
    filter_result_slots = character(),
    cores = NULL,
    debug_parallel = FALSE,
    mode = "default",
    mode_args = list(),
    checkpoint_resumed = FALSE,
    dt_adjust = FALSE
  )))

  expect_s3_class(result, "dataquieR_resultset2")
  expect_true(attr(result, "undisclosed_for_test", exact = TRUE))
})

test_that("util_evaluate_calls skips checkpointed storr results", {
  skip_on_cran()
  skip_if_not_installed("storr")

  backend <- prep_create_storr_factory(
    namespace = "Test",
    db_dir = withr::local_tempdir()
  )()
  backend$set(
    "des_summary.v1",
    TRUE,
    namespace = util_get_storr_stat_namespace(backend)
  )

  my_call <- quote(des_summary(v1))
  attr(my_call, VAR_NAMES) <- "v1"
  attr(my_call, STUDY_SEGMENT) <- "all_observations"
  all_calls <- list(des_summary.v1 = my_call)
  attr(all_calls, "cn") <- "des_summary"
  attr(all_calls, "rn") <- "v1"

  meta_data <- data.frame(
    VAR_NAMES = "v1",
    DATA_TYPE = "integer",
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(suppressMessages(util_evaluate_calls(
    all_calls = all_calls,
    study_data = data.frame(v1 = 1:3),
    meta_data = meta_data,
    label_col = VAR_NAMES,
    meta_data_segment = data.frame(),
    meta_data_dataframe = data.frame(),
    meta_data_cross_item = data.frame(),
    resp_vars = "v1",
    filter_result_slots = character(),
    cores = NULL,
    debug_parallel = FALSE,
    mode = "default",
    mode_args = list(),
    my_storr_object = backend,
    checkpoint_resumed = TRUE,
    dt_adjust = FALSE
  )))

  expect_s3_class(result, "dataquieR_resultset2")
  expect_true(is.na(unclass(result)[["des_summary.v1"]]))
  expect_identical(
    attr(result, "my_storr_object", exact = TRUE),
    backend
  )
})

test_that("worker cache helpers strip and restore raw study data attrs", {
  skip_on_cran()

  prepared <- data.frame(x = 1)
  raw_study_data <- data.frame(x = 2)
  attr(prepared, "study_data") <- raw_study_data
  other <- data.frame(y = 1)

  stripped <- util_worker_cache_strip_study_data_attrs(list(
    prepared = prepared,
    other = other
  ))

  prepared_study_data <- attr(
    stripped$study_data_cache$prepared,
    "study_data",
    exact = TRUE
  )
  expect_null(prepared_study_data)
  raw_key <- attr(
    stripped$study_data_cache$prepared,
    "dataquieR_study_data_cache_raw_key",
    exact = TRUE
  )
  expect_match(raw_key, "^study_data@")
  expect_named(stripped$study_data_cache_study_data_attrs, raw_key)
  other_study_data <- attr(
    stripped$study_data_cache$other,
    "study_data",
    exact = TRUE
  )
  expect_null(other_study_data)

  restored <- util_worker_cache_restore_study_data_attrs(
    stripped$study_data_cache,
    stripped$study_data_cache_study_data_attrs
  )

  expect_identical(
    attr(restored$prepared, "study_data", exact = TRUE),
    raw_study_data
  )
  restored_raw_key <- attr(
    restored$prepared,
    "dataquieR_study_data_cache_raw_key",
    exact = TRUE
  )
  expect_null(restored_raw_key)
  expect_null(attr(restored$other, "study_data", exact = TRUE))
})

test_that("worker cache helpers handle missing raw attr payloads", {
  skip_on_cran()

  prepared <- data.frame(x = 1)
  attr(prepared, "dataquieR_study_data_cache_raw_key") <- "study_data@missing"

  restored <- util_worker_cache_restore_study_data_attrs(
    study_data_cache = list(prepared = prepared),
    study_data_attrs = list()
  )

  expect_identical(
    attr(restored$prepared, "dataquieR_study_data_cache_raw_key", exact = TRUE),
    "study_data@missing"
  )
  expect_null(attr(restored$prepared, "study_data", exact = TRUE))

  empty_payload <- util_worker_cache_payload(precompute = FALSE)

  expect_named(empty_payload, c(
    "cache_as_list",
    "study_data_cache",
    "study_data_cache_study_data_attrs",
    "study_data_cache_input_keys",
    "study_data_cache_meta_data"
  ))
  expect_true(all(vapply(empty_payload, length, integer(1)) == 0L))
})
