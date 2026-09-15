skip_on_cran()

test_that("prep_extract_classes_by_functions is defunct", {
  external_env <- new.env(parent = globalenv())
  external_env$external_call <- get(
    "prep_extract_classes_by_functions",
    envir = asNamespace("dataquieR")
  )

  expect_error(
    evalq(external_call(list()), envir = external_env),
    "was deprecated in dataquieR 2.8.10.9001.1"
  )
})

test_that("prep_extract_classes_by_functions keeps legacy internal output", {
  skip_on_cran()

  make_result <- function(summary_table) {
    structure(
      list(SummaryTable = summary_table),
      class = "dataquieR_result"
    )
  }

  report <- structure(
    list(
      com_item_missingness.x = make_result(data.frame(
        Variables = "x",
        NUM_com_qum_nonresp = 1,
        check.names = FALSE
      ))
    ),
    class = "dataquieR_resultset2",
    all_calls = list(
      com_item_missingness.x = quote(com_item_missingness(x))
    ),
    rn = "Variable X",
    cn = "com_item_missingness",
    names = "com_item_missingness.x",
    matrix_list = structure(
      list(),
      row_indices = c("Variable X" = 1),
      col_indices = c(com_item_missingness = 1),
      function_alias_map = data.frame(
        alias = "com_item_missingness",
        name = "com_item_missingness",
        stringsAsFactors = FALSE
      )
    ),
    meta_data = data.frame(
      VAR_NAMES = "x",
      LABEL = "Variable X",
      GRADING_RULESET = 0,
      stringsAsFactors = FALSE
    ),
    label_col = LABEL
  )

  internal_env <- new.env(parent = asNamespace("dataquieR"))
  internal_env$report <- report

  classes <- evalq(
    prep_extract_classes_by_functions(report),
    envir = internal_env
  )

  expect_s3_class(classes, "data.frame")
  expect_named(classes, c(
    VAR_NAMES, "class", "indicator_metric", "value", "values_raw",
    "n_classes", STUDY_SEGMENT, "call_names", "function_name"
  ))
  expect_equal(classes[[VAR_NAMES]], "x")
  expect_equal(classes$indicator_metric, "NUM_com_qum_nonresp")
  expect_equal(classes$value, "1")
  expect_equal(classes$values_raw, 1)
  expect_equal(classes$call_names, "com_item_missingness")
  expect_equal(classes$function_name, "com_item_missingness")
})

test_that("prep_extract_classes_by_functions fills missing legacy columns", {
  skip_on_cran()

  testthat::local_mocked_bindings(
    prep_extract_summary = function(r) {
      "summary"
    },
    prep_summary_to_classes = function(summary) {
      data.frame(
        VAR_NAMES = "x",
        STUDY_SEGMENT = "main",
        call_names = "legacy_call",
        stringsAsFactors = FALSE
      )
    },
    util_cll_nm2fkt_nm = function(...) {
      "legacy_function"
    }
  )

  internal_env <- new.env(parent = asNamespace("dataquieR"))
  internal_env$report <- list()

  classes <- evalq(
    prep_extract_classes_by_functions(report),
    envir = internal_env
  )

  expect_named(classes, c(
    VAR_NAMES, "class", "indicator_metric", "value", "values_raw",
    "n_classes", STUDY_SEGMENT, "call_names", "function_name"
  ))
  expect_equal(classes[[VAR_NAMES]], "x")
  expect_true(is.na(classes$class))
  expect_true(is.na(classes$indicator_metric))
  expect_true(is.na(classes$value))
  expect_true(is.na(classes$values_raw))
  expect_true(is.na(classes$n_classes))
  expect_equal(classes$call_names, "legacy_call")
  expect_equal(classes$function_name, "legacy_function")
})

test_that("prep_extract_classes_by_functions handles empty legacy summaries", {
  skip_on_cran()

  testthat::local_mocked_bindings(
    prep_extract_summary = function(r) {
      "summary"
    },
    prep_summary_to_classes = function(summary) {
      data.frame(
        VAR_NAMES = character(),
        STUDY_SEGMENT = character(),
        stringsAsFactors = FALSE
      )
    }
  )

  internal_env <- new.env(parent = asNamespace("dataquieR"))
  internal_env$report <- list()

  classes <- evalq(
    prep_extract_classes_by_functions(report),
    envir = internal_env
  )

  expect_named(classes, c(
    VAR_NAMES, "class", "indicator_metric", "value", "values_raw",
    "n_classes", STUDY_SEGMENT, "call_names", "function_name"
  ))
  expect_equal(nrow(classes), 0)
})
