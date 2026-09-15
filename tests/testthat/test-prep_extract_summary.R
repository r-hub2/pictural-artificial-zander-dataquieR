test_that("prep_extract_summary handles single result metrics", {
  skip_on_cran()

  cl <- quote(dummy_indicator(resp_vars = "x"))
  attr(cl, "entity_name") <- "Variable X"
  attr(cl, VAR_NAMES) <- "x"
  attr(cl, GRADING_RULESET) <- "rules"
  attr(cl, STUDY_SEGMENT) <- "Segment A"
  attr(cl, "label_col") <- LABEL

  summary_table <- data.frame(
    PCT_com_qum_nonresp = 12.345,
    NUM_com_qum_nonresp = 2,
    FLG_com_qum_nonresp = TRUE,
    check.names = FALSE
  )
  result <- structure(
    list(SummaryTable = summary_table),
    class = c("dataquieR_result", "list")
  )
  attr(result, "call") <- cl
  attr(result, "cn") <- "com_item_missingness"

  expect_silent(summary <- prep_extract_summary(result))

  expect_s3_class(summary, "dq_report2_summary")
  expect_equal(summary$Data$VAR_NAMES, "x")
  expect_equal(summary$Data$STUDY_SEGMENT, "Segment A")
  expect_equal(
    summary$Data$com_item_missingness.NUM_com_qum_nonresp,
    "2"
  )
  expect_equal(
    summary$Data$com_item_missingness.PCT_com_qum_nonresp,
    "12.34%"
  )
  expect_equal(
    summary$Data$com_item_missingness.FLG_com_qum_nonresp,
    "T"
  )
  expect_equal(
    summary$Table$com_item_missingness.NUM_com_qum_nonresp,
    2
  )
  expect_equal(
    summary$Table$com_item_missingness.PCT_com_qum_nonresp,
    12.345
  )
  expect_true(summary$Table$com_item_missingness.FLG_com_qum_nonresp)
  expect_equal(summary$meta_data[[VAR_NAMES]], "x")
  expect_equal(summary$meta_data[[LABEL]], "Variable X")
})

test_that("prep_extract_summary handles missing single-result call metadata", {
  skip_on_cran()

  cl <- quote(dummy_indicator())
  attr(cl, "entity_name") <- "Variable X"

  summary_table <- data.frame(
    NUM_com_qum_nonresp = 2,
    check.names = FALSE
  )
  result <- structure(
    list(SummaryTable = summary_table),
    class = c("dataquieR_result", "list")
  )
  attr(result, "call") <- cl
  attr(result, "cn") <- "com_item_missingness"

  summary <- prep_extract_summary(result)

  expect_equal(summary$Data[[VAR_NAMES]], "Variable X")
  expect_equal(summary$Data[[STUDY_SEGMENT]], "Study")
  expect_equal(summary$meta_data[[GRADING_RULESET]], 0)
  expect_equal(summary$meta_data[[LABEL]], "Variable X")
})

test_that("prep_extract_summary handles resultset2 segment defaults", {
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
        PCT_com_qum_nonresp = 25,
        FLG_com_qum_nonresp = FALSE,
        check.names = FALSE
      )),
      com_item_missingness.y = make_result(data.frame(
        Variables = "y",
        not_a_metric = "ignored"
      ))
    ),
    class = "dataquieR_resultset2",
    all_calls = list(
      com_item_missingness.x = quote(com_item_missingness(x)),
      com_item_missingness.y = quote(com_item_missingness(y))
    ),
    rn = c("Variable X", "Variable Y"),
    cn = c("com_item_missingness", "com_item_missingness"),
    names = c("com_item_missingness.x", "com_item_missingness.y"),
    matrix_list = structure(
      list(),
      row_indices = c("Variable X" = 1, "Variable Y" = 2),
      col_indices = c(com_item_missingness = 1)
    ),
    meta_data = data.frame(
      VAR_NAMES = c("x", "y"),
      LABEL = c("Variable X", "Variable Y"),
      stringsAsFactors = FALSE
    ),
    label_col = LABEL
  )

  expect_silent(summary <- prep_extract_summary(report))

  expect_s3_class(summary, "dq_report2_summary")
  expect_equal(nrow(summary$Data), 1)
  expect_equal(summary$Data[[VAR_NAMES]], "x")
  expect_equal(summary$Data[[STUDY_SEGMENT]], "Study")
  expect_equal(
    summary$Data$com_item_missingness.NUM_com_qum_nonresp,
    "1"
  )
  expect_equal(
    summary$Data$com_item_missingness.PCT_com_qum_nonresp,
    "25.00%"
  )
  expect_equal(
    summary$Data$com_item_missingness.FLG_com_qum_nonresp,
    "F"
  )
  expect_equal(summary$Table$com_item_missingness.NUM_com_qum_nonresp, 1)
  expect_equal(summary$Table$com_item_missingness.PCT_com_qum_nonresp, 25)
  expect_false(summary$Table$com_item_missingness.FLG_com_qum_nonresp)
})

test_that("prep_extract_summary preserves metrics without type prefixes", {
  skip_on_cran()

  result <- structure(
    list(SummaryTable = data.frame(
      Variables = "x",
      plain = 7,
      check.names = FALSE
    )),
    class = "dataquieR_result"
  )
  report <- structure(
    list(plain_metric.x = result),
    class = "dataquieR_resultset2",
    all_calls = list(plain_metric.x = quote(plain_metric(x))),
    rn = "Variable X",
    cn = "plain_metric",
    names = "plain_metric.x",
    matrix_list = structure(
      list(),
      row_indices = c("Variable X" = 1),
      col_indices = c(plain_metric = 1)
    ),
    meta_data = data.frame(
      VAR_NAMES = "x",
      LABEL = "Variable X",
      stringsAsFactors = FALSE
    ),
    label_col = LABEL
  )
  local_mocked_bindings(
    util_extract_indicator_metrics = function(x) x["plain"]
  )

  summary <- prep_extract_summary(report)

  expect_equal(summary$Data$plain_metric.plain, 7, ignore_attr = TRUE)
  expect_equal(summary$Table$plain_metric.plain, 7, ignore_attr = TRUE)
})

test_that("prep_extract_summary maps resultset2 study segments", {
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
        NUM_com_qum_nonresp = 3,
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
      col_indices = c(com_item_missingness = 1)
    ),
    meta_data = data.frame(
      VAR_NAMES = "x",
      LABEL = "Variable X",
      STUDY_SEGMENT = "Segment A",
      stringsAsFactors = FALSE
    ),
    label_col = LABEL
  )

  summary <- prep_extract_summary(report)

  expect_equal(summary$Data[[VAR_NAMES]], "x")
  expect_equal(summary$Data[[STUDY_SEGMENT]], "Segment A")
  expect_equal(summary$Table[[STUDY_SEGMENT]], "Segment A")
})

test_that("prep_extract_summary handles empty resultset2 segment maps", {
  skip_on_cran()

  report <- structure(
    list(),
    class = "dataquieR_resultset2",
    all_calls = list(),
    rn = character(),
    cn = character(),
    names = character(),
    matrix_list = structure(
      list(),
      row_indices = numeric(),
      col_indices = numeric()
    ),
    meta_data = data.frame(
      VAR_NAMES = character(),
      LABEL = character(),
      STUDY_SEGMENT = character(),
      stringsAsFactors = FALSE
    ),
    label_col = LABEL
  )

  summary <- prep_extract_summary(report)

  expect_s3_class(summary, "dq_report2_summary")
  expect_equal(nrow(summary$Data), 0)
  expect_identical(summary$Data[[STUDY_SEGMENT]], character())
  expect_identical(summary$Data[[VAR_NAMES]], character())
})

test_that("prep_extract_summary warns external callers about deprecation", {
  skip_on_cran()

  withr::local_options(lifecycle_verbosity = "warning")

  cl <- quote(dummy_indicator(resp_vars = "x"))
  attr(cl, "entity_name") <- "Variable X"
  attr(cl, VAR_NAMES) <- "x"
  attr(cl, STUDY_SEGMENT) <- "Study"
  attr(cl, "label_col") <- LABEL

  result <- structure(
    list(SummaryTable = data.frame(NUM_com_qum_nonresp = 1)),
    class = c("dataquieR_result", "list")
  )
  attr(result, "call") <- cl
  attr(result, "cn") <- "com_item_missingness"
  assign(".qif_prep_extract_summary_result", result, envir = globalenv())
  withr::defer(
    rm(".qif_prep_extract_summary_result", envir = globalenv())
  )

  seen <- new.env(parent = emptyenv())
  seen$classes <- list()
  summary <- withCallingHandlers(
    evalq(
      prep_extract_summary(.qif_prep_extract_summary_result),
      envir = globalenv()
    ),
    warning = function(w) {
      seen$classes[[length(seen$classes) + 1L]] <- class(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(vapply(
    seen$classes,
    function(x) "lifecycle_warning_deprecated" %in% x,
    logical(1)
  )))
  expect_s3_class(summary, "dq_report2_summary")
})
