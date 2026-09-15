skip_on_cran()

test_that("util_generate_table_scale", {
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  skip_on_cran() # slow test
  target <- withr::local_tempdir("testdqareport_ssi")

  sd0 <- head(prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE), 20) # nolint: line_length_linter.
  sd0 <- sd0[, 1:7]
  sd0[2:3, 3:7]  <- NA


  cil <- data.frame(VARIABLE_LIST = c("v00003 | v00004 | v00005| v01003"),
    CHECK_LABEL = c("ssi_test"),
    MISS_RESP = c("[;2)"))
  itl <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|item_level") # nolint: line_length_linter.

  itl <- itl[itl$VAR_NAMES %in% c("v00000",
      "v00001",
      "v00002",
      "v00003",
      "v00004",
      "v00005",
      "v01003"), ]
  itl <- itl[, c("VAR_NAMES",
      "LABEL",
      "DATA_TYPE",
      "SCALE_LEVEL",
      "VALUE_LABELS")]

  itl$JUMP_LIST <- ""
  itl$MISSING_LIST <- ""
  itl$LONG_LABEL <- itl$LABEL

  suppressWarnings(rep_ssi_test <- dq_report2(study_data = sd0,
      cross_item_level = cil,
      item_level = itl,
      dimensions = "Int",
      cores = NULL
    ))

  tab1 <- util_generate_table_scale(report = rep_ssi_test,
    repsum = summary(rep_ssi_test))

  expect_false("N" %in% colnames(tab1))
  expect_equal(nrow(tab1), 1)
  expect_equal(tab1[["Requested variable-group metric"]], "Missing responses")
  expect_equal(sum(tab1[["Computed variable-group results"]]), 1)
  expect_true(is.data.frame(attr(tab1, "computed_variable_group_rows")))
  expect_true(all(c("SSI", "CHECK_ID", "VAR_NAMES") %in% colnames(
    attr(tab1, "computed_variable_group_rows")
  )))

  tab2 <- util_generate_table_scale(report = rep_ssi_test)
  expect_identical(tab2, tab1)
})

test_that("util_generate_table_scale represents an empty report", {
  function_alias_map <- data.frame(
    alias = character(),
    name = character()
  )
  matrix_list <- structure(list(),
    function_alias_map = function_alias_map
  )
  report <- structure(list(),
    class = "dataquieR_resultset2",
    matrix_list = matrix_list
  )
  this <- new.env(parent = emptyenv())
  this$result <- matrix(numeric(), nrow = 0, ncol = 0)
  repsum <- structure(list(), this = this)

  tab <- util_generate_table_scale(report = report, repsum = repsum)

  expect_equal(nrow(tab), 0)
  expect_false("N" %in% colnames(tab))
  expect_true("Computed variable-group results" %in% colnames(tab))
  expect_equal(nrow(attr(tab, "computed_variable_group_rows")), 0L)
})

test_that("util_generate_table_scale keeps requested zero-result metrics", {
  function_alias_map <- data.frame(
    alias = character(),
    name = character()
  )
  matrix_list <- structure(list(),
    function_alias_map = function_alias_map
  )
  report <- structure(list(),
    class = "dataquieR_resultset2",
    matrix_list = matrix_list,
    meta_data_cross_item = data.frame(
      VARIABLE_LIST = "v1 | v2",
      CHECK_LABEL = "missingness",
      MISS_RESP = "[;2)",
      stringsAsFactors = FALSE
    )
  )
  this <- new.env(parent = emptyenv())
  this$result <- matrix(numeric(), nrow = 0, ncol = 0)
  repsum <- structure(list(), this = this)

  tab <- util_generate_table_scale(report = report, repsum = repsum)

  expect_equal(nrow(tab), 1)
  expect_equal(tab[["Requested variable-group metric"]], "Missing responses")
  expect_equal(tab[["Computed variable-group results"]], 0)
  expect_equal(nrow(attr(tab, "computed_variable_group_rows")), 0L)
})

test_that("util_generate_table_scale handles no successful group metrics", {
  function_alias_map <- data.frame(
    alias = character(),
    name = character()
  )
  matrix_list <- structure(list(),
    function_alias_map = function_alias_map
  )
  report <- structure(list(),
    class = "dataquieR_resultset2",
    matrix_list = matrix_list
  )
  this <- new.env(parent = emptyenv())
  this$result <- data.frame(value = 1)
  this$stopped_functions <- logical()
  repsum <- structure(list(), this = this)

  tab <- util_generate_table_scale(report = report, repsum = repsum)

  expect_equal(nrow(tab), 0L)
  expect_equal(nrow(attr(tab, "computed_variable_group_rows")), 0L)
  expect_true(all(c("SSI", "CHECK_ID", VAR_NAMES) %in% names(
    attr(tab, "computed_variable_group_rows")
  )))
})

test_that("util_generate_table_scale skips results without group metadata", {
  function_alias_map <- data.frame(alias = "limits", name = "limits_ssi")
  matrix_list <- structure(list(), function_alias_map = function_alias_map)
  unmapped_call <- quote(limits_ssi())
  attr(unmapped_call, VAR_NAMES) <- "computed"
  report <- structure(
    list(
      limits.no_vars = structure(list(), call = quote(limits_ssi())),
      limits.unmapped = structure(list(), call = unmapped_call)
    ),
    class = "dataquieR_resultset2",
    matrix_list = matrix_list,
    meta_data = data.frame(VAR_NAMES = "computed", stringsAsFactors = FALSE)
  )
  this <- new.env(parent = emptyenv())
  this$result <- data.frame(value = 1)
  this$stopped_functions <- c(
    "limits.no_vars" = FALSE,
    "limits.unmapped" = FALSE
  )
  repsum <- structure(list(), this = this)

  tab <- util_generate_table_scale(report = report, repsum = repsum)

  expect_equal(nrow(attr(tab, "computed_variable_group_rows")), 0L)
})
