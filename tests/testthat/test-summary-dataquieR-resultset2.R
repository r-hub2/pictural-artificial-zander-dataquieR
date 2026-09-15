skip_on_cran()

make_summary_resultset2 <- function(values, rows, cols) {
  results <- lapply(values, function(value) {
    structure(list(value = value), class = c("dataquieR_result", "list"))
  })
  names(results) <- paste(cols, rows, sep = ".")
  class(results) <- c("dataquieR_resultset2", "list")
  attr(results, "names") <- names(results)
  attr(results, "rn") <- rows
  attr(results, "cn") <- cols

  matrix_list <- list()
  attr(matrix_list, "row_indices") <- setNames(seq_along(unique(rows)),
    unique(rows))
  attr(matrix_list, "col_indices") <- setNames(seq_along(unique(cols)),
    unique(cols))
  attr(results, "matrix_list") <- matrix_list

  results
}

test_that(
  "summary.dataquieR_resultset2 applies explicit FUN by row and column",
  {
    skip_on_cran()

    report <- make_summary_resultset2(
      values = c("a1", "a2", "b1", "b2"),
      rows = c("v1", "v2", "v1", "v2"),
      cols = c("call_a", "call_a", "call_b", "call_b")
    )

    result <- summary(
      report,
      aspect = "applicability",
      collapse = " | ",
      FUN = function(x, aspect, collapse, rn, cn) {
        paste(rn, cn, aspect, collapse, x$value, sep = "::")
      }
    )

    expect_equal(
      result,
      rbind(
        v1 = c(
          call_a = "v1::call_a::applicability:: | ::a1",
          call_b = "v1::call_b::applicability:: | ::b1"
        ),
        v2 = c(
          call_a = "v2::call_a::applicability:: | ::a2",
          call_b = "v2::call_b::applicability:: | ::b2"
        )
      )
    )
  }
)

test_that(
  "summary.dataquieR_resultset2 returns empty matrix without item results",
  {
    skip_on_cran()

    report <- make_summary_resultset2(
      values = "all",
      rows = "[ALL]",
      cols = "call_a"
    )

    result <- summary(
      report,
      aspect = "applicability",
      FUN = function(...) "unused"
    )

    expect_identical(dim(result), c(0L, 2L))
    expect_identical(colnames(result), c(VAR_NAMES, STUDY_SEGMENT))
  }
)

test_that("summary.dataquieR_resultset2 keeps an empty result schema", {
  skip_on_cran()

  result <- structure(
    list(SummaryTable = data.frame()),
    class = c("dataquieR_result", "list")
  )
  attr(result, "r_summary") <- data.frame()
  report <- structure(
    list(call_a.v1 = result),
    class = c("dataquieR_resultset2", "list")
  )
  attr(report, "names") <- "call_a.v1"
  attr(report, "rn") <- "v1"
  attr(report, "cn") <- "call_a"
  attr(report, "meta_data") <- data.frame(
    VAR_NAMES = "v1",
    LABEL = "Variable 1",
    STUDY_SEGMENT = "SEG",
    stringsAsFactors = FALSE
  )
  attr(report, "label_col") <- LABEL
  matrix_list <- list()
  attr(matrix_list, "row_indices") <- c(v1 = 1L)
  attr(matrix_list, "col_indices") <- c(call_a = 1L)
  attr(matrix_list, "function_alias_map") <- data.frame(
    name = "call_a",
    alias = "Call A",
    stringsAsFactors = FALSE
  )
  attr(report, "matrix_list") <- matrix_list

  empty_summary <- suppressWarnings(suppressMessages(summary(report)))
  empty_result <- attr(empty_summary, "this", exact = TRUE)$result

  expect_s3_class(empty_summary, "dataquieR_summary")
  expect_identical(nrow(empty_result), 0L)
})

test_that("summary.dataquieR_resultset2 reuses fresh cached summaries", {
  skip_on_cran()

  report <- structure(list(), class = c("dataquieR_resultset2", "list"))
  cached <- structure(NA, class = "dataquieR_summary")
  attr(cached, "rule_digest") <- rlang::hash(list(
    util_get_rule_sets(),
    util_get_ruleset_formats()
  ))
  attr(report, "repsum") <- cached

  expect_identical(summary(report), cached)
  expect_error(
    summary(report, aspect = "error"),
    "aspect is only supported for specific FUN values"
  )
})

test_that(
  paste0(
    "summary.dataquieR_resultset2 falls back for incomplete ",
    "grading formats"
  ),
  {
    skip_on_cran()

    result <- structure(
      list(SummaryTable = data.frame()),
      class = c("dataquieR_result", "list")
    )
    attr(result, "r_summary") <- data.frame(
      VAR_NAMES = "v1",
      class = "1",
      indicator_metric = "metric",
      value = "x",
      values_raw = "x",
      n_classes = 5,
      STUDY_SEGMENT = "SEG",
      call_names = "call_a",
      function_name = "call_a",
      stringsAsFactors = FALSE
    )
    report <- structure(
      list(call_a.v1 = result),
      class = c("dataquieR_resultset2", "list")
    )
    attr(report, "names") <- "call_a.v1"
    attr(report, "rn") <- "v1"
    attr(report, "cn") <- "call_a"
    attr(report, "meta_data") <- data.frame(
      VAR_NAMES = "v1",
      LABEL = "Variable 1",
      STUDY_SEGMENT = "SEG",
      stringsAsFactors = FALSE
    )
    attr(report, "label_col") <- LABEL
    matrix_list <- list()
    attr(matrix_list, "row_indices") <- c(v1 = 1L)
    attr(matrix_list, "col_indices") <- c(call_a = 1L)
    attr(matrix_list, "function_alias_map") <- data.frame(
      name = "call_a",
      alias = "Call A",
      stringsAsFactors = FALSE
    )
    attr(report, "matrix_list") <- matrix_list

    grading_calls <- new.env(parent = emptyenv())
    grading_calls$n <- 0L
    testthat::local_mocked_bindings(
      util_get_labels_grading_class = function() {
        grading_calls$n <- grading_calls$n + 1L
        if (identical(grading_calls$n, 1L)) {
          c(`1` = "Only")
        } else {
          setNames(paste("Class", seq_len(5)), seq_len(5))
        }
      },
      util_get_colors = function() {
        if (identical(grading_calls$n, 1L)) {
          c(`1` = "#111111")
        } else {
          setNames(rep("#ffffff", 5), seq_len(5))
        }
      }
    )

    summary_result <- suppressWarnings(suppressMessages(summary(report)))
    summary_context <- attr(summary_result, "this", exact = TRUE)

    expect_s3_class(summary_result, "dataquieR_summary")
    expect_true(all(
      paste0("cat", seq_len(5)) %in% names(summary_context$labels)
    ))
    expect_true(all(
      paste0("cat", seq_len(5)) %in% names(summary_context$colors)
    ))
    expect_equal(summary_context$ordered_call_names, "call_a")
    expect_gte(grading_calls$n, 2L)
  }
)

test_that("util_compute_rowmaxes tolerates missing class column", {
  result <- data.frame(
    VAR_NAMES = "v1",
    indicator_metric = "metric",
    stringsAsFactors = FALSE
  )
  labels <- setNames(as.list(c(" ", " ")), c("NA", "catNA"))
  colors <- setNames(as.list(c("#fff", "#fff")), c("NA", "catNA"))
  order_of <- setNames(c(NA_integer_, NA_integer_), c("NA", "catNA"))
  filter_of <- setNames(as.list(c(" ", " ")), c("NA", "catNA"))

  rowmaxes <- util_compute_rowmaxes(
    result = result,
    labels = labels,
    colors = colors,
    order_of = order_of,
    filter_of = filter_of,
    labels_of_var_names_in_report = c(v1 = "Variable 1")
  )

  expect_equal(rowmaxes$VAR_NAMES, "v1")
  expect_true(is.na(rowmaxes$rowmax))
  expect_equal(unname(rowmaxes$label), " ")
  expect_equal(unname(rowmaxes$color), "#fff")
  expect_true(grepl("VAR_Variable1.html#Variable1",
      rowmaxes$cell_text, fixed = TRUE))
})

test_that("util_compute_rowmaxes falls back to variable names", {
  result <- data.frame(
    VAR_NAMES = c("unknown", "unlabelled"),
    indicator_metric = "metric",
    stringsAsFactors = FALSE
  )
  labels <- setNames(as.list(c(" ", " ")), c("NA", "catNA"))
  colors <- setNames(as.list(c("#fff", "#fff")), c("NA", "catNA"))
  order_of <- setNames(c(NA_integer_, NA_integer_), c("NA", "catNA"))
  filter_of <- setNames(as.list(c(" ", " ")), c("NA", "catNA"))

  rowmaxes <- util_compute_rowmaxes(
    result = result,
    labels = labels,
    colors = colors,
    order_of = order_of,
    filter_of = filter_of,
    labels_of_var_names_in_report = c(unlabelled = NA_character_)
  )

  expect_named(rowmaxes$cell_text, c("unknown", "unlabelled"))
  expect_match(rowmaxes$cell_text[["unlabelled"]], "VAR_unlabelled")
  expect_match(rowmaxes$cell_text[["unknown"]], "VAR_unknown")
})

test_that("util_reclassify_dataquieR_summary reuses current rule digests", {
  skip_on_cran()

  summary <- structure(NA, class = "dataquieR_summary")
  attr(summary, "rule_digest") <- rlang::hash(list(
    util_get_rule_sets(),
    util_get_ruleset_formats()
  ))

  expect_identical(util_reclassify_dataquieR_summary(summary), summary)
})

test_that("util_reclassify_dataquieR_summary refreshes stale classes", {
  skip_on_cran()

  summary <- structure(NA, class = "dataquieR_summary")
  this <- new.env(parent = emptyenv())
  this$result <- data.frame(
    VAR_NAMES = "v1",
    indicator_metric = "metric",
    class = "1",
    stringsAsFactors = FALSE
  )
  this$meta_data <- data.frame(VAR_NAMES = "v1", stringsAsFactors = FALSE)
  this$labels <- as.list(c(
    cat1 = "Ok",
    cat2 = "Warn",
    cat3 = "Problem",
    cat4 = "Bad",
    cat5 = "Critical",
    catNA = " ",
    `NA` = " "
  ))
  this$colors <- as.list(c(
    cat1 = "#111111",
    cat2 = "#222222",
    cat3 = "#333333",
    cat4 = "#444444",
    cat5 = "#555555",
    catNA = "#ffffff",
    `NA` = "#ffffff"
  ))
  this$order_of <- c(
    cat1 = 1L,
    cat2 = 2L,
    cat3 = 3L,
    cat4 = 4L,
    cat5 = 5L,
    catNA = NA_integer_,
    `NA` = NA_integer_
  )
  this$filter_of <- as.list(c(
    cat1 = "Ok",
    cat2 = "Warn",
    cat3 = "Problem",
    cat4 = "Bad",
    cat5 = "Critical",
    catNA = " ",
    `NA` = " "
  ))
  this$labels_of_var_names_in_report <- c(v1 = "Variable 1")
  attr(summary, "this") <- this
  attr(summary, "rule_digest") <- "stale"

  testthat::local_mocked_bindings(
    util_metrics_to_classes = function(rs_table_long, meta_data) {
      expect_equal(meta_data$VAR_NAMES, "v1")
      rs_table_long$class <- "5"
      rs_table_long
    },
    util_get_rule_sets = function() list(rules = "new"),
    util_get_ruleset_formats = function() list(formats = "new")
  )

  refreshed <- util_reclassify_dataquieR_summary(summary)
  refreshed_this <- attr(refreshed, "this", exact = TRUE)

  expect_s3_class(refreshed, "dataquieR_summary")
  expect_false(identical(refreshed_this, this))
  expect_equal(this$result$class, "1")
  expect_equal(refreshed_this$result$class, "5")
  expect_equal(refreshed_this$rowmaxes$rowmax, util_as_cat("5"))
  expect_equal(unname(refreshed_this$rowmaxes$label), "Critical")
  expect_equal(
    attr(refreshed, "rule_digest", exact = TRUE),
    rlang::hash(list(list(rules = "new"), list(formats = "new")))
  )
})

test_that("variable-group summary rows require explicit CHECK_ID values", {
  meta_data_cross_item <- data.frame(
    CHECK_ID = c("group_a", "group_b"),
    CHECK_LABEL = c("Group A", "Group B"),
    VARIABLE_LIST = c("item_a | item_b", "item_a | item_b"),
    stringsAsFactors = FALSE
  )
  result <- data.frame(
    call_names = c("item_call", "group_call", "group_call"),
    VAR_NAMES = c("item_a", "technical_a", "technical_b"),
    LABEL = c("Item A", "Metric A", "Metric B"),
    CHECK_ID = c(NA_character_, "group_a", "group_b"),
    stringsAsFactors = FALSE
  )

  normalized <- util_summary_normalize_variable_group_rows(
    result = result,
    meta_data_cross_item = meta_data_cross_item,
    label_col = LABEL
  )

  expect_identical(
    normalized[[VAR_NAMES]],
    c("item_a", "group_a", "group_b")
  )
  expect_identical(normalized[[LABEL]], c("Item A", "Group A", "Group B"))
  expect_identical(
    normalized[[".variable_group_result_label"]],
    c(NA_character_, "Metric A", "Metric B")
  )
})

test_that("variable-group summary rows are not inferred without CHECK_ID", {
  result <- data.frame(
    call_names = "group_call",
    VAR_NAMES = "Item A | Item B",
    LABEL = "Item A | Item B",
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    CHECK_ID = "group_a",
    CHECK_LABEL = "Group A",
    VARIABLE_LIST = "item_a | item_b",
    stringsAsFactors = FALSE
  )

  expect_identical(
    util_summary_normalize_variable_group_rows(
      result = result,
      meta_data_cross_item = meta_data_cross_item,
      label_col = LABEL
    ),
    result
  )
})

test_that("explicit unknown group identities remain usable", {
  result <- data.frame(
    call_names = "external_group_call",
    VAR_NAMES = "technical_name",
    LABEL = "External group",
    CHECK_ID = "external_group",
    stringsAsFactors = FALSE
  )

  normalized <- util_summary_normalize_variable_group_rows(
    result = result,
    meta_data_cross_item = data.frame(),
    label_col = LABEL
  )

  expect_identical(normalized[[VAR_NAMES]], "external_group")
  expect_identical(normalized[[LABEL]], "External group")
  expect_identical(
    normalized[[".variable_group_result_label"]],
    "External group"
  )
})
