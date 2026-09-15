skip_on_cran()

test_that("util_render_report_scope_tree creates assessment tree grids", {
  info_dim_dq <- data.frame(
    Dimension = c("Integrity", "Completeness"),
    "No. DQ indicators" = c(1, 2),
    check.names = FALSE
  )
  info_scale_dq <- data.frame(
    "Requested variable-group metric" = c("Missing responses",
      "Mahalanobis distance"),
    "Computed variable-group results" = c(2, 0),
    check.names = FALSE
  )

  report <- structure(list(),
    class = "dataquieR_resultset2",
    matrix_list = structure(list(),
      function2category = c(int_datatype_matrix = "Integrity")
    ),
    meta_data = data.frame(
      VAR_NAMES = c("item_1", "item_2"),
      COMPUTED_VARIABLE_ROLE = c("", ""),
      stringsAsFactors = FALSE
    ),
    meta_data_cross_item = data.frame(
      CHECK_LABEL = c("group_1", "group_2"),
      MISS_RESP = c("[;1)", "[;1)"),
      stringsAsFactors = FALSE
    )
  )
  this <- new.env(parent = emptyenv())
  this$result <- data.frame(
    VAR_NAMES = c("item_1", "item_2"),
    function_name = "int_datatype_matrix",
    indicator_metric = "int_vfe_type",
    class = c("Ok", "Ok"),
    stringsAsFactors = FALSE
  )
  repsum <- structure(list(), this = this)
  tree <- util_render_report_scope_tree(info_dim_dq, info_scale_dq,
    report = report, repsum = repsum
  )
  html <- paste(as.character(tree), collapse = "")

  expect_match(html, "dq-report-scope-tree-table", fixed = TRUE)
  expect_false(grepl("<style", html, fixed = TRUE))
  expect_false(grepl("<script", html, fixed = TRUE))
  expect_match(html, "Expected results", fixed = TRUE)
  expect_match(html, "Assessed items", fixed = TRUE)
  expect_match(html, "Assessed groups", fixed = TRUE)
  expect_match(html, "<th>Computed</th>", fixed = TRUE)
  expect_false(grepl("<th>Classified</th>", html, fixed = TRUE))
  expect_false(grepl("Results with a grade", html, fixed = TRUE))
  expect_false(grepl("Expected classifications", html, fixed = TRUE))
  expect_match(html, "Result coverage", fixed = TRUE)
  expect_match(html, "dq-report-scope-header-help", fixed = TRUE)
  expect_match(html, "not applicable by definition", fixed = TRUE)
  expect_false(grepl("intrinsic", html, ignore.case = TRUE))
  expect_match(html, "DQ_OBS concept coverage", fixed = TRUE)
  expect_match(
    html,
    "computed results divided by expected results",
    fixed = TRUE
  )
  expect_match(
    html,
    "requested divided by the total number of applicable reference concepts",
    fixed = TRUE
  )
  expect_match(html, "Item-level assessment scope", fixed = TRUE)
  expect_match(html, "Variable-group assessment scope", fixed = TRUE)
  expect_match(html, "Integrity", fixed = TRUE)
  expect_match(html, "Consistency", fixed = TRUE)
  expect_match(html, "Missing responses", fixed = TRUE)
  expect_match(
    html,
    "The denominator is the number of expected results.",
    fixed = TRUE
  )
  expect_match(
    html,
    paste(
      "The requested percentage is the number of distinct reference concepts",
      "requested divided by the total number of applicable reference concepts."
    ),
    fixed = TRUE
  )
  expect_match(
    html,
    paste(
      "Each concept is counted once; unrequested concepts are excluded from",
      "these two percentages."
    ),
    fixed = TRUE
  )
  expect_match(html, "Requested concepts not computed", fixed = TRUE)
  expect_match(html, "Reference concepts not requested", fixed = TRUE)
})

test_that("scope-tree tables keep tree parent-child relationships", {
  nodes <- util_report_scope_tree_nodes(
    data.frame(
      Dimension = "Integrity",
      "No. DQ indicators" = 1,
      check.names = FALSE
    ),
    data.frame(
      "Requested variable-group metric" = "Missing responses",
      "Computed variable-group results" = 1,
      check.names = FALSE
    )
  )

  child <- nodes[[1]]
  child[["label"]] <- "Child node"
  child[["children"]] <- list()
  nodes[[1]][["children"]] <- list(child)

  table <- util_render_report_scope_tree_table(nodes[[1]])
  html <- paste(as.character(table), collapse = "")
  repeated_html <- paste(as.character(
    util_render_report_scope_tree_table(nodes[[1]])
  ), collapse = "")

  expect_identical(repeated_html, html)
  expect_match(html, "data-node-id", fixed = TRUE)
  expect_match(html, "data-parent-id", fixed = TRUE)
  expect_match(html, "dq-report-scope-tree-toggle", fixed = TRUE)
  expect_match(html, "dq_report_scope_tree_table_items", fixed = TRUE)
  expect_false(grepl("padding-left:0em;background", html, fixed = TRUE))
})

test_that("variable-group scope hierarchy follows DQ_OBS metric mappings", {
  hierarchy <- util_report_scope_variable_group_hierarchy_spec(c(
    "Missing responses",
    "Mahalanobis distance",
    "Not represented in DQ_OBS"
  ))
  labels_for <- function(node) {
    c(node[["label"]], unlist(lapply(
      node[["children"]] %||% list(), labels_for
    ), use.names = FALSE))
  }
  labels <- unlist(lapply(hierarchy, labels_for), use.names = FALSE)

  expect_true(all(c(
    "Consistency",
    "Range and value violations",
    "Invalid or missing responses",
    "Accuracy",
    "Mahalanobis Distance"
  ) %in% labels))
  expect_false("Unmapped variable-group metrics" %in% labels)
})

test_that("scope-tree DQ_OBS leaves do not repeat their metric", {
  info <- data.frame(
    "Requested variable-group metric" = "Missing responses",
    "Computed variable-group results" = 1L,
    check.names = FALSE
  )
  hierarchy <- util_report_scope_variable_group_hierarchy_spec(
    info[["Requested variable-group metric"]]
  )
  leaf <- hierarchy[[1]][["children"]][[1]][["children"]][[1]]
  node <- util_report_scope_variable_group_hierarchy_node(
    leaf,
    info_scale_dq = info,
    requested_groups = list("Missing responses" = "Group A"),
    computed_groups = list("Missing responses" = "Group A"),
    classified_groups = list("Missing responses" = "Group A")
  )

  expect_identical(node[["label"]], "Invalid or missing responses")
  expect_length(node[["children"]], 0L)
})

test_that("scope-tree hierarchy tolerates empty and missing DQ_OBS parents", {
  empty_dqi <- data.frame(
    Level = integer(),
    IndicatorID = character(),
    Parent_Element_ID = character(),
    Name = character(),
    public_name = character(),
    order_nr = integer(),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    util_get_concept_info = function(sheet) {
      expect_identical(sheet, "dqi")
      empty_dqi
    }
  )
  expect_identical(util_report_scope_dqi_hierarchy("missing"), list())

  orphan_dqi <- data.frame(
    Level = 3L,
    IndicatorID = "leaf",
    Parent_Element_ID = "missing-parent",
    Name = "Leaf concept",
    public_name = ".",
    order_nr = 1L,
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    util_get_concept_info = function(sheet) {
      expect_identical(sheet, "dqi")
      orphan_dqi
    }
  )
  hierarchy <- util_report_scope_dqi_hierarchy("leaf")
  expect_length(hierarchy, 1L)
  expect_identical(hierarchy[[1]][["label"]], "Leaf concept")
  expect_identical(hierarchy[[1]][["indicator_ids"]], "leaf")
})

test_that("scope-tree counts indicators but not descriptors", {
  matrix_list <- structure(list(), function2category = c(
    acc_distributions_only = "Accuracy",
    com_item_missingness = "Completeness"
  ))
  report <- structure(list(),
    class = "dataquieR_resultset2",
    matrix_list = matrix_list,
    meta_data = data.frame(
      VAR_NAMES = "v1",
      COMPUTED_VARIABLE_ROLE = "",
      stringsAsFactors = FALSE
    )
  )
  this <- new.env(parent = emptyenv())
  this$result <- data.frame(
    VAR_NAMES = c("v1", "v1"),
    function_name = c("acc_distributions_only", "com_item_missingness"),
    indicator_metric = c("acc_ud_loc", "PCT_com_crm_mv"),
    class = c("Ok", NA_character_),
    stringsAsFactors = FALSE
  )
  repsum <- structure(list(), this = this)

  coverage <- util_report_scope_item_coverage(report, repsum)

  expect_identical(coverage$function_name, "com_item_missingness")
  expect_identical(coverage$dimension, "Completeness")
  expect_identical(coverage$classifications, 0L)
  expect_identical(coverage$possible_classifications, 1L)
  expect_false(util_report_scope_function_is_indicator(
    "acc_distributions_only"
  ))
})

test_that("scope-tree counts a shared indicator metric once per item", {
  report <- structure(list(),
    class = "dataquieR_resultset2",
    meta_data = data.frame(
      VAR_NAMES = "v1",
      COMPUTED_VARIABLE_ROLE = "",
      stringsAsFactors = FALSE
    )
  )
  this <- new.env(parent = emptyenv())
  this$result <- data.frame(
    VAR_NAMES = c("v1", "v1"),
    function_name = c("acc_distributions", "acc_margins"),
    indicator_metric = c("acc_ud_loc", "acc_ud_loc"),
    class = c("Ok", "Important"),
    stringsAsFactors = FALSE
  )
  repsum <- structure(list(), this = this)

  coverage <- util_report_scope_item_coverage(report, repsum)

  expect_identical(nrow(coverage), 1L)
  expect_identical(coverage$indicator_id, "DQ_3_2_1_3")
  expect_identical(coverage$dimension, "Accuracy")
  expect_identical(coverage$classifications, 1L)
  expect_identical(coverage$possible_classifications, 1L)
})

test_that("variable-group scope uses direct group result metrics", {
  report <- structure(list(),
    class = "dataquieR_resultset2",
    meta_data_cross_item = data.frame(
      CHECK_ID = "1",
      CHECK_LABEL = "Repeated measurement group",
      stringsAsFactors = FALSE
    )
  )
  this <- new.env(parent = emptyenv())
  this$meta_data <- data.frame(
    VAR_NAMES = "item_1",
    LABEL = "Item 1",
    stringsAsFactors = FALSE
  )
  this$summary_meta_data <- this$meta_data
  this$label_col <- LABEL
  this$rownames_of_report <- "Item 1"
  this$variable_group_call_names <- "acc_repeated_measurements"
  this$result <- data.frame(
    VAR_NAMES = "1",
    indicator_metric = "NUM_acc_drm_inter",
    values_raw = 0.9,
    class = "Ok",
    call_names = "acc_repeated_measurements",
    stringsAsFactors = FALSE
  )
  repsum <- structure(list(), this = this)

  results <- util_report_scope_variable_group_result_metrics(report, repsum)
  summary <- util_report_scope_variable_group_result_summary(results)

  expect_identical(results$metric, "Inter-Class reliability")
  expect_identical(results$group, "Repeated measurement group")
  expect_true(results$computed)
  expect_true(results$classified)
  expect_identical(
    summary[["Requested variable-group metric"]],
    "Inter-Class reliability"
  )
  expect_identical(summary[["Computed variable-group results"]], 1L)
})

test_that("scope-tree tables initially expand only the first level", {
  node <- list(
    label = "Variable-group assessment scope",
    coverage = list(classifications = 1L, possible = 1L),
    concept_coverage = list(assessed = 1L, possible = 1L),
    measures = 1L,
    assessed = 1L,
    children = list(list(
      label = "Indirect measures",
      coverage = list(classifications = 1L, possible = 1L),
      concept_coverage = list(assessed = 1L, possible = 1L),
      measures = 1L,
      assessed = 1L,
      children = list(list(
        label = "Missing responses",
        coverage = list(classifications = 1L, possible = 1L),
        concept_coverage = list(assessed = 1L, possible = 1L),
        measures = 1L,
        assessed = 1L,
        children = list()
      ))
    ))
  )

  html <- paste(as.character(util_render_report_scope_tree_table(node)),
    collapse = ""
  )

  expect_match(html, 'aria-expanded="true"', fixed = TRUE)
  expect_match(html, 'aria-expanded="false"', fixed = TRUE)
})

test_that("scope-tree coverage reflects classified possible results", {
  matrix_list <- structure(list(),
    function2category = c(int_datatype_matrix = "Integrity")
  )
  report <- structure(list(),
    class = "dataquieR_resultset2",
    matrix_list = matrix_list,
    meta_data = data.frame(
      VAR_NAMES = c("v1", "v2", "v3"),
      COMPUTED_VARIABLE_ROLE = c("", "", ""),
      stringsAsFactors = FALSE
    )
  )
  this <- new.env(parent = emptyenv())
  this$result <- data.frame(
    VAR_NAMES = c("v1", "v2", "v3"),
    function_name = rep("int_datatype_matrix", 3),
    indicator_metric = "int_vfe_type",
    class = c("Ok", NA, "Ok"),
    stringsAsFactors = FALSE
  )
  repsum <- structure(list(), this = this)

  coverage <- util_report_scope_item_coverage(report, repsum)
  node <- util_report_scope_item_tree_nodes(
    data.frame(
      Dimension = "Integrity",
      "No. DQ indicators" = 1,
      check.names = FALSE
    ),
    report,
    repsum
  )
  style <- util_report_scope_tree_coverage_style(node[["coverage"]])

  expect_equal(coverage$classifications, c(1L, 0L, 1L))
  expect_equal(coverage$possible_classifications, rep(1L, 3))
  expect_match(node[["text"]],
    "2 of 3 expected results computed (67%); 2 classified (67%)",
    fixed = TRUE
  )
  expect_equal(style$background, "#D88C4B")
  expect_equal(style$title,
    "2 of 3 expected results computed (67%); 2 classified (67%)"
  )
})

test_that("item coverage retains computed results without classifications", {
  report <- structure(list(),
    class = "dataquieR_resultset2",
    matrix_list = structure(list(),
      function2category = c(int_datatype_matrix = "Integrity")
    ),
    meta_data = data.frame(
      VAR_NAMES = c("v1", "v2", "v3"),
      COMPUTED_VARIABLE_ROLE = "",
      stringsAsFactors = FALSE
    )
  )
  this <- new.env(parent = emptyenv())
  this$result <- data.frame(
    VAR_NAMES = c("v1", "v2", "v3"),
    function_name = "int_datatype_matrix",
    indicator_metric = "int_vfe_type",
    values_raw = c(1, 2, NA),
    class = c("Ok", NA, NA),
    stringsAsFactors = FALSE
  )
  repsum <- structure(list(), this = this)

  coverage <- util_report_scope_item_coverage(report, repsum)
  items <- util_report_scope_item_classification_items(coverage, report)

  expect_identical(coverage$computations, c(1L, 1L, 0L))
  expect_identical(coverage$classifications, c(1L, 0L, 0L))
  expect_identical(items$matrix$computed, c(TRUE, TRUE, FALSE))
  expect_identical(items$matrix$classified, c(TRUE, FALSE, FALSE))
})

test_that("scope-tree uses distinct result and concept coverage palettes", {
  result_style <- util_report_scope_tree_coverage_style(list(
    classifications = 2L,
    possible = 3L
  ))
  concept_style <- util_report_scope_tree_concept_coverage_style(list(
    assessed = 2L,
    possible = 3L
  ))

  expect_equal(result_style$background, "#D88C4B")
  expect_equal(concept_style$background, "#6495C6")
  computed_result_style <- util_report_scope_tree_coverage_style(list(
    classifications = 1L,
    computed = 3L,
    possible = 4L
  ))
  expect_identical(
    computed_result_style$background,
    util_report_scope_tree_coverage_style(list(
      classifications = 3L,
      possible = 4L
    ))$background
  )
  computed_concept_style <- util_report_scope_tree_concept_coverage_style(list(
    assessed = 1L,
    computed = 2L,
    possible = 3L
  ))
  expect_identical(
    computed_concept_style$background,
    concept_style$background
  )
  expect_match(concept_style$title,
    paste(
      "2 of 3 possible reference concepts requested (67%);",
      "2 of 2 requested concepts computed (100%);",
      "2 classified (100%)"
    ),
    fixed = TRUE
  )
  expect_match(
    paste(as.character(util_report_scope_tree_table_concept_coverage_cell(
      list(assessed = 2L, possible = 3L)
    )), collapse = ""),
    "67%",
    fixed = TRUE
  )
  coverage_html <- paste(as.character(
    util_report_scope_tree_table_coverage_cell(
      list(classifications = 2L, computed = 3L, possible = 4L)
    )
  ), collapse = "")
  expect_match(coverage_html, "75%", fixed = TRUE)
  expect_match(coverage_html, "50% classified", fixed = TRUE)
})

test_that("scope-tree coverage tooltips explain counts and concepts", {
  classification_tooltip <- util_report_scope_tree_coverage_tooltip(
    list(classifications = 2L, possible = 3L),
    list(
      possible = stats::setNames(
        c("Crude item missingness of SBP_0", "Inadmissible values of ITEM_0_1"),
        c("missing_sbp", "invalid_item")
      ),
      classified = stats::setNames(
        "Crude item missingness of SBP_0",
        "missing_sbp"
      ),
      row_label = "Item",
      matrix = data.frame(
        unit = c("SBP_0", "ITEM_0_1"),
        analysis = c("Crude item missingness", "Inadmissible values"),
        classified = c(TRUE, FALSE),
        stringsAsFactors = FALSE
      )
    )
  )
  concept_tooltip <- util_report_scope_tree_concept_coverage_tooltip(
    list(assessed = 1L, possible = 2L),
    list(
      assessed = stats::setNames("Covered concept", "covered"),
      possible = stats::setNames(
        c("Covered concept", "Reference concept"),
        c("covered", "reference")
      )
    )
  )

  expect_match(
    classification_tooltip,
    "Classified results: 2 of 3 (67%)",
    fixed = TRUE
  )
  expect_match(
    classification_tooltip,
    "Expected results: 3",
    fixed = TRUE
  )
  expect_match(
    classification_tooltip,
    "Computation unavailable: 1",
    fixed = TRUE
  )
  expect_match(classification_tooltip, "Classification matrix", fixed = TRUE)
  expect_match(
    classification_tooltip,
    "Inadmissible values",
    fixed = TRUE
  )
  expect_match(classification_tooltip, "ITEM_0_1", fixed = TRUE)
  expect_match(concept_tooltip, "Classified concepts (1)", fixed = TRUE)
  expect_match(
    concept_tooltip,
    "Requested concepts not computed (1)",
    fixed = TRUE
  )
  expect_match(concept_tooltip, "Reference concept", fixed = TRUE)
})

test_that("scope-tree tooltips distinguish computation from classification", {
  items <- list(
    possible = stats::setNames(
      paste("Metric of", c("Group A", "Group B", "Group C")),
      paste("Metric", c("Group A", "Group B", "Group C"), sep = "\f")
    ),
    computed = stats::setNames(
      paste("Metric of", c("Group A", "Group B")),
      paste("Metric", c("Group A", "Group B"), sep = "\f")
    ),
    classified = stats::setNames(
      "Metric of Group A",
      paste("Metric", "Group A", sep = "\f")
    ),
    row_label = "Variable group",
    matrix = data.frame(
      unit = c("Group A", "Group B", "Group C"),
      unit_label = c("Group A", "Group B", "Group C"),
      analysis = "Metric",
      computed = c(TRUE, TRUE, FALSE),
      classified = c(TRUE, FALSE, FALSE),
      applicable = TRUE,
      stringsAsFactors = FALSE
    )
  )
  tooltip <- util_report_scope_tree_coverage_tooltip(
    list(classifications = 1L, computed = 2L, possible = 3L),
    items
  )

  expect_match(tooltip, "Computed results: 2 of 3 (67%)", fixed = TRUE)
  expect_match(tooltip, "Classified results: 1 of 3 (33%)", fixed = TRUE)
  expect_match(tooltip, "Expected results: 3", fixed = TRUE)
  expect_match(tooltip, "Computed but not classified: 1", fixed = TRUE)
  expect_match(tooltip, "Computation unavailable: 1", fixed = TRUE)
  expect_match(tooltip, "Computed, not classified", fixed = TRUE)
})

test_that("DQ_OBS tooltip reports computed and classified concepts", {
  tooltip <- util_report_scope_tree_concept_coverage_tooltip(
    list(assessed = 1L, computed = 2L, possible = 3L),
    list(
      possible = stats::setNames(c("Concept A", "Concept B", "Concept C"),
        c("a", "b", "c")
      ),
      computed = stats::setNames(c("Concept A", "Concept B"), c("a", "b")),
      assessed = stats::setNames("Concept A", "a")
    )
  )

  expect_match(tooltip, "Computed concepts (2)", fixed = TRUE)
  expect_match(tooltip, "Classified concepts (1)", fixed = TRUE)
  expect_match(
    tooltip,
    "Computed concepts without a classification (1)",
    fixed = TRUE
  )
  expect_match(tooltip, "Concept B", fixed = TRUE)
  expect_match(tooltip, "Requested concepts not computed (1)", fixed = TRUE)
  expect_match(tooltip, "Concept C", fixed = TRUE)
})

test_that("DQ_OBS tooltip separates missing computation causes", {
  coverage <- list(assessed = 1L, computed = 1L, possible = 3L)
  items <- list(
    possible = stats::setNames(
      c("Concept A", "Concept B", "Concept C"),
      c("a", "b", "c")
    ),
    requested = stats::setNames(c("Concept A", "Concept B"), c("a", "b")),
    computed = stats::setNames("Concept A", "a"),
    assessed = stats::setNames("Concept A", "a")
  )

  tooltip <- util_report_scope_tree_concept_coverage_tooltip(coverage, items)

  expect_match(
    tooltip,
    paste(
      "2 of 3 possible reference concepts requested (67%);",
      "1 of 2 requested concepts computed (50%);",
      "1 classified (50%)"
    ),
    fixed = TRUE
  )
  expect_false(grepl(
    "3 of 3 possible reference concepts requested",
    tooltip,
    fixed = TRUE
  ))
  expect_match(tooltip, "Requested concepts not computed (1)", fixed = TRUE)
  expect_match(tooltip, "Concept B", fixed = TRUE)
  expect_match(tooltip, "Reference concepts not requested (1)", fixed = TRUE)
  expect_match(tooltip, "Concept C", fixed = TRUE)
  expect_match(
    tooltip,
    "Requested concepts not classified (1)",
    fixed = TRUE
  )
  not_classified <- sub(
    ".*Requested concepts not classified \\(1\\)",
    "",
    tooltip
  )
  not_classified <- sub(
    "Computed concepts without a classification.*",
    "",
    not_classified
  )
  expect_match(not_classified, "Concept B", fixed = TRUE)
  expect_false(grepl("Concept C", not_classified, fixed = TRUE))
})

test_that("scope-tree group classification labels retain group names", {
  rows <- data.frame(
    "Requested variable-group metric" = "Missing responses",
    "Computed variable-group results" = 2L,
    check.names = FALSE
  )
  items <- util_report_scope_variable_group_classification_items(
    rows,
    list("Missing responses" = c("Group A", "Group B")),
    list("Missing responses" = c("Group A", "Group B"))
  )

  expect_equal(
    unname(items[["classified"]]),
    c("Missing responses of Group A", "Missing responses of Group B")
  )
})

test_that("scope-tree group coverage retains concrete partial results", {
  rows <- data.frame(
    "Requested variable-group metric" = "Missing responses",
    "Computed variable-group results" = 2L,
    check.names = FALSE
  )
  summary <- util_report_scope_variable_group_summary(
    rows,
    list("Missing responses" = c("Group A", "Group B", "Group C", "Group D")),
    list("Missing responses" = c("Group B", "Group D")),
    list("Missing responses" = c("Group B", "Group D"))
  )

  expect_equal(summary$coverage$classifications, 2L)
  expect_equal(summary$coverage$possible, 4L)
  expect_match(
    util_report_scope_tree_coverage_label(summary$coverage),
    "2 of 4 expected results computed (50%); 2 classified (50%)",
    fixed = TRUE
  )
  expect_identical(
    summary$classification_items$matrix$classified,
    c(FALSE, TRUE, FALSE, TRUE)
  )
})

test_that("concept coverage counts only classified metrics as assessed", {
  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    util_report_scope_variable_group_concepts = function(
      assessed_metrics,
      concept_scope,
      report,
      requested_metrics,
      computed_metrics
    ) {
      seen$assessed <- assessed_metrics
      seen$computed <- computed_metrics
      seen$requested <- requested_metrics
      list(
        possible = c(a = "A", b = "B"),
        requested = c(a = "A", b = "B"),
        computed = c(a = "A", b = "B"),
        assessed = c(b = "B")
      )
    }
  )
  rows <- data.frame(
    "Requested variable-group metric" = c("Metric A", "Metric B"),
    "Computed variable-group results" = c(1L, 1L),
    check.names = FALSE
  )

  summary <- util_report_scope_variable_group_summary(
    rows,
    list("Metric A" = "Group", "Metric B" = "Group"),
    list("Metric A" = "Group", "Metric B" = "Group"),
    list("Metric B" = "Group"),
    concept_scope = list(),
    report = structure(list(), class = "dataquieR_resultset2")
  )

  expect_identical(seen$assessed, "Metric B")
  expect_identical(seen$computed, c("Metric A", "Metric B"))
  expect_identical(seen$requested, c("Metric A", "Metric B"))
  expect_identical(
    summary$concept_coverage,
    list(assessed = 1L, possible = 2L, computed = 2L, requested = 2L)
  )
})

test_that("variable-group summary retains computed and classified coverage", {
  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    util_report_scope_variable_group_concepts = function(
      assessed_metrics,
      concept_scope,
      report,
      requested_metrics,
      computed_metrics
    ) {
      seen$assessed <- assessed_metrics
      seen$computed <- computed_metrics
      seen$requested <- requested_metrics
      list(
        possible = c(a = "Concept A", b = "Concept B"),
        requested = c(a = "Concept A", b = "Concept B"),
        computed = c(a = "Concept A", b = "Concept B"),
        assessed = c(b = "Concept B")
      )
    }
  )
  rows <- data.frame(
    "Requested variable-group metric" = c("Metric A", "Metric B"),
    "Computed variable-group results" = c(1L, 1L),
    check.names = FALSE
  )
  summary <- util_report_scope_variable_group_summary(
    rows,
    requested_groups = list(
      "Metric A" = c("Group 1", "Group 2"),
      "Metric B" = "Group 1"
    ),
    computed_groups = list(
      "Metric A" = "Group 1",
      "Metric B" = "Group 1"
    ),
    classified_groups = list("Metric B" = "Group 1"),
    concept_scope = list(),
    report = structure(list(), class = "dataquieR_resultset2")
  )

  expect_identical(seen$assessed, "Metric B")
  expect_identical(seen$computed, c("Metric A", "Metric B"))
  expect_identical(seen$requested, c("Metric A", "Metric B"))
  expect_identical(
    summary$coverage,
    list(classifications = 1L, possible = 3L, computed = 2L)
  )
  expect_identical(
    summary$concept_coverage,
    list(assessed = 1L, possible = 2L, computed = 2L, requested = 2L)
  )
  expect_identical(
    summary$classification_items$matrix$computed,
    c(TRUE, FALSE, TRUE)
  )
  expect_identical(
    summary$classification_items$matrix$classified,
    c(FALSE, FALSE, TRUE)
  )
})

test_that("scope-tree maps computed result names to requested groups", {
  info_scale_dq <- data.frame(
    "Requested variable-group metric" = "Missing responses",
    "Computed variable-group results" = 1L,
    check.names = FALSE
  )
  attr(info_scale_dq, "computed_variable_group_rows") <- data.frame(
    SSI = "MISS_RESP",
    CHECK_ID = "8",
    VAR_NAMES = "MISS_RESP_internal_8",
    stringsAsFactors = FALSE
  )
  report <- structure(list(), meta_data_cross_item = data.frame(
    CHECK_ID = c("1", "8"),
    CHECK_LABEL = c("Group A", "Group B"),
    stringsAsFactors = FALSE
  ))

  computed_groups <- util_report_scope_variable_group_computed_groups(
    info_scale_dq,
    list("Missing responses" = c("Group A", "Group B")),
    report = report
  )

  expect_identical(computed_groups, list("Missing responses" = "Group B"))
})

test_that("scope-tree resolves CHECK_ID values to variable-group labels", {
  report <- structure(list(), meta_data_cross_item = data.frame(
    CHECK_ID = c("8", "1", "2"),
    CHECK_LABEL = c("Scale H", "Scale A", "Scale B"),
    stringsAsFactors = FALSE
  ))

  expect_identical(
    util_report_scope_variable_group_result_labels(report, c("8", "1", "2")),
    c("Scale H", "Scale A", "Scale B")
  )
})

test_that("scope-tree group matrices retain unclassified requested groups", {
  rows <- data.frame(
    "Requested variable-group metric" = "Missing responses",
    "Computed variable-group results" = 0L,
    check.names = FALSE
  )
  items <- util_report_scope_variable_group_classification_items(
    rows,
    list("Missing responses" = c("Group A", "Group B")),
    list()
  )

  expect_length(items$possible, 2L)
  expect_length(items$classified, 0L)
  expect_true(all(!items$matrix$classified))
})

test_that("scope-tree item classification labels use indicator names", {
  items <- util_report_scope_item_classification_items(data.frame(
    function_name = "int_datatype_matrix",
    variable = "SBP_0",
    classifications = 0L,
    stringsAsFactors = FALSE
  ))

  expect_equal(unname(items[["possible"]]), "Data type mismatch of SBP_0")
})

test_that("scope-tree item labels use prepared long metadata labels", {
  report <- structure(list(),
    meta_data = data.frame(
      VAR_NAMES = "v1",
      LABEL = "Short item",
      DATA_TYPE = "integer",
      stringsAsFactors = FALSE
    ),
    label_col = LABEL
  )
  testthat::local_mocked_bindings(
    prep_get_labels = function(
      resp_vars,
      item_level,
      label_col,
      label_class,
      resp_vars_are_var_names_only
    ) {
      expect_identical(resp_vars, "v1")
      expect_identical(item_level, attr(report, "meta_data"))
      expect_identical(as.character(label_col), as.character(LABEL))
      expect_identical(label_class, "LONG")
      expect_true(resp_vars_are_var_names_only)
      "Prepared long item label"
    }
  )

  expect_identical(
    util_report_scope_item_display_labels("v1", report),
    "v1: Prepared long item label"
  )
})

test_that("variable-group concept coverage uses its reachable DQ_OBS subtree", {
  concepts <- util_report_scope_variable_group_concepts(
    "Missing responses",
    list()
  )

  expect_equal(length(concepts[["assessed"]]), 1L)
  expect_gt(length(concepts[["possible"]]), length(concepts[["assessed"]]))
})

test_that("represented concepts never have an empty denominator", {
  metric <- "Inter-Class reliability"
  mapping <- util_report_scope_variable_group_dqi_metrics(metric)
  mapping <- mapping[!is.na(mapping[["IndicatorID"]]), , drop = FALSE]
  if (!nrow(mapping)) {
    skip("Repeated-measurement metric has no DQ_OBS mapping")
  }
  report <- structure(list(), class = "dataquieR_resultset2")

  concepts <- util_report_scope_variable_group_concepts(
    metric,
    data.frame(
      IndicatorID = mapping[["IndicatorID"]],
      stringsAsFactors = FALSE
    ),
    report = report
  )

  expect_gt(length(concepts[["possible"]]), 0L)
  expect_equal(length(concepts[["assessed"]]), 1L)
})

test_that("variable-group concepts distinguish computation and grading", {
  metrics <- c("Inter-Class reliability", "Disagreement with gold standard")
  mapping <- util_report_scope_variable_group_dqi_metrics(metrics)
  mapping <- mapping[!is.na(mapping[["IndicatorID"]]), , drop = FALSE]
  if (nrow(mapping) != 2L) {
    skip("Repeated-measurement metrics have no complete DQ_OBS mapping")
  }
  report <- structure(list(), class = "dataquieR_resultset2")

  concepts <- util_report_scope_variable_group_concepts(
    assessed_metrics = metrics[[2]],
    concept_scope = data.frame(
      IndicatorID = mapping[["IndicatorID"]],
      stringsAsFactors = FALSE
    ),
    report = report,
    requested_metrics = metrics,
    computed_metrics = metrics
  )

  expect_equal(length(concepts[["possible"]]), 2L)
  expect_equal(length(concepts[["requested"]]), 2L)
  expect_equal(length(concepts[["computed"]]), 2L)
  expect_equal(length(concepts[["assessed"]]), 1L)
  expect_identical(unname(concepts[["assessed"]]), "Disagr. gold st.")
})

test_that("variable-group concept requests do not change coverage sets", {
  metrics <- c(
    "Missing responses",
    "Maximum Long String",
    "Multivariate outliers"
  )
  mapping <- util_report_scope_variable_group_dqi_metrics(metrics)
  mapping <- mapping[!is.na(mapping[["IndicatorID"]]), , drop = FALSE]
  if (nrow(mapping) != 3L) {
    skip("Variable-group metrics have no complete DQ_OBS mapping")
  }
  report <- structure(list(), class = "dataquieR_resultset2")

  concepts <- util_report_scope_variable_group_concepts(
    assessed_metrics = metrics[[1]],
    concept_scope = data.frame(
      IndicatorID = mapping[["IndicatorID"]],
      stringsAsFactors = FALSE
    ),
    report = report,
    requested_metrics = metrics[1:2],
    computed_metrics = metrics[[1]]
  )
  coverage <- util_report_scope_tree_concept_coverage(
    length(concepts[["assessed"]]),
    length(concepts[["possible"]]),
    length(concepts[["computed"]]),
    length(concepts[["requested"]])
  )
  concept_labels <- c(
    "Invalid or missing responses",
    "Maximum Long String",
    "Multivariate outliers"
  )

  expect_setequal(unname(concepts[["possible"]]), concept_labels)
  expect_setequal(unname(concepts[["requested"]]), concept_labels[1:2])
  expect_identical(unname(concepts[["computed"]]), concept_labels[[1]])
  expect_identical(unname(concepts[["assessed"]]), concept_labels[[1]])
  expect_identical(
    coverage,
    list(assessed = 1L, possible = 3L, computed = 1L, requested = 2L)
  )
})

test_that("scope-tree target entities cover all report implementations", {
  implementations <- util_get_concept_info("implementations")
  expect_true("target_entity" %in% names(implementations))
  expect_false(anyNA(implementations[["target_entity"]]))

  provisional <- utils::read.csv(system.file(
    "function_target_entities.csv",
    package = "dataquieR"
  ), stringsAsFactors = FALSE)
  provisional <- provisional[match(
    implementations[["function_R"]], provisional[["function_name"]]
  ), , drop = FALSE]
  expect_identical(
    implementations[["target_entity"]],
    provisional[["target_entity"]]
  )

  target_functions <- unique(unlist(lapply(
    c("item", "variable_group", "observational_unit", "segment", "dataframe"),
    util_report_scope_target_functions
  )))

  expect_setequal(target_functions, implementations[["function_R"]])
})

test_that("scope-tree denominators use result applicability", {
  intrinsic <- structure(
    list(message = "Not available in reports"),
    class = c(
      dataquieR.intrinsic_applicability_problem,
      "error",
      "condition"
    )
  )
  unavailable <- structure(list(), error = list(intrinsic))
  available <- structure(list(value = 1), error = list())

  expect_false(util_report_scope_results_are_applicable(
    list(unavailable, unavailable)
  ))
  expect_true(util_report_scope_results_are_applicable(
    list(unavailable, available)
  ))

  single_result <- structure(
    list(),
    class = "dataquieR_result",
    error = list(intrinsic)
  )
  single_report <- matrix(list(single_result), nrow = 1L)
  colnames(single_report) <- "acc_mahalanobis"
  expect_false(util_report_scope_function_is_applicable(
    "acc_mahalanobis",
    single_report
  ))
})

test_that("scope-tree item denominators distinguish applicability errors", {
  intrinsic <- structure(
    list(message = "Not applicable by definition"),
    class = c(
      dataquieR.intrinsic_applicability_problem,
      dataquieR.applicability_problem,
      "error",
      "condition"
    )
  )
  external <- structure(
    list(message = "Required metadata are missing"),
    class = c(dataquieR.applicability_problem, "error", "condition")
  )
  intrinsic_result <- structure(list(), error = list(intrinsic))
  external_result <- structure(list(), error = list(external))
  available_result <- structure(list(), error = list())
  report <- array(
    list(intrinsic_result, external_result, available_result),
    dim = c(3L, 1L, 1L),
    dimnames = list(
      c("Item one", "Item two", "Item three"),
      "com_item_missingness",
      "SummaryTable"
    )
  )
  attr(report, "matrix_list") <- structure(list(),
    function2category = c(com_item_missingness = "Completeness")
  )
  attr(report, "label_col") <- LABEL
  attr(report, "meta_data") <- data.frame(
    VAR_NAMES = c("v1", "v2", "v3"),
    LABEL = c("Item one", "Item two", "Item three"),
    LONG_LABEL = c("Item one long", "Item two long", "Item three long"),
    COMPUTED_VARIABLE_ROLE = "",
    stringsAsFactors = FALSE
  )
  this <- new.env(parent = emptyenv())
  this$result <- data.frame(
    VAR_NAMES = c("v1", "v2", "v3"),
    function_name = "com_item_missingness",
    indicator_metric = "PCT_com_crm_mv",
    class = c(NA_character_, NA_character_, "Ok"),
    stringsAsFactors = FALSE
  )
  repsum <- structure(list(), this = this)

  coverage <- util_report_scope_item_coverage(report, repsum)
  items <- util_report_scope_item_classification_items(coverage, report)
  tooltip <- util_report_scope_tree_coverage_tooltip(
    list(classifications = 1L, possible = 2L),
    items
  )

  expect_identical(coverage$variable, c("v1", "v2", "v3"))
  expect_identical(coverage$classifications, c(0L, 0L, 1L))
  expect_identical(coverage$possible_classifications, c(0L, 1L, 1L))
  expect_identical(coverage$applicable, c(FALSE, TRUE, TRUE))
  expect_match(tooltip, "v1: Item one long", fixed = TRUE)
  expect_match(tooltip, "N/A = not applicable by definition", fixed = TRUE)
  expect_match(tooltip, "required metadata are missing", fixed = TRUE)
  expect_match(tooltip, ">N/A<", fixed = TRUE)
  expect_false(grepl("intrinsic", tooltip, ignore.case = TRUE))
  expect_false(util_report_scope_result_is_applicable(
    "com_item_missingness", "v1", report
  ))
  expect_true(util_report_scope_result_is_applicable(
    "com_item_missingness", "v2", report
  ))
})

test_that("scope-tree uses the actual suffixed call summarized by function", {
  intrinsic <- structure(
    list(message = "Missing entries not allowed in argument group_vars"),
    class = c(
      dataquieR.intrinsic_applicability_problem,
      dataquieR.applicability_problem,
      "error",
      "condition"
    )
  )
  unavailable <- structure(list(), error = list(intrinsic))
  available <- structure(list(value = 1), error = list())
  report <- array(
    list(unavailable, available),
    dim = c(1L, 2L, 1L),
    dimnames = list(
      "Item one",
      c("acc_varcomp", "acc_varcomp_center"),
      "SummaryTable"
    )
  )
  attr(report, "label_col") <- LABEL
  attr(report, "meta_data") <- data.frame(
    VAR_NAMES = "v1",
    LABEL = "Item one",
    COMPUTED_VARIABLE_ROLE = "",
    stringsAsFactors = FALSE
  )
  this <- new.env(parent = emptyenv())
  this$result <- data.frame(
    VAR_NAMES = c("v1", "v1"),
    function_name = c("acc_varcomp", "acc_varcomp"),
    call_names = c("acc_varcomp", "acc_varcomp_center"),
    indicator_metric = c("ICC_acc_ud_loc", "ICC_acc_ud_loc"),
    values_raw = c(NA, 0.2),
    class = c(NA_character_, "Ok"),
    stringsAsFactors = FALSE
  )
  repsum <- structure(list(), this = this)

  coverage <- util_report_scope_item_coverage(report, repsum)
  node <- util_report_scope_item_tree_nodes(
    data.frame(
      Dimension = "Accuracy",
      "No. DQ indicators" = 1,
      check.names = FALSE
    ),
    report,
    repsum,
    item_coverage = coverage
  )

  expect_identical(nrow(coverage), 1L)
  expect_identical(coverage$function_name, "acc_varcomp")
  expect_identical(coverage$indicator_id, "DQ_3_2_1_3")
  expect_identical(coverage$computations, 1L)
  expect_identical(coverage$classifications, 1L)
  expect_identical(coverage$possible_classifications, 1L)
  expect_true(coverage$applicable)
  expect_identical(node$measures, 1L)
  expect_identical(node$concept_coverage$requested, 1L)
  expect_identical(node$concept_coverage$computed, 1L)
  expect_identical(node$concept_coverage$assessed, 1L)
})

test_that("scope-tree concept denominators respect the result entity", {
  dqi <- util_get_concept_info("dqi")
  contradiction_ids <- na.omit(unique(dqi[["IndicatorID"]][
    dqi[["function_R"]] == "con_contradictions"
  ]))
  inadmissible_ids <- na.omit(unique(dqi[["IndicatorID"]][
    dqi[["function_R"]] == "con_inadmissible_categorical"
  ]))
  limit_ids <- na.omit(unique(dqi[["IndicatorID"]][
    dqi[["function_R"]] == "con_limit_deviations"
  ]))
  item_ids <- util_report_scope_target_indicator_ids("item")
  variable_group_ids <- util_report_scope_target_indicator_ids(
    "variable_group"
  )

  expect_false(any(contradiction_ids %in% item_ids))
  expect_false(any(inadmissible_ids %in% variable_group_ids))
  expect_true(all(limit_ids %in% item_ids))
  expect_false(any(limit_ids %in% variable_group_ids))
})

test_that("scope-tree concept applicability respects item data types", {
  report <- structure(list(),
    class = "dataquieR_resultset2",
    meta_data = data.frame(
      VAR_NAMES = c("numeric_item", "date_item"),
      DATA_TYPE = c(DATA_TYPES$FLOAT, DATA_TYPES$DATETIME),
      stringsAsFactors = FALSE
    )
  )

  expect_identical(
    util_report_scope_metric_data_type_is_applicable(
      c("con_rvv_inum", "con_rvv_itdat", "com_crm_mv"),
      c("numeric_item", "numeric_item", "numeric_item"),
      report
    ),
    c(TRUE, FALSE, TRUE)
  )
  expect_identical(
    util_report_scope_metric_data_type_is_applicable(
      c("con_rvv_inum", "con_rvv_itdat"),
      c("date_item", "date_item"),
      report
    ),
    c(FALSE, TRUE)
  )

  numeric_report <- report
  attr(numeric_report, "meta_data") <- attr(report, "meta_data")[1, ,
    drop = FALSE
  ]
  dqi <- util_get_concept_info("dqi")
  possible <- util_report_scope_indicator_data_type_is_possible(
    dqi[["abbreviation"]],
    numeric_report
  )
  expect_true(possible[dqi[["abbreviation"]] == "con_rvv_inum"])
  expect_false(possible[dqi[["abbreviation"]] == "con_rvv_itdat"])

  derived_numeric_report <- structure(list(),
    class = "dataquieR_resultset2",
    meta_data = data.frame(
      VAR_NAMES = c("text_item", "derived_numeric"),
      DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$FLOAT),
      COMPUTED_VARIABLE_ROLE = c("", "DERIVED"),
      stringsAsFactors = FALSE
    )
  )
  possible <- util_report_scope_indicator_data_type_is_possible(
    dqi[["abbreviation"]],
    derived_numeric_report
  )
  expect_false(possible[dqi[["abbreviation"]] == "con_rvv_inum"])
  expect_false(possible[dqi[["abbreviation"]] == "con_rvv_unum"])
})

test_that("scope-tree maps the joint shape-or-scale result to both concepts", {
  report <- structure(list(),
    class = "dataquieR_resultset2",
    matrix_list = structure(list(),
      function2category = c(acc_shape_or_scale = "Accuracy")
    ),
    meta_data = data.frame(
      VAR_NAMES = "numeric_item",
      DATA_TYPE = DATA_TYPES$FLOAT,
      COMPUTED_VARIABLE_ROLE = "",
      stringsAsFactors = FALSE
    )
  )
  this <- new.env(parent = emptyenv())
  this$result <- data.frame(
    VAR_NAMES = "numeric_item",
    function_name = "acc_shape_or_scale",
    indicator_metric = "FLG_acc_ud_shape",
    values_raw = 0,
    class = "Ok",
    stringsAsFactors = FALSE
  )
  repsum <- structure(list(), this = this)

  coverage <- util_report_scope_item_coverage(report, repsum)
  concepts <- util_report_scope_coverage_concept_ids(coverage)
  node <- util_report_scope_item_tree_nodes(
    data.frame(
      Dimension = "Accuracy",
      "No. DQ indicators" = 1,
      check.names = FALSE
    ),
    report,
    repsum,
    item_coverage = coverage
  )
  tooltip <- util_report_scope_tree_concept_coverage_tooltip(
    node$concept_coverage,
    node$concept_items
  )
  possible_ids <- util_report_scope_target_indicator_ids("item", report)
  dqi <- util_get_concept_info("dqi")
  possible_accuracy_ids <- unique(dqi$IndicatorID[
    dqi$Level == 3L &
      dqi$Dimension == "Accuracy" &
      dqi$IndicatorID %in% possible_ids
  ])
  accuracy_node <- node$children[[which(vapply(
    node$children,
    function(child) identical(child$label, "Accuracy"),
    logical(1)
  ))]]

  expect_identical(nrow(coverage), 1L)
  expect_identical(coverage$indicator_id, "DQ_3_2_1_4")
  expect_identical(coverage$computations, 1L)
  expect_identical(coverage$classifications, 1L)
  expect_setequal(
    concepts[[1]],
    c("DQ_3_2_1_4", "DQ_3_2_1_5")
  )
  expect_identical(node$concept_coverage$computed, 2L)
  expect_identical(node$concept_coverage$assessed, 2L)
  expect_identical(
    node$concept_coverage$possible,
    as.integer(length(possible_ids))
  )
  expect_identical(
    accuracy_node$concept_coverage$possible,
    as.integer(length(possible_accuracy_ids))
  )
  expect_match(coverage$indicator_label, "joint", ignore.case = TRUE)
  expect_match(tooltip, "Joint assessments", fixed = TRUE)
  expect_match(
    tooltip,
    "separate metric values were not computed",
    fixed = TRUE
  )
})

test_that("scope-tree prunes unavailable variable-group results", {
  info_scale_dq <- data.frame(
    "Requested variable-group metric" = c("Missing responses",
      "Mahalanobis distance"),
    "Computed variable-group results" = c(2, 0),
    check.names = FALSE
  )
  report <- structure(list(),
    class = "dataquieR_resultset2",
    meta_data_cross_item = data.frame(
      CHECK_LABEL = c("g1", "g2", "g3"),
      MISS_RESP = c("[;1)", "[;1)", ""),
      MAHALANOBIS_RATIO = c("", "", "[0;1]"),
      stringsAsFactors = FALSE
    )
  )

  node <- util_report_scope_variable_group_tree_nodes(info_scale_dq, report)
  pruned <- util_report_scope_tree_prune_unavailable(node)
  html <- paste(as.character(util_render_report_scope_tree_table(pruned)),
    collapse = ""
  )

  expect_match(html, "Missing responses", fixed = TRUE)
  expect_match(html, "Requested concepts not computed", fixed = TRUE)
  expect_match(html, "Reference concepts not requested", fixed = TRUE)
  expect_false(grepl("N/A", html, fixed = TRUE))
})

test_that("util_report_scope_variable_group_counts counts requested groups", {
  report <- structure(list(),
    class = "dataquieR_resultset2",
    meta_data_cross_item = data.frame(
      CHECK_LABEL = c("g1", "g2", "g3"),
      MISS_RESP = c("[;1)", "", "[;2)"),
      MAXIMUM_LONG_STRING = c("", "[;3)", ""),
      stringsAsFactors = FALSE
    )
  )

  counts <- util_report_scope_variable_group_counts(report)

  expect_equal(counts[["Missing responses"]], c("g1", "g3"))
  expect_equal(counts[["Maximum long string"]], "g2")
})

test_that("scope-tree maps each contradiction rule to one analysis", {
  report <- structure(list(),
    class = "dataquieR_resultset2",
    meta_data_cross_item = data.frame(
      CHECK_ID = c("1", "2", "3"),
      CHECK_LABEL = c("Logical group", "Empirical group", "Future group"),
      CONTRADICTION_TYPE = c("LOGICAL", "EMPIRICAL", "QUALITY"),
      stringsAsFactors = FALSE
    )
  )

  groups <- util_report_scope_variable_group_categorical_groups(report)

  expect_identical(groups[["Logical contradictions"]], "Logical group")
  expect_identical(groups[["Empirical contradictions"]], "Empirical group")
  expect_identical(groups[["Quality contradictions"]], "Future group")
})

test_that("contradiction scope excludes structurally inapplicable pairs", {
  report <- structure(list(),
    class = "dataquieR_resultset2",
    meta_data_cross_item = data.frame(
      CHECK_ID = c("1", "2"),
      CHECK_LABEL = c("Logical group", "Empirical group"),
      CONTRADICTION_TYPE = c("LOGICAL", "EMPIRICAL"),
      stringsAsFactors = FALSE
    )
  )
  this <- new.env(parent = emptyenv())
  this$meta_data <- data.frame(VAR_NAMES = "item_1")
  this$summary_meta_data <- this$meta_data
  this$label_col <- VAR_NAMES
  this$rownames_of_report <- "item_1"
  this$variable_group_call_names <- "con_contradictions_redcap"
  this$result <- data.frame(marker = 1L)
  repsum <- structure(list(), this = this)
  testthat::local_mocked_bindings(
    util_filter_repsum = function(...) {
      data.frame(
        VAR_NAMES = rep(c("1", "2"), 2),
        indicator_metric = c(
          "NUM_con_con_contc", "NUM_con_con_contc",
          "PCT_con_con_contu", "PCT_con_con_contu"
        ),
        values_raw = c(0, NA, NA, 16.7),
        class = c("Ok", NA, NA, "Important"),
        stringsAsFactors = FALSE
      )
    }
  )
  info <- data.frame(
    "Requested variable-group metric" = character(),
    "Computed variable-group results" = integer(),
    check.names = FALSE
  )

  node <- util_report_scope_variable_group_tree_nodes(info, report, repsum)
  descendants <- function(x) {
    c(list(x), unlist(lapply(x$children, descendants), recursive = FALSE))
  }
  nodes <- descendants(node)
  contradiction <- nodes[[which(vapply(
    nodes,
    function(x) identical(x$label, "Contradictions"),
    logical(1)
  ))]]
  logical <- nodes[[which(vapply(
    nodes,
    function(x) identical(x$label, "Logic. contrad."),
    logical(1)
  ))]]
  empirical <- nodes[[which(vapply(
    nodes,
    function(x) identical(x$label, "Empir. contrad."),
    logical(1)
  ))]]

  expect_identical(contradiction$assessed, 2L)
  expect_identical(contradiction$coverage, list(
    classifications = 2L,
    possible = 2L,
    computed = 2L
  ))
  expect_identical(logical$assessed, 1L)
  expect_identical(logical$coverage$possible, 1L)
  expect_identical(empirical$assessed, 1L)
  expect_identical(empirical$coverage$possible, 1L)
})

test_that("contradiction scope does not expect unconfigured sibling types", {
  report <- structure(list(),
    class = "dataquieR_resultset2",
    meta_data_cross_item = data.frame(
      CHECK_ID = "1",
      CHECK_LABEL = "Logical group",
      CONTRADICTION_TYPE = "LOGICAL",
      stringsAsFactors = FALSE
    )
  )
  this <- new.env(parent = emptyenv())
  this$meta_data <- data.frame(VAR_NAMES = "item_1")
  this$summary_meta_data <- this$meta_data
  this$label_col <- VAR_NAMES
  this$rownames_of_report <- "item_1"
  this$variable_group_call_names <- "con_contradictions_redcap"
  this$result <- data.frame(marker = 1L)
  repsum <- structure(list(), this = this)
  testthat::local_mocked_bindings(
    util_filter_repsum = function(...) {
      data.frame(
        VAR_NAMES = c("1", "1"),
        indicator_metric = c(
          "NUM_con_con_contc",
          "PCT_con_con_contu"
        ),
        values_raw = c(0, NA),
        class = c("Ok", NA),
        stringsAsFactors = FALSE
      )
    }
  )
  info <- data.frame(
    "Requested variable-group metric" = character(),
    "Computed variable-group results" = integer(),
    check.names = FALSE
  )

  node <- util_report_scope_variable_group_tree_nodes(info, report, repsum)
  descendants <- function(x) {
    c(list(x), unlist(lapply(x$children, descendants), recursive = FALSE))
  }
  labels <- vapply(descendants(node), `[[`, character(1), "label")

  expect_true("Logic. contrad." %in% labels)
  expect_false("Empir. contrad." %in% labels)
})

test_that("direct group analyses retain expected results without a grade", {
  report <- structure(list(),
    class = "dataquieR_resultset2",
    meta_data_cross_item = data.frame(
      CHECK_LABEL = "Repeated group",
      stringsAsFactors = FALSE
    )
  )
  this <- new.env(parent = emptyenv())
  this$meta_data <- data.frame(VAR_NAMES = "item_1")
  this$summary_meta_data <- this$meta_data
  this$label_col <- VAR_NAMES
  this$rownames_of_report <- "item_1"
  this$variable_group_call_names <- "acc_repeated_measurements"
  this$result <- data.frame(marker = 1L)
  repsum <- structure(list(), this = this)
  testthat::local_mocked_bindings(
    util_filter_repsum = function(...) {
      data.frame(
        VAR_NAMES = rep("Repeated group", 2),
        indicator_metric = c(
          "NUM_acc_drm_inter", "NUM_acc_drm_gold"
        ),
        values_raw = c(NA, 1),
        class = c(NA, "Ok"),
        stringsAsFactors = FALSE
      )
    }
  )
  info <- data.frame(
    "Requested variable-group metric" = character(),
    "Computed variable-group results" = integer(),
    check.names = FALSE
  )

  node <- util_report_scope_variable_group_tree_nodes(info, report, repsum)
  descendants <- function(x) {
    c(list(x), unlist(lapply(x$children, descendants), recursive = FALSE))
  }
  nodes <- descendants(node)
  inter_class <- nodes[[which(vapply(
    nodes,
    function(x) identical(x$label, "Inter-class rel."),
    logical(1)
  ))]]
  gold_standard <- nodes[[which(vapply(
    nodes,
    function(x) identical(x$label, "Disagr. gold st."),
    logical(1)
  ))]]

  expect_identical(inter_class$assessed, 1L)
  expect_identical(inter_class$coverage, list(
    classifications = 0L,
    possible = 1L,
    computed = 0L
  ))
  expect_identical(gold_standard$assessed, 1L)
  expect_identical(gold_standard$coverage, list(
    classifications = 1L,
    possible = 1L,
    computed = 1L
  ))
})

test_that("util_render_report_scope_tree renders the native tree grid", {
  info_dim_dq <- data.frame(
    Dimension = "Integrity",
    "No. DQ indicators" = 1,
    check.names = FALSE
  )
  info_scale_dq <- data.frame(
    "Requested variable-group metric" = "Missing responses",
    "Computed variable-group results" = 1,
    check.names = FALSE
  )
  report <- structure(list(),
    class = "dataquieR_resultset2",
    matrix_list = structure(list(),
      function2category = c(int_datatype_matrix = "Integrity")
    ),
    meta_data = data.frame(
      VAR_NAMES = "item_1",
      COMPUTED_VARIABLE_ROLE = "",
      stringsAsFactors = FALSE
    )
  )
  this <- new.env(parent = emptyenv())
  this$result <- data.frame(
    VAR_NAMES = "item_1",
    function_name = "int_datatype_matrix",
    indicator_metric = "int_vfe_type",
    class = "Ok",
    stringsAsFactors = FALSE
  )
  repsum <- structure(list(), this = this)

  tree <- util_render_report_scope_tree(info_dim_dq, info_scale_dq,
    report = report, repsum = repsum
  )

  expect_s3_class(tree, "shiny.tag")
  expect_match(
    paste(as.character(tree), collapse = ""),
    "dq-report-scope-tree-table",
    fixed = TRUE
  )
})

test_that("scope-tree item coverage rejects incomplete reports", {
  empty_coverage <- util_report_scope_item_coverage()

  expect_identical(nrow(empty_coverage), 0L)

  report <- structure(list(), class = "dataquieR_resultset2")
  expect_identical(nrow(util_report_scope_item_coverage(
    report,
    structure(list())
  )), 0L)
  this <- new.env(parent = emptyenv())
  this$result <- data.frame(
    VAR_NAMES = "v1",
    function_name = "int_datatype_matrix",
    class = "Ok",
    stringsAsFactors = FALSE
  )
  report <- structure(list(),
    class = "dataquieR_resultset2",
    matrix_list = structure(list(),
      function2category = c(int_datatype_matrix = "Integrity")
    ),
    meta_data = data.frame(VAR_NAMES = "v1", stringsAsFactors = FALSE)
  )
  repsum <- structure(list(), this = this)
  this$stopped_functions <- c(int_datatype_matrix.v1 = TRUE)

  expect_identical(nrow(util_report_scope_item_coverage(report, repsum)), 0L)

  this$result$indicator_metric <- "int_vfe_type"

  this$stopped_functions <- c(
    int_datatype_matrix.v1 = TRUE,
    int_datatype_matrix.v2 = FALSE
  )
  expect_identical(
    util_report_scope_item_coverage(report, repsum)$classifications,
    1L
  )
})

test_that("scope-tree concept helpers handle empty scopes", {
  expect_identical(
    util_report_scope_variable_group_concepts("Missing responses", FALSE),
    list(
      possible = character(),
      requested = character(),
      assessed = character(),
      computed = character()
    )
  )

  out_of_scope <- util_report_scope_variable_group_concepts(
    "Missing responses",
    data.frame(Dimension = "not a dimension", stringsAsFactors = FALSE)
  )
  expect_identical(
    out_of_scope,
    list(
      possible = character(),
      requested = character(),
      assessed = character(),
      computed = character()
    )
  )

})

test_that("scope-tree target functions validate DQ_OBS target entities", {
  expect_error(util_report_scope_target_functions(character()))
  expect_error(util_report_scope_target_functions("unknown"))

  testthat::local_mocked_bindings(
    util_get_concept_info = function(sheet) {
      expect_identical(sheet, "implementations")
      data.frame(function_R = "example", stringsAsFactors = FALSE)
    }
  )
  expect_error(util_report_scope_target_functions("item"))

  testthat::local_mocked_bindings(
    util_get_concept_info = function(sheet) {
      expect_identical(sheet, "implementations")
      data.frame(
        function_R = "example",
        target_entity = '"not_supported"',
        stringsAsFactors = FALSE
      )
    }
  )
  expect_error(util_report_scope_target_functions("item"))
})

test_that("scope-tree merge helpers preserve unique named entries", {
  expect_identical(
    util_report_scope_tree_merge_concept_items(list(NULL)),
    list(
      possible = character(),
      requested = character(),
      assessed = character(),
      computed = character(),
      joint_assessments = character()
    )
  )
  merged_concepts <- util_report_scope_tree_merge_concept_items(list(
    list(
      possible = c(a = "A", b = "B"),
      requested = c(a = "A"),
      computed = c(a = "A"),
      assessed = c(a = "A")
    ),
    list(
      possible = c(a = "duplicate", c = "C"),
      requested = c(c = "C"),
      computed = c(c = "C"),
      assessed = c(c = "C")
    )
  ))
  expect_identical(merged_concepts[["possible"]], c(a = "A", b = "B", c = "C"))
  expect_identical(merged_concepts[["requested"]], c(a = "A", c = "C"))
  expect_identical(merged_concepts[["computed"]], c(a = "A", c = "C"))
  expect_identical(merged_concepts[["assessed"]], c(a = "A", c = "C"))

  legacy_concepts <- util_report_scope_tree_merge_concept_items(list(list(
    possible = c(a = "A", b = "B"),
    computed = c(a = "A"),
    assessed = c(a = "A")
  )))
  expect_identical(legacy_concepts[["requested"]], c(a = "A", b = "B"))

  sparse_concepts <- util_report_scope_tree_merge_concept_items(list(list(
    possible = c(a = "A"),
    assessed = c(a = "A")
  )))
  expect_identical(sparse_concepts[["computed"]], c(a = "A"))

  empty_classifications <- util_report_scope_tree_merge_classification_items(
    list(NULL)
  )
  expect_identical(empty_classifications[["row_label"]], "Unit")
  expect_identical(nrow(empty_classifications[["matrix"]]), 0L)

  merged_classifications <-
    util_report_scope_tree_merge_classification_items(list(
      list(
        possible = c(a = "A"),
        classified = c(a = "A"),
        unresolved = character(),
        row_label = "Item",
        matrix = data.frame(
          unit = "A",
          analysis = "check",
          classified = TRUE,
          stringsAsFactors = FALSE
        )
      ),
      list(
        possible = c(a = "duplicate", b = "B"),
        classified = character(),
        unresolved = c(b = "B"),
        matrix = data.frame()
      )
    ))
  expect_identical(merged_classifications[["possible"]], c(a = "A", b = "B"))
  expect_identical(merged_classifications[["computed"]], c(a = "A"))
  expect_identical(merged_classifications[["row_label"]], "Item")
  expect_identical(nrow(merged_classifications[["matrix"]]), 1L)
})

test_that("scope-tree coverage styles handle unavailable and bounded values", {
  expect_null(util_report_scope_tree_coverage_style(NULL))

  unavailable <- util_report_scope_tree_coverage_style(list(
    classifications = 0L,
    possible = NA_integer_
  ))
  expect_identical(unavailable[["background"]], "#f0f0f0")
  expect_identical(unavailable[["title"]], "No expected results")

  below_zero <- util_report_scope_tree_coverage_style(list(
    classifications = -1L,
    possible = 2L
  ))
  above_one <- util_report_scope_tree_coverage_style(list(
    classifications = 3L,
    possible = 2L
  ))
  expect_identical(below_zero[["color"]], "#1f2933")
  expect_identical(above_one[["color"]], "#ffffff")
})

test_that("scope-tree helpers retain stable empty results", {
  expect_identical(
    util_report_scope_item_tree_nodes(NULL),
    list(text = "Item-level data quality assessment", children = list())
  )
  expect_identical(
    util_report_scope_variable_group_tree_nodes(NULL),
    list(text = "Variable-group assessment scope", children = list())
  )

  this <- new.env(parent = emptyenv())
  this$result <- NULL
  repsum <- structure(list(), this = this)
  report <- structure(list(), class = "dataquieR_resultset2")
  expect_identical(
    nrow(util_report_scope_variable_group_result_metrics(report, repsum)),
    0L
  )
})

test_that("scope-tree omits technical variable-group result columns", {
  report <- structure(list(), meta_data_cross_item = data.frame(
    CHECK_ID = "group_1",
    CHECK_LABEL = "Group 1",
    stringsAsFactors = FALSE
  ))
  this <- new.env(parent = emptyenv())
  this$meta_data <- data.frame(
    VAR_NAMES = "item_1",
    LABEL = "Item 1",
    stringsAsFactors = FALSE
  )
  this$summary_meta_data <- this$meta_data
  this$label_col <- LABEL
  this$rownames_of_report <- "Item 1"
  this$variable_group_call_names <- "example_group_function"
  this$result <- data.frame(
    VAR_NAMES = "group_1",
    indicator_metric = "CAT_example",
    values_raw = "category",
    class = "Ok",
    call_names = "example_group_function",
    stringsAsFactors = FALSE
  )
  repsum <- structure(list(), this = this)

  expect_identical(
    nrow(util_report_scope_variable_group_result_metrics(report, repsum)),
    0L
  )
})

test_that("scope-tree collapses anonymous hierarchy nodes", {
  info <- data.frame(
    "Requested variable-group metric" = character(),
    "Computed variable-group results" = integer(),
    check.names = FALSE
  )
  empty <- util_report_scope_variable_group_hierarchy_node(
    list(label = NULL, concept_scope = FALSE),
    info,
    list(),
    list(),
    list()
  )
  expect_identical(empty$label, "")
  expect_identical(empty$measures, 0L)

  child <- util_report_scope_variable_group_hierarchy_node(
    list(
      label = NULL,
      concept_scope = FALSE,
      children = list(list(
        label = "Child concept",
        concept_scope = FALSE,
        metrics = character()
      ))
    ),
    info,
    list(),
    list(),
    list()
  )
  expect_identical(child$label, "Child concept")
})

test_that("scope-tree maps only requested computed variable groups", {
  info <- data.frame(
    "Requested variable-group metric" = c(
      "Missing responses", "Maximum long string"
    ),
    "Computed variable-group results" = c(0L, 1L),
    check.names = FALSE
  )
  attr(info, "computed_variable_group_rows") <- data.frame(
    SSI = c("UNKNOWN", "MISS_RESP", "MAX_LONG_STRING", "MAX_LONG_STRING"),
    CHECK_ID = c(NA, NA, NA, "1"),
    VAR_NAMES = c("ignored", "ignored", NA, "Group A"),
    stringsAsFactors = FALSE
  )
  requested <- list(
    "Missing responses" = character(),
    "Maximum long string" = "Group A"
  )
  report <- structure(list(), meta_data_cross_item = data.frame(
    CHECK_ID = "1",
    CHECK_LABEL = "Group A",
    stringsAsFactors = FALSE
  ))
  testthat::local_mocked_bindings(
    util_get_concept_info = function(sheet) {
      expect_identical(sheet, "ssi")
      data.frame(
        menu_label = c("Missing responses", "Maximum long string"),
        SSI_METRICS = c("MISS_RESP", "MAX_LONG_STRING"),
        stringsAsFactors = FALSE
      )
    }
  )

  expect_identical(
    util_report_scope_variable_group_computed_groups(
      info,
      requested,
      report = report
    ),
    list("Maximum long string" = "Group A")
  )
})

test_that("scope-tree target mappings handle empty mappings", {
  testthat::local_mocked_bindings(
    util_report_scope_target_functions = function(target_entity) {
      expect_identical(target_entity, "variable_group")
      character()
    }
  )
  expect_identical(
    util_report_scope_target_indicator_ids("variable_group"),
    character()
  )
  expect_identical(
    util_report_scope_variable_group_concepts("Missing responses"),
    list(
      possible = character(),
      requested = character(),
      assessed = character(),
      computed = character()
    )
  )
})

test_that("scope-tree fallbacks remain explicit for incomplete evidence", {
  testthat::local_mocked_bindings(
    util_report_scope_tree_nodes = function(...) list(NULL, NULL)
  )
  expect_null(util_render_report_scope_tree(data.frame(), data.frame()))

  empty_items <- util_report_scope_item_classification_items(data.frame())
  expect_identical(empty_items$row_label, "Item")
  expect_identical(nrow(empty_items$matrix), 0L)

  labels <- util_report_scope_function_label(c(
    "com_item_missingness",
    "int_datatype_matrix",
    "acc_margins",
    "con_contradictions",
    "custom_function"
  ))
  expect_identical(labels, c(
    "Completeness: item missingness",
    "Integrity: datatype matrix",
    "Accuracy: margins",
    "Consistency: contradictions",
    "custom function"
  ))

  testthat::local_mocked_bindings(
    util_get_concept_info = function(sheet) {
      expect_identical(sheet, "dqi")
      data.frame(
        function_R = c("com_item_missingness", "com_item_missingness"),
        public_name = c("Missing values", "Response rate"),
        Name = c("Missing values", "Response rate"),
        stringsAsFactors = FALSE
      )
    }
  )
  expect_identical(
    unname(util_report_scope_item_indicator_labels("com_item_missingness")),
    "item missingness"
  )

  report <- array(
    list(structure(list(value = 1), class = "dataquieR_result")),
    dim = c(1L, 1L),
    dimnames = list("item_1", "example_function")
  )
  expect_true(util_report_scope_result_is_applicable(
    "example_function",
    "item_not_in_report",
    report
  ))
})

test_that("scope-tree ignores technical and unmapped group metrics", {
  this <- new.env(parent = emptyenv())
  this$meta_data <- data.frame(VAR_NAMES = "item_1")
  this$summary_meta_data <- this$meta_data
  this$rownames_of_report <- "item_1"
  this$label_col <- VAR_NAMES
  this$variable_group_call_names <- "group_call"
  this$result <- data.frame(marker = 1L)
  repsum <- structure(list(), this = this)
  report <- structure(list(), meta_data_cross_item = data.frame(
    CHECK_LABEL = "Group A",
    stringsAsFactors = FALSE
  ))

  testthat::local_mocked_bindings(
    util_filter_repsum = function(...) {
      data.frame(
        VAR_NAMES = "group_a",
        indicator_metric = "CAT_technical",
        values_raw = "technical",
        stringsAsFactors = FALSE
      )
    }
  )
  expect_identical(
    nrow(util_report_scope_variable_group_result_metrics(report, repsum)),
    0L
  )

  testthat::local_mocked_bindings(
    util_filter_repsum = function(...) {
      data.frame(
        VAR_NAMES = "group_a",
        indicator_metric = "unknown_metric",
        values_raw = 1,
        stringsAsFactors = FALSE
      )
    },
    util_get_concept_info = function(sheet) {
      expect_identical(sheet, "dqi")
      data.frame(
        Level = integer(),
        abbreviation = character(),
        Name = character(),
        IndicatorID = character(),
        stringsAsFactors = FALSE
      )
    }
  )
  expect_identical(
    nrow(util_report_scope_variable_group_result_metrics(report, repsum)),
    0L
  )
})
