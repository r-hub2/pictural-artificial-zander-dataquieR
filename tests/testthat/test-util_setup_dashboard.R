skip_on_cran()

test_that("util_setup_dashboard returns NULL without jsonlite", {
  skip_on_cran()

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) FALSE
  )

  expect_null(util_setup_dashboard(list()))
})

test_that("util_setup_dashboard returns early for empty dashboard tables", {
  skip_on_cran()

  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) TRUE,
    util_dashboard_table = function(repsum) {
      seen$repsum <- repsum
      data.frame()
    }
  )

  report <- structure(
    list(),
    class = "fake_dashboard_report",
    label_col = VAR_NAMES
  )

  expect_null(util_setup_dashboard(
    report,
    repsum = data.frame(marker = "from_repsum")
  ))
  expect_identical(seen$repsum$marker, "from_repsum")
})

test_that("util_setup_dashboard applies current grading rules", {
  skip_on_cran()

  seen <- new.env(parent = emptyenv())
  stale_summary <- structure(
    data.frame(marker = "stale"),
    class = c("dataquieR_summary", "data.frame")
  )
  current_summary <- structure(
    data.frame(marker = "current"),
    class = c("dataquieR_summary", "data.frame")
  )
  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) TRUE,
    util_reclassify_dataquieR_summary = function(repsum) {
      expect_identical(repsum, stale_summary)
      current_summary
    },
    util_dashboard_table = function(repsum) {
      seen$repsum <- repsum
      data.frame()
    }
  )

  report <- structure(
    list(),
    class = "fake_dashboard_report",
    label_col = VAR_NAMES
  )

  expect_null(util_setup_dashboard(report, repsum = stale_summary))
  expect_identical(seen$repsum, current_summary)
})

test_that("dashboard column tags use the displayed group label", {
  table <- data.frame(
    LABEL = "Scale D response group",
    VAR_NAMES = "scale_d",
    Call = "Limits",
    stringsAsFactors = FALSE
  )
  translated <- util_translate(names(table), ns = "dashboard_table")
  translated[names(table) == LABEL] <- "Variable group"
  translated[names(table) == VAR_NAMES] <- "Group ID"
  util_translated_colnames(table) <- translated

  expect_identical(
    util_dashboard_display_columns(table, c(LABEL, "Call")),
    c("Variable group", "Call")
  )
  expect_false(
    "Group ID" %in% util_dashboard_display_columns(table, c(LABEL, "Call"))
  )
})

test_that("util_setup_dashboard embeds result summary plots", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("htmltools")

  dashboard_table <- data.frame(
    VAR_NAMES = "x",
    call_names = "call",
    value = "T",
    Variable_names = "x",
    var_class = "item",
    class = "dataquieR_result",
    href = "report.html",
    popup_href = "report.html",
    title = "Result x",
    stringsAsFactors = FALSE
  )
  attr(dashboard_table, "label_col") <- VAR_NAMES
  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) TRUE,
    util_dashboard_table = function(...) dashboard_table
  )

  report <- structure(
    list(
      call.x = list(
        SummaryPlot = ggplot2::ggplot(data.frame(x = 1, y = 1)) +
          ggplot2::geom_point(ggplot2::aes(x, y))
      )
    ),
    label_col = VAR_NAMES
  )

  table <- util_setup_dashboard(
    report,
    repsum = data.frame(),
    return_table_only = TRUE
  )

  untranslated_names <- util_untranslated_colnames(table)
  figure <- table[[match("Figure", untranslated_names)]][[1]]
  value <- table[[match("value", untranslated_names)]][[1]]
  expect_match(figure, "<img", fixed = TRUE)
  expect_identical(value, figure)
})

test_that("util_setup_dashboard does not render links without targets", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  dashboard_table <- data.frame(
    VAR_NAMES = "group_without_id",
    call_names = "call",
    value = "1",
    Variable_names = "Group without ID",
    var_class = "item",
    class = "1",
    href = NA_character_,
    popup_href = NA_character_,
    title = "Result without target",
    stringsAsFactors = FALSE
  )
  attr(dashboard_table, "label_col") <- VAR_NAMES
  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) TRUE,
    util_dashboard_table = function(...) dashboard_table
  )

  table <- util_setup_dashboard(
    structure(list(), label_col = VAR_NAMES),
    repsum = data.frame(),
    return_table_only = TRUE
  )
  variable <- table[[match(
    VAR_NAMES,
    util_untranslated_colnames(table)
  )]][[1]]

  expect_identical(variable, "group_without_id")
  expect_false(grepl("showDataquieRResult", variable, fixed = TRUE))
  expect_false(grepl("<a", variable, fixed = TRUE))
})

test_that("util_setup_dashboard merges one descriptive graph family", {
  skip_on_cran()

  method_name <- "[.fake_dashboard_report"
  old_method <- get0(method_name, envir = globalenv(), inherits = FALSE)
  assign(method_name, function(x, i, j, k, drop = TRUE) {
    x$summaries[[j]]
  }, envir = globalenv())
  withr::defer({
    if (is.null(old_method)) {
      rm(list = method_name, envir = globalenv())
    } else {
      assign(method_name, old_method, envir = globalenv())
    }
  })
  dashboard_table <- data.frame(
    VAR_NAMES = "v00001",
    LABEL = "x",
    call_names = "call",
    value = "1",
    Figure = NA_character_,
    Variable_names = NA_character_,
    var_class = "item",
    class = "1",
    href = "report.html",
    popup_href = "report.html",
    title = "Result x",
    stringsAsFactors = FALSE
  )
  attr(dashboard_table, "label_col") <- LABEL
  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) TRUE,
    util_dashboard_table = function(...) dashboard_table
  )

  make_report <- function(summary_name, graph) {
    summaries <- setNames(list(data.frame(
      Variables = "x",
      Graph = graph,
      stringsAsFactors = FALSE
    )), summary_name)
    structure(
      list(summaries = summaries),
      class = "fake_dashboard_report",
      label_col = LABEL,
      dim = c(1L, 1L, 1L),
      dimnames = list("x", summary_name, "SummaryTable")
    )
  }
  continuous <- util_setup_dashboard(
    make_report("des_summary_continuous", "continuous graph"),
    repsum = data.frame(),
    return_table_only = TRUE
  )
  categorical <- util_setup_dashboard(
    make_report("des_summary_categorical", "categorical graph"),
    repsum = data.frame(),
    return_table_only = TRUE
  )

  expect_identical(
    continuous[[match("Graph", util_untranslated_colnames(continuous))]],
    "continuous graph"
  )
  expect_identical(
    categorical[[match("Graph", util_untranslated_colnames(categorical))]],
    "categorical graph"
  )
})

test_that("dashboard display columns also support untranslated tables", {
  table <- data.frame(LABEL = "Group", Call = "Check")

  expect_identical(
    util_dashboard_display_columns(table, c(LABEL, "Call", "Metric")),
    c(LABEL, "Call")
  )
})
test_that("dashboard preview images retain their deferred source", {
  table <- data.frame(
    Figure = c(
      '<img src="dashboard_images/img_0001.png" width="250" height="200">',
      "<img class='preview' src='data:image/png;base64,AQIDBA=='>",
      "plain text"
    ),
    Value = c("1", "2", "3"),
    stringsAsFactors = FALSE
  )

  deferred <- util_defer_dashboard_images(table)

  expect_true(all(grepl('loading="lazy"', deferred$Figure[1:2], fixed = TRUE)))
  expect_true(all(grepl(
    'decoding="async"', deferred$Figure[1:2], fixed = TRUE
  )))
  expect_match(
    deferred$Figure[[1]],
    'data-dq-lazy-src="dashboard_images/img_0001.png"',
    fixed = TRUE
  )
  expect_match(
    deferred$Figure[[2]],
    "data-dq-lazy-src='data:image/png;base64,AQIDBA=='",
    fixed = TRUE
  )
  expect_identical(deferred$Figure[[3]], "plain text")
  expect_identical(deferred$Value, table$Value)
})

test_that("dashboard preview images refresh after column and scroll changes", {
  report_js <- paste(readLines(system.file(
    "report-dt-style/report_dt.js",
    package = "dataquieR"
  )), collapse = "\n")

  expect_match(
    report_js,
    "draw.dt column-visibility.dt responsive-resize.dt",
    fixed = TRUE
  )
  expect_match(
    report_js,
    '$(document).on("scroll", ".dt-scroll-body"',
    fixed = TRUE
  )
  expect_match(
    report_js,
    "dqInitLazyDashboardImages(root, true)",
    fixed = TRUE
  )
})

test_that("dashboard widgets virtualize table rows", {
  skip_if_not_installed("DT")

  table <- data.frame(
    LABEL = paste("Variable", seq_len(100)),
    Class = rep("Ok", 100),
    class = rep("Ok", 100),
    var_class = rep("Ok", 100),
    Call = rep("Check", 100),
    Metric = rep("Metric", 100),
    value = seq_len(100),
    Graph = rep("", 100),
    Figure = rep("", 100),
    stringsAsFactors = FALSE
  )
  attr(table, "indexes") <- list(
    var_class_col_js_idx = 3L,
    class_col_js_idx = 2L,
    class_raw_col_js_idx = 1L,
    name_col_js_idx = 0L
  )
  util_translated_colnames(table) <- util_translate(
    names(table),
    ns = "dashboard_table"
  )

  widget <- util_dashboard_table2widget(
    table,
    LABEL,
    vars_to_include = "variable_group"
  )
  widget <- widget[[3]][[2]]$children[[1]]

  expect_true(widget$x$options$scroller)
  expect_true(widget$x$options$paging)
  expect_true(widget$x$options$deferRender)
  expect_false(widget$x$options$scrollCollapse)
  expect_false(widget$x$options$autoFill)
  expect_false(widget$x$options$responsive)
  expect_null(widget$x$options$columnControl)
  expect_match(
    as.character(widget$x$options$initComplete),
    '"deferred_column_filters":true',
    fixed = TRUE
  )
  init_button <- widget$x$options$buttons[
    vapply(widget$x$options$buttons, function(button) {
      identical(button$name, "init")
    }, logical(1))
  ][[1L]]
  figure_column <- match(
    util_translate("Figure", as_this_translation = colnames(table)),
    colnames(table)
  ) - 1L
  expect_true(figure_column %in% init_button$show)
  expect_true(widget$x$fillContainer)
})
