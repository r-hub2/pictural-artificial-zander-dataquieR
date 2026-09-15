skip_on_cran()

test_that("DT2 extension list mirrors current util_html_table features", {
  expect_identical(
    util_html_table_dt2_extensions(searchBuilder = FALSE),
    c("Buttons", "FixedColumns", "Responsive")
  )

  expect_identical(
    util_html_table_dt2_extensions(searchBuilder = TRUE),
    c("Buttons", "FixedColumns", "Responsive", "SearchBuilder")
  )

  expect_identical(
    util_html_table_dt2_extensions(columnControl = TRUE),
    c("Buttons", "FixedColumns", "Responsive", "ColumnControl")
  )

  expect_identical(
    util_html_table_dt2_extensions(fixed_header = TRUE),
    c("Buttons", "FixedColumns", "FixedHeader", "Responsive")
  )

  expect_identical(
    util_html_table_dt2_extensions(scroller = TRUE),
    c("Buttons", "FixedColumns", "Responsive", "Scroller")
  )

  expect_identical(
    util_html_table_dt2_extensions(
      searchBuilder = TRUE,
      buttons = FALSE,
      fixedColumns = FALSE,
      responsive = FALSE
    ),
    "SearchBuilder"
  )
})

test_that("DT2 filter positions use a dedicated ColumnControl row", {
  expect_identical(util_html_table_dt2_filter_position(NULL), "none")
  expect_identical(util_html_table_dt2_filter_position("top"), "top")
  expect_identical(
    util_html_table_dt2_filter_position(list(position = "bottom")),
    "bottom"
  )
  expect_identical(
    util_html_table_dt2_filter_position(list(other = "ignored")),
    "none"
  )

  top <- util_html_table_dt2_normalize_options(
    list(),
    filter = "top"
  )
  expect_identical(top[["columnControl"]], list(
    target = 1,
    content = list("search")
  ))

  bottom <- util_html_table_dt2_normalize_options(
    list(),
    filter = "bottom"
  )
  expect_identical(bottom[["columnControl"]][["target"]], "tfoot:0")

  none <- util_html_table_dt2_normalize_options(
    list(),
    filter = "none"
  )
  expect_null(none[["columnControl"]])

  explicit <- list(target = 0, content = list("searchDropdown"))
  overridden <- util_html_table_dt2_normalize_options(
    list(columnControl = explicit),
    filter = "top"
  )
  expect_identical(overridden[["columnControl"]], explicit)
})

test_that("DT2 supplies defaults when options are omitted", {
  options <- util_html_table_dt2_normalize_options(NULL)

  expect_type(options, "list")
  expect_null(options[["dom"]])
  expect_true(all(c("topStart", "topEnd") %in% names(options[["layout"]])))
})

test_that("DT2 filter data preparation handles matrices and row names", {
  tb <- matrix(
    c("a", "b", "1", "2"),
    nrow = 2,
    dimnames = list(c("row_a", "row_b"), c("label", "value"))
  )

  prepared <- util_html_table_dt2_prepare_filter_data(
    tb,
    copy_row_names_to_column = TRUE
  )

  expect_s3_class(prepared, "data.frame")
  expect_identical(names(prepared), c("Variables", "label", "value"))
  expect_equal(prepared$Variables, c("row_a", "row_b"))
  expect_equal(rownames(prepared), c("1", "2"))
})

test_that("DT2 normalizes legacy FixedColumns options", {
  legacy <- util_html_table_dt2_normalize_options(
    list(fixedColumns = list(leftColumns = 2L, rightColumns = 1L))
  )
  expect_identical(legacy[["fixedColumns"]], list(start = 2L, end = 1L))

  legacy_aliases <- util_html_table_dt2_normalize_options(
    list(fixedColumns = list(left = 2L, right = 1L))
  )
  expect_identical(legacy_aliases[["fixedColumns"]], list(start = 2L, end = 1L))

  explicit <- util_html_table_dt2_normalize_options(
    list(fixedColumns = list(start = 3L, end = 1L))
  )
  expect_identical(explicit[["fixedColumns"]], list(start = 3L, end = 1L))

  enabled <- util_html_table_dt2_normalize_options(
    list(fixedColumns = TRUE)
  )
  expect_identical(enabled[["fixedColumns"]], list(start = 1L, end = 0L))

  invalid_passthrough <- util_html_table_dt2_normalize_options(
    list(fixedColumns = "left")
  )
  expect_identical(invalid_passthrough[["fixedColumns"]], "left")

  defaults <- util_html_table_dt2_normalize_options(
    list(fixedColumns = list())
  )
  expect_identical(defaults[["fixedColumns"]], list(start = 1L, end = 0L))
})

test_that("DT2 column filters use type-specific controls", {
  search_list_control <- util_html_table_dt2_search_list_control()
  data <- data.frame(
    Label = c("Systolic blood pressure", "Diastolic blood pressure"),
    Count = c(10L, 20L),
    Date = as.Date(c("2026-01-01", "2026-01-02"))
  )

  options <- util_html_table_dt2_normalize_options(
    list(),
    filter = "top",
    data = data
  )

  column_defs <- options[["columnDefs"]]
  expect_length(column_defs, 3)
  expect_identical(
    column_defs[[1]][["columnControl"]][["content"]][[1]][["content"]],
    search_list_control[["content"]]
  )
  expect_identical(
    column_defs[[2]][["columnControl"]][["content"]],
    list("searchNumber")
  )
  expect_identical(
    column_defs[[3]][["columnControl"]][["content"]],
    list("searchDateTime")
  )
})

test_that("DT2 column filters prefer DATA_TYPE attributes", {
  search_list_control <- util_html_table_dt2_search_list_control()
  numeric_text <- c("1", "2")
  attr(numeric_text, DATA_TYPE) <- DATA_TYPES$STRING
  float_text <- c("0.17", "15.43")
  attr(float_text, DATA_TYPE) <- DATA_TYPES$FLOAT
  integer_text <- c("one", "two")
  attr(integer_text, DATA_TYPE) <- DATA_TYPES$INTEGER
  datetime_text <- c("2026-01-01", "2026-01-02")
  attr(datetime_text, DATA_TYPE) <- DATA_TYPES$DATETIME
  number_paren_text <- c("12 (3.4)", "0 (0%)")
  attr(number_paren_text, DATA_TYPE) <- DATA_TYPE_NUMBER_PAREN

  expect_identical(
    util_html_table_dt2_column_filter_control(numeric_text)[["content"]],
    search_list_control[["content"]]
  )
  expect_identical(
    util_html_table_dt2_column_filter_control(number_paren_text),
    "searchNumber"
  )
  expect_identical(
    util_html_table_dt2_column_filter_control(float_text),
    "searchNumber"
  )
  expect_identical(
    util_html_table_dt2_column_filter_control(integer_text),
    "searchNumber"
  )
  expect_identical(
    util_html_table_dt2_column_filter_control(datetime_text),
    "searchDateTime"
  )
})

test_that("DT2 filters contradiction percentage as numeric", {
  tb <- data.frame(
    CHECK_LABEL = c("rule a", "rule b", "rule c"),
    NUM_con_con = c(1L, 3L, 0L),
    PCT_con_con = c(5, 15.43, 0.17),
    check.names = FALSE
  )
  tb <- util_make_data_slot_from_table_slot(tb)

  options <- util_html_table_dt2_normalize_options(
    list(),
    filter = "top",
    data = tb
  )

  num_column_def <- options[["columnDefs"]][[2]]
  pct_column_def <- options[["columnDefs"]][[3]]
  expect_identical(
    num_column_def[["columnControl"]][["content"]],
    list("searchNumber")
  )
  expect_identical(
    pct_column_def[["columnControl"]][["content"]],
    list("searchNumber")
  )
})

test_that("DT2 number with parenthesized percentage filters use filter data", {
  number_paren_text <- c("12 (3.4)", "0 (0%)")
  attr(number_paren_text, DATA_TYPE) <- DATA_TYPE_NUMBER_PAREN
  data <- data.frame(`N (%)` = number_paren_text, check.names = FALSE)

  options <- util_html_table_dt2_normalize_options(
    list(),
    filter = "top",
    data = data
  )

  column_def <- options[["columnDefs"]][[1]]
  expect_identical(column_def[["columnControl"]][["content"]], list("searchNumber")) # nolint: line_length_linter.
  expect_identical(column_def[["type"]], "num")
  expect_identical(column_def[["searchBuilderType"]], "num")
  expect_identical(column_def[["searchBuilder"]], list(orthogonal = "filter"))
})

test_that("DT2 infers parenthesized percentage filters without DATA_TYPE", {
  data <- data.frame(
    `N (%)` = c("12 (3.4)", "0 (0%)"),
    check.names = FALSE
  )

  options <- util_html_table_dt2_normalize_options(
    list(),
    filter = "top",
    data = data
  )

  column_def <- options[["columnDefs"]][[1]]
  expect_identical(column_def[["columnControl"]][["content"]], list("searchNumber")) # nolint: line_length_linter.
  expect_identical(column_def[["type"]], "num")
  expect_identical(column_def[["searchBuilderType"]], "num")
  expect_identical(column_def[["searchBuilder"]], list(orthogonal = "filter"))
})

test_that("DT2 column filters use lists for small discrete columns", {
  search_list_control <- util_html_table_dt2_search_list_control()
  small_text <- c("A", "A", "B", "B")
  small_numeric <- c(1, 1, 2, 2)
  long_numeric <- seq_len(11)
  html_like <- c("<span>1</span>", "<span>2</span>")

  expect_identical(
    util_html_table_dt2_column_filter_control(small_text)[["content"]],
    search_list_control[["content"]]
  )
  expect_identical(
    util_html_table_dt2_column_filter_control(small_numeric),
    "searchNumber"
  )
  expect_identical(
    util_html_table_dt2_column_filter_control(long_numeric),
    "searchNumber"
  )
  expect_identical(
    util_html_table_dt2_column_filter_control(html_like),
    "searchNumber"
  )
})

test_that("DT2 discrete-filter detection excludes unsupported text columns", {
  expect_true(util_html_table_dt2_is_discrete_filter_column(c("A", "B")))
  expect_false(util_html_table_dt2_is_discrete_filter_column(factor(c(
    "A",
    "B"
  ))))
  expect_false(util_html_table_dt2_is_discrete_filter_column(list("A", "B")))
  expect_false(
    util_html_table_dt2_is_discrete_filter_column(as.character(1:11))
  )
  expect_false(util_html_table_dt2_is_discrete_filter_column(c(
    "<b>A</b>",
    "B"
  )))
})

test_that("DT2 list filters use filter values instead of display HTML", {
  control <- util_html_table_dt2_search_list_control()
  expect_identical(
    control[["content"]],
    list(list(extend = "searchList", orthogonal = "dq_filter_label"))
  )
})

test_that("DT2 recognizes number with parenthesized percentage text", {
  expect_true(util_html_table_dt2_is_number_paren_filter_column(
    c("12 (3.4)", "0 (0%)")
  ))
  expect_false(util_html_table_dt2_is_number_paren_filter_column(
    c("12", "free text")
  ))
})

test_that("DT2 layout keeps old button-only table chrome minimal", {
  layout <- util_html_table_dt2_layout(searchBuilder = FALSE, buttons = TRUE)

  expect_identical(layout[["topStart"]], "buttons")
  expect_null(layout[["topEnd"]])
  expect_null(layout[["bottomStart"]])
  expect_null(layout[["bottomEnd"]])
})

test_that("DT2 layout can suppress the top chrome", {
  layout <- util_html_table_dt2_layout(searchBuilder = FALSE, buttons = FALSE)

  expect_null(layout[["topStart"]])
  expect_null(layout[["topEnd"]])
  expect_null(layout[["bottomStart"]])
  expect_null(layout[["bottomEnd"]])
})

test_that("DT2 layout includes SearchBuilder before buttons", {
  layout <- util_html_table_dt2_layout(searchBuilder = TRUE, buttons = TRUE)

  expect_identical(layout[["topStart"]], c("searchBuilder", "buttons"))
  expect_null(layout[["topEnd"]])
  expect_null(layout[["bottomStart"]])
  expect_null(layout[["bottomEnd"]])
})

test_that("DT2 option normalization translates old dom intent once", {
  options <- util_html_table_dt2_normalize_options(list(dom = "QBt"))

  expect_null(options[["dom"]])
  expect_identical(
    options[["layout"]][["topStart"]],
    c("searchBuilder", "buttons")
  )
})

test_that("DT2 option normalization keeps explicit layout", {
  explicit_layout <- list(topStart = "search", bottomEnd = "paging")

  options <- util_html_table_dt2_normalize_options(list(
    dom = "QBt",
    layout = explicit_layout
  ))

  expect_null(options[["dom"]])
  expect_identical(options[["layout"]], explicit_layout)
})

test_that("DT2 option normalization translates legacy export button aliases", {
  options <- util_html_table_dt2_normalize_options(list(
    buttons = list(
      list(extend = "copy"),
      list(extend = "excel"),
      list(extend = "csv"),
      list(extend = "pdf"),
      list(extend = "print"),
      "copy"
    )
  ))

  expect_identical(
    vapply(options[["buttons"]][1:5], `[[`, character(1), "extend"),
    c("copyHtml5", "excelHtml5", "csvHtml5", "pdfHtml5", "print")
  )
  expect_identical(options[["buttons"]][[6]], "copyHtml5")
})

test_that("DT2 button aliases are normalized recursively", {
  nested <- util_html_table_dt2_normalize_button_extensions(list(
    list(
      extend = "collection",
      buttons = list("copy", list(extend = "pdf"))
    ),
    TRUE
  ))

  expect_identical(nested[[1]][["extend"]], "collection")
  expect_identical(nested[[1]][["buttons"]][[1]], "copyHtml5")
  expect_identical(nested[[1]][["buttons"]][[2]][["extend"]], "pdfHtml5")
  expect_true(nested[[2]])
  expect_null(util_html_table_dt2_normalize_button_extensions(NULL))
  expect_true(util_html_table_dt2_normalize_button_extensions(TRUE))
  expect_identical(
    util_html_table_dt2_normalize_button_extensions(c("copy", "csv", "print")),
    c("copyHtml5", "csvHtml5", "print")
  )
})

test_that("html table backend defaults to DT2 and falls back to DT", {
  expect_identical(
    util_html_table_backend(installed = c(DT2 = TRUE, DT = TRUE)),
    "DT2"
  )
  expect_identical(
    util_html_table_backend(installed = c(DT2 = FALSE, DT = TRUE)),
    "DT"
  )
  expect_identical(
    util_html_table_backend(NULL, installed = c(DT2 = TRUE, DT = TRUE)),
    "DT2"
  )
  expect_identical(
    util_html_table_backend("DT", installed = c(DT2 = TRUE, DT = TRUE)),
    "DT"
  )
  expect_identical(
    util_html_table_backend("DT2", installed = c(DT2 = TRUE, DT = TRUE)),
    "DT2"
  )
  expect_error(
    util_html_table_backend("auto", installed = c(DT2 = FALSE, DT = FALSE)),
    "Missing either package"
  )
  expect_error(
    util_html_table_backend("DT2", installed = c(DT2 = FALSE, DT = TRUE)),
    "Missing the package"
  )
  expect_error(
    util_html_table_backend("DT", installed = c(DT2 = TRUE, DT = FALSE)),
    "Missing the package"
  )
})

test_that("DT2 widget helper renders a minimal htmlwidget", {
  skip_on_cran()
  skip_if_not_installed("DT2")
  skip_if_not_installed("htmltools")

  widget <- util_html_table_dt2_widget(
    data.frame(a = 1),
    options = util_html_table_dt2_normalize_options(list(dom = "Bt")),
    extensions = util_html_table_dt2_extensions(),
    class = "myDT"
  )

  html <- htmltools::renderTags(widget)$html

  expect_s3_class(widget, "htmlwidget")
  expect_true(grepl("myDT", html, fixed = TRUE))
  expect_true(grepl("classList.add('datatables')", html, fixed = TRUE))
})

test_that("DT2 widget helper honours fillContainer", {
  skip_on_cran()
  skip_if_not_installed("DT2")

  widget <- util_html_table_dt2_widget(
    data.frame(a = 1),
    fillContainer = TRUE
  )

  expect_true(widget$x$fillContainer)
  expect_true(widget$x$options$scrollX)
  expect_identical(widget$x$options$scrollY, "100px")
  expect_false(widget$x$options$autoWidth)
  expect_identical(widget$width, "100%")
  expect_identical(widget$height, "100%")
  expect_true(widget$sizingPolicy$viewer$fill)
  expect_match(widget$x$options$dt2_theme$class, "fill-container")

  html <- htmltools::renderTags(widget)$html
  expect_true(grepl("function hasExplicitHeight(node)", html, fixed = TRUE))
  expect_true(grepl("function hasExplicitHeightContext(node)", html, fixed = TRUE)) # nolint: line_length_linter.
  expect_true(grepl(
    "if (!hasExplicitHeightContext(el.parentElement))",
    html,
    fixed = TRUE
  ))
  expect_true(grepl("return 0;", html, fixed = TRUE))
  expect_true(grepl("el.style.height = '100%'", html, fixed = TRUE))
  expect_true(grepl("el.style.height = 'auto'", html, fixed = TRUE))
  expect_true(grepl("scrollBody.style.width = '100%'", html, fixed = TRUE))
  expect_true(grepl("scroll.style.maxWidth = ''", html, fixed = TRUE))
  expect_false(grepl("scroll.style.maxWidth = '100%'", html, fixed = TRUE))
  expect_true(grepl(
    "el._dataquieRFillObserver.observe(el.parentElement)",
    html,
    fixed = TRUE
  ))

  preserved <- util_html_table_dt2_widget(
    data.frame(a = 1),
    options = list(scrollY = "55vh"),
    fillContainer = TRUE
  )
  expect_identical(preserved$x$options$scrollY, "55vh")

  not_filled <- util_html_table_dt2_widget(data.frame(a = 1))
  expect_false(not_filled$x$fillContainer)
  expect_false(not_filled$sizingPolicy$viewer$fill)
})

test_that("DT2 widget helper keeps display titles separate from transport names", { # nolint: line_length_linter.
  skip_on_cran()
  skip_if_not_installed("DT2")

  duplicated_names <- c(
    "<div class=\"colheader\"><span> </span></div>",
    "<div class=\"colheader\"><span> </span></div>"
  )
  data <- data.frame(
    c("a", "b"),
    c("ok", "critical"),
    check.names = FALSE
  )
  colnames(data) <- duplicated_names

  widget <- util_html_table_dt2_widget(data)

  expect_false(any(endsWith(names(widget$x$data), ".1")))
  expect_null(names(widget$x$options$columns))
  expect_identical(
    unname(vapply(widget$x$options$columns, `[[`, character(1), "title")),
    duplicated_names
  )
  expect_identical(
    unname(vapply(widget$x$options$columns, `[[`, character(1), "data")),
    names(widget$x$data)
  )
})

test_that("util_html_table renders through the default DT2 backend", {
  skip_on_cran()
  skip_if_not_installed("DT2")
  skip_if_not_installed("htmltools")

  withr::local_options(list(dataquieR.html_table_backend = NULL))

  rendered <- htmltools::renderTags(util_html_table(
    data.frame(Variables = "x", Total = 1),
    link_variables = FALSE
  ))
  html <- rendered$html
  dependency_names <- vapply(
    rendered$dependencies,
    `[[`,
    character(1),
    "name"
  )

  expect_true(grepl("dt2 html-widget", html, fixed = TRUE))
  expect_true(grepl("dataquieRdtCallback", html, fixed = TRUE))
  expect_true(grepl("classList.add('datatables')", html, fixed = TRUE))
  expect_true(grepl('"columnControl"', html, fixed = TRUE))
  expect_true(grepl('"target":1', html, fixed = TRUE))
  expect_true(grepl('"content":["search"]', html, fixed = TRUE))
  expect_true(grepl('"searchList"', html, fixed = TRUE))
  expect_true(grepl('"searchNumber"', html, fixed = TRUE))
  expect_true("dt-columncontrol-css" %in% dependency_names)
  expect_true("dt-columncontrol-js" %in% dependency_names)
})

test_that("util_html_table includes Scroller for both table backends", {
  skip_on_cran()
  skip_if_not_installed("DT")
  skip_if_not_installed("DT2")
  skip_if_not_installed("htmltools")

  expected_dependency <- c(DT = "dt-ext-scroller", DT2 = "dt-scroller-js")
  for (backend in names(expected_dependency)) {
    withr::local_options(list(dataquieR.html_table_backend = backend))
    rendered <- htmltools::renderTags(util_html_table(
      data.frame(a = seq_len(20)),
      link_variables = FALSE,
      filter = "none",
      options = list(scroller = TRUE, paging = TRUE, deferRender = TRUE)
    ))
    dependency_names <- vapply(
      rendered$dependencies,
      `[[`,
      character(1),
      "name"
    )
    expect_true(expected_dependency[[backend]] %in% dependency_names)
  }
})

test_that("DT2 dashboard tables defer SearchBuilder initialization", {
  skip_on_cran()
  skip_if_not_installed("DT2")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("jsonlite")

  withr::local_options(list(dataquieR.html_table_backend = "DT2"))

  rendered <- htmltools::renderTags(util_html_table(
    data.frame(Variables = c("a", "b"), Class = c("Ok", "Critical")),
    link_variables = FALSE,
    searchBuilder = TRUE,
    init_search = list(
      criteria = list(list(
        condition = "!=",
        data = "Class",
        type = "string",
        value = list("Ok")
      )),
      logic = "AND"
    )
  ))

  dependency_names <- vapply(
    rendered$dependencies,
    `[[`,
    character(1),
    "name"
  )
  report_js <- paste(readLines(system.file(
    "report-dt-style/report_dt.js",
    package = "dataquieR"
  )), collapse = "\n")
  report_css <- paste(readLines(system.file(
    "report-dt-style/report-dt-style.css",
    package = "dataquieR"
  )), collapse = "\n")

  expect_true("dt-searchbuilder-js" %in% dependency_names)
  expect_true("dt-datetime-js" %in% dependency_names)
  expect_true(grepl(
    '"searchBuilder":{"liveSearch":false}',
    rendered$html,
    fixed = TRUE
  ))
  expect_true(grepl(
    '"topStart":["searchBuilder","buttons"]',
    rendered$html,
    fixed = TRUE
  ))
  expect_true(grepl(
    "dataquieRInitSearchBuilder(config.table, config.initSearch)",
    report_js,
    fixed = TRUE
  ))
  expect_false(grepl(
    "config.table.searchBuilder.rebuild(config.initSearch)",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "function dqDataTableColumnNodesByNameOrHeader",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "function dqDataTableColumnConfigName",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "function dqDataTableCellValue",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "function dqDataTableSortValue",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "function dqDataTableNumberParenValue",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "function dqDataTableSearchValue",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    'columnApi.search.fixed("dq-number-paren", function(searchData)',
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "dq-number-paren-operator",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "function matchesNumberParenFilter",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "dqDataTableApiFromMeta(meta)",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "dqDataTableColumnNodesByNameOrHeader(tb, column).each",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "dataquieRcolorize(config)",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "function dataquieRBuildDeferredColumnFilters",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    '"<input type=\'checkbox\'>"',
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "column.search(value, false, true).draw()",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    ".dq-table-tool-separator",
    report_css,
    fixed = TRUE
  ))
  expect_true(grepl(
    "tr.dq-deferred-column-filters",
    report_css,
    fixed = TRUE
  ))
  expect_true(grepl(
    'dataquieRSetButtonText(this, "Initial columns")',
    report_js,
    fixed = TRUE
  ))
})

test_that("Excel colors use export row and column positions", {
  skip_on_cran()

  report_js <- paste(readLines(system.file(
    "report-dt-style/report_dt.js",
    package = "dataquieR"
  )), collapse = "\n")

  expect_true(grepl(
    "rowIndex.dom2data.forEach(function(dataRowIdx, exportRowIdx)",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    'that.columns(":visible").indexes().toArray()',
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "xlRow = excelLayout.dataStartRow + exportRowIdx",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "headerRows = exportData.headerStructure.length",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "excelLayout.dataStartRow - 1",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "createXlColLetter(exportColIdx)",
    report_js,
    fixed = TRUE
  ))
  expect_false(grepl(
    "tdNode.is(\":visible\")",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "function dqDataTableGradingFillColors",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "function dqDataTableGradingStyle",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "var cellData = dqDataTableCellValue",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "that.row(dataRowIdx).data()",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "that.row(rowIndex.dom2data[rowIdx - 1]).data()",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "function dqCustomizePrint",
    report_js,
    fixed = TRUE
  ))
  expect_false(grepl(
    "function getEffectiveCellBgColor",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "dqDataTableColumnConfigName(that, gradingCfg, dataColIdx) == null",
    report_js,
    fixed = TRUE
  ))
  expect_true(grepl(
    "highlightCells(that, gradingCfg",
    report_js,
    fixed = TRUE
  ))
})

test_that("util_html_table respects explicit DT backend", {
  skip_on_cran()
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")

  withr::local_options(list(dataquieR.html_table_backend = "DT"))

  html <- htmltools::renderTags(util_html_table(
    data.frame(Variables = "x", Total = 1),
    link_variables = FALSE
  ))$html

  expect_true(grepl("datatables html-widget", html, fixed = TRUE))
  expect_false(grepl("dt2 html-widget", html, fixed = TRUE))
})

test_that("DT2 keeps variable and label columns readable", {
  skip_if_not_installed("DT2")

  withr::local_options(list(dataquieR.html_table_backend = "DT2"))

  variable_names <- c(
    "very_long_variable_name_that_needs_space",
    "another_long_variable_name"
  )
  labels <- c(
    "A long readable label that should not be squeezed into a tiny column",
    "Another label that needs space"
  )
  meta_data <- data.frame(
    VAR_NAMES = variable_names,
    LABEL = labels,
    LONG_LABEL = labels,
    DATA_TYPE = DATA_TYPES$FLOAT,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    stringsAsFactors = FALSE
  )

  explicit_tags <- util_html_table(
    data.frame(
      Variables = variable_names,
      Labels = labels,
      Total = c(1, 2),
      check.names = FALSE
    ),
    link_variables = FALSE
  )
  split_tags <- util_html_table(
    data.frame(
      Variables = variable_names,
      Total = c(1, 2),
      check.names = FALSE
    ),
    meta_data = meta_data
  )

  get_column_defs <- function(tags) {
    tags[[2]][["children"]][[1]]$x$options$columnDefs
  }
  explicit_column_defs <- get_column_defs(explicit_tags)
  split_column_defs <- get_column_defs(split_tags)

  get_targets_for_width <- function(column_defs, width) {
    unlist(lapply(column_defs, function(column_def) {
      if (identical(column_def[["width"]], width)) {
        return(column_def[["targets"]])
      }
      integer(0)
    }))
  }

  get_readable_targets <- function(column_defs) {
    readable_defs <- Filter(function(column_def) {
      isTRUE(grepl(
        "dq-readable-text",
        column_def[["className"]],
        fixed = TRUE
      ))
    }, column_defs)
    readable_defs[[1]][["targets"]]
  }

  expect_identical(get_targets_for_width(explicit_column_defs, "5em"), 2)
  expect_identical(get_targets_for_width(explicit_column_defs, "14em"), 0)
  expect_identical(get_targets_for_width(explicit_column_defs, "24em"), 1)
  expect_identical(get_readable_targets(explicit_column_defs), c(0, 1))
  expect_identical(
    get_targets_for_width(split_column_defs, "5em"),
    get_targets_for_width(explicit_column_defs, "5em")
  )
  expect_identical(
    get_targets_for_width(split_column_defs, "14em"),
    get_targets_for_width(explicit_column_defs, "14em")
  )
  expect_identical(
    get_targets_for_width(split_column_defs, "24em"),
    get_targets_for_width(explicit_column_defs, "24em")
  )
  expect_identical(
    get_readable_targets(split_column_defs),
    get_readable_targets(explicit_column_defs)
  )
})

test_that("DT SearchBuilder tables include DateTime for date columns", {
  skip_on_cran()
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")

  withr::local_options(list(dataquieR.html_table_backend = "DT"))

  rendered <- htmltools::renderTags(util_html_table(
    data.frame(Date = as.Date("2026-01-01"), Label = "x"),
    link_variables = FALSE,
    searchBuilder = TRUE
  ))
  dependency_names <- vapply(
    rendered$dependencies,
    `[[`,
    character(1),
    "name"
  )

  expect_true("dt-ext-datetime" %in% dependency_names)
})

test_that("ReportSummaryTable can render through the default DT2 backend", {
  skip_on_cran()
  skip_if_not_installed("DT2")
  skip_if_not_installed("htmltools")

  withr::local_options(list(dataquieR.html_table_backend = NULL))

  summary_table <- structure(
    data.frame(
      Variables = c("a", "b"),
      N = c(10, 10),
      Missing = c(1, 2)
    ),
    class = c("ReportSummaryTable", "data.frame")
  )

  widget <- print.ReportSummaryTable(
    summary_table,
    dt = TRUE,
    fillContainer = TRUE,
    view = FALSE
  )
  html <- htmltools::renderTags(widget)$html

  expect_s3_class(widget, "htmlwidget")
  expect_true(grepl("dt2 html-widget", html, fixed = TRUE))
  expect_true(grepl("ReportSummaryTable", html, fixed = TRUE))
  expect_true(grepl("myDT matrixTable", html, fixed = TRUE))
  expect_true(grepl("classList.add('datatables')", html, fixed = TRUE))
  expect_true(widget$x$fillContainer)
  expect_true(widget$x$options$scrollX)
  expect_identical(widget$x$options$scrollY, "100px")
  expect_false(widget$x$options$autoWidth)
  expect_true(widget$sizingPolicy$viewer$fill)
  expect_identical(widget$x$options$columnDefs[[1]]$targets, seq_len(2))
  expect_true(grepl(
    "scrollHeadInner.style.width = ''",
    html,
    fixed = TRUE
  ))
  expect_true(grepl(
    "dq-matrix-fill-container",
    html,
    fixed = TRUE
  ))
  expect_true(grepl(
    "syncMatrixWidths(wrapper)",
    html,
    fixed = TRUE
  ))
  expect_true(grepl(
    "col.style.minWidth = '0'",
    html,
    fixed = TRUE
  ))
  expect_true(grepl(
    "api.fixedColumns().relayout",
    html,
    fixed = TRUE
  ))
})
