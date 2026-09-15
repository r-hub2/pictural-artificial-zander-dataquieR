skip_on_cran()

test_that("util_html_table serializes columns with incomplete value names", {
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")

  tb <- structure(
    list(Total = structure(c("a", "b", "c"),
        names = c("row1", NA_character_, "row3")
      )),
    class = "data.frame",
    row.names = c(NA_integer_, -3L)
  )

  expect_silent(htmltools::renderTags(util_html_table(
    tb,
    link_variables = FALSE
  )))
})

test_that("util_html_table can skip alignment inference for known text", {
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")

  testthat::local_mocked_bindings(
    util_get_datatables_alignments = function(...) {
      util_error("alignment inference should not run")
    },
    .package = "dataquieR"
  )

  expect_silent(htmltools::renderTags(util_html_table(
    data.frame(Name = "plain text", Value = "linked text"),
    infer_column_alignments = FALSE,
    link_variables = FALSE
  )))
})

test_that("DT filters reuse unescaped prepared display data", {
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")
  withr::local_options(list(dataquieR.html_table_backend = "DT"))

  original_prepare <- get(
    "util_html_table_prepare_data",
    envir = asNamespace("dataquieR")
  )
  calls <- logical()
  testthat::local_mocked_bindings(
    util_html_table_prepare_data = function(..., df_escape) {
      calls <<- c(calls, df_escape)
      original_prepare(..., df_escape = df_escape)
    },
    .package = "dataquieR"
  )

  widget <- util_html_table(
    data.frame(Variables = "x", Total = "12"),
    link_variables = FALSE,
    filter = "top"
  )[[2]][["children"]][[1]]

  expect_identical(calls, FALSE)
  expect_match(widget$x$filterHTML, 'data-type="integer"', fixed = TRUE)
})

test_that("prepared table types avoid a second alignment type guess", {
  tb <- data.frame(Code = c("1", "2"))
  attr(tb, "dataquieR_html_table_data_types") <- c(
    Code = DATA_TYPES$INTEGER
  )

  testthat::local_mocked_bindings(
    prep_robust_guess_data_type = function(...) {
      util_error("alignment inference should reuse prepared types")
    },
    .package = "dataquieR"
  )

  alignments <- util_get_datatables_alignments(tb)

  expect_identical(alignments[[1]][["className"]], "dt-right")
})

test_that("util_html_table carries hidden colvis button classes", {
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")

  tb <- data.frame(
    Variables = "x",
    Visible = 1,
    Hidden = 2,
    Total = 3
  )
  col_tags <- list(
    `All columns` = names(tb),
    Hidden = util_attach_attr(
      c("Variables", "Hidden", "Total"),
      cssClass = "dq-hidden-col"
    )
  )

  html <- htmltools::renderTags(util_html_table(
    tb,
    link_variables = FALSE,
    col_tags = col_tags
  ))$html

  expect_true(grepl(
    '"className":"buttons-colvisGroup dq-hidden-col"',
    html,
    fixed = TRUE
  ))
})

test_that("export button headers do not treat plain labels as selectors", {
  skip_if_not_installed("DT")

  buttons <- util_make_export_buttons(
    dl_fn = "example",
    title = "Example",
    messageBottom = NULL,
    messageTop = NULL
  )
  export_header <- buttons[[1]][["exportOptions"]][["format"]][["header"]]

  expect_match(export_header, "return text;", fixed = TRUE)
  expect_match(export_header, "$('<div/>').html(text)", fixed = TRUE)
  expect_false(grepl("$(data)", export_header, fixed = TRUE))

  print_button <- buttons[[5]]
  expect_identical(
    as.character(print_button[["customize"]]),
    "dqCustomizePrint"
  )
})

test_that("variable links replace an enclosing report overview", {
  skip_if_not_installed("htmltools")

  tb <- data.frame(Variables = "x")
  meta_data <- data.frame(
    VAR_NAMES = "x",
    LABEL = "Variable X",
    LONG_LABEL = "Variable X long",
    DATA_TYPE = "string",
    SCALE_LEVEL = "nominal"
  )

  linked <- util_html_table_link_variables(
    tb = tb,
    descs = NULL,
    meta_data = meta_data,
    label_col = LABEL
  )$tb[["Labels"]][[1]]

  expect_match(linked, 'href="VAR_x.html#x"', fixed = TRUE)
  expect_match(linked, 'target="_top"', fixed = TRUE)
  expect_false(grepl("navigateDataquieRFrame", linked, fixed = TRUE))
})

test_that("variable links use raw labels preserved during HTML escaping", {
  skip_if_not_installed("htmltools")

  labels <- c(
    "Higher > 30",
    "Lower < 10",
    "A & B",
    "A \"quoted\" label"
  )
  meta_data <- data.frame(
    VAR_NAMES = paste0("v", seq_along(labels)),
    LABEL = labels,
    LONG_LABEL = labels,
    DATA_TYPE = "string",
    SCALE_LEVEL = "nominal"
  )
  tb <- util_df_escape(data.frame(Variables = labels))

  linked <- util_html_table_link_variables(
    tb = tb,
    descs = NULL,
    meta_data = meta_data,
    label_col = LABEL
  )$tb

  expected_hrefs <- paste0(
    'href="VAR_',
    prep_link_escape(labels),
    ".html#",
    prep_link_escape(labels),
    '"'
  )
  expect_identical(
    as.vector(linked[["Variables"]]),
    meta_data[[VAR_NAMES]]
  )
  expect_identical(
    util_attr(linked[["Variables"]], "plain_label", exact = TRUE),
    labels
  )
  expect_true(all(vapply(seq_along(labels), function(index) {
    grepl(expected_hrefs[[index]], linked[["Labels"]][[index]], fixed = TRUE)
  }, logical(1))))
  expect_false(any(grepl(
    "Highergt30|Lowerlt10|AampB|Aquotquotedquotlabel",
    linked[["Labels"]]
  )))
})

test_that("variable links send SSI computed metrics to scale pages", {
  tb <- data.frame(Variables = "MISS_RESP_all_questionnaire")
  meta_data <- data.frame(
    VAR_NAMES = "MISS_RESP_all_questionnaire",
    LABEL = "Missing responses",
    DATA_TYPE = "float",
    SCALE_LEVEL = "ratio",
    LONG_LABEL = "Missing responses (all_questionnaire)",
    COMPUTED_VARIABLE_ROLE = "MISS_RESP",
    CHECK_ID = "1",
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    CHECK_ID = 1,
    CHECK_LABEL = "all_questionnaire",
    SCALE_NAME = NA_character_,
    SCALE_ACRONYM = NA_character_,
    stringsAsFactors = FALSE
  )

  linked <- util_html_table_link_variables(
    tb = tb,
    descs = NULL,
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item,
    label_col = LABEL
  )$tb[["Labels"]][[1]]

  expect_match(linked, 'href="SSIGROUP_MISS_RESP.html#MISS_RESP"',
    fixed = TRUE)
  expect_false(grepl("VAR_Missingresponses.html", linked, fixed = TRUE))
})

test_that("variable links can send SSI computed metrics to cross-item pages", {
  tb <- data.frame(Variables = "MISS_RESP_all_questionnaire")
  meta_data <- data.frame(
    VAR_NAMES = "MISS_RESP_all_questionnaire",
    LABEL = "Missing responses",
    DATA_TYPE = "float",
    SCALE_LEVEL = "ratio",
    LONG_LABEL = "Missing responses (all_questionnaire)",
    COMPUTED_VARIABLE_ROLE = "MISS_RESP",
    CHECK_ID = "all_questionnaire",
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    CHECK_ID = "all_questionnaire",
    CHECK_LABEL = "all_questionnaire",
    SCALE_NAME = NA_character_,
    SCALE_ACRONYM = NA_character_,
    stringsAsFactors = FALSE
  )

  linked <- util_html_table_link_variables(
    tb = tb,
    descs = NULL,
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item,
    label_col = LABEL,
    ssi_link_target = "cross_item"
  )$tb[["Labels"]][[1]]

  expect_match(linked, 'href="allquestionnaire.html#all_questionnaire"',
    fixed = TRUE)
  expect_match(linked, ">all_questionnaire</a>", fixed = TRUE)
  expect_false(grepl("SSIGROUP_MISS_RESP.html#MISS_RESP", linked,
      fixed = TRUE))
})

test_that("compact N percent columns get numeric DT filters on percentages", {
  skip_if_not_installed("DT")
  withr::local_options(list(dataquieR.html_table_backend = "DT"))

  n_pct <- c("12 (3.4%)", "12 (10.1%)", "2 (0.7%)")
  attr(n_pct, DATA_TYPE) <- DATA_TYPE_NUMBER_PAREN
  tb <- data.frame(
    Variables = c("a", "b", "c"),
    check.names = FALSE
  )
  tb[["Missing codes N (%)"]] <- n_pct

  widget <- util_html_table(
    tb,
    filter = "top",
    link_variables = FALSE
  )[[2]][["children"]][[1]]

  expect_match(widget$x$filterHTML, 'data-type="number"', fixed = TRUE)
  expect_match(widget$x$filterHTML, 'data-min="0.7"', fixed = TRUE)
  expect_match(widget$x$filterHTML, 'data-max="10.1"', fixed = TRUE)
  expect_false(grepl('data-type="factor"', widget$x$filterHTML, fixed = TRUE))
})

test_that("compact N percent columns are inferred for DT filters without DATA_TYPE", { # nolint: line_length_linter.
  skip_if_not_installed("DT")
  withr::local_options(list(dataquieR.html_table_backend = "DT"))

  tb <- data.frame(
    Variables = c("a", "b", "c"),
    `Missing codes N (%)` = c("12 (3.4%)", "12 (10.1%)", "2 (0.7%)"),
    check.names = FALSE
  )

  widget <- util_html_table(
    tb,
    filter = "top",
    link_variables = FALSE
  )[[2]][["children"]][[1]]

  expect_match(widget$x$filterHTML, 'data-type="number"', fixed = TRUE)
  expect_match(widget$x$filterHTML, 'data-min="0.7"', fixed = TRUE)
  expect_match(widget$x$filterHTML, 'data-max="10.1"', fixed = TRUE)
})

test_that("DT filters use DATA_TYPE attributes from non-display data", {
  skip_if_not_installed("DT")
  withr::local_options(list(dataquieR.html_table_backend = "DT"))

  counts <- c("1", "2", "3")
  attr(counts, DATA_TYPE) <- DATA_TYPES$INTEGER
  tb <- data.frame(
    Variables = c("a", "b", "c"),
    Count = counts,
    check.names = FALSE
  )

  widget <- util_html_table(
    tb,
    filter = "top",
    link_variables = FALSE
  )[[2]][["children"]][[1]]

  expect_match(widget$x$filterHTML, 'data-type="integer"', fixed = TRUE)
  expect_match(widget$x$filterHTML, 'data-min="1"', fixed = TRUE)
  expect_match(widget$x$filterHTML, 'data-max="3"', fixed = TRUE)
})

test_that("util_html_table uses report metadata and rotates filter data", {
  skip_on_cran()
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("knitr")

  withr::local_options(list(
    dataquieR.html_table_backend = "DT",
    knitr.in.progress = TRUE
  ))
  knit_global <- knitr::knit_global()
  report_existed <- exists("report", envir = knit_global, inherits = FALSE)
  old_report <- if (report_existed) {
    get("report", envir = knit_global, inherits = FALSE)
  } else {
    NULL
  }
  withr::defer({
    if (report_existed) {
      assign("report", old_report, envir = knit_global)
    } else if (exists("report", envir = knit_global, inherits = FALSE)) {
      rm("report", envir = knit_global)
    }
  })
  assign("report", list(meta_data = data.frame(
    VAR_NAMES = "x",
    LABEL = "Label X",
    LONG_LABEL = "Long X",
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    stringsAsFactors = FALSE
  )), envir = knit_global)

  html <- htmltools::renderTags(util_html_table(
    data.frame(Variables = "x", Total = 1),
    link_variables = TRUE
  ))$html

  expect_match(html, "VAR_x.html#x", fixed = TRUE)
  expect_match(html, "Label X", fixed = TRUE)

  widget <- util_html_table(
    data.frame(Variables = "x", Total = 1),
    link_variables = FALSE,
    rotate_for_one_row = TRUE,
    filter = "top"
  )[[2]][["children"]][[1]]

  expect_equal(nrow(widget$x$data), 2L)
  expect_equal(as.character(widget$x$data[[1]]), c("Variables", "Total"))
  expect_equal(as.character(widget$x$data[[2]]), c("x", "TRUE"))
  expect_equal(length(gregexpr("<td", widget$x$filterHTML, fixed = TRUE)[[1]]), 2L) # nolint: line_length_linter.
})

test_that("util_html_table handles small argument guardrails", {
  skip_if_not_installed("DT")
  skip_if_not_installed("htmlwidgets")

  expect_null(util_html_table(data.frame()))
  expect_null(util_html_table(data.frame(A = integer())))
  expect_error(
    util_html_table(
      data.frame(A = 1L),
      link_variables = FALSE,
      cols_are_indicatormetrics = TRUE,
      colnames_aliases2acronyms = TRUE
    ),
    "cols_are_indicatormetrics",
    fixed = TRUE
  )
  expect_error(
    util_html_table(
      data.frame(A = 1L, B = 2L),
      link_variables = FALSE,
      descs = "only one"
    ),
    "Need one description per column",
    fixed = TRUE
  )
})

test_that("util_html_table renders indicator metric column headers", {
  skip_on_cran()
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")
  withr::local_options(list(dataquieR.html_table_backend = "DT"))

  widget <- util_html_table(
    data.frame(
      NUM_acc_ud_outlu = 1L,
      PCT_acc_ud_outlu = 2.5,
      check.names = FALSE
    ),
    link_variables = FALSE,
    cols_are_indicatormetrics = TRUE,
    colnames_aliases2acronyms = FALSE
  )[[2]][["children"]][[1]]

  expect_match(widget$x$container, 'title="NUM_acc_ud_outlu', fixed = TRUE)
  expect_match(widget$x$container, 'title="PCT_acc_ud_outlu', fixed = TRUE)
  expect_match(widget$x$container, ". (N)", fixed = TRUE)
  expect_match(widget$x$container, ". (%)", fixed = TRUE)
  expect_match(widget$x$container, "TODO", fixed = TRUE)
})

test_that("DT filters contradiction percentage as numeric", {
  skip_if_not_installed("DT")
  withr::local_options(list(dataquieR.html_table_backend = "DT"))

  tb <- data.frame(
    CHECK_LABEL = c("rule a", "rule b", "rule c"),
    NUM_con_con = c(1L, 3L, 0L),
    PCT_con_con = c(5, 15.43, 0.17),
    check.names = FALSE
  )
  tb <- util_make_data_slot_from_table_slot(tb)

  widget <- util_html_table(
    tb,
    filter = "top",
    link_variables = FALSE
  )[[2]][["children"]][[1]]

  expect_match(widget$x$filterHTML, 'data-type="number"', fixed = TRUE)
  expect_match(widget$x$filterHTML, 'data-type="integer"', fixed = TRUE)
  expect_match(widget$x$filterHTML, 'data-min="0.17"', fixed = TRUE)
  expect_match(widget$x$filterHTML, 'data-max="15.4"', fixed = TRUE)
})

test_that("DT variable link filters stay aligned after adding label columns", {
  skip_if_not_installed("DT")
  withr::local_options(list(dataquieR.html_table_backend = "DT"))

  tb <- data.frame(
    `Miss values-Item, . (%)` = c("0 (0%)", "1 (5%)"),
    Total = c("Ok", "Issue"),
    check.names = FALSE
  )
  rownames(tb) <- c("v1", "v2")
  meta_data <- data.frame(
    VAR_NAMES = c("v1", "v2"),
    LABEL = c("Variable 1", "Variable 2"),
    LONG_LABEL = c("Variable 1 long", "Variable 2 long"),
    DATA_TYPE = c("integer", "integer"),
    SCALE_LEVEL = c("ratio", "ratio")
  )

  widget <- util_html_table(
    tb,
    filter = "top",
    meta_data = meta_data,
    is_matrix_table = TRUE
  )[[2]][["children"]][[1]]

  count_matches <- function(pattern, text) {
    match <- gregexpr(pattern, text, perl = TRUE)[[1]]
    if (identical(match, -1L)) 0L else length(match)
  }

  expect_equal(
    count_matches("<td\\b", widget$x$filterHTML),
    count_matches("<th\\b", widget$x$container)
  )
  expect_match(widget$x$filterHTML, 'data-type="number"', fixed = TRUE)
  expect_match(widget$x$filterHTML, 'data-min="0"', fixed = TRUE)
  expect_match(widget$x$filterHTML, 'data-max="5"', fixed = TRUE)
})

test_that("filter data alignment derives display-only columns", {
  filter_data <- data.frame(A = "kept", check.names = FALSE)
  display_data <- data.frame(
    A = "shown",
    B = '<span filter="raw value">Pretty</span>',
    C = "<strong>Trimmed</strong>",
    check.names = FALSE
  )
  attr(display_data, "number_paren_cols") <- "A"

  aligned <- util_html_table_align_filter_data(filter_data, display_data)

  expect_identical(names(aligned), names(display_data))
  expect_identical(aligned$A, "kept")
  expect_identical(aligned$B, "raw value")
  expect_identical(aligned$C, "Trimmed")
  expect_identical(util_attr(aligned, "number_paren_cols", exact = TRUE), "A")
  expect_identical(
    util_html_table_align_filter_data(NULL, display_data),
    NULL
  )
})

test_that("search-builder configuration handles JSON setup and fallback", {
  configured <- util_configure_search_builder(
    .options = list(dom = "Bfrtip"),
    init_search = list(criteria = list(condition = "=")),
    additional_init_args = list(depthLimit = 2L)
  )

  expect_true(configured$.options$searchBuilder)
  expect_true(configured$.options$search$return)
  expect_identical(configured$.options$dom, "QBfrtip")
  expect_match(configured$init_search, "criteria", fixed = TRUE)
  expect_match(configured$additional_init_args, "depthLimit", fixed = TRUE)

  fallback <- testthat::with_mocked_bindings(
    util_ensure_suggested = function(...) FALSE,
    util_configure_search_builder(
      .options = list(dom = "frtip"),
      init_search = list(unused = TRUE),
      additional_init_args = list(unused = TRUE)
    )
  )

  expect_identical(fallback$.options$dom, "Qfrtip")
  expect_identical(fallback$init_search, "null")
  expect_identical(fallback$additional_init_args, "null")
})

test_that("filter patching keeps widgets unchanged when disabled", {
  widget <- list(x = list(data = "unchanged"))

  expect_identical(
    util_html_table_patch_filters(
      widget,
      filter_data = data.frame(a = 1L),
      filter = "none",
      rownames = FALSE,
      number_paren_cols = NULL
    ),
    widget
  )
  expect_identical(
    util_html_table_patch_filters(
      widget,
      filter_data = data.frame(a = 1L),
      filter = list(position = "none"),
      rownames = FALSE,
      number_paren_cols = NULL
    ),
    widget
  )
  expect_identical(
    util_html_table_patch_filters(
      widget,
      filter_data = NULL,
      filter = "top",
      rownames = FALSE,
      number_paren_cols = NULL
    ),
    widget
  )
})

test_that("compact N percent parser can use percentage values", {
  expect_equal(
    util_html_table_parse_number_paren(
      c("12 (3.4%)", "2 (0,7)"),
      value = "secondary"
    ),
    c(3.4, 0.7)
  )
  expect_equal(
    util_html_table_parse_number_paren(
      c("12 (3.4%)", "2 (0,7)"),
      value = "primary"
    ),
    c(12, 2)
  )
})

test_that("compact N percent range helpers handle edge cases", {
  expect_null(util_html_table_number_paren_range("not compact"))
  expect_identical(
    util_html_table_number_paren_range(c("12 (3.4%)", "2 (3.4%)")),
    list(min = 3.4, max = 3.4, scale = 0, enabled = FALSE)
  )
  expect_identical(
    util_html_table_number_paren_range(c("12 (3,4%)", "2 (10.1%)")),
    list(min = 3.4, max = 10.1, scale = 0, enabled = TRUE)
  )

  expect_false(util_html_table_is_number_paren_column(1:2))
  expect_false(util_html_table_is_number_paren_column(c("", NA_character_)))
  expect_true(util_html_table_is_number_paren_column(
    c("<span>12 (3.4%)</span>", "2 (10.1%)")
  ))
})

test_that(
  "util_html_table_prepare_data normalizes matrices and logical metadata",
  {
    skip_on_cran()

    tb <- matrix(c("TRUE", "FALSE", "1.23456", "2.34567"),
      nrow = 2,
      dimnames = list(NULL, c("flag", "value"))
    )

    prepared_matrix <- util_html_table_prepare_data(
      tb,
      rotate_for_one_row = FALSE,
      copy_row_names_to_column = FALSE,
      df_escape = TRUE
    )

    expect_s3_class(prepared_matrix, "data.frame")
    expect_equal(prepared_matrix$flag, c("TRUE", "FALSE"))

    logical_text <- c("t", "f")
    attr(logical_text, DATA_TYPE) <- "logical"
    prepared_data_frame <- util_html_table_prepare_data(
      data.frame(flag = logical_text),
      rotate_for_one_row = FALSE,
      copy_row_names_to_column = FALSE,
      df_escape = TRUE
    )

    expect_type(prepared_data_frame$flag, "character")
    expect_equal(prepared_data_frame$flag, c("TRUE", "FALSE"))

    boolean_text <- c("+", "-")
    attr(boolean_text, DATA_TYPE) <- "boolean"
    prepared_boolean <- util_html_table_prepare_data(
      data.frame(flag = boolean_text),
      rotate_for_one_row = FALSE,
      copy_row_names_to_column = FALSE,
      df_escape = FALSE
    )
    expect_equal(as.vector(prepared_boolean[[1]]), c(TRUE, FALSE))
  }
)

test_that("DT2 filter data preparation keeps matrix row names when requested", {
  skip_on_cran()

  tb <- matrix(1:4,
    nrow = 2,
    dimnames = list(c("row_a", "row_b"), c("x", "y"))
  )

  prepared <- util_html_table_dt2_prepare_filter_data(
    tb,
    copy_row_names_to_column = TRUE
  )

  expect_s3_class(prepared, "data.frame")
  expect_identical(names(prepared), c("Variables", "x", "y"))
  expect_identical(prepared$Variables, c("row_a", "row_b"))
  expect_identical(rownames(prepared), c("1", "2"))
})
