test_that("util_html_table_patch_filters preserves defensive no-op cases", {
  skip_on_cran()

  dtable <- list(x = list(filterHTML = "unchanged"))

  expect_identical(
    util_html_table_patch_filters(dtable, NULL, NULL, FALSE, character(0)),
    dtable
  )
  expect_identical(
    util_html_table_patch_filters(dtable, NULL, "none", FALSE, character(0)),
    dtable
  )
  expect_identical(
    util_html_table_patch_filters(
      dtable,
      NULL,
      list(position = "none"),
      FALSE,
      character(0)
    ),
    dtable
  )
})

test_that("util_html_table_patch_filters uses numeric secondary values", {
  skip_on_cran()
  skip_if_not_installed("DT")

  filter_data <- data.frame(value = c("1 (20%)", "2 (30%)"))
  dtable <- DT::datatable(filter_data, filter = "top", rownames = FALSE)

  patched <- util_html_table_patch_filters(
    dtable = dtable,
    filter_data = filter_data,
    filter = list(position = "top"),
    rownames = FALSE,
    number_paren_cols = "value"
  )

  expect_s3_class(patched, "datatables")
  expect_match(patched$x$filterHTML, 'data-min="20"', fixed = TRUE)
  expect_match(patched$x$filterHTML, 'data-max="30"', fixed = TRUE)
})

test_that("util_html_table_patch_filters keeps row-name filter cells aligned", {
  skip_on_cran()
  skip_if_not_installed("DT")

  filter_data <- data.frame(
    plain = c("alpha", "beta"),
    check.names = FALSE
  )
  dtable <- DT::datatable(filter_data, filter = "top", rownames = TRUE)

  patched <- util_html_table_patch_filters(
    dtable = dtable,
    filter_data = filter_data,
    filter = "top",
    rownames = TRUE,
    number_paren_cols = character(0)
  )

  expect_s3_class(patched, "datatables")
  expect_match(patched$x$filterHTML, "<td></td>", fixed = TRUE)
  expect_false(grepl("alpha", patched$x$filterHTML, fixed = TRUE))
})

test_that("util_html_table_patch_filters ignores unparseable numeric columns", {
  skip_on_cran()
  skip_if_not_installed("DT")

  filter_data <- data.frame(
    count = c("1 (20%)", "2 (30%)"),
    plain = c("alpha", "beta"),
    check.names = FALSE
  )
  dtable <- DT::datatable(filter_data, filter = "top", rownames = FALSE)

  patched <- util_html_table_patch_filters(
    dtable = dtable,
    filter_data = filter_data,
    filter = "top",
    rownames = FALSE,
    number_paren_cols = c("count", "plain")
  )

  expect_match(patched$x$filterHTML, 'data-min="20"', fixed = TRUE)
  expect_false(grepl('data-min="alpha"', patched$x$filterHTML, fixed = TRUE))
})

test_that("util_dt_normalize_cell_text extracts sortable text", {
  skip_on_cran()

  expect_identical(
    util_dt_normalize_cell_text("<b>count</b><br>30&nbsp;%"),
    "30"
  )
  expect_identical(
    util_dt_normalize_cell_text("<span>count</span><br>30&nbsp;%",
      keep_first_number = FALSE
    ),
    "count\n30"
  )
  expect_identical(
    util_dt_normalize_cell_text("no number"),
    "no number"
  )
})

test_that("util_get_datatables_alignments uses storage and metadata types", {
  skip_on_cran()

  tb <- data.frame(
    int = 1L,
    num = 1.2,
    date = as.Date("2026-07-27"),
    flag = TRUE,
    time = "12:34:56",
    text = "left",
    check.names = FALSE
  )
  attr(tb[["time"]], DATA_TYPE) <- DATA_TYPES$TIME

  alignments <- util_get_datatables_alignments(tb,
    prefer_right_for_datetime = FALSE
  )

  expect_identical(
    vapply(alignments, `[[`, "className", FUN.VALUE = character(1)),
    c("dt-right", "dt-right", "dt-center", "dt-center", "dt-center",
      "dt-left")
  )
  expect_identical(
    vapply(alignments, `[[`, "targets", FUN.VALUE = integer(1)),
    0:5
  )

  right_datetime <- util_get_datatables_alignments(
    tb["date"],
    prefer_right_for_datetime = TRUE
  )
  empty_text <- util_get_datatables_alignments(
    data.frame(empty = c("", NA_character_))
  )

  expect_identical(right_datetime[[1]][["className"]], "dt-right")
  expect_identical(empty_text[[1]][["className"]], "dt-right")
})
