skip_on_cran()

test_that("util_free_varname skips metadata and caller-provided names", {
  meta_data <- data.frame(VAR_NAMES = c("tmp", "tmp_1"))

  expect_equal(
    util_free_varname(
      meta_data = meta_data,
      prefix = "tmp",
      target = "VAR_NAMES",
      also_not = "tmp_2"
    ),
    "tmp_3"
  )
})

test_that("util_ggplot_text builds a text-only ggplot", {
  plot <- util_ggplot_text(c("first line", "second line"))

  expect_s3_class(plot, "ggplot")
  expect_s3_class(plot$layers[[1]]$geom, "GeomText")
  expect_equal(plot$layers[[1]]$aes_params$label, "first line\nsecond line")
})

test_that("text-only plot helpers collapse text and strip ANSI escapes", {
  skip_if_not_installed("cli")
  skip_if_not_installed("plotly")

  plot <- util_ggplot_text(c(cli::col_red("red"), "plain"))
  expect_equal(plot$layers[[1]]$aes_params$label, "red\nplain")

  ply <- util_plotly_text(cli::col_blue("blue"))
  plotly_key <- ply$x$cur_data
  expect_s3_class(ply, "plotly")
  expect_equal(ply$x$attrs[[plotly_key]]$text, "blue")
  expect_false(ply$x$layoutAttrs[[plotly_key]]$xaxis$visible)
  expect_false(ply$x$layoutAttrs[[plotly_key]]$yaxis$visible)
  expect_false(ply$x$layoutAttrs[[plotly_key]]$showlegend)

  expect_error(util_plotly_text(c("blue", "plain")), "Need exactly one")
})

test_that("prep_list_voc returns bundled vocabulary names", {
  voc <- prep_list_voc()

  expect_type(voc, "character")
  expect_true("ICD10" %in% voc)
})

test_that("util_get_voc_tab merges user-provided bookmark tables", {
  cache <- new.env(parent = emptyenv())
  cache[["<>"]] <- data.frame(
    voc = "LOCAL",
    url = "https://example.test/voc"
  )

  voc_tab <- with_dataframe_environment(
    util_get_voc_tab(),
    env = cache
  )

  expect_equal(voc_tab$voc[[1]], "LOCAL")
  expect_equal(voc_tab$url[[1]], "https://example.test/voc")
  expect_true("ICD10" %in% voc_tab$voc)

  cache[["<>"]] <- data.frame(not_voc = "LOCAL")
  expect_warning(
    voc_tab <- with_dataframe_environment(util_get_voc_tab(), env = cache),
    "Invalid <>-Bookmark-Table"
  )
  expect_false("LOCAL" %in% voc_tab$voc)
})

test_that("util_paste_with_na keeps missing inputs missing", {
  expect_equal(
    util_paste_with_na(c("a", NA, "c"), c("1", "2", NA), sep = "-"),
    c("a-1", NA, NA)
  )
  expect_equal(
    util_paste0_with_na(c("a", NA, "c"), c("1", "2", NA)),
    c("a1", NA, NA)
  )
})

test_that("util_empty handles scalar and list-like text", {
  expect_equal(
    util_empty(c("", " ", NA, "x")),
    c(TRUE, TRUE, TRUE, FALSE)
  )
  expect_equal(
    util_empty(list(" ", NA_character_, "x")),
    c(TRUE, TRUE, FALSE)
  )
  expect_equal(
    util_empty(list(NULL, " ", c("x", "y"))),
    c(FALSE, TRUE, FALSE)
  )
})

test_that("tail-file helper and HTML completion check trailing bytes", {
  file <- tempfile(fileext = ".html")
  writeBin(charToRaw("<html><body>ok</body></html>"), file)

  expect_equal(util_tail_file(file, 7), "</html>")
  expect_equal(util_tail_file(file, 100), "<html><body>ok</body></html>")
  expect_equal(util_tail_file(file, 0), "")
  expect_true(util_is_html_file_complete(file))

  writeBin(charToRaw("<html><body>ok</body>\n</HTML>   "), file)
  expect_true(util_is_html_file_complete(file))

  writeBin(charToRaw("<html><body>incomplete"), file)
  expect_false(util_is_html_file_complete(file))

  expect_false(util_is_html_file_complete(file.path(tempdir(), "missing.html")))
})
