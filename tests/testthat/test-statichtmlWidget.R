skip_on_cran()

test_that("statichtmlWidget works", {
  skip_if_not_installed("htmlwidgets")

  shw <- statichtmlWidget(htmltools::HTML("<div>Hello!!</div>"),
    width = 100,
    height = 20,
    elementId = "asdf",
    sizingPolicy =
      htmlwidgets::sizingPolicy(),
    js = "alert('Hello!')"
  )
  tmpdir <- withr::local_tempdir()
  tmpfil <- file.path(tmpdir, "index.html")
  htmltools::save_html(shw, tmpfil)
  expect_true(file.exists(tmpfil))
  expect_gt(file.size(tmpfil), 620)
  expect_true(dir.exists(file.path(tmpdir, "lib")))
  expect_true(any(grepl('id="asdf"',
        fixed = TRUE,
        x = readLines(tmpfil)
      )))
  expect_true(any(grepl("<div>Hello!!<\\/div>",
        fixed = TRUE,
        x = readLines(tmpfil)
      )))
  expect_true(any(grepl("alert('Hello!')",
        fixed = TRUE,
        x = readLines(tmpfil)
      )))
})

test_that("statichtmlWidget accepts its default JavaScript payload", {
  skip_if_not_installed("htmlwidgets")

  expect_s3_class(
    statichtmlWidget(htmltools::HTML("<span>default</span>")),
    "htmlwidget"
  )
})
