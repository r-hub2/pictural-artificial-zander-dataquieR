skip_on_cran()

test_that("util_create_page_file resumes complete HTML pages", {
  skip_on_cran()

  page_file <- tempfile(fileext = ".html")
  writeLines("<html><body>done</body></html>", page_file)

  page_name <- basename(page_file)
  pages <- list(htmltools::HTML("unused"))
  names(pages) <- page_name
  rendered_pages <- list(list(dependencies = list()))
  names(rendered_pages) <- page_name

  progress_values <- numeric()
  result <- withr::with_options(
    list(dataquieR.resume_print = TRUE),
    util_create_page_file(
      page_nr = 1,
      pages = pages,
      rendered_pages = rendered_pages,
      dir = dirname(page_file),
      template_file = "missing-template.html",
      report = list(),
      logo = NULL,
      loading = NULL,
      packageName = "dataquieR",
      deps = NULL,
      progress_msg = function(...) stop("should not render"),
      progress = function(value) progress_values <<- c(progress_values, value),
      title = "Title",
      by_report = FALSE
    )
  )

  expect_identical(result, page_file)
  expect_identical(progress_values, 100)
  expect_identical(readLines(page_file), "<html><body>done</body></html>")
})

test_that("util_create_page_file writes a page from rendered tags", {
  skip_on_cran()

  out_dir <- tempfile()
  dir.create(out_dir)
  template_file <- tempfile(fileext = ".html")
  writeLines(
    paste(
      "<!doctype html><html><head><title>{{ title }}</title></head>",
      "<body>{{ spage }}</body></html>"
    ),
    template_file
  )
  pages <- list(htmltools::HTML("<main>page</main>"))
  names(pages) <- "page.html"
  rendered_pages <- list(htmltools::renderTags(pages[[1]]))
  names(rendered_pages) <- names(pages)
  progress_state <- new.env(parent = emptyenv())
  progress_state$values <- numeric()
  progress_state$messages <- character()

  result <- util_create_page_file(
    page_nr = 1,
    pages = pages,
    rendered_pages = rendered_pages,
    dir = out_dir,
    template_file = template_file,
    report = structure(list(), title = "Report", subtitle = "Sub"),
    logo = NULL,
    loading = NULL,
    packageName = "dataquieR",
    deps = NULL,
    progress_msg = function(...) {
      progress_state$messages <- c(progress_state$messages, paste(...))
    },
    progress = function(value) {
      progress_state$values <- c(progress_state$values, value)
    },
    title = "Browser title",
    by_report = TRUE
  )

  expect_identical(result, file.path(out_dir, "page.html"))
  expect_identical(progress_state$values, 100)
  expect_match(progress_state$messages, "Writing")
  expect_match(paste(readLines(result), collapse = "\n"), "<main>page</main>",
    fixed = TRUE
  )
})

test_that("util_create_page_file muffles known Plotly attribute warnings", {
  skip_on_cran()

  out_dir <- tempfile()
  dir.create(out_dir)
  template_file <- tempfile(fileext = ".html")
  writeLines("<!doctype html><html><body>{{ spage }}</body></html>",
    template_file
  )
  page <- htmltools::tagFunction(function() {
    warning("'box' objects don't have these attributes: 'mode'")
    htmltools::div("ok")
  })
  pages <- list(page)
  names(pages) <- "page.html"
  rendered_pages <- list(list(html = page, dependencies = list()))
  names(rendered_pages) <- names(pages)

  expect_silent(
    result <- util_create_page_file(
      page_nr = 1,
      pages = pages,
      rendered_pages = rendered_pages,
      dir = out_dir,
      template_file = template_file,
      report = structure(list()),
      logo = NULL,
      loading = NULL,
      packageName = "dataquieR",
      deps = NULL,
      progress_msg = function(...) NULL,
      progress = function(...) NULL,
      title = "Browser title",
      by_report = FALSE
    )
  )
  expect_match(paste(readLines(result), collapse = "\n"), "<div>ok</div>",
    fixed = TRUE
  )
})

test_that("util_make_report_id returns unique report-safe identifiers", {
  ids <- replicate(5, util_make_report_id())

  expect_true(all(startsWith(ids, "dq-")))
  expect_true(all(grepl("-p[0-9]+-", ids)))
  expect_true(all(nchar(ids) > nchar("dq--p-")))
  expect_length(unique(ids), length(ids))
})
