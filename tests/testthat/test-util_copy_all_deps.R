test_that("util_copy_all_deps keeps jquery before plotly crosstalk and DT2", {
  skip_on_cran()
  skip_if_not_installed("DT2")
  skip_if_not_installed("plotly")

  tmp <- tempfile("deps")
  dir.create(tmp)

  page <- htmltools::tagList(
    suppressMessages(plotly::plot_ly(x = 1, y = 1)),
    util_html_table_dt2_widget(
      data.frame(a = 1),
      extensions = util_html_table_dt2_extensions(),
      options = list()
    )
  )

  deps <- suppressMessages(util_copy_all_deps(
    tmp,
    list(page),
    rmarkdown::html_dependency_jquery()
  )[["deps"]])
  rendered_deps <- paste(deps, collapse = "\n")

  jquery_pos <- regexpr("jquery", rendered_deps)[[1]]
  htmlwidgets_pos <- regexpr("htmlwidgets", rendered_deps)[[1]]
  plotly_pos <- regexpr("plotly-binding", rendered_deps)[[1]]
  crosstalk_pos <- regexpr("crosstalk", rendered_deps)[[1]]
  dt2_pos <- regexpr("dt2-binding", rendered_deps)[[1]]

  expect_gt(jquery_pos, 0)
  expect_gt(htmlwidgets_pos, 0)
  expect_gt(plotly_pos, 0)
  expect_gt(crosstalk_pos, 0)
  expect_gt(dt2_pos, 0)
  expect_lt(jquery_pos, htmlwidgets_pos)
  expect_lt(htmlwidgets_pos, plotly_pos)
  expect_lt(jquery_pos, crosstalk_pos)
  expect_lt(jquery_pos, dt2_pos)
})

test_that("util_copy_all_deps orders DT2 core before binding and extensions", {
  skip_on_cran()

  tmp <- tempfile("deps")
  dir.create(tmp)
  core_dir <- tempfile("datatables-core")
  fixed_header_dir <- tempfile("dt-fixedheader")
  dir.create(core_dir)
  dir.create(fixed_header_dir)
  writeLines("window.DataTable = {};", file.path(core_dir, "core.js"))
  writeLines(
    "window.DataTable.FixedHeader = {};",
    file.path(fixed_header_dir, "fixed-header.js")
  )

  core <- htmltools::htmlDependency(
    name = "datatables-core-js",
    version = "2.3.4",
    src = c(file = core_dir),
    script = "core.js"
  )
  fixed_header <- htmltools::htmlDependency(
    name = "dt-fixedheader-js",
    version = "4.0.3",
    src = c(file = fixed_header_dir),
    script = "fixed-header.js"
  )
  binding <- htmltools::htmlDependency(
    name = "dt2-binding",
    version = "0.1.1",
    src = c(file = fixed_header_dir),
    script = "fixed-header.js"
  )
  pages <- list(
    extension_first = htmltools::tagList(
      binding,
      fixed_header,
      htmltools::div()
    ),
    core_later = htmltools::tagList(core, htmltools::div())
  )

  deps <- util_copy_all_deps(tmp, pages)$deps
  rendered_deps <- paste(deps, collapse = "\n")

  expect_lt(
    regexpr("datatables-core-js", rendered_deps)[[1]],
    regexpr("dt-fixedheader-js", rendered_deps)[[1]]
  )
  expect_lt(
    regexpr("datatables-core-js", rendered_deps)[[1]],
    regexpr("dt2-binding", rendered_deps)[[1]]
  )
})

test_that("util_copy_all_deps orders DT core before extensions", {
  skip_on_cran()

  tmp <- tempfile("deps")
  dir.create(tmp)
  core_dir <- tempfile("dt-core")
  fixed_header_dir <- tempfile("dt-fixedheader")
  dir.create(core_dir)
  dir.create(fixed_header_dir)
  writeLines("window.jQuery.fn.dataTable = {};", file.path(core_dir, "core.js"))
  writeLines(
    "window.jQuery.fn.dataTable.FixedHeader = {};",
    file.path(fixed_header_dir, "fixed-header.js")
  )

  core <- htmltools::htmlDependency(
    name = "dt-core",
    version = "1.13.6",
    src = c(file = core_dir),
    script = "core.js"
  )
  fixed_header <- htmltools::htmlDependency(
    name = "dt-ext-fixedheader",
    version = "1.13.6",
    src = c(file = fixed_header_dir),
    script = "fixed-header.js"
  )

  deps <- util_copy_all_deps(
    tmp,
    list(
      extension_first = htmltools::tagList(fixed_header, htmltools::div()),
      core_later = htmltools::tagList(core, htmltools::div())
    )
  )$deps
  rendered_deps <- paste(deps, collapse = "\n")

  expect_lt(
    regexpr("dt-core", rendered_deps)[[1]],
    regexpr("dt-ext-fixedheader", rendered_deps)[[1]]
  )
})

test_that("util_copy_all_deps muffles known Plotly attribute warnings", {
  skip_on_cran()

  tmp <- tempfile("deps")
  dir.create(tmp)
  dep_dir <- tempfile("depdir")
  dir.create(dep_dir)
  writeLines("console.log('ok');", file.path(dep_dir, "x.js"))

  page <- htmltools::tagFunction(function() {
    warning("'bar' objects don't have these attributes: 'mode'")
    htmltools::div("ok")
  })
  dep <- htmltools::htmlDependency(
    name = "xdep",
    version = "1.0.0",
    src = c(file = dep_dir),
    script = "x.js"
  )

  expect_silent(
    deps <- util_copy_all_deps(tmp, list(page), dep)
  )
  expect_match(deps$deps, "xdep-1.0.0/x.js", fixed = TRUE)
})

test_that("util_copy_all_deps renders dependency paths with forward slashes", {
  skip_on_cran()

  tmp <- tempfile("deps")
  dir.create(tmp)

  deps <- util_copy_all_deps(
    tmp,
    list(htmltools::div("ok")),
    rmarkdown::html_dependency_jquery()
  )$deps

  expect_false(grepl("\\\\", deps, fixed = TRUE))
})

test_that("util_copy_all_deps removes external font references from CSS", {
  skip_on_cran()

  tmp <- tempfile("deps")
  dir.create(tmp)
  dep_dir <- tempfile("depdir")
  dir.create(dep_dir)
  writeLines(c(
    "@import url('https://fonts.googleapis.com/css2?family=Jost');",
    ".dt-container {",
    "  font-family: 'Jost', system-ui, sans-serif;",
    "}"
  ), file.path(dep_dir, "font.css"))

  dep <- htmltools::htmlDependency(
    name = "fontdep",
    version = "1.0.0",
    src = c(file = dep_dir),
    stylesheet = "font.css"
  )

  deps <- util_copy_all_deps(tmp, list(htmltools::div("ok")), dep)$deps
  copied_css <- readLines(file.path(
    tmp, "lib", "fontdep-1.0.0", "font.css"
  ))

  expect_match(deps, "fontdep-1.0.0/font.css", fixed = TRUE)
  expect_false(any(grepl("fonts.googleapis.com", copied_css, fixed = TRUE)))
  expect_false(any(grepl("fonts.gstatic.com", copied_css, fixed = TRUE)))
  expect_false(any(grepl("Jost", copied_css, fixed = TRUE)))
  expect_true(any(grepl("system-ui, sans-serif", copied_css, fixed = TRUE)))
})

test_that("util_copy_all_deps reuses already copied dependencies", {
  skip_on_cran()

  tmp <- tempfile("deps")
  dir.create(tmp)
  dep_dir <- tempfile("depdir")
  dir.create(dep_dir)
  writeLines("console.log('ok');", file.path(dep_dir, "x.js"))

  dep <- htmltools::htmlDependency(
    name = "xdep",
    version = "1.0.0",
    src = c(file = dep_dir),
    script = "x.js"
  )
  page <- htmltools::tagList(dep, htmltools::div("ok"))

  first <- util_copy_all_deps(tmp, list(page))
  reused <- util_copy_all_deps(
    tmp,
    list(page),
    copy_dependencies = FALSE
  )

  expect_true(file.exists(file.path(tmp, "lib", "xdep-1.0.0", "x.js")))
  expect_identical(reused$deps, first$deps)
})
