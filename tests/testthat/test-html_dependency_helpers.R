test_that("HTML dependency helpers describe bundled assets", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  clipboard <- html_dependency_clipboard()
  expect_s3_class(clipboard, "html_dependency")
  expect_equal(clipboard$name, "clipboard")
  expect_equal(clipboard$version, "2.0.11")
  expect_equal(clipboard$script, "clipboard.min.js")

  tippy <- html_dependency_tippy()
  expect_s3_class(tippy, "html_dependency")
  expect_equal(tippy$name, "tippy")
  expect_equal(tippy$version, "6.7.3")
  expect_equal(tippy$script, c("core.js", "tippy.js"))

  report <- html_dependency_report_dt()
  expect_s3_class(report, "html_dependency")
  expect_equal(report$name, "report-dt-style")
  expect_equal(report$stylesheet, "report-dt-style.css")
  expect_equal(report$script, "report_dt.js")

  vertical <- html_dependency_vert_dt()
  expect_s3_class(vertical, "html_dependency")
  expect_equal(vertical$name, "vertical-dt-style")
  expect_equal(vertical$stylesheet, "vertical-dt-style.css")
  expect_equal(vertical$script, "sort_heatmap_dt.js")
})

test_that("html_dependency_jspdf uses dummy fallback without visNetwork", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) FALSE
  )

  dep <- html_dependency_jspdf()
  expect_s3_class(dep, "html_dependency")
  expect_equal(dep$name, "dummy")
  expect_equal(dep$version, "0.0.1")
})

test_that("html_dependency_jspdf describes the visNetwork jsPDF asset", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) TRUE
  )

  dep <- html_dependency_jspdf()
  expect_s3_class(dep, "html_dependency")
  expect_equal(dep$name, "jspdf")
  expect_equal(dep$version, "1.3.2")
  expect_equal(dep$script, "jspdf.debug.js")
})

test_that("util_html_table_js marks JavaScript for htmlwidgets", {
  skip_on_cran()
  skip_if_not_installed("htmlwidgets")

  js <- util_html_table_js(
    "return 1;",
    "return 2;"
  )

  expect_s3_class(js, "JS_EVAL")
  expect_match(as.character(js), "return 1;", fixed = TRUE)
  expect_match(as.character(js), "return 2;", fixed = TRUE)
})
