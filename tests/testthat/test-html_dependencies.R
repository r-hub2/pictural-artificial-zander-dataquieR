test_that(
  "legacy DT dependency aliases return the underlying html dependencies",
  {
    skip_on_cran()
    skip_if_not_installed("htmltools")

    report_dep <- html_dependency_report_dt()
    vert_dep <- html_dependency_vert_dt()

    expect_s3_class(report_dep, "html_dependency")
    expect_s3_class(vert_dep, "html_dependency")
    expect_equal(report_dep$name, "report-dt-style")
    expect_equal(report_dep$script, "report_dt.js")
    expect_equal(vert_dep$name, "vertical-dt-style")
    expect_equal(vert_dep$script, "sort_heatmap_dt.js")
  }
)
