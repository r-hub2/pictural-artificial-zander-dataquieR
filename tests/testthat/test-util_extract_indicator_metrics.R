test_that("util_extract_indicator_metrics keeps recognized metric columns", {
  skip_on_cran()

  table <- data.frame(
    VAR_NAMES = "x",
    PCT_com_qum_nonresp = 50,
    NUM_com_qum_nonresp = 2,
    OTHER = "ignored",
    check.names = FALSE
  )

  metrics <- util_extract_indicator_metrics(table)

  expect_equal(
    colnames(metrics),
    c("PCT_com_qum_nonresp", "NUM_com_qum_nonresp")
  )
  expect_equal(metrics$PCT_com_qum_nonresp, 50)
  expect_equal(metrics$NUM_com_qum_nonresp, 2)
})

test_that(
  "util_extract_indicator_metrics returns empty tables without matches",
  {
    skip_on_cran()

    table <- data.frame(
      VAR_NAMES = "x",
      OTHER = "ignored",
      stringsAsFactors = FALSE
    )

    metrics <- util_extract_indicator_metrics(table)

    expect_s3_class(metrics, "data.frame")
    expect_equal(ncol(metrics), 0L)
    expect_equal(nrow(metrics), 1L)
  }
)
