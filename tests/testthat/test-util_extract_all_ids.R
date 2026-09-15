test_that("util_extract_all_ids collects page and anchor targets", {
  skip_on_cran()

  pages <- list(
    "index.html" = list(
      children = list(
        list(attribs = list(id = "top")),
        list(children = list(
          list(attribs = list(id = "details")),
          "plain text"
        ))
      )
    ),
    "other.html" = list(
      attribs = list(id = "single")
    )
  )

  expect_equal(
    util_extract_all_ids(pages),
    c("report.html", "index.html", "other.html", "index.html#top",
      "index.html#details", "other.html#single")
  )
})

test_that("util_extract_all_ids handles pages without ids", {
  skip_on_cran()

  pages <- list(
    "empty.html" = list(children = list("plain text"))
  )

  expect_equal(util_extract_all_ids(pages), "report.html")
})
