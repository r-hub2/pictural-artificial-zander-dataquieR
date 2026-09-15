test_that("util_df_escape escapes data frames and preserves column metadata", {
  skip_on_cran()

  x <- data.frame(
    text = "<b>bold & raw</b>",
    number = 7,
    stringsAsFactors = FALSE
  )
  attr(x$text, "plain_label") <- "Plain text"
  attr(x$text, DATA_TYPE) <- DATA_TYPES$STRING
  attr(x$number, DATA_TYPE) <- DATA_TYPES$INTEGER

  escaped <- util_df_escape(x)

  expect_s3_class(escaped, "data.frame")
  expect_true(util_attr(escaped, "is_html_escaped", exact = TRUE))
  expect_identical(as.vector(escaped$text),
    "&lt;b&gt;bold &amp; raw&lt;/b&gt;")
  expect_identical(as.vector(escaped$number), "7")
  expect_identical(attr(escaped$text, "plain_label", exact = TRUE),
    "Plain text")
  expect_identical(attr(escaped$text, DATA_TYPE, exact = TRUE),
    DATA_TYPES$STRING)
  expect_identical(attr(escaped$number, DATA_TYPE, exact = TRUE),
    DATA_TYPES$INTEGER)
})

test_that("util_df_escape returns already escaped data frames unchanged", {
  skip_on_cran()

  x <- data.frame(text = "&lt;b&gt;", stringsAsFactors = FALSE)
  attr(x, "is_html_escaped") <- TRUE

  expect_identical(util_df_escape(x), x)
})

test_that("util_df_escape preserves raw variable values for links", {
  skip_on_cran()

  x <- data.frame(
    Variables = c("Higher > 30", "A & B"),
    stringsAsFactors = FALSE
  )

  escaped <- util_df_escape(x)

  expect_identical(
    util_attr(escaped$Variables, "plain_label", exact = TRUE),
    x$Variables
  )
  expect_identical(
    as.vector(escaped$Variables),
    c("Higher &gt; 30", "A &amp; B")
  )
})
