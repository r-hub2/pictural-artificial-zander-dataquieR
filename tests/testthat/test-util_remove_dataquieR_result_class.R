test_that(
  "util_remove_dataquieR_result_class strips result metadata recursively",
  {
    skip_on_cran()

    condition <- structure(
      list(message = "diagnostic"),
      class = c("simpleWarning", "warning", "condition")
    )
    nested <- structure(
      list(value = 1),
      class = c("dataquieR_result", "list", "custom_result")
    )
    attr(nested, "warning") <- list(condition)
    attr(nested, "message") <- list(condition)
    result <- structure(
      list(
        nested = nested,
        plain = "ok"
      ),
      class = c("dataquieR_result", "list")
    )
    attr(result, "error") <- list(condition)

    stripped <- util_remove_dataquieR_result_class(result)

    expect_false(inherits(stripped, "dataquieR_result"))
    expect_false(inherits(stripped$nested, "dataquieR_result"))
    expect_s3_class(stripped$nested, "custom_result")
    expect_null(attr(stripped, "error", exact = TRUE))
    expect_null(attr(stripped$nested, "warning", exact = TRUE))
    expect_null(attr(stripped$nested, "message", exact = TRUE))
    expect_identical(as.vector(stripped$nested$value), 1)
    expect_identical(as.vector(stripped$plain), "ok")
  }
)

test_that("util_remove_dataquieR_result_class unwraps pure result objects", {
  skip_on_cran()

  result <- structure(1, class = "dataquieR_result")
  attr(result, "message") <- list(simpleMessage("diagnostic"))

  stripped <- util_remove_dataquieR_result_class(result)

  expect_identical(stripped, 1)
  expect_identical(class(stripped), "numeric")
  expect_null(attr(stripped, "message", exact = TRUE))
})

test_that(
  "util_remove_dataquieR_result_class preserves plot-like list objects",
  {
    skip_on_cran()

    proxy <- structure(
      list(
        svg = charToRaw("<svg />"),
        child = structure(
          list(value = 1),
          class = c("dataquieR_result", "list")
        )
      ),
      class = c("svg_plot_proxy", "dataquieR_result", "list")
    )
    attr(proxy, "warning") <- list(simpleWarning("plot warning"))

    stripped <- util_remove_dataquieR_result_class(proxy)

    expect_s3_class(stripped, "svg_plot_proxy")
    expect_false(inherits(stripped, "dataquieR_result"))
    expect_true(inherits(stripped$child, "dataquieR_result"))
    expect_null(attr(stripped, "warning", exact = TRUE))
  }
)
