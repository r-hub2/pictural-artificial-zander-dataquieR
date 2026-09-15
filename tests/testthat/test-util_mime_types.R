skip_on_cran()

test_that("util_fetch_ext maps content types and filename headers", {
  plain <- with_mocked_bindings(
    curlGetHeaders = function(...) {
      c(
        "HTTP/2 200",
        "content-type: image/png; charset=binary",
        "content-disposition: attachment; filename=\"plot.png\""
      )
    },
    util_fetch_ext("https://example.invalid/plot"),
    .package = "base"
  )

  expect_identical(as.character(plain), "png")
  expect_identical(attr(plain, "file-name"), "plot.png")

  encoded <- with_mocked_bindings(
    curlGetHeaders = function(...) {
      c(
        "HTTP/2 200",
        "content-type: application/json",
        "content-disposition: attachment; filename*=UTF-8''data%20set.json"
      )
    },
    util_fetch_ext("https://example.invalid/data"),
    .package = "base"
  )

  expect_identical(as.character(encoded), "json")
  expect_identical(attr(encoded, "file-name"), "data set.json")
})

test_that("util_fetch_ext returns conditions for header failures", {
  no_content_type <- with_mocked_bindings(
    curlGetHeaders = function(...) "HTTP/2 200",
    util_fetch_ext("https://example.invalid/no-content-type"),
    .package = "base"
  )
  expect_s3_class(no_content_type, "condition")
  expect_match(conditionMessage(no_content_type), "No content-type")

  curl_error <- with_mocked_bindings(
    curlGetHeaders = function(...) stop("offline", call. = FALSE),
    util_fetch_ext("https://example.invalid/offline"),
    .package = "base"
  )
  expect_s3_class(curl_error, "condition")
  expect_match(conditionMessage(curl_error), "offline")
})

test_that("util_fetch_ext reports missing content type for local files", {
  skip_on_cran()

  file <- tempfile(fileext = ".csv")
  writeLines("a,b", file)

  fetched <- util_fetch_ext(paste0("file://", file))

  expect_s3_class(fetched, "error")
  expect_match(conditionMessage(fetched), "No content-type header found")
})

test_that("util_fetch_ext returns curl conditions for unsupported URLs", {
  skip_on_cran()

  fetched <- util_fetch_ext("data:text/plain,hello")

  expect_s3_class(fetched, "error")
})
