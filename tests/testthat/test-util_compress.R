skip_on_cran()

test_that("util_compress roundtrips supported R objects", {
  x <- list(a = 1:3, b = data.frame(y = c("a", "b")))

  compressed <- util_compress(x, algo = "none")

  expect_s3_class(compressed, "compressed")
  expect_equal(util_attr(compressed, "method", exact = TRUE), "memCompress")
  expect_equal(util_attr(compressed, "algo", exact = TRUE), "none")
  expect_equal(util_decompress(compressed), x)
})

test_that("util_compress chooses a supported default algorithm", {
  skip_on_cran()

  x <- list(value = 1:3)
  compressed <- util_compress(x)

  expect_s3_class(compressed, "compressed")
  expect_true(
    util_attr(compressed, "algo", exact = TRUE) %in% MEM_COMPRESS_CAPABILITIES
  )
  expect_equal(util_decompress(compressed), x)
})

test_that("util_decompress uses the default algorithm for old objects", {
  skip_on_cran()

  x <- list(value = "legacy")
  compressed <- util_compress(x, algo = head(MEM_COMPRESS_CAPABILITIES, 1))
  attr(compressed, "algo") <- NULL

  expect_equal(util_decompress(compressed), x)
})

test_that("util_decompress rejects unsupported objects", {
  expect_error(
    util_decompress(charToRaw("not compressed")),
    "unsupported"
  )
})

test_that("compression capability probing reports supported algorithms", {
  skip_on_cran()

  capabilities <- .util_mem_compress_capabilities(c("none", "not-an-algo"))

  expect_identical(names(capabilities), c("none", "not-an-algo"))
  expect_true(capabilities[["none"]])
  expect_false(capabilities[["not-an-algo"]])
})
