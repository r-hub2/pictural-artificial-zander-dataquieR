skip_on_cran()

test_that("prep_guess_encoding handles simple text without stringi", {
  ascii_text <- c("abc", "def")

  ascii_guess <- with_mocked_bindings(
    .package = "base",
    requireNamespace = function(package, ...) {
      if (identical(package, "stringi")) {
        return(FALSE)
      }
      TRUE
    },
    prep_guess_encoding(ascii_text)
  )

  expect_equal(ascii_guess$encoding, "ASCII")
  expect_equal(ascii_guess$confidence, 1)

  unknown_guess <- with_mocked_bindings(
    .package = "base",
    requireNamespace = function(package, ...) {
      if (identical(package, "stringi")) {
        return(FALSE)
      }
      TRUE
    },
    prep_guess_encoding("ä")
  )

  expect_equal(unknown_guess$encoding, "unknown")
  expect_equal(unknown_guess$confidence, 1)
})

test_that("prep_guess_encoding handles files and empty text conservatively", {
  skip_on_cran()

  ascii_file <- withr::local_tempfile(fileext = ".txt")
  writeBin(charToRaw("plain ASCII\n"), ascii_file)

  file_guess <- with_mocked_bindings(
    .package = "base",
    requireNamespace = function(package, ...) {
      if (identical(package, "stringi")) {
        return(FALSE)
      }
      TRUE
    },
    prep_guess_encoding(file = ascii_file)
  )

  expect_equal(file_guess$encoding, "ASCII")
  expect_equal(file_guess$confidence, 1)

  empty_guess <- prep_guess_encoding(NA_character_)

  expect_equal(empty_guess$encoding, "unknown")
  expect_equal(empty_guess$confidence, 1)
})

test_that("encoding helpers cover argument and reference edge cases", {
  skip_on_cran()

  expect_error(
    prep_guess_encoding(),
    "both nor none"
  )
  expect_error(
    prep_guess_encoding("x", file = tempfile()),
    "both nor none"
  )
  expect_false(util_all_ascii_strings(character(0)))

  dt <- data.frame(
    txt = c("a", "b", NA_character_),
    stringsAsFactors = FALSE
  )
  verified <- util_verify_encoding(dt, ref_encs = c(txt = "unknown"))

  expect_length(verified, 0)
  enc <- attr(verified, "cols_enc", exact = TRUE)
  expect_named(enc, "txt")
  expect_true(any(enc$txt$encoding %in% c("ASCII", "UTF-8")))
})

test_that(
  "util_guess_encoding_without_stringi classifies file bytes conservatively",
  {
    skip_on_cran()

    ascii_file <- withr::local_tempfile(fileext = ".txt")
    writeBin(charToRaw("plain ASCII\n"), ascii_file)

    ascii_guess <- util_guess_encoding_without_stringi(file = ascii_file)
    expect_identical(as.character(ascii_guess$encoding), "ASCII")
    expect_identical(ascii_guess$confidence, 1)

    missing_guess <- util_guess_encoding_without_stringi(
      file = file.path(tempdir(), "not-present.txt")
    )
    expect_identical(as.character(missing_guess$encoding), "unknown")

    non_ascii_file <- withr::local_tempfile(fileext = ".txt")
    writeBin(as.raw(c(0xc3, 0xa4)), non_ascii_file)
    non_ascii_guess <- util_guess_encoding_without_stringi(
      file = non_ascii_file
    )
    expect_identical(as.character(non_ascii_guess$encoding), "unknown")
  }
)

test_that("int_encoding_errors accepts ASCII columns without stringi", {
  study_data <- data.frame(txt = c("a", "b", "c"), stringsAsFactors = FALSE)

  result <- with_mocked_bindings(
    .package = "base",
    requireNamespace = function(package, ...) {
      if (identical(package, "stringi")) {
        return(FALSE)
      }
      TRUE
    },
    suppressWarnings(suppressMessages(int_encoding_errors(
      resp_vars = "txt",
      study_data = study_data,
      ref_encs = c(txt = "UTF-8")
    )))
  )

  expect_equal(as.vector(result$SummaryTable$NUM_int_uenc), 0)
  expect_equal(as.vector(result$SummaryTable$`Guessed Encoding`), "ASCII = 1")
})
