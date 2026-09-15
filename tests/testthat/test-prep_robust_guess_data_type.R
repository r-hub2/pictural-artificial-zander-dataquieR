test_that(
  "prep_robust_guess_data_type maps parser guesses to dataquieR types",
  {
    skip_on_cran()
    skip_if_not_installed("readr")

    expect_identical(
      prep_robust_guess_data_type(c("", " ", NA_character_), k = 2, it = 5),
      DATA_TYPES$INTEGER
    )

    integer_guess <- prep_robust_guess_data_type(c("1", "2", "3"),
      k = 2, it = 5
    )
    expect_identical(as.vector(integer_guess), DATA_TYPES$INTEGER)
    expect_identical(attr(integer_guess, "orig_type", exact = TRUE), "integer")

    float_guess <- prep_robust_guess_data_type(c("1.2", "3.4", "5.6"),
      k = 2, it = 5
    )
    expect_identical(as.vector(float_guess), DATA_TYPES$FLOAT)
    expect_identical(attr(float_guess, "orig_type", exact = TRUE), "double")

    time_guess <- prep_robust_guess_data_type(c("12:34:00", "01:02:03"),
      k = 2, it = 5
    )
    expect_identical(as.vector(time_guess), DATA_TYPES$TIME)
    expect_identical(attr(time_guess, "orig_type", exact = TRUE), "time")

    date_guess <- prep_robust_guess_data_type(c("2020-01-01", "2020-02-02"),
      k = 2, it = 5
    )
    expect_identical(as.vector(date_guess), DATA_TYPES$DATETIME)
    expect_identical(attr(date_guess, "orig_type", exact = TRUE), "date")

    datetime_guess <- prep_robust_guess_data_type(
      c("2020-01-01 12:13:14", "2020-02-02 01:02:03"),
      k = 2,
      it = 5
    )
    expect_identical(as.vector(datetime_guess), DATA_TYPES$DATETIME)
    expect_identical(
      attr(datetime_guess, "orig_type", exact = TRUE),
      "datetime"
    )

    character_guess <- prep_robust_guess_data_type(c("alpha", "beta"),
      k = 2, it = 5
    )
    expect_identical(as.vector(character_guess), DATA_TYPES$STRING)
    expect_identical(
      attr(character_guess, "orig_type", exact = TRUE),
      "character"
    )

    logical_guess <- prep_robust_guess_data_type(c("TRUE", "FALSE"),
      k = 2, it = 5
    )
    expect_identical(as.vector(logical_guess), DATA_TYPES$INTEGER)
    expect_identical(attr(logical_guess, "orig_type", exact = TRUE), "logical")

    number_guess <- prep_robust_guess_data_type(c("1,234", "5,678"),
      k = 2, it = 5
    )
    expect_identical(as.vector(number_guess), DATA_TYPES$FLOAT)
    expect_identical(attr(number_guess, "orig_type", exact = TRUE), "number")
  }
)

test_that("prep_robust_guess_data_type rejects unsupported inputs", {
  skip_on_cran()
  skip_if_not_installed("readr")

  expect_error(
    prep_robust_guess_data_type(c(NA_character_, NA_character_)),
    "cannot be NA only"
  )
  expect_error(
    prep_robust_guess_data_type(1:3),
    "Need a character vector"
  )
})

test_that("encoding helpers handle ASCII and unknown fallbacks", {
  skip_on_cran()

  ascii_file <- tempfile("ascii")
  writeBin(charToRaw("plain ascii"), ascii_file)
  binary_file <- tempfile("binary")
  writeBin(as.raw(c(0xff, 0xfe)), binary_file)

  expect_false(util_all_ascii_strings(character(0)))
  expect_true(util_all_ascii_strings(c("abc", "123")))
  expect_false(util_all_ascii_strings("ä"))
  expect_equal(
    util_guess_encoding_without_stringi(file = ascii_file),
    data.frame(encoding = "ASCII", confidence = 1)
  )
  expect_equal(
    util_guess_encoding_without_stringi(file = binary_file),
    util_unknown_encoding_guess()
  )
  expect_equal(
    util_guess_encoding_without_stringi(file = file.path(tempdir(), "absent")),
    util_unknown_encoding_guess()
  )
  expect_equal(
    util_guess_encoding_without_stringi(x = "abc"),
    data.frame(encoding = "ASCII", confidence = 1)
  )
  expect_equal(
    util_guess_encoding_without_stringi(x = "ä"),
    util_unknown_encoding_guess()
  )
})

test_that("prep_guess_encoding validates arguments and uses fallback paths", {
  skip_on_cran()

  ascii_file <- tempfile("ascii")
  writeBin(charToRaw("plain ascii"), ascii_file)

  expect_error(
    prep_guess_encoding(),
    "Can neither have both nor none"
  )
  expect_error(
    prep_guess_encoding(x = "abc", file = ascii_file),
    "Can neither have both nor none"
  )

  testthat::local_mocked_bindings(
    util_stringi_available = function() FALSE
  )

  expect_equal(
    prep_guess_encoding(file = ascii_file),
    data.frame(encoding = "ASCII", confidence = 1)
  )
  expect_equal(
    prep_guess_encoding(x = c("abc", NA_character_)),
    data.frame(encoding = "ASCII", confidence = 1)
  )
})

test_that("encoding verification reports unclear columns against references", {
  skip_on_cran()

  invalid_byte <- rawToChar(as.raw(0xff))
  Encoding(invalid_byte) <- "UTF-8"
  encoded_data <- data.frame(
    clear = c("abc", "def"),
    unclear = c(invalid_byte, NA_character_),
    stringsAsFactors = FALSE
  )

  inferred <- util_verify_encoding(encoded_data)
  expect_length(inferred, 0)
  expect_equal(
    attr(inferred, "cols_enc", exact = TRUE)$unclear,
    data.frame(encoding = "unknown", confidence = 1)
  )

  accepted_unknown <- util_verify_encoding(
    encoded_data,
    ref_encs = c(unclear = "unknown")
  )
  expect_length(accepted_unknown, 0)

  utf8_mismatches <- util_verify_encoding(
    encoded_data,
    ref_encs = c(unclear = "UTF-8")
  )
  expect_equal(as.integer(utf8_mismatches$unclear), 1L)
  expect_equal(
    attr(utf8_mismatches$unclear, "ref_enc", exact = TRUE),
    "UTF-8"
  )
  expect_equal(
    attr(utf8_mismatches$unclear, "act_enc", exact = TRUE)$`1`,
    data.frame(encoding = "unknown", confidence = 1)
  )
})

test_that("prep_guess_encoding handles invalid character vectors", {
  skip_on_cran()

  invalid_byte <- rawToChar(as.raw(0xff))

  expect_equal(
    prep_guess_encoding(invalid_byte),
    data.frame(encoding = "unknown", confidence = 1)
  )
})
