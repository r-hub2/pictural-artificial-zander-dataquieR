skip_on_cran()

test_that("util_extract_matches returns all regex matches per element", {
  x <- c(
    "values Ref. 12 and Ref. 13",
    "nothing here",
    NA_character_
  )

  matches <- util_extract_matches(x, "Ref\\. [0-9]+")

  expect_equal(matches[[1]], c("Ref. 12", "Ref. 13"))
  expect_null(matches[[2]])
  expect_true(is.na(matches[[3]]))
})

test_that("util_col2rgb normalizes named, hex, and space-separated colors", {
  expect_equal(
    util_col2rgb(c("red", "#00ff00", "0 0 255", "255 255 255 128")),
    c("#ff0000ff", "#00ff00ff", "#0000ff", "#ffffff80")
  )

  expect_warning(
    expect_warning(
      expect_equal(
        util_col2rgb(c("not-a-color", "1 bad 3")),
        c("#000000ff", "#000000")
      ),
      "No known colors"
    ),
    "No known colors"
  )
})

test_that(
  "util_duplicated_inclding_first flags all members of duplicate sets",
  {
    expect_equal(
      util_duplicated_inclding_first(c("a", "b", "a", "c")),
      c(TRUE, FALSE, TRUE, FALSE)
    )
    expect_equal(
      util_duplicated_inclding_first(data.frame(
        x = c(1, 1, 2), y = c("a", "a", "b")
      )),
      c(TRUE, TRUE, FALSE)
    )
    expect_error(util_duplicated_inclding_first(list(a = 1)), "unsupported")
  }
)

test_that("file tail helpers detect complete HTML endings", {
  skip_on_cran()

  html_file <- tempfile(fileext = ".html")
  incomplete_file <- tempfile(fileext = ".html")
  missing_file <- tempfile(fileext = ".html")

  writeLines(
    c("<html>", "<body>content</body>", "</html>"),
    html_file
  )
  writeLines(
    c("<html>", "<body>content</body>"),
    incomplete_file
  )

  expect_identical(util_tail_file(html_file, 7L), "/html>\n")
  expect_true(util_is_html_file_complete(html_file))
  expect_false(util_is_html_file_complete(incomplete_file))
  expect_false(util_is_html_file_complete(missing_file))
})

test_that("util_is_integer checks values rather than storage mode", {
  expect_identical(
    util_is_integer(c(1, 1.5, NA_real_, NaN, Inf)),
    c(TRUE, FALSE, TRUE, FALSE, FALSE)
  )
  expect_identical(util_is_integer(c("1", "2")), c(FALSE, FALSE))
})

test_that("util_ensure_in warns, errors, and returns accepted values", {
  allowed <- c("alpha", "beta", "gamma")

  expect_warning(
    accepted <- util_ensure_in(c("alpha", "alfa", "gamma"), allowed),
    "Missing .alfa. from"
  )
  expect_identical(accepted, c("alpha", "gamma"))

  expect_error(
    util_ensure_in("alfa", allowed, error = TRUE),
    "Missing .alfa. from"
  )
})

test_that("util_verify_names separates typo proposals from unknown names", {
  skip_if_not_installed("stringdist")

  verified <- .util_verify_names(
    standard_names = "study_data",
    observed_names = c("study_dat", "unexpected_table"),
    name_of_study_data = character(0)
  )

  expect_identical(verified$warn, c(study_dat = "study_data"))
  expect_identical(verified$warn2, "study_dat")
  expect_identical(verified$dontknow, "unexpected_table")
  expect_true("study_data" %in% rownames(verified$case_sensitive))
  expect_identical(
    colnames(verified$case_sensitive),
    c("study_dat", "unexpected_table")
  )
})

test_that(
  "util_verify_names forwards typo warnings and unknown-name messages",
  {
    testthat::local_mocked_bindings(
      .util_verify_names = function(...) {
        list(
          warn = c(study_dat = "study_data"),
          warn2 = "study_dat",
          dontknow = "unexpected_table"
        )
      }
    )

    expect_warning(
      expect_message(
        util_verify_names(),
        "unexpected_table"
      ),
      "study_dat"
    )
  }
)

test_that("quote helpers use plain ASCII quotes", {
  withr::local_options(useFancyQuotes = TRUE)

  expect_equal(
    as.character(util_set_dQuoteString(c("a", "b c"))),
    c("\"a\"", "\"b c\"")
  )
  expect_equal(
    as.character(util_set_sQuoteString(c("a", "b c"))),
    c("'a'", "'b c'")
  )
})

test_that("util_free_varname skips existing and explicitly reserved names", {
  meta_data <- data.frame(VAR_NAMES = c("tmp", "tmp_1", "other"))

  expect_identical(
    util_free_varname(meta_data, "tmp", VAR_NAMES, also_not = "tmp_2"),
    "tmp_3"
  )
  expect_identical(
    util_free_varname(meta_data, "fresh", VAR_NAMES, also_not = character()),
    "fresh"
  )
})

test_that("paste helpers propagate missing values row-wise", {
  expect_identical(
    util_paste_with_na(c("a", NA, "c"), c("x", "y", NA), sep = "-"),
    c("a-x", NA_character_, NA_character_)
  )
  expect_identical(
    util_paste0_with_na("id_", c("a", NA, "c")),
    c("id_a", NA_character_, "id_c")
  )
})
