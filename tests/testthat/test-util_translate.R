skip_on_cran()

test_that("util_translate keeps keys and translated values accessible", {
  translated <- util_translate("VAR_NAMES", ns = "dashboard_table")

  expect_s3_class(translated, "dataquieR_translated")
  expect_identical(names(translated), "VAR_NAMES")
  expect_identical(as.character(translated), "Variable Names")
  expect_true(prep_is_translated(translated))
  expect_output(print(translated), "Variable Names")
})

test_that("util_translate can reverse and reuse translation metadata", {
  translated <- util_translate("VAR_NAMES", ns = "dashboard_table")

  expect_identical(
    as.character(util_translate("Variable Names",
        ns = "dashboard_table",
        reverse = TRUE)),
    "VAR_NAMES"
  )
  expect_identical(
    as.character(util_translate("LABEL",
        as_this_translation = translated)),
    "Variable Label"
  )
  expect_identical(
    as.character(util_translate("unknown-key", ns = "dashboard_table")),
    "unknown-key"
  )
  expect_error(
    util_translate("LABEL",
      ns = "dashboard_table",
      as_this_translation = translated),
    "not both"
  )
})

test_that(
  "translated names can be assigned only through the stable helper path",
  {
    translated <- util_translate("VAR_NAMES", ns = "dashboard_table")

    renamed <- setNames(translated, translated)
    expect_identical(as.character(renamed), "Variable Names")
    expect_s3_class(names(renamed), "dataquieR_translated")
    expect_identical(as.character(names(renamed)), "Variable Names")
    expect_error(names(translated) <- "changed", "cannot change")

    data <- data.frame(VAR_NAMES = "x")
    util_translated_colnames(data) <- translated
    expect_s3_class(names(data), "dataquieR_translated")
    expect_identical(as.character(names(data)), "Variable Names")

    matrix_data <- matrix(1, ncol = 1)
    colnames(matrix_data) <- "VAR_NAMES"
    util_translated_colnames(matrix_data) <- translated
    expect_s3_class(colnames(matrix_data), "dataquieR_translated")
    expect_identical(as.character(colnames(matrix_data)), "Variable Names")
    expect_identical(util_untranslated_colnames(matrix_data), "VAR_NAMES")
  }
)

test_that("translated values expose JSON-ready translated labels", {
  skip_if_not_installed("jsonlite")

  translated <- util_translate("VAR_NAMES", ns = "dashboard_table")

  expect_identical(
    jsonlite::toJSON(translated),
    jsonlite::toJSON("Variable Names")
  )
})

test_that("translation helpers report duplicate and unavailable JSON paths", {
  translated <- util_translate("VAR_NAMES", ns = "dashboard_table")

  testthat::local_mocked_bindings(
    util_get_concept_info = function(...) {
      data.frame(
        namespace = "dashboard_table",
        key = "VAR_NAMES",
        lang = getOption("dataquieR.lang", dataquieR.lang_default),
        translation = c("Variable Names", "Variable Names duplicated")
      )
    },
    util_error = function(...) {
      stop(sprintf(...), call. = FALSE)
    }
  )

  expect_error(
    util_translate("VAR_NAMES", ns = "dashboard_table"),
    ">1 translation"
  )

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) FALSE
  )

  expect_error(
    asJSON.dataquieR_translated(translated),
    "Should not be reached"
  )
})
