test_that("result message helpers cover descriptor and formatting branches", {
  skip_on_cran()

  assign("zz_test_indicator", TRUE, envir = .indicator_or_descriptor)
  assign("zz_test_descriptor", FALSE, envir = .indicator_or_descriptor)
  withr::defer({
    rm("zz_test_indicator", envir = .indicator_or_descriptor)
    rm("zz_test_descriptor", envir = .indicator_or_descriptor)
  })

  indicator <- structure(
    list(),
    class = "dataquieR_result",
    function_name = "zz_test_indicator"
  )
  descriptor <- structure(
    list(),
    class = "dataquieR_result",
    function_name = "zz_test_descriptor"
  )

  expect_equal(
    util_get_category_for_result(indicator, "indicator_or_descriptor"),
    cat1
  )
  expect_equal(
    util_get_category_for_result(descriptor, "indicator_or_descriptor"),
    cat3
  )
  expect_equal(
    util_get_message_for_result(indicator, "indicator_or_descriptor"),
    ""
  )
  expect_equal(
    util_get_message_for_result(list(), "error"),
    "No results computed"
  )

  warning <- simpleWarning("Intrinsic applicability\n> trace")
  attr(warning, "applicability_problem") <- TRUE
  attr(warning, "intrinsic_applicability_problem") <- TRUE
  result <- structure(
    list(),
    class = "dataquieR_result",
    warning = list(warning)
  )

  msg <- util_get_message_for_result(result, "anamat")
  expect_match(msg, "dataquieR-warning-message", fixed = TRUE)
  expect_match(msg, "Intrinsic applicability", fixed = TRUE)
  expect_false(grepl("trace", msg, fixed = TRUE))

  expect_equal(
    util_get_message_for_result(
      result,
      "anamat",
      collapse = function(x) paste(x, collapse = "|")
    ),
    msg
  )
})

test_that("util_get_message_for_result filters ordinary result conditions", {
  skip_on_cran()

  result_message <- simpleMessage("plain note\n> trace")
  result_warning <- simpleWarning("plain warning")
  result_error <- simpleError("plain error")

  result <- structure(
    list(),
    class = "dataquieR_result",
    message = list(result_message),
    warning = list(result_warning),
    error = list(result_error)
  )

  msg <- util_get_message_for_result(result, "error")

  expect_match(msg, "dataquieR-error-message", fixed = TRUE)
  expect_match(msg, "plain error", fixed = TRUE)
  expect_match(msg, "dataquieR-warning-message", fixed = TRUE)
  expect_match(msg, "plain warning", fixed = TRUE)
  expect_match(msg, "dataquieR-message-message", fixed = TRUE)
  expect_match(msg, "plain note", fixed = TRUE)
  expect_false(grepl("trace", msg, fixed = TRUE))
})

test_that("util_get_message_for_result filters applicability conditions", {
  skip_on_cran()

  mark_applicability <- function(condition) {
    attr(condition, "applicability_problem") <- TRUE
    attr(condition, "intrinsic_applicability_problem") <- FALSE
    condition
  }
  result_message <- mark_applicability(simpleMessage("applicability note"))
  result_warning <- mark_applicability(simpleWarning("applicability warning"))
  result_error <- mark_applicability(simpleError("applicability error"))

  result <- structure(
    list(),
    class = "dataquieR_result",
    message = list(result_message),
    warning = list(result_warning),
    error = list(result_error)
  )

  msg <- util_get_message_for_result(result, "applicability")

  expect_match(msg, "applicability error", fixed = TRUE)
  expect_match(msg, "applicability warning", fixed = TRUE)
  expect_match(msg, "applicability note", fixed = TRUE)
  expect_match(msg, "dataquieR-error-message", fixed = TRUE)
  expect_match(msg, "dataquieR-warning-message", fixed = TRUE)
  expect_match(msg, "dataquieR-message-message", fixed = TRUE)
})

test_that("result category helpers classify conditions by aspect", {
  skip_on_cran()

  expect_equal(as.character(util_as_cat(c("cat1", "2", NA, "cat6"))),
    c("cat1", "cat2", NA, "cat6"))
  expect_equal(util_as_integer_cat(util_as_cat(c("cat1", "2", NA, "cat6"))),
    c(1, 2, NA, 6))
  expect_equal(util_as_integer_cat(c("cat1", "2", NA, "cat6")),
    c(1, 2, NA, 6))
  expect_equal(as.character(util_as_cat(character())), character())
  expect_equal(levels(util_as_cat(character())), paste0("cat", 1:5))
  expect_error(util_as_integer_cat(ordered("bad")), "is not TRUE")

  result_without_function <- structure(list(), class = "dataquieR_result")
  expect_true(is.na(util_get_category_for_result(
    result_without_function,
    "indicator_or_descriptor"
  )))

  ordinary_error <- simpleError("ordinary error")
  result <- structure(
    list(),
    class = "dataquieR_result",
    error = list(ordinary_error)
  )
  expect_equal(util_get_category_for_result(result, "error"), cat5)
  expect_equal(util_get_category_for_result(result, "applicability"), cat3)
  expect_equal(util_get_category_for_result(result, "anamat"), cat1)

  intrinsic_error <- simpleError("intrinsic applicability")
  attr(intrinsic_error, "applicability_problem") <- TRUE
  attr(intrinsic_error, "intrinsic_applicability_problem") <- TRUE
  result <- structure(
    list(),
    class = "dataquieR_result",
    error = list(intrinsic_error)
  )
  expect_equal(util_get_category_for_result(result, "anamat"), cat5)
  expect_equal(util_get_category_for_result(result, "applicability"), cat3)

  external_error <- simpleError("external applicability")
  attr(external_error, "applicability_problem") <- TRUE
  attr(external_error, "intrinsic_applicability_problem") <- FALSE
  result <- structure(
    list(),
    class = "dataquieR_result",
    error = list(external_error)
  )
  expect_equal(util_get_category_for_result(result, "anamat"), cat1)
  expect_equal(util_get_category_for_result(result, "applicability"), cat5)
  expect_equal(util_get_category_for_result(result, "error"), cat3)

  external_warning <- simpleWarning("external applicability")
  attr(external_warning, "applicability_problem") <- TRUE
  attr(external_warning, "intrinsic_applicability_problem") <- FALSE
  result <- structure(
    list(),
    class = "dataquieR_result",
    warning = list(external_warning)
  )
  expect_equal(util_get_category_for_result(result, "applicability"), cat3)
  expect_equal(util_get_category_for_result(result, "error"), cat1)

  ordinary_warning <- simpleWarning("ordinary warning")
  result <- structure(
    list(),
    class = "dataquieR_result",
    warning = list(ordinary_warning)
  )
  expect_equal(util_get_category_for_result(result, "anamat"), cat1)
  expect_equal(util_get_category_for_result(result, "error"), cat3)
  expect_equal(util_get_category_for_result(result, "applicability"), cat1)
})
