test_that("util_warning works", {
  skip_on_cran()

  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  expect_warning(
    util_warning(
      "The one and everything is %d (%s).", 42,
      "Douglas Adams"
    ),
    regexp = ".*The one and everything is 42 .Douglas Adams.*",
    perl = TRUE
  )

  testenv <- new.env(parent = baseenv())

  testenv$g <- function(...) {
    util_warning(...)
  }
  environment(testenv$g) <- asNamespace("dataquieR")

  f <- function(...) {
    g(...)
  }
  environment(f) <- testenv

  h <- function(...) {
    do.call(g, list(...))
  }
  environment(h) <- testenv

  expect_warning(
    f(
      "The one and everything is %d (%s).", 42,
      "Douglas Adams"
    ),
    regexp =
      paste(
        ".*The one and everything is",
        "42 \\(Douglas Adams\\).*"
      ),
    perl = TRUE
  )
  expect_warning(
    h(
      "The one and everything is %d (%s).", 42,
      "Douglas Adams"
    ),
    regexp =
      paste(
        ".*The one and everything is",
        "42 \\(Douglas Adams\\).*"
      ),
    perl = TRUE
  )
})
test_that("util_warning works", {
  skip_on_cran()

  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  expect_warning(
    util_warning(
      "The one and everything is %d (%s).", 42,
      "Douglas Adams"
    ),
    regexp = "The one and everything is 42 (Douglas Adams).",
    fixed = TRUE
  )

  g <- function(...) {
    util_warning(...)
  }
  environment(g) <- asNamespace("dataquieR")

  testenv <- new.env(parent = baseenv())
  testenv$gg <- force(g)

  f <- function(...) {
    gg(...)
  }
  environment(f) <- testenv

  h <- function(...) {
    do.call(gg, list(...))
  }
  environment(h) <- testenv

  expect_warning(
    f(
      "The one and everything is %d (%s).", 42,
      "Douglas Adams"
    ),
    regexp =
      paste(
        ".*The one and everything is",
        "42 \\(Douglas Adams\\).*"
      ),
    perl = TRUE
  )
  expect_warning(
    h(
      "The one and everything is %d (%s).", 42,
      "Douglas Adams"
    ),
    regexp =
      paste(
        ".*The one and everything is",
        "42 \\(Douglas Adams\\).*"
      ), ,
    perl = TRUE
  )

  x <- function(m) {
    warning(m)
  }
  w <- function(w) {
    util_warning(w)
    invokeRestart("muffleWarning")
  }
  expect_warning(
    withCallingHandlers(x(""), warning = w),
    regexp = "Warning",
    fixed = TRUE
  )
  expect_warning(
    withCallingHandlers(x("CAVE CANEM"), warning = w),
    regexp = "CAVE CANEM",
    fixed = TRUE
  )
})

test_that("condition helpers mark one-shot warnings and metadata classes", {
  skip_on_cran()

  util_clean_condition_once_cache()
  withr::defer(util_clean_condition_once_cache())

  expect_false(util_condition_once_seen("coverage-once"))

  first <- expect_warning(
    util_warning(
      "one-shot warning for %s",
      "coverage",
      once_id = "coverage-once",
      applicability_problem = TRUE,
      intrinsic_applicability_problem = TRUE,
      additional_classes = "coverage_warning",
      varname = "x"
    ),
    "one-shot warning for coverage",
    fixed = TRUE
  )

  expect_s3_class(first, "coverage_warning")
  expect_s3_class(first, dataquieR.applicability_problem)
  expect_s3_class(first, dataquieR.intrinsic_applicability_problem)
  expect_true(util_attr(first, "applicability_problem", exact = TRUE))
  expect_true(util_attr(first, "intrinsic_applicability_problem", exact = TRUE))
  expect_equal(util_attr(first, "varname", exact = TRUE), "x")
  expect_true(util_condition_once_seen("coverage-once"))

  second <- expect_warning(
    util_warning("one-shot warning for %s", "coverage",
      once_id = "coverage-once"),
    NA
  )
  expect_s3_class(second, "rlang_warning")

  expect_null(util_clean_condition_once_cache())
  expect_false(util_condition_once_seen("coverage-once"))
})

test_that(
  "condition helpers validate metadata and normalize empty conditions",
  {
    skip_on_cran()

    expect_error(
      util_warning("bad integrity", integrity_indicator = "not_a_dqi"),
      "not a supported"
    )

    expect_error(
      util_warning("bad once id", once_id = 1),
      "once_id"
    )

    expect_warning(
      util_warning(simpleWarning("")),
      "Warning",
      fixed = TRUE
    )

    expect_message(
      util_message(simpleMessage("")),
      "Message",
      fixed = TRUE
    )

    expect_error(
      util_error(simpleError("")),
      "Error",
      fixed = TRUE
    )
  }
)

test_that(
  "condition constructor factory creates each supported signal helper",
  {
    skip_on_cran()

    expect_warning(
      util_condition_constructor_factory("warning")("factory warning"),
      "factory warning",
      fixed = TRUE
    )
    expect_message(
      util_condition_constructor_factory("message")("factory message"),
      "factory message",
      fixed = TRUE
    )
    expect_error(
      util_condition_constructor_factory("error")("factory error"),
      "factory error",
      fixed = TRUE
    )
  }
)

test_that("condition helpers signal once_id conditions only once", {
  skip_on_cran()
  util_clean_condition_once_cache()
  withr::defer(util_clean_condition_once_cache())

  expect_warning(
    first <- util_warning("only once", once_id = "unit-test-once-id"),
    "only once",
    fixed = TRUE
  )
  expect_s3_class(first, "rlang_warning")
  expect_true(util_condition_once_seen("unit-test-once-id"))

  second <- expect_silent(
    util_warning("only once", once_id = "unit-test-once-id")
  )
  expect_s3_class(second, "rlang_warning")
  expect_equal(conditionMessage(second), conditionMessage(first))
})

test_that(
  "condition helpers retain suppressed and try-error conditions",
  {
    skip_on_cran()

    withr::local_options(
      dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf,
      dataquieR.CONDITIONS_WITH_STACKTRACE = FALSE,
      dataquieR.WARNINGS_WITH_CALLER = FALSE
    )

    suppressed <- expect_silent(util_warning("suppressed warning"))
    expect_s3_class(suppressed, "rlang_warning")
    expect_equal(conditionMessage(suppressed), "suppressed warning")
    expect_null(conditionCall(suppressed))

    stored_error <- try(stop("stored error"), silent = TRUE)
    unwrapped <- expect_silent(util_warning(stored_error))
    expect_equal(conditionMessage(unwrapped), "stored error")
  }
)

test_that("condition helpers skip unused stack traversal", {
  skip_on_cran()

  withr::local_options(
    dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf,
    dataquieR.CONDITIONS_WITH_STACKTRACE = FALSE,
    dataquieR.WARNINGS_WITH_CALLER = FALSE
  )
  testthat::local_mocked_bindings(
    util_find_first_externally_called_functions_in_stacktrace = function() {
      util_error("The disabled stacktrace path must not be entered")
    }
  )

  condition <- expect_silent(util_warning("fast warning"))
  expect_equal(conditionMessage(condition), "fast warning")
  expect_null(conditionCall(condition))
})

test_that("condition helpers preserve literals and bound long messages", {
  skip_on_cran()

  withr::local_options(
    dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf,
    dataquieR.CONDITIONS_WITH_STACKTRACE = FALSE,
    dataquieR.WARNINGS_WITH_CALLER = FALSE
  )

  literal_percent <- expect_silent(
    util_warning("a literal percent sign: 50%")
  )
  expect_equal(conditionMessage(literal_percent), "a literal percent sign: 50%")

  long_warning <- expect_silent(util_warning(paste0(strrep("x", 8192), "%")))
  expect_equal(nchar(conditionMessage(long_warning)), 8192)
  expect_false(endsWith(conditionMessage(long_warning), "%"))
})

test_that("condition helpers retain silent metadata and trace information", {
  skip_on_cran()

  withr::local_options(
    dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf,
    dataquieR.CONDITIONS_WITH_STACKTRACE = FALSE,
    dataquieR.WARNINGS_WITH_CALLER = FALSE,
    dataquieR.traceback = TRUE
  )

  condition <- expect_silent(util_warning(
    "silent %s",
    "warning",
    title = "Prefix: ",
    integrity_indicator = "int",
    applicability_problem = FALSE,
    intrinsic_applicability_problem = FALSE,
    additional_classes = "silent_coverage_warning"
  ))

  expect_equal(conditionMessage(condition), "Prefix: silent warning")
  expect_null(conditionCall(condition))
  expect_s3_class(condition, "silent_coverage_warning")
  expect_identical(
    util_attr(condition, "integrity_indicator", exact = TRUE),
    "int"
  )
  expect_s3_class(condition$trace, "rlang_trace")
})

test_that("pipeline conditions do not suppress repeated once identifiers", {
  skip_on_cran()

  util_clean_condition_once_cache()
  withr::defer(util_clean_condition_once_cache())
  withr::local_options(dataquieR.CONDITIONS_WITH_STACKTRACE = FALSE)

  with_pipeline({
    expect_warning(
      util_warning("pipeline warning", once_id = "pipeline-coverage-once"),
      "pipeline warning",
      fixed = TRUE
    )
    expect_warning(
      util_warning("pipeline warning", once_id = "pipeline-coverage-once"),
      "pipeline warning",
      fixed = TRUE
    )
  })

  expect_false(util_condition_once_seen("pipeline-coverage-once"))
})
