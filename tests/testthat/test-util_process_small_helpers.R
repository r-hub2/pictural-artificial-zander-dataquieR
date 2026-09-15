test_that("quote helpers use plain ASCII quote characters", {
  skip_on_cran()

  withr::local_options(useFancyQuotes = TRUE)

  expect_equal(
    as.character(util_set_dQuoteString(c("alpha", "beta"))),
    c("\"alpha\"", "\"beta\"")
  )
  expect_equal(
    as.character(util_set_sQuoteString(c("alpha", "beta"))),
    c("'alpha'", "'beta'")
  )
})

test_that("util_suppress_output hides output and collects warnings", {
  skip_on_cran()

  warnings <- new.env(parent = emptyenv())

  printed <- capture.output(
    value <- util_suppress_output({
      cat("stdout text")
      message("message text")
      warning("warning text")
      42
    }, warns = warnings)
  )

  expect_equal(printed, character(0))
  expect_equal(value, 42)
  expect_length(warnings$warns, 1)
  expect_s3_class(warnings$warns[[1]], "warning")
  expect_match(conditionMessage(warnings$warns[[1]]), "warning text")
})

test_that("util_isolate_function keeps only requested bindings", {
  skip_on_cran()

  x <- 2
  y <- 10
  f <- function(z) {
    x + z
  }

  isolated <- util_isolate_function(f, vars = "x")
  x <- 100

  expect_equal(isolated(3), 5)
  expect_true(exists("x", envir = environment(isolated), inherits = FALSE))
  expect_false(exists("y", envir = environment(isolated), inherits = FALSE))
})

test_that("util_deparse1 returns one collapsed expression string", {
  skip_on_cran()

  expr <- quote({
    alpha <- 1
    beta <- alpha + 1
  })

  deparsed <- util_deparse1(expr, collapse = " | ")

  expect_length(deparsed, 1)
  expect_match(deparsed, "alpha <- 1", fixed = TRUE)
  expect_match(deparsed, "beta <- alpha + 1", fixed = TRUE)
  expect_match(deparsed, " | ", fixed = TRUE)
})

test_that("util_attach_attr adds named attributes without changing values", {
  skip_on_cran()

  x <- c(alpha = 1, beta = 2)

  y <- util_attach_attr(
    x,
    source = "metadata",
    flags = c(TRUE, FALSE)
  )

  expect_equal(as.numeric(y), c(1, 2))
  expect_equal(names(y), names(x))
  expect_equal(attr(y, "source", exact = TRUE), "metadata")
  expect_equal(attr(y, "flags", exact = TRUE), c(TRUE, FALSE))
})

test_that("util_int_breaks_rounded keeps integer pretty breaks", {
  skip_on_cran()

  breaks <- util_int_breaks_rounded(c(0.2, 4.8), n = 4)

  expect_true(all(breaks %% 1 == 0))
  expect_equal(breaks, unique(breaks))
  expect_true(min(breaks) <= 1)
  expect_true(max(breaks) >= 4)
})

test_that("label length limit validation rejects unsafe limits", {
  skip_on_cran()

  expect_equal(
    .util_validate_label_length_limit(.MIN_LABEL_LEN),
    .MIN_LABEL_LEN
  )
  expect_equal(
    .util_validate_label_length_limit(.MAX_LABEL_LEN + 1, clamp_to_max = TRUE),
    .MAX_LABEL_LEN
  )

  expect_error(
    .util_validate_label_length_limit(NA_real_, argument = "max_len"),
    "finite whole number"
  )
  expect_error(
    .util_validate_label_length_limit(.MIN_LABEL_LEN - 1, option = "limit"),
    "must be at least"
  )
})

test_that("util_collapse_msgs groups comparable result conditions", {
  skip_on_cran()

  withr::local_options(useFancyQuotes = FALSE)

  make_warning <- function(message, intrinsic = FALSE) {
    condition <- simpleWarning(message)
    attr(condition, "intrinsic_applicability_problem") <- intrinsic
    condition
  }

  age_result <- util_attach_attr(
    list(),
    warning = list(make_warning("AGE_0 has 12 invalid values"))
  )
  bmi_result <- util_attach_attr(
    list(),
    warning = list(make_warning("BMI_0 has 15 invalid values"))
  )
  intrinsic_result <- util_attach_attr(
    list(),
    warning = list(make_warning("SKIP_0 cannot be assessed", intrinsic = TRUE))
  )

  collapsed <- util_collapse_msgs(
    "warning",
    list(
      result.AGE_0 = age_result,
      result.BMI_0 = bmi_result,
      result.SKIP_0 = intrinsic_result
    )
  )

  expect_equal(length(collapsed), 1)
  expect_match(collapsed, '"AGE_0", "BMI_0"', fixed = TRUE)
  expect_match(collapsed, "<VARIABLE>has <NUMBER> invalid values",
    fixed = TRUE
  )
  expect_false(grepl("SKIP_0", collapsed, fixed = TRUE))

  expect_equal(
    util_collapse_msgs("warning", list(result.AGE_0 = list())),
    character(0)
  )

  many_results <- setNames(
    rep(list(age_result), 6),
    paste0("result.VAR_", seq_len(6))
  )
  collapsed_many <- util_collapse_msgs("warning", many_results)

  expect_match(collapsed_many, '"VAR_1", "VAR_2", "VAR_3", "VAR_4", ...',
    fixed = TRUE
  )

  all_result <- util_attach_attr(
    list(),
    warning = list(make_warning("[ALL] has 12 invalid values"))
  )
  collapsed_all <- util_collapse_msgs(
    "warning",
    list(`result.[ALL]` = all_result)
  )

  expect_match(collapsed_all, "For all variables:", fixed = TRUE)
})
