test_that("redcap parser handles interval endpoint variants", {
  skip_on_cran()

  date_interval <- util_parse_redcap_rule(
    "[2020-01-01;2020-01-31]",
    entry_pred = "interval",
    must_eof = TRUE
  )
  date_interval <- .util_redcap_rule2_unparen(date_interval)
  expect_identical(as.character(date_interval[[1]]), "interval")
  expect_true(date_interval[["inc_l"]])
  expect_true(date_interval[["inc_u"]])
  expect_identical(
    as.character(date_interval[["low"]][[1]]),
    "util_parse_date"
  )
  expect_identical(date_interval[["low"]][["tz"]], "")

  time_interval <- util_parse_redcap_rule(
    "[12:00:00;13:00:00 Europe/Berlin]",
    entry_pred = "interval",
    must_eof = TRUE
  )
  time_interval <- .util_redcap_rule2_unparen(time_interval)
  expect_identical(
    as.character(time_interval[["upp"]][[1]]),
    "util_parse_time"
  )
  expect_identical(time_interval[["upp"]][["tz"]], "Europe/Berlin")

  open_interval <- util_parse_redcap_rule(
    "(-Inf;+Inf]",
    entry_pred = "interval",
    must_eof = TRUE
  )
  open_interval <- .util_redcap_rule2_unparen(open_interval)
  expect_false(open_interval[["inc_l"]])
  expect_true(open_interval[["inc_u"]])
  expect_identical(open_interval[["low"]], -Inf)
  expect_identical(open_interval[["upp"]], Inf)
})

test_that("redcap parser handles set and not-in expressions", {
  skip_on_cran()

  parsed <- util_parse_redcap_rule("[x] not in {1, 2, 3}", must_eof = TRUE)
  parsed <- .util_redcap_rule2_unparen(parsed)
  expect_identical(as.character(parsed[[1]]), "not in")
  expect_identical(as.character(parsed[[2]]), "x")
  expect_identical(as.character(parsed[[3]][[1]]), "(")
  expect_identical(as.character(parsed[[3]][[2]][[1]]), "set")

  set_call <- util_parse_redcap_rule("{1;2;3}", must_eof = TRUE)
  set_call <- .util_redcap_rule2_unparen(set_call)
  expect_identical(as.character(set_call[[1]]), "set")
  expect_equal(as.list(set_call[-1]), list(1, 2, 3))
})

test_that("redcap tokenizer keeps operator and literal tokens", {
  skip_on_cran()

  tokens <- .util_redcap_rule2_tokenize_standalone(
    "('a\\'b' != 5.0e+1) and false or NaN"
  )

  expect_equal(
    vapply(tokens, `[[`, character(1), "type"),
    c("(", "ATOM", "OP", "ATOM", ")", "OP", "ATOM", "OP", "ATOM", "EOF")
  )
  expect_identical(tokens[[2]]$value, "a\\'b")
  expect_identical(tokens[[3]]$value, "!=")
  expect_equal(tokens[[4]]$value, 50)
  expect_false(tokens[[7]]$value)
  expect_true(is.nan(tokens[[9]]$value))

  expect_error(
    .util_redcap_rule2_tokenize_standalone("@"),
    "Unexpected character"
  )
  expect_error(
    .util_redcap_rule2_tokenize_standalone("[abc"),
    "Unclosed square-bracket expression"
  )
  expect_error(
    .util_redcap_rule2_tokenize_standalone("[abc)"),
    "Invalid square-bracket expression"
  )
  expect_error(
    .util_redcap_rule2_tokenize_standalone("."),
    "Invalid numeric literal"
  )

  interval_tokens <- .util_redcap_rule2_tokenize_standalone("(1;2]")
  expect_equal(vapply(interval_tokens, `[[`, character(1), "type"),
    c("INTERVAL", "EOF"))
  expect_identical(interval_tokens[[1L]]$value$open, "(")
  expect_identical(interval_tokens[[1L]]$value$close, "]")
})

test_that("redcap parser reports token and trailing-input failures", {
  skip_on_cran()

  expect_warning(
    trailing <- util_parse_redcap_rule("[x] > 1 trailing", must_eof = TRUE),
    "Parser error in REDCap rule"
  )
  expect_identical(util_attr(trailing, "src", exact = TRUE),
    "[x] > 1 trailing")

  expect_warning(
    unclosed <- util_parse_redcap_rule("'unterminated", must_eof = TRUE),
    "Parser error in REDCap rule"
  )
  expect_identical(util_attr(unclosed, "src", exact = TRUE), "'unterminated")

  expect_warning(
    invalid_interval <- util_parse_redcap_rule("[1 2]",
      entry_pred = "interval",
      must_eof = TRUE),
    "Parser error in REDCap interval"
  )
  expect_identical(util_attr(invalid_interval, "src", exact = TRUE), "[1 2]")
})

test_that("interval parser entry validates tokenized interval input", {
  skip_on_cran()

  tokens <- .util_redcap_rule2_tokenize_standalone("[1;2]")
  parsed <- .util_redcap_rule2_unparen(
    .util_redcap_rule2_parse_interval_entry(tokens, "[1;2]")
  )
  expect_true(parsed[["inc_l"]])
  expect_true(parsed[["inc_u"]])
  expect_identical(parsed[["low"]], 1)
  expect_identical(parsed[["upp"]], 2)

  wrong_type <- tokens
  wrong_type[[1L]]$type <- "ATOM"
  expect_error(
    .util_redcap_rule2_parse_interval_entry(wrong_type, "[1;2]"),
    "Expected interval"
  )

  trailing <- tokens
  trailing[[2L]]$type <- "ATOM"
  expect_error(
    .util_redcap_rule2_parse_interval_entry(trailing, "[1;2]"),
    "Unexpected trailing input"
  )

  invalid <- tokens
  invalid[[1L]]$value$inner <- "1 2"
  expect_error(
    .util_redcap_rule2_parse_interval_entry(invalid, "[1 2]"),
    "Invalid interval"
  )
})

test_that("redcap parser covers expression and unary operator branches", {
  skip_on_cran()

  env <- util_get_redcap_rule_env()
  data <- data.frame(x = 1:2)

  expr <- expression(1 + 2)
  attr(expr, "src") <- "1 + 2"
  expect_equal(.util_redcap_rule2_unstructure(expr), quote(1 + 2))

  unary_call <- .util_redcap_rule2_unparen(
    util_parse_redcap_rule("-(2)", must_eof = TRUE)
  )
  expect_identical(as.character(unary_call[[1L]]), "-")
  expect_identical(unary_call[[2L]], 2)

  expect_equal(
    eval(util_parse_redcap_rule("+(1, 2)", must_eof = TRUE), data, env),
    3
  )
  expect_equal(
    eval(util_parse_redcap_rule("8 / 2 ^ 2", must_eof = TRUE), data, env),
    2
  )
  expect_warning(
    util_parse_redcap_rule("foo", must_eof = TRUE),
    "Parser error in REDCap rule"
  )
})
