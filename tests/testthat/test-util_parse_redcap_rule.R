test_that("util_parse_redcap_rule works", {
  skip_on_cran()
  skip_if_not_installed(c("withr"))
  skip_if_not_installed(c("callr"))
  debug <- 1
  suppressMessages({
    expect_message2(util_parse_redcap_rule("(12)", debug = debug))
    expect_message2(util_parse_redcap_rule("12", debug = debug))
    expect_warning(util_parse_redcap_rule("xxxsin(12)", debug = debug))
    expect_message2(util_parse_redcap_rule("sum(12)", debug = debug))
    expect_message2(util_parse_redcap_rule("(12 + 1)", debug = debug))
    expect_message2(util_parse_redcap_rule("(12 + 1 * 2)", debug = debug))
    expect_message2(util_parse_redcap_rule("(12 + 1) * 2", debug = debug))
    expect_message2(util_parse_redcap_rule("12 + 1 * 2", debug = debug))
    expect_message2(util_parse_redcap_rule("12 + (1 * 2)", debug = debug))
    expect_message2(util_parse_redcap_rule("(12 + (1 * 2))", debug = debug))
    expect_message2(util_parse_redcap_rule("(12 + (1 * 2)) > 1", debug = debug))
    expect_message2(util_parse_redcap_rule("((12 + (1 * 2)) > 1)", debug = debug)) # nolint: line_length_linter.
    expect_message2(util_parse_redcap_rule("12 + (1 * 2) > 1", debug = debug))
    expect_message2(util_parse_redcap_rule("(12 + (1 * 2) > 1)", debug = debug))
    expect_silent(util_parse_redcap_rule("12 + (1 * 2) > 1"))
    expect_silent(util_parse_redcap_rule("12 + (1 * 2) > 1 and true"))
    expect_silent(util_parse_redcap_rule("12 + (1 * 2) > 1 and [speed] > 1"))
    expect_silent(util_parse_redcap_rule("(12 + (1 * 2) > 1) and ([speed] > 1)")) # nolint: line_length_linter.
    expect_silent(util_parse_redcap_rule("(1 * 2) > 1 and true"))
    expect_message2(util_parse_redcap_rule("(1 * 2) > 1 and 2 > 1", debug = debug)) # nolint: line_length_linter.
    expect_silent(util_parse_redcap_rule(rule = '[speed] > 5 and [dist] > 42 or 1 = "2"')) # nolint: line_length_linter.
    expect_silent(util_parse_redcap_rule(rule = '[speed] > 5 or [dist] > 42 and 1 = "2"')) # nolint: line_length_linter.
    expect_error(util_parse_redcap_rule(rule = "[speed] > 5", entry_pred = "non existing")) # nolint: line_length_linter.
    expect_silent(util_parse_redcap_rule(rule = "([speed] > 5)"))
    expect_silent(util_parse_redcap_rule(rule = "[speed]", entry_pred = "term_expression")) # nolint: line_length_linter.
    expect_silent(util_parse_redcap_rule(rule = "[speed] > 5 and [dist] > 42"))
    expect_silent(util_parse_redcap_rule(rule = "[speed] > 5"))
    expect_silent(util_parse_redcap_rule(rule = "[speed] > 5", entry_pred = "term_expression")) # nolint: line_length_linter.
    expect_silent(util_parse_redcap_rule(rule = "[speed] > 5 or 2 > 1", entry_pred = "term_expression")) # nolint: line_length_linter.
    expect_silent(util_parse_redcap_rule(rule = "[speed] > 5 and 12 > 5"))
    expect_silent(util_parse_redcap_rule(rule = "[speed]"))
    expect_silent(util_parse_redcap_rule(rule = "12"))
    expect_silent(util_parse_redcap_rule(rule = "([speed] > 5) and (1 = 2)"))
    expect_silent(util_parse_redcap_rule(rule = "5*5"))
    expect_silent(util_parse_redcap_rule(rule = "prod(5*5)"))
    expect_silent(util_parse_redcap_rule(rule = "prod(5*5) < 1"))
    expect_silent(util_parse_redcap_rule(rule = "prod(5*5) < 1 and true"))
    expect_silent(util_parse_redcap_rule(rule = "prod(5*5) < 1 and true or false")) # nolint: line_length_linter.
    expect_silent(util_parse_redcap_rule(rule = "prod(5*5) < 1 and (true or false)")) # nolint: line_length_linter.
    expect_silent(util_parse_redcap_rule(rule = "(prod(5*5) < 1 and (true) or false)")) # nolint: line_length_linter.
    expect_silent(util_parse_redcap_rule(rule = "(prod(5*5) < 1 and true) or false")) # nolint: line_length_linter.
    expect_silent(util_parse_redcap_rule(rule = "(prod(5*5) < (1) and true) or false")) # nolint: line_length_linter.
    expect_silent(util_parse_redcap_rule(rule = "1 + 3 * 3"))
    expect_silent(util_parse_redcap_rule(rule = "1 + 3 * 3 and false"))
    expect_silent(util_parse_redcap_rule("[a] = 12 or [b] = 13"))
    expect_silent(util_parse_redcap_rule('[con_consentdt] <> "" and [sda_osd1dt] <> "" and datediff([con_consentdt],[sda_osd1dt],"d",true) < 0')) # nolint: line_length_linter.
    expect_silent(cars[eval(util_parse_redcap_rule(rule = '[speed] > 5 and [dist] > 42 or 1 = "2"'), cars, util_get_redcap_rule_env()), ]) # nolint: line_length_linter.
    expect_silent(util_parse_redcap_rule('datediff([con_consentdt],[sda_osd1dt],"d",true) < 0')) # nolint: line_length_linter.
    expect_silent(util_parse_redcap_rule('datediff([con_consentdt],[sda_osd1dt],"d",true)')) # nolint: line_length_linter.
    expect_message2(util_parse_redcap_rule('datediff([con_consentdt],[sda_osd1dt],"d",true)', debug = debug)) # nolint: line_length_linter.
    expect_message2(util_parse_redcap_rule('datediff([con_consentdt],[sda_osd1dt],"d",true)', debug = debug, entry_pred = "function_expression")) # nolint: line_length_linter.
    expect_message2(util_parse_redcap_rule('[con_consentdt],[sda_osd1dt],"d",true', debug = debug, entry_pred = "arg_part")) # nolint: line_length_linter.
    expect_message2(util_parse_redcap_rule("[con_consentdt]", debug = debug, entry_pred = "term_expression")) # nolint: line_length_linter.
    expect_message2(util_parse_redcap_rule("[con_consentdt]", debug = debug, entry_pred = "arg")) # nolint: line_length_linter.
    expect_message2(util_parse_redcap_rule("[con_consentdt]", debug = debug, entry_pred = "symbol_expression")) # nolint: line_length_linter.
    expect_error(eval(util_parse_redcap_rule('datediff([con_consentdt],[sda_osd1dt],"d",true)', debug = debug, entry_pred = "function_expression"), cars, util_get_redcap_rule_env())) # nolint: line_length_linter.
    expect_error(eval(util_parse_redcap_rule('datediff([con_consentdt],[sda_osd1dt],"d",true)', entry_pred = "function_expression"), cars, util_get_redcap_rule_env())) # nolint: line_length_linter.
    expect_error(suppressWarnings(suppressMessages(eval(util_parse_redcap_rule('datediff([con_consentdt],[sda_osd1dt],"d", "Y-M-D",true)', entry_pred = "function_expression"), cars, util_get_redcap_rule_env())))) # nolint: line_length_linter.
    x <- data.frame(con_consentdt = c(as.POSIXct("2020-01-01"), as.POSIXct("2020-10-20")), sda_osd1dt = c(as.POSIXct("2020-01-20"), as.POSIXct("2020-10-01"))) # nolint: line_length_linter.
    expect_silent(eval(util_parse_redcap_rule('datediff([con_consentdt],[sda_osd1dt],"d", "Y-M-D",true)', entry_pred = "function_expression"), x, util_get_redcap_rule_env())) # nolint: line_length_linter.
    expect_silent(eval(util_parse_redcap_rule('[con_consentdt] <> "" and [sda_osd1dt] <> "" and datediff([con_consentdt],[sda_osd1dt],"d", "Y-M-D",true)'), x, util_get_redcap_rule_env())) # nolint: line_length_linter.
    x <- cars
    expect_error(eval(util_parse_redcap_rule('[con_consentdt] <> "" and [sda_osd1dt] <> "" and datediff([con_consentdt],[sda_osd1dt],"d", "Y-M-D",true)'), x, util_get_redcap_rule_env())) # nolint: line_length_linter.
    x <- data.frame(con_consentdt = c(as.POSIXct("2020-01-01"), as.POSIXct("2020-10-20")), sda_osd1dt = c(as.POSIXct("2020-01-20"), as.POSIXct("2020-10-01"))) # nolint: line_length_linter.
    expect_silent(eval(util_parse_redcap_rule('[con_consentdt] <> "" and [sda_osd1dt] <> "" and datediff([con_consentdt],[sda_osd1dt],"d", "Y-M-D",true) < 10'), x, util_get_redcap_rule_env())) # nolint: line_length_linter.
    expect_silent(eval(util_parse_redcap_rule('datediff([con_consentdt],[sda_osd1dt],"d", "Y-M-D",true) < 10'), x, util_get_redcap_rule_env())) # nolint: line_length_linter.
    expect_silent(eval(util_parse_redcap_rule('[con_consentdt] <> "" and [sda_osd1dt] <> ""'), x, util_get_redcap_rule_env())) # nolint: line_length_linter.
    expect_silent(eval(util_parse_redcap_rule('[con_consentdt] <> "" and [sda_osd1dt] == ""'), x, util_get_redcap_rule_env())) # nolint: line_length_linter.
    x$sda_osd1dt <- NA
    expect_silent(eval(util_parse_redcap_rule('[con_consentdt] <> "" and [sda_osd1dt] == ""'), x, util_get_redcap_rule_env())) # nolint: line_length_linter.
    expect_silent(eval(util_parse_redcap_rule('[con_consentdt] == "" and [sda_osd1dt] == ""'), x, util_get_redcap_rule_env())) # nolint: line_length_linter.
    x$sda_osd1dt[1] <- as.POSIXct("2020-01-01")
    expect_silent(eval(util_parse_redcap_rule('[con_consentdt] == "" and [sda_osd1dt] == ""'), x, util_get_redcap_rule_env())) # nolint: line_length_linter.
    expect_silent(eval(util_parse_redcap_rule('[con_consentdt] == "" or [sda_osd1dt] == ""'), x, util_get_redcap_rule_env())) # nolint: line_length_linter.
    expect_silent(expect_error(eval(util_parse_redcap_rule('[con_consentdt] <> "" and [sda_osd1dt] <> "" and datediff([con_consentdt],[sda_osd1dt],"d", "Y-M-D",true) < 10'), x, util_get_redcap_rule_env()))) # nolint: line_length_linter.
    x <- data.frame(con_consentdt = c(as.POSIXct("2020-01-01"), as.POSIXct("2020-10-20")), sda_osd1dt = c(as.POSIXct("2020-01-20"), as.POSIXct("2020-10-01"))) # nolint: line_length_linter.
    expect_silent(eval(util_parse_redcap_rule('[con_consentdt] <> "" and [sda_osd1dt] <> "today" and datediff([con_consentdt],[sda_osd1dt],"d", "Y-M-D",true) < 10'), x, util_get_redcap_rule_env())) # nolint: line_length_linter.
    x$sda_osd1dt[1] <- Sys.Date()
    expect_silent(eval(util_parse_redcap_rule('[con_consentdt] <> "" and [sda_osd1dt] <> "today" and datediff([con_consentdt],[sda_osd1dt],"d", "Y-M-D",true) < 10'), x, util_get_redcap_rule_env())) # nolint: line_length_linter.
    expect_silent(eval(util_parse_redcap_rule('[con_consentdt] <> "" and [sda_osd1dt] = "today" and datediff([con_consentdt],[sda_osd1dt],"d", "Y-M-D",true) < 10'), x, util_get_redcap_rule_env())) # nolint: line_length_linter.
    expect_silent(lapply(lapply(setNames(nm = c("(12) > 0 and [speed] > 12", "4+4*5+6")), util_parse_redcap_rule, debug = 0), eval, cars, util_get_redcap_rule_env())) # nolint: line_length_linter.
    expect_equal(
      util_eval_rule(util_parse_redcap_rule("12 * ([speed] + not(not(true or false)))", debug = 0), ds1 = cars, use_value_labels = FALSE), # nolint: line_length_linter.
      with(cars, 12 * (speed + !(!(TRUE || FALSE))))
    )
    expect_equal(
      util_eval_rule(util_parse_redcap_rule("if([speed] > 5, 1, 0)", debug = 0), ds1 = cars, use_value_labels = FALSE), # nolint: line_length_linter.
      with(cars, ifelse(speed > 5, 1, 0))
    )

    x <- tibble::tribble(
      ~con_consentdt, ~cont_inddt, ~sda_osd1dt,
      as.POSIXct("2020-01-01"), as.POSIXct("2020-01-20"), as.POSIXct("2020-01-20"), # nolint: line_length_linter.
      as.POSIXct("2020-10-20"), as.POSIXct("2020-10-20 15:00:00"), as.POSIXct("2020-10-20 17:00:00") # nolint: line_length_linter.
    )

    expect_equal(
      util_eval_rule(util_parse_redcap_rule("successive_dates([con_consentdt], [cont_inddt], [sda_osd1dt])", debug = 0), # nolint: line_length_linter.
        ds1 = x, use_value_labels = FALSE
      ),
      with(x, con_consentdt <= cont_inddt & cont_inddt <= sda_osd1dt)
    )

    expect_equal(
      util_eval_rule(util_parse_redcap_rule("strictly_successive_dates([con_consentdt], [cont_inddt], [sda_osd1dt])", debug = 0), # nolint: line_length_linter.
        ds1 = x, use_value_labels = FALSE
      ),
      with(x, con_consentdt < cont_inddt & cont_inddt < sda_osd1dt)
    )
  })
})

test_that("util_parse_redcap_rule covers standalone parser branches", {
  skip_on_cran()

  data <- data.frame(speed = c(1, 3))
  env <- util_get_redcap_rule_env()

  date_expr <- util_parse_redcap_rule("[2020-01-02]",
    entry_pred = "term_expression",
    must_eof = TRUE
  )
  date_call <- .util_redcap_rule2_unparen(date_expr)
  expect_identical(as.character(date_call[[1]]), "util_parse_date")
  expect_identical(date_call[[2]], "2020-01-02")
  expect_identical(date_call[["tz"]], "")

  time_expr <- util_parse_redcap_rule("[12:34:56 UTC]",
    entry_pred = "term_expression",
    must_eof = TRUE
  )
  expect_equal(
    as.character(eval(time_expr, data, env)),
    "12:34:56"
  )

  set_expr <- util_parse_redcap_rule("{1, 2; 3}",
    entry_pred = "term_expression",
    must_eof = TRUE
  )
  expect_equal(eval(set_expr, data, env), c(1, 2, 3))

  not_in_expr <- util_parse_redcap_rule("[speed] not in {1, 2}",
    must_eof = TRUE
  )
  expect_equal(eval(not_in_expr, data, env), c(FALSE, TRUE))

  ignored_tail_expr <- util_parse_redcap_rule("(1; 2)",
    entry_pred = "term_expression",
    must_eof = TRUE
  )
  expect_equal(eval(ignored_tail_expr, data, env), 1)

  closed_interval <- eval(util_parse_redcap_rule("[1; 2]",
      entry_pred = "interval",
      must_eof = TRUE
    ), data, env)
  expect_equal(unclass(closed_interval), list(
    inc_l = TRUE,
    low = 1,
    upp = 2,
    inc_u = TRUE
  ))

  open_interval <- eval(util_parse_redcap_rule("(1; 2]",
      entry_pred = "interval",
      must_eof = TRUE
    ), data, env)
  expect_equal(unclass(open_interval), list(
    inc_l = FALSE,
    low = 1,
    upp = 2,
    inc_u = TRUE
  ))

  expect_warning(
    util_parse_redcap_rule("[speed] trailing", must_eof = TRUE),
    "Parser error"
  )
  expect_warning(
    util_parse_redcap_rule("[1; 2] trailing", must_eof = TRUE),
    "Found extra characters"
  )
  expect_warning(
    util_parse_redcap_rule("[]", must_eof = TRUE),
    "Parser error"
  )
  expect_warning(
    util_parse_redcap_rule("\"unterminated", must_eof = TRUE),
    "Parser error"
  )
})

test_that("util_parse_redcap_rule covers interval and arg-part edge cases", {
  skip_on_cran()

  data <- data.frame(speed = c(1, 3))
  env <- util_get_redcap_rule_env()

  unbounded_interval <- eval(
    util_parse_redcap_rule("[-Inf; +Inf]",
      entry_pred = "interval",
      must_eof = TRUE
    ),
    data,
    env
  )
  expect_equal(unclass(unbounded_interval), list(
    inc_l = TRUE,
    low = -Inf,
    upp = Inf,
    inc_u = TRUE
  ))

  date_time_interval <- util_parse_redcap_rule(
    "[2020-01-01 12:00:00 UTC; 2020-01-02]",
    entry_pred = "interval",
    must_eof = TRUE
  )
  interval_call <- .util_redcap_rule2_unparen(date_time_interval)
  expect_identical(as.character(interval_call[[1L]]), "interval")
  expect_identical(as.character(interval_call$low[[1L]]), "util_parse_date")
  expect_identical(interval_call$low[[2L]], "2020-01-01 12:00:00")
  expect_identical(interval_call$low$tz, "UTC")

  power_expr <- util_parse_redcap_rule("2 ** 3", must_eof = TRUE)
  expect_equal(eval(power_expr, data, env), 8)

  arg_part <- util_parse_redcap_rule(
    "[speed], if([speed] > 1, 1, 0), {1, 2}",
    entry_pred = "arg_part",
    must_eof = TRUE
  )
  expect_identical(as.character(arg_part[[1L]]), "list")
  expect_length(as.list(arg_part)[-1L], 3L)

  expect_error(
    .util_redcap_rule2_square_value(""),
    "Empty variable reference"
  )
  expect_error(
    .util_redcap_rule2_parse_interval_string("1;2",
      must_eof = TRUE
    ),
    "Expected interval"
  )
})

test_that("util_parse_redcap_rule covers internal parser guardrails", {
  skip_on_cran()

  tok <- function(type, value = NULL) {
    list(type = type, value = value)
  }

  expect_identical(
    .util_redcap_rule2_interval_endpoint("", side = "upp"),
    Inf
  )
  expect_error(
    .util_redcap_rule2_parse_interval_string("[", must_eof = TRUE),
    "Invalid interval while parsing REDCap interval"
  )
  expect_error(
    .util_redcap_rule2_parse_interval_string("[1]", must_eof = TRUE),
    "Invalid interval while parsing REDCap interval"
  )
  expect_error(
    .util_redcap_rule2_tokenize_standalone("[speed"),
    "Unclosed square-bracket expression"
  )
  expect_error(
    .util_redcap_rule2_tokenize_standalone("[speed)"),
    "Invalid square-bracket expression"
  )
  expect_error(
    .util_redcap_rule2_parse_tokens(
      list(tok("("), tok("ATOM", 1), tok("]"), tok("EOF")),
      "(1]",
      must_eof = TRUE
    ),
    "Unexpected token while parsing REDCap rule"
  )
  expect_error(
    .util_redcap_rule2_parse_tokens(
      list(tok("OP", "*"), tok("ATOM", 1), tok("EOF")),
      "*1",
      must_eof = TRUE
    ),
    "Unexpected operator while parsing REDCap rule"
  )
  expect_error(
    .util_redcap_rule2_parse_tokens(
      list(tok("NAME", "x"), tok("EOF")),
      "x",
      must_eof = TRUE
    ),
    "Unexpected bare name while parsing REDCap rule"
  )
  expect_equal(
    eval(.util_redcap_rule2_parse_tokens(
      list(
        tok("OP", "+"),
        tok("("),
        tok("ATOM", 1),
        tok(","),
        tok("ATOM", 2),
        tok(")"),
        tok("EOF")
      ),
      "+(1, 2)",
      must_eof = TRUE
    )),
    3
  )
  expect_error(
    .util_redcap_rule2_parse_tokens(
      list(
        tok("INTERVAL", list(open = "[", inner = "1", close = "]")),
        tok("EOF")
      ),
      "[1]",
      must_eof = TRUE
    ),
    "Invalid interval while parsing REDCap rule"
  )
  expect_error(
    .util_redcap_rule2_parse_arg_part(
      list(
        tok("ATOM", 1),
        tok("OP", "+"),
        tok("ATOM", 2),
        tok("ATOM", 3),
        tok("EOF")
      ),
      "1+2 3",
      must_eof = TRUE
    ),
    "Unexpected trailing input while parsing REDCap rule"
  )
  expect_error(
    .util_redcap_rule2_parse_interval_entry(
      list(tok("ATOM", 1), tok("EOF")),
      "1",
      must_eof = TRUE
    ),
    "Expected interval while parsing REDCap interval"
  )
  expect_error(
    .util_redcap_rule2_parse_interval_entry(
      list(
        tok("INTERVAL", list(open = "[", inner = "1;2", close = "]")),
        tok("ATOM", 3),
        tok("EOF")
      ),
      "[1;2] 3",
      must_eof = TRUE
    ),
    "Unexpected trailing input while parsing REDCap interval"
  )
})
