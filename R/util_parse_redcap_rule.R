# util_parse_redcap_rule2.R
# Prototype replacements for util_parse_redcap_rule() without qmrparser.
# Both frontends use the same parser core; util_parse_redcap_rule2_rly()
# uses rly for lexing only.

#' Internal helper: util redcap rule2 parse fail
#'
#' @noRd
.util_redcap_rule2_parse_fail <- function(rule) {
  structure(list(), src = rule)
}

.util_redcap_rule2_valid_entry_preds <- c(
  "REDcapPred",
  "term_expression",
  "function_expression",
  "arg_part",
  "arg",
  "symbol_expression",
  "interval"
)

#' Internal helper: util redcap rule2 valid function
#'
#' @noRd
.util_redcap_rule2_valid_function <- function(name) {
  if (identical(name, "if")) {
    return(TRUE)
  }

  env <- tryCatch(util_get_redcap_rule_env(), error = function(e) NULL)
  if (is.environment(env) && exists(name, envir = env, inherits = TRUE)) {
    return(TRUE)
  }

  exists(name, envir = baseenv(), inherits = FALSE)
}

#' Internal helper: util redcap rule2 dbg result
#'
#' @noRd
.util_redcap_rule2_dbg_result <- function(debug, result) {
  # The former parser emitted diagnostic messages from grammar actions when
  # debug > 0. Keep this behaviour for tests and callers that rely on it.
  if (isTRUE(debug > 0)) {
    util_message(
      "%s",
      util_deparse1(result),
      applicability_problem = FALSE,
      intrinsic_applicability_problem = FALSE
    )
  }
  invisible(NULL)
}

#' Internal helper: util redcap rule2 dbg fkt
#'
#' @noRd
.util_redcap_rule2_dbg_fkt <- function(debug, f) {
  if (isTRUE(debug >= 1)) {
    util_message(sprintf("fkt: %s", dQuote(f)),
      applicability_problem = FALSE,
      intrinsic_applicability_problem = FALSE)
  }
  invisible(NULL)
}

#' Internal helper: util redcap rule2 dbg function expression
#'
#' @noRd
.util_redcap_rule2_dbg_function_expression <- function(debug, f) {
  if (isTRUE(debug >= 1)) {
    util_message(sprintf("function_expression: %s", sQuote(f)),
      applicability_problem = FALSE,
      intrinsic_applicability_problem = FALSE)
  }
  invisible(NULL)
}

#' Internal helper: util redcap rule2 dbg arg
#'
#' @noRd
.util_redcap_rule2_dbg_arg <- function(debug, x) {
  if (isTRUE(debug >= 1)) {
    util_message(sprintf("arg %s", dQuote(paste0(deparse(x), collapse = " "))),
      applicability_problem = FALSE,
      intrinsic_applicability_problem = FALSE)
  }
  invisible(NULL)
}

#' Internal helper: util redcap rule2 warn parse
#'
#' @noRd
.util_redcap_rule2_warn_parse <- function(rule, what = "rule") {
  # The previous qmrparser-based implementation used warning() from errorFun()
  # for parser failures. In dataquieR, parser failures are rule/metadata
  # applicability
  # problems, but not intrinsic applicability problems.
  util_warning(sprintf("Parser error in REDCap %s (will ignore this rule): %s",
      what, rule),
    applicability_problem = TRUE,
    intrinsic_applicability_problem = FALSE)
}

#' Internal helper: util redcap rule2 warn extra
#'
#' @noRd
.util_redcap_rule2_warn_extra <- function(rule) {
  util_warning(sprintf("Found extra characters in %s (will ignore this rule)",
      rule),
    applicability_problem = TRUE,
    intrinsic_applicability_problem = FALSE)
}

#' Internal helper: util redcap rule2 string value
#'
#' @noRd
.util_redcap_rule2_string_value <- function(x) {
  quote <- substr(x, 1L, 1L)
  if (quote %in% c("\"", "'")) {
    x <- substr(x, 2L, nchar(x) - 1L)
    if (identical(quote, "\"")) {
      x <- gsub("\\\\\"", "\"", x, fixed = TRUE)
    } else {
      x <- gsub("\\\\'", "'", x, fixed = TRUE)
    }
    x <- gsub("\\\\\\\\", "\\\\", x, fixed = TRUE)
  }

  if (identical(x, "today")) {
    return(as.call(list(as.name("util_parse_date"), Sys.Date())))
  }

  x
}

#' Internal helper: util redcap rule2 square value
#'
#' @noRd
.util_redcap_rule2_square_value <- function(x) {
  x <- trimws(x)

  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x, perl = TRUE)) {
    return(as.call(list(as.name("util_parse_date"), x, tz = "")))
  }

  if (grepl("^\\d{2}:\\d{2}:\\d{2}(\\s+[A-Za-z_][A-Za-z0-9_./+-]*)?$",
      x, perl = TRUE)) {
    parts <- strsplit(x, "\\s+", perl = TRUE)[[1L]]
    return(as.call(list(
      as.name("util_parse_time"),
      parts[[1L]],
      tz = if (length(parts) > 1L) parts[[2L]] else ""
    )))
  }

  if (!nzchar(x)) {
    util_error("Empty variable reference.",
      applicability_problem = TRUE,
      intrinsic_applicability_problem = FALSE)
  }

  as.name(x)
}

#' Internal helper: util redcap rule2 interval endpoint
#'
#' @noRd
.util_redcap_rule2_interval_endpoint <- function(x, side = c("low", "upp"),
  debug = 0) {
  side <- match.arg(side)
  x <- trimws(x)

  if (!nzchar(x)) {
    if (identical(side, "low")) {
      return(-Inf)
    } else {
      return(Inf)
    }
  }

  if (x %in% c("Inf", "+Inf")) {
    return(Inf)
  }
  if (identical(x, "-Inf")) {
    return(-Inf)
  }

  if (grepl("^[+-]?(?:\\d+(?:\\.\\d+)?|\\.\\d+)(?:[eE][+-]?\\d+)?$", x, perl = TRUE)) { # nolint: line_length_linter.
    return(as.numeric(x))
  }

  if (grepl("^\\d{4}-\\d{2}-\\d{2}(\\s+\\d{2}:\\d{2}:\\d{2}(\\s+[A-Za-z_][A-Za-z0-9_./+-]*)?)?$", # nolint: line_length_linter.
      x, perl = TRUE)) {
    parts <- strsplit(x, "\\s+", perl = TRUE)[[1L]]
    if (length(parts) >= 2L) {
      tz <- if (length(parts) >= 3L) parts[[3L]] else ""
      return(as.call(list(
        as.name("util_parse_date"),
        paste(parts[[1L]], parts[[2L]]),
        tz = tz
      )))
    }
    return(as.call(list(as.name("util_parse_date"), parts[[1L]], tz = "")))
  }

  if (grepl("^\\d{2}:\\d{2}:\\d{2}(\\s+[A-Za-z_][A-Za-z0-9_./+-]*)?$",
      x, perl = TRUE)) {
    parts <- strsplit(x, "\\s+", perl = TRUE)[[1L]]
    return(as.call(list(
      as.name("util_parse_time"),
      parts[[1L]],
      tz = if (length(parts) > 1L) parts[[2L]] else ""
    )))
  }

  .util_redcap_rule2_unstructure(
    .util_parse_redcap_rule_standalone(x, debug = debug,
      entry_pred = "REDcapPred",
      must_eof = TRUE)
  )
}

#' Internal helper: util redcap rule2 split interval
#'
#' @noRd
.util_redcap_rule2_split_interval <- function(x) {
  pos <- regexpr("[;,]", x, perl = TRUE)[[1L]]
  if (pos < 0L) {
    return(character(0))
  }

  c(substr(x, 1L, pos - 1L),
    substr(x, pos + 1L, nchar(x)))
}

#' Internal helper: util redcap rule2 parse interval string
#'
#' @noRd
.util_redcap_rule2_parse_interval_string <- function(rule, debug = 0,
  must_eof = TRUE) {
  x <- trimws(rule)
  n <- nchar(x)
  if (n < 2L) {
    util_error("Invalid interval while parsing REDCap interval.",
      applicability_problem = TRUE,
      intrinsic_applicability_problem = FALSE)
  }

  open <- substr(x, 1L, 1L)
  close <- substr(x, n, n)

  if (!open %in% c("[", "(") || !close %in% c("]", ")")) {
    util_error("Expected interval while parsing REDCap interval.",
      applicability_problem = TRUE,
      intrinsic_applicability_problem = FALSE)
  }

  inner <- substr(x, 2L, n - 1L)
  parts <- .util_redcap_rule2_split_interval(inner)
  if (length(parts) != 2L) {
    util_error("Invalid interval while parsing REDCap interval.",
      applicability_problem = TRUE,
      intrinsic_applicability_problem = FALSE)
  }

  .util_redcap_rule2_paren(as.call(list(
    as.name("interval"),
    inc_l = identical(open, "["),
    low = .util_redcap_rule2_interval_endpoint(parts[[1L]], side = "low",
      debug = debug),
    upp = .util_redcap_rule2_interval_endpoint(parts[[2L]], side = "upp",
      debug = debug),
    inc_u = identical(close, "]")
  )))
}

#' Internal helper: util redcap rule2 tokenize standalone
#'
#' @noRd
.util_redcap_rule2_tokenize_standalone <- function(rule) {
  n <- nchar(rule)
  i <- 1L
  token_env <- new.env(parent = emptyenv())
  assign("out", list(), envir = token_env)

  add <- function(type, value = NULL, pos = i) {
    out <- get("out", envir = token_env, inherits = FALSE)
    out[[length(out) + 1L]] <- list(type = type, value = value, pos = pos)
    assign("out", out, envir = token_env)
  }

  while (i <= n) {
    ch <- substr(rule, i, i)

    if (grepl("\\s", ch, perl = TRUE)) {
      i <- i + 1L
      next
    }

    pos <- i

    if (ch == "[") {
      j_sq <- regexpr("\\]", substring(rule, i + 1L), perl = TRUE)[[1L]]
      j_pa <- regexpr("\\)", substring(rule, i + 1L), perl = TRUE)[[1L]]
      candidates <- c(
        if (j_sq > 0L) i + j_sq else NA_integer_,
        if (j_pa > 0L) i + j_pa else NA_integer_
      )
      j <- suppressWarnings(min(candidates, na.rm = TRUE))
      if (!is.finite(j)) {
        util_error("Unclosed square-bracket expression.",
          applicability_problem = TRUE,
          intrinsic_applicability_problem = FALSE)
      }

      inner <- substring(rule, i + 1L, j - 1L)
      close <- substr(rule, j, j)

      if (grepl(";", inner, fixed = TRUE)) {
        add("INTERVAL", list(open = "[", close = close, inner = inner), pos)
      } else {
        if (!identical(close, "]")) {
          util_error("Invalid square-bracket expression.",
            applicability_problem = TRUE,
            intrinsic_applicability_problem = FALSE)
        }
        add("ATOM", .util_redcap_rule2_square_value(inner), pos)
      }

      i <- j + 1L
      next
    }

    if (ch == "(") {
      j_sq <- regexpr("\\]", substring(rule, i + 1L), perl = TRUE)[[1L]]
      j_pa <- regexpr("\\)", substring(rule, i + 1L), perl = TRUE)[[1L]]
      candidates <- c(
        if (j_sq > 0L) i + j_sq else NA_integer_,
        if (j_pa > 0L) i + j_pa else NA_integer_
      )
      j <- suppressWarnings(min(candidates, na.rm = TRUE))

      if (is.finite(j)) {
        inner <- substring(rule, i + 1L, j - 1L)
        close <- substr(rule, j, j)
        if (grepl(";", inner, fixed = TRUE) && identical(close, "]")) {
          add("INTERVAL", list(open = "(", close = close, inner = inner), pos)
          i <- j + 1L
          next
        }
      }

      add("(", "(", pos)
      i <- i + 1L
      next
    }

    if (ch %in% c("\"", "'")) {
      quote <- ch
      j <- i + 1L
      val <- character()

      while (j <= n) {
        cj <- substr(rule, j, j)
        if (cj == "\\" && j < n) {
          val <- c(val, substr(rule, j, j + 1L))
          j <- j + 2L
          next
        }
        if (cj == quote) break
        val <- c(val, cj)
        j <- j + 1L
      }

      if (j > n) {
        util_error("Unclosed string literal.",
          applicability_problem = TRUE,
          intrinsic_applicability_problem = FALSE)
      }

      add("ATOM", .util_redcap_rule2_string_value(
        paste0(quote, paste0(val, collapse = ""), quote)
      ), pos)
      i <- j + 1L
      next
    }

    two <- if (i < n) substr(rule, i, i + 1L) else ""
    if (two %in% c("<=", ">=", "!=", "==", "<>", "**")) {
      add("OP", two, pos)
      i <- i + 2L
      next
    }

    if (ch %in% c(")", ",", ";", "{", "}", "+", "-", "*", "/", "^", "<", ">", "=")) { # nolint: line_length_linter.
      add(if (ch %in% c(")", ",", ";", "{", "}")) ch else "OP", ch, pos)
      i <- i + 1L
      next
    }

    if (grepl("[0-9.]", ch, perl = TRUE)) {
      m <- regexpr("^(?:[0-9]+(?:\\.[0-9]+)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?", substring(rule, i), perl = TRUE) # nolint: line_length_linter.
      if (m[[1L]] < 0L) {
        util_error("Invalid numeric literal.",
          applicability_problem = TRUE,
          intrinsic_applicability_problem = FALSE)
      }
      txt <- regmatches(substring(rule, i), m)
      add("ATOM", as.numeric(txt), pos)
      i <- i + nchar(txt)
      next
    }

    if (grepl("[A-Za-z_]", ch, perl = TRUE)) {
      m <- regexpr("^[A-Za-z_][A-Za-z0-9_.]*", substring(rule, i), perl = TRUE)
      txt <- regmatches(substring(rule, i), m)
      low <- tolower(txt)

      if (identical(low, "not")) {
        j <- i + nchar(txt)
        rest <- substring(rule, j)
        m2 <- regexpr("^\\s+in\\b", rest, perl = TRUE)
        if (m2[[1L]] > 0L) {
          add("OP", "not in", pos)
          i <- j + util_attr(m2, "match.length", exact = TRUE)
          next
        }
      }

      if (low %in% c("and", "or", "in")) {
        add("OP", low, pos)
      } else if (low %in% c("true", "false")) {
        add("ATOM", low == "true", pos)
      } else if (identical(low, "nan")) {
        add("ATOM", NaN, pos)
      } else {
        add("NAME", txt, pos)
      }

      i <- i + nchar(txt)
      next
    }

    util_error(sprintf("Unexpected character %s.", dQuote(ch)),
      applicability_problem = TRUE,
      intrinsic_applicability_problem = FALSE)
  }

  add("EOF", NULL, n + 1L)
  get("out", envir = token_env, inherits = FALSE)
}

#' Internal helper: util redcap rule2 node
#'
#' @noRd
.util_redcap_rule2_node <- function(expr, kind = "expr", grouped = FALSE) {
  list(expr = expr, kind = kind, grouped = grouped)
}

#' Internal helper: util redcap rule2 paren
#'
#' @noRd
.util_redcap_rule2_paren <- function(expr) {
  as.call(list(as.name("("), expr))
}

#' Internal helper: util redcap rule2 unparen
#'
#' @noRd
.util_redcap_rule2_unparen <- function(expr) {
  if (is.call(expr) && identical(expr[[1L]], as.name("(")) && length(expr) == 2L) { # nolint: line_length_linter.
    return(expr[[2L]])
  }
  expr
}

#' Internal helper: util redcap rule2 call name
#'
#' @noRd
.util_redcap_rule2_call_name <- function(expr) {
  expr <- .util_redcap_rule2_unparen(expr)
  if (is.call(expr)) {
    return(as.character(expr[[1L]]))
  }
  NA_character_
}

#' Internal helper: util redcap rule2 unstructure
#'
#' @noRd
.util_redcap_rule2_unstructure <- function(expr) {
  if (!is.null(util_attr(expr, "src", exact = TRUE))) {
    if (is.expression(expr) && length(expr) == 1L) {
      return(expr[[1L]])
    }
    attr(expr, "src") <- NULL
  }
  expr
}

#' Internal helper: util redcap rule2 parse tokens
#'
#' @noRd
.util_redcap_rule2_parse_tokens <- function(tokens, rule, debug = 0, must_eof = TRUE) { # nolint: line_length_linter.
  parser_env <- new.env(parent = emptyenv())
  assign("p", 1L, envir = parser_env)

  cur <- function() tokens[[get("p", envir = parser_env, inherits = FALSE)]]
  eat <- function(type = NULL, value = NULL) {
    tok <- cur()
    if (!is.null(type) && !identical(tok$type, type)) {
      util_error("Unexpected token while parsing REDCap rule.",
        applicability_problem = TRUE,
        intrinsic_applicability_problem = FALSE)
    }
    if (!is.null(value) && !identical(tok$value, value)) {
      util_error("Unexpected token value while parsing REDCap rule.",
        applicability_problem = TRUE,
        intrinsic_applicability_problem = FALSE)
    }
    assign("p", get("p", envir = parser_env, inherits = FALSE) + 1L,
      envir = parser_env)
    tok
  }

  op_info <- function(op) {
    switch(op,
      "or"     = list(bp = 10L, assoc = "left", fn = "or"),
      "and"    = list(bp = 20L, assoc = "left", fn = "and"),
      "="      = list(bp = 30L, assoc = "left", fn = "="),
      "=="     = list(bp = 30L, assoc = "left", fn = "=="),
      "!="     = list(bp = 30L, assoc = "left", fn = "!="),
      "<>"     = list(bp = 30L, assoc = "left", fn = "<>"),
      "<"      = list(bp = 30L, assoc = "left", fn = "<"),
      "<="     = list(bp = 30L, assoc = "left", fn = "<="),
      ">"      = list(bp = 30L, assoc = "left", fn = ">"),
      ">="     = list(bp = 30L, assoc = "left", fn = ">="),
      "in"     = list(bp = 30L, assoc = "left", fn = "in"),
      "not in" = list(bp = 30L, assoc = "left", fn = "not in"),
      "+"      = list(bp = 40L, assoc = "left", fn = "+"),
      "-"      = list(bp = 40L, assoc = "left", fn = "-"),
      "*"      = list(bp = 50L, assoc = "left", fn = "*"),
      "/"      = list(bp = 50L, assoc = "left", fn = "/"),
      "^"      = list(bp = 60L, assoc = "right", fn = "^"),
      "**"     = list(bp = 60L, assoc = "right", fn = "**"),
      NULL
    )
  }

  parse_expr <- function(min_bp = 0L) {
    tok <- cur()

    left <- switch(tok$type,
      ATOM = {
        eat("ATOM")
        if (is.name(tok$value)) {
          .util_redcap_rule2_node(tok$value, kind = "name")
        } else if (is.call(tok$value)) {
          .util_redcap_rule2_node(.util_redcap_rule2_paren(tok$value), kind = "call") # nolint: line_length_linter.
        } else {
          .util_redcap_rule2_node(tok$value, kind = "atom")
        }
      },
      NAME = {
        name <- tok$value
        eat("NAME")
        if (cur()$type == "(") {
          .util_redcap_rule2_dbg_fkt(debug, name)
          if (!.util_redcap_rule2_valid_function(name)) {
            util_error(sprintf("Unknown REDCap function: %s", name),
              applicability_problem = TRUE,
              intrinsic_applicability_problem = FALSE)
          }
          eat("(")
          args <- list()
          if (cur()$type != ")") {
            repeat {
              args[[length(args) + 1L]] <- parse_expr(0L)$expr
              .util_redcap_rule2_dbg_arg(debug, args[[length(args)]])
              if (cur()$type != ",") break
              eat(",")
            }
          }
          eat(")")
          .util_redcap_rule2_dbg_function_expression(debug, name)
          if (identical(name, "if") && length(args) == 3L) {
            .util_redcap_rule2_node(
              .util_redcap_rule2_paren(as.call(c(list(as.name("if")), args))),
              kind = "call"
            )
          } else {
            .util_redcap_rule2_node(
              .util_redcap_rule2_paren(as.call(c(list(as.name(name)), args))),
              kind = "call"
            )
          }
        } else {
          util_error("Unexpected bare name while parsing REDCap rule.",
            applicability_problem = TRUE,
            intrinsic_applicability_problem = FALSE)
        }
      },
      `(` = {
        eat("(")
        expr <- parse_expr(0L)
        # This intentionally accepts trailing '; ...' inside parentheses in the
        # same spirit as the qmrparser implementation used by the old parser.
        if (cur()$type == ";") {
          repeat {
            eat(";")
            if (cur()$type %in% c(")", "EOF")) break
            invisible(parse_expr(0L))
            if (cur()$type != ";") break
          }
        }
        eat(")")
        expr$expr <- .util_redcap_rule2_paren(expr$expr)
        expr$grouped <- TRUE
        expr
      },
      `{` = {
        eat("{")
        args <- list()
        if (cur()$type != "}") {
          repeat {
            args[[length(args) + 1L]] <- parse_expr(0L)$expr
            if (!cur()$type %in% c(",", ";")) break
            eat(cur()$type)
          }
        }
        eat("}")
        .util_redcap_rule2_node(
          .util_redcap_rule2_paren(as.call(c(list(as.name("set")), args))),
          kind = "call"
        )
      },
      INTERVAL = {
        eat("INTERVAL")
        parts <- .util_redcap_rule2_split_interval(tok$value$inner)
        if (length(parts) != 2L) {
          util_error("Invalid interval while parsing REDCap rule.",
            applicability_problem = TRUE,
            intrinsic_applicability_problem = FALSE)
        }
        low <- .util_redcap_rule2_interval_endpoint(parts[[1L]],
          side = "low",
          debug = debug)
        upp <- .util_redcap_rule2_interval_endpoint(parts[[2L]],
          side = "upp",
          debug = debug)
        .util_redcap_rule2_node(.util_redcap_rule2_paren(as.call(list(
          as.name("interval"),
          inc_l = identical(tok$value$open, "["),
          low = low,
          upp = upp,
          inc_u = identical(tok$value$close, "]")
        ))), kind = "call")
      },
      OP = {
        op <- tok$value
        if (!op %in% c("+", "-")) {
          util_error("Unexpected operator while parsing REDCap rule.",
            applicability_problem = TRUE,
            intrinsic_applicability_problem = FALSE)
        }
        eat("OP")
        if (cur()$type == "(") {
          eat("(")
          args <- list()
          if (cur()$type != ")") {
            repeat {
              args[[length(args) + 1L]] <- parse_expr(0L)$expr
              if (cur()$type != ",") break
              eat(",")
            }
          }
          eat(")")
          .util_redcap_rule2_node(
            .util_redcap_rule2_paren(as.call(c(list(as.name(op)), args))),
            kind = "call"
          )
        } else {
          .util_redcap_rule2_node(
            .util_redcap_rule2_paren(as.call(list(as.name(op), parse_expr(70L)$expr))), # nolint: line_length_linter.
            kind = "call"
          )
        }
      },
      util_error("Unexpected token while parsing REDCap rule.",
        applicability_problem = TRUE,
        intrinsic_applicability_problem = FALSE)
    )

    repeat {
      tok <- cur()
      if (tok$type != "OP") break
      info <- op_info(tok$value)
      if (is.null(info) || info$bp < min_bp) break
      eat("OP")
      next_min_bp <- if (identical(info$assoc, "left")) info$bp + 1L else info$bp # nolint: line_length_linter.
      right <- parse_expr(next_min_bp)

      left_expr <- left$expr
      if (identical(info$fn, "and") && identical(.util_redcap_rule2_call_name(left_expr), "and")) { # nolint: line_length_linter.
        left_expr <- .util_redcap_rule2_unparen(left_expr)
      }
      if (identical(info$fn, "or") && identical(.util_redcap_rule2_call_name(left_expr), "or")) { # nolint: line_length_linter.
        left_expr <- .util_redcap_rule2_unparen(left_expr)
      }
      if (identical(info$fn, "+") && identical(.util_redcap_rule2_call_name(left_expr), "+")) { # nolint: line_length_linter.
        left_expr <- .util_redcap_rule2_unparen(left_expr)
      }

      left <- .util_redcap_rule2_node(
        .util_redcap_rule2_paren(as.call(list(as.name(info$fn), left_expr, right$expr))), # nolint: line_length_linter.
        kind = "call"
      )
    }

    left
  }

  node <- parse_expr(0L)
  if (cur()$type != "EOF") {
    if (must_eof) {
      util_error("Unexpected trailing input while parsing REDCap rule.",
        applicability_problem = TRUE,
        intrinsic_applicability_problem = FALSE)
    }
    parsed_call <- .util_redcap_rule2_unparen(node$expr)
    if (!(is.call(parsed_call) && identical(as.character(parsed_call[[1L]]), "interval"))) { # nolint: line_length_linter.
      util_error("Unexpected trailing input while parsing REDCap rule.",
        applicability_problem = TRUE,
        intrinsic_applicability_problem = FALSE)
    }
  }

  expr <- node$expr
  if (!node$grouped && node$kind == "name") {
    return(structure(as.expression(expr), src = rule))
  }
  if (!node$grouped && node$kind == "atom") {
    return(structure(expr, src = rule))
  }

  expr
}


#' Internal helper: util redcap rule2 parse arg part
#'
#' @noRd
.util_redcap_rule2_parse_arg_part <- function(tokens, rule, debug = 0,
  must_eof = TRUE) {
  p <- 1L
  args <- list()
  n <- length(tokens)

  repeat {
    start <- p
    level <- 0L
    while (p <= n && tokens[[p]]$type != "EOF") {
      typ <- tokens[[p]]$type
      if (typ %in% c("(", "{")) {
        level <- level + 1L
      } else if (typ %in% c(")", "}")) {
        level <- max(0L, level - 1L)
      } else if (identical(typ, ",") && level == 0L) {
        break
      }
      p <- p + 1L
    }

    part_tokens <- c(tokens[start:(p - 1L)],
      list(list(type = "EOF", value = NULL, pos = nchar(rule) + 1L)))
    args[[length(args) + 1L]] <-
      .util_redcap_rule2_parse_tokens(part_tokens, rule, debug = debug,
        must_eof = TRUE)

    if (p > n || tokens[[p]]$type == "EOF") {
      break
    }
    if (!identical(tokens[[p]]$type, ",")) {
      util_error("Unexpected token while parsing REDCap arg_part.",
        applicability_problem = TRUE,
        intrinsic_applicability_problem = FALSE)
    }
    p <- p + 1L
  }

  if (must_eof && p <= n && tokens[[p]]$type != "EOF") {
    util_error("Unexpected trailing input while parsing REDCap arg_part.",
      applicability_problem = TRUE,
      intrinsic_applicability_problem = FALSE)
  }

  as.call(c(list(as.name("list")), args))
}


#' Internal helper: util redcap rule2 parse interval entry
#'
#' @noRd
.util_redcap_rule2_parse_interval_entry <- function(tokens, rule, debug = 0,
  must_eof = TRUE) {
  if (!identical(tokens[[1L]]$type, "INTERVAL")) {
    util_error("Expected interval while parsing REDCap interval.",
      applicability_problem = TRUE,
      intrinsic_applicability_problem = FALSE)
  }
  if (must_eof && !identical(tokens[[2L]]$type, "EOF")) {
    util_error("Unexpected trailing input while parsing REDCap interval.",
      applicability_problem = TRUE,
      intrinsic_applicability_problem = FALSE)
  }

  tok <- tokens[[1L]]
  parts <- .util_redcap_rule2_split_interval(tok$value$inner)
  if (length(parts) != 2L) {
    util_error("Invalid interval while parsing REDCap interval.",
      applicability_problem = TRUE,
      intrinsic_applicability_problem = FALSE)
  }

  .util_redcap_rule2_paren(as.call(list(
    as.name("interval"),
    inc_l = identical(tok$value$open, "["),
    low = .util_redcap_rule2_interval_endpoint(parts[[1L]], side = "low",
      debug = debug),
    upp = .util_redcap_rule2_interval_endpoint(parts[[2L]], side = "upp",
      debug = debug),
    inc_u = identical(tok$value$close, "]")
  )))
}

#' Internal helper: util parse redcap rule standalone
#'
#' @noRd
.util_parse_redcap_rule_standalone <- function(rule, debug = 0,
  entry_pred = "REDcapPred",
  must_eof = TRUE) {
  if (!entry_pred %in% .util_redcap_rule2_valid_entry_preds) {
    util_error(sprintf("Unknown parser entry predicate: %s", entry_pred),
      applicability_problem = FALSE,
      intrinsic_applicability_problem = FALSE)
  }

  if (identical(entry_pred, "interval")) {
    out <- tryCatch(
      .util_redcap_rule2_parse_interval_string(rule, debug = debug,
        must_eof = must_eof),
      error = function(e) .util_redcap_rule2_parse_fail(rule)
    )
    ok <- !identical(out, .util_redcap_rule2_parse_fail(rule))
    .util_redcap_rule2_dbg_result(debug, out)
    if (!ok) {
      .util_redcap_rule2_warn_parse(rule, "interval")
    }
    return(out)
  }

  tokens <- tryCatch(
    .util_redcap_rule2_tokenize_standalone(rule),
    error = function(e) NULL
  )
  if (is.null(tokens)) {
    .util_redcap_rule2_dbg_result(debug, .util_redcap_rule2_parse_fail(rule))
    .util_redcap_rule2_warn_parse(rule, "rule")
    return(.util_redcap_rule2_parse_fail(rule))
  }

  out <- tryCatch({
    if (identical(entry_pred, "interval")) {
      .util_redcap_rule2_parse_interval_entry(tokens, rule, debug = debug,
        must_eof = must_eof)
    } else if (identical(entry_pred, "arg_part")) {
      .util_redcap_rule2_parse_arg_part(tokens, rule, debug = debug,
        must_eof = must_eof)
    } else {
      .util_redcap_rule2_parse_tokens(tokens, rule, debug = debug,
        must_eof = must_eof)
    }
  }, error = function(e) .util_redcap_rule2_parse_fail(rule))

  ok <- !identical(out, .util_redcap_rule2_parse_fail(rule))
  .util_redcap_rule2_dbg_result(debug, out)
  if (!ok) {
    loose <- tryCatch({
      if (identical(entry_pred, "interval")) {
        .util_redcap_rule2_parse_interval_entry(tokens, rule, debug = 0,
          must_eof = FALSE)
      } else if (identical(entry_pred, "arg_part")) {
        .util_redcap_rule2_parse_arg_part(tokens, rule, debug = 0,
          must_eof = FALSE)
      } else {
        .util_redcap_rule2_parse_tokens(tokens, rule, debug = 0,
          must_eof = FALSE)
      }
    }, error = function(e) .util_redcap_rule2_parse_fail(rule))
    if (must_eof && !identical(loose, .util_redcap_rule2_parse_fail(rule))) {
      .util_redcap_rule2_warn_extra(rule)
    } else {
      .util_redcap_rule2_warn_parse(rule, if (identical(entry_pred, "interval")) "interval" else "rule") # nolint: line_length_linter.
    }
  }

  out
}

.redcap_cache <- new.env(parent = emptyenv())
# nolint start: line_length_linter.
#' Interpret a `REDcap`-style rule and create an expression, that represents this rule
#'
#' @param rule [character] `REDcap` style rule
#' @param debug [integer] debug level (0 = off, 1 = log, 2 = breakpoints)
#' @param entry_pred [character] for debugging reasons: The production
#'        rule used entry point for the parser
#' @param must_eof [logical] if `TRUE`, expect the input to be `eof`, when the
#'        parser succeeded, fail, if not.
#'
#' @return [expression] the interpreted rule
#'
#' [`REDcap` rules 1](https://help.redcap.ualberta.ca/help-and-faq/project-best-practices/data-quality/example-data-quality-rules)
#' [`REDcap` rules 2](https://docs.google.com/document/d/1l3nGBgqqPKi5PtMe75g7q0dny8QzGMd_/edit?tab=t.0)
#'
#'
#' For resolving left-recursive rules,
#' [StackOverflow](https://stackoverflow.com/a/9934631)
#' helps understanding the grammar below, just in case, theoretical computer
#' science is not right in your mind currently.
#'
#'
#' @examples
#' \dontrun{
#' #  rules:
#' # pregnancies <- 9999 ~ SEX == 'm' |  is.na(SEX)
#' # pregnancies <- 9998 ~ AGE < 12 |  is.na(AGE)
#' # pregnancies = 9999 ~ dist > 2 |  speed == 0
#'
#' data.frame(target = "SEX_0",
#'   rule = '[speed] > 5 and [dist] > 42 or 1 = "2"',
#'   CODE = 99999, LABEL = "PREGNANCIES_NOT_ASSESSED FOR MALES",
#'   class = "JUMP")
#' ModifyiedStudyData <- replace in SEX_0 where SEX_0 is empty, if rule fits
#' ModifyedMetaData <- add missing codes with labels and class here
#'
#' subset(study_data, eval(pregnancies[[3]]))
#'
#' rule <-
#'  paste0('[con_consentdt] <> "" and [sda_osd1dt] <> "" and',
#'  ' datediff([con_consentdt],[sda_osd1dt],"d",true) < 0')
#'
#' x <- data.frame(con_consentdt = c(util_parse_date("2020-01-01"),
#'                 util_parse_date("2020-10-20")),
#'                 sda_osd1dt = c(util_parse_date("2020-01-20"),
#'                 util_parse_date("2020-10-01")))
#' eval(util_parse_redcap_rule(paste0(
#'   '[con_consentdt] <> "" and [sda_osd1dt] <> "" and ',
#'   'datediff([con_consentdt],[sda_osd1dt],"d", "Y-M-D",true) < 10')),
#'   x, util_get_redcap_rule_env())
#'
#' util_parse_redcap_rule("[a] = 12 or [b] = 13")
#' cars[eval(util_parse_redcap_rule(
#'   rule = '[speed] > 5 and [dist] > 42 or 1 = "2"'), cars,
#'   util_get_redcap_rule_env()), ]
#' cars[eval(util_parse_redcap_rule(
#'   rule = '[speed] > 5 and [dist] > 42 or 2 = "2"'), cars,
#'   util_get_redcap_rule_env()), ]
#' cars[eval(util_parse_redcap_rule(
#'   rule = '[speed] > 5 or [dist] > 42 and 1 = "2"'), cars,
#'   util_get_redcap_rule_env()), ]
#' cars[eval(util_parse_redcap_rule(
#'   rule = '[speed] > 5 or [dist] > 42 and 2 = "2"'), cars,
#'   util_get_redcap_rule_env()), ]
#' util_parse_redcap_rule(rule = '(1 = "2" or true) and (false)')
#' eval(util_parse_redcap_rule(rule =
#'   '[dist] > sum(1, +(2, [dist] + 5), [speed]) + 3 + [dist]'),
#' cars, util_get_redcap_rule_env())
#' }
#'
#' @family parser_functions
#' @concept metadata_management
#' @noRd
# nolint end

util_parse_redcap_rule <- function(rule, debug = 0, entry_pred = "REDcapPred",
  must_eof = FALSE) {
  util_expect_scalar(must_eof, check_type = is.logical)

  # entry_pred is kept for API compatibility with the former qmrparser-based
  # implementation. For the dedicated interval entry point, require an interval
  # as the first token; other entry points use the normal REDcap predicate path.
  .util_parse_redcap_rule_standalone(rule = rule, debug = debug,
    entry_pred = entry_pred,
    must_eof = must_eof)
}
