# util_expand_pattern_rules.R
# This file contains a helper to expand pattern-based variable references
# inside cross-item rules. Only content inside square brackets [ ... ] is
# interpreted as a pattern. Everything else is left unchanged.

#' Internal helper: expand pattern rules
#'
#' @noRd
util_expand_pattern_rules <- function(pattern_rules, valid_names,
  meta_data = NULL, context = NULL) {
  e <- environment()

  mismatches <- list()

  # Basic input checks
  util_stop_if_not(is.character(pattern_rules), is.character(valid_names))
  if (any(inv <- grepl("[][]", valid_names))) {
    util_message(
      c(
        "Some variable names or labels contain '[' or ']'.",
        "These names are ignored for pattern-based rule expansion",
        "because square brackets are reserved by the rule-template",
        "language."
      ),
      applicability_problem = TRUE,
      once_id = "pattern_rules_square_brackets_in_valid_names"
    )
    valid_names <- valid_names[!inv]
  }

  # Escape regex meta characters to ensure user input cannot alter regex logic
  .rx_escape_chr <- function(x) {
    gsub("([][{}()+*?^$|\\\\.,])", "\\\\\\1", x, perl = TRUE)
  }

  # Read escaped character (e.g. \*, \?, \#)
  .read_escaped <- function(x, i) {
    n <- nchar(x)
    if (i < n && substr(x, i, i) == "\\") {
      list(value = substr(x, i + 1L, i + 1L), next_i = i + 2L)
    } else {
      list(value = substr(x, i, i), next_i = i + 1L)
    }
  }

  # Find closing brace for capture definitions { ... }
  .find_unescaped <- function(x, char, start) {
    n <- nchar(x)
    i <- start
    while (i <= n) {
      if (substr(x, i, i) == "\\") {
        i <- i + 2L
      } else if (substr(x, i, i) == char) {
        return(i)
      } else {
        i <- i + 1L
      }
    }
    NA_integer_
  }

  # Resolve capture references such as {W}, {W-1}, or {W+1}.
  # Arithmetic is only attempted for integer capture values.
  .resolve_capture_reference <- function(cap_expr, captures) {
    m <- regexec("^([A-Za-z][A-Za-z0-9_]*)([+-][0-9]+)?$", cap_expr, perl = TRUE) # nolint: line_length_linter.
    mm <- regmatches(cap_expr, m)[[1L]]

    if (!length(mm)) {
      util_error("Invalid capture reference {%s}.", cap_expr)
    }

    ref_name <- mm[[2L]]
    offset <- if (length(mm) >= 3L && nzchar(mm[[3L]])) {
      as.integer(mm[[3L]])
    } else {
      0L
    }

    if (is.null(captures[[ref_name]])) {
      util_error("Capture {%s} used before being defined.", ref_name)
    }

    ref_value <- captures[[ref_name]]

    if (offset != 0L) {
      if (!grepl("^-?[0-9]+$", ref_value)) {
        return(NULL)
      }
      ref_value <- as.character(as.integer(ref_value) + offset)
    }

    ref_value
  }

  # Translate wildcard mini-language into regex
  .compile_fragment <- function(x) {
    rx <- character()
    i <- 1L
    n <- nchar(x)

    while (i <= n) {
      ch <- substr(x, i, i)
      ch2 <- if (i < n) substr(x, i, i + 1L) else ""

      if (ch == "\\") {
        esc <- .read_escaped(x, i)
        rx <- c(rx, .rx_escape_chr(esc$value))
        i <- esc$next_i
      } else if (ch2 == "#?") {
        rx <- c(rx, "[0-9]{1,2}")
        i <- i + 2L
      } else if (ch2 == "##") {
        rx <- c(rx, "[0-9]{2}")
        i <- i + 2L
      } else if (ch == "#") {
        rx <- c(rx, "[0-9]")
        i <- i + 1L
      } else if (ch == "*") {
        rx <- c(rx, ".*")
        i <- i + 1L
      } else if (ch == "?") {
        rx <- c(rx, ".")
        i <- i + 1L
      } else {
        rx <- c(rx, .rx_escape_chr(ch))
        i <- i + 1L
      }
    }

    paste0(rx, collapse = "")
  }

  # Compile full variable pattern with optional capture groups
  .compile_var_pattern <- function(pattern, captures = list()) {
    rx <- character()
    capture_names <- character()
    pattern_invalid <- FALSE
    i <- 1L
    n <- nchar(pattern)

    while (i <= n) {
      ch <- substr(pattern, i, i)

      if (ch == "\\") {
        esc <- .read_escaped(pattern, i)
        rx <- c(rx, .rx_escape_chr(esc$value))
        i <- esc$next_i
      } else if (ch == "{") {
        close <- .find_unescaped(pattern, "}", i + 1L)
        if (is.na(close)) util_error("Unclosed capture in pattern")

        inner <- substr(pattern, i + 1L, close - 1L)
        def <- strsplit(inner, ":", fixed = TRUE)[[1L]]
        cap_name <- def[[1L]]

        if (length(def) == 1L) {
          ref_value <- .resolve_capture_reference(cap_name, captures)

          if (is.null(ref_value)) {
            pattern_invalid <- TRUE
            rx <- c(rx, "(?!)")
          } else {
            rx <- c(rx, .rx_escape_chr(ref_value))
          }
        } else if (!is.null(captures[[cap_name]])) {
          rx <- c(rx, .rx_escape_chr(captures[[cap_name]]))
        } else {
          rx <- c(rx, "(", .compile_fragment(def[[2L]]), ")")
          capture_names <- c(capture_names, cap_name)
        }

        i <- close + 1L
      } else {
        next_special <- regexpr("[\\\\{]", substr(pattern, i, n), perl = TRUE)[[1L]] # nolint: line_length_linter.
        end <- if (next_special < 0L) n else i + next_special - 2L
        rx <- c(rx, .compile_fragment(substr(pattern, i, end)))
        i <- end + 1L
      }
    }

    list(
      regex = paste0("^", paste0(rx, collapse = ""), "$"),
      capture_names = capture_names,
      pattern_invalid = pattern_invalid
    )
  }

  # Expand a single token against valid names
  .expand_token <- function(token, captures = list()) {
    group_hits <- .expand_group_token(token)
    if (!is.null(group_hits)) {
      return(lapply(group_hits, function(nm) {
        list(name = nm, captures = captures)
      }))
    }

    compiled <- .compile_var_pattern(token, captures)

    hits <- valid_names[grepl(compiled$regex, valid_names, perl = TRUE)]

    if (!length(hits)) {
      return(structure(list(), pattern_invalid = compiled$pattern_invalid))
    }

    lapply(hits, function(nm) {
      new_captures <- captures

      if (length(compiled$capture_names)) {
        m <- regexec(compiled$regex, nm, perl = TRUE)
        vals <- regmatches(nm, m)[[1L]][-1L]

        for (i in seq_along(compiled$capture_names)) {
          new_captures[[compiled$capture_names[[i]]]] <- vals[[i]]
        }
      }

      list(name = nm, captures = new_captures)
    })
  }

  .expand_group_token <- function(token) {
    token <- trimws(token)
    where_hits <- .expand_where_token(token)
    if (!is.null(where_hits)) {
      return(where_hits)
    }

    if (identical(token, "ALL")) {
      if (is.data.frame(meta_data) && VAR_NAMES %in% colnames(meta_data)) {
        all_vars <- meta_data[[VAR_NAMES]]
        return(all_vars[!util_empty(all_vars)])
      }
      return(valid_names)
    }

    segment_match <- regexec(
      "^SEGMENT(?::([^][]+))?$",
      token,
      perl = TRUE
    )
    segment_parts <- regmatches(token, segment_match)[[1L]]
    dataframe_match <- regexec(
      "^DATAFRAME(?::([^][]+))?$",
      token,
      perl = TRUE
    )
    dataframe_parts <- regmatches(token, dataframe_match)[[1L]]
    if (!length(segment_parts) && !length(dataframe_parts)) {
      return(NULL)
    }

    if (is.null(meta_data) || !is.data.frame(meta_data)) {
      util_warning(
        "%s needs item-level metadata and was ignored.",
        paste0("[", token, "]"),
        applicability_problem = TRUE
      )
      return(character(0))
    }

    group_column <- if (length(segment_parts)) {
      STUDY_SEGMENT
    } else {
      DATAFRAMES
    }
    group_label <- if (length(segment_parts)) {
      "SEGMENT"
    } else {
      "DATAFRAME"
    }
    group_parts <- if (length(segment_parts)) {
      segment_parts
    } else {
      dataframe_parts
    }

    if (!group_column %in% colnames(meta_data)) {
      util_warning(
        "%s needs %s in item-level metadata and was ignored.",
        paste0("[", token, "]"),
        sQuote(group_column),
        applicability_problem = TRUE
      )
      return(character(0))
    }

    has_group_name <- length(group_parts) >= 2L && nzchar(group_parts[2])
    group <- if (has_group_name) {
      trimws(group_parts[2])
    } else if (is.data.frame(context) && group_column %in% colnames(context)) {
      context[[group_column]][[1L]]
    } else if (is.list(context) && group_column %in% names(context)) {
      context[[group_column]][[1L]]
    } else {
      NA_character_
    }

    if (util_empty(group)) {
      util_warning(
        "%s needs %s in the current metadata row and was ignored.",
        paste0("[", token, "]"),
        sQuote(group_column),
        applicability_problem = TRUE
      )
      return(character(0))
    }

    group_vars <- meta_data[
      meta_data[[group_column]] == group,
      VAR_NAMES,
      drop = TRUE
    ]
    group_vars <- group_vars[!util_empty(group_vars)]
    if (!length(group_vars)) {
      util_warning(
        "%s does not match any variables in %s %s.",
        paste0("[", token, "]"),
        sQuote(group_label),
        sQuote(group),
        applicability_problem = TRUE
      )
    }
    group_vars
  }

  .expand_where_token <- function(token) {
    token <- trimws(token)
    where_match <- regexec("^WHERE[[:space:]]*\\{(.*)\\}[[:space:]]*$",
      token,
      perl = TRUE
    )
    where_parts <- regmatches(token, where_match)[[1L]]
    if (!length(where_parts)) {
      return(NULL)
    }

    if (is.null(meta_data) || !is.data.frame(meta_data)) {
      util_warning(
        "%s needs item-level metadata and was ignored.",
        paste0("[", token, "]"),
        applicability_problem = TRUE
      )
      return(character(0))
    }
    if (!VAR_NAMES %in% colnames(meta_data)) {
      util_warning(
        "%s needs %s in item-level metadata and was ignored.",
        paste0("[", token, "]"),
        sQuote(VAR_NAMES),
        applicability_problem = TRUE
      )
      return(character(0))
    }

    where_rule <- trimws(where_parts[2L])
    parsed_rule <- try(
      suppressWarnings(util_parse_redcap_rule(where_rule)),
      silent = TRUE
    )
    if (util_is_try_error(parsed_rule) ||
        !(is.language(parsed_rule) || is.atomic(parsed_rule))) {
      util_warning(
        "%s could not be parsed as a REDCap-like metadata rule.",
        paste0("[", token, "]"),
        applicability_problem = TRUE
      )
      return(character(0))
    }

    matches <- try(
      eval(parsed_rule, meta_data, util_get_redcap_rule_env()),
      silent = TRUE
    )
    if (util_is_try_error(matches)) {
      util_warning(
        "%s could not be evaluated against item-level metadata.",
        paste0("[", token, "]"),
        applicability_problem = TRUE
      )
      return(character(0))
    }
    if (!is.logical(matches) || length(matches) != nrow(meta_data)) {
      util_warning(
        "%s did not evaluate to one logical value per item-level metadata row.",
        paste0("[", token, "]"),
        applicability_problem = TRUE
      )
      return(character(0))
    }

    matches[is.na(matches)] <- FALSE
    where_vars <- meta_data[[VAR_NAMES]][matches]
    where_vars <- where_vars[!util_empty(where_vars)]
    if (!length(where_vars)) {
      util_warning(
        "%s does not match any variables in item-level metadata.",
        paste0("[", token, "]"),
        applicability_problem = TRUE
      )
    }
    where_vars
  }

  # Expand full rule
  .extract_bracket_tokens <- function(rule) {
    if (is.na(rule)) {
      return(list())
    }
    n <- nchar(rule)
    if (!n) {
      return(list())
    }
    tokens <- list()
    i <- 1L
    while (i <= n) {
      if (substr(rule, i, i) != "[") {
        i <- i + 1L
        next
      }

      start <- i
      i <- i + 1L
      brace_depth <- 0L
      while (i <= n) {
        ch <- substr(rule, i, i)
        if (ch == "{") {
          brace_depth <- brace_depth + 1L
        } else if (ch == "}" && brace_depth > 0L) {
          brace_depth <- brace_depth - 1L
        } else if (ch == "]" && brace_depth == 0L) {
          raw <- substr(rule, start, i)
          token <- substr(raw, 2L, nchar(raw) - 1L)
          if (nzchar(token)) {
            tokens[[length(tokens) + 1L]] <- list(
              raw = raw,
              token = token
            )
          }
          break
        }
        i <- i + 1L
      }
      if (i > n) {
        raw <- substr(rule, start, n)
        tokens[[length(tokens) + 1L]] <- list(
          raw = raw,
          token = substr(raw, 2L, nchar(raw))
        )
      }
      i <- i + 1L
    }
    tokens
  }

  .expand_rule <- function(rule) {
    tokens <- .extract_bracket_tokens(rule)
    states <- list(list(text = rule, captures = list()))

    for (token_def in tokens) {
      token <- token_def$token
      new_states <- list()
      token_had_hit <- FALSE

      for (state in states) {
        hits <- .expand_token(token, state$captures)

        if (length(hits) ||
            isTRUE(util_attr(hits, "pattern_invalid", exact = TRUE))) {
          token_had_hit <- TRUE
        }

        for (hit in hits) {
          new_states[[length(new_states) + 1L]] <- list(
            text = sub(
              token_def$raw,
              paste0("[", hit$name, "]"),
              state$text,
              fixed = TRUE
            ),
            captures = hit$captures
          )
        }
      }

      if (!token_had_hit) {
        assign("mismatches",
          c(e$mismatches, token),
          envir = e
        )
      }

      states <- new_states
      if (!length(states)) break
    }

    unique(vapply(states, `[[`, character(1), "text"))
  }

  res <- force(unlist(lapply(pattern_rules, .expand_rule), use.names = FALSE))

  util_attach_attr(
    res,
    mismatches = unique(mismatches)
  )
}
