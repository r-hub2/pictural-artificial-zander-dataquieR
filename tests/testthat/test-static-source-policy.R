skip_on_cran()

find_unsafe_format_string_calls <- function(path) {
  format_calls <- c(
    "sprintf", "gettextf",
    "util_error", "util_warning", "util_message", "util_user_hint"
  )
  trusted_format_parameters <- list(
    util_as_numeric = "warn"
  )
  control_args <- c(
    "applicability_problem", "intrinsic_applicability_problem",
    "integrity_indicator", "level", "immediate", "title",
    "additional_classes", "varname", "once_id", "call.", "call"
  )

  call_name <- function(expr) {
    if (!is.call(expr)) {
      return("")
    }
    head <- expr[[1]]
    if (is.symbol(head)) {
      return(as.character(head))
    }
    if (is.call(head) && identical(as.character(head[[1]]), "::") &&
        length(head) == 3L) {
      return(paste(as.character(head[[2]]), as.character(head[[3]]), sep = "::")) # nolint: line_length_linter.
    }
    ""
  }

  is_string_literal <- function(expr) {
    is.character(expr) && length(expr) == 1L
  }

  is_literal_format_arg <- function(expr) {
    if (is_string_literal(expr)) {
      return(TRUE)
    }
    is.call(expr) &&
      identical(call_name(expr), "c") &&
      length(expr) == 2L &&
      is_string_literal(expr[[2L]])
  }

  has_format_args <- function(expr, name) {
    fmt_pos <- format_arg_pos(expr, name)
    if (is.na(fmt_pos) || length(expr) <= fmt_pos) {
      return(FALSE)
    }
    if (name %in% c("sprintf", "gettextf")) {
      return(TRUE)
    }
    arg_names <- names(as.list(expr))[-seq_len(fmt_pos)]
    any(is.na(arg_names) | arg_names == "" | !(arg_names %in% control_args))
  }

  format_arg_pos <- function(expr, name) {
    args <- as.list(expr)[-1L]
    arg_names <- names(args)
    if (name %in% c("sprintf", "gettextf")) {
      fmt <- which(arg_names %in% c("fmt", "format"))
      if (length(fmt)) {
        return(fmt[[1L]] + 1L)
      }
      return(2L)
    }
    actual <- which(is.na(arg_names) | arg_names == "" |
        !(arg_names %in% control_args))
    if (!length(actual)) {
      return(NA_integer_)
    }
    actual[[1L]] + 1L
  }

  classify <- function(expr, bindings, current_function, formals) {
    if (is_string_literal(expr)) {
      return("format")
    }
    if (is.symbol(expr)) {
      nm <- as.character(expr)
      if (nm %in% names(bindings)) {
        return(bindings[[nm]])
      }
      if (nm %in% formals &&
          nm %in% trusted_format_parameters[[current_function]]) {
        return("format")
      }
      return("unknown")
    }
    if (!is.call(expr)) {
      return("unknown")
    }

    nm <- call_name(expr)
    if (nm %in% c(
      "try", "tryCatch", "util_condition_from_try_error",
      "errorCondition", "warningCondition", "messageCondition",
      "rlang::error_cnd", "rlang::warning_cnd", "rlang::message_cnd"
    )) {
      return("condition")
    }
    if (nm == "util_attr" && length(expr) >= 3L &&
        is_string_literal(expr[[3L]]) && identical(expr[[3L]], "condition")) {
      return("condition")
    }
    if (nm %in% c("c", "paste", "paste0", "sprintf", "gettextf")) {
      return("format")
    }
    if (startsWith(nm, "cli::")) {
      cls <- vapply(as.list(expr)[-1L], classify, character(1),
        bindings = bindings,
        current_function = current_function,
        formals = formals
      )
      if (any(cls == "unknown")) {
        return("unknown")
      }
      return("format")
    }
    "unknown"
  }

  expr_line <- function(expr) {
    sr <- attr(expr, "srcref")
    if (is.null(sr)) {
      return(NA_integer_)
    }
    as.integer(sr[[1L]])
  }

  expr_text <- function(expr) {
    paste(deparse(expr, width.cutoff = 120), collapse = " ")
  }

  walk <- function(expr, file, bindings = list(), current_function = "",
    formals = character()) {
    violations <- character()
    if (!is.call(expr)) {
      return(list(violations = violations, bindings = bindings))
    }

    nm <- call_name(expr)
    if (nm == "{" && length(expr) >= 2L) {
      for (i in seq_along(expr)[-1L]) {
        res <- walk(expr[[i]], file, bindings, current_function, formals)
        violations <- c(violations, res$violations)
        bindings <- res$bindings
      }
      return(list(violations = violations, bindings = bindings))
    }

    if (nm == "<-" && length(expr) == 3L &&
        is.symbol(expr[[2L]])) {
      lhs <- as.character(expr[[2L]])
      rhs <- expr[[3L]]
      if (is.call(rhs) && identical(call_name(rhs), "function")) {
        fn_formals <- names(as.list(rhs[[2L]]))
        return(walk(rhs[[3L]], file, list(), lhs, fn_formals))
      }
      bindings[[lhs]] <- classify(rhs, bindings, current_function, formals)
    }

    if (nm %in% format_calls && length(expr) >= 2L) {
      fmt_pos <- format_arg_pos(expr, nm)
      if (is.na(fmt_pos)) {
        return(list(violations = violations, bindings = bindings))
      }
      fmt <- expr[[fmt_pos]]
      cls <- classify(fmt, bindings, current_function, formals)
      if (nm %in% c("sprintf", "gettextf") &&
          !is_literal_format_arg(fmt) &&
          !has_format_args(expr, nm)) {
        rel_file <- sub(
          paste0("^", normalizePath(dirname(path)), "/"),
          "",
          normalizePath(file)
        )
        violations <- c(violations, sprintf(
          "%s:%s: %s; non-literal sprintf/gettextf format strings need explicit values. If this is finished text, use sprintf(\"%%s\", x) or the text directly.", # nolint: line_length_linter.
          rel_file,
          expr_line(expr),
          expr_text(expr)
        ))
      }
      if (identical(cls, "unknown")) {
        rel_file <- sub(
          paste0("^", normalizePath(dirname(path)), "/"),
          "",
          normalizePath(file)
        )
        violations <- c(violations, sprintf(
          "%s:%s: %s; first argument is not a trusted package format string. If it is finished text, call %s(\"%%s\", x); if it is intentionally a package format string, add an explicit allowlist entry with a rationale.", # nolint: line_length_linter.
          rel_file,
          expr_line(expr),
          expr_text(expr),
          nm
        ))
      }
      if (identical(cls, "condition") && has_format_args(expr, nm)) {
        rel_file <- sub(
          paste0("^", normalizePath(dirname(path)), "/"),
          "",
          normalizePath(file)
        )
        violations <- c(violations, sprintf(
          "%s:%s: %s; condition objects must not be mixed with sprintf arguments.", # nolint: line_length_linter.
          rel_file,
          expr_line(expr),
          expr_text(expr)
        ))
      }
    }

    for (i in seq_along(expr)[-1L]) {
      res <- walk(
        expr[[i]], file, bindings, current_function, formals
      )
      violations <- c(violations, res$violations)
    }
    list(violations = violations, bindings = bindings)
  }

  r_files <- list.files(path,
    pattern = "[.][Rr]$", recursive = TRUE,
    full.names = TRUE
  )
  violations <- lapply(r_files, function(file) {
    expressions <- parse(file, keep.source = TRUE)
    unlist(lapply(expressions, function(expr) {
      walk(expr, file)$violations
    }), use.names = FALSE)
  })
  unlist(violations, use.names = FALSE)
}

find_direct_base_condition_calls <- function(path) {
  condition_calls <- c(
    "stop", "warning", "message", "stopifnot",
    "base::stop", "base::warning", "base::message", "base::stopifnot"
  )
  allowed_files <- c(
    "util_error.R",
    "util_message.R",
    "util_warning.R"
  )

  call_name <- function(expr) {
    if (!is.call(expr)) {
      return("")
    }
    head <- expr[[1]]
    if (is.symbol(head)) {
      return(as.character(head))
    }
    if (is.call(head) && identical(as.character(head[[1]]), "::") &&
        length(head) == 3L) {
      return(paste(as.character(head[[2]]), as.character(head[[3]]), sep = "::")) # nolint: line_length_linter.
    }
    ""
  }

  expr_line <- function(expr) {
    sr <- attr(expr, "srcref")
    if (is.null(sr)) {
      return(NA_integer_)
    }
    as.integer(sr[[1L]])
  }

  expr_text <- function(expr) {
    paste(deparse(expr, width.cutoff = 120), collapse = " ")
  }

  walk <- function(expr, file) {
    violations <- character()
    if (!is.call(expr)) {
      return(violations)
    }
    nm <- call_name(expr)
    if (nm %in% condition_calls &&
        !(basename(file) %in% allowed_files)) {
      rel_file <- sub(
        paste0("^", normalizePath(dirname(path)), "/"),
        "",
        normalizePath(file)
      )
      violations <- c(violations, sprintf(
        "%s:%s: %s",
        rel_file,
        expr_line(expr),
        expr_text(expr)
      ))
    }
    for (i in seq_along(expr)[-1L]) {
      violations <- c(violations, walk(expr[[i]], file))
    }
    violations
  }

  r_files <- list.files(path,
    pattern = "[.][Rr]$", recursive = TRUE,
    full.names = TRUE
  )
  violations <- lapply(r_files, function(file) {
    expressions <- parse(file, keep.source = TRUE)
    unlist(lapply(expressions, function(expr) {
      walk(expr, file)
    }), use.names = FALSE)
  })
  unlist(violations, use.names = FALSE)
}

find_undocumented_top_level_functions <- function(path) {
  r_files <- list.files(path,
    pattern = "[.][Rr]$", recursive = TRUE,
    full.names = TRUE
  )
  violations <- lapply(r_files, function(file) {
    lines <- readLines(file, warn = FALSE)
    expressions <- parse(file, keep.source = TRUE)
    source_refs <- util_attr(expressions, "srcref", exact = TRUE)
    unlist(lapply(seq_along(expressions), function(i) {
      expression <- expressions[[i]]
      is_function_assignment <- is.call(expression) &&
        identical(as.character(expression[[1L]]), "<-") &&
        is.symbol(expression[[2L]]) &&
        is.call(expression[[3L]]) &&
        identical(as.character(expression[[3L]][[1L]]), "function")
      if (!is_function_assignment) {
        return(character())
      }

      function_name <- as.character(expression[[2L]])
      function_line <- as.integer(source_refs[[i]][[1L]])
      comment_lines <- character()
      if (function_line > 1L) {
        previous_line <- function_line - 1L
        while (previous_line >= 1L &&
            (startsWith(lines[[previous_line]], "#") ||
                !nzchar(trimws(lines[[previous_line]])))) {
          previous_line <- previous_line - 1L
        }
        comment_start <- previous_line + 1L
        comment_end <- function_line - 1L
        comment_lines <- lines[seq.int(comment_start, comment_end)]
      }
      has_roxygen <- any(startsWith(comment_lines, "#'"))
      util_has_no_rd <- !startsWith(function_name, "util_") ||
        any(grepl("@noRd", comment_lines, fixed = TRUE))
      if (has_roxygen && util_has_no_rd) {
        return(character())
      }
      sprintf(
        "%s:%d: %s%s",
        basename(file),
        function_line,
        function_name,
        if (!has_roxygen) {
          " has no Roxygen documentation"
        } else {
          " must use @noRd"
        }
      )
    }), use.names = FALSE)
  })
  unlist(violations, use.names = FALSE)
}

find_r_source_path <- function() {
  has_r_sources <- function(path) {
    dir.exists(path) &&
      length(list.files(path,
          pattern = "[.][Rr]$", recursive = TRUE,
          full.names = TRUE
        )) > 0L
  }

  ci_project_dir <- Sys.getenv("CI_PROJECT_DIR", unset = "")
  r_path_candidates <- c(
    file.path(ci_project_dir, "R"),
    file.path(ci_project_dir, "QualityIndicatorFunctions", "R"),
    file.path(
      ci_project_dir, "QualityIndicatorFunctions",
      "QualityIndicatorFunctions", "R"
    ),
    file.path(testthat::test_path(), "..", "..", "R")
  )
  r_path <- r_path_candidates[vapply(
    r_path_candidates, has_r_sources,
    logical(1)
  )][1L]
  if (is.na(r_path)) {
    return(NA_character_)
  }
  normalizePath(r_path, mustWork = TRUE)
}

test_that("static source policy flags sprintf calls that use derived text as format", { # nolint: line_length_linter.
  tmp <- tempfile("format-string-scan")
  dir.create(file.path(tmp, "R"), recursive = TRUE)
  writeLines(
    c(
      "demo <- function() {",
      "  intrinsic_applicability_problem <- \"bad\"",
      "  sprintf(intrinsic_applicability_problem)",
      "}"
    ),
    file.path(tmp, "R", "demo.R")
  )

  violations <- find_unsafe_format_string_calls(file.path(tmp, "R"))
  expect_match(
    violations,
    "non-literal sprintf/gettextf format strings need explicit values",
    fixed = TRUE
  )
})

test_that("static source policy flags direct stopifnot calls", {
  tmp <- tempfile("condition-scan")
  dir.create(file.path(tmp, "R"), recursive = TRUE)
  writeLines(
    "demo <- function(x) stopifnot(is.numeric(x))",
    file.path(tmp, "R", "demo.R")
  )

  violations <- find_direct_base_condition_calls(file.path(tmp, "R"))
  expect_match(violations, "stopifnot(is.numeric(x))", fixed = TRUE)
})

test_that("static source policy flags undocumented package functions", {
  skip_on_cran()
  tmp <- tempfile("roxygen-scan")
  dir.create(file.path(tmp, "R"), recursive = TRUE)
  writeLines(
    c(
      "undocumented <- function() NULL",
      "#' Documented helper",
      "documented <- function() NULL",
      "#' Documented internal helper",
      "util_missing_no_rd <- function() NULL"
    ),
    file.path(tmp, "R", "demo.R")
  )

  violations <- find_undocumented_top_level_functions(file.path(tmp, "R"))
  expect_match(violations[[1L]], "undocumented", fixed = TRUE)
  expect_match(violations[[2L]], "must use @noRd", fixed = TRUE)
})

test_that("package condition and formatting helpers use trusted format strings", { # nolint: line_length_linter.
  r_path <- find_r_source_path()
  testthat::skip_if_not(
    !is.na(r_path),
    "R source files are not available"
  )

  violations <- find_unsafe_format_string_calls(r_path)
  expect_equal(
    violations,
    character(),
    info = paste(
      "Do not pass user- or metadata-derived text as a sprintf format string.",
      "Trusted package format strings should be literals or explicitly allowed",
      "format parameters. Finished text should be emitted with a literal %s.",
      paste(violations, collapse = "\n"),
      sep = "\n"
    )
  )
})

test_that("package implementation code routes conditions through dataquieR helpers", { # nolint: line_length_linter.
  r_path <- find_r_source_path()
  testthat::skip_if_not(
    !is.na(r_path),
    "R source files are not available"
  )

  violations <- find_direct_base_condition_calls(r_path)
  expect_equal(
    violations,
    character(),
    info = paste(
      paste0(
        "Do not call base stop(), warning(), message(), or stopifnot() ",
        "directly in package"
      ),
      "implementation code. Use util_error(), util_warning(), or",
      "util_message(), or util_stop_if_not() so conditions keep dataquieR",
      "metadata and handling.",
      "Direct base calls are only allowed in the helper implementations.",
      paste(violations, collapse = "\n"),
      sep = "\n"
    )
  )
})

test_that("all package functions have Roxygen documentation", {
  skip_on_cran()
  r_path <- find_r_source_path()
  testthat::skip_if_not(
    !is.na(r_path),
    "R source files are not available"
  )

  violations <- find_undocumented_top_level_functions(r_path)
  expect_equal(
    violations,
    character(),
    info = paste(
      "Document every top-level package function with Roxygen.",
      "Internal util_* functions must include @noRd.",
      paste(violations, collapse = "\n"),
      sep = "\n"
    )
  )
})
