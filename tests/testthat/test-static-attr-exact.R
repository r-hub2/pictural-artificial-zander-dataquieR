find_attr_read_policy_violations <- function(path) {
  is_lhs_replacement <- function(pd, token) {
    call_expr <- pd[pd$id == token$parent, , drop = FALSE]
    if (nrow(call_expr) != 1L) {
      return(TRUE)
    }

    repeat {
      parent <- pd[pd$id == call_expr$parent, , drop = FALSE]
      if (nrow(parent) != 1L) {
        return(FALSE)
      }

      assigns <- pd[pd$parent == parent$id &
        pd$token %in% c(
          "LEFT_ASSIGN", "EQ_ASSIGN",
          "RIGHT_ASSIGN"
        ), , drop = FALSE]
      assignment_after_call <-
        nrow(assigns) &&
        any(assigns$line1 > call_expr$line2 |
            (assigns$line1 == call_expr$line2 &
                assigns$col1 > call_expr$col2))
      if (assignment_after_call) {
        return(TRUE)
      }
      if (parent$parent == 0L) {
        return(FALSE)
      }
      call_expr <- parent
    }
  }

  is_allowed_base_attr <- function(pd, token, file) {
    if (!identical(basename(file), "util_attr.R")) {
      return(FALSE)
    }
    parent <- pd[pd$id == token$parent, , drop = FALSE]
    if (nrow(parent) != 1L) {
      return(FALSE)
    }
    siblings <- pd[pd$parent == parent$id, , drop = FALSE]
    any(siblings$token == "SYMBOL_PACKAGE" & siblings$text == "base") &&
      any(siblings$token == "NS_GET")
  }

  r_files <- list.files(path,
    pattern = "[.][Rr]$", recursive = TRUE,
    full.names = TRUE
  )
  if (!length(r_files)) {
    stop("No R source files found below: ", path)
  }

  violations <- lapply(r_files, function(file) {
    expressions <- parse(file, keep.source = TRUE)
    pd <- getParseData(expressions)
    tokens <- pd[pd$token %in% c("SYMBOL_FUNCTION_CALL", "SYMBOL") &
        pd$text == "attr", , drop = FALSE]
    if (!nrow(tokens)) {
      return(character(0))
    }
    read_calls <- tokens[vapply(seq_len(nrow(tokens)), function(i) {
      !is_lhs_replacement(pd, tokens[i, ]) &&
        !is_allowed_base_attr(pd, tokens[i, ], file)
    }, logical(1)), , drop = FALSE]
    if (!nrow(read_calls)) {
      return(character(0))
    }
    rel_file <- sub(
      paste0("^", normalizePath(dirname(path)), "/"), "",
      normalizePath(file)
    )
    paste0(rel_file, ":", read_calls$line1, ":", read_calls$col1)
  })

  unlist(violations, use.names = FALSE)
}

test_that("package attribute reads go through util_attr", {
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
  testthat::skip_if_not(
    !is.na(r_path),
    "R source files are not available"
  )
  r_path <- normalizePath(r_path, mustWork = TRUE)

  violations <- find_attr_read_policy_violations(r_path)
  expect_equal(
    violations,
    character(0),
    info = paste(
      "Use util_attr(x, name) for reading attributes.",
      "Use attr(x, name) <- value only for setting attributes.",
      paste(violations, collapse = "\n")
    )
  )
})
