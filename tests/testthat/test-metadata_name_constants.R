skip_on_cran()

test_that("metadata name constants are used in implementation code", {
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

  metadata_names <- c(names(WELL_KNOWN_META_VARIABLE_NAMES), DF_NAME, DF_CODE)
  metadata_name_pattern <- paste(metadata_names, collapse = "|")
  r_files <- list.files(
    r_path,
    pattern = "[.][Rr]$",
    recursive = TRUE,
    full.names = TRUE
  )

  hard_coded_bracket_access <- sprintf(
    "\\[\\[\\s*['\"](%s)['\"]\\s*\\]\\]",
    metadata_name_pattern
  )
  hard_coded_dollar_access <- sprintf(
    "\\$\\s*(%s)\\b",
    metadata_name_pattern
  )
  allowed_dollar_access <- sprintf(
    "WELL_KNOWN_META_VARIABLE_NAMES\\$\\s*(%s)\\b",
    metadata_name_pattern
  )

  violations <- unlist(lapply(r_files, function(file) {
    expressions <- parse(file, keep.source = FALSE)
    lines <- unlist(lapply(expressions, function(expr) {
      deparse(expr, width.cutoff = 500)
    }))
    bracket_hits <- grepl(hard_coded_bracket_access, lines, perl = TRUE)
    dollar_hits <- grepl(
      paste0(
        "\\b(meta_data|meta_info|md[0-9A-Za-z_.]*|il)\\s*",
        hard_coded_dollar_access
      ),
      lines,
      perl = TRUE
    ) &
      !grepl(allowed_dollar_access, lines, perl = TRUE)
    hits <- bracket_hits | dollar_hits
    if (!any(hits)) {
      return(character())
    }
    sprintf(
      "%s:%s: %s",
      basename(file),
      which(hits),
      trimws(lines[hits])
    )
  }))

  expect_equal(
    violations,
    character(),
    info = paste(
      "Use metadata name constants such as VAR_NAMES or DF_CODE instead of",
      "hard-coded metadata column or attribute names:",
      paste(violations, collapse = "\n"),
      sep = "\n"
    )
  )
})
