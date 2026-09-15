skip_on_cran()

test_that("Roxygen template users do not repeat standard parameter docs", {
  has_r_sources <- function(path) {
    dir.exists(path) &&
      length(list.files(path, pattern = "[.][Rr]$", recursive = TRUE,
          full.names = TRUE)) > 0L
  }

  ci_project_dir <- Sys.getenv("CI_PROJECT_DIR", unset = "")
  r_path_candidates <- c(
    file.path(ci_project_dir, "R"),
    file.path(ci_project_dir, "QualityIndicatorFunctions", "R"),
    file.path(ci_project_dir, "QualityIndicatorFunctions",
      "QualityIndicatorFunctions", "R"),
    file.path(testthat::test_path(), "..", "..", "R")
  )
  r_path <- r_path_candidates[vapply(r_path_candidates, has_r_sources,
      logical(1))][1L]
  testthat::skip_if_not(!is.na(r_path),
    "R source files are not available")
  r_path <- normalizePath(r_path, mustWork = TRUE)

  r_files <- setdiff(
    list.files(r_path, pattern = "[.][Rr]$", full.names = TRUE),
    file.path(r_path, c(
      "DOT.template_function_indicator.R",
      "DOT.template_function_developer.R",
      "DOT.template_function_report_plan.R",
      "DOT.template_function_metadata_generation.R"
    ))
  )
  inherited_templates <- c(
    "@inheritParams .template_function_indicator",
    "@inheritParams .template_function_developer",
    "@inheritParams .template_function_report_plan",
    "@inheritParams .template_function_metadata_generation"
  )
  # nolint start: line_length_linter.
  redundant_param_starts <- c(
    "#' @param resp_vars [variable] the name",
    "#' @param resp_vars [variable] the names",
    "#' @param resp_vars [variable list] the name",
    "#' @param resp_vars [variable list] the names",
    "#' @param group_vars [variable] the name",
    "#' @param group_vars [variable] the names",
    "#' @param group_vars [variable list] the name",
    "#' @param group_vars [variable list] the names",
    "#' @param group_vars name",
    "#' @param co_vars [variable] the name",
    "#' @param co_vars [variable] the names",
    "#' @param co_vars [variable list] the name",
    "#' @param co_vars [variable list] the names",
    "#' @param time_vars [variable] the name",
    "#' @param time_vars [variable] the names",
    "#' @param time_vars name",
    "#' @param time_vars [variable list] selected",
    "#' @param resp_vars [variable list] selected",
    "#' @param group_vars [variable list] selected",
    "#' @param co_vars [variable list] selected",
    "#' @param resp_vars [variable] names of the variables to fetch",
    "#' @param resp_vars [variable list] the names of variables to inspect",
    "#' @param study_data [data.frame] the data frame that contains the measurements",
    "#' @param study_data the data frame that contains the measurements",
    "#' @param study_data [data.frame] study data inspected to derive metadata",
    "#' @param guess_character [logical] guess a data type for character columns",
    "#' @param meta_data [data.frame] item-level metadata",
    "#' @param meta_data the data frame that contains metadata",
    "#' @param label_col [variable attribute] the name of the column in the metadata",
    "#' @param item_level [data.frame] the data frame that contains metadata",
    "#' @param meta_data [data.frame] old name for `item_level`",
    "#' @param meta_data_v2 [character] path to workbook like metadata file, see",
    "#' @param meta_data_dataframe [data.frame] the data frame that contains the",
    "#' @param meta_data_segment [data.frame] -- optional: Segment level metadata",
    "#' @param meta_data_cross_item [data.frame] -- optional: Cross-item level",
    "#' @param meta_data_item_computation [data.frame] -- optional: Computed items",
    "#' @param dataframe_level [data.frame] alias for `meta_data_dataframe`",
    "#' @param segment_level [data.frame] alias for `meta_data_segment`",
    "#' @param cross_item_level [data.frame] alias for `meta_data_cross_item`",
    "#' @param `cross-item_level` [data.frame] alias for `meta_data_cross_item`"
  )
  # nolint end

  roxygen_blocks <- function(lines) {
    is_roxygen <- grepl("^#'", lines)
    starts <- is_roxygen & !c(FALSE, head(is_roxygen, -1L))
    block_id <- cumsum(starts)
    split(seq_along(lines)[is_roxygen], block_id[is_roxygen])
  }

  violations <- unname(unlist(lapply(r_files, function(file) {
    lines <- readLines(file, warn = FALSE)
    blocks <- roxygen_blocks(lines)
    hits <- unlist(lapply(blocks, function(idx) {
      block <- lines[idx]
      if (!any(vapply(inherited_templates, function(pattern) {
        any(grepl(pattern, block, fixed = TRUE))
      }, logical(1)))) {
        return(character())
      }
      duplicate <- vapply(redundant_param_starts, function(pattern) {
        any(startsWith(block, pattern))
      }, logical(1))
      if (!any(duplicate)) {
        return(character())
      }
      duplicate_lines <- idx[vapply(block, function(line) {
        any(startsWith(line, redundant_param_starts))
      }, logical(1))]
      sprintf(
        "%s:%s: %s",
        basename(file),
        duplicate_lines,
        trimws(lines[duplicate_lines])
      )
    }))
    hits
  })))

  expect_equal(
    violations,
    character(),
    info = paste(
      "Roxygen blocks using @inheritParams .template_function_indicator",
      "@inheritParams .template_function_developer,",
      "or @inheritParams .template_function_report_plan",
      "should not repeat the standard parameter text locally:",
      paste(violations, collapse = "\n"),
      sep = "\n"
    )
  )
})
