#' Resolve `dir` and `output_dir` aliases
#'
#' @param dir [character] legacy output-directory argument.
#' @param output_dir [character] canonical output-directory argument.
#' @param dir_missing [logical] whether `dir` was omitted by the caller.
#' @param output_dir_missing [logical] whether `output_dir` was omitted.
#'
#' @return A character scalar or `NULL`.
#' @noRd
util_resolve_output_dir_alias <- function(dir = NULL, output_dir = NULL,
  dir_missing = TRUE, output_dir_missing = TRUE) {
  has_dir <- !dir_missing && !is.null(dir)
  has_output_dir <- !output_dir_missing && !is.null(output_dir)

  if (has_dir) {
    util_expect_scalar(dir, check_type = is.character)
    if (is.na(dir) || !nzchar(dir)) {
      util_error("%s must be a non-empty path", sQuote("dir"))
    }
  }
  if (has_output_dir) {
    util_expect_scalar(output_dir, check_type = is.character)
    if (is.na(output_dir) || !nzchar(output_dir)) {
      util_error("%s must be a non-empty path", sQuote("output_dir"))
    }
  }
  if (has_dir && has_output_dir) {
    same_path <- identical(
      util_normalize_path(dir),
      util_normalize_path(output_dir)
    )
    if (!same_path) {
      util_error(
        "%s and %s must refer to the same directory if both are supplied",
        sQuote("dir"), sQuote("output_dir")
      )
    }
  }
  if (has_output_dir) {
    return(output_dir)
  }
  if (has_dir) {
    return(dir)
  }
  NULL
}

#' Create a report-by bundle result
#'
#' @param x [list] nested report results or lightweight placeholders.
#' @param output_dir [character] directory containing flushed report files.
#' @param meta [list] metadata needed to create the bundle overview.
#' @param report_files [character] expected `.dq2` file names.
#'
#' @return A `dataquieR_report_by` object.
#' @noRd
util_new_dataquieR_report_by <- function(x, output_dir = NULL, meta = list(),
  report_files = character()) {
  attr(x, "output_dir") <- output_dir
  attr(x, "report_by_meta") <- meta
  attr(x, "report_files") <- report_files
  class(x) <- c("dataquieR_report_by", "list")
  x
}

#' Collect in-memory reports from a report-by bundle
#'
#' @param x a nested list.
#'
#' @return A list of `dataquieR_resultset2` objects.
#' @noRd
util_report_by_in_memory_reports <- function(x) {
  if (inherits(x, "dataquieR_resultset2")) {
    return(list(x))
  }
  if (!is.list(x)) {
    return(list())
  }
  unlist(lapply(unclass(x), util_report_by_in_memory_reports),
    recursive = FALSE
  )
}

#' Check whether a generated report entry point is complete
#'
#' @param file [character] HTML file to inspect.
#'
#' @return A scalar logical.
#' @noRd
util_report_by_html_complete <- function(file) {
  if (!file.exists(file) || !isTRUE(file.info(file)$size > 0L)) {
    return(FALSE)
  }
  lines <- suppressWarnings(try(readLines(file, warn = FALSE), silent = TRUE))
  !util_is_try_error(lines) && any(grepl("<!-- done -->", lines, fixed = TRUE))
}

#' Print a report bundle created by `dq_report_by()`
#'
#' Each nested report is rendered into its own subdirectory and an overview is
#' created at `index.html`. A disk-backed result contains no computed reports;
#' it refers to the `.dq2` files in its original output directory.
#'
#' @param x a [dq_report_by()] result.
#' @param dir [character] output directory. Alias for `output_dir`.
#' @param output_dir [character] output directory. If both aliases are omitted,
#'   a disk-backed bundle reuses its stored directory and an in-memory bundle
#'   uses a temporary directory.
#' @param view [logical] open the bundle overview after rendering.
#' @param force_overwrite [logical] overwrite existing rendered sub-reports.
#' @param ... arguments forwarded to [print.dataquieR_resultset2()], notably
#'   `cores` and `block_load_factor`.
#'
#' @return `x`, invisibly.
#' @export
print.dataquieR_report_by <- function(x, dir, output_dir,
  view = TRUE, force_overwrite = FALSE, ...) {
  target_dir <- util_resolve_output_dir_alias(
    dir = if (missing(dir)) NULL else dir,
    output_dir = if (missing(output_dir)) NULL else output_dir,
    dir_missing = missing(dir),
    output_dir_missing = missing(output_dir)
  )
  source_dir <- util_attr(x, "output_dir", exact = TRUE)
  if (length(source_dir) == 1L &&
      (!dir.exists(source_dir) || file.access(source_dir, 4L) != 0L)) {
    util_error(
      paste0(
        "This disk-backed report bundle needs its original output ",
        "directory %s, but that directory is not readable."
      ),
      dQuote(source_dir)
    )
  }
  meta <- util_attr(x, "report_by_meta", exact = TRUE)
  report_manifest <- util_attr(x, "report_files", exact = TRUE)
  if (length(source_dir) == 1L) {
    meta_file <- file.path(source_dir, "report_by_meta.RDS")
    if (!file.exists(meta_file)) {
      util_error(
        "The disk-backed report bundle is missing %s",
        dQuote(meta_file)
      )
    }
    stored_meta <- readRDS(meta_file)
    if (!length(meta)) {
      meta <- stored_meta
    }
    if (!length(report_manifest)) {
      report_manifest <- stored_meta$report_files
    }
    if (!length(report_manifest)) {
      util_error("The disk-backed report bundle has no report manifest")
    }
    report_paths <- file.path(source_dir, report_manifest)
    if (any(!file.exists(report_paths)) || any(file.access(report_paths, 4L))) {
      util_error(
        "The disk-backed report bundle is missing one or more readable reports"
      )
    }
  }
  if (is.null(target_dir)) {
    target_dir <- if (length(source_dir) == 1L && dir.exists(source_dir)) {
      source_dir
    } else {
      tempfile("dataquieR-report-by-")
    }
  }
  util_expect_scalar(view, check_type = is.logical)
  util_expect_scalar(force_overwrite, check_type = is.logical)

  same_as_source <- length(source_dir) == 1L && dir.exists(source_dir) &&
    identical(
      util_normalize_path(target_dir),
      util_normalize_path(source_dir)
    )
  existing_overview <- file.path(target_dir, "index.html")
  completed_reports <- if (same_as_source) {
    report_dirs <- sub("[.]dq2$", "", report_manifest)
    vapply(
      file.path(target_dir, report_dirs, "index.html"),
      util_report_by_html_complete,
      FUN.VALUE = logical(1)
    )
  } else {
    FALSE
  }
  if (same_as_source && util_report_by_html_complete(existing_overview) &&
      all(completed_reports) && !force_overwrite) {
    if (view) {
      util_view_file(existing_overview)
    }
    return(invisible(x))
  }
  if (!same_as_source && dir.exists(target_dir)) {
    util_overwrite_if_requested(target_dir, force_overwrite)
  }
  if (!dir.exists(target_dir) &&
      !dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)) {
    util_error("Could not create %s", dQuote(target_dir))
  }
  target_dir <- util_normalize_path(target_dir)

  saveRDS(meta, file.path(target_dir, "report_by_meta.RDS"))

  report_files <- character()
  reports <- list()
  if (length(source_dir) == 1L && dir.exists(source_dir)) {
    report_files <- file.path(source_dir, report_manifest)
  } else {
    reports <- util_report_by_in_memory_reports(x)
  }

  render_one <- function(report, fallback_name) {
    or_else <- function(value, fallback) {
      if (is.null(value)) fallback else value
    }
    info <- util_attr(report, "report_by_info", exact = TRUE)
    if (!is.list(info)) {
      properties <- util_attr(report, "properties", exact = TRUE)
      info <- list(
        sdn = fallback_name,
        level_name = or_else(properties[["Stratum"]], fallback_name),
        name_of_study_data = or_else(
          properties[["Study_data"]], "study_data"
        ),
        cur_seg = or_else(properties[["Segment"]], fallback_name)
      )
    }
    safe_name <- util_report_by_safe_name(info$sdn, target_dir)
    report_dir <- file.path(target_dir, paste0("report_", safe_name))
    report_index <- file.path(report_dir, "index.html")
    if (util_report_by_html_complete(report_index) && !force_overwrite) {
      return(invisible(NULL))
    }
    repair_incomplete <- dir.exists(file.path(report_dir, ".report"))
    util_report_by_output(
      r = report,
      output_dir = target_dir,
      sdn = info$sdn,
      level_name = info$level_name,
      name_of_study_data = info$name_of_study_data,
      cur_seg = info$cur_seg,
      also_print = TRUE,
      dots = list(...),
      disable_plotly = isTRUE(meta$disable_plotly),
      advanced_options = or_else(meta$advanced_options, list()),
      html_table_backend = or_else(
        meta$html_table_backend,
        getOption(
          "dataquieR.html_table_backend",
          dataquieR.html_table_backend_default
        )
      ),
      view_meta_data = util_attr(report, "meta_data", exact = TRUE),
      force_overwrite = force_overwrite || repair_incomplete
    )
    invisible(NULL)
  }

  if (length(report_files)) {
    for (report_file in report_files) {
      report <- prep_load_report(report_file)
      fallback_name <- sub("^report_(.*)[.]dq2$", "\\1",
        basename(report_file)
      )
      render_one(report, fallback_name)
      rm(report)
      gc()
    }
  } else {
    for (index in seq_along(reports)) {
      render_one(reports[[index]], paste0("report_", index))
    }
  }

  util_create_report_by_overview(target_dir)
  if (view) {
    util_view_file(file.path(target_dir, "index.html"))
  }
  invisible(x)
}
