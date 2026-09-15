#' Save and optionally render one dq_report_by subreport
#'
#' @param r dataquieR result set or a `try-error`.
#' @param output_dir output directory for by-report artifacts.
#' @param sdn report name component.
#' @param level_name stratum label for summaries and dashboard payloads.
#' @param name_of_study_data display name of the study data source.
#' @param cur_seg current segment name.
#' @param also_print if `TRUE`, render the HTML subreport.
#' @param dots additional arguments originally passed to `dq_report_by()`.
#' @param disable_plotly if `TRUE`, pass plotly disabling to HTML rendering.
#' @param advanced_options report-rendering options.
#' @param html_table_backend HTML table backend used to render subreports.
#' @param view_meta_data full item-level metadata used for consistent display
#'   labels across report-by subreports and the overview.
#' @param force_overwrite whether existing rendered subreports may be replaced.
#'
#' @return `invisible(NULL)` when artifacts are written, otherwise `r`.
#'
#' @noRd
util_report_by_output <- function(r,
  output_dir,
  sdn,
  level_name,
  name_of_study_data,
  cur_seg,
  also_print,
  dots,
  disable_plotly,
  advanced_options,
  html_table_backend,
  view_meta_data = NULL,
  force_overwrite = FALSE) {
  if (!(length(output_dir) == 1 && dir.exists(output_dir))) {
    return(r)
  }

  if (inherits(r, "try-error")) {
    util_warning(
      "Could not compute report for %s: %s.",
      dQuote(sdn),
      sQuote(conditionMessage(util_attr(r, "condition", exact = TRUE)))
    )
    return(invisible(NULL))
  }
  safe_name <- util_report_by_safe_name(sdn, output_dir)

  s_res <- try(prep_save_report(r, file.path(
    output_dir,
    sprintf("report_%s.dq2", safe_name)
  )))
  if (inherits(s_res, "try-error")) {
    util_warning(
      "Could not save report for %s: %s.",
      dQuote(sdn),
      sQuote(conditionMessage(util_attr(s_res, "condition", exact = TRUE)))
    )
  }
  rm(s_res)

  r_for_view <- withr::with_options(
    advanced_options,
    util_prepare_report_labels_for_view(
      report = r,
      reference_meta_data = view_meta_data
    )
  )
  obj <- summary(r_for_view)
  repsum <- obj
  this <- util_attr(obj, "this", exact = TRUE)
  this$stratum <- level_name
  this$used_data_file <- name_of_study_data
  this$segment <- cur_seg
  this$sdn <- sdn
  this$safe_name <- safe_name
  attr(obj, "this") <- this

  s_res <- try(saveRDS(obj, file.path(
    output_dir,
    sprintf("report_summary_%s.RDS", safe_name)
  )))
  if (inherits(s_res, "try-error")) {
    util_warning(
      "Could not save report summary for %s: %s.",
      dQuote(sdn),
      sQuote(conditionMessage(util_attr(s_res, "condition", exact = TRUE)))
    )
  }

  obj <- NULL
  try({
    obj <- util_setup_dashboard(r_for_view,
      make_links = FALSE,
      return_table_only = TRUE,
      repsum = repsum
    )
    if (!is.null(obj)) {
      attr(obj, "name_of_study_data") <- name_of_study_data
      attr(obj, "level_name") <- level_name
    }
  })
  s_res <- try(saveRDS(obj, file.path(
    output_dir,
    sprintf("report_dashboard_%s.RDS", safe_name)
  )))
  if (inherits(s_res, "try-error")) {
    util_warning(
      "Could not save report dashboard table for %s: %s.",
      dQuote(sdn),
      sQuote(conditionMessage(util_attr(s_res, "condition", exact = TRUE)))
    )
  }
  rm(repsum)
  rm(s_res)

  .dir <- file.path(output_dir, sprintf("report_%s", safe_name))
  dir.create(.dir,
    showWarnings = FALSE,
    recursive = TRUE
  )
  if (!dir.exists(.dir)) {
    util_warning(
      paste0(
        "Could not create directory %s. ",
        "No output for this segment"
      ),
      dQuote(.dir)
    )
  }

  if (also_print && dir.exists(.dir)) {
    pass_args <- dots[names(dots) %in% c("cores", "block_load_factor")]
    if (force_overwrite) {
      pass_args$force_overwrite <- TRUE
    }
    p_res <- util_try_with_trace(rlang::eval_bare(
      rlang::call2(print.dataquieR_resultset2,
        r_for_view,
        dir = .dir,
        view = FALSE,
        disable_plotly = disable_plotly,
        by_report = TRUE,
        advanced_options = advanced_options,
        html_table_backend = html_table_backend,
        !!!pass_args
      )
    ))
    if (inherits(p_res, "try-error")) {
      if (isTRUE(getOption(
        "dataquieR.traceback",
        dataquieR.traceback_default
      ))) {
        util_warning(util_condition_from_try_error(p_res))
      }
      util_warning(
        "Could not create HTML report for %s: %s.",
        dQuote(.dir),
        sQuote(conditionMessage(util_attr(p_res, "condition", exact = TRUE)))
      )
    }
    rm(p_res)
  }

  rm(r_for_view)

  invisible(NULL)
}

#' Create a collision-safe report-by file name component
#'
#' Existing file names remain unchanged unless another report name maps to the
#' same sanitized component.
#'
#' @param sdn original report name component.
#' @param output_dir report bundle directory.
#'
#' @return A file-system-safe character scalar.
#' @noRd
util_report_by_safe_name <- function(sdn, output_dir) {
  candidate <- gsub("[^a-zA-Z0-9_\\.]", "", sdn)
  if (!nzchar(candidate)) {
    candidate <- "report"
  }
  summary_file <- file.path(
    output_dir,
    sprintf("report_summary_%s.RDS", candidate)
  )
  if (!file.exists(summary_file)) {
    return(candidate)
  }
  if (dir.exists(summary_file)) {
    return(candidate)
  }
  summary_object <- try(readRDS(summary_file), silent = TRUE)
  this <- if (util_is_try_error(summary_object)) {
    NULL
  } else {
    util_attr(summary_object, "this", exact = TRUE)
  }
  if (is.list(this) && identical(this$sdn, sdn)) {
    return(candidate)
  }
  paste0(candidate, "_", substr(rlang::hash(enc2utf8(sdn)), 1L, 8L))
}

#' Save dq_report_by overview metadata
#'
#' @param output_dir output directory for by-report artifacts.
#' @param names character vector of names to persist from `env`.
#' @param env environment to read metadata values from.
#'
#' @return `invisible(NULL)`.
#'
#' @noRd
util_report_by_meta <- function(output_dir, names, env = parent.frame()) {
  if (!(length(output_dir) == 1 && dir.exists(output_dir))) {
    return(invisible(NULL))
  }

  values <- util_report_by_meta_values(names, env)

  saveRDS(
    values,
    file = file.path(output_dir, "report_by_meta.RDS")
  )
  invisible(NULL)
}

#' Collect available report-by overview metadata
#'
#' @param names character names to collect from `env`.
#' @param env environment containing the values.
#'
#' @return A named list.
#' @noRd
util_report_by_meta_values <- function(names, env = parent.frame()) {
  unlist(lapply(names, function(x) {
    if (exists(x, envir = env, inherits = FALSE)) {
      setNames(
        list(rlang::maybe_missing(get(x, envir = env, inherits = FALSE))),
        nm = x
      )
    } else {
      list()
    }
  }), recursive = FALSE)
}
