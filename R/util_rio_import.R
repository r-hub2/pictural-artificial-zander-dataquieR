#' Import a data frame
#'
#' see [`rio::import`], but with argument `keep_types` and modified error
#' handling.
#'
#' @param fn the file name to load.
#' @param keep_types [logical] keep types as possibly defined in the file.
#'                             set `TRUE` for study data.
#' @param ... additional arguments for [rio::import]
#'
#' @return [data.frame] as in [rio::import]
#' @noRd
util_rio_import <- function(fn, keep_types, ...) {
  r <- .util_rio_import_lambda(fn, keep_types, lambda = rio::import, ...)
  .util_warn_on_possible_study_data_read_with_keep_types_false_or_vv(
    r,
    fn = fn,
    keep_types = keep_types
  )
  r
}

#' Import list of data frames
#'
#' see [`rio::import_list`], but with argument `keep_types` and modified error
#' handling.
#'
#' @param fn the file name to load.
#' @param keep_types [logical] keep types as possibly defined in the file.
#'                             set `TRUE` for study data.
#' @param ... additional arguments for [rio::import_list]
#'
#' @return [list] as in [rio::import_list]
#' @noRd
#'
util_rio_import_list <- function(fn, keep_types, ...) {
  if (tolower(tools::file_ext(trimws(fn))) %in% c("odm", "xml") &&
      file.exists(fn) && !dir.exists(fn) &&
      util_is_odm_xml(fn)) {
    if (!odm_installed) {
      util_error(
        c(
          "%s looks like an ODM file. %s cannot read these without",
          "the help of dataquieR2odm, which you need to install",
          "separately, see %s"
        ),
        dQuote(fn),
        sQuote(utils::packageName()),
        sQuote(
          "https://dataquality.qihs.uni-greifswald.de/DownloadR.html"
        )
      )
    }
    my_read_list <- try(util_odm2dataquieR(fn), silent = TRUE)
    if (!util_is_try_error(my_read_list) && is.list(my_read_list)) {
      if (all(c("item_level", CODE_LIST_TABLE) %in% names(my_read_list))) {
        return(my_read_list[c("item_level", CODE_LIST_TABLE)])
      }
    }
  }
  r <- .util_rio_import_lambda(fn, keep_types, lambda = rio::import_list, ...)
  .util_warn_on_possible_study_data_read_with_keep_types_false_or_vv(
    r,
    fn = fn,
    keep_types = keep_types
  )
  r
}

#' Internal helper: warn on possible study-data reads without preserved types
#'
#' @noRd
.util_warn_on_possible_study_data_read_with_keep_types_false_or_vv <-
  function(x, fn, keep_types, sheet = "", segments = character(0)) {
    segments <- unique(c(segments, unlist(eapply(
      .dataframe_environment(),
      function(df0) {
        if (!is.data.frame(df0)) {
          return(character(0))
        }
        df0[, intersect(colnames(df0), c(KEY_STUDY_SEGMENT, STUDY_SEGMENT)), drop = FALSE] # nolint: line_length_linter.
      }
    ))))
    other_allowed_names <- sort(c(
      "indicator_metric",
      "Description",
      paste0("dqi_cat_", 1:20),
      "category",
      "label",
      "color",
      "ID",
      "Metadata sheet",
      "Column name",
      "Definition",
      "Use in dataquieR reporting",
      c(
        "SETTING_ID", "METRIC", "IS_DEFAULT", "ENABLED", "METRIC_CLASS",
        "ALLOWED_DATA_TYPE", "ALLOWED_SCALE_LEVEL", "REQUIRES_REFERENCE",
        "ALLOWS_REFERENCE", "COMPARISON_MODE_DEFAULT", "MIN_N", "MIN_REPEATS",
        "MAX_REPEATS", "MISSING_RULE", "CI_METHOD", "BOOTSTRAP_N",
        "ICC_MODEL", "ICC_TYPE", "ICC_UNIT", "KAPPA_WEIGHTS", "CV_SCALE",
        "HIGHER_IS_BETTER", "METHOD_LABEL", "METHOD_NOTE"
      ),
      tolower(c(
        "SETTING_ID", "METRIC", "IS_DEFAULT", "ENABLED", "METRIC_CLASS",
        "ALLOWED_DATA_TYPE", "ALLOWED_SCALE_LEVEL", "REQUIRES_REFERENCE",
        "ALLOWS_REFERENCE", "COMPARISON_MODE_DEFAULT", "MIN_N", "MIN_REPEATS",
        "MAX_REPEATS", "MISSING_RULE", "CI_METHOD", "BOOTSTRAP_N",
        "ICC_MODEL", "ICC_TYPE", "ICC_UNIT", "KAPPA_WEIGHTS", "CV_SCALE",
        "HIGHER_IS_BETTER", "METHOD_LABEL", "METHOD_NOTE"
      ))
    ))
    if (keep_types) { # expecting study data
      if (!is.data.frame(x)) {
        segments <- unique(c(segments, unlist(lapply(x, function(df0) {
          if (!is.data.frame(df0)) {
            return(character(0))
          }
          df0[, intersect(colnames(df0), c(KEY_STUDY_SEGMENT, STUDY_SEGMENT)), drop = FALSE] # nolint: line_length_linter.
        }))))
        segments <- segments[!util_empty(segments)]
        mapply(
          SIMPLIFY = FALSE,
          x = x,
          sheet = names(x),
          FUN =
            .util_warn_on_possible_study_data_read_with_keep_types_false_or_vv,
          MoreArgs = list(
            fn = fn,
            keep_types = TRUE,
            segments = segments
          )
        )
      } else {
        ratio <- sum(vapply(colnames(x), # of known metadata column names
          function(cn) {
            exists(cn,
              envir = getNamespace("dataquieR"),
              inherits = FALSE
            ) || cn %in% c(
              other_allowed_names,
              segments
            )
          },
          FUN.VALUE = logical(1)
        )) / ncol(x)
        if (ratio >= .8) { # >= 80% well known metadata names
          util_message(
            c(
              "%s: Found many (%g %%) column names that look like",
              "meta data but %s was set to %s -- did you read",
              "the file keeping column types on purpose, this",
              "is not suitable for metadata. Colunm names: %s"
            ),
            dQuote(fn),
            100 * ratio, sQuote("keep_types"), dQuote(keep_types),
            util_pretty_vector_string(colnames(x))
          )
        }
      }
    } else { # expecting metadata
      if (!is.data.frame(x)) {
        segments <- unique(c(segments, unlist(lapply(x, function(df0) {
          if (!is.data.frame(df0)) {
            return(character(0))
          }
          df0[, intersect(colnames(df0), c(KEY_STUDY_SEGMENT, STUDY_SEGMENT)), drop = FALSE] # nolint: line_length_linter.
        }))))
        segments <- segments[!util_empty(segments)]
        mapply(
          SIMPLIFY = FALSE,
          x = x,
          sheet = names(x),
          FUN =
            .util_warn_on_possible_study_data_read_with_keep_types_false_or_vv,
          MoreArgs = list(
            fn = fn,
            keep_types = FALSE,
            segments = segments
          )
        )
      } else {
        ratio <- sum(vapply(colnames(x), # of known metadata column names
          function(cn) {
            exists(cn,
              envir = getNamespace("dataquieR"),
              inherits = FALSE
            ) || cn %in% c(
              other_allowed_names,
              segments
            )
          },
          FUN.VALUE = logical(1)
        )) / ncol(x)
        if (ratio < .05) { # < 5% well known metadata names
          util_message(
            c(
              "%s: Found only few (%g %%) column names that look like",
              "meta data but %s was set to %s -- did you read",
              "the file as text-only on purpose, this",
              "is not suitable for study data. Colunm names: %s"
            ),
            dQuote(fn),
            100 * ratio, sQuote("keep_types"), dQuote(keep_types),
            util_pretty_vector_string(colnames(x))
          )
        }
      }
    }
  }

#' Internal helper: util is transient url import error
#'
#' @noRd
.util_is_transient_url_import_error <- function(x) {
  if (!util_is_try_error(x)) {
    return(FALSE)
  }
  condition <- util_attr(x, "condition", exact = TRUE)
  error_message <- if (inherits(condition, "condition")) {
    conditionMessage(condition)
  } else {
    paste(x, collapse = " ")
  }
  error_message <- tolower(error_message)
  transient_messages <- c(
    "timeout",
    "timed out",
    "proxy connect",
    "could not resolve host",
    "temporary failure",
    "connection reset",
    "connection refused",
    "failed to connect",
    "network is unreachable",
    "http error 408",
    "http error 429",
    "http error 500",
    "http error 502",
    "http error 503",
    "http error 504"
  )
  any(vapply(
    transient_messages,
    grepl,
    logical(1),
    x = error_message,
    fixed = TRUE
  ))
}

#' Internal helper: util rio import with retry
#'
#' @noRd
.util_rio_import_with_retry <- function(fn, import) {
  is_url <- length(fn) == 1 && !is.na(fn) &&
    grepl("^https?://", fn, ignore.case = TRUE)
  attempts <- getOption(
    "dataquieR.url_import_attempts",
    suppressWarnings(as.integer(Sys.getenv(
      "DATAQUIER_URL_IMPORT_ATTEMPTS",
      "1"
    )))
  )
  retry_delay <- getOption(
    "dataquieR.url_import_retry_delay",
    suppressWarnings(as.numeric(Sys.getenv(
      "DATAQUIER_URL_IMPORT_RETRY_DELAY",
      "0"
    )))
  )
  attempts <- suppressWarnings(as.integer(attempts))
  retry_delay <- suppressWarnings(as.numeric(retry_delay))
  if (length(attempts) != 1 || is.na(attempts) || attempts < 1) {
    attempts <- 1L
  }
  if (length(retry_delay) != 1 || is.na(retry_delay) || retry_delay < 0) {
    retry_delay <- 0
  }
  if (!is_url) {
    attempts <- 1L
  }

  for (attempt in seq_len(attempts)) {
    result <- import()
    retry <- attempt < attempts &&
      .util_is_transient_url_import_error(result)
    if (!retry) {
      return(result)
    }
    wait <- retry_delay * 2^(attempt - 1)
    util_message(
      "Transient network error reading %s; retrying in %g seconds (%d/%d).",
      dQuote(fn),
      wait,
      attempt + 1,
      attempts
    )
    if (wait > 0) {
      Sys.sleep(wait)
    }
  }
}

#' Internal helper: util rio import lambda
#'
#' @noRd
.util_rio_import_lambda <- function(fn, keep_types, lambda, ...) {
  withr::local_options(list("datatable.na.strings" = c(
    "NA",
    "-",
    "",
    "na",
    "n.a.",
    "n/a"
  )))
  is_list <- (identical(lambda, rio::import_list))
  if (!identical(lambda, rio::import) && !identical(lambda, rio::import_list)) {
    util_error(
      "Internal error, sorry, please report: invalid lambda in %s",
      sQuote("util_rio_import_lambda")
    )
  }
  util_expect_scalar(keep_types,
    check_type = is.logical,
    error_message =
      sprintf(
        "%s needs to be to be a logical value.",
        sQuote("keep_types")
      )
  )
  # Historical explicit encoding guess removed here.
  file_type_needs_type_adjust <-
    getOption(
      "dataquieR.fix_column_type_on_read",
      dataquieR.fix_column_type_on_read_default
    ) &&
    (all(grepl(".", fn, fixed = TRUE)) &&
      rio::get_info(fn)$input %in% .formats_with_weird_types)
  need_type_adjust <- FALSE
  # Historical robust type-guessing experiments removed here. Inspect commits
  # 51c9f8410c, 214dd76a7d, and 8b5827eb83 before restoring them.
  if (endsWith(tolower(fn), ".ods")) {
    col_types <- NA
  } else if (!endsWith(tolower(fn), ".xls") &&
      !endsWith(tolower(fn), ".xlsx")) {
    col_types <- "character"
  } else {
    col_types <- "text"
  }
  need_read_as_text <-
    (!keep_types || file_type_needs_type_adjust)
  warns1 <- new.env(parent = emptyenv())
  warns1$warns <- list()
  warns2 <- new.env(parent = emptyenv())
  warns2$warns <- list()
  r <- .util_rio_import_with_retry(fn, function() {
    suppressWarnings(suppressMessages(
      util_suppress_output(try(lambda(fn, ...), silent = TRUE), warns = warns1)
    ))
  })
  if (need_read_as_text && !util_is_try_error(r)) {
    .rio_res_is_not_null <- function(rx, .is_list) {
      if (.is_list) {
        any(vapply(rx, .rio_res_is_not_null,
            .is_list = FALSE,
            FUN.VALUE = logical(1)
          ))
      } else {
        !!prod(dim(rx))
      }
    }
    r2 <- .util_rio_import_with_retry(fn, function() {
      util_suppress_output(suppressMessages(suppressWarnings(
        try(lambda(fn, ...,
            col_types = col_types,
            colClasses = "character"
          ), silent = TRUE)
      )), warns = warns2)
    })
    if (!util_is_try_error(r2) && .rio_res_is_not_null(r2,
        .is_list = is_list
      )) {
      r <- r2
      need_type_adjust <- keep_types
    } else if (.rio_res_is_not_null(r, .is_list = is_list)) {
      util_warning("Could not read %s as text-only data frame",
        dQuote(fn),
        applicability_problem = TRUE
      )
    }
  }
  if (all(grepl(".", fn, fixed = TRUE)) &&
      rio::get_info(fn)$input %in% c("csv", "tsv")) { # data.table::fread not returning NA for "", they are only used, if not in quotes in the csv/tsv # nolint: line_length_linter.
    r[] <- lapply(r, function(x) {
      x[x %in% getOption("datatable.na.strings")] <- NA
      x
    })
  }
  if (need_type_adjust) {
    e <- environment()
    trouble_cols <- character(0)
    .adjust_type <- function(dfr, sheet) {
      dfr[] <-
        lapply(setNames(nm = colnames(dfr)), function(nm, sheet) {
          x <- dfr[[nm]]
          dt0 <- try(prep_robust_guess_data_type(x, it = 200),
            silent = TRUE
          )
          if (!util_is_try_error(dt0) && !is.na(dt0)) {
            x <- util_data_type_conversion(x, dt0)
          } else {
            if (!missing(sheet)) {
              nm <- paste(sheet, SPLIT_CHAR, nm)
            }
            e$trouble_cols <- c(e$trouble_cols, nm)
          }
          return(x)
        }, sheet = sheet)
      dfr
    }
    if (is_list) {
      r <- mapply(
        SIMPLIFY = FALSE, dfr = r, sheet = names(r),
        FUN = .adjust_type
      )
    } else {
      r <- .adjust_type(r)
    }
    if (length(e$trouble_cols) > 0) {
      util_warning(
        "Could not adjust the data type for the following columns: %s",
        util_pretty_vector_string(e$trouble_cols, n_max = 5)
      )
    }
  }
  is_excel <- endsWith(tolower(fn), ".xls") || endsWith(tolower(fn), ".xlsx")
  if (isTRUE(keep_types) && is_excel) {
    if (is_list) {
      r <- lapply(r, .util_convert_time_only_cols_df)
    } else {
      r <- .util_convert_time_only_cols_df(r)
    }
  }
  all_warns <- c(
    warns1$warns,
    warns2$warns
  )
  if (length(all_warns) > 0) {
    all_warns <- lapply(all_warns, conditionMessage)
    util_warning(
      "Could not correctly read %s: %s",
      fn, util_pretty_vector_string(all_warns, n_max = 4)
    )
  }
  r
}

.formats_with_weird_types <- local({
  suppressMessages(all_formats <- gsub("^.import.rio_", "", as.character(utils::.S3methods(".import", envir = asNamespace("rio"))))) # nolint: line_length_linter.
  mime_ext <- strsplit(.mime_chache$tb$Extensions, "\\s+", perl = TRUE)
  formats_with_mime_types <- lapply(setNames(nm = all_formats), function(fmt) {
    r <-
      .mime_chache$tb[
        (vapply(mime_ext, `%in%`,
            x = fmt,
            FUN.VALUE = logical(1)
          )),
        c("Type", "Subtype"),
        drop = TRUE
      ]
    cbind(list(fmt = rep(fmt, nrow(r))), r)
  })
  formats_with_mime_types <- formats_with_mime_types[0 < vapply(
    formats_with_mime_types,
    nrow,
    FUN.VALUE = integer(1)
  )]
  formats_with_mime_types <- util_rbind(
    data_frames_list =
      formats_with_mime_types[0 < vapply(
        formats_with_mime_types,
        nrow,
        FUN.VALUE = integer(1)
      )]
  )
  formats_with_weird_types <-
    formats_with_mime_types[
      formats_with_mime_types$Type == "text" |
      formats_with_mime_types$fmt %in% # the latter feature typed cells, not columns # nolint: line_length_linter.
      c("ods", "xls", "xlsx"),
      "fmt",
      drop = TRUE
    ]
  formats_with_weird_types
})

# Detect whether a POSIXct vector is an Excel "time-only" column
#' Internal helper: util is excel time only
#'
#' @noRd
.util_is_excel_time_only <- function(x) {
  if (!inherits(x, "POSIXct")) {
    return(FALSE)
  }
  if (all(is.na(x))) {
    return(FALSE)
  }
  # Excel time-only values are datetimes anchored at an origin date
  # (Windows origin 1899-12-30/31; macOS origin 1904-01-01)
  ux <- unique(as.Date(x[!is.na(x)], tz = "UTC"))
  origins <- as.Date(c("1899-12-30", "1899-12-31", "1904-01-01"))
  # treat as time-only if ALL non-NA dates are one of the known origins
  all(ux %in% origins)
}

# Convert POSIXct Excel time-only to hms (or difftime fallback)
#' Internal helper: util posixct to hms
#'
#' @noRd
.util_posixct_to_hms <- function(x) {
  secs <- suppressWarnings(as.numeric(x)) %% (24 * 3600)
  secs[is.na(x)] <- NA_real_
  if (requireNamespace("hms", quietly = TRUE)) {
    return(hms::as_hms(secs))
  } else {
    # fallback to difftime if hms isn't available
    return(structure(secs, class = "difftime", units = "secs"))
  }
}

# Convert all time-only columns in a data.frame
#' Internal helper: util convert time only cols df
#'
#' @noRd
.util_convert_time_only_cols_df <- function(dfr) {
  if (!is.data.frame(dfr) || !ncol(dfr)) {
    return(dfr)
  }
  for (cn in names(dfr)) {
    x <- dfr[[cn]]
    if (.util_is_excel_time_only(x)) {
      dfr[[cn]] <- .util_posixct_to_hms(x)
    }
  }
  dfr
}
