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
#' @keywords internal
util_rio_import <- function(fn, keep_types, ...) {
  .util_rio_import_lambda(fn, keep_types, lambda = rio::import, ...)
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
#' @keywords internal
#'
util_rio_import_list <- function(fn, keep_types, ...) {
  .util_rio_import_lambda(fn, keep_types, lambda = rio::import_list, ...)
}

.util_rio_import_lambda <- function(fn, keep_types, lambda, ...) { # FIXME: Detect invalid utf-8 codes and remove such. use readr::guess_encoding()??
  withr::local_options(list("datatable.na.strings" = c("NA",
                                                       "-",
                                                       "",
                                                       "na",
                                                       "n.a.",
                                                       "n/a")))
  is_list <- (identical(lambda, rio::import_list))
  if (!identical(lambda, rio::import) && !identical(lambda, rio::import_list)) {
    util_error(
      "Internal error, sorry, please report: invalid lambda in %s",
      sQuote("util_rio_import_lambda"))
  }
  util_expect_scalar(keep_types,
                     check_type = is.logical,
                     error_message =
                       sprintf("%s needs to be to be a logical value.",
                               sQuote("keep_types")))
  # prep_guess_encoding(fn)
  file_type_needs_type_adjust <-
    getOption("dataquieR.fix_column_type_on_read",
              dataquieR.fix_column_type_on_read_default) &&
      (all(grepl(".", fn, fixed = TRUE)) &&
         rio::get_info(fn)$input %in% .formats_with_weird_types)
  need_type_adjust <- FALSE
  # prep_robust_guess_data_type(c("2021-02-29"))
  # lapply(util_rio_import("/Library/Frameworks/R.framework/Versions/4.1/Resources/library/rgdal/gdal/gcs.csv", keep_types = FALSE), prep_robust_guess_data_type)
#
#   study_data <- util_adjust_data_type(
#     study_data = study_data,
#     meta_data = meta_data,
#     relevant_vars_for_warnings = relevant_vars_for_warnings)
  # rio::get_info("/Library/Frameworks/R.framework/Versions/4.1/Resources/library/rgdal/gdal/gcs.csv")$input
  # study_data <- util_adjust_data_type(
  #   study_data = study_data,
  #   meta_data = meta_data,
  #   relevant_vars_for_warnings = relevant_vars_for_warnings)
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
  r <- suppressWarnings(suppressMessages(
    util_suppress_output(try(lambda(fn, ...), silent = TRUE))))
  if (need_read_as_text && !util_is_try_error(r)) {
    .rio_res_is_not_null <- function(rx, .is_list) {
      if (.is_list) {
        any(vapply(rx, .rio_res_is_not_null, .is_list = FALSE,
                   FUN.VALUE = logical(1)))
      } else {
        !!prod(dim(rx))
      }
    }
    r2 <- util_suppress_output(suppressMessages(suppressWarnings(
      try(lambda(fn, ..., col_types = col_types,
                      colClasses = "character"), silent = TRUE))))
    if (!util_is_try_error(r2) && .rio_res_is_not_null(r2,
                                                       .is_list = is_list)) {
      r <- r2
      need_type_adjust <- keep_types
    } else if (.rio_res_is_not_null(r, .is_list = is_list)) {
      util_warning("Could not read %s as text-only data frame",
                   dQuote(fn),
                   applicability_problem = TRUE)
    }
  }
  if (all(grepl(".", fn, fixed = TRUE)) &&
    rio::get_info(fn)$input %in% c("csv", "tsv")) { # data.table::fread not returning NA for "", they are only used, if not in quotes in the csv/tsv
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
                     silent = TRUE)
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
      r <- mapply(SIMPLIFY = FALSE, dfr = r, sheet = names(r),
                  FUN = .adjust_type)
    } else {
      r <- .adjust_type(r)
    }
    if (length(e$trouble_cols) > 0) {
      util_warning(
        "Could not adjust the data type for the following columns: %s",
        util_pretty_vector_string(e$trouble_cols, n_max = 5))
    }
  }
  r
}

.formats_with_weird_types <- local({
  suppressMessages(all_formats <- gsub("^.import.rio_", "", as.character(utils::.S3methods(".import", envir = asNamespace("rio")))))
  mime_ext <- strsplit(.mime_chache$tb$Extensions, "\\s+", perl = TRUE)
  formats_with_mime_types <- lapply(setNames(nm = all_formats), function(fmt) {
    r <-
      .mime_chache$tb[(vapply(mime_ext, `%in%`, x = fmt,
                              FUN.VALUE = logical(1))),
                      c("Type", "Subtype"), drop = TRUE]
    cbind(list(fmt = rep(fmt, nrow(r))), r)
  })
  formats_with_mime_types <- formats_with_mime_types[0 < vapply(
    formats_with_mime_types,
    nrow,
    FUN.VALUE = integer(1)
  )]
  formats_with_mime_types <- util_rbind(data_frames_list =
                                          formats_with_mime_types[0 < vapply(
                                            formats_with_mime_types,
                                            nrow,
                                            FUN.VALUE = integer(1)
                                          )])
  formats_with_weird_types <-
    formats_with_mime_types[formats_with_mime_types$Type == "text" |
                              formats_with_mime_types$fmt %in%  # the latter feature typed cells, not columns
                              c("ods", "xls", "xlsx"),
                            "fmt", drop = TRUE];
  formats_with_weird_types
})

# FIXME: If I read the files with robust data types, int_datatype_somehing cannot run, so find a way to read it as string, first.
