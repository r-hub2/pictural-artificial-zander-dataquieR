# nolint start: line_length_linter.
#' Pre-load a file with named (usually more than) one table(s)
#'
#' These can thereafter be referred to by their names only. Such files are,
#' e.g., spreadsheet-workbooks or `RData`-files.
#' For workbook-like files, each loaded sheet is cached by its sheet name and
#' by qualified names that include the workbook base name, separated by `|`.
#' For example, sheet `tab1` in `meta_data_v2.xlsx` can be referred to as
#' `tab1`, `meta_data_v2.xlsx|tab1`, or `meta_data_v2|tab1` after the workbook
#' has been loaded. The short sheet name is convenient when the whole workbook is
#' loaded together. If item-level metadata are supplied without loading the full
#' metadata workbook, references in item-level columns such as `CODE_LIST_TABLE`
#' or `MISSING_LIST_TABLE` must use a name that is already present in the
#' data-frame cache, for example the qualified name `meta_data_v2.xlsx|tab1`.
#'
#' Note, that this function in contrast to [prep_get_data_frame] does neither
#' support selecting specific sheets/columns from a file.
#'
#' @param file the file name to load.
#' @param keep_types [logical] keep types as possibly defined in the file.
#'                             set `TRUE` for study data.
#' @param append [logical] if a data frame already exists in the cache
#'                         (by name), extend the existing one
#'
#' @return `invisible(the cache environment)`
#' @export
#' @seealso [prep_add_data_frames]
#' @seealso [prep_get_data_frame]
#' @family data-frame-cache
# nolint end
prep_load_workbook_like_file <- function(file,
  keep_types = FALSE,
  append = FALSE) {
  util_expect_scalar(append, check_type = is.logical)
  if (!missing(keep_types)) {
    util_expect_scalar(keep_types,
      check_type = is.logical,
      error_message =
        sprintf(
          "%s needs to be to be a logical value.",
          sQuote("keep_types")
        )
    )
  }

  util_expect_scalar(file,
    check_type = is.character,
    allow_more_than_one = TRUE
  )

  if (length(file) > 1 && (append || all(vapply(file, function(fn) {
    (tolower(tools::file_ext(trimws(fn))) %in% c("odm", "xml") &&
        file.exists(fn) && !dir.exists(fn) &&
        util_is_odm_xml(fn))
  }, FUN.VALUE = logical(1))))) {
    if (!append) {}
    for (fn in file) {
      prep_load_workbook_like_file(
        file = fn,
        keep_types = keep_types,
        append = TRUE
      )
    }
    return(invisible(.dataframe_environment()))
  }

  if (length(file) > 1) {
    util_error(
      c(
        "Only for ODM files, we assume append = TRUE,",
        "if more than one is read at the same time"
      )
    )
  }

  if (file %in% c(
    "meta_data_v2",
    "ship_meta_dataframe",
    "ship_meta_v2"
  )) {
    util_stop_if_not(
      `no shortcuts in tests` =
        !identical(Sys.getenv("TESTTHAT"), "true")
    )
    file <- sprintf(
      "https://dataquality.qihs.uni-greifswald.de/extdata/%s.xlsx",
      file
    )
  }

  if (startsWith(file, "https://") ||
      startsWith(file, "http://")) {
    fp <- tempfile()
    util_stop_if_not(!file.exists(fp))
    dir.create(fp)
    withr::defer(try(unlink(fp, force = TRUE, recursive = TRUE, expand = FALSE),
        silent = TRUE
      ))
    file_dec <- utils::URLdecode(file)
    fn <- gsub("^.*\\/", "", file_dec, perl = TRUE)
    fn <- gsub("\\?.*$", "", fn)
    fn <- gsub("#.*$", "", fn)

    ext <- ""
    ext <- try(util_fetch_ext(file), silent = TRUE)
    # do not ignore content-disposition headers sent by the server (if they
    # propose a file name)
    if (inherits(ext, "try-error") ||
        inherits(ext, "condition") ||
        length(ext) != 1 ||
        !is.character(ext)) {
      msg <- "unknown reason"
      if (inherits(ext, "try-error")) {
        msg <- conditionMessage(util_attr(ext, "condition", exact = TRUE))
      } else if (inherits(ext, "condition")) {
        msg <- conditionMessage(ext)
      }
      util_warning(
        "Could not determine the file type of %s: %s",
        dQuote(file),
        sQuote(msg)
      )
      ext <- ""
    } else {
      ext_file_name <- util_attr(ext, "file-name", exact = TRUE)
      if (!is.null(ext_file_name)) {
        if (length(ext_file_name) == 1 &&
            !is.na(ext_file_name)) {
          fn <- ext_file_name
        }
      }
      ext <- paste0(".", ext)
    }

    if (!endsWith(fn, ext)) {
      fn <- paste0(fn, ext)
    }

    file_new <- file.path(fp, fn)

    try(utils::download.file(file, destfile = file_new, quiet = TRUE, mode = "wb"), silent = TRUE) # nolint: line_length_linter.
    if (file.exists(file_new)) {
      file <- file_new
    }
  }

  fn <- file

  r <- util_rio_import_list(fn, keep_types = keep_types)

  if (inherits(r, "try-error") || !is.list(r) ||
      any(vapply(r, is.data.frame, FUN.VALUE = logical(1)))) {
    fn0 <- system.file("extdata", paste0(fn, ".RDS"),
      package = "dataquieR"
    )
    if (file.exists(fn0)) {
      fn <- fn0
    } else {
      fn0 <- system.file("extdata", paste0(fn, ".RData"),
        package = "dataquieR"
      )
      if (file.exists(fn0)) {
        fn <- fn0
      } else {
        fn0 <- system.file("extdata", paste0(fn, ".xlsx"),
          package = "dataquieR"
        )
        if (file.exists(fn0)) {
          fn <- fn0
        }
      }
    }
    r <- util_rio_import_list(fn, keep_types = keep_types)
  }

  if (inherits(r, "try-error") || !is.list(r) ||
    any(
      !vapply(r, is.null, FUN.VALUE = logical(1)) &
        !vapply(r, is.data.frame, FUN.VALUE = logical(1))
    )) {
    if (inherits(r, "try-error")) {
      error <- gsub("%", "%%",
        fixed = TRUE,
        conditionMessage(util_attr(r, "condition",
            exact = TRUE
          ))
      )
    } else if (!file.exists(fn)) {
      error <- "File not found"
    } else if (file.access(fn, 4)) { # file.access has inverse result logics
      error <- "Access to denied"
    } else if (dir.exists(fn)) {
      error <- "It is a folder"
    } else if (any(
      !vapply(r, is.null, FUN.VALUE = logical(1)) &
        !vapply(r, is.data.frame, FUN.VALUE = logical(1))
    )) {
      error <-
        "File does not feature a workbook-like structure, i.e., a named list of data frames" # nolint: line_length_linter.
    } else {
      error <- "Unkonwn reasons."
    }
    util_error(
      "Could not load/find file %s: %s", dQuote(file),
      error
    )
  }
  data_frame_list <- r

  # addition to load a csv file
  if (is.null(names(data_frame_list)) && length(data_frame_list) == 1) {
    names(data_frame_list) <- paste0(basename(fn))
  }

  prep_add_data_frames(data_frame_list = data_frame_list, append = append)
  data_frame_list2 <- data_frame_list
  names(data_frame_list2) <- paste0(
    basename(fn),
    SPLIT_CHAR,
    names(data_frame_list2)
  )
  r <- prep_add_data_frames(data_frame_list = data_frame_list2, append = append)
  trunc_name <- basename(fn)
  if (all(grepl("\\.(RDS|RData|xlsx)$", trunc_name, perl = TRUE))) {
    trunc_name <- gsub("\\.(RDS|RData|xlsx)$", "", trunc_name, perl = TRUE)
    data_frame_list3 <- data_frame_list
    names(data_frame_list3) <- paste0(
      trunc_name,
      SPLIT_CHAR,
      names(data_frame_list3)
    )
    r <- prep_add_data_frames(
      data_frame_list = data_frame_list3,
      append = append
    )
  }
  invisible(r)
}
