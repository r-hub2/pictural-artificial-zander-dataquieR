#' Pre-load a folder with named (usually more than) one table(s)
#'
#' The original purpose of this function is to load metadata, not study data.
#' If you want to load study data, you should keep them in a different folder,
#' then you can call this function once for the metadata and once for the study
#' data but this time setting `keep_types = TRUE` to avoid all data being read
#' as [character()].
#'
#' Note, that once loaded to the data frame cache, a file won't be read again,
#' except you call [prep_purge_data_frame_cache()] or
#' [prep_remove_from_cache()]. That is, if you call this function first, and
#' [prep_get_data_frame()] later, or if `dataquieR` wants to read a file, e.g.,
#' for [dq_report2()], the file will come from the cache in the way it was
#' initially read in (`keep_types` may thus be used inadequately).
#'
#' By default, this function does not search nested folders. Pass
#' `recursive = TRUE` through `...` to [list.files()] to include them.
#'
#' Loaded files can thereafter be referred to by name only, for example
#' spreadsheet workbooks or `RData` files.
#'
#' Note, that this function in contrast to [prep_get_data_frame] does not
#' support selecting specific sheets/columns from a file.
#'
#' @param folder the folder name to load.
#' @param keep_types [logical] keep types as possibly defined in the file.
#'                             set `TRUE` for study data.
#' @param append [logical] if a data frame already exists in the cache
#'                         (by name), extend the existing one
#' @param ... arguments passed to [list.files()]
#'
#' @return `invisible(the cache environment)`
#' @export
#' @seealso [prep_add_data_frames]
#' @seealso [prep_get_data_frame]
#' @family data-frame-cache
prep_load_folder_with_metadata <- function(
  folder,
  keep_types = FALSE,
  append = FALSE,
  ...
) {
  util_expect_scalar(append, check_type = is.logical)
  util_expect_scalar(folder, check_type = is.character)
  util_stop_if_not(
    "full.names not supported by prep_load_folder_with_metadata" =
      (!"full.names" %in% rlang::call_args_names(
        rlang::current_call()
      ))
  )
  if (startsWith(folder, "https://") ||
      startsWith(folder, "http://")) {
    folder <- util_load_folder_from_url(folder)
    withr::defer(unlink(folder, recursive = TRUE, force = TRUE,
        expand = FALSE))
  }

  util_stop_if_not(`Folder not found` = dir.exists(folder))
  util_stop_if_not(`Access denied` = file.access(folder) == 0)

  fls <- list.files(folder,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE,
    include.dirs = FALSE,
    ...
  )
  if (any(startsWith(basename(fls), "~$") &
        (endsWith(basename(fls), ".xlsx") |
            endsWith(basename(fls), ".xls")
        ))) {
    util_warning(
      c(
        "Found files that look like Excel",
        "working copies https://superuser.com/a/901749",
        "Do you have Excel open? This may cause warnings about",
        "files that cannot be opened/are locked. If not open, maybe, Excel",
        "had crashed before. You should close Excel, if it is not running,",
        "open it, it may address this. If nothing helps, consider moving",
        "all files whose names start with ~$ and end with .xls or xlsx away."
      )
    )
  }
  if (any(
    startsWith(basename(fls), ".~lock.") &
      endsWith(basename(fls), "#") # libreoffice
  )) {
    util_warning(
      c(
        "Found files that look like LibreOffice/OpenOffice/StarOffice",
        "working copies https://superuser.com/a/901749",
        "Do you have one of these apps open? This may cause warnings about",
        "files that cannot be opened/are locked. If not open, maybe, one of",
        "these apps had crashed before. You should close all such apps.",
        "If they don't run, open them, they may address this. ",
        "If nothing helps, consider moving",
        "all files whose names start with .~lock. and end with # away."
      )
    )
  }
  lapply(fls, function(fn) {
    if (inherits(
      suppressWarnings(try(
        prep_load_workbook_like_file(fn,
          keep_types = keep_types,
          append = append
        ),
        silent = TRUE
      )),
      "try-error"
    )) {
      if (inherits(
        suppressWarnings(try(
          prep_get_data_frame(fn,
            keep_types = keep_types
          ),
          silent = TRUE
        )),
        "try-error"
      )) {
        util_warning("Could not load %s, ignoring...", dQuote(fn))
      }
    }
  })

  invisible(.dataframe_environment())
}
