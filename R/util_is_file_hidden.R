#' Check whether a file is hidden on the current platform
#'
#' @param fp A character vector of file paths.
#' @return A logical vector indicating hidden files.
#' @noRd
util_is_file_hidden <- function(fp) {
  if (.Platform$OS.type != "windows") {
    startsWith(basename(fp), ".")
  } else {
    res <- system(paste0("attrib ", fp), intern = TRUE)
    grepl(r"[^(.*\s+)?H]", res, perl = TRUE)
  }
}
