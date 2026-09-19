#' Load a folder by assuming an html page has an index
#' and following every link on that index
#'
#' @param url the url to download from
#'
#' @return path to a temporary folder containing the downloaded files
#' @noRd
util_load_folder_from_url <- function(url) {
  util_ensure_suggested("rvest",
    goal = paste0(
      "read data from the internet",
      "using prep_load_folder_with_metadata()"
    )
  )
  fp <- tempfile()
  util_stop_if_not(!file.exists(fp))
  dir.create(fp)
  completed <- FALSE
  withr::defer(if (!completed) {
    unlink(fp, recursive = TRUE, force = TRUE, expand = FALSE)
  })

  file_new <- file.path(fp, "index.html")
  try(utils::download.file(url,
      destfile = file_new,
      quiet = TRUE, mode = "wb"
    ), silent = TRUE)

  if (file.exists(file_new)) {
    fl <- try(rvest::read_html(file_new), silent = TRUE)
    if (inherits(fl, "try-error")) {
      util_error(
        "Could not read index from %s (%s): %s",
        dQuote(url),
        dQuote(file_new),
        conditionMessage(util_attr(fl, "condition", exact = TRUE))
      )
    }
  }

  links <- rvest::html_nodes(fl, "a")
  all_refs <- rvest::html_attr(links, "href")
  relative <- !startsWith(tolower(all_refs), "http://") &
    !startsWith(tolower(all_refs), "https://")
  if (startsWith(tolower(url), "file://")) {
    relative <- relative & !startsWith(tolower(all_refs), "file://")
  }
  all_refs[relative] <- paste0(url, "/", all_refs[relative])

  all_refs <- trimws(all_refs)
  lapply(
    all_refs,
    function(ref) {
      file_name <- gsub("^.*\\/", "", ref, perl = TRUE)
      file_name <- gsub("\\?.*$", "", file_name)
      file_name <- gsub("#.*$", "", file_name)
      ext <- ""
      ext <- try(util_fetch_ext(ref), silent = TRUE)
      # do not ignore content-disposition headers sent by the server (if
      # they propose a file name)
      if (length(ext) != 1 ||
          !is.character(ext)) {
        msg <- "unknown reason"
        if (inherits(ext, "try-error")) {
          msg <- conditionMessage(util_attr(ext, "condition", exact = TRUE))
        } else if (inherits(ext, "condition")) {
          msg <- conditionMessage(ext)
        }
        util_warning(
          "Could not determine the file type of %s: %s",
          dQuote(ref),
          sQuote(msg)
        )
        ext <- ""
      } else {
        ext_file_name <- util_attr(ext, "file-name", exact = TRUE)
        if (!is.null(ext_file_name)) {
          if (length(ext_file_name) == 1 &&
              !is.na(ext_file_name)) {
            file_name <- ext_file_name
          }
        }
        ext <- paste0(".", ext)
      }

      if (!endsWith(file_name, ext)) {
        file_name <- paste0(file_name, ext)
      }

      try(
        utils::download.file(ref,
          destfile = file.path(fp, file_name),
          quiet = TRUE, mode = "wb"
        ),
        silent = TRUE
      )
    }
  )
  unlink(file_new, force = TRUE)
  completed <- TRUE
  fp
}
