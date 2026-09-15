# nolint start: line_length_linter.
#' Create an HTML file for the [dq_report2]
#'
#' @param page_nr the number of the page being created
#' @param pages list with all page-contents named by their desired file names
#' @param rendered_pages list with all rendered (`htmltools::renderTags`) page-contents named by their desired file names
#' @param template_file the report template file to use
#' @param report the output of [dq_report2]
#' @param packageName the name of the current package
#' @param dir target directory
#' @param logo logo `PNG` file
#' @param loading loading animation div
#' @param deps dependencies, as pre-processed by
#'             `htmltools::copyDependencyToDir` and
#'             `htmltools::renderDependencies`
#' @param progress_msg [closure] to call with progress information
#' @param progress [closure] to call with progress information
#' @param title [character] the web browser's window name
#' @param by_report [logical] this report html is part of a set of reports,
#'                            add a back-link
#'
#' @return `invisible(file_name)`
#'
#' @family reporting_functions
#' @concept process
#' @noRd
# nolint end
util_create_page_file <- function(page_nr,
  pages,
  rendered_pages,
  dir,
  template_file,
  report,
  logo,
  loading,
  packageName,
  deps,
  progress_msg,
  progress,
  title,
  by_report) {
  page <- names(pages)[page_nr] # the name of the page-file being created

  file_name <- file.path(dir, page)

  if (getOption("dataquieR.resume_print", dataquieR.resume_print_default) &&
      util_is_html_file_complete(file_name)) {
    progress(page_nr / length(pages) * 100)
    return(invisible(file_name))
  }

  # Historical explicit writing message removed here.
  progress_msg("", sprintf("Writing %s...", dQuote(page)))

  util_stop_if_not(endsWith(page, ".html"))
  util_stop_if_not(!is.null(pages[[page]]))

  pg <- rendered_pages[[page]]

  pg[["dependencies"]] <- NULL

  backlink <- NULL

  report_title <- util_attr(report, "title", exact = TRUE)
  report_subtitle <- util_attr(report, "subtitle", exact = TRUE)

  if (!is.null(report_title) &&
      !isTRUE(util_attr(report_title, "default", exact = TRUE))) {
    header_text <- report_title
    if (!is.null(report_subtitle) &&
        !isTRUE(util_attr(report_subtitle, "default", exact = TRUE))) {
      header_text <- paste0(header_text, ": ", report_subtitle)
    }
  } else {
    header_text <- NULL
  }

  backlink_header <- NULL
  if (by_report) {
    backlink_header <- htmltools::a(
      class = "dq-report-overview-back",
      href = "#",
      title = "Back to reports' overview",
      `aria-label` = "Back to reports' overview",
      onclick =
        'if (window.__dqPersistPopupHistory) window.__dqPersistPopupHistory();window.location.href = "../../index.html"', # nolint: line_length_linter.
      htmltools::HTML("&larr;")
    )
  }

  if (!is.null(header_text) || by_report) {
    title_header <- NULL
    if (!is.null(header_text)) {
      title_header <- htmltools::tags$a(
        class = "dq-report-title-link",
        href = "report.html",
        header_text
      )
    }
    header <- htmltools::tagList(
      htmltools::p(
        class = "dq-title",
        backlink_header,
        title_header
      )
    )
  } else {
    header <- NULL
  }

  if (by_report) {
    by_report <- "true"
  } else {
    by_report <- "false"
  }

  html_report <- htmltools::htmlTemplate(template_file,
    by_report = by_report,
    document_ = TRUE,
    spage = pg,
    logo = logo,
    menu = .menu_env$menu(pages),
    loading = loading,
    deps = deps,
    title = title,
    backlink = backlink,
    header = header
  )

  # fix: sort reportsummarytable by first (sysmiss) and varaible column

  # https://atomiks.github.io/tippyjs/v6/all-props/

  # Historical htmltools::save_html() branch removed here.

  f <- file(description = file_name, open = "w", encoding = "utf-8")
  withr::defer(close(f))

  withCallingHandlers(
    {
      cat(as.character(html_report), file = f)
    },
    warning = function(cond) { # suppress a waning caused by ggplotly for barplots # nolint: line_length_linter.
      if (startsWith(
        conditionMessage(cond),
        "'bar' objects don't have these attributes: 'mode'"
      ) ||
        startsWith(
          conditionMessage(cond),
          "'box' objects don't have these attributes: 'mode'"
        )) {
        invokeRestart("muffleWarning")
      }
    }
  )

  progress(page_nr / length(pages) * 100)

  invisible(file_name)
}

#' Internal helper: make report id
#'
#' @noRd
util_make_report_id <- function() {
  # Stable enough uniqueness: time + pid + random
  ts <- format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC")
  pid <- Sys.getpid()
  rnd <- paste(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "") # nolint: line_length_linter.
  paste0("dq-", ts, "-p", pid, "-", rnd)
}
