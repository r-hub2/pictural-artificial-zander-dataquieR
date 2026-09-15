# This file creates an environment with functions, that generate an
# html menu
# used by [print.dataquieR_resultset2]

#' `.menu_env` -- an environment for HTML menu creation
#'
#' used by the dq_report2-pipeline
#' @name menu_env
#' @noRd
.menu_env <- new.env(parent = environment())

.menu_env$display_title <- function(title, dropdown, max_chars = 64L) {
  full_title <- as.character(title)
  if (!identical(dropdown, "Single Variables") ||
      nchar(full_title) <= max_chars) {
    return(title)
  }
  display_title <- paste0(substr(full_title, 1L, max_chars - 3L), "...")
  attributes(display_title) <- attributes(title)
  attr(display_title, "full_menu_title") <- full_title
  display_title
}

#' Create a single menu entry
#'
#' @param title of the entry
#' @param id linked `href`, defaults to modified title. can be a word, then
#'        a single-page-link with an anchor tag is created.
#' @param ... additional arguments for the menu link
#' @return html-a-tag object
#' @name menu_env_menu_entry
#' @keywords internal
.menu_env$menu_entry <- function(title,
  id = title,
  ...) {
  full_menu_title <- util_attr(title, "full_menu_title", exact = TRUE)
  if (!grepl("#", id, fixed = TRUE) &&
      !startsWith(id, "http://") &&
      !startsWith(id, "https://") &&
      !startsWith(id, "file:") &&
      !startsWith(id, "ftp://") &&
      !startsWith(id, "ftps://") &&
      !startsWith(id, "sftp://") &&
      !startsWith(id, "ssh:") &&
      !startsWith(id, "gopher:") &&
      !startsWith(id, "scp:") &&
      !startsWith(id, "rsync:")) {
    hash_if_needed <- "#"
    id <- prep_link_escape(id,
      html = TRUE
    )
  } else {
    hash_if_needed <- ""
    link <- sub("#.*$", "", id)
    hash <- sub("^.*?#", "", id)
    hash <- htmltools::urlEncodePath(as.character(hash))
    id <- paste0(link, "#", hash)
  }
  if (!is.null(util_attr(title, "alternative_names", exact = TRUE)) &&
      suppressWarnings(util_ensure_suggested("jsonlite",
          goal = "Alias names in menu for search",
          err = FALSE
        ))) {
    alternative_names <-
      jsonlite::toJSON(util_attr(title, "alternative_names", exact = TRUE),
        auto_unbox = TRUE
      )
  } else {
    alternative_names <- NULL
  }

  htmltools::a(
    href = sprintf(
      "%s%s",
      hash_if_needed,
      id
    ),
    `data-alternative-names` = alternative_names,
    title = full_menu_title,
    title,
    ...
  )
}

.menu_env$menu_separator <- function() {
  htmltools::tags$hr(class = "dropdown-divider")
}

#' Creates a drop-down menu
#'
#' @param title name of the entry in the main menu
#' @param menu_description description, displayed, if the main
#'                         menu entry itself is clicked
#' @param ... the sub-menu-entries
#' @param id id for the entry, defaults to modified title
#'
#' @return html div object
#' @name menu_env_drop_down
#' @keywords internal
.menu_env$drop_down <- function(title,
  menu_description,
  ..., id = prep_link_escape(title)) {
  htmltools::div(
    class = "dropdown",
    id = id,
    onclick = sprintf(
      "showDescription('%s', '%s'); event.stopPropagation()",
      id,
      htmltools::htmlEscape(
        menu_description,
        TRUE
      )
    ),
    htmltools::tags$button(
      class = "dropbtn",
      htmltools::tagList(
        htmltools::tags$p(htmltools::htmlEscape(title)),
        htmltools::tags$i(
          class = "fa fa-caret-down"
        )
      )
    ),
    htmltools::div(
      class = "dropdown-content",
      # https://stackoverflow.com/a/33225276
      ...
    )
  )
}

.menu_env$menu_separator <- function() {
  htmltools::tags$hr(class = "dropdown-divider")
}

#' Generate the menu for a report
#'
#' @param pages encapsulated `list` with report pages as `tagList` objects,
#'              its names are the desired file names
#'
#' @return the html-`taglist` for the menu
#' @name menu_env-menu
#' @keywords internal
.menu_env$menu <- function(pages) {
  entry_env <- environment()
  entries_of_dd <- lapply(names(pages), function(fn) {
    dd <- lapply(names(pages[[fn]]), function(sp) {
      r <- util_attr(pages[[fn]][[sp]], "dropdown", exact = TRUE)
      if (length(r) != 1) {
        r <- "Dropdown"
        attr(entry_env$pages[[fn]][[sp]], "dropdown") <- r
      }
      r
    })
    dd <- unique(dd)
    lapply(setNames(nm = dd), function(ddn) {
      eoddn <- lapply(names(pages[[fn]]), function(sp) {
        if (util_attr(pages[[fn]][[sp]], "dropdown", exact = TRUE) == ddn) {
          util_attach_attr(sp,
            fn = fn,
            menu_separator_before =
              util_attr(
                pages[[fn]][[sp]],
                "menu_separator_before",
                exact = TRUE
              ),
            alternative_names =
              util_attr(pages[[fn]][[sp]]$attribs$id,
                "alternative_names",
                exact = TRUE
              ),
            menu_separator_before =
              util_attr(
                pages[[fn]][[sp]],
                "menu_separator_before",
                exact = TRUE
              )
          )
        } else {
          NULL
        }
      })
      eoddn
    })
  })
  all_dd <- unique(unlist(lapply(entries_of_dd, names)))

  menu_from_pages <- lapply(all_dd, function(ddn) {
    util_ensure_suggested("markdown")
    ddmen <- do.call(c, lapply(
      unlist(lapply(entries_of_dd, `[[`, ddn), recursive = FALSE),
      function(me) {
        if (is.null(me)) {
          list(NULL)
        } else {
          display_title <- .menu_env$display_title(me, ddn)
          menu_entry <- do.call("call",
            args = c(list(
              "menu_entry",
              display_title,
              sprintf(
                "%s#%s",
                util_attr(me, "fn",
                  exact = TRUE
                ),
                me
              )
            )),
            quote = TRUE
          )
          if (isTRUE(util_attr(me, "menu_separator_before", exact = TRUE))) {
            list(call("menu_separator"), menu_entry)
          } else {
            list(menu_entry)
          }
        }
      }
    ))
    ddmen <- Filter(Negate(is.null), ddmen)
    concept_info <- subset(util_get_concept_info("dqi"),
      get("Dimension") == ddn & get("Level") == 1 &
        get("dataquierR pipeline include") == 1,
      select = c(
        "Definition", "Explanation", "Guidance", "abbreviation",
        "IndicatorID"
      ),
      drop = FALSE
    )
    if (nrow(concept_info) == 1) { # https://dataquality.qihs.uni-greifswald.de/PDQC_DQ_1_0_0_0.html # nolint: line_length_linter.
      if (!util_empty(concept_info$IndicatorID)) {
        href <- sprintf(
          "https://dataquality.qihs.uni-greifswald.de/PDQC_%s.html",
          concept_info$IndicatorID
        )
      } else {
        href <- NULL
      }
      menu_description <-
        htmltools::tagList(
          htmltools::h2(ddn),
          htmltools::tags$p(
            htmltools::a(
              href =
                href,
              target = "_blank",
              title = "Online reference",
              ddn
            ),
            ifelse(util_empty(concept_info$abbreviation), "", sprintf(
              " -- related indicator function names are prefixed with %s",
              dQuote(concept_info$abbreviation)
            ))
          ),
          htmltools::h3("Definition"),
          htmltools::tags$p(
            htmltools::HTML(markdown::markdownToHTML(
              text = concept_info$Definition, fragment.only = TRUE
            ))
          ),
          htmltools::h3("Explanation"),
          htmltools::tags$p(
            htmltools::HTML(markdown::markdownToHTML(
              text = concept_info$Explanation, fragment.only = TRUE
            ))
          ),
          htmltools::h3("Guidance"),
          htmltools::tags$p(
            htmltools::HTML(markdown::markdownToHTML(
              text = concept_info$Guidance, fragment.only = TRUE
            ))
          )
        )
    } else if (identical(ddn, VARIABLE_GROUP_REPORT_MENU)) {
      menu_description <- htmltools::tagList(
        htmltools::h2(ddn),
        htmltools::h3("Definition"),
        htmltools::tags$p(
          paste(
            "Results from checks that assess a defined group of variables,",
            "including scale metrics and other group-level analyses."
          )
        ),
        htmltools::h3("Explanation"),
        htmltools::tags$p(
          paste(
            "This menu provides one place for scale metrics and other",
            "variable-group results without duplicating results already",
            "available on a shared dimension page."
          )
        ),
        htmltools::h3("Guidance"),
        htmltools::tags$p(
          paste(
            "Use the drop-down menu to review scale metrics or results by",
            "variable group. Shared correlation and contradiction results",
            "remain on their dimension pages."
          )
        )
      )
    } else {
      menu_description <-
        htmltools::tagList(
          htmltools::h2(ddn),
          htmltools::tags$p(
            sprintf("No unique description for %s in DQ concept", dQuote(ddn)),
          )
        )
    }
    do.call("call", args = c(
      list("drop_down",
        ddn,
        menu_description =
          as.character(menu_description)
      ),
      ddmen
    ), quote = TRUE)
  })
  do.call(htmltools::tagList,
    lapply(unlist(menu_from_pages), eval, envir = entry_env),
    quote = FALSE
  )
}

# make all the functions in the environment enclosed by this environment, too,
# so that they can look up this environment for metadata
for (f in ls(.menu_env)) {
  if (is.function(.menu_env[[f]])) {
    environment(.menu_env[[f]]) <- .menu_env
  }
}
