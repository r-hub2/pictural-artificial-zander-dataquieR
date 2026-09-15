#' Copy default dependencies to the report's lib directory
#'
#' @param dir report directory
#' @param pages all pages to write
#' @param ... additional `htmltools::htmlDependency` objects to be added to all
#'   pages, also
#' @param copy_dependencies [logical] whether to copy dependency files to the
#'   report library. Set this to `FALSE` only after a previous render step has
#'   already copied the same dependency directories.
#'
#' @return pre-processed dependencies -- a list with `deps` and rendered `pages`
#'
#' @family reporting_functions
#' @concept process
#' @noRd
util_copy_all_deps <- function(dir, pages, ..., copy_dependencies = TRUE) {
  dir <- normalizePath(dir, winslash = "/", mustWork = FALSE)
  libdir <- file.path(dir, "lib")

  withCallingHandlers(
    {
      rendered_pages <- lapply(pages, htmltools::renderTags)
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

  deps <- c(lapply(rendered_pages, `[[`, "dependencies"),
    GLOBAL_ = list(list(...))
  )

  names(deps)[util_empty(names(deps))] <- paste0(
    "__NULL__",
    seq_len(sum(util_empty(
      names(deps)
    ))),
    "__NULL__"
  )

  deps_cnt <- lapply(deps, length)

  pos_in_file <- lapply(deps_cnt, seq_len)

  deps <- unlist(deps, recursive = FALSE)

  deps_info <- data.frame(
    index = seq_along(deps),
    file_name =
      unlist(lapply(names(deps_cnt), function(nm) rep(nm, deps_cnt[[nm]]))),
    pos_in_file = unlist(pos_in_file)
  )
  deps_info$version <- vapply(deps, `[[`, "version", FUN.VALUE = character(1))
  deps_info$name <- vapply(deps, `[[`, "name", FUN.VALUE = character(1))
  deps_info$take <- rep(FALSE, nrow(deps_info))

  # Omit older version, if libraries are duplicated
  deps_info <- split(deps_info, deps_info$name)

  deps_info <- lapply(deps_info, function(x) {
    r <- x[order(numeric_version(x$version),
        decreasing = TRUE,
        na.last = TRUE
      ), , drop = FALSE]
    r[1, "take"] <- TRUE
    r
  })

  deps_info <- do.call(rbind, deps_info)

  # overall order of dependencies

  n_deps <- length(unique(deps_info$name))

  # find dep, which is either not in a file or at first position

  order_of_deps <- unique(unname(unlist(lapply(setNames(nm = seq_len(n_deps)), function(pos) { # nolint: line_length_linter.
    lapply(setNames(nm = unique(deps_info$name)), function(nm) {
      r <- deps_info[
        deps_info$name == nm & deps_info$pos_in_file > pos, ,
        drop = FALSE
      ]
      if (nrow(r)) {
        NULL
      } else {
        nm
      }
    })
  }))))


  index <- deps_info[deps_info$take, "index", drop = TRUE]

  deps <- deps[index]

  names(deps) <- vapply(deps, `[[`, "name", FUN.VALUE = character(1))

  deps <- deps[order_of_deps]
  first_deps <- intersect(c("jquery", "htmlwidgets"), names(deps))
  if (length(first_deps)) {
    deps <- c(deps[first_deps], deps[setdiff(names(deps), first_deps)])
  }
  deps <- util_order_report_dependencies(deps)

  if (isTRUE(copy_dependencies)) {
    deps <-
      lapply(deps,
        htmltools::copyDependencyToDir,
        outputDir = libdir,
        mustWork = TRUE
      ) # no external http-dependencies allowed (mustWork)
  } else {
    deps <- lapply(deps, function(dep) {
      copied_dir <- file.path(libdir, paste0(dep$name, "-", dep$version))
      if (dir.exists(copied_dir)) {
        dep$src$file <- copied_dir
        dep
      } else {
        htmltools::copyDependencyToDir(
          dep,
          outputDir = libdir,
          mustWork = TRUE
        )
      }
    })
  }

  deps <- lapply(deps, util_remove_external_font_references)

  deps <- lapply(deps, function(dep) {
    dep$src$file <- normalizePath(
      dep$src$file,
      winslash = "/",
      mustWork = TRUE
    )
    htmltools::makeDependencyRelative(
      dep,
      basepath = dir,
      mustWork = TRUE
    )
  }) # no external http-dependencies allowed (mustWork)

  list(
    deps = htmltools::renderDependencies(deps, "file"),
    rendered_pages = rendered_pages
  )
}

#' Keep report-wide JavaScript dependencies executable
#'
#' Report pages contribute dependencies independently. Their merged order can
#' therefore place a DataTables extension before the DataTables core library.
#' This helper preserves the merged order except for known prerequisites.
#'
#' @param deps named list of [htmltools::htmlDependency()] objects
#'
#' @return `deps` in executable order
#'
#' @noRd
util_order_report_dependencies <- function(deps) {
  move_before <- function(dependencies, prerequisite, dependents) {
    dependency_names <- names(dependencies)
    prerequisite_position <- match(prerequisite, dependency_names)
    dependent_positions <- match(dependents, dependency_names, nomatch = 0L)
    dependent_positions <- dependent_positions[dependent_positions > 0L]
    if (is.na(prerequisite_position) || length(dependent_positions) == 0L ||
        prerequisite_position < min(dependent_positions)) {
      return(dependencies)
    }

    prerequisite_dependency <- dependencies[prerequisite]
    dependencies <- dependencies[dependency_names != prerequisite]
    insertion_position <- min(match(
      dependents,
      names(dependencies),
      nomatch = length(dependencies) + 1L
    ))
    append(
      dependencies,
      prerequisite_dependency,
      after = insertion_position - 1L
    )
  }

  dependency_names <- names(deps)
  data_tables_extensions <- grep(
    "^dt-.*-js$",
    dependency_names,
    value = TRUE
  )
  deps <- move_before(
    deps,
    "datatables-core-js",
    c("jspdf", "dt2-binding", data_tables_extensions)
  )
  data_tables_extensions <- grep(
    "^dt-ext-",
    names(deps),
    value = TRUE
  )
  deps <- move_before(
    deps,
    "dt-core",
    c("jspdf", data_tables_extensions)
  )
  deps <- move_before(deps, "moment", "dt-datetime-js")
  deps <- move_before(deps, "jszip", "dt-buttons-js")
  deps <- move_before(deps, "pdfmake", "dt-buttons-js")
  deps
}

#' Remove remote font references from copied report dependencies
#'
#' @param dep an [htmltools::htmlDependency()] copied to a local directory
#'
#' @return `dep`, with copied CSS files normalized in place
#'
#' @noRd
util_remove_external_font_references <- function(dep) {
  css_files <- dep[["stylesheet"]]
  dep_dir <- dep[["src"]][["file"]]
  if (is.null(css_files) || is.null(dep_dir)) {
    return(dep)
  }

  for (css_file in css_files) {
    css_path <- file.path(dep_dir, css_file)
    if (!file.exists(css_path)) {
      next
    }
    css <- readLines(css_path, warn = FALSE)
    css <- util_remove_external_font_css(css)
    writeLines(css, css_path, useBytes = TRUE)
  }

  dep
}

#' Remove remote font imports and prefer system fonts
#'
#' @param css CSS file contents as a character vector
#'
#' @return sanitized CSS file contents
#'
#' @noRd
util_remove_external_font_css <- function(css) {
  remote_font_line <- grepl(
    "@import[^;]+fonts\\.(googleapis|gstatic)\\.com",
    css
  )
  css <- css[!remote_font_line]
  css <- gsub(
    "url\\(['\"]?https://fonts\\.(googleapis|gstatic)\\.com[^)]*\\)",
    "",
    css
  )
  css <- gsub(
    "['\"]Jost['\"],[[:space:]]*",
    "",
    css
  )
  css
}
