#' Inform about cached data frame retrieval
#'
#' @param x cached data frame.
#' @param data_frame_name requested cache key.
#' @param keep_types requested `keep_types` value.
#' @param keep_types_missing whether `keep_types` was omitted by the caller.
#'
#' @return Invisibly returns `x`.
#' @noRd
util_message_cached_data_frame <- function(x,
  data_frame_name,
  keep_types,
  keep_types_missing) {
  cached_keep_types <- util_attr(x, "dataquieR_data_frame_keep_types",
    exact = TRUE
  )
  source <- util_attr(x, "dataquieR_data_frame_source", exact = TRUE)
  loaded_at <- util_attr(x, "dataquieR_data_frame_loaded_at", exact = TRUE)

  keep_types_known <- isTRUE(cached_keep_types) ||
    identical(cached_keep_types, FALSE)
  keep_types_differs <- keep_types_known &&
    isTRUE(keep_types) &&
    !identical(cached_keep_types, keep_types)
  keep_types_explicit <- !keep_types_missing

  if (!keep_types_differs &&
      !(keep_types_explicit && isTRUE(keep_types) && !keep_types_known)) {
    return(invisible(x))
  }

  requested <- if (keep_types_missing) {
    sprintf("the default %s = %s", sQuote("keep_types"), dQuote(keep_types))
  } else {
    sprintf("%s = %s", sQuote("keep_types"), dQuote(keep_types))
  }
  cached <- if (keep_types_known) {
    sprintf(
      "originally loaded with %s = %s",
      sQuote("keep_types"), dQuote(cached_keep_types)
    )
  } else {
    "registered without a recorded keep_types setting"
  }
  source_msg <- if (length(source) == 1 && nzchar(source)) {
    sprintf(" from %s", dQuote(source))
  } else {
    ""
  }
  loaded_msg <- if (inherits(loaded_at, "POSIXt")) {
    sprintf(" at %s", format(loaded_at, usetz = TRUE))
  } else {
    ""
  }

  util_message(
    c(
      "Returning cached data frame %s%s%s, %s.",
      "The current %s request is not used to reload or re-type cached",
      "data frames. Use %s or %s to force a fresh load."
    ),
    dQuote(data_frame_name),
    source_msg,
    loaded_msg,
    cached,
    requested,
    sQuote(sprintf("prep_remove_from_cache('%s')", data_frame_name)),
    sQuote("prep_purge_data_frame_cache()"),
    applicability_problem = FALSE,
    intrinsic_applicability_problem = FALSE,
    once_id = paste0(
      "cached_data_frame_keep_types:",
      rlang::hash(list(
        data_frame_name,
        cached_keep_types,
        keep_types,
        keep_types_missing
      ))
    )
  )
  invisible(x)
}

#' Mark a data frame as read into the data frame cache
#'
#' @param x data frame.
#' @param data_frame_name cache key.
#' @param source source file, URL, or object reference.
#' @param keep_types `keep_types` value used while loading.
#'
#' @return `x` with cache metadata attributes.
#' @noRd
util_set_data_frame_cache_attrs <- function(x,
  data_frame_name,
  source,
  keep_types) {
  if (!is.data.frame(x)) {
    return(x)
  }
  attr(x, "dataquieR_data_frame_cache_name") <- data_frame_name
  attr(x, "dataquieR_data_frame_source") <- source
  attr(x, "dataquieR_data_frame_keep_types") <- keep_types
  attr(x, "dataquieR_data_frame_loaded_at") <- Sys.time()
  x
}

#' Internal helper: system file
#'
#' @noRd
util_system_file <- function(...) {
  system.file(...)
}

# nolint start: line_length_linter.
#' Read data from files/URLs
#'
#' data_frame_name can be a file path or an URL you can append a pipe and a
#' sheet name for Excel files or object name e.g. for `RData` files. Numbers
#' may also work. All file formats supported by your `rio` installation will
#' work.
#'
#' The data frames will be cached automatically. This cache is mainly a registry
#' of already loaded or explicitly registered tables: repeated calls by the same
#' name return the registered object and do not re-read the source file or URL.
#' Arguments such as `keep_types` therefore affect loading, but not later
#' retrieval of an already cached data frame. You can define an alternative
#' environment for this using the argument `.data_frame_list`, and you can purge
#' the cache using [prep_purge_data_frame_cache].
#'
#' Use [prep_add_data_frames] to manually add data frames to the
#' cache, e.g., if you have loaded them from more complex sources, before.
#'
#' @param data_frame_name [character] name of the data frame to read, see
#'   details
#' @param .data_frame_list [environment] cache for loaded data frames
#' @param keep_types [logical] keep types as possibly defined in a file, if the
#'                             data frame is loaded from one. set `TRUE` for
#'                             study data.
#' @param column_names_only [logical] if TRUE imports only headers (column names)
#'                                    of the data frame and no content
#'                                    (an empty data frame)
#'
#' @return [data.frame] a data frame
#' @export
#' @seealso [prep_add_data_frames]
#' @seealso [prep_load_workbook_like_file]
#' @family data-frame-cache
#'
#' @examples
#' \dontrun{
#' bl <- as.factor(prep_get_data_frame(
#'   paste0(
#'     "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus",
#'     "/Projekte_RKI/COVID-19_Todesfaelle.xlsx?__blob=",
#'     "publicationFile|COVID_Todesfälle_BL|Bundesland"
#'   )
#' )[[1]])
#'
#' n <- as.numeric(prep_get_data_frame(paste0(
#'   "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/",
#'   "Projekte_RKI/COVID-19_Todesfaelle.xlsx?__blob=",
#'   "publicationFile|COVID_Todesfälle_BL|Anzahl verstorbene",
#'   " COVID-19 Fälle"
#' ))[[1]])
#' plot(bl, n)
#' # Working names would be to date (2022-10-21), e.g.:
#' #
#' # https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/ \
#' #    Projekte_RKI/COVID-19_Todesfaelle.xlsx?__blob=publicationFile
#' # https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/  \
#' #    Projekte_RKI/COVID-19_Todesfaelle.xlsx?__blob=publicationFile|2
#' # https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/ \
#' #    Projekte_RKI/COVID-19_Todesfaelle.xlsx?__blob=publicationFile|name
#' # study_data
#' # ship
#' # meta_data
#' # ship_meta
#' #
#' prep_get_data_frame("meta_data | meta_data")
#' }
# nolint end
prep_get_data_frame <- function(data_frame_name,
  .data_frame_list = .dataframe_environment(),
  keep_types = FALSE,
  column_names_only = FALSE) {
  keep_types_missing <- missing(keep_types)
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

  util_expect_scalar(column_names_only,
    check_type = is.logical,
    error_message =
      sprintf(
        "%s needs to be to be a logical value.",
        sQuote("column_names_only")
      )
  )

  data_frame_name <- gsub("\\s*\\|\\s*", "|", data_frame_name)

  util_expect_scalar(data_frame_name, check_type = is.character)
  if (!is.environment(.data_frame_list)) {
    util_error(
      "%s must be an environment, if specified",
      sQuote(".data_frame_list")
    )
  }

  # Special case of ship.RDS not present in the package anymore
  if (grepl(sprintf("^ship(\\s*\\%s.*)?$", SPLIT_CHAR), data_frame_name)) {
    util_stop_if_not(
      `no shortcuts in tests` =
        !identical(Sys.getenv("TESTTHAT"), "true")
    )

    # nocov start
    # this cannot be checked, because we disallow shortcuts in tests, see above.
    data_frame_name <-
      sub(
        sprintf("^ship(\\s*\\%s.*)?$", SPLIT_CHAR),
        "https://dataquality.qihs.uni-greifswald.de/extdata/ship.RDS\\1",
        data_frame_name
      )
    # nocov end
  }

  # Special case of ship.RDS not present in the package anymore
  if (grepl(
    sprintf("^ship_subset[1-3](\\s*\\%s.*)?$", SPLIT_CHAR),
    data_frame_name
  )) {
    util_stop_if_not(
      `no shortcuts in tests` =
        !identical(Sys.getenv("TESTTHAT"), "true")
    )

    # nocov start
    # this cannot be checked, because we disallow shortcuts in tests, see above.
    data_frame_name <-
      sub(
        sprintf("^ship_subset([1-3])(\\s*\\%s.*)?$", SPLIT_CHAR),
        "https://dataquality.qihs.uni-greifswald.de/extdata/ship_subset\\1.RDS\\2", # nolint: line_length_linter.
        data_frame_name
      )
    # nocov end
  }

  # Special case speed-up
  if ("grading_rulesets" == data_frame_name &&
      !exists(data_frame_name,
        envir = .dataframe_environment(),
        mode = "list"
      )) {
    data_frame_name <- util_system_file("grading_rulesets.xlsx",
      package = "dataquieR"
    )
  }
  # Special case speed-up
  if ("grading_formats" == data_frame_name &&
      !exists(data_frame_name,
        envir = .dataframe_environment(),
        mode = "list"
      )) {
    data_frame_name <- util_system_file("grading_formats.xlsx",
      package = "dataquieR"
    )
  }


  # Special case of ship.RDS not present in the package anymore
  if (grepl(sprintf("^ship_meta_v2(\\s*\\%s.*)?$", SPLIT_CHAR), data_frame_name)) { # nolint: line_length_linter.
    util_stop_if_not(
      `no shortcuts in tests` =
        !identical(Sys.getenv("TESTTHAT"), "true")
    )

    # nocov start
    # this cannot be checked, because we disallow shortcuts in tests, see above.
    data_frame_name <-
      sub(
        sprintf("^ship_meta_v2(\\s*\\%s.*)?$", SPLIT_CHAR),
        "https://dataquality.qihs.uni-greifswald.de/extdata/ship_meta_v2.xlsx\\1", # nolint: line_length_linter.
        data_frame_name
      )
    # nocov end
  }

  # Special case of ship.RDS not present in the package anymore
  if (grepl(sprintf("^ship_meta_dataframe(\\s*\\%s.*)?$", SPLIT_CHAR), data_frame_name)) { # nolint: line_length_linter.
    util_stop_if_not(
      `no shortcuts in tests` =
        !identical(Sys.getenv("TESTTHAT"), "true")
    )

    # nocov start
    # this cannot be checked, because we disallow shortcuts in tests, see above.
    data_frame_name <-
      sub(
        sprintf("^ship_meta_dataframe(\\s*\\%s.*)?$", SPLIT_CHAR),
        "https://dataquality.qihs.uni-greifswald.de/extdata/ship_meta_dataframe.xlsx\\1", # nolint: line_length_linter.
        data_frame_name
      )
    # nocov end
  }

  # Special case of ship.RDS not present in the package anymore
  if (grepl(sprintf("^ship_meta(\\s*\\%s.*)?$", SPLIT_CHAR), data_frame_name)) {
    util_stop_if_not(
      `no shortcuts in tests` =
        !identical(Sys.getenv("TESTTHAT"), "true")
    )

    # nocov start
    # this cannot be checked, because we disallow shortcuts in tests, see above.
    data_frame_name <-
      sub(
        sprintf("^ship_meta(\\s*\\%s.*)?$", SPLIT_CHAR),
        "https://dataquality.qihs.uni-greifswald.de/extdata/ship_meta.RDS\\1",
        data_frame_name
      )
    # nocov end
  }

  # Special case of study_data.RData not present in the package anymore
  if (grepl(
    sprintf("^study_data(\\s*\\%s.*)?$", SPLIT_CHAR),
    data_frame_name
  ) && !exists(data_frame_name,
    envir = .dataframe_environment(),
    mode = "list"
  )) {
    util_stop_if_not(
      `no shortcuts in tests` =
        !identical(Sys.getenv("TESTTHAT"), "true")
    )

    # nocov start
    # this cannot be checked, because we disallow shortcuts in tests, see above.
    data_frame_name <-
      sub(
        sprintf("^study_data(\\s*\\%s.*)?$", SPLIT_CHAR),
        "https://dataquality.qihs.uni-greifswald.de/extdata/study_data.RData\\1", # nolint: line_length_linter.
        data_frame_name
      )
    # nocov end
  }

  # Special case of study_data.RData not present in the package anymore
  if (grepl(
    sprintf("^meta_data_v2(\\s*\\%s.*)?$", SPLIT_CHAR),
    data_frame_name
  )) {
    util_stop_if_not(
      `no shortcuts in tests` =
        !identical(Sys.getenv("TESTTHAT"), "true")
    )

    # nocov start
    # this cannot be checked, because we disallow shortcuts in tests, see above.
    data_frame_name <-
      sub(
        sprintf("^meta_data_v2(\\s*\\%s.*)?$", SPLIT_CHAR),
        "https://dataquality.qihs.uni-greifswald.de/extdata/meta_data_v2.xlsx\\1", # nolint: line_length_linter.
        data_frame_name
      )
    # nocov end
  }

  # Special case of study_data.RData not present in the package anymore
  if (grepl(
    sprintf("^meta_data(\\s*\\%s.*)?$", SPLIT_CHAR),
    data_frame_name
  )) {
    util_stop_if_not(
      `no shortcuts in tests` =
        !identical(Sys.getenv("TESTTHAT"), "true")
    )

    # nocov start
    # this cannot be checked, because we disallow shortcuts in tests, see above.
    data_frame_name <-
      sub(
        sprintf("^meta_data(\\s*\\%s.*)?$", SPLIT_CHAR),
        "https://dataquality.qihs.uni-greifswald.de/extdata/meta_data.RData\\1",
        data_frame_name
      )
    # nocov end
  }

  if (all(startsWith(
    data_frame_name,
    "https://pfau.qihs.uni-greifswald.de/dfg_website_rendered/extdata/fortests/"
  ) |
    startsWith(
      data_frame_name,
      "https://dataquality.qihs.uni-greifswald.de/extdata/"
    ))) {
    # trust our own example files
    withr::local_options(list(rio.import.trust = TRUE))
  }

  if (!column_names_only && nzchar(data_frame_name) && exists(data_frame_name,
      envir = .data_frame_list, mode = "list"
    )) {
    r <- get(data_frame_name, envir = .data_frame_list, mode = "list")
    if (is.data.frame(r)) {
      util_message_cached_data_frame(
        x = r,
        data_frame_name = data_frame_name,
        keep_types = keep_types,
        keep_types_missing = keep_types_missing
      )
      return(r)
    }
  }

  # Historical early rio dependency check removed here.

  fn <- data_frame_name
  which <- NULL
  col <- NULL

  if (any(grepl(SPLIT_CHAR, data_frame_name, fixed = TRUE))) {
    splitted <- trimws(strsplit(data_frame_name, SPLIT_CHAR, fixed = TRUE)[[1]])

    if (length(splitted) > 2) { # 1-column data frame requested
      col <- tail(splitted, 1)
      splitted <- head(splitted, -1)
    }

    which <- tail(splitted, 1)
    suppressWarnings(
      if (is.finite(as.integer(which)) && as.integer(which) < 500) {
        which <- as.integer(which)
      }
    )
    fn <- paste0(head(splitted, -1), collapse = SPLIT_CHAR)
  }

  r <- NULL

  if (startsWith(fn, "<") &&
      endsWith(fn, ">")) {
    fn <- paste0("voc:", substr(fn, 2, nchar(fn) - 1))
  }
  if (startsWith(fn, "voc:")) {
    .voc <- sub("^voc:", "", fn)
    voc_tab <- util_get_voc_tab()
    # table lookup to modify fn to package: or data:
    recall_arg <-
      try(subset(voc_tab, get("voc") == .voc, "url", drop = TRUE))
    if (!inherits(recall_arg, "try-error") && length(recall_arg) == 1) {
      return(Recall(recall_arg))
    }
    # Historical icd_meta_codes data loading removed here.
  }
  # auto request to install missing packages
  if (startsWith(fn, "dbx:")) {
    try({
      util_ensure_suggested("dbx", goal = "accessing databases")
      dbxurl <- sub("^dbx:", "", fn)
      connect_args <- list(url = dbxurl)
      opt_args <- getOption("dataquieR.dbx")
      if (!is.list(opt_args)) {
        opt_args <- getOption("dataquieR")
      }
      if (is.list(opt_args)) {
        connect_args <-
          connect_args[!(names(connect_args) %in% names(opt_args))]
        connect_args <- c(connect_args, opt_args)
      }
      con <- do.call(dbx::dbxConnect, connect_args)
      withr::with_db_connection(con, {
        if (is.null(which)) {
          util_error("Need a table name for database access %s.",
            dQuote(fn),
            applicability_problem = TRUE
          )
        }
        which <- gsub("[^a-zA-Z0-9_\\.\\[\\]]", "", which)
        if (is.null(col)) {
          sql_col <- "*"
        } else {
          col <- gsub("[^a-zA-Z0-9_\\.\\[\\]]", "", col)
          sql_col <- col
        }
        r <- dbx::dbxSelect(
          con,
          sprintf(
            "SELECT %s FROM %s",
            sql_col,
            which
          )
        )
      })
      if (!is.data.frame(r)) {
        util_error("Could not load %s",
          dQuote(fn),
          applicability_problem = TRUE
        )
      }
    })
  } else if (startsWith(fn, "extdata:")) {
    p <- sub("^extdata:", "", fn)
    p <- strsplit(p, "/", fixed = TRUE)[[1]]
    package <- p[[1]]
    util_ensure_suggested(package, goal = "load data from it")
    p[[1]] <- "extdata"
    fn <- do.call(util_system_file, c(as.list(p), list(package = package)))
  } else if (startsWith(fn, "package:")) {
    p <- sub("^package:", "", fn)
    p <- strsplit(p, "/", fixed = TRUE)[[1]]
    package <- p[[1]]
    util_ensure_suggested(package, goal = "load data from it")
    p <- p[-1]
    fn <- do.call(util_system_file, c(as.list(p), list(package = package)))
  } else if (startsWith(fn, "data:")) {
    try({
      p <- sub("^data:", "", fn)
      p <- strsplit(p, "/", fixed = TRUE)[[1]]
      package <- p[[1]]
      util_ensure_suggested(package, goal = "load data from it")
      p <- p[-1]
      if (length(p) > 0 || length(which) != 1) {
        util_error("Cannot load %s", dQuote(fn), applicability_problem = TRUE)
      }
      # also allow missing package, then use search() or installed.packages()
      .e <- new.env(parent = emptyenv())
      utils::data(
        list = which,
        package = package,
        envir = .e
      )
      r <- .e[[which]]
      if (!is.data.frame(r)) {
        util_error("Could not load %s",
          dQuote(fn),
          applicability_problem = TRUE
        )
      }
    })
  }

  # create a conditional named list based on argument column_names_only
  if (!is.data.frame(r)) {
    arguments_for_rio_import <- list(
      fn = fn,
      keep_types = keep_types
    )
    if (column_names_only) {
      to_add_list <- list(n_max = 0, nrows = 0)
      arguments_for_rio_import <- c(arguments_for_rio_import, to_add_list)
    } else {
      if (!is.null(which)) {
        to_add_list <- list(which = which)
        arguments_for_rio_import <- c(arguments_for_rio_import, to_add_list)
      }
    }
    r <- do.call(util_rio_import, arguments_for_rio_import)
  }

  if (inherits(r, "try-error") || !is.data.frame(r)) {
    fn0 <- util_system_file("extdata", paste0(fn, ".RDS"),
      package = "dataquieR"
    )
    if (file.exists(fn0)) {
      fn <- fn0
    } else {
      fn0 <- util_system_file("extdata", paste0(fn, ".RData"),
        package = "dataquieR"
      )
      if (file.exists(fn0)) {
        fn <- fn0
      } else {
        fn0 <- util_system_file("extdata", paste0(fn, ".xlsx"),
          package = "dataquieR"
        )
        if (file.exists(fn0)) {
          fn <- fn0
        }
      }
    }


    # create a conditional named list based on argument column_names_only
    arguments_for_rio_import <- list(
      fn = fn,
      keep_types = keep_types
    )
    if (column_names_only) {
      to_add_list <- list(n_max = 0, nrows = 0)
      arguments_for_rio_import <- c(arguments_for_rio_import, to_add_list)
    } else {
      if (!is.null(which)) {
        to_add_list <- list(which = which)
        arguments_for_rio_import <- c(arguments_for_rio_import, to_add_list)
      }
    }

    r <- do.call(util_rio_import, arguments_for_rio_import)


    if (inherits(r, "try-error")) {
      if (conditionMessage(util_attr(r, "condition", exact = TRUE)) ==
          "No such file" && length(prep_list_dataframes()) == 0) {
        hint <- sprintf(
          "\nDid you forget to call %s, %s or %s?",
          sQuote("prep_load_workbook_like_file"),
          sQuote("prep_add_data_frames"),
          sQuote("prep_load_folder_with_metadata")
        )
      } else {
        hint <- ""
      }
      util_error(
        "Cannot read file %s using %s: %s%s",
        dQuote(fn),
        sQuote("rio"),
        conditionMessage(util_attr(r, "condition", exact = TRUE)),
        hint
      )
    }

    if (!is.data.frame(r)) {
      util_error(
        "File %s did not contain a table (data frame) according to %s",
        dQuote(fn),
        sQuote("rio")
      )
    }
  }

  if (!is.null(col)) {
    if (grepl("+", col, fixed = TRUE)) {
      col <- strsplit(col, "+", fixed = TRUE)[[1]]
    }
    if (!(all(col %in% colnames(r)))) {
      util_error(
        "%s does not contain all of the columns %s on/in %s",
        dQuote(fn),
        dQuote(util_pretty_vector_string(col)),
        dQuote(which)
      )
    }
    r <- r[, col, drop = FALSE]
  }

  # delete invalid character codes
  r[] <- util_fix_encoding_cols(r)
  r <- util_set_data_frame_cache_attrs(
    x = r,
    data_frame_name = data_frame_name,
    source = fn,
    keep_types = keep_types
  )

  if (!column_names_only) {
    assign(data_frame_name, r, envir = .data_frame_list)
  }
  return(r)
}

.global.dataframe_environment <- new.env(parent = emptyenv())

#' Internal helper: set dataframe environment
#'
#' @noRd
util_set_dataframe_environment <- function(env = NULL) {
  if (is.null(env)) {
    assign(
      x = "dataframe_environment",
      value = .global.dataframe_environment,
      envir = dataquieR.properties
    )
  } else {
    util_stop_if_not(is.environment(env))
    assign(
      x = "dataframe_environment",
      value = env,
      envir = dataquieR.properties
    )
  }
}

#' Internal helper: dataframe environment
#'
#' @noRd
.dataframe_environment <- function() {
  return(get(
    x = "dataframe_environment",
    envir = dataquieR.properties,
    mode = "environment"
  ))
}

util_set_dataframe_environment(NULL)

#' Internal helper: with dataframe environment
#'
#' @noRd
with_dataframe_environment <- function(expr,
  env = new.env(parent = emptyenv())) {
  old_env <- .dataframe_environment()
  withr::defer({
    util_set_dataframe_environment(old_env)
  })
  util_set_dataframe_environment(env)
  force(rlang::eval_tidy(expr, env = parent.frame()))
}
