#' Check for package updates
#'
#' @param beta [logical] check for beta version too
#' @param deps [logical] check for missing (optional) dependencies
#' @param ask [logical] ask for updates
#' @param byte_compile [logical] install `dataquieR` from source with
#'   byte-compiled R code
#'
#' @return `invisible(NULL)`
#' @export
prep_check_for_dataquieR_updates <- function(beta = FALSE,
  deps = TRUE,
  ask = interactive(),
  byte_compile = TRUE) { # nocov start
  util_expect_scalar(beta, check_type = is.logical)
  util_expect_scalar(deps, check_type = is.logical)
  util_expect_scalar(ask, check_type = is.logical)
  util_expect_scalar(byte_compile, check_type = is.logical)
  pn <- packageName()
  util_message(
    "Looking for updates of %s. Currently installed: %s",
    sQuote(pn),
    sQuote(packageVersion(pn))
  )
  if (beta) {
    r <- list(
      QIHS = "https://packages.qihs.uni-greifswald.de//repository/ship-snapshot-r/" # nolint: line_length_linter.
    )
  } else {
    r <- list()
  }
  rlang::local_options(repos = c(
    r,
    getOption("repos")
  ))
  loaded_packages <- util_loaded_package_install_markers()
  install_opts <- if (byte_compile) {
    "--byte-compile"
  } else {
    "--no-byte-compile"
  }
  installed_byte_compile <- util_installed_package_is_byte_compiled(pn)
  restart_expected <- util_loaded_packages_with_available_updates(pn,
    type = "source")
  if (pn %in% names(loaded_packages) &&
      !is.na(installed_byte_compile) &&
      !identical(installed_byte_compile, byte_compile)) {
    restart_expected <- union(restart_expected, pn)
  }
  if (!util_confirm_loaded_package_updates(restart_expected, ask)) {
    return(invisible(NULL))
  }
  update_args <- list(oldPkgs = pn, ask = ask, dependencies = deps,
    type = "source", INSTALL_opts = install_opts)
  do.call(util_update_packages, update_args)
  installed_byte_compile <- util_installed_package_is_byte_compiled(pn)
  if (!is.na(installed_byte_compile) &&
      !identical(installed_byte_compile, byte_compile)) {
    util_message(
      paste(
        "Installed %s does not match byte_compile = %s.",
        "Reinstalling from source %s."
      ),
      sQuote(pn),
      byte_compile,
      if (byte_compile) {
        "with byte compilation"
      } else {
        "without byte compilation"
      }
    )
    install_args <- list(pkgs = pn, dependencies = deps, type = "source",
      INSTALL_opts = install_opts)
    do.call(util_install_packages, install_args)
  }
  if (deps) {
    all_deps <- unique(trimws(unlist(strsplit(
      split = ",", fixed = TRUE, x =
        unname(unlist(util_package_description("dataquieR",
              fields = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances") # nolint: line_length_linter.
            )))
    ))))
    all_deps <- gsub("\\s+", " ", all_deps)
    all_deps <- all_deps[!util_empty(all_deps)]
    all_deps <- all_deps[!startsWith(all_deps, "R ")]
    if (ask) {
      rlang::check_installed(
        all_deps,
        sprintf(
          "as (soft) dependencies of %s",
          dQuote(packageName())
        )
      )
    } else {
      to_install <- !vapply(all_deps, rlang::is_installed,
        FUN.VALUE = logical(1)
      )
      if (rlang::is_installed("pak")) {
        cat("Installing using pak: ",
          dQuote(names(to_install[to_install])), "\n")
        pkg_install <- rlang::env_get(rlang::ns_env("pak"), "pkg_install")
        pkg_install(names(to_install[to_install]), ask = FALSE)
      } else {
        cat("Installing ", dQuote(names(to_install[to_install])), "\n")
        util_install_packages(names(to_install[to_install]))
      }
    }
  }
  changed_loaded_packages <-
    util_loaded_packages_with_changed_install(loaded_packages)
  if (length(changed_loaded_packages)) {
    util_restart_or_warn_about_loaded_updates(changed_loaded_packages)
  }
  invisible(NULL)
} # nocov end

#' Internal helper: update packages
#'
#' @noRd
util_update_packages <- function(...) {
  utils::update.packages(...)
}

#' Internal helper: install packages
#'
#' @noRd
util_install_packages <- function(...) {
  utils::install.packages(...)
}

#' Internal helper: package description
#'
#' @noRd
util_package_description <- function(...) {
  utils::packageDescription(...)
}

#' Internal helper: old packages
#'
#' @noRd
util_old_packages <- function(...) {
  utils::old.packages(...)
}

#' Internal helper: ask yes no
#'
#' @noRd
util_ask_yes_no <- function(...) {
  utils::askYesNo(...)
}

#' Internal helper: loaded package install markers
#'
#' @noRd
util_loaded_package_install_markers <- function(
  packages = loadedNamespaces()) {
  packages <- sort(unique(packages))
  markers <- lapply(packages, util_installed_package_marker)
  names(markers) <- packages
  markers[!vapply(markers, is.null, logical(1))]
}

#' Internal helper: installed package marker
#'
#' @noRd
util_installed_package_marker <- function(package) {
  package_path <- util_installed_package_path(package)
  if (is.null(package_path)) {
    return(NULL)
  }
  description <- file.path(package_path, "DESCRIPTION")
  package_db <- file.path(package_path, "R", package)
  desc <- tryCatch(read.dcf(description), error = function(e) NULL)
  version <- if (!is.null(desc) && "Version" %in% colnames(desc)) {
    unname(desc[1, "Version", drop = TRUE])
  } else {
    NA_character_
  }
  c(
    version = version,
    description_mtime = util_file_mtime(description),
    rdb_mtime = util_file_mtime(paste0(package_db, ".rdb")),
    rdx_mtime = util_file_mtime(paste0(package_db, ".rdx"))
  )
}

#' Internal helper: file mtime
#'
#' @noRd
util_file_mtime <- function(path) {
  if (!file.exists(path)) {
    return(NA_real_)
  }
  as.numeric(file.info(path)$mtime)
}

#' Internal helper: installed package path
#'
#' @noRd
util_installed_package_path <- function(package) {
  package_path <- file.path(.libPaths(), package)
  package_path <- package_path[file.exists(file.path(package_path,
        "DESCRIPTION"))]
  if (!length(package_path)) {
    return(NULL)
  }
  package_path[[1]]
}

#' Internal helper: loaded packages with available updates
#'
#' @noRd
util_loaded_packages_with_available_updates <- function(packages,
  type = getOption("pkgType")) {
  packages <- intersect(packages, loadedNamespaces())
  if (!length(packages)) {
    return(character())
  }
  old_packages <- tryCatch(util_old_packages(type = type),
    error = function(e) NULL)
  if (is.null(old_packages) || !NROW(old_packages)) {
    return(character())
  }
  intersect(packages, rownames(old_packages))
}

#' Internal helper: confirm loaded package updates
#'
#' @noRd
util_confirm_loaded_package_updates <- function(packages, ask) {
  packages <- sort(unique(packages))
  if (!length(packages)) {
    return(TRUE)
  }
  msg <- sprintf(
    paste(
      "The following currently loaded package(s) may be updated or",
      "reinstalled:",
      "%s.",
      "The current R session should be restarted afterwards. Continue?"
    ),
    paste(sQuote(packages), collapse = ", ")
  )
  if (ask && interactive()) {
    return(isTRUE(util_ask_yes_no(msg, default = FALSE)))
  }
  util_warning("%s", sub("\\?\\z", ".", msg), immediate = TRUE)
  TRUE
}

#' Internal helper: loaded packages with changed install
#'
#' @noRd
util_loaded_packages_with_changed_install <- function(before) {
  if (!length(before)) {
    return(character())
  }
  changed <- vapply(names(before), function(package) {
    !identical(before[[package]], util_installed_package_marker(package))
  }, FUN.VALUE = logical(1), USE.NAMES = FALSE)
  names(before)[changed]
}

#' Internal helper: restart or warn about loaded updates
#'
#' @noRd
util_restart_or_warn_about_loaded_updates <- function(packages) {
  packages <- sort(unique(packages))
  msg <- sprintf(
    paste(
      "The following loaded package(s) were updated or reinstalled:",
      "%s.",
      "Restart the R session before continuing to avoid stale code."
    ),
    paste(sQuote(packages), collapse = ", ")
  )
  if (util_restart_rstudio_session()) {
    return(invisible(TRUE))
  }
  util_warning("%s", msg, immediate = TRUE)
  invisible(FALSE)
}

#' Internal helper: restart rstudio session
#'
#' @noRd
util_restart_rstudio_session <- function() {
  if (util_really_rstudio() &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      exists("restartSession", asNamespace("rstudioapi"),
        mode = "function") &&
      rstudioapi::isAvailable()) {
    rstudioapi::restartSession()
    return(TRUE)
  }
  FALSE
}

#' Internal helper: installed package is byte compiled
#'
#' @noRd
util_installed_package_is_byte_compiled <- function(package) {
  package_path <- util_installed_package_path(package)
  if (is.null(package_path)) {
    return(NA)
  }
  package_db <- file.path(package_path, "R", package)
  if (!file.exists(paste0(package_db, ".rdb"))) {
    return(NA)
  }
  map <- tryCatch(readRDS(paste0(package_db, ".rdx")),
    error = function(e) NULL)
  if (is.null(map)) {
    return(NA)
  }
  load_object <- get("lazyLoadDBfetch", envir = baseenv())
  names <- names(map$variables)
  names <- names[!startsWith(names, ".")]
  functions <- vapply(names, function(name) {
    object <- tryCatch(load_object(map$variables[[name]],
        paste0(package_db, ".rdb"), map$compressed, identity),
      error = function(e) NULL)
    if (!is.function(object) || !identical(typeof(object), "closure")) {
      return(NA)
    }
    util_function_is_byte_compiled(object)
  }, FUN.VALUE = logical(1), USE.NAMES = FALSE)
  functions <- functions[!is.na(functions)]
  if (!length(functions)) {
    return(NA)
  }
  mean(functions) > 0.5
}

#' Internal helper: function is byte compiled
#'
#' @noRd
util_function_is_byte_compiled <- function(fun) {
  tryCatch({
    capture.output(compiler::disassemble(fun))
    TRUE
  }, error = function(e) FALSE)
}
