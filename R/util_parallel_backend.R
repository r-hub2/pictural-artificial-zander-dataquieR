# nolint start: line_length_linter.
#' Lightweight parallel backend (replacement for `parallelMap`)
#'
#' This module replaces the (old) `parallelMap` package with a minimal
#' wrapper around base R's `parallel` package. It exposes the small set of
#' verbs that `dataquieR` historically used from `parallelMap`:
#' start/stop, map, export, library, get_options, get_cpus.
#'
#' The implementation purposefully mirrors `parallelMap`'s semantics so that
#' existing call sites can be migrated mechanically:
#'   * a single, package-global cluster is kept in
#'     `parallel::setDefaultCluster()` (and a copy in `.dq2_par$cluster`)
#'     -- so existing code that uses `parallel::clusterEvalQ(cl = NULL, ...)`
#'     keeps working,
#'   * "modes" are tracked: `"local"` (no cluster), `"socket"` (`PSOCK`),
#'     `"multicore"` (FORK on non-Windows), `"BatchJobs"`/`"batchtools"`
#'     (stubs: treated as `"local"` for execution, but the mode label is kept
#'     for code paths that still test for it),
#'   * `impute.error = identity` is implemented via `tryCatch(error = identity)`,
#'   * options `parallelMap.*` (legacy) and `dataquieR.parallel.*` (new) are
#'     read on start as a backwards-compatibility layer.
#'
#' All exported names use the `util_parallel_*` prefix and are package-internal.
#'
#' @name util_parallel_backend
#' @keywords internal
#' @noRd
# nolint end
NULL


# --- internal state -----------------------------------------------------------

#' Package-internal state of the parallel backend
#'
#' @keywords internal
#' @noRd
.dq2_par <- new.env(parent = emptyenv())
# Status values are started and stopped.
.dq2_par$status <- "stopped"
# Mode values include local, socket, multicore, BatchJobs, and batchtools.
.dq2_par$mode <- "local"
.dq2_par$cpus <- 1L
.dq2_par$load_balancing <- TRUE
.dq2_par$show_info <- FALSE
.dq2_par$logging <- FALSE
.dq2_par$cluster <- NULL # the PSOCK cluster we created (if any)
.dq2_par$owns_cluster <- FALSE # TRUE if we started it (then we stop it)
.dq2_par$prev_default_cluster <- NULL # to restore on stop


# --- option backwards-compat --------------------------------------------------

#' Read a dataquieR or legacy `parallelMap` option
#'
#' Looks up `dataquieR.parallel.<name>` first, falls back to the
#' historical `parallelMap.<name>` option, then to `default`.
#'
#' @param name option name (without prefix)
#' @param default default value, returned if neither option is set
#'
#' @keywords internal
#' @noRd
util_parallel_opt <- function(name, default = NULL) {
  v <- getOption(paste0("dataquieR.parallel.", name), default = NULL)
  if (is.null(v)) {
    v <- getOption(paste0("parallelMap.", name), default = default)
  }
  v
}


# --- getters ------------------------------------------------------------------

#' Return current backend settings
#'
#' Shape-compatible with `parallelMap::parallelGetOptions()$settings` for the
#' fields actually queried in dataquieR (`mode`, `cpus`, `load.balancing`).
#'
#' While the backend is not running (`status == "stopped"`), the `mode`
#' field falls back to the user-visible option lookup
#' (`dataquieR.parallel.mode`, then legacy `parallelMap.mode`). This matches
#' the historical behaviour where `getOption("parallelMap.mode")` was the
#' single source of truth -- in particular, callers like
#' `util_generate_pages_from_report()` need to see a user-configured mode
#' (e.g. "snow") even when the backend has not been started.
#'
#' @return a list with elements `settings = list(mode, cpus, load.balancing,
#'   show.info, logging)`.
#'
#' @keywords internal
#' @noRd
util_parallel_get_options <- function() {
  mode <- .dq2_par$mode
  if (identical(.dq2_par$status, "stopped")) {
    opt_mode <- util_parallel_opt("mode", default = NULL)
    if (!is.null(opt_mode) && is.character(opt_mode) &&
        length(opt_mode) == 1L && nzchar(opt_mode)) {
      mode <- opt_mode
    }
  }
  list(settings = list(
    mode           = mode,
    cpus           = .dq2_par$cpus,
    load.balancing = .dq2_par$load_balancing,
    show.info      = .dq2_par$show_info,
    logging        = .dq2_par$logging
  ))
}

#' Current number of workers
#' @keywords internal
#' @noRd
util_parallel_get_cpus <- function() {
  cl <- parallel::getDefaultCluster()
  if (!is.null(cl)) {
    return(length(cl))
  }
  .dq2_par$cpus
}

#' Backend status -- "started" or "stopped"
#' @keywords internal
#' @noRd
util_parallel_status <- function() {
  .dq2_par$status
}


# --- start / stop -------------------------------------------------------------

#' Start the parallel backend
#'
#' Replacement for `parallelMap::parallelStart()`.
#'
#' If a default cluster is already registered (e.g. set up by the caller via
#' `parallel::setDefaultCluster()`), it is reused and no new cluster is
#' spawned; in this case the backend will NOT stop that cluster on shutdown.
#'
#' @param mode one of `"local"`, `"socket"`, `"multicore"`,
#'   `"BatchJobs"`, `"batchtools"`. `BatchJobs`/`batchtools` are stubs (kept
#'   for backwards compatibility) and are downgraded to sequential execution,
#'   but the mode label is recorded so legacy code paths that test for it
#'   keep working. Defaults to the option `dataquieR.parallel.mode`, with
#'   a fallback to the legacy `parallelMap.mode` option, and finally to
#'   `"local"`.
#' @param cpus integer, number of worker processes for `mode = "socket"` /
#'   `"multicore"`. Defaults to the option `dataquieR.parallel.cpus`, then
#'   to `util_detect_cores()`.
#' @param logging logical (currently ignored, kept for signature compat).
#' @param load.balancing logical, whether to use load-balanced dispatch.
#'   Defaults to the option `dataquieR.parallel.load.balancing`.
#' @param show.info logical, whether to print info messages. Defaults to
#'   the option `dataquieR.parallel.show.info`.
#' @param ... ignored, swallowed for `do.call()` call-site compatibility.
#'
#' @keywords internal
#' @noRd
util_parallel_start <- function(mode = util_parallel_opt("mode", "local"),
  cpus = NULL,
  logging = FALSE,
  load.balancing = TRUE,
  show.info = FALSE,
  ...) {
  # Resolve mode -------------------------------------------------------------
  if (is.null(mode) || !is.character(mode) || length(mode) != 1) {
    mode <- "local"
  }
  if (!mode %in% c("local", "socket", "multicore", "BatchJobs", "batchtools")) {
    util_warning(c("Unknown parallel mode %s, falling back to %s."),
      dQuote(mode), dQuote("local"),
      applicability_problem = TRUE
    )
    mode <- "local"
  }

  # Resolve cpus -------------------------------------------------------------
  if (is.null(cpus)) {
    cpus <- suppressWarnings(util_parallel_opt("cpus", default = NULL))
  }
  if (is.null(cpus) || !is.finite(suppressWarnings(as.numeric(cpus))[1])) {
    cpus <- util_detect_cores()
  }
  cpus <- max(1L, as.integer(cpus))

  # Resolve show.info / load.balancing from options if not explicit ----------
  show.info <- if (missing(show.info)) {
    isTRUE(util_parallel_opt("show.info", FALSE))
  } else {
    isTRUE(show.info)
  }
  load.balancing <- if (missing(load.balancing)) {
    isTRUE(util_parallel_opt("load.balancing", TRUE))
  } else {
    isTRUE(load.balancing)
  }

  # If we still own a previous cluster, shut it down before starting a new
  # backend session. (If we don't own one, we leave it alone -- a caller may
  # have registered it via parallel::setDefaultCluster() and expects to keep
  # managing its lifecycle.)
  if (.dq2_par$status == "started" && isTRUE(.dq2_par$owns_cluster)) {
    util_parallel_stop()
  }

  .dq2_par$mode <- mode
  .dq2_par$cpus <- cpus
  .dq2_par$load_balancing <- load.balancing
  .dq2_par$show_info <- show.info
  .dq2_par$logging <- isTRUE(logging)

  # If the caller asks for a cluster mode AND a default cluster is already
  # registered, reuse it (do not spawn another one, do not stop it on exit).
  # For mode = "local" / "BatchJobs" / "batchtools" we never reuse: those modes
  # are explicit requests for sequential / non-cluster execution.
  existing <- parallel::getDefaultCluster()
  if (!is.null(existing) && mode %in% c("socket", "multicore")) {
    .dq2_par$cluster <- existing
    .dq2_par$owns_cluster <- FALSE
    .dq2_par$cpus <- length(existing)
    .dq2_par$status <- "started"
    # also mirror to legacy options so older code paths still see something
    options(
      parallelMap.status = "started",
      parallelMap.mode = .dq2_par$mode,
      parallelMap.cpus = .dq2_par$cpus,
      parallelMap.load.balancing = .dq2_par$load_balancing
    )
    return(invisible(NULL))
  }

  # Spawn one ----------------------------------------------------------------
  cl <- NULL
  if (mode == "socket") {
    cl <- parallel::makePSOCKcluster(cpus)
  } else if (mode == "multicore") {
    if (.Platform$OS.type != "windows") {
      cl <- parallel::makeForkCluster(cpus)
    } else {
      # FORK is not supported on Windows; fall back to PSOCK transparently
      cl <- parallel::makePSOCKcluster(cpus)
    }
  } else {
    # "local" / "BatchJobs" / "batchtools": no cluster, sequential execution
    cl <- NULL
  }

  if (!is.null(cl)) {
    .dq2_par$prev_default_cluster <- parallel::getDefaultCluster()
    parallel::setDefaultCluster(cl)
    .dq2_par$cluster <- cl
    .dq2_par$owns_cluster <- TRUE
  } else {
    .dq2_par$cluster <- NULL
    .dq2_par$owns_cluster <- FALSE
  }

  .dq2_par$status <- "started"

  # mirror to legacy options so any remaining code that still tests them
  # keeps working during the transition.
  options(
    parallelMap.status         = "started",
    parallelMap.mode           = .dq2_par$mode,
    parallelMap.cpus           = .dq2_par$cpus,
    parallelMap.load.balancing = .dq2_par$load_balancing,
    parallelMap.show.info      = .dq2_par$show_info
  )

  invisible(NULL)
}

#' Stop the parallel backend
#'
#' Replacement for `parallelMap::parallelStop()`.
#'
#' @keywords internal
#' @noRd
util_parallel_stop <- function() {
  if (.dq2_par$status == "stopped") {
    return(invisible(NULL))
  }
  if (isTRUE(.dq2_par$owns_cluster) && !is.null(.dq2_par$cluster)) {
    try(parallel::stopCluster(.dq2_par$cluster), silent = TRUE)
    try(parallel::setDefaultCluster(.dq2_par$prev_default_cluster),
      silent = TRUE
    )
  }
  .dq2_par$cluster <- NULL
  .dq2_par$owns_cluster <- FALSE
  .dq2_par$prev_default_cluster <- NULL
  .dq2_par$status <- "stopped"
  .dq2_par$mode <- "local"
  .dq2_par$cpus <- 1L

  options(
    parallelMap.status = "stopped"
  )

  invisible(NULL)
}


# --- export / library ---------------------------------------------------------

#' Export variables to all workers
#'
#' Replacement for `parallelMap::parallelExport()`. Variables are looked up
#' in the caller's frame (`parent.frame()`), matching `parallelMap`'s
#' behaviour.
#'
#' @param ... unnamed variable names as character (or a single character vector
#'   passed through `objnames =`).
#' @param objnames optional, character vector of names to export.
#' @param show.info logical, ignored (kept for signature compat).
#' @param envir environment to read the values from. Defaults to the caller.
#'
#' @keywords internal
#' @noRd
util_parallel_export <- function(..., objnames = NULL, show.info = FALSE,
  envir = parent.frame()) {
  vars <- c(objnames, unlist(list(...)))
  vars <- vars[nzchar(vars)]
  if (!length(vars)) {
    return(invisible(NULL))
  }

  cl <- parallel::getDefaultCluster()
  if (is.null(cl)) {
    # sequential / "local": no-op, the values are already visible.
    return(invisible(NULL))
  }
  # parallel::clusterExport reads names from `envir`.
  parallel::clusterExport(cl = cl, varlist = vars, envir = envir)
  invisible(NULL)
}


#' Load packages on all workers
#'
#' Replacement for `parallelMap::parallelLibrary()`. Like `parallelMap`,
#' this attaches the requested packages to the worker's search path (not
#' just their namespaces), so that downstream code like
#' `parallel::clusterEvalQ(cl = NULL, some_pkg_fun())` can resolve
#' unqualified function names exactly as in an interactive session.
#'
#' To stay out of `R CMD check`'s "library()/require() in package code"
#' NOTE, the attach step uses `loadNamespace()` + `attachNamespace()`
#' (which is what `library()` itself does internally for non-base
#' packages) instead of a direct `library()` call.
#'
#' @param ... package names (character).
#' @param show.info logical, ignored.
#'
#' @keywords internal
#' @noRd
util_parallel_library <- function(..., show.info = FALSE) {
  pkgs <- unlist(list(...))
  pkgs <- pkgs[nzchar(pkgs)]
  if (!length(pkgs)) {
    return(invisible(NULL))
  }

  cl <- parallel::getDefaultCluster()

  # Self-contained worker helper (serialisable -- no closure captures).
  attach_pkg <- function(pkg) {
    ns <- loadNamespace(pkg)
    if (!paste0("package:", pkg) %in% search()) {
      suppressMessages(suppressPackageStartupMessages(
        attachNamespace(ns)
      ))
    }
    invisible(NULL)
  }

  if (is.null(cl)) {
    # sequential / "local": attach locally
    for (p in pkgs) attach_pkg(p)
    return(invisible(NULL))
  }
  for (p in pkgs) {
    parallel::clusterCall(cl, attach_pkg, p)
  }
  invisible(NULL)
}


# --- the map verb -------------------------------------------------------------

#' Apply a function in parallel over one or more vectors
#'
#' Replacement for `parallelMap::parallelMap()`. Semantics:
#'   * vectorised over `...` (all `...` must have the same length, like
#'     `mapply()` / `parallelMap::parallelMap()`),
#'   * `more.args` are constant arguments,
#'   * `impute.error` -- if a function, errors are caught with
#'     `tryCatch(error = impute.error)` and the result of `impute.error(e)`
#'     takes the place of the failing call's result (matching
#'     `parallelMap`'s `impute.error = identity`),
#'   * `simplify = FALSE` returns a list; `simplify = TRUE` tries to coerce
#'     via `simplify2array()`,
#'   * `use.names = TRUE` takes the names of the first iterable as the names
#'     of the result, matching `parallelMap`.
#'
#' Note: there is intentionally NO `nm` keyword argument here. `parallelMap`
#' does not have one either, and call sites in `dataquieR` rely on being
#' able to pass `nm = ...` as a named iterable that is forwarded to `fun`
#' as a per-call argument (see e.g. `util_parallel_classic.R`, where `nm`
#' ends up as an argument of `util_eval_to_dataquieR_result()`).
#'
#' @param ... vectors/lists to iterate over jointly.
#' @param fun the function to call. The first arguments are taken positionally
#'   from `...`, followed by `more.args`.
#' @param more.args named list of constant arguments to `fun`.
#' @param simplify logical (default `FALSE`).
#' @param use.names logical (default `TRUE`).
#' @param impute.error `NULL` (no catch) or a function used in
#'   `tryCatch(error = impute.error)`.
#' @param show.info logical, ignored.
#'
#' @keywords internal
#' @noRd
util_parallel_map <- function(...,
  fun,
  more.args = list(),
  simplify = FALSE,
  use.names = TRUE,
  impute.error = NULL,
  show.info = FALSE) {
  iter <- list(...)
  force(fun)
  force(more.args)
  force(impute.error)
  if (!length(iter)) {
    return(if (simplify) logical(0) else list())
  }

  # name resolution (parallelMap honours names of the first iterable)
  result_names <- NULL
  if (isTRUE(use.names) && !is.null(names(iter[[1]]))) {
    result_names <- names(iter[[1]])
  }

  n <- length(iter[[1]])
  if (n == 0L) {
    out <- list()
    if (!is.null(result_names)) names(out) <- result_names
    return(if (simplify) simplify2array(out) else out)
  }

  # validation: all iterables same length
  lens <- vapply(iter, length, integer(1))
  if (!all(lens == n)) {
    util_error(
      c(
        "All iterables passed to %s must have the same length,",
        "got lengths: %s"
      ),
      sQuote("util_parallel_map"),
      paste(lens, collapse = ", ")
    )
  }

  # wrap the user function so it can be called positionally on the i-th element
  worker <- function(i) {
    pos <- lapply(iter, function(v) v[[i]])
    call_args <- c(pos, more.args)
    if (is.function(impute.error)) {
      tryCatch(do.call(fun, call_args), error = function(e) impute.error(e))
    } else {
      do.call(fun, call_args)
    }
  }

  cl <- parallel::getDefaultCluster()
  use_cluster <- !is.null(cl) &&
    .dq2_par$mode %in% c("socket", "multicore")

  if (use_cluster) {
    # parallel execution
    worker_map <- function(..., .fun, .more_args, .impute_error) {
      call_args <- c(list(...), .more_args)
      if (is.function(.impute_error)) {
        tryCatch(do.call(.fun, call_args),
          error = function(e) .impute_error(e)
        )
      } else {
        do.call(.fun, call_args)
      }
    }
    if (isTRUE(.dq2_par$load_balancing)) {
      out <- parallel::clusterMap(cl = cl,
        fun = worker_map,
        MoreArgs = list(
          .fun = fun,
          .more_args = more.args,
          .impute_error = impute.error
        ),
        RECYCLE = FALSE,
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE,
        .scheduling = "dynamic",
        ...
      )
    } else {
      out <- parallel::clusterMap(cl = cl,
        fun = worker_map,
        MoreArgs = list(
          .fun = fun,
          .more_args = more.args,
          .impute_error = impute.error
        ),
        RECYCLE = FALSE,
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE,
        .scheduling = "static",
        ...
      )
    }
  } else {
    # sequential ("local" / "BatchJobs" / "batchtools" stubs)
    out <- lapply(seq_len(n), worker)
  }

  if (!is.null(result_names)) names(out) <- result_names

  if (isTRUE(simplify)) {
    return(simplify2array(out))
  }
  out
}


# --- convenience -- evaluate an expression on all workers --------------------

#' Evaluate an expression on the cluster, or locally if none is registered
#'
#' Used by callers that previously open-coded
#' `if (is.null(parallel::getDefaultCluster())) eval(expr) else
#'  parallel::clusterEvalQ(cl = NULL, expr)`.
#'
#' @param expr an expression
#'
#' @keywords internal
#' @noRd
util_parallel_eval <- function(expr) {
  cl <- parallel::getDefaultCluster()
  if (is.null(cl)) {
    eval(expr, envir = parent.frame())
  } else {
    parallel::clusterCall(cl, function(e) eval(e), expr)
  }
}
