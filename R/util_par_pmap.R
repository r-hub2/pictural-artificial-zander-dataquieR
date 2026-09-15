#' Utility function parallel version of `purrr::pmap`
#'
#' Parallel version of `purrr::pmap`.
#'
#'
#' @param .l [data.frame] with one call per line and one function argument
#'                        per column
#' @param .f [`function`] to call with the arguments from `.l`
#' @param ... additional, static arguments for calling `.f`
#'
#' @param cores number of cpu cores to use or a (named) list with arguments for
#'              the internal parallel backend (`util_parallel_start`) or NULL,
#'              if parallel has already been started by the caller. In RStudio
#'              report rendering, caller-owned clusters can make HTML
#'              finalization hang; prefer letting `dataquieR` create the
#'              cluster from a number or backend list.
#' @param use_cache [logical] set to FALSE to omit re-using already distributed
#'                            study- and metadata on a parallel cluster
#'
#' @seealso `purrr::pmap`
#' @seealso [Stack Overflow post](https://stackoverflow.com/a/47575143)
#'
#' @author [Aurèle](https://stackoverflow.com/users/6197649)
#' @author S Struckmann
#'
#' @return [list] of results of the function calls
#'
#' @family process_functions
#' @concept reporting
#' @noRd
util_par_pmap <- function(.l, .f, ...,
  cores = list(
    mode = "socket",
    cpus = util_detect_cores(),
    logging = FALSE,
    load.balancing = TRUE
  ),
  use_cache = FALSE) {
  if (!is.null(cores)) {
    if (inherits(cores, "list")) {
      suppressMessages(do.call(util_parallel_start, cores))
    } else {
      suppressMessages(util_parallel_start("socket",
          cpus = cores,
          logging = FALSE,
          load.balancing = TRUE
        ))
    }
    withr::defer(
      {
        Sys.sleep(2)
        suppressMessages(util_parallel_stop())
      }
    ) # whyever, rstudio needs these two seconds, it hangs, otherwise.
  }
  more_args <- list(...)
  if ("meta_data" %in% names(more_args)) {
    meta_data <- more_args[["meta_data"]]
    if (use_cache &&
        !all(unlist(util_parallel_map(fun = exists, "meta_data")))) {
      suppressWarnings(util_parallel_export("meta_data"))
      more_args[["meta_data"]] <- NULL
    }
  }
  if ("study_data" %in% names(more_args)) {
    study_data <- more_args[["study_data"]]
    if (use_cache &&
        !all(unlist(util_parallel_map(fun = exists, "study_data")))) {
      suppressWarnings(util_parallel_export("study_data"))
      more_args[["study_data"]] <- NULL
    }
  }
  do.call(
    util_parallel_map,
    c(.l, list(
      fun = .f, more.args = more_args, simplify = FALSE, use.names = FALSE,
      show.info = FALSE, impute.error = identity
    ))
  )
  #  do.call(
  #    parallel::mcmapply,
  #    c(.l, list(FUN = .f, MoreArgs = list(...), SIMPLIFY = FALSE,
  #         mc.cores = mc.cores))
  #  )
}
