#' @keywords internal
util_parallel_futures <- function(all_calls,
                                  worker,
                                  n_nodes,
                                  progress,
                                  debug_parallel,
                                  my_storr_object) { # nocov start
  util_ensure_suggested("future")
  if (!is.null(parallel::getDefaultCluster())) {
    oplan <- future::plan(list(future::tweak(future::cluster,
                                             persistent = TRUE,
                             workers = parallel::getDefaultCluster()),
                       future::multisession))
    on.exit(future::plan(oplan), add = TRUE)
  }
    # don't use any auto graphics device (needed for certain
    # parallelization methods)
  rp <- lapply(setNames(seq_along(all_calls), nm = names(all_calls)),
           function(i) {
             progress(100 * i/length(all_calls))

             # covr instrumentation breaks the results, here.
             # nocov start
             future::future(seed = TRUE, # IDEA: Use future_promise: https://rstudio.github.io/promises/articles/future_promise.html
               {
                 worker(all_calls[[i]], env = environment(), nm =
                          names(all_calls)[[i]],
                        function_name = rlang::call_name(all_calls[[i]]),
                        my_storr_object = my_storr_object)
               }
              )
             # nocov end

             #   label = names(all_calls)[[i]],
             #   expr = {
             #       R.devices::suppressGraphics(
             #           worker(all_calls[[i]], env = environment()))
             #      }
             # )
           }
  )
  r <- lapply(rp, future::value)
  # repeat {
  #   rsvld <- vapply(rp, resolved, FUN.VALUE = logical(1))
  #   if (all(rsvld)) break;
  #   progress(100 * sum(rsvld) / length(f))
  # }
  # later::run_now()
  # repeat {
  #   rsvld <- vapply(rp, resolved, FUN.VALUE = logical(1))
  #   if (all(rsvld)) break;
  #   progress(100 * sum(rsvld) / length(f))
  # }
  # as.list(r)
  r
} # nocov end
