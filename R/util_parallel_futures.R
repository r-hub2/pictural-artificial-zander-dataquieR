#' @noRd
util_parallel_futures <- function(all_calls,
  worker,
  n_nodes,
  progress,
  debug_parallel,
  my_storr_object) {
  util_ensure_suggested("future")
  outer_env <- parent.frame()
  if (!is.null(parallel::getDefaultCluster())) {
    oplan <- future::plan(list(
      future::tweak(future::cluster,
        persistent = TRUE,
        workers = parallel::getDefaultCluster()
      ),
      future::multisession
    ))
    withr::defer(future::plan(oplan))
  }
  # don't use any auto graphics device (needed for certain
  # parallelization methods)
  rp <- lapply(
    setNames(seq_along(all_calls), nm = names(all_calls)),
    function(i) {
      progress(100 * i / length(all_calls))

      future::future(
        seed = TRUE,
        {
          worker(all_calls[[i]],
            env = outer_env, nm =
              names(all_calls)[[i]],
            function_name = rlang::call_name(all_calls[[i]]),
            my_storr_object = my_storr_object
          )
        }
      )

      # Historical labelled, graphics-suppressed future call removed here.
      # Inspect commit 214dd76a7d before restoring that experiment.
    }
  )
  r <- future::value(rp)
  # Historical manual future polling removed here. Inspect commit fb82b88d45
  # before restoring the older progress loop.
  r
}
