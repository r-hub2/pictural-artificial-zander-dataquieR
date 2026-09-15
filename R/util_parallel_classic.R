#' @noRd
util_parallel_classic <- function(all_calls,
  worker,
  n_nodes,
  progress,
  debug_parallel,
  my_storr_object) {
  outer_env <- parent.frame()
  # was length(parallel::getDefaultCluster()), but parallelMap doesn't use
  # defaultcluster any more!!
  tasks_per_node <- ceiling(length(all_calls) / n_nodes)
  indices <- seq_along(all_calls)
  length(indices) <- n_nodes * tasks_per_node # make equal length
  task_matrix <- matrix(indices,
    ncol = n_nodes, nrow = tasks_per_node,
    byrow = TRUE
  )

  function_names <- lapply(all_calls, rlang::call_name)

  if (util_parallel_get_options()$settings$mode %in%
      c("BatchJobs", "batchtools")) {
    task_matrix <- matrix(seq_along(all_calls), nrow = 1) # for batch jobs, don't split # nolint: line_length_linter.
  }

  r <- unlist(lapply(
    seq_len(nrow(task_matrix)),
    function(row) {
      slices <- task_matrix[row, , drop = FALSE]
      slices <- slices[!is.na(slices)]
      if (length(all_calls)) {
        progress(100 * row / nrow(task_matrix))
      }
      if (!dynGet(".is_testing", ifnotfound = FALSE)) {
        util_message(
          sprintf(
            "%s [%s] %d of %d, %s", Sys.time(), "INFO",
            row, nrow(task_matrix), "DQ"
          )
        )
      }
      if (debug_parallel) {
      }
      acs <- all_calls[slices]
      util_suppress_graphics(
        # don't use any auto graphics device (needed for certain
        # parallelization methods)
        util_parallel_map(
          impute.error = identity,
          setNames(nm = names(acs), seq_along(acs)),
          function_name = function_names[slices],
          fun = function(cl, function_name, nm, acs, ...) {
            worker(
              expression = acs[[cl]],
              nm = nm,
              function_name = function_name,
              my_call = acs[[cl]],
              ...
            )
          },
          more.args = list(
            env = outer_env,
            my_storr_object = my_storr_object,
            acs = acs
          ),
          simplify = FALSE,
          use.names = TRUE,
          nm = names(acs)
        )
      )
    }
  ), recursive = FALSE)

  r
}
