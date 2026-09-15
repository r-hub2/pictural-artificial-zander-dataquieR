#' Internal helper: worker cache strip study data attrs
#'
#' @noRd
util_worker_cache_strip_study_data_attrs <- function(study_data_cache) {
  study_data_attrs <- new.env(parent = emptyenv())
  stripped_cache <- lapply(study_data_cache, function(ds1) {
    raw_study_data <- util_attr(ds1, "study_data", exact = TRUE)
    if (is.data.frame(raw_study_data)) {
      raw_key <- paste0("study_data@", rlang::hash(raw_study_data))
      assign(raw_key, raw_study_data, envir = study_data_attrs)
      attr(ds1, "study_data") <- NULL
      attr(ds1, "dataquieR_study_data_cache_raw_key") <- raw_key
    }
    ds1
  })
  list(
    study_data_cache = stripped_cache,
    study_data_cache_study_data_attrs = as.list(study_data_attrs)
  )
}

#' Internal helper: worker cache restore study data attrs
#'
#' @noRd
util_worker_cache_restore_study_data_attrs <- function(
  study_data_cache,
  study_data_attrs
) {
  lapply(study_data_cache, function(ds1) {
    raw_key <- util_attr(ds1, "dataquieR_study_data_cache_raw_key",
      exact = TRUE
    )
    if (is.character(raw_key) &&
        length(raw_key) == 1 &&
        raw_key %in% names(study_data_attrs)) {
      attr(ds1, "study_data") <- study_data_attrs[[raw_key]]
      attr(ds1, "dataquieR_study_data_cache_raw_key") <- NULL
    }
    ds1
  })
}

#' Internal helper: worker cache payload
#'
#' @noRd
util_worker_cache_payload <- function(precompute = getOption(
  "dataquieR.precomputeStudyData",
  default = dataquieR.precomputeStudyData_default
)) {
  if (isTRUE(precompute)) {
    study_data_payload <- util_worker_cache_strip_study_data_attrs(
      as.list(.study_data_cache)
    )
    list(
      cache_as_list = as.list(.cache[[".cache"]]),
      study_data_cache = study_data_payload$study_data_cache,
      study_data_cache_study_data_attrs =
        study_data_payload$study_data_cache_study_data_attrs,
      study_data_cache_input_keys = as.list(.study_data_cache_input_keys),
      study_data_cache_meta_data = as.list(.study_data_cache_meta_data)
    )
  } else {
    list(
      cache_as_list = list(),
      study_data_cache = list(),
      study_data_cache_study_data_attrs = list(),
      study_data_cache_input_keys = list(),
      study_data_cache_meta_data = list()
    )
  }
}

#' Generate a full DQ report, v2
#'
#' @inheritParams .template_function_report_plan
#'
#' @param cores [integer] number of cpu cores to use or a named list with
#'                        arguments for the internal parallel backend
#'                        (`util_parallel_start`) or NULL, if parallel has
#'                        already been started by the caller. Can also be
#'                        a cluster. In RStudio report rendering, caller-owned
#'                        clusters can make HTML finalization hang; prefer
#'                        letting `dataquieR` create the cluster from a number
#'                        or backend list.
#' @param debug_parallel [logical] print blocks currently evaluated in parallel
#' @param all_calls [list] a list of calls
#' @param filter_result_slots [character] regular expressions, only
#'                                               if an indicator function's
#'                                               result's name
#'                                               matches one of these, it'll
#'                                               be used for the report. If
#'                                               of length zero, no filtering
#'                                               is performed.
#' @param checkpoint_resumed [logical] if using a `storr_factory` and the back-
#'                                     end there is already filled
#'                                     compute all missing result and add them
#'                                     to the back-end.
#' @param content_file [character] internal use-- only for progress messages
#' @inheritParams dq_report2
#'
#' @return a [dataquieR_resultset2]. Can be printed creating a RMarkdown-report.
#'
#' @family reporting_functions
#' @concept process
#' @noRd
util_evaluate_calls <-
  function(all_calls,
    study_data,
    meta_data,
    label_col,
    meta_data_segment,
    meta_data_dataframe,
    meta_data_cross_item,
    resp_vars,
    filter_result_slots,
    cores,
    debug_parallel,
    mode = c("default", "futures", "queue", "parallel"),
    mode_args,
    my_storr_object = NULL,
    checkpoint_resumed,
    dt_adjust = as.logical(getOption(
      "dataquieR.dt_adjust",
      dataquieR.dt_adjust_default
    )),
    content_file) {
    conds <- NULL # integrity issues outside the pipeline are collected here

    my_storr_object <- util_fix_storr_object(my_storr_object)

    function_names <- vapply(lapply(all_calls, `[[`, 1), as.character,
      FUN.VALUE = character(1)
    )

    if (!is.null(my_storr_object) && checkpoint_resumed) {
      all_calls_cp <- all_calls[setdiff(
        names(all_calls),
        my_storr_object$list(
          namespace =
            util_get_storr_stat_namespace(my_storr_object)
        )
      )]
    } else {
      all_calls_cp <- all_calls
    }

    # Historical storr initialization prototype removed here. Inspect with
    # `git show bc7730c4e3 -- R/util_evaluate_calls.R` before restoring.

    if (length(mode_args) > 0) {
      if (!is.list(mode_args) || is.null(names(mode_args)) ||
          any(util_empty(names(mode_args)))) {
        util_message(
          "%s needs to be a named list",
          sQuote("mode_args")
        )
        mode_args <- list()
      }
    } else {
      mode_args <- list()
    }


    mode <- util_match_arg(mode)

    # Initialize the internal backend options before code below depends on them.
    invisible(force(util_parallel_get_options()$settings$mode))

    # maybe also
    # https://cran.r-project.org/web/packages/parabar/readme/README.html
    r <- list()
    util_setup_rstudio_job(
      "Computing dq_report2, parallel computation",
      n = length(all_calls)
    )

    progress_msg("Cluster setup", "initializing parallel mode, if applicable")
    if (!missing(content_file)) {
      progress_msg(
        "Cluster setup",
        sprintf("content_file = %s", dQuote(content_file))
      )
    }

    if (mode == "queue") {
      cores_was_missing <- missing(cores)
      cores_matches_caller_default <- FALSE
      if (!cores_was_missing) {
        cores_matches_caller_default <- tryCatch(
          eval.parent(call("missing", as.symbol("cores"))) &&
            identical(
              cores,
              eval.parent(formals(rlang::caller_fn())$cores)
            ),
          error = function(...) FALSE
        )
      }
      if (cores_was_missing || cores_matches_caller_default) {
        cores <- util_detect_cores()
      }
      if (length(cores) != 1 ||
          !util_is_integer(cores) ||
          is.na(cores) ||
          cores > util_detect_cores()) {
        cores <- util_detect_cores()
        util_message(
          c(
            "For mode %s, %s can only be an integer(1) <= %d,",
            "it to its maximum"
          ),
          dQuote(mode),
          sQuote("cores"),
          cores
        )
      }
      q <- util_queue_cluster_setup(
        n_nodes = cores,
        progress = progress,
        debug_parallel = debug_parallel,
        my_storr_object = my_storr_object
      )
    } else {
      q <- NULL
    }

    if (length(all_calls_cp) > 0) {
      if (is.null(q)) {
        if (!is.null(cores)) {
          if (inherits(cores, "cluster")) {
            old_def_cl <- parallel::getDefaultCluster()
            parallel::setDefaultCluster(cores)
            withr::defer(parallel::setDefaultCluster(old_def_cl))
            # also let the internal backend reflect the caller's cluster
            suppressMessages(util_parallel_start(
              mode = "socket",
              cpus = length(parallel::getDefaultCluster()),
              load.balancing = TRUE
            ))
            withr::defer(suppressMessages(util_parallel_stop()))
          } else if (inherits(cores, "list")) {
            suppressMessages(do.call(util_parallel_start, cores))
            withr::defer(
              {
                Sys.sleep(2)
                suppressMessages(util_parallel_stop())
              }
            ) # whyever, rstudio needs these two seconds, it hangs, otherwise.
          } else {
            suppressMessages(util_parallel_start("socket",
                cpus = cores,
                logging = FALSE,
                load.balancing = TRUE
              ))
            withr::defer(
              {
                Sys.sleep(2)
                suppressMessages(util_parallel_stop())
              }
            ) # whyever, rstudio needs these two seconds, it hangs, otherwise.
          }
          cores <- NULL
        } else if
        (!util_parallel_get_options()$settings$mode %in%
            c("BatchJobs", "batchtools") &&
            !is.null(parallel::getDefaultCluster())) {
          suppressMessages(util_parallel_start(
            mode = "socket",
            cpus = length(parallel::getDefaultCluster()),
            load.balancing = TRUE
          ))
          withr::defer(suppressMessages(util_parallel_stop()))
        }

        parlib <- function(lib) {
          suppressMessages(suppressPackageStartupMessages(
            util_parallel_library(lib, show.info = FALSE)
          ))
        }
        parload_ns <- function(lib) {
          .exp <- substitute({
            suppressMessages(suppressPackageStartupMessages(
              loadNamespace(lib)
            ))
            invisible(NULL)
          })
          parexp(".exp")
          par_eval_q(eval(.exp))
        }
        parexp <- util_parallel_export
        par_eval_q <- function(expr) {
          if (is.null(parallel::getDefaultCluster())) {
            eval(expr)
          } else {
            do.call(
              parallel::clusterEvalQ,
              list(cl = NULL, substitute(expr))
            )
          }
        }
      } else {
        parload_ns <- function(lib) {
          q$workerEval(function(lib) {
            suppressMessages(suppressPackageStartupMessages(
              loadNamespace(lib)
            ))
          }, list(lib = lib))
        }
        parlib <- function(lib) {
          q$workerEval(function(lib) {
            suppressMessages(suppressPackageStartupMessages(
              require(lib, quietly = TRUE, character.only = TRUE)
            ))
          }, list(lib = lib))
        }
        parexp <- function(...) {
          q$export(...)
        }
        par_eval_q <- function(expr) { # may not work as expected
          q$workerEval(function(expr) {
            eval(expr)
          }, list(expr = substitute(expr)))
        }
      }

      progress_msg("Cluster setup: initializing parallel mode, if applicable", "loading library") # nolint: line_length_linter.

      if (suppressWarnings(util_ensure_suggested("pkgload",
            err = FALSE,
            goal =
              "not really needed"
          ))) {
        dev_package <- pkgload::is_dev_package(utils::packageName())
      } else {
        dev_package <- FALSE
      }

      if (dev_package && !is.null(parallel::getDefaultCluster()) &&
          !isTRUE(getOption("dataquieR.tmp_no_load_all"))) {
        .d <- getNamespaceInfo(asNamespace(utils::packageName()), "path")
        .exp <- substitute({
          pkgload::load_all(path = .d)
          invisible(NULL)
        })
        parexp(".exp")
        par_eval_q(eval(.exp))
      } else {
        suppressWarnings(suppressMessages(try(
          {
            parlib(utils::packageName())
          },
          silent = TRUE
        )))
      }
      parload_ns("hms")

      ..e <- environment()
      conds <- list()

      suppressWarnings(suppressMessages(withCallingHandlers(
        {
          if (dt_adjust) {
            progress_msg(
              "Cluster setup: initializing parallel mode, if applicable",
              "consolidating data types 1..."
            )

            ## Adjust data type and compute int_data_type_matrix before

            withr::with_options(
              list(dataquieR.testdebug = TRUE),
              int_datatype_matrix_res <- int_datatype_matrix(
                study_data = study_data,
                meta_data = meta_data,
                label_col = label_col
              )
            )
          } else {
            withr::with_options(
              list(dataquieR.testdebug = TRUE),
              int_datatype_matrix_res <- int_datatype_matrix(
                study_data = study_data[1, , drop = FALSE],
                meta_data = meta_data,
                label_col = label_col
              )
            )
          }
        },
        condition = function(cnd) {
          ..e$conds <- c(..e$conds, list(cnd))
        }
      )))

      conds <- unique(conds[vapply(
        lapply(conds, util_attr, "integrity_indicator",
          exact = TRUE
        ),
        length,
        FUN.VALUE = integer(1)
      ) == 1])


      old_called_in_pipeline <- .dq2_globs$.called_in_pipeline
      .dq2_globs$.called_in_pipeline <- TRUE
      withr::defer(.dq2_globs$.called_in_pipeline <-
          old_called_in_pipeline)

      if (dt_adjust) {
        progress_msg(
          "Cluster setup: initializing parallel mode, if applicable",
          "consolidating data types 2..."
        )
        study_data <- util_adjust_data_type(
          study_data = study_data,
          meta_data = meta_data,
          relevant_vars_for_warnings = NULL
        )
      }

      progress_msg("Cluster setup: initializing parallel mode, if applicable", "exporting data") # nolint: line_length_linter.

      suppressWarnings(parexp(
        "study_data", "meta_data", "label_col", "meta_data_segment", "meta_data_dataframe", "meta_data_cross_item", # nolint: line_length_linter.
        "my_storr_object"
      ))
      .options <- options() # options to be copied to the children (child process) # nolint: line_length_linter.
      .options <- .options[startsWith(names(.options), "dataquieR.")] # only dataquieR options selected # nolint: line_length_linter.

      progress_msg("Cluster setup: initializing parallel mode, if applicable", "exporting options") # nolint: line_length_linter.

      suppressWarnings(parexp(".options"))

      progress_msg("Cluster setup: initializing parallel mode, if applicable", "exporting data frame cache") # nolint: line_length_linter.

      dataframes_list <- as.list(.dataframe_environment())
      suppressWarnings(parexp("dataframes_list"))

      progress_msg("Cluster setup: initializing parallel mode, if applicable", "exporting other caches") # nolint: line_length_linter.

      cache_payload <- util_worker_cache_payload()
      cache_as_list <- cache_payload$cache_as_list
      study_data_cache <- cache_payload$study_data_cache
      study_data_cache_study_data_attrs <-
        cache_payload$study_data_cache_study_data_attrs
      study_data_cache_input_keys <- cache_payload$study_data_cache_input_keys
      study_data_cache_meta_data <- cache_payload$study_data_cache_meta_data

      suppressWarnings(parexp("cache_as_list"))
      suppressWarnings(parexp("study_data_cache"))
      suppressWarnings(parexp("study_data_cache_study_data_attrs"))
      suppressWarnings(parexp("study_data_cache_input_keys"))
      suppressWarnings(parexp("study_data_cache_meta_data"))
      suppressWarnings(parexp("util_worker_cache_restore_study_data_attrs"))

      progress_msg("Cluster setup: initializing parallel mode, if applicable", "finalizing setup of compute nodes") # nolint: line_length_linter.

      if (!is.null(q) || !is.null(parallel::getDefaultCluster())) {
        par_eval_q({
          ..glbs <- get(".dq2_globs", envir = asNamespace("dataquieR"))
          ..glbs$.called_in_pipeline <- TRUE
        })
        par_eval_q(options(.options))
        par_eval_q(dataquieR::prep_add_data_frames(
          data_frame_list = dataframes_list
        ))
        par_eval_q({
          assign(
            x = ".cache",
            envir = get(".cache", envir = asNamespace("dataquieR")),
            value = as.environment(cache_as_list)
          )
          study_data_cache <-
            util_worker_cache_restore_study_data_attrs(
              study_data_cache = study_data_cache,
              study_data_attrs = study_data_cache_study_data_attrs
            )
          list2env(study_data_cache, get(
            ".study_data_cache",
            envir = asNamespace("dataquieR")
          ))
          list2env(study_data_cache_input_keys, get(
            ".study_data_cache_input_keys",
            envir = asNamespace("dataquieR")
          ))
          list2env(study_data_cache_meta_data, get(
            ".study_data_cache_meta_data",
            envir = asNamespace("dataquieR")
          ))
        })
        u8 <- par_eval_q(l10n_info()[["UTF-8"]])
        u8 <- vapply(u8, identity, FUN.VALUE = logical(1))
        if (!all(u8)) {
          util_warning(
            c(
              "%d of the %d cluster nodes do not support",
              "UTF-8, this may cause trouble with the encoding.",
              "For Windows nodes, you should use R > 4.2.0",
              "on all nodes. Also, all nodes should use a UTF-8",
              "character set by default (see Sys.setlocale())"
            ),
            sum(!u8), length(u8)
          )
        }
      } else {
        dataquieR::prep_add_data_frames(
          data_frame_list = dataframes_list
        )
        assign(
          x = ".cache",
          envir = get(".cache", envir = asNamespace("dataquieR")),
          value = as.environment(cache_as_list)
        )
      }
      # util_parallel_map could also serve a parLapply-style use case here.

      # this exports the static data to the cluster, this is always done, even
      # if
      # already avail. functions don't touch the exported data.
      current_cpus <- util_parallel_get_options()$settings$cpus

      worker <- util_eval_to_dataquieR_result
      formals(worker)$filter_result_slots <- filter_result_slots
      force(formals(worker)$filter_result_slots)
      formals(worker)$checkpoint_resumed <- checkpoint_resumed
      force(formals(worker)$checkpoint_resumed)

      .par_mode <- util_parallel_get_options()$settings$mode
      if (identical(.par_mode, "multicore")) {
        worker_env <- environment()
      } else {
        worker_env <- new.env(parent = asNamespace(utils::packageName()))
      }
      worker_env$util_add_result_conditions_to_summary <-
        util_add_result_conditions_to_summary
      worker_env$util_add_variable_group_identity <-
        util_add_variable_group_identity
      worker_env$util_entity_grading_rulesets <-
        util_entity_grading_rulesets
      entity_grading_worker <- util_attach_entity_grading_context
      environment(entity_grading_worker) <- worker_env
      worker_env$util_attach_entity_grading_context <- entity_grading_worker
      environment(worker) <- worker_env

      progress_msg("Computation", "computing report")

      n_nodes <- max(1, as.integer(current_cpus[[1]]), na.rm = TRUE)

      if (mode == "futures") { # have/use futures
        r <- util_parallel_futures(
          all_calls = all_calls_cp,
          n_nodes = n_nodes,
          progress = progress,
          worker = worker,
          debug_parallel = debug_parallel,
          my_storr_object = my_storr_object
        )
      } else if (mode == "queue") {
        step <- 6
        if ("step" %in% names(mode_args)) {
          step <- mode_args[["step"]]
          if (length(step) != 1 || !is.numeric(step) ||
              !is.vector(step) || !is.finite(step) || !util_is_integer(step) ||
              step <= 0 || step > 10000) {
            util_message(
              c(
                "%s needs to be a positive scalar integer value <= %d, falling",
                "back to default %d"
              ),
              dQuote("step"),
              10000,
              6
            )
            step <- 6
          }
        }
        r <- q$compute_report(
          all_calls = all_calls_cp,
          worker = worker,
          step = step
        )
      } else {
        r <- util_parallel_classic(
          all_calls = all_calls_cp,
          n_nodes = n_nodes,
          progress = progress,
          worker = worker,
          debug_parallel = debug_parallel,
          my_storr_object = my_storr_object
        )
      }

      if (!dynGet(".is_testing", ifnotfound = FALSE)) {
        util_message(
          sprintf(
            "%s [%s], %s", Sys.time(), "INFO",
            "DQ -- done"
          )
        )
      }
    }

    r_with_already_computed_r <- setNames(r[names(all_calls)], names(all_calls))
    r_with_already_computed_r[setdiff(names(all_calls), names(r))] <- NA

    r <- r_with_already_computed_r

    my_storr_object <- util_fix_storr_object(my_storr_object)

    if (!is.null(my_storr_object) && length(unclass(r)) > 0 &&
        length(all_calls_cp) > 0 &&
        my_storr_object$exists(NO_SHARED_STORR) &&
        identical(my_storr_object$get(NO_SHARED_STORR), TRUE)
    ) {
      my_storr_object$del(NO_SHARED_STORR)
      util_error("Your storr backend must be shared amongst the compute nodes.")
    }

    progress_msg("Computation", "finalizing report")

    # Previous result classes are intentionally replaced below.
    attr(r, "all_calls") <- all_calls

    class(r) <- union(
      dataquieR_resultset_class2,
      "square_results"
    )
    attr(r, "my_storr_object") <- my_storr_object
    # Restoring the previous class order is not currently needed.
    if (!is.null(my_storr_object) &&
        inherits(my_storr_object, "storr") &&
        !util_is_try_error(try(my_storr_object$list(), silent = TRUE))) {
      # to make summary work
      atts_r <- attributes(r)
      atts_r[["my_storr_object"]] <- NULL # dont save this ever
      my_storr_object$mset(
        key = names(atts_r), value = atts_r, namespace =
          util_get_storr_att_namespace(my_storr_object)
      )
    }

    if (!is.null(my_storr_object)) {
      my_storr_object$flush_cache()
      # Historical missing-object fallback removed here. Inspect Git history
      # before restoring placeholder results for incomplete storr backends.
    } else {
      # Historical in-memory result repair removed here. The relevant
      # attributes are now assigned when each result is computed.
    }

    # Historical early summary aggregation removed here. It must stay after
    # `int_datatype_matrix`; inspect Git history before moving it back.

    if (!is.null(my_storr_object) &&
        inherits(my_storr_object, "storr") &&
        !util_is_try_error(try(my_storr_object$list(), silent = TRUE))) {
      # to make summary work
      atts_r <- attributes(r)
      atts_r[["my_storr_object"]] <- NULL # dont save this ever
      my_storr_object$mset(
        key = names(atts_r), value = atts_r, namespace =
          util_get_storr_att_namespace(my_storr_object)
      )
    }


    if (any(is.na(names(r)))) {
      nms <- paste(
        vapply(r, util_attr, "cn", exact = TRUE, FUN.VALUE = character(1)),
        vapply(lapply(r, util_attr, "call", exact = TRUE), function(cl) {
          util_attr(cl, "entity_name", exact = TRUE)
        }, FUN.VALUE = character(1)),
        sep = "."
      )

      names(r)[is.na(names(r))] <-
        nms[is.na(names(r))]
      util_stop_if_not(
        `Internal error, sorry, please report: report name inconsistency` =
          all(nms == names(r))
      )
    }

    # add left-out integrity results to the report -----
    if (length(r)) {
      idtm_variable_labels <-
        vapply(
          lapply(.access_dq_rs2(r, startsWith(names(r), "int_datatype_matrix.")), # nolint: line_length_linter.
            util_attr, "call",
            exact = TRUE
          ), util_attr, "entity_name",
          exact = TRUE,
          FUN.VALUE = character(1)
        )

      int_datatype_matrix. <-
        lapply(idtm_variable_labels, function(lab) {
          res <- r[[paste0("int_datatype_matrix.", lab)]]
          attr(res, "error") <- NULL
          attr(res, "warning") <- NULL
          attr(res, "message") <- NULL
          if (!dt_adjust) {
            attr(res, "error") <- list(util_attr(try(
              util_error(
                c(
                  "data type check was disabled",
                  "(argument dt_adjust or option dataquieR.dt_adjust)"
                ),
                applicability_problem = TRUE
              ),
              silent = TRUE
            ), "condition", exact = TRUE))
            attr(res, "error")[[1]]$trace <- NULL
            res$SummaryTable <- NULL
            res$SummaryData <- NULL
            res$ReportSummaryTable <- NULL
            class(res) <- union("dataquieR_NULL", class(res))
          } else {
            res$SummaryTable <-
              int_datatype_matrix_res$SummaryTable[
                int_datatype_matrix_res$SummaryTable$Variables == lab, ,
                FALSE
              ]

            res$SummaryData <-
              int_datatype_matrix_res$SummaryData[
                int_datatype_matrix_res$SummaryData$Variables == lab, ,
                FALSE
              ]

            res$ReportSummaryTable <- NULL
            res$ReportSummaryTable <-
              int_datatype_matrix_res$ReportSummaryTable[
                int_datatype_matrix_res$ReportSummaryTable$Variables == lab, ,
                FALSE
              ]
          }
          my_conds <- conds[vapply(conds, function(cnd) {
            (identical(util_attr(cnd, "varname", exact = TRUE), lab)) ||
              (is.null(cnd))
          }, FUN.VALUE = logical(1))]

          if (length(my_conds) > 0) {
            for (cnd in my_conds) {
              if (inherits(cnd, "error")) {
                attr(res, "error") <- c(
                  util_attr(res, "error", exact = TRUE),
                  list(cnd)
                )
              }
              if (inherits(cnd, "warning")) {
                attr(res, "warning") <- c(
                  util_attr(res, "warning", exact = TRUE),
                  list(cnd)
                )
              }
              if (inherits(cnd, "message")) {
                attr(res, "message") <- c(
                  util_attr(res, "message", exact = TRUE),
                  list(cnd)
                )
              }
            }
          }

          res
        })
      # }

      int_datatype_matrix. <- mapply(
        SIMPLIFY = FALSE,
        r = int_datatype_matrix.,
        nm = names(int_datatype_matrix.),
        function(r, nm) {
          s <- prep_extract_summary(r)
          r_summary1 <-
            suppressWarnings(
              prep_summary_to_classes(s)
            )

          r_summary <- util_add_result_conditions_to_summary(
            result = r,
            summary = r_summary1,
            function_name = "int_datatype_matrix"
          )

          attr(r_summary, "resnames") <- names(r)
          attr(r, "r_summary") <- r_summary
          if (!is.null(my_storr_object) &&
              inherits(my_storr_object, "storr") &&
              !util_is_try_error(try(my_storr_object$list(), silent = TRUE))) {
            my_storr_object$set(key = nm, value = r)
            my_storr_object$set(
              key = nm,
              value = r_summary,
              namespace = util_get_storr_summ_namespace(my_storr_object)
            )
            my_storr_object$flush_cache()
            try(my_storr_object$driver$disconnect(), silent = TRUE)
            try(my_storr_object$driver$env$sync(force = TRUE), silent = TRUE)

            r <- NA
          } else if (!is.null(my_storr_object)) {
            r <- "Invalid storr object"
          }
          r
        }
      )

      if (is.null(my_storr_object)) { # only the RAM based version needs this line, otherwise, we overwrite the results also in the back-end with NAs, but we won't write the r_summary, then # nolint: line_length_linter.
        .access_dq_rs2(r, startsWith(names(r), "int_datatype_matrix.")) <-
          int_datatype_matrix.
      }

      # do after itdm, see above
      if (!is.null(my_storr_object)) {
        namespace <- util_get_storr_summ_namespace(my_storr_object)
        all_sums <- my_storr_object$mget(
          my_storr_object$list(
            namespace = namespace
          ),
          namespace = namespace
        )
      } else {
        all_sums <- lapply(r, util_attr, "r_summary", exact = TRUE)
      }

      rsn <- sort(unique(unname(unlist(lapply(all_sums, util_attr, "resnames",
                exact = TRUE
              )))))
      attr(r, "resnames") <- rsn


      # overwrite the result for int_datatype_matrix_res / DONE
    }

    # make report compatible with old Square2 reports -----

    function_names <- setNames(nm = function_names)

    aliases <- gsub("\\..*$", "", names(r))

    matrix_list <- lapply(
      setNames(nm = setdiff(
        aliases,
        util_attr(all_calls,
          "multivariatcol",
          exact = TRUE
        )
      )),
      function(alias) {
        lapply(
          setNames(nm = resp_vars),
          function(vn) {
            if (is.call(
              all_calls[[paste0(alias, ".", vn)]]
            )) {
              rlang::call_args(
                all_calls[[paste0(alias, ".", vn)]]
              )
            } else {
              setNames(list(), nm = character(0))
            }
          }
        )
      }
    )

    function_names <- function_names[!duplicated(aliases)]
    aliases <- aliases[!duplicated(aliases)]

    descriptions <- .manual$descriptions[function_names]
    if (length(descriptions) == 0) {
      descriptions <- rep(NA_character_, length(function_names))
    }
    descriptions[vapply(descriptions, is.null, FUN.VALUE = logical(1))] <-
      NA_character_
    descriptions <- unlist(descriptions, recursive = FALSE)

    function_alias_map <- data.frame(
      fk_report = rep(NA_integer_, length(aliases)),
      alias = aliases,
      acronym = util_abbreviate(aliases),
      description = descriptions,
      fk_function = rep(NA_integer_, length(aliases)),
      function_description = descriptions,
      name = function_names,
      stringsAsFactors = FALSE
    )

    attr(matrix_list, "function_alias_map") <- function_alias_map

    fn <- unique(function_names)

    function2category <- setNames(
      c(
        des = "Descriptors",
        int = "Integrity",
        com = "Completeness",
        con = "Consistency",
        acc = "Accuracy"
      )[gsub(
        "_.*$",
        "",
        fn
      )],
      nm = fn
    )

    attr(matrix_list, "function2category") <- function2category

    dim_ranks <- setNames(
      c(
        des = 0,
        int = 1,
        com = 2,
        con = 3,
        acc = 4
      )[gsub(
        "_.*$",
        "",
        aliases
      )],
      nm = aliases
    )


    drr <- rank(dim_ranks)
    fnr <- rank(function_names)
    alr <- rank(aliases)

    base <- max(c(drr, fnr, alr, 10), na.rm = TRUE)

    attr(matrix_list, "col_indices") <- setNames(
      (drr * base * base + fnr * base + alr) * 10,
      nm = aliases
    )

    vo <- as.numeric(meta_data[[VARIABLE_ORDER]])
    if (0 == length(vo)) {
      vo <- seq_len(nrow(meta_data)) * 10
    }
    offset <- max(vo, na.rm = TRUE) + 1
    if (is.infinite(offset)) {
      offset <- 1
    }
    vo[util_empty(vo)] <- offset + seq_len(sum(util_empty(vo)))
    attr(matrix_list, "row_indices") <- setNames(vo,
      nm = meta_data[[label_col]]
    )
    if (!length(r)) {
      r <- list()
    }
    attr(r, "matrix_list") <- matrix_list
    attr(r, "label_col") <- label_col
    attr(r, "meta_data") <- meta_data # one discapency from SQ2 reults, here: We have *always* v2.0 metadata, here. Will not yet write a back-converter, as long as this is not really needed. # nolint: line_length_linter.

    attr(r, "meta_data_segment") <- meta_data_segment
    attr(r, "meta_data_dataframe") <- meta_data_dataframe
    attr(r, "meta_data_cross_item") <- meta_data_cross_item

    # Include all tables referred to by the standard metadata ----
    meta_data_frames <- util_report_meta_data_frames(r)

    refs <- unname(unlist(lapply(
      meta_data_frames,
      function(mdf_name) {
        mdf <- util_attr(r, mdf_name, exact = TRUE)
        cls <- grep("_TABLE$", colnames(mdf), value = TRUE)
        if (length(cls)) {
          unlist(mdf[, cls, drop = TRUE], recursive = TRUE)
        } else {
          character(0)
        }
      }
    ), recursive = FALSE))


    refs <- refs[!util_empty(refs)]
    refs <- gsub(
      sprintf("\\s*\\%s\\s*", SPLIT_CHAR),
      SPLIT_CHAR, refs
    )
    refs <- gsub(
      sprintf("(\\%s.*?)\\%s.*$", SPLIT_CHAR, SPLIT_CHAR),
      "\\1", refs
    )
    refs <- unique(sort(refs))

    refs <- lapply(setNames(nm = refs), function(dfn) {
      r <- data.frame(
        `NA` = paste(dQuote(dfn), "is not available."),
        check.names = FALSE
      )
      if (!getOption(
        "dataquieR.non_disclosure",
        dataquieR.non_disclosure_default
      )) { # this is anyway removed below, if the non-disclosure option is TRUE, but we do not needto add it first. # nolint: line_length_linter.
        try(
          r <- prep_get_data_frame(dfn),
          silent = TRUE
        )
      }
      r
    })

    if (!getOption(
      "dataquieR.non_disclosure",
      dataquieR.non_disclosure_default
    )) {
      result_refs <- lapply(r, function(result) {
        if (is.raw(result)) {
          result <- util_decompress(result)
        }
        util_attr(result, "referred_tables", exact = TRUE)
      })
      result_refs <- Filter(is.list, result_refs)
      result_refs <- unlist(unname(result_refs), recursive = FALSE)
      result_refs <- result_refs[
        !duplicated(names(result_refs), fromLast = TRUE)
      ]
      refs[names(result_refs)] <- result_refs
    }

    attr(r, "referred_tables") <- refs

    # add information on the dimension names ----

    attr(r, "study_data_dimnames") <- dimnames(study_data)
    attr(r, "cn") <- util_attr(all_calls, "cn", exact = TRUE)
    attr(r, "rn") <- util_attr(all_calls, "rn", exact = TRUE)
    attr(r, "integrity_issues_before_pipeline") <- conds

    attr(r, "dt_adjust") <- dt_adjust

    # class is to be compatible with Square2 -----

    class(r) <- union(
      dataquieR_resultset_class2,
      "square_results"
    )

    if (getOption(
      "dataquieR.non_disclosure",
      dataquieR.non_disclosure_default
    )) {
      progress_msg("Computation", "undisclosing report")
      r <- util_undisclose(r)
    }

    progress_msg("Computation", "finished")

    # Return report ----
    r
  }

NO_SHARED_STORR <- "NO_SHARED_STORR"
