#' Evaluate an expression and create a `dataquieR_result` object from
#' it's evaluated value
#'
#' if an error occurs, the function will return a corresponding object
#' representing that error. all conditions will be recorded and replayed,
#' whenever the result is printed by [print.dataquieR_result].
#'
#' @param expression the expression
#' @param env the environment to evaluate the expression in
#' @param filter_result_slots [character] regular expressions, only
#'                                               if an indicator function's
#'                                               result's name
#'                                               matches one of these, it'll
#'                                               be used for the report. If
#'                                               of length zero, no filtering
#'                                               is performed.
#' @param nm [character] name for the computed result
#' @param function_name [character] name of the function to be executed
#' @param my_storr_object a `storr` object to store the result in
#' @param my_call the call being executed (equivalent to `expression`)
#' @param init [logical] is this an initial call to compute dummy results?
#' @param called_in_pipeline [logical] if the evaluation should be considered
#'                                     as part of a pipeline.
#' @param checkpoint_resumed [logical] if `my_storr_object` and `nm` are set,
#'                                     check, if the result already exists and
#'                                     do not re-compute in such cases.
#'
#' @return a `dataquieR_result` object
#'
#' @family reporting_functions
#' @concept process
#' @noRd
util_eval_to_dataquieR_result <- function(expression, env = parent.frame(),
  filter_result_slots, nm,
  function_name,
  my_call = expression,
  my_storr_object = NULL,
  init = FALSE,
  called_in_pipeline = TRUE,
  checkpoint_resumed = FALSE) {
  withr::local_options(list(rlang_trace_top_env = rlang::current_env()))
  if (!isTRUE(getOption(
    "dataquieR.traceback",
    dataquieR.traceback_default
  ))) {
    withr::local_options(list(rlang_backtrace_on_error = "none"))
  }
  if (missing(function_name)) {
    error <- try(
      function_name <- rlang::call_name(expression),
      silent = TRUE
    )
    if (util_is_try_error(error)) {
      function_name <- "unknown function"
    }
  }

  use_storr <- FALSE

  storr_err <-
    try(my_storr_object <- util_fix_storr_object(my_storr_object),
      silent = TRUE
    )

  if (util_is_try_error(storr_err)) {
    return(util_attr(storr_err, "condition", exact = TRUE))
  }
  if (!is.null(my_storr_object) &&
      inherits(my_storr_object, "storr") &&
      !util_is_try_error(try(my_storr_object$list(), silent = TRUE))) {
    use_storr <- TRUE
    if (!init) {
      my_storr_object$del(NO_SHARED_STORR)
    }
  }
  .old_.dq2_globs_.called_in_pipeline <- .dq2_globs$.called_in_pipeline
  .dq2_globs$.called_in_pipeline <- called_in_pipeline
  withr::defer({
    .dq2_globs$.called_in_pipeline <- .old_.dq2_globs_.called_in_pipeline
  })
  errors <- list()
  warnings <- list()
  messages <- list()
  e <- environment()
  collect_condition <- function(cnd) {
    if (!isTRUE(getOption(
      "dataquieR.traceback",
      dataquieR.traceback_default
    ))) {
      attr(cnd, "trace") <- NULL
      cnd$trace <- NULL
    }
    if (inherits(cnd, "error")) {
      e$errors <- c(e$errors, list(cnd))
    } else if (inherits(cnd, "warning")) {
      e$warnings <- c(e$warnings, list(cnd))
    } else if (inherits(cnd, "message")) {
      e$messages <- c(e$messages, list(cnd))
    } else {
      util_error("")
    }
  }
  # `r_summary` is assembled only after condition categories are known.

  need_compute <- TRUE

  if (checkpoint_resumed &&
      called_in_pipeline &&
      use_storr &&
      my_storr_object$exists(nm,
        namespace =
          util_get_storr_stat_namespace(my_storr_object)
      )) {
    # Historical checkpoint-resume inspection removed here. Inspect with
    # `git show 0b2b77aaef -- R/util_eval_to_dataquieR_result.R`.
    need_compute <- FALSE
  }
  if (!need_compute) {
    r <- NA
  } else {
    add_attribs <- new.env(parent = emptyenv())
    r_summary1 <- data.frame()
    r <- list()
    class(r) <- union("empty", class(r))
    tm <- system.time(
      suppressWarnings(suppressMessages(try(withCallingHandlers(
        {
          r <- eval(expression, envir = env)
          if (length(r)) {
            if (length(filter_result_slots)) {
              r <- util_filter_names_by_regexps(
                r,
                filter_result_slots
              )
            }
            if (.called_in_pipeline) {
              r <- util_compress_ggplots_in_res(r)
            }
            check_id <- util_attr(my_call, CHECK_ID, exact = TRUE)
            check_label <- util_attr(my_call, CHECK_LABEL, exact = TRUE)
            r <- util_add_variable_group_identity(
              r,
              check_id = check_id,
              check_label = check_label
            )
            r <- util_attach_entity_grading_context(
              result = r,
              env = env,
              function_name = function_name
            )
          }
        },
        error = collect_condition,
        warning = collect_condition,
        message = collect_condition
      ), silent = TRUE)))
    )
    if (length(r) == 0) {
      r <- list()
      class(r) <- union("dataquieR_NULL", class(r))
    } else if ("SummaryTable" %in% names(r)) {
      meta_data <- try(eval(quote(meta_data), envir = env), silent = TRUE)
      label_col <- try(eval(quote(label_col), envir = env), silent = TRUE)
      if (is.data.frame(meta_data) &&
          is.character(label_col) &&
          length(label_col) == 1) {
        r$SummaryTable <- util_map_to_other_metrics(
          r$SummaryTable,
          eval(quote(meta_data),
            envir = env
          ),
          get("label_col",
            envir = env,
            inherits = TRUE
          ),
          add_attribs = add_attribs
        )
      }
    }
    for (a in names(add_attribs)) {
      attr(r, a) <- add_attribs[[a]]
    }
    attr(r, "error") <- errors
    attr(r, "warning") <- warnings
    attr(r, "message") <- messages
    attr(r, "system.time") <- tm

    attr(r, "function_name") <- function_name #### from util_evalute_calls
    attr(r, "cn") <- util_sub_string_left_from_.(nm)
    attr(r, "call") <- my_call

    class(r) <- union("dataquieR_result", class(r))
    res_check <- try(r <- util_dataquieR_result(r), silent = TRUE)
    if (util_is_try_error(res_check)) {
      attr(r, "error") <- util_attach_attr(
        list(util_attr(res_check, "condition",
            exact = TRUE
          )),
        class = "dataquieR_invalid_result_error"
      )
    }

    if (called_in_pipeline) {
      if (length(r) > 0) {
        try(
          {
            s <- prep_extract_summary(r)
            r_summary1 <- suppressWarnings(prep_summary_to_classes(s))
          },
          silent = TRUE
        )
      }

      r_summary <- util_add_result_conditions_to_summary(
        result = r,
        summary = r_summary1,
        function_name = function_name
      )

      attr(r_summary, "resnames") <- names(r)

      attr(r, "r_summary") <- r_summary
    }

    # Historical local storr debug dump removed here.

    if (!is.null(my_storr_object) &&
        inherits(my_storr_object, "storr") &&
        !util_is_try_error(try(my_storr_object$list(), silent = TRUE))) {
      # Historical storr disk/object-size debug output removed here. Inspect
      # with `git show 22c6c5bfb8 -- R/util_eval_to_dataquieR_result.R`.
      my_storr_object$set(
        key = nm,
        value = r_summary,
        namespace = util_get_storr_summ_namespace(my_storr_object)
      )
      if (!init && (!inherits(r, "dataquieR_NULL") ||
            length(util_attr(r, "error", exact = TRUE)) == 0 ||
            isTRUE(util_attr(util_attr(r, "error", exact = TRUE)[[1]],
                "intrinsic_applicability_problem",
                exact = TRUE
              )))) {
        if (!init) {
          my_storr_object$set(
            key = nm, value = TRUE, namespace =
              util_get_storr_stat_namespace(my_storr_object)
          )
        }
        if (called_in_pipeline) {
          r <- util_compress(r)
        }
        my_storr_object$set(key = nm, value = r)
      }
      my_storr_object$flush_cache()
      # Historical post-flush disk/RAM guard prototype removed here. Inspect
      # with `git show 22c6c5bfb8 -- R/util_eval_to_dataquieR_result.R`.
      r <- NA
    } else if (!is.null(my_storr_object)) {
      r <- "Invalid storr object"
    } else {
      if (called_in_pipeline) {
        r <- util_compress(r)
      }
    }

    try(my_storr_object$driver$disconnect(), silent = TRUE)

    try(my_storr_object$driver$env$sync(force = TRUE), silent = TRUE)
  }

  invisible(gc(verbose = FALSE, full = FALSE))
  r
}

.dq2_globs <- new.env(parent = emptyenv())
.dq2_globs$.called_in_pipeline <- FALSE
