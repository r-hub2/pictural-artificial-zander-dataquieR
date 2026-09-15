cat1 <- as.character(util_as_cat(1))
cat2 <- as.character(util_as_cat(2))
cat3 <- as.character(util_as_cat(3))
cat4 <- as.character(util_as_cat(4))
cat5 <- as.character(util_as_cat(5))


#' Internal helper: funins
#'
#' @noRd
util_funins <- function(f, expr, after = 1) { # https://stackoverflow.com/a/38733185 # nolint: line_length_linter.
  expr <- substitute(expr)
  rlang::fn_body(f) <- as.call(append(as.list(rlang::fn_body(f)), expr, after = after)) # nolint: line_length_linter.
  f
}

#' Internal helper: funwrap
#'
#' @noRd
util_funwrap <- function(f, fn) {
  rlang::fn_body(f) <-
    call(
      "{",
      call(fn, rlang::fn_body(f))
    )
  f
}

#' Internal helper: first arg study data or resp vars
#'
#' @noRd
util_first_arg_study_data_or_resp_vars <- function() {
  cl <- rlang::caller_call()
  clan <- rlang::call_args_names(cl)
  cf <- rlang::caller_fn()
  formals_names <- names(formals(cf))
  if (length(formals_names) > 1 &&
      formals_names[[1]] %in% c("resp_vars", "variable_group", "label_col") &&
      "study_data" %in% formals_names &&
      length(clan) >= 1
  ) {
    study_data_formal_pos <- which(formals_names == "study_data")
    first_arg_nm <- formals_names[[1]]
    if (clan[[1]] == "" &&
        !first_arg_nm %in% clan &&
        !"study_data" %in% clan) {
      missing_first_arg <- eval(
        call(
          "missing",
          as.symbol(first_arg_nm)
        ),
        envir = rlang::caller_env()
      )
      if (!missing_first_arg) {
        first_arg_val <- get(first_arg_nm, rlang::caller_env())
        if (is.data.frame(first_arg_val) ||
          isTRUE(try(first_arg_val %in% prep_list_dataframes(),
              silent = TRUE
            )) ||
          is.data.frame(try(
            prep_get_data_frame(first_arg_val,
              column_names_only = TRUE,
              keep_types = TRUE
            ),
            silent = TRUE
          ))) {
          args <- rlang::call_args(cl)[-1]

          # Historical preservation of the original first argument was removed
          # here. Inspect commit 67e1265376 before restoring call rewriting.
          args[["study_data"]] <- first_arg_val
          repl_call <-
            do.call(call, c(list(rlang::call_name(cl)), args), quote = TRUE)
          return(repl_call)
        }
      }
    }
  }
  return(NULL)
}

#' Internal helper: apply meta data cross fallback
#'
#' @noRd
util_apply_meta_data_cross_fallback <- function(env_args, what_i_have) {
  if (!("meta_data_cross_item" %in% what_i_have) &&
      !is.null(env_args$meta_data_cross)) {
    env_args$meta_data_cross_item <- env_args$meta_data_cross
    env_args$meta_data_cross <- NULL
  }
  env_args
}

#' Internal helper: apply decorator meta data alias
#'
#' @noRd
util_apply_decorator_meta_data_alias <- function(env_args, canonical,
  aliases, call_arg_names,
  env) {
  present_aliases <- aliases[aliases %in% call_arg_names]
  if (length(present_aliases) < 1 || canonical %in% call_arg_names) {
    return(env_args)
  }

  alias <- present_aliases[[1]]
  env_args[[canonical]] <- get(alias, envir = env, inherits = FALSE)
  env_args[[paste0(".", canonical, "_arg_name")]] <- alias
  env_args
}

#' Internal helper: should run decorator preps
#'
#' @noRd
util_should_run_decorator_preps <- function() {
  !.called_in_pipeline &&
    !getOption("dataquieR.testdebug", dataquieR.testdebug_default) &&
    (!identical(Sys.getenv("TESTTHAT"), "true") ||
        isTRUE(getOption("dataquieR.test_decorator", FALSE)))
}

#' Internal helper: testthat blocks interactive wrapper
#'
#' @noRd
util_testthat_blocks_interactive_wrapper <- function() {
  identical(Sys.getenv("TESTTHAT"), "true") &&
    !isTRUE(getOption("dataquieR.test_interactive_wrapper", FALSE))
}

#' Internal helper: resolve decorator label col
#'
#' @noRd
util_resolve_decorator_label_col <- function(label_col, what_i_have,
  item_level, env) {
  if ("label_col" %in% what_i_have &&
      is.character(label_col) &&
      length(label_col) == 1 &&
      !is.na(label_col)) {
    return(label_col)
  }

  caller_label_col <- try(get("label_col", envir = env, inherits = FALSE),
    silent = TRUE
  )
  if (!util_is_try_error(caller_label_col) &&
      is.character(caller_label_col) &&
      length(caller_label_col) == 1 &&
      !is.na(caller_label_col)) {
    return(caller_label_col)
  }

  item_level_cols <- if (is.data.frame(item_level)) {
    colnames(item_level)
  } else {
    character(0)
  }
  if (LABEL %in% item_level_cols) {
    return(LABEL)
  }
  if (VAR_NAMES %in% item_level_cols) {
    return(VAR_NAMES)
  }

  if (is.character(label_col) &&
      length(label_col) == 1 &&
      !is.na(label_col)) {
    return(label_col)
  }

  LABEL
}

#' Internal helper: decorator relevant var names
#'
#' @noRd
util_decorator_relevant_var_names <- function(env,
  variable_arg_names =
    unique(c(
      .variable_arg_roles$name,
      "variable_group"
    ))) {
  relevant_var_names <- unlist(
    mget(
      intersect(variable_arg_names, ls(envir = env)),
      envir = env,
      inherits = FALSE
    ),
    recursive = TRUE,
    use.names = FALSE
  )
  util_clean_relevant_var_names(relevant_var_names)
}

#' Internal helper: maybe eval to dataquieR result
#'
#' @noRd
util_maybe_eval_to_dataquieR_result <- function(my_call) {
  # detect recursion
  recursive_call <-
    sum(as.character(rlang::call_name(sys.call())) == vapply(sys.calls(),
      function(x) {
        r <-
          rlang::call_name(
            x
          )
        if (is.null(r)) {
          r <-
            NA_character_
        }
        return(r)
      },
      FUN.VALUE =
        character(1)
    )) > 1
  if (is.na(recursive_call)) {
    recursive_call <- FALSE
  }
  my_call <- substitute(my_call)
  if (.called_in_pipeline ||
      getOption("dataquieR.testdebug", dataquieR.testdebug_default) ||
      getOption("dataquieR.dontwrapresults", dataquieR.dontwrapresults_default) || # nolint: line_length_linter.
      util_testthat_blocks_interactive_wrapper() ||
      recursive_call) { # don't do, if in testthat runs, so that expect_condition still works. # nolint: line_length_linter.
    eval.parent(my_call)
  } else {
    call_to_attach <- as.symbol("<call not found>")
    if (util_is_try_error(try(call_to_attach <- rlang::call_match(
      rlang::caller_call(),
      rlang::caller_fn()
    ), silent = TRUE))) {
      util_warning(c(
        "Internal Warning, sorry. Please report: This will",
        "show a deprecation warning, but it should anyway",
        "not happen."
      ))
      try(call_to_attach <- rlang::call_match(my_call, rlang::call_fn(my_call)),
        silent = TRUE
      )
    }
    fn <- rlang::call_name(rlang::caller_call())
    .v <- try(eval(as.symbol("resp_vars"), envir = rlang::caller_env()),
      silent = TRUE
    )
    if (!util_is_try_error(.v) && !is.null(.v)) {
      nm <- paste0(.v, collapse = SPLIT_CHAR)
    } else {
      nm <- "[ALL]"
    }
    nm <- paste0(fn, ".", nm)
    r <- util_eval_to_dataquieR_result(my_call,
      nm = nm,
      function_name = fn,
      filter_result_slots = c(),
      env = parent.frame(),
      checkpoint_resumed = FALSE,
      called_in_pipeline = FALSE
    ) # dont handle stuff twice
    if (!util_is_try_error(.v)) {
      attr(call_to_attach, "entity_name") <- .v
    }
    .v <- try(eval(as.symbol("label_col"), envir = rlang::caller_env()),
      silent = TRUE
    )
    if (!util_is_try_error(.v)) {
      attr(call_to_attach, "label_col") <- .v
    }
    attr(r, "call") <- call_to_attach
    attr(r, "function_name") <- fn
    r
  }
}

preps <- quote({
  # maybe fix order ####
  ....alt_call_res <- NULL
  ..cl <- util_first_arg_study_data_or_resp_vars()
  if (is.call(..cl)) {
    # Use util_warning() or util_message() locally to inspect redirected calls.
    ....alt_call_res <- force(eval(..cl, rlang::caller_env(3)))
    return(....alt_call_res)
  } else {
    rm(..cl)
  }
  #######
  if (util_should_run_decorator_preps()) {
    my_call <- rlang::caller_call(2)
    ..explicit_call_args <- names(rlang::call_args(my_call))
    if ("meta_data_v2" %in% names(formals()) &&
        !is.null(my_call$meta_data_v2)) {
      ..meta_data_v2_value <- try(meta_data_v2, silent = TRUE)
      if (util_is_try_error(..meta_data_v2_value)) {
        util_error(
          "Could not evaluate argument %s: %s",
          sQuote("meta_data_v2"),
          conditionMessage(util_condition_from_try_error(..meta_data_v2_value)),
          applicability_problem = TRUE
        )
      }
      meta_data_v2 <- ..meta_data_v2_value
      util_maybe_load_meta_data_v2()
      meta_data_v2 <- rlang::missing_arg()
      my_call$meta_data_v2 <- NULL
    }
    ..call_arg_names <- rlang::call_args_names(my_call)
    ..meta_data_in_call <- "meta_data" %in% ..call_arg_names
    ..item_level_in_call <- "item_level" %in% ..call_arg_names
    ..dataquieR_meta_data_in_call <- ..meta_data_in_call
    ..dataquieR_item_level_in_call <- ..item_level_in_call
    env_args <- formals(util_meta_data_env)
    ..what_i_have <- intersect(names(env_args), ls())
    missing_env_args <- vapply(..what_i_have,
      FUN.VALUE = logical(1),
      function(object) {
        .m <- call("missing", object)
        r <- try(eval(.m, envir = rlang::caller_env(2)), silent = TRUE)
        return(identical(r, TRUE))
      }
    )
    ..what_i_have <- ..what_i_have[!missing_env_args]
    env_args[..what_i_have] <- mget(..what_i_have,
      inherits = FALSE
    )
    if ("meta_data" %in% names(formals()) && ..meta_data_in_call) {
      ..meta_data <- get("meta_data", inherits = FALSE)
      if (..item_level_in_call &&
          !identical(env_args$item_level, ..meta_data)) {
        util_error(
          "Please provide only one of %s and %s.",
          sQuote("item_level"),
          sQuote("meta_data")
        )
      }
      env_args$item_level <- ..meta_data
      env_args$.item_level_arg_name <- "meta_data"
      ..what_i_have <- union(..what_i_have, "item_level")
      rm(..meta_data)
    }
    ..metadata_aliases <- util_metadata_level_aliases()
    for (..canonical in intersect(
      unique(unname(..metadata_aliases)),
      names(env_args)
    )) {
      env_args <- util_apply_decorator_meta_data_alias(
        env_args,
        canonical = ..canonical,
        aliases = names(..metadata_aliases)[..metadata_aliases == ..canonical],
        call_arg_names = ..call_arg_names,
        env = environment()
      )
    }
    env_args <- util_apply_meta_data_cross_fallback(env_args, ..what_i_have)
    my_call <- rlang::call_match(my_call, rlang::current_fn())
    if ("meta_data" %in% names(formals()) &&
        ..item_level_in_call &&
        !..meta_data_in_call) {
      my_call$meta_data <- NULL
    }
    if ("item_level" %in% names(formals()) &&
        ..meta_data_in_call &&
        !..item_level_in_call) {
      my_call$item_level <- NULL
    }
    ..name_of_study_data <- "study_data"
    if (is.null(my_call$study_data) && "study_data" %in% names(formals())) {
      ..input_dir <- NULL
      if ("input_dir" %in% names(formals())) {
        ..input_dir <- try(if (missing(input_dir)) NULL else input_dir,
          silent = TRUE
        )
        if (util_is_try_error(..input_dir)) {
          ..input_dir <- NULL
        }
      } else if (exists("input_dir",
          envir = rlang::caller_env(3),
          inherits = FALSE
        )) {
        ..input_dir <- get("input_dir", envir = rlang::caller_env(3))
      }
      ..df_study_data <- util_find_study_data_from_dataframe_level(
        meta_data_dataframe = env_args$meta_data_dataframe,
        input_dir = ..input_dir
      )
      if (!is.null(..df_study_data)) {
        util_message(
          "Using %s from your dataframe level metadata",
          dQuote(..df_study_data$name_of_study_data)
        )
        .sd <- ..df_study_data$study_data
        ..name_of_study_data <- ..df_study_data$name_of_study_data
        env_args$study_data <- .sd
      } else if ("study_data" %in% prep_list_dataframes()) {
        util_message("Using %s from the dataframe cache.", dQuote("study_data"))
        .sd <- prep_get_data_frame("study_data")
        env_args$study_data <- .sd
      } else if (exists("study_data", envir = rlang::caller_env(3))) {
        util_message(
          "Using %s from your calling environment",
          sQuote("study_data")
        )
        .sd <- get("study_data", envir = rlang::caller_env(3))
        env_args$study_data <- .sd
      } else if (exists("study_data", envir = rlang::global_env())) {
        util_message(
          "Using %s from your global environment",
          sQuote("study_data")
        )
        .sd <- get("study_data", envir = rlang::global_env())
        env_args$study_data <- .sd
      } else {
        util_warning(
          c(
            "likely, the call will fail, because I could not find",
            "some %s, anywhere. Please pass them in your call or",
            "put them in your environment or data frame cache with",
            "the name %s.",
            "If this call was copied from a dataquieR report, note",
            "that the copied R call contains the indicator call only,",
            "not the study data object that was available when the",
            "report was created. Load the study data first, call %s,",
            "or pass %s explicitly."
          ),
          sQuote("study_data"),
          dQuote("study_data"),
          sQuote("prep_add_data_frames(study_data = <your data>)"),
          sQuote("study_data")
        )
      }
      rm(..input_dir, ..df_study_data)
    }
    ..study_data <- try(
      util_expect_data_frame(env_args$study_data,
        keep_types = TRUE,
        dont_assign = TRUE,
        arg_name = "study_data"
      ),
      silent = TRUE
    )
    ..item_level <- try(
      util_expect_data_frame(env_args$item_level,
        dont_assign = TRUE,
        arg_name = "meta_data"
      ),
      silent = TRUE
    )
    ..meta_data_item_computation <- try(
      util_expect_data_frame(
        env_args$meta_data_item_computation,
        dont_assign = TRUE,
        arg_name = env_args$.meta_data_item_computation_arg_name
      ),
      silent = TRUE
    )
    if (util_is_try_error(..meta_data_item_computation)) {
      ..meta_data_item_computation <- data.frame()
    }
    if (!util_is_try_error(..study_data) &&
        !util_is_try_error(..item_level)) {
      env_args$label_col <- util_resolve_decorator_label_col(
        label_col = env_args$label_col,
        what_i_have = ..what_i_have,
        item_level = ..item_level,
        env = rlang::caller_env(3)
      )
      if (!util_dataquieR_inputs_prepared(..study_data, ..item_level)) {
        ..prepared_inputs <- util_prepare_dataquieR_inputs(
          study_data = ..study_data,
          meta_data = ..item_level,
          label_col = env_args$label_col,
          meta_data_cross_item = env_args$meta_data_cross_item,
          meta_data_item_computation = ..meta_data_item_computation,
          name_of_study_data = ..name_of_study_data,
          update_registry = TRUE,
          relevant_var_names =
            util_decorator_relevant_var_names(environment())
        )
        env_args$study_data <- ..prepared_inputs$study_data
        env_args$item_level <- ..prepared_inputs$meta_data
        if ("study_data" %in% names(formals())) {
          study_data <- ..prepared_inputs$study_data
        }
        if ("item_level" %in% names(formals())) {
          item_level <- ..prepared_inputs$meta_data
        }
        if ("meta_data" %in% names(formals())) {
          meta_data <- ..prepared_inputs$meta_data
        }
        rm(..prepared_inputs)
      }
      if ("item_level" %in% names(formals())) {
        item_level <- env_args$item_level
      }
      if ("meta_data" %in% names(formals())) {
        meta_data <- env_args$item_level
      }
    }
    rm(..study_data, ..item_level, ..name_of_study_data)
    .mde <- do.call(util_meta_data_env, env_args, quote = FALSE)
    #
    my_call_orig <- my_call
    my_call <- try(
      .mde$provisionize_call(my_call,
        internal = TRUE,
        env = environment(),
        expand_ambiguous = TRUE
      ),
      silent = TRUE
    )
    if (!util_is_try_error(my_call) &&
        isTRUE(util_attr(my_call,
            "dataquieR_decorator_expanded",
            exact = TRUE
          ))) {
      ..expanded_calls <- my_call
      ..expanded_env <- environment()
      ..expanded_names <- names(..expanded_calls)
      if (is.null(..expanded_names)) {
        ..expanded_names <- rep("", length(..expanded_calls))
      }
      ..expanded_results <- lapply(seq_along(..expanded_calls), function(i) {
        ..single_call <- ..expanded_calls[[i]]
        ..single_call_name <- ..expanded_names[[i]]
        ..function_name <- rlang::call_name(..single_call)
        if (!nzchar(..single_call_name)) {
          ..single_call_name <- paste0(
            ..function_name,
            ".",
            util_deparse1(..single_call[[2]])
          )
        }
        ..result_name <- paste0(..function_name, ".", ..single_call_name)
        ..single_result <- util_eval_to_dataquieR_result(
          ..single_call,
          env = ..expanded_env,
          filter_result_slots = c(),
          nm = ..result_name,
          function_name = ..function_name,
          my_call = ..single_call,
          checkpoint_resumed = FALSE,
          called_in_pipeline = FALSE
        )
        class(..single_result) <- unique(c(
          class(..single_result),
          "master_result"
        ))
        ..single_result
      })
      names(..expanded_results) <- ..expanded_names
      class(..expanded_results) <- unique(c(
        "list",
        "master_result",
        class(..expanded_results)
      ))
      ....alt_call_res <- ..expanded_results
      return(....alt_call_res)
    }
    if (util_is_try_error(my_call)) {
      if (.called_in_pipeline ||
          getOption("dataquieR.testdebug", dataquieR.testdebug_default) ||
          getOption("dataquieR.dontwrapresults", dataquieR.dontwrapresults_default) || # nolint: line_length_linter.
          util_testthat_blocks_interactive_wrapper()) { # don't do, if in testthat runs, so that expect_condition still works. # nolint: line_length_linter.
        util_error(my_call)
      } else {
        function_name <- "dataquieR"
        nm <- paste0(function_name, ".?????")
        try(
          {
            my_call_condition <- util_attr(my_call, "condition", exact = TRUE)
            function_name <- rlang::call_name(my_call_condition$trace[[1]][[1]])
            fkt <- get(function_name,
              mode = "function"
            )
            rvs <- rlang::call_match(
              my_call_condition$trace[[1]][[1]],
              fkt
            )[["resp_vars"]]
            if (length(rvs) > 1) rvs <- "[ALL]"
            if (length(rvs) < 1) rvs <- "?"
            gvs <- rlang::call_match(
              my_call_condition$trace[[1]][[1]],
              fkt
            )[["group_vars"]]
            if (length(gvs) == 1) {
              nm <- paste0(function_name, "_", tolower(gvs), ".", rvs)
            } else {
              nm <- paste0(function_name, ".", rvs)
            }
          },
          silent = TRUE
        )
        .r <- util_eval_to_dataquieR_result(
          util_error(my_call),
          function_name = function_name,
          nm = nm
        )
        class(.r) <- unique(c(class(.r), "master_result"))
        ....alt_call_res <- .r
        return(....alt_call_res)
      }
    }

    for (n in names(my_call)) {
      if (n %in% names(formals())) {
        if (!is.language(my_call[[n]])) {
          .v <- try(my_call[[n]], silent = TRUE)
        } else {
          .v <- try(eval(as.symbol(n)), silent = TRUE)
        }
        if (util_is_try_error(.v) || is.null(.v)) {
          .v2 <- eval.parent(my_call[[n]], n = 2)
          if (!util_is_try_error(.v2)) {
            .v <- .v2
          }
        }
        assign(n, .v)
      }
    }
    ..decorator_meta_data <- NULL
    if (exists("meta_data", inherits = FALSE) &&
        is.data.frame(meta_data)) {
      ..decorator_meta_data <- meta_data
      ..decorator_meta_data_arg <- "meta_data"
    } else if (exists("item_level", inherits = FALSE) &&
        is.data.frame(item_level)) {
      ..decorator_meta_data <- item_level
      ..decorator_meta_data_arg <- "item_level"
    }
    if (is.data.frame(..decorator_meta_data)) {
      ..decorator_relevant_var_names <-
        util_decorator_relevant_var_names(environment())
      if (length(..decorator_relevant_var_names) > 0) {
        ..decorator_label_col <- if (exists("label_col", inherits = FALSE)) {
          label_col
        } else {
          VAR_NAMES
        }
        try(
          ..decorator_meta_data <- util_prepare_item_level_metadata(
            meta_data = ..decorator_meta_data,
            label_col = ..decorator_label_col
          ),
          silent = TRUE
        )
        ..decorator_relevant_meta_data <- util_prepare_relevant_meta_data(
          meta_data = ..decorator_meta_data,
          relevant_var_names = ..decorator_relevant_var_names,
          label_col = ..decorator_label_col
        )
        ..decorator_meta_data <- ..decorator_relevant_meta_data$meta_data
        assign(..decorator_meta_data_arg, ..decorator_meta_data)
      }
    }
  }
})

#' Internal helper: decorator
#'
#' @noRd
util_decorator <- function(x,
  f_nm = as.character(substitute(x))) {
  f <- util_funwrap(x, "util_maybe_eval_to_dataquieR_result")

  # reverse order (appends code)
  f <- util_funins(
    f,
    if (!is.null(....alt_call_res)) {
      return(....alt_call_res)
    }
  )
  # f <- util_funins(f,
  # )
  f <- util_funins(f, eval(preps))
  # /reverse order (appends code)
  f
}

for (fkt in ls(pattern = "^(int|com|con|acc|des)_")) {
  assign(fkt, util_decorator(get(fkt), fkt))
}

try(
  {
    if (suppressWarnings(util_ensure_suggested("pkgload",
          err = FALSE,
          goal =
            "provide help on report sections during development"
        ))) {
      dev_package <- pkgload::is_dev_package(utils::packageName())
    } else {
      dev_package <- FALSE
    }

    if (dev_package) {
      roxygenise_call <- 0
      called_from_rogygen <- FALSE
      while (!is.null(cl <- rlang::caller_call(roxygenise_call)) &&
          !called_from_rogygen) {
        ns <- rlang::call_ns(cl)
        nm <- rlang::call_name(cl)
        if (is.null(ns)) ns <- "" else ns <- paste0(ns, "::")
        if (paste0(ns, nm) == "roxygen2::roxygenise") {
          called_from_rogygen <- TRUE
        } else {
          roxygenise_call <- roxygenise_call + 1
        }
      }
      if (called_from_rogygen) {
        withr::defer(local({
          use_cli_format <-
            suppressWarnings(util_ensure_suggested("cli",
                err = FALSE,
                goal =
                  "nicer messages"
              ))
          if (use_cli_format) {
            cmd <- sprintf(
              "{.run [%s](%s)}", "dataquieR:::util_load_manual(TRUE)",
              "dataquieR:::util_load_manual(TRUE)"
            )
          } else {
            cmd <- "dataquieR:::util_load_manual(TRUE)"
          }
          need_rebuild <- FALSE
          pp <- dynGet("base_path", ifnotfound = getwd(), inherits = TRUE)
          withr::local_dir(pp)
          max_man <- max(vapply(
            list.files("man/",
              full.names = TRUE, all.files = TRUE,
              pattern = "*.Rd", ignore.case = TRUE
            ), file.mtime,
            FUN.VALUE = file.mtime("/")
          ), na.rm = TRUE)
          rd_files <- sort(dir("man", pattern = "\\.[Rr]d$", full.names = TRUE))
          md5 <- tools::md5sum(rd_files)
          man_hash <- rlang::hash(unname(paste(names(md5), md5, sep = "\n")))
          old_man_hash <- ""
          try(
            old_man_hash <- rio::import("inst/manual.RData",
              which = "man_hash",
              trust = TRUE
            ),
            silent = TRUE
          )
          if (old_man_hash == "" || old_man_hash != man_hash) {
            .update <- file.mtime("inst/manual.RData") < max_man
            .update <- .update || is.na(.update)
            if (.update) {
              rlang::inform(
                use_cli_format = use_cli_format,
                c(i = cli::format_inline(sprintf(
                  paste(
                    "inst/manual.RData may be out of date, as a dataquieR developer,", # nolint: line_length_linter.
                    "please, consider updating it with %s -- doing this, now"
                  ),
                  cmd
                )))
              )
              need_rebuild <- TRUE
            }
          }
          old_man_hash <- ""
          try(
            old_man_hash <- rio::import("inst/indicator_or_descriptor.RData",
              which = "man_hash",
              trust = TRUE
            ),
            silent = TRUE
          )
          if (old_man_hash == "" || old_man_hash != man_hash) {
            .update <- file.mtime("inst/indicator_or_descriptor.RData") < max_man # nolint: line_length_linter.
            .update <- .update || is.na(.update)
            if (.update) {
              rlang::inform(
                use_cli_format = use_cli_format,
                c(i = cli::format_inline(sprintf(paste(
                  "inst/indicator_or_descriptor.RData may be out of date, as a",
                  " dataquieR developer, please, consider updating it with",
                  "%s -- doing this, now"
                ), cmd)))
              )
              need_rebuild <- TRUE
            }
          }
          if (need_rebuild) {
            if (suppressWarnings(util_ensure_suggested("devtools",
                  err = FALSE,
                  goal =
                    "provide help on report sections during development"
                )) &&
                suppressWarnings(util_ensure_suggested("callr",
                    err = FALSE,
                    goal =
                      "provide help on report sections during development"
                  ))) {
              recursive_call <- Sys.getenv("dataquieR_child", "") == "TRUE"
              if (!recursive_call) {
                callr::r(
                  env =
                    c(dataquieR_child = "TRUE"),
                  function(man_hash, pp) {
                    withr::local_dir(pp)
                    devtools::document()
                    devtools::load_all()
                    util_fix_backticks()
                    util_load_manual(TRUE, man_hash = man_hash)
                  }, args = list(man_hash = man_hash, pp = pp)
                )
              }
            }
          } else {
            util_fix_backticks()
          }
        }), envir = rlang::caller_env(roxygenise_call), priority = "last")
      }
    }
  },
  silent = !TRUE
)

.git_hash <- try(suppressWarnings(suppressMessages(
  system("git rev-parse HEAD",
    intern = TRUE, ignore.stderr = TRUE
  )
)), silent = TRUE)

if (util_is_try_error(.git_hash) ||
    !is.character(.git_hash) ||
    length(.git_hash) != 1 ||
    !grepl("^[a-z0-9]{40}$", .git_hash)) {
  .git_hash <- ""
}

#' Internal helper: dataquieR version
#'
#' @noRd
util_dataquieR_version <- function() {
  gh <- .git_hash
  if (nzchar(gh)) {
    gh <- paste0(" (", gh, ")")
  }
  sprintf(
    "%s%s",
    as.character(packageVersion(
      utils::packageName()
    )),
    gh
  )
}

SSI_functions <-
  unlist(unique(lapply(strsplit(
    unlist(lapply(util_get_concept_info("ssi")$functions,
        util_parse_assignments,
        multi_variate_text = TRUE
      )),
    split = ".",
    fixed = TRUE
  ), `[[`, 1)))

# To debug with result wrapping disabled, set the dontwrapresults option around
# the indicator call.
