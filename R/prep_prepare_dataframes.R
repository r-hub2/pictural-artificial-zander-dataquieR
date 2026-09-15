#' Internal helper: raw study data mapped colnames
#'
#' @noRd
util_raw_study_data_mapped_colnames <- function(ds1, meta_data) {
  label_col <- util_attr(ds1, "label_col", exact = TRUE)
  mapped_colnames <- try(
    util_map_labels(
      colnames(ds1),
      meta_data = meta_data,
      from = label_col,
      to = VAR_NAMES,
      ifnotfound = colnames(ds1),
      warn_ambiguous = FALSE
    ),
    silent = TRUE
  )
  if (util_is_try_error(mapped_colnames)) {
    return(NULL)
  }
  unname(as.character(mapped_colnames))
}

#' Internal helper: raw study data attr matches
#'
#' @noRd
util_raw_study_data_attr_matches <- function(ds1, raw_study_data, meta_data) {
  if (!is.data.frame(ds1) || !is.data.frame(raw_study_data) ||
      !identical(dim(raw_study_data), dim(ds1)) ||
      !identical(row.names(raw_study_data), row.names(ds1))) {
    return(FALSE)
  }
  mapped_colnames <- util_raw_study_data_mapped_colnames(
    ds1 = ds1,
    meta_data = meta_data
  )
  if (is.null(mapped_colnames)) {
    return(FALSE)
  }
  identical(colnames(raw_study_data), mapped_colnames)
}

#' Internal helper: raw study data attr subset
#'
#' @noRd
util_raw_study_data_attr_subset <- function(ds1, raw_study_data, meta_data) {
  if (!is.data.frame(ds1) || !is.data.frame(raw_study_data) ||
      !identical(row.names(raw_study_data), row.names(ds1))) {
    return(NULL)
  }
  mapped_colnames <- util_raw_study_data_mapped_colnames(
    ds1 = ds1,
    meta_data = meta_data
  )
  if (is.null(mapped_colnames)) {
    return(NULL)
  }
  if (!all(mapped_colnames %in% colnames(raw_study_data))) {
    return(NULL)
  }
  raw_study_data[, mapped_colnames, drop = FALSE]
}

#' Internal helper: study data cache hash
#'
#' @noRd
util_study_data_cache_hash <- function(x) {
  provenance_attrs <- c(
    "dataquieR_data_frame_cache_name",
    "dataquieR_data_frame_source",
    "dataquieR_data_frame_keep_types",
    "dataquieR_data_frame_loaded_at"
  )

  normalize <- function(y, depth = 0L) {
    if (depth > 20L) {
      return("<dataquieR_cache_hash_depth_limit>")
    }
    if (is.environment(y) || is.function(y) || is.call(y)) {
      return(paste0("<", typeof(y), ">"))
    }
    if (inherits(y, "condition")) {
      return(list(
        condition_class = class(y),
        message = conditionMessage(y)
      ))
    }
    if (is.data.frame(y)) {
      for (att in provenance_attrs) {
        attr(y, att) <- NULL
      }
      atts <- attributes(y)
      atts$names <- NULL
      atts <- lapply(atts[sort(names(atts))], normalize, depth = depth + 1L)
      y <- list(
        attributes = atts,
        columns = stats::setNames(
          lapply(as.list(y), normalize, depth = depth + 1L),
          colnames(y)
        )
      )
    } else if (is.list(y)) {
      y <- lapply(y, normalize, depth = depth + 1L)
    }
    y
  }

  rlang::hash(normalize(x))
}

#' Internal helper: study data cache meta table refs
#'
#' @noRd
util_study_data_cache_meta_table_refs <- function(meta_data, meta_tab_hashes) {
  if (!length(meta_tab_hashes)) {
    return(meta_data)
  }

  table_cols <- endsWith(colnames(meta_data), "_TABLE")
  meta_data[table_cols] <- lapply(meta_data[table_cols], function(x) {
    res <- as.character(x)
    idx <- match(res, names(meta_tab_hashes))
    res[!is.na(idx)] <- unname(meta_tab_hashes[idx[!is.na(idx)]])
    res
  })
  meta_data
}

#' Internal helper: relevant var names from indicator call
#'
#' @noRd
util_relevant_var_names_from_indicator_call <- function(variable_arg_names =
    unique(c(
      .variable_arg_roles$name,
      "variable_group"
    ))) {
  relevant_var_names <- lapply(
    setNames(nm = variable_arg_names),
    util_find_indicator_function_in_callers
  )
  if (is.null(relevant_var_names$resp_vars)) {
    return(NULL)
  }
  util_clean_relevant_var_names(relevant_var_names)
}

#' Internal helper: prepare dataframes cache meta tabs hash
#'
#' @noRd
util_prepare_dataframes_cache_meta_tabs_hash <- function(meta_data) {
  .tabs <- sort(unique(unlist(meta_data[,
          endsWith(colnames(meta_data),
            "_TABLE"), drop = TRUE])))
  .tabs <- .tabs[!is.na(.tabs)]
  rlang::hash(lapply(.tabs,
    function(tb) {
      try(prep_get_data_frame(tb), silent = TRUE)
    }))
}

#' Internal helper: prepare dataframes cache hit
#'
#' @noRd
util_prepare_dataframes_cache_hit <- function(key, early = FALSE) {
  if (getOption("dataquieR.study_data_cache_metrics",
      dataquieR.study_data_cache_metrics_default)) {
    e <- getOption("dataquieR.study_data_cache_metrics_env",
      dataquieR.study_data_cache_metrics_env_default)
    if (!is.environment(e) ||
      rlang::env_is_locked(e) ||
      any(rlang::env_binding_are_locked(e, intersect(
        names(e),
        c("usage"))))) {
      util_warning(c("in `option()` %s, %s expects an unlocked",
          "environment with an unlocked binding in %s.
                         Use %s, instead."),
        sQuote("dataquieR.study_data_cache_metrics_env"),
        sQuote(packageName()),
        sQuote("usage"),
        sQuote("dataquieR.study_data_cache_metrics_env_default"))
      e <- dataquieR.study_data_cache_metrics_env_default
      e$usage <- list()
    }
    if (is.null(e$usage)) e$usage <- list()
    if (is.null(e$usage[[key]])) e$usage[[key]] <- 0
    e$usage[[key]] <- e$usage[[key]] + 1
    if (early) {
      if (is.null(e$early_usage)) e$early_usage <- list()
      if (is.null(e$early_usage[[key]])) e$early_usage[[key]] <- 0
      e$early_usage[[key]] <- e$early_usage[[key]] + 1
    }
  }
  ds1 <- .study_data_cache[[key]]
  util_as_prepared_data_frame(ds1)
}

#' Internal helper: prepare dataframes input cache key
#'
#' @noRd
util_prepare_dataframes_input_cache_key <- function(study_data,
  meta_data,
  label_col,
  relevant_var_names,
  .replace_hard_limits,
  .replace_missings,
  .sm_code,
  .allow_empty,
  .adjust_data_type,
  .amend_scale_level,
  .apply_factor_metadata,
  .apply_factor_metadata_inadm) {
  paste0("input@", rlang::hash(list(
    study_data = study_data,
    meta_data = meta_data,
    meta_tabs_hash = util_prepare_dataframes_cache_meta_tabs_hash(meta_data),
    label_col = label_col,
    relevant_var_names = relevant_var_names,
    switches = list(
      .replace_hard_limits = .replace_hard_limits,
      .replace_missings = .replace_missings,
      .sm_code = .sm_code,
      .allow_empty = .allow_empty,
      .adjust_data_type = .adjust_data_type,
      .amend_scale_level = .amend_scale_level,
      .apply_factor_metadata = .apply_factor_metadata,
      .apply_factor_metadata_inadm = .apply_factor_metadata_inadm
    ),
    options = list(
      dataquieR.study_data_colnames_case_sensitive =
        getOption("dataquieR.study_data_colnames_case_sensitive",
          dataquieR.study_data_colnames_case_sensitive_default),
      dataquieR.old_type_adjust =
        getOption("dataquieR.old_type_adjust",
          dataquieR.old_type_adjust_default)
    ),
    called_in_pipeline = .called_in_pipeline
  )))
}

#' Internal helper: prepare dataframes input cache safe
#'
#' @noRd
util_prepare_dataframes_input_cache_safe <- function(study_data,
  meta_data,
  .amend_scale_level) {
  if (!(DATA_TYPE %in% colnames(meta_data)) ||
      any(util_empty(meta_data[[DATA_TYPE]])) ||
      !all(meta_data[[DATA_TYPE]] %in% DATA_TYPES)) {
    return(FALSE)
  }

  if (.amend_scale_level &&
    (!(SCALE_LEVEL %in% colnames(meta_data)) ||
      !(VAR_NAMES %in% colnames(meta_data)) ||
      any(util_empty(meta_data[[SCALE_LEVEL]][
        meta_data[[VAR_NAMES]] %in% colnames(study_data)
      ])))) {
    return(FALSE)
  }

  TRUE
}

#' Prepare and verify study data with metadata
#'
#' @description
#' This function ensures, that a data frame `ds1` with suitable variable
#' names study_data and meta_data exist as base [data.frame]s.
#'
#' @details
#'
#' This function defines `ds1` and modifies `study_data` and `meta_data` in the
#' environment of its caller (see [eval.parent]). It also defines or modifies
#' the object `label_col` in the calling environment. Almost all functions
#' exported by `dataquieR` call this function initially, so that aspects common
#' to all functions live here, e.g. testing, if an argument `meta_data` has been
#' given and features really a [data.frame]. It verifies the existence of
#' required metadata attributes ([VARATT_REQUIRE_LEVELS]). It can also replace
#' missing codes by `NA`s, and calls [prep_study2meta] to generate a minimum
#' set of metadata from the study data on the fly (should be amended, so
#' on-the-fly-calling is not recommended for an instructive use of `dataquieR`).
#'
#' The function also detects `tibbles`, which are then converted to base-R
#' [data.frame]s, which are expected by `dataquieR`.
#'
#' If `.internal` is `TRUE`, differently from the other utility function that
#' work in their caller's environment, this function modifies objects in the
#' calling function's environment. It defines a new object `ds1`,
#' it modifies `study_data` and/or `meta_data`
#' and `label_col`.
#'
#' @param .study_data if provided, use this data set as study_data
#' @param .meta_data if provided, use this data set as meta_data
#' @param .label_col if provided, use this as label_col
#' @param .replace_hard_limits replace `HARD_LIMIT` violations by `NA`,
#'                             defaults to `FALSE`.
#' @param .replace_missings replace missing codes, defaults to `TRUE`
#' @param .sm_code missing code for `NAs`, if they have been
#'                 re-coded by `util_combine_missing_lists`
#' @param .allow_empty allow `ds1` to be empty, i.e., 0 rows and/or 0 columns
#' @param .adjust_data_type ensure that the data type of variables in the study
#'                data corresponds to their data type specified in the metadata
#' @param .amend_scale_level ensure that `SCALE_LEVEL` is available in the
#'                           item-level `meta_data`. internally used to prevent
#'                           recursion, if called from
#'                           [prep_scalelevel_from_data_and_metadata()].
#' @param .internal [logical] internally called, modify caller's environment.
#' @param .apply_factor_metadata  [logical] convert categorical variables to
#'                                          labeled factors.
#' @param .apply_factor_metadata_inadm  [logical] convert categorical variables
#'                                          to labeled factors keeping
#'                                          inadmissible values. Implies, that
#'                                          .apply_factor_metadata will be set
#'                                          to `TRUE`, too.
#'
#' @seealso acc_margins
#'
#' @return `ds1` the study data with mapped column names, `invisible()`, if
#'         not `.internal`
#'
#' @examples
#' \dontrun{
#' acc_test1 <- function(resp_variable, aux_variable,
#'                       time_variable, co_variables,
#'                       group_vars, study_data, meta_data) {
#'   prep_prepare_dataframes()
#'   invisible(ds1)
#' }
#' acc_test2 <- function(resp_variable, aux_variable,
#'                       time_variable, co_variables,
#'                       group_vars, study_data, meta_data, label_col) {
#'   ds1 <- prep_prepare_dataframes(study_data, meta_data)
#'   invisible(ds1)
#' }
#' environment(acc_test1) <- asNamespace("dataquieR")
#' # perform this inside the package (not needed for functions that have been
#' # integrated with the package already)
#'
#' environment(acc_test2) <- asNamespace("dataquieR")
#' # perform this inside the package (not needed for functions that have been
#' # integrated with the package already)
#' acc_test3 <- function(resp_variable, aux_variable, time_variable,
#'                       co_variables, group_vars, study_data, meta_data,
#'                       label_col) {
#'   prep_prepare_dataframes()
#'   invisible(ds1)
#' }
#' acc_test4 <- function(resp_variable, aux_variable, time_variable,
#'                       co_variables, group_vars, study_data, meta_data,
#'                       label_col) {
#'   ds1 <- prep_prepare_dataframes(study_data, meta_data)
#'   invisible(ds1)
#' }
#' environment(acc_test3) <- asNamespace("dataquieR")
#' # perform this inside the package (not needed for functions that have been
#' # integrated with the package already)
#'
#' environment(acc_test4) <- asNamespace("dataquieR")
#' # perform this inside the package (not needed for functions that have been
#' # integrated with the package already)
#' meta_data <- prep_get_data_frame("meta_data")
#' study_data <- prep_get_data_frame("study_data")
#' try(acc_test1())
#' try(acc_test2())
#' acc_test1(study_data = study_data)
#' try(acc_test1(meta_data = meta_data))
#' try(acc_test2(study_data = 12, meta_data = meta_data))
#' print(head(acc_test1(study_data = study_data, meta_data = meta_data)))
#' print(head(acc_test2(study_data = study_data, meta_data = meta_data)))
#' print(head(acc_test3(study_data = study_data, meta_data = meta_data)))
#' print(head(acc_test3(
#'   study_data = study_data, meta_data = meta_data,
#'   label_col = LABEL
#' )))
#' print(head(acc_test4(study_data = study_data, meta_data = meta_data)))
#' print(head(acc_test4(
#'   study_data = study_data, meta_data = meta_data,
#'   label_col = LABEL
#' )))
#' try(acc_test2(study_data = NULL, meta_data = meta_data))
#' }
#'
#' @export
#'
#' @importFrom rlang caller_fn
#' @importFrom utils object.size
prep_prepare_dataframes <- function(.study_data, .meta_data, .label_col,
  .replace_hard_limits,
  .replace_missings, .sm_code = NULL,
  .allow_empty = FALSE,
  .adjust_data_type = TRUE,
  .amend_scale_level = TRUE,
  .apply_factor_metadata = FALSE,
  .apply_factor_metadata_inadm = FALSE,
  .internal =
    rlang::env_inherits(
      rlang::caller_env(),
      parent.env(environment())
    )) {
  case_insens <- util_is_na_0_empty_or_false(
    getOption(
      "dataquieR.study_data_colnames_case_sensitive",
      dataquieR.study_data_colnames_case_sensitive_default
    )
  )

  # Historical caller-dimension extraction removed here.
  util_expect_scalar(.sm_code,
    check_type = util_all_is_integer,
    allow_null = TRUE
  )

  util_expect_scalar(.apply_factor_metadata, check_type = is.logical)
  util_expect_scalar(.apply_factor_metadata_inadm, check_type = is.logical)
  if (.apply_factor_metadata_inadm)
    .apply_factor_metadata <- TRUE

  if (missing(.replace_hard_limits)) .replace_hard_limits <- FALSE
  util_expect_scalar(.replace_hard_limits, check_type = is.logical)

  util_expect_scalar(.allow_empty, check_type = is.logical)
  if (!missing(.replace_missings) && (length(.replace_missings) != 1 ||
        !is.logical(.replace_missings) ||
        is.na(.replace_missings))) {
    util_error(
      c(
        "Internal error, sorry, please report: .replace_missings needs to",
        "be 1 logical value."
      )
    )
  }
  if (missing(.replace_missings)) .replace_missings <- TRUE


  callfn <- caller_fn(1)

  caller_defaults <- suppressWarnings(formals(callfn))
  caller_formals <- names(caller_defaults)
  caller_has_default <- suppressWarnings({
    !vapply(formals(callfn), identical, rlang::missing_arg(),
      FUN.VALUE = logical(1)
    )
  })

  missing_in_parent <- suppressWarnings({
    vapply(names(formals(callfn)), function(x) {
      eval(
        call(
          "missing",
          as.symbol(x)
        ),
        envir = parent.frame(3)
      )
    },
    FUN.VALUE = logical(1)
    )
  })

  if (missing(.label_col)) {
    if ("label_col" %in% caller_formals) {
      if (!eval.parent(substitute({
        missing(label_col)
      }))) {
        quoted_label_col <- eval.parent(substitute(substitute(label_col)))
        .label_col <- try(eval(quoted_label_col, envir = parent.frame()),
          silent = TRUE
        )
        if (inherits(.label_col, "try-error")) {
          .label_col <- try(
            eval(quoted_label_col,
              envir =
                dataquieR::WELL_KNOWN_META_VARIABLE_NAMES
            ),
            silent = TRUE
          )
        }
        if (inherits(.label_col, "try-error")) {
          util_error("Cannot resolve %s", dQuote(paste0(
            "label_col", " = ",
            quoted_label_col
          )),
          applicability_problem = TRUE
          )
        }
      } else if (caller_has_default[["label_col"]]) {
        .label_col <- eval(caller_defaults[["label_col"]],
          envir = asNamespace("dataquieR"), enclos =
            parent.frame(2)
        )
      } else {
        .label_col <- VAR_NAMES
      }
    } else if (exists("label_col", parent.frame())) {
      .label_col <- get("label_col", parent.frame())
    } else {
      .label_col <- VAR_NAMES
    }
  }

  # if no study_data have been provided -> error
  if (missing(.study_data)) {
    if ("study_data" %in% caller_formals) {
      if (!eval.parent(substitute({
        missing(study_data)
      }))) {
        if (exists("study_data", parent.frame())) {
          .study_data <- try(eval(quote(study_data), parent.frame()),
            silent = TRUE
          )
          if (inherits(.study_data, "try-error")) {
            cnd <- util_attr(.study_data, "condition", exact = TRUE)
            cnd$call <- sys.call(1)
            util_error(cnd)
          }
        } else {
          util_error("object %s not found", dQuote("study_data"))
        }
      } else if (caller_has_default[["study_data"]]) {
        .study_data <- try(eval.parent(caller_defaults[["study_data"]], n = 2),
          silent = TRUE
        )
        if (inherits(.study_data, "try-error")) {
          cnd <- util_attr(.study_data, "condition", exact = TRUE)
          cnd$call <- sys.call(1)
          util_error(cnd)
        }
      } else {
        .study_data <- NULL
      }
    } else if (exists("study_data", parent.frame())) {
      .study_data <- get("study_data", parent.frame())
    } else {
      .study_data <- NULL
    }
  }

  e <- new.env(parent = environment())
  e$study_data <- .study_data
  .study_data <-
    eval(
      quote(try(util_expect_data_frame(study_data, keep_types = TRUE),
          silent = TRUE
        )),
      e
    )
  if (inherits(.study_data, "try-error")) {
    util_error(
      "Need study data as a data frame: %s",
      conditionMessage(util_attr(.study_data, "condition",
          exact = TRUE
        ))
    )
  }
  .study_data <- util_normalize_time_only_columns(.study_data)

  item_level_in_call <- "item_level" %in% names(missing_in_parent) &&
    identical(missing_in_parent[["item_level"]], FALSE)
  meta_data_in_call <- "meta_data" %in% names(missing_in_parent) &&
    identical(missing_in_parent[["meta_data"]], FALSE)
  if (exists("..dataquieR_item_level_in_call", parent.frame(),
      inherits = FALSE
    )) {
    item_level_in_call <- isTRUE(get(
      "..dataquieR_item_level_in_call",
      parent.frame()
    ))
  }
  if (exists("..dataquieR_meta_data_in_call", parent.frame(),
      inherits = FALSE
    )) {
    meta_data_in_call <- isTRUE(get(
      "..dataquieR_meta_data_in_call",
      parent.frame()
    ))
  }

  if (.internal &&
      !exists("already_ppdf", parent.frame()) &&
      all(c("item_level", "meta_data") %in% caller_formals) &&
      item_level_in_call &&
      meta_data_in_call &&
      !identical(dynGet("item_level"), dynGet("meta_data"))
  ) {
    util_error(
      c(
        "You cannot provide both, %s as well as %s",
        "these arguments are synonyms and must be",
        "used mutually exclusively"
      ),
      sQuote("item_level"),
      sQuote("meta_data")
    )
    # see prep_get_labels
  }

  if (missing(.meta_data)) {
    if ("meta_data" %in% caller_formals &&
        (!isTRUE(missing_in_parent["meta_data"]) ||
            caller_has_default[["meta_data"]])) {
      if (isTRUE(missing_in_parent["meta_data"])) {
        .meta_data <- try(eval(caller_defaults[["meta_data"]], parent.frame(2)),
          silent = TRUE
        )
        if (util_is_try_error(.meta_data)) {
          .meta_data <- try(eval(caller_defaults[["meta_data"]], parent.frame(1)),
            silent = TRUE
          )
        }
      } else {
        .meta_data <- try(eval(quote(meta_data), parent.frame(1)),
          silent = TRUE
        )
      }
      if (inherits(.meta_data, "try-error")) {
        cnd <- util_attr(.meta_data, "condition", exact = TRUE)
        cnd$call <- sys.call(1)
        util_error(cnd)
      }
    }
  }


  if (missing(.meta_data)) {
    if ("item_level" %in% caller_formals &&
        (!isTRUE(missing_in_parent["item_level"]) ||
            caller_has_default[["item_level"]])) {
      if (isTRUE(missing_in_parent["item_level"])) {
        .meta_data <- try(eval(caller_defaults[["item_level"]], parent.frame(2)),
          silent = TRUE
        )
        if (util_is_try_error(.meta_data)) {
          .meta_data <- try(eval(caller_defaults[["item_level"]], parent.frame(1)),
            silent = TRUE
          )
        }
      } else {
        .meta_data <- try(eval(quote(item_level), parent.frame(1)),
          silent = TRUE
        )
      }
      if (inherits(.meta_data, "try-error")) {
        cnd <- util_attr(.meta_data, "condition", exact = TRUE)
        cnd$call <- sys.call(1)
        util_error(cnd)
      }
    }
  }
  if (missing(.meta_data)) {
    if (exists("item_level", envir = parent.frame()) &&
        !("item_level" %in% caller_formals)) {
      .meta_data <- get("item_level", envir = parent.frame())
    } else if (exists("meta_data", envir = parent.frame()) &&
        !("meta_data" %in% caller_formals)) {
      .meta_data <- get("meta_data", envir = parent.frame())
    }
  }
  if (missing(.meta_data)) {
    if ("item_level" %in% prep_list_dataframes()) {
      .meta_data <- util_expect_data_frame("item_level")
    }
  }
  if (!missing(.meta_data) && !is.data.frame(.meta_data)) {
    if (util_is_try_error(try(util_expect_data_frame(.meta_data, keep_types = FALSE), # nolint: line_length_linter.
          silent = TRUE
        ))) {
      .meta_data <- rlang::missing_arg()
      # util_error("Need metadata as a data frame", applicability_problem =
      # TRUE)
    }
  }
  if (missing(.meta_data)) {
    w <- paste(
      "Missing %s, try to guess a preliminary one from the data",
      "using %s. Please consider amending this minimum guess manually."
    )
    if (requireNamespace("cli", quietly = TRUE)) {
      w <- cli::bg_red(cli::col_br_yellow(w))
    }

    util_warning(
      w,
      dQuote("meta_data"),
      dQuote("prep_study2meta"),
      applicability_problem = TRUE, immediate = TRUE
    )
    .meta_data <- prep_study2meta(.study_data,
      level =
        VARATT_REQUIRE_LEVELS$REQUIRED
    ) # recommended would include part vars, which causes many warnings
  } else if (!is.data.frame(.meta_data)) {
    util_error("Need metadata as a data frame", applicability_problem = TRUE)
  }

  # if no meta_data have been provided -> error
  e <- new.env(parent = environment())
  e$meta_data <- .meta_data
  .meta_data <-
    eval(
      quote(try(util_expect_data_frame(meta_data), silent = TRUE)),
      e
    )

  if (inherits(.meta_data, "try-error")) {
    util_error(
      "Need metadata as a data frame: %s",
      conditionMessage(util_attr(.meta_data, "condition",
          exact = TRUE
        ))
    )
  }

  util_expect_data_frame(.meta_data)

  if (!prod(dim(.meta_data))) {
    util_warning(
      c(
        "Missing %s, try to guess a preliminary one from the data using %s.",
        "Please consider amending this minimum guess manually."
      ),
      dQuote("meta_data"),
      dQuote("prep_prepare_dataframes"),
      applicability_problem = TRUE
    )
    .meta_data <- prep_study2meta(.study_data,
      level =
        VARATT_REQUIRE_LEVELS$REQUIRED
    )
  }

  if (.internal) {
    assign("already_ppdf", TRUE, envir = parent.frame())
  }

  if (is.null(.label_col)) {
    .label_col <- VAR_NAMES
  }

  study_data <- .study_data
  meta_data <- .meta_data
  label_col <- .label_col

  if (any(is.na(colnames(study_data)))) {
    util_error("%s must not feature columns with columns names being %s",
      sQuote("study_data"),
      sQuote("NA"),
      applicability_problem = TRUE,
      intrinsic_applicability_problem = TRUE
    )
  }

  if (case_insens) {
    colnames(study_data) <-
      util_align_colnames_case(
        .colnames = colnames(study_data),
        .var_names = meta_data[[VAR_NAMES]]
      )
  }

  # Exchanged to "label_col"
  if (!exists("label_col")) {
    label_col <- VAR_NAMES
  }

  try(if (missing(label_col)) {
    label_col <- VAR_NAMES
  }, silent = TRUE)

  util_expect_scalar(label_col,
    check_type = is.character,
    error_message =
      sprintf(
        "%s needs to be of type character",
        sQuote("label_col")
      )
  )

  relevant_var_names <- util_relevant_var_names_from_indicator_call()
  if (is.null(relevant_var_names) || length(relevant_var_names) == 0) {
    relevant_var_names <- colnames(study_data)
  }

  input_cache_key <- NULL
  if (util_prepare_dataframes_input_cache_safe(
    study_data = study_data,
    meta_data = meta_data,
    .amend_scale_level = .amend_scale_level
  )) {
    input_cache_key <- util_prepare_dataframes_input_cache_key(
      study_data = study_data,
      meta_data = meta_data,
      label_col = label_col,
      relevant_var_names = relevant_var_names,
      .replace_hard_limits = .replace_hard_limits,
      .replace_missings = .replace_missings,
      .sm_code = .sm_code,
      .allow_empty = .allow_empty,
      .adjust_data_type = .adjust_data_type,
      .amend_scale_level = .amend_scale_level,
      .apply_factor_metadata = .apply_factor_metadata,
      .apply_factor_metadata_inadm = .apply_factor_metadata_inadm
    )
  }
  if (!is.null(input_cache_key) &&
      exists(input_cache_key, envir = .study_data_cache_input_keys,
        inherits = FALSE)) {
    key_from_input <- get(input_cache_key,
      envir = .study_data_cache_input_keys,
      inherits = FALSE)
    if (exists(key_from_input, envir = .study_data_cache, inherits = FALSE) &&
        (!.internal ||
            exists(key_from_input, envir = .study_data_cache_meta_data,
              inherits = FALSE))) {
      ds1 <- util_prepare_dataframes_cache_hit(key_from_input, early = TRUE)
      study_data <- util_attr(ds1, "study_data", exact = TRUE)
      if (.internal) {
        meta_data <- get(key_from_input,
          envir = .study_data_cache_meta_data,
          inherits = FALSE)
        assign("study_data", study_data, parent.frame())
        assign("meta_data", meta_data, parent.frame())
        assign("ds1", ds1, parent.frame())
        assign("label_col", label_col, parent.frame())
        return(invisible(ds1))
      } else {
        return(ds1)
      }
    }
  }

  # Fill empty labels before later code maps to metadata label columns.
  meta_data <- util_fill_empty_label_columns(meta_data, label_col = label_col)

  meta_data <- util_prepare_item_level_metadata(
    meta_data = meta_data,
    label_col = label_col
  )

  if (!(DATA_TYPE %in% colnames(meta_data)) ||
      any(util_empty(meta_data[[DATA_TYPE]])) ||
      !all(meta_data[[DATA_TYPE]] %in% DATA_TYPES)) {
    if (!(DATA_TYPE %in% colnames(meta_data))) {
      meta_data[[DATA_TYPE]] <- NA_character_
    }

    meta_data <- .util_fix_data_types(meta_data, study_data)
  }

  relevant_meta_data <- util_prepare_relevant_meta_data(
    meta_data = meta_data,
    relevant_var_names = relevant_var_names,
    label_col = label_col
  )
  meta_data <- relevant_meta_data$meta_data
  relevant_var_names <- relevant_meta_data$relevant_var_names

  if (!.called_in_pipeline) {
    meta_data <- util_validate_known_meta(
      meta_data,
      relevant_var_names = relevant_var_names
    )
  }

  # Build the cache key from the input basis, not from a mapped display variant.
  # A prepared/mapped data frame can carry the original study data in
  # attr(., "study_data"). If that raw attribute is still aligned with the
  # visible data, using it keeps calls with raw data and calls with a matching
  # prepared data frame on the same cache key. If the attribute looks stale, we
  # deliberately fall back to the visible data frame to avoid cache collisions.
  .study_data_for_cache <- study_data
  if (isTRUE(util_attr(study_data, "MAPPED", exact = TRUE))) {
    .raw_study_data_for_cache <- util_attr(study_data, "study_data", exact = TRUE)
    if (util_raw_study_data_attr_matches(
      ds1 = study_data,
      raw_study_data = .raw_study_data_for_cache,
      meta_data = meta_data
    )) {
      .study_data_for_cache <- .raw_study_data_for_cache
    }
  }
  .tabs <- sort(unique(unlist(meta_data[
    ,
    endsWith(
      colnames(
        meta_data
      ),
      "_TABLE"
    )
    , drop = TRUE])))
  .tabs <- .tabs[!is.na(.tabs)]
  meta_tab_hashes <- vapply(.tabs,
    function(tb) {
      meta_tab <- tryCatch(
        prep_get_data_frame(tb),
        error = function(e) {
          list(
            dataquieR_cache_hash_error = TRUE,
            message = conditionMessage(e)
          )
        }
      )
      util_study_data_cache_hash(meta_tab)
    },
    FUN.VALUE = character(1)
  )
  study_data_hash <- util_study_data_cache_hash(.study_data_for_cache)
  meta_data_hash <- util_study_data_cache_hash(
    util_study_data_cache_meta_table_refs(
      meta_data = meta_data,
      meta_tab_hashes = meta_tab_hashes
    )
  )
  meta_tabs_hash <- rlang::hash(sort(unique(unname(meta_tab_hashes))))
  a <- formals(prep_prepare_dataframes)[.to_combine]
  a2 <- rlang::call_args(sys.call())
  a2 <- a2[intersect(names(a2), .to_combine)]
  a[names(a2)] <- a2
  missing_from_a <- vapply(a, rlang::is_missing, FUN.VALUE = logical(1))
  a[missing_from_a] <- mget(names(which(missing_from_a)))
  a <- rlang::hash(a)
  key <-
    paste0(
      a,
      "@", study_data_hash,
      "@", meta_data_hash,
      "@", meta_tabs_hash,
      "@", label_col,
      "@", getOption(
        "dataquieR.old_type_adjust",
        dataquieR.old_type_adjust_default
      )
    )

  if (key %in% names(.study_data_cache)) {
    ds1 <- util_prepare_dataframes_cache_hit(key)
    study_data <- util_attr(ds1, "study_data", exact = TRUE)
  } else {
    ds1 <- NULL
  }

  # If study_data exist and metadata were already mapped, then we can return ds1
  # directly, but only if all requested modifications are already considered
  # (if possible).
  if (isTRUE(util_attr(study_data, "MAPPED", exact = TRUE))) { # here, we known, study_data was a ds1, so it features a study_data already with original study data
    ds1 <- study_data # study_data is actually a ds1 already, that may be used
    raw_study_data <- util_attr(ds1, "study_data", exact = TRUE)
    raw_study_data_subset <- util_raw_study_data_attr_subset(
      ds1 = ds1,
      raw_study_data = raw_study_data,
      meta_data = meta_data
    )
    has_matching_raw_study_data <- is.data.frame(raw_study_data_subset)
    study_data <- if (has_matching_raw_study_data) {
      raw_study_data_subset # get the matching original study_data w/o any attributes (XXX) # nolint: line_length_linter.
    } else {
      ds1
    }
    # labels can be modified easily
    ds1_label_col <- util_attr(ds1, "label_col", exact = TRUE)
    if (!identical(ds1_label_col, label_col)) {
      colnames(ds1) <-
        util_map_labels(colnames(ds1),
          meta_data = meta_data,
          from = ds1_label_col,
          to = label_col
        )
      attr(ds1, "label_col") <- label_col
    }
    ds1_ready <- TRUE

    # find out, if ds1 really can be re-used

    # replacement of missing value codes can not be undone
    if (isTRUE(util_attr(ds1, "Codes_to_NA", exact = TRUE)) &&
        !.replace_missings) {
      ds1_ready <- FALSE
    } else if (isTRUE(util_attr(ds1, "Codes_to_NA", exact = TRUE)) !=
        .replace_missings) {
      ds1_ready <- FALSE
    }
    # replacement of hard limits can not be undone
    if (isTRUE(util_attr(ds1, "HL_viol_to_NA", exact = TRUE)) &&
        !.replace_hard_limits) {
      ds1_ready <- FALSE
    } else if (isTRUE(util_attr(ds1, "HL_viol_to_NA", exact = TRUE)) !=
        .replace_hard_limits) {
      ds1_ready <- FALSE
    }
    # replacement of hard limits can not be undone, also, it may prevent hard
    # limit replacement, e.g.
    if (isTRUE(util_attr(ds1, "apply_fact_md", exact = TRUE))) {
      ds1_ready <- FALSE
    }
    if (isTRUE(util_attr(ds1, "apply_fact_md_inadm", exact = TRUE))) {
      ds1_ready <- FALSE
    }
    # data type correction can not be undone
    if (isTRUE(util_attr(ds1, "Data_type_matches", exact = TRUE)) &&
        !.adjust_data_type) {
      ds1_ready <- FALSE
    } else if (isTRUE(util_attr(ds1, "Data_type_matches", exact = TRUE)) !=
        .adjust_data_type) {
      ds1_ready <- FALSE
    }
    check_sl <-
      (.amend_scale_level && SCALE_LEVEL %in% colnames(meta_data) &&
        !any(util_empty(meta_data[[SCALE_LEVEL]][meta_data[[VAR_NAMES]] %in%
                colnames(study_data)]))) ||
      !.amend_scale_level

    ds1_ready <- ds1_ready &&
      check_sl

    if (ds1_ready) { # ds1 can be re-used
      ds1 <- util_as_prepared_data_frame(ds1)
      attr(ds1, "dataquieR_preparation_signature") <- key
      if (.internal) {
        assign("study_data", study_data, parent.frame())
        assign("meta_data", meta_data, parent.frame())
        assign("ds1", ds1, parent.frame())
        assign("label_col", label_col, parent.frame())
        return(invisible(ds1))
      } else {
        return(ds1)
      }
    } else if (!has_matching_raw_study_data) {
      util_error(
        c(
          "Cannot rebuild %s from an already prepared data frame,",
          "because its %s attribute is missing or stale. This can",
          "happen after subsetting or similar operations. Please pass",
          "an unprepared data frame or remove dataquieR attributes.",
          "If you've passed a plain data file, there may be an",
          "internal processing error, so sorry and please report.",
          "If you've passed some data frame that came out of",
          "prep_prepare_dataframes(), maybe strip its attributes or",
          "use the original data file. Maybe, if you have reduced",
          "the data filtering something, you should do so before",
          "calling prep_prepare_dataframes() on it."
        ),
        sQuote("study_data"),
        sQuote("study_data")
      )
    } else { # need to create a valid ds1, now
      ds1 <- NULL
    }
  }

  # study_data does not feature ds1- attributes any more and ds1 needs to be
  # built.

  if (.replace_missings) {
    # Are missing codes replaced?
    if (!isTRUE(util_attr(study_data, "Codes_to_NA", exact = TRUE))) { # always true, here, because study_data is now not mapped
      study_data <-
        util_replace_codes_by_NA(
          study_data = study_data, meta_data = meta_data,
          split_char = SPLIT_CHAR, sm_code = .sm_code
        )
    }
  }

  if (!"VAR_NAMES" %in% colnames(meta_data)) {
    # Should get caught before by 'util_validate_known_meta'.
    util_error("'VAR_NAMES' not found in metadata [%s]",
      paste0(colnames(meta_data), collapse = ", "),
      applicability_problem = TRUE
    )
  }

  if (!isTRUE(util_attr(study_data, "MAPPED", exact = TRUE))) { # always true, here, because study_data is now not mapped
    study_data <- study_data[, order(colnames(study_data)), drop = FALSE]
    meta_data <- meta_data[order(meta_data[[VAR_NAMES]]), , drop = FALSE]
  }

  relevant_vars_for_warnings <- NULL

  # adjust data types, if enabled
  util_stop_if_not(
    `Pipeline should never request study data w/ unchanged datatypes, sorry, internal error, please report` = # nolint: line_length_linter.
      !(!.adjust_data_type && .called_in_pipeline)
  )
  if (.adjust_data_type || .apply_factor_metadata) {
    relevant_vars_for_warnings <- relevant_var_names
  }
  if (.adjust_data_type && !.called_in_pipeline) {
    study_data <- util_adjust_data_type(
      study_data = study_data,
      meta_data = meta_data,
      relevant_vars_for_warnings = relevant_vars_for_warnings
    )
  }

  if (.amend_scale_level && (!(SCALE_LEVEL %in% colnames(meta_data)) ||
    any(util_empty(
      meta_data[[SCALE_LEVEL]][meta_data[[VAR_NAMES]] %in%
          colnames(study_data)]
    )))) {
    util_message(
      c(
        "Missing some or all entries in %s column in item-level %s. Predicting",
        "it from the data -- please verify these predictions, they",
        "may be wrong and lead to functions claiming not to be",
        "reasonably applicable to a variable."
      ),
      sQuote(SCALE_LEVEL), "meta_data",
      applicability_problem = TRUE,
      intrinsic_applicability_problem = FALSE
    )

    meta_data <- prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data,
      label_col = label_col
    )
    meta_data <- util_prepare_item_level_metadata(
      meta_data = meta_data,
      label_col = label_col
    )
  }

  # create ds1 -----------------------------------------------------------------
  if (is.null(ds1) || !isTRUE(util_attr(ds1, "MAPPED", exact = TRUE))) {
    ds1 <- util_map_all(
      label_col = label_col, study_data = study_data,
      meta_data = meta_data
    )$df
  }
  if (!(.allow_empty) && ncol(ds1) * nrow(ds1) == 0) {
    util_error(
      "No data left. Aborting, since mapping of %s on %s was not possible",
      sQuote("meta_data"),
      sQuote("study_data"),
      applicability_problem = FALSE
    )
  }

  my_atts <- attributes(ds1)[.ds1_attribute_names]
  ds1 <- as.data.frame(ds1)
  attributes(ds1)[.ds1_attribute_names] <- my_atts

  meta_data <- as.data.frame(meta_data)

  .all <- ncol(ds1)
  my_atts <- attributes(ds1)[.ds1_attribute_names]
  ds1 <- ds1[, colnames(ds1) %in% meta_data[[label_col]], drop = FALSE]
  attributes(ds1)[.ds1_attribute_names] <- my_atts
  .mapped <- ncol(ds1)
  if (.all > .mapped) { # nocov start
    # Should be dead code, because util_map_all is called above, which
    # performs an analogous check and clears out the unannotated variables
    # from the study data.
    util_warning("Lost %d variables, that I could not map using %s",
      .all - .mapped, dQuote(label_col),
      applicability_problem = TRUE
    )
  } # nocov end
  if (!.allow_empty) {
    # this would delete the metadata, if the mapping has failed totally
    meta_data <- meta_data[meta_data[[label_col]] %in% colnames(ds1), , drop = FALSE] # nolint: line_length_linter.
  }

  if (STUDY_SEGMENT %in% colnames(meta_data) &&
      any(util_empty(meta_data[[STUDY_SEGMENT]]))) {
    dummy_name <- "SEGMENT"
    i <- 1
    while (dummy_name %in% meta_data[[STUDY_SEGMENT]]) {
      dummy_name <- sprintf("SEGMENT %d", i)
      i <- i + 1
    }
    util_message(
      c(
        "Some %s are NA. Will assign those to an artificial",
        "segment %s"
      ), sQuote(STUDY_SEGMENT), dQuote(dummy_name),
      applicability_problem = TRUE
    )
    meta_data[[STUDY_SEGMENT]][util_empty(meta_data[[STUDY_SEGMENT]])] <-
      dummy_name
  }

  if (VARIABLE_ORDER %in% colnames(meta_data)) {
    meta_data[] <- meta_data[order(meta_data[[VARIABLE_ORDER]]), , drop = FALSE]
    vars <- meta_data[, label_col, drop = TRUE]
    vars <- vars[vars %in% colnames(ds1)]
    my_atts <- attributes(ds1)[.ds1_attribute_names]
    ds1 <- ds1[, vars, drop = FALSE]
    attributes(ds1)[.ds1_attribute_names] <- my_atts
  }

  if (!isTRUE(util_attr(study_data, "MAPPED", exact = TRUE)) && # !mapped is always true, here, because study_data is now not mapped
      PART_VAR %in% colnames(meta_data)) {
    local({
      .kssvs <- intersect(colnames(study_data), meta_data[, PART_VAR, drop = TRUE]) # nolint: line_length_linter.
      for (rv in .kssvs) {
        vals <- util_replace_codes_by_NA(
          study_data = study_data[, rv, drop = FALSE],
          meta_data = meta_data, sm_code = .sm_code
        )[[rv]]
        vals <- vals[!is.na(vals)]
        if (is.character(vals)) vals <- vals[trimws(vals) != ""]
        vals <- suppressWarnings(as.numeric(vals))
        if (!all(vals %in% c(0:1, NA), na.rm = TRUE)) {
          util_warning(
            c(
              "Found entries different from TRUE/FALSE/1/0 and <empty>, segment", # nolint: line_length_linter.
              "participation is expected, if values different from 0 are found.", # nolint: line_length_linter.
              "For the segment indicator variable %s, a table of inadmissible",
              "values: %s"
            ),
            dQuote(rv),
            paste(capture.output(print(table(setdiff(vals, c(0:1, NA))))),
              collapse = "\n"
            )
          )
        }
      }
    })
  }

  attr(study_data, "Codes_to_NA") <- .replace_missings
  attr(ds1, "Codes_to_NA") <- .replace_missings

  attr(ds1, "MAPPED") <- TRUE
  attr(ds1, "label_col") <- label_col
  attr(ds1, "Data_type_matches") <- .adjust_data_type

  if (.replace_hard_limits) {
    # Are hard limit violations replaced?
    if (!isTRUE(util_attr(ds1, "HL_viol_to_NA", exact = TRUE))) {
      ds1 <-
        util_replace_hard_limit_violations(
          study_data = ds1, meta_data = meta_data, label_col = label_col
        )
    }
  }

  attr(ds1, "HL_viol_to_NA") <- .replace_hard_limits

  if (.apply_factor_metadata) {
    relevant_vars_for_warnings_lb <-
      util_map_labels(relevant_vars_for_warnings,
        meta_data = meta_data,
        from = VAR_NAMES, to = label_col,
        relevant_vars_for_warnings
      )

    # convert columns to factors?
    if (!isTRUE(util_attr(ds1, "apply_fact_md", exact = TRUE))) { # !apply_fact_md is always true, here, because study_data is now not mapped
      ds1_label_col <- util_attr(ds1, "label_col", exact = TRUE)
      which_cols <- intersect(
        meta_data[[ds1_label_col]],
        colnames(ds1)
      )
      ds1[, which_cols] <- lapply(
        which_cols,
        function(cl) {
          # Use prep_load_workbook_like_file() locally to inspect metadata v2.
          if (!(
            SCALE_LEVEL %in% colnames(meta_data))) {
            util_error(
              c(
                "%s was called with %s = %s",
                "and %s = %s",
                "but %s does not provide a",
                "column %s -- please provide",
                "this column or call with ",
                "%s = %s"
              ),
              sQuote(rlang::call_name(sys.call(1))),
              sQuote(".apply_factor_metadata"),
              dQuote(.apply_factor_metadata),
              sQuote(".amend_scale_level"),
              dQuote(.amend_scale_level),
              "meta_data",
              sQuote(SCALE_LEVEL),
              sQuote(".amend_scale_level"),
              dQuote(TRUE),
              applicability_problem = TRUE
            )
          }
          sl <- meta_data[meta_data[[ds1_label_col]] == cl,
            SCALE_LEVEL,
            drop = TRUE
          ]
          if (sl %in% c(
            SCALE_LEVELS$NOMINAL, SCALE_LEVELS$ORDINAL
          )) {
            util_stop_if_not(
              `Internal error, sorry, please report: unexp. VALUE_LABELS` =
                util_empty(meta_data[
                  meta_data[[ds1_label_col]] == cl,
                  VALUE_LABELS,
                  drop = TRUE
                ])
            )
            vlt <- try(prep_get_data_frame(
              meta_data[meta_data[[ds1_label_col]] == cl,
                VALUE_LABEL_TABLE,
                drop = TRUE
              ]
            ), silent = TRUE)
            levels <- NULL
            labels <- NULL
            if (!util_is_try_error(vlt)) {
              if (CODE_VALUE %in% colnames(vlt)) {
                levels <- vlt[[CODE_VALUE]]
              }
              if (CODE_LABEL %in% colnames(vlt)) {
                labels <- vlt[[CODE_LABEL]]
              }
              if (is.null(levels) && !is.null(labels)) {
                util_error(
                  applicability_problem = TRUE,
                  "Have only code labels, but not code levels for %s",
                  dQuote(cl)
                )
              } else if (is.null(labels)) {
                labels <- levels
              }
            }
            ordered <- sl == SCALE_LEVELS$ORDINAL
            if (is.null(levels)) {
              levels <- unique(sort(ds1[[cl]]))
            }
            if (is.null(labels)) {
              labels <- levels
            }
            if (!.replace_missings) {
              .m <- util_get_code_list(
                x = cl,
                code_name = MISSING_LIST,
                mdf = meta_data,
                label_col = label_col,
                warning_if_no_list = FALSE,
                warning_if_unsuitable_list = FALSE
              )
              .j <- util_get_code_list(
                x = cl,
                code_name = JUMP_LIST,
                mdf = meta_data,
                label_col = label_col,
                warning_if_no_list = FALSE,
                warning_if_unsuitable_list = FALSE
              )
              defined <- levels %in% c(
                unname(.m),
                unname(.j)
              )
              levels <- levels[!defined]
              labels <- labels[!defined]

              levels <- c(levels, unname(.j))
              labels <- c(labels, names(.j))
              levels <- c(levels, unname(.m))
              labels <- c(labels, names(.m))
            }
            if (.apply_factor_metadata_inadm) {
              empir <- unique(ds1[[cl]])
              empir <- empir[!is.na(empir)]
              empir <- setdiff(empir, levels)
              levels <- c(levels, empir)
              labels <- c(labels, empir)
            }
            .fct <- do.call(factor, list(
              x = ds1[[cl]],
              levels = levels,
              labels = labels,
              ordered = ordered
            ), quote = TRUE)
            if
            (any(is.na(.fct) != is.na(ds1[[cl]]))) {
              hint <- sprintf(
                paste(
                  c(
                    "Found the following",
                    "inadmissible categorical",
                    "values in %s: %s -- I've",
                    "removed them."
                  )
                ),
                sQuote(cl),
                util_pretty_vector_string(
                  unique(ds1[[cl]][is.na(.fct) !=
                        is.na(ds1[[cl]])])
                )
              )
            } else {
              hint <- character(0)
            }
            .fct <-
              util_attach_attr(
                .fct,
                hint = hint
              )
          } else {
            .fct <- ds1[[cl]]
          }
          return(.fct)
        }
      )
    }
    # message for hints in columns - currently only inadm.cat.values.
    for (cl in intersect(
      relevant_vars_for_warnings_lb,
      colnames(ds1)
    )) {
      cl_hint <- util_attr(ds1[[cl]], "hint", exact = TRUE)
      if (length(cl_hint) > 0 &&
          !all(is.na(cl_hint))) {
        util_message(cl_hint)
      }
    }

    ##
  }

  attr(ds1, "apply_fact_md") <- .apply_factor_metadata
  attr(ds1, "apply_fact_md_inadm") <- .apply_factor_metadata_inadm

  # Keep the raw study-data attribute column-aligned with the visible ds1.
  # ds1 may have been mapped to labels and reordered by metadata, while
  # study_data still uses VAR_NAMES in its preparation order.
  study_data_for_attr <- util_raw_study_data_attr_subset(
    ds1 = ds1,
    raw_study_data = study_data,
    meta_data = meta_data
  )
  if (!is.data.frame(study_data_for_attr)) {
    study_data_for_attr <- study_data
  }

  study_data <- util_cast_off(study_data, "study_data", TRUE)
  study_data_for_attr <- util_cast_off(study_data_for_attr, "study_data", TRUE)
  meta_data <- util_cast_off(meta_data, "meta_data", TRUE)
  ds1 <- util_cast_off(ds1, "ds1", TRUE)
  attr(ds1, "study_data") <- study_data_for_attr
  attr(ds1, "dataquieR_preparation_signature") <- key
  ds1 <- util_as_prepared_data_frame(ds1)

  if (.internal) {
    assign("study_data", study_data, parent.frame())
    assign("meta_data", meta_data, parent.frame())
    assign("ds1", ds1, parent.frame())
    assign("label_col", label_col, parent.frame())
  }

  dataquieR.study_data_cache_max <-
    getOption(
      "dataquieR.study_data_cache_max",
      dataquieR.study_data_cache_max_default
    )

  if (is.data.frame(ds1) &&
      !(key %in% names(.study_data_cache))) {
    if (sum(object.size(ds1),
        vapply(.study_data_cache, object.size, FUN.VALUE = numeric(1)),
        na.rm = TRUE
      ) <= dataquieR.study_data_cache_max) {
      .study_data_cache[[key]] <- util_attach_attr(ds1, call = sys.call())
    } else if (getOption(
      "dataquieR.study_data_cache_metrics",
      dataquieR.study_data_cache_metrics_default
    )) {
      class(dataquieR.study_data_cache_max) <- "object_size"
      rlang::inform(
        sprintf(
          paste(
            "Maximum size for study_data cache (%s) reached, not caching.",
            "Adjust the %s to control"
          ),
          format(dataquieR.study_data_cache_max, units = "auto"),
          sQuote("option(dataquieR.study_data_cache_max = )")
        ),
        .frequency = "once", .frequency_id = paste(key)
      )
    }
  }
  if (!is.null(input_cache_key) &&
      is.data.frame(ds1) &&
      exists(key, envir = .study_data_cache, inherits = FALSE)) {
    .study_data_cache_input_keys[[input_cache_key]] <- key
    .study_data_cache_meta_data[[key]] <- meta_data
  }

  if (.internal) {
    invisible(ds1)
  } else {
    ds1
  }
}

#' Internal helper: maybe load meta data v2
#'
#' @noRd
util_maybe_load_meta_data_v2 <- function() {
  if ("meta_data_v2" %in% rlang::fn_fmls_names(fn = rlang::caller_fn(1))) {
    eval.parent(substitute({
      if (!missing(meta_data_v2)) {
        util_message(
          "Have %s set, so I'll remove all loaded data frames",
          sQuote("meta_data_v2")
        )
        prep_purge_data_frame_cache()
        prep_load_workbook_like_file(meta_data_v2)
        if (!exists("item_level", .dataframe_environment())) {
          w <- paste(
            "Did not find any sheet named %s in %s, is this",
            "really dataquieR version 2 metadata?"
          )
          if (requireNamespace("cli", quietly = TRUE)) {
            w <- cli::bg_red(cli::col_br_yellow(w))
          }
          util_warning(w, dQuote("item_level"), dQuote(meta_data_v2),
            immediate = TRUE
          )
        }
      }
    }))
  } else {
    util_error(
      c(
        "Internal error: Need a %s formal, sorry. Please report.",
        "As a dataquieR developer: You need to declare a formal",
        "argument %s, if you want to call %s."
      ),
      sQuote("meta_data_v2"),
      sQuote("meta_data_v2"),
      sQuote("util_maybe_load_meta_data_v2")
    )
  }
}

#' Internal helper: metadata level aliases
#'
#' @noRd
util_metadata_level_aliases <- function() {
  c( # item_level/meta_data is addressed by prep_prepare_dataframes()
    segment_level = "meta_data_segment",
    cross_item_level = "meta_data_cross_item",
    `cross-item_level` = "meta_data_cross_item",
    dataframe_level = "meta_data_dataframe",
    item_computation_level =
      "meta_data_item_computation"
  )
}

#' Internal helper: metadata level alternative names
#'
#' @noRd
util_metadata_level_alternative_names <- function(canonical) {
  arg_maps_to <- util_metadata_level_aliases()
  switch(canonical,
    meta_data = c("meta_data", "item_level"),
    meta_data_cross = c(
      "meta_data_cross",
      util_metadata_level_alternative_names(
        "meta_data_cross_item"
      )
    ),
    unique(c(canonical, names(arg_maps_to)[arg_maps_to == canonical]))
  )
}

# fixes aliases for all metadata levels except item level for the data frame
# names as well as for arguments of exported functions
#' Internal helper: ck arg aliases
#'
#' @noRd
util_ck_arg_aliases <- function() {
  arg_maps_to <- util_metadata_level_aliases()

  # handle metadata argument aliases --------
  caller_formal_names <- rlang::fn_fmls_names(fn = rlang::caller_fn(1))
  caller_formals <- rlang::fn_fmls(fn = rlang::caller_fn(1))
  missing_in_parent <- suppressWarnings({
    vapply(caller_formal_names, function(x) {
      eval(
        call(
          "missing",
          as.symbol(x)
        ),
        envir = parent.frame(3)
      )
    },
    FUN.VALUE = logical(1)
    )
  })

  for (arg in intersect(
    names(arg_maps_to),
    caller_formal_names
  )) {
    list_of_synonyms <- names(arg_maps_to)[arg_maps_to == arg_maps_to[[arg]]]
    for (syn in setdiff(
      union(list_of_synonyms, arg_maps_to[[arg]]), arg
    )) {
      if (syn %in% caller_formal_names) { # have both
        if (!missing_in_parent[[syn]] &&
            !missing_in_parent[[arg]] &&
            !identical(dynGet(syn), dynGet(arg))) {
          util_error(
            c(
              "You cannot provide both, %s as well as %s",
              "these arguments are synonyms and must be",
              "used mutually exclusively"
            ),
            sQuote(syn),
            sQuote(arg)
          )
        }
      }
    }
    v <- rlang::missing_arg()
    if (!missing_in_parent[[arg]]) {
      v <- eval.parent(as.symbol(arg))
    } else {
      for (syn in setdiff(
        union(list_of_synonyms, arg_maps_to[[arg]]), arg
      )) {
        if (!missing_in_parent[[syn]]) {
          v <- eval.parent(as.symbol(syn))
        }
      }
    }
    if (!missing(v)) {
      assign(arg_maps_to[[arg]], v, envir = parent.frame())
      for (syn in list_of_synonyms) {
        assign(syn, v, envir = parent.frame())
      }
    }
  }

  # handle data frame alias names --------
  # order does matter, because if the above stops,
  # the data frame cache stays untouched
  df_names <- prep_list_dataframes()
  cils2rn <- grep("(^|\\|)\\s*cross_item_level$",
    perl = TRUE, value = TRUE,
    df_names
  )
  for (cil2rn in cils2rn) { # rename dataframes named cross_item_level
    new_nm <- sub("cross_item_level$", "cross-item_level", cil2rn)
    if (!new_nm %in% df_names) {
      prep_add_data_frames(
        data_frame_list =
          setNames(list(
            prep_get_data_frame(cil2rn)
          ), nm = new_nm)
      )
    }
  }
}

# used, in case ds1 is copied or similar to keep the attributes in place
.ds1_attribute_names <- c(
  "Codes_to_NA", "MAPPED", "label_col", "HL_viol_to_NA",
  "Data_type_matches", "apply_fact_md",
  "apply_fact_md_inadm", "study_data",
  "normalized", "version"
)

#' Internal helper: util fix data types
#'
#' @noRd
.util_fix_data_types <- function(meta_data, study_data) {
  if (missing(study_data))
    study_data <- data.frame()
  which_wrong <-
    util_empty(meta_data[[DATA_TYPE]]) |
    !(meta_data[[DATA_TYPE]] %in% DATA_TYPES)

  wrong_names <- meta_data[which_wrong, VAR_NAMES, drop = TRUE]

  wrong_names <- intersect(colnames(study_data), wrong_names)

  datatypes <- prep_datatype_from_data(
    resp_vars = wrong_names,
    study_data = study_data
  )

  if (length(datatypes) != 0) {
    util_warning(
      c(
        "For the variables %s, I have no valid %s in the %s. I've predicted",
        "the %s from the %s yielding %s."
      ),
      util_pretty_vector_string(wrong_names),
      sQuote(DATA_TYPE),
      sQuote("meta_data"),
      sQuote(DATA_TYPE),
      sQuote("study_data"),
      dQuote(prep_deparse_assignments(names(datatypes), datatypes,
          mode = "string_codes"
        )),
      applicability_problem = TRUE,
      intrinsic_applicability_problem = FALSE
    )
  }
  meta_data[which_wrong, DATA_TYPE] <-
    datatypes[meta_data[which_wrong, VAR_NAMES, drop = TRUE]]

  meta_data
}
