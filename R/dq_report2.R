# nolint start: line_length_linter.
#' Generate a full DQ report, v2
#'
#' @details
#' If `resp_vars` is set, only item-level indicator functions are filtered by
#' the selected measurement variables so far.
#'
#' @inheritParams .template_function_indicator
#'
#' @param ... arguments to be passed to all called indicator functions if
#'            applicable.
#' @param cores [integer] number of cpu cores to use or a named list with
#'                        arguments for the internal parallel backend
#'                        (`util_parallel_start`) or NULL, if parallel has
#'                        already been started by the caller. Can also be a
#'                        cluster. In RStudio, caller-created clusters may hang
#'                        during HTML report finalization while thumbnail and
#'                        embedded HTML files are written. Prefer passing a number, e.g.,
#'                        `cores = 4`, or a backend list, e.g.,
#'                        `cores = list(mode = "socket", cpus = 4)`, so
#'                        `dataquieR` can create and stop the cluster itself.
#'                        Alternatively run the same command outside RStudio
#'                        (on Windows start R from the Start menu, on macOS open
#'                        Terminal and run R, on Linux run R in a terminal).
#'                        To force a caller-owned
#'                        cluster in RStudio, set
#'                        `options(dataquieR.force_rstudio_user_cluster = TRUE)`
#'                        or pass
#'                        `advanced_options =
#'                        list(dataquieR.force_rstudio_user_cluster = TRUE)`.
#' @param ignore_empty_vars [enum] TRUE | FALSE | auto. See
#'                             [dataquieR.ignore_empty_vars].
#' @param specific_args [list] named list of arguments specifically for one of
#'                             the called functions, the of the list elements
#'                             correspond to the indicator functions whose calls
#'                             should be modified. The elements are lists of
#'                             arguments.
#' @param dimensions [dimensions] Vector of dimensions to address in the report.
#'                   Allowed values in the vector are Completeness, Consistency,
#'                   and Accuracy. The generated report will only cover the
#'                   listed data quality dimensions. Accuracy is computational
#'                   expensive, so this dimension is not enabled by default.
#'                   Completeness should be included, if Consistency is
#'                   included, and Consistency should be included, if Accuracy
#'                   is included to avoid misleading detections of e.g. missing
#'                   codes as outliers, please refer to the data quality concept
#'                   for more details. Integrity is always included.
#'                   If dimensions is equal to NULL or "all", all dimensions
#'                   will be covered.
#' @param author [character] author for the report documents.
#' @param debug_parallel [logical] print blocks currently evaluated in parallel
#' @param user_info [list] additional info stored with the report, e.g.,
#'                         comments, title, ...
#' @param filter_indicator_functions [character] regular expressions, only
#'                                               if an indicator function's name
#'                                               matches one of these, it'll
#'                                               be used for the report. If
#'                                               of length zero, no filtering
#'                                               is performed.
#' @param exclude_indicator_functions [character] regular expressions,
#'                                               if an indicator function's name
#'                                               matches one of these, it'll
#'                                               be excluded from the report. If
#'                                               of length zero, no filtering
#'                                               is performed.
#' @param filter_result_slots [character] regular expressions, only
#'                                               if an indicator function's
#'                                               result's name
#'                                               matches one of these, it'll
#'                                               be used for the report. If
#'                                               of length zero, no filtering
#'                                               is performed.
#' @param mode [character] work mode for parallel execution. default is
#'              "default", the values mean:
#'              - default: use `queue` except `cores` has been set explicitly
#'              - futures: use the `future` package
#'              - queue: use a queue as described in the examples
#'                from the `callr` package by Csárdi and Chang and start
#'                sub-processes as workers that evaluate the queue.
#'              - parallel: use the cluster from `cores` to evaluate all
#'                         calls of indicator functions using the classic
#'                         R `parallel` back-ends
#'
#' @param mode_args [list] of arguments for the selected `mode`. As of writing
#'                         this manual, only for the mode `queue` the argument
#'                         `step` is supported, which gives the number of
#'                         function calls that are run by one worker at a time.
#'                         the default is 6, which gives on most of the tested
#'                         systems a good balance between synchronization
#'                         overhead and idling workers.
#' @param notes_from_wrapper [list] a list containing notes about changed labels
#'                                  by `dq_report_by` (otherwise NULL)
#' @param title [character] optional argument to specify the title for
#'                          the data quality report
#' @param subtitle [character] optional argument to specify a subtitle for
#'                             the data quality report
#' @param advanced_options [list] options to set during report computation,
#'                                see [options()]
#' @param meta_data_item_computation [data.frame] optional. computation rules
#'                                              for computed variables.
#' @param storr_factory [function] `NULL`, or
#'                        a function returning a `storr` object as
#'                        back-end for the report's results. If used with
#'                        `cores > 1`, the storage must be accessible from all
#'                        cores and capable of concurrent writing according
#'                        to `storr`. Hint: `dataquieR` currently only supports
#'                        `storr::storr_rds()`, officially, while other back-
#'                        ends may nevertheless work, yet, they are not tested.
#' @param amend [logical] if there is already data in.`storr_factory`,
#'                        use it anyways -- unsupported, so far!
#' @param checkpoint_resumed [logical] if using a `storr_factory` and the back-
#'                                     end there is already filled, and if
#'                                     `amend` is missing or set to `TRUE`,
#'                                     compute all missing result and add them
#'                                     to the back-end.
#' @param item_computation_level [data.frame] alias for
#'                               `meta_data_item_computation`
#' @param .internal [logical] internal use, only.
#' @param name_of_study_data [character] name for study data inside the report,
#'                                       internal use.
#' @param dt_adjust [logical] whether to trust data types in the study data. if
#'                            `TRUE`, data types are checked based on the
#'                            metadata and later casted to the declared type.
#'                            if your data source is already typed, this can
#'                            be turned off to speed up computations.
#'                            see [dataquieR.dt_adjust]
#' @param output_dir [character] if `output_dir` is not `NULL`, also create
#'                             `HTML` output for the report using
#'                             [print.dataquieR_resultset2()]
#'                               written to the path `output_dir`
#' @param dir [character] alias for `output_dir`.
#' @param force_overwrite [logical] force to overwrite `output_dir`, even if it
#'                                 exists
#' @return a [dataquieR_resultset2] that can be
#' [printed][print.dataquieR_resultset2] creating a `HTML`-report.
#'
#' @details
#' See [dq_report_by] for a way to generate stratified or splitted reports
#' easily.
#'
#' @seealso
#' `r paste0(" * [", methods(class="dataquieR_resultset"), "]", collapse="\n")`
#' * [dq_report_by]
#' @export
#' @importFrom stats alias
#' @importFrom utils osVersion packageName packageVersion
#' @importFrom stats setNames
# nolint end
dq_report2 <- function(study_data,
  item_level = "item_level",
  label_col = LABEL,
  meta_data_segment = "segment_level",
  meta_data_dataframe = "dataframe_level",
  meta_data_cross_item = "cross-item_level",
  meta_data_item_computation =
    "item_computation_level",
  meta_data = item_level,
  meta_data_v2,
  ...,
  dimensions = c("Completeness", "Consistency"),
  cores = list(
    mode = "socket",
    logging = FALSE,
    cpus = util_detect_cores(),
    load.balancing = TRUE
  ),
  ignore_empty_vars =
    getOption(
      "dataquieR.ignore_empty_vars",
      dataquieR.ignore_empty_vars_default
    ),
  specific_args = list(),
  advanced_options = list(),
  author = prep_get_user_name(),
  title = "Data quality report",
  subtitle = as.character(Sys.Date()),
  user_info = NULL,
  debug_parallel = FALSE,
  resp_vars = character(0),
  filter_indicator_functions = character(0),
  exclude_indicator_functions = character(0),
  filter_result_slots = c(
    "^Summary",
    "^Segment",
    "^DataTypePlotList",
    "^ReportSummaryTable",
    "^Dataframe",
    "^Result",
    "^VariableGroup"
  ),
  mode = c("default", "futures", "queue", "parallel"),
  mode_args = list(),
  notes_from_wrapper = list(),
  storr_factory = NULL,
  amend = FALSE,
  cross_item_level,
  `cross-item_level`,
  segment_level,
  dataframe_level,
  item_computation_level,
  .internal =
    rlang::env_inherits(
      rlang::caller_env(),
      parent.env(environment())
    ),
  checkpoint_resumed =
    getOption(
      "dataquieR.resume_checkpoint",
      dataquieR.resume_checkpoint_default
    ),
  name_of_study_data,
  dt_adjust = as.logical(getOption(
    "dataquieR.dt_adjust",
    dataquieR.dt_adjust_default
  )),
  output_dir = NULL,
  force_overwrite = FALSE,
  dir = NULL) {
  output_dir <- util_resolve_output_dir_alias(
    dir = dir,
    output_dir = output_dir,
    dir_missing = missing(dir),
    output_dir_missing = missing(output_dir)
  )
  has_output_dir <- !is.null(output_dir)
  util_expect_scalar(force_overwrite, check_type = is.logical)

  rep_id <- util_make_report_id()

  util_stop_if_not(is.list(advanced_options))
  util_guard_rstudio_user_cluster(cores, advanced_options)

  withr::local_options(
    c(
      list(
        dataquieR.CONDITIONS_WITH_STACKTRACE = FALSE,
        dataquieR.ERRORS_WITH_CALLER = FALSE,
        dataquieR.MESSAGES_WITH_CALLER = FALSE,
        dataquieR.WARNINGS_WITH_CALLER = FALSE
      ),
      advanced_options
    )
  )

  my_storr_object <- util_storr_object(storr_factory)

  util_expect_scalar(dt_adjust, check_type = is.logical)
  util_expect_scalar(amend, check_type = is.logical)
  util_expect_scalar(checkpoint_resumed, check_type = is.logical)

  if (missing(amend) && !amend && checkpoint_resumed) {
    util_message(
      c(
        "%s was %s, but %s was unset (default = %s).",
        "I will set %s to %s to make %s work."
      ),
      sQuote("checkpoint_resumed"),
      sQuote("TRUE"),
      sQuote("amend"),
      sQuote("FALSE"),
      sQuote("amend"),
      sQuote("TRUE"),
      sQuote("checkpoint_resumed")
    )
    amend <- TRUE
  } else if (!missing(amend) && !amend && checkpoint_resumed) {
    util_message(
      c(
        "%s was %s, but %s was %s.",
        "I will set %s to %s to avoid results from being touched",
        "(%s = %s)"
      ),
      sQuote("amend"),
      sQuote("FALSE"),
      sQuote("checkpoint_resumed"),
      sQuote("TRUE"),
      sQuote("checkpoint_resumed"),
      sQuote("FALSE"),
      sQuote("amend"),
      sQuote("FALSE")
    )
    checkpoint_resumed <- FALSE
  }

  if (!is.null(my_storr_object) && (
    length(my_storr_object$list()) > 0 ||
      length(my_storr_object$list(
        util_get_storr_att_namespace(my_storr_object)
      )) > 0 ||
      length(my_storr_object$list(
        util_get_storr_summ_namespace(my_storr_object)
      )) > 0
  )) {
    if (amend) {
      util_message(
        c(
          "Your storr-object is not empty, but %s was set %s,",
          "so I'll amend the storage object. This is unsupported,",
          "yet, so expect strange behavior."
        ),
        dQuote("amend"), sQuote(TRUE)
      )
    } else {
      util_error(
        c(
          "Your storr-object is not empty, and %s was set %s,",
          "so I won't amend the storage object, which would",
          "still be unsupported, so could cause strange behavior.",
          "We strongly recommend to use clear storr objects (or",
          "at least the default namespace (%s in your case)",
          "and its sister namespaces (the default namespace suffixed",
          "with %s and %s, should be empty. In case of %s, just",
          "delete the folder that backs the storr."
        ),
        dQuote("amend"),
        sQuote(FALSE),
        sQuote(my_storr_object$default_namespace),
        sQuote(".attributes"),
        sQuote(".summary"),
        sQuote("driver_rds")
      )
    }
  }

  util_match_arg(
    ignore_empty_vars,
    c("TRUE", "FALSE", "auto")
  )

  mode <- util_match_arg(mode)

  if (missing(title)) {
    attr(title, "default") <- TRUE
  } else {
    attr(title, "default") <- FALSE
  }

  if (missing(subtitle)) {
    attr(subtitle, "default") <- TRUE
  } else {
    attr(subtitle, "default") <- FALSE
  }

  util_expect_scalar(title,
    check_type = is.character,
    error_message = sprintf(
      "%s needs to be character(1)",
      sQuote("title")
    )
  )

  util_expect_scalar(subtitle,
    check_type = is.character,
    error_message = sprintf(
      "%s needs to be character(1)",
      sQuote("subtitle")
    )
  )

  .hi <- .hp <- .hm <- NULL
  content_file <- NULL

  if (has_output_dir) {
    output_dir <- util_normalize_path(output_dir)

    content_file <- file.path(output_dir, "index.html")

    util_expect_scalar(output_dir, check_type = is.character)

    util_overwrite_if_requested(output_dir, force_overwrite)

    util_message("Writing to %s", dQuote(content_file))

    packageName <- utils::packageName()

    list2env(util_init_html_progress(
      output_dir = output_dir,
      content_file = content_file,
      title = title,
      view = TRUE,
      rep_id = rep_id
    ), envir = environment())
  }

  if (!is.null(cores) && (missing(cores) || (
    is.vector(cores) && length(cores) == 1 && util_is_integer(cores)) &&
    cores > 1) &&
    mode == "default") {
    if (util_ensure_suggested(c("R6", "processx"),
        goal =
          "use the queue mode, which is faster than parallel (but cannot run on distributed cluster nodes)", # nolint: line_length_linter.
        err = FALSE
      )) {
      mode <- "queue"
    } else {
      mode <- "parallel"
    }
  } else if (mode == "default") {
    mode <- "parallel"
  }

  if (suppressWarnings(util_ensure_suggested("testthat", err = FALSE))) {
    if (testthat::is_testing()) {
      if (!(mode %in% c("parallel", "queue"))) {
        util_warning(
          "Internal problem: %s should be %s, %s or %s in the context of %s",
          sQuote("mode"), dQuote("queue"), dQuote("parallel"),
          dQuote("default"),
          sQuote("testthat")
        )
      }
      if (!rlang::is_scalar_integerish(cores) ||
          cores > 1L) {
        if (!is.null(cores)) {
          util_warning(
            "Internal problem: %s should be an integer below %s in the context of %s", # nolint: line_length_linter.
            sQuote("cores"), dQuote("2"), sQuote("testthat")
          )
        }
      }
    }
  }

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

  # checks and fixes the function arguments
  util_ck_arg_aliases()

  if (missing(study_data)) {
    df_study_data <- util_find_study_data_from_dataframe_level(
      meta_data_dataframe = meta_data_dataframe
    )
    if (!is.null(df_study_data)) {
      util_message(
        "Using %s from your dataframe level metadata",
        dQuote(df_study_data$name_of_study_data)
      )
      study_data <- df_study_data$study_data
      if (missing(name_of_study_data)) {
        name_of_study_data <- df_study_data$name_of_study_data
      }
    } else if ("study_data" %in% prep_list_dataframes()) {
      util_message("Using %s from the dataframe cache.", dQuote("study_data"))
      study_data <- prep_get_data_frame("study_data", keep_types = TRUE)
      if (missing(name_of_study_data)) {
        name_of_study_data <- "study_data"
      }
    } else {
      util_error(
        c(
          "Missing %s. Please pass it explicitly, load it into the",
          "data-frame cache, or provide dataframe-level metadata with",
          "loadable study-data references."
        ),
        sQuote("study_data")
      )
    }
  }

  if (missing(name_of_study_data)) {
    if (is.data.frame(study_data)) {
      name_of_study_data <- substr(
        head(as.character(substitute(study_data)), 1),
        1, 500
      )
    } else if (length(study_data) == 1 && is.character(study_data)) {
      name_of_study_data <- study_data
    } else {
      name_of_study_data <- "??No study data found??"
    }
  } else {
    util_expect_scalar(name_of_study_data,
      check_type = is.character
    )
    substr(name_of_study_data, 1, 500)
  }
  util_expect_data_frame(study_data, keep_types = TRUE)
  # Empty study-data handling is covered by `util_expect_data_frame()`.
  util_register_primary_study_data(
    study_data = study_data,
    name_of_study_data = name_of_study_data
  )
  util_handle_val_tab()

  # checks the data frame names in the dataframe cache
  if (!.internal) {
    util_verify_names(name_of_study_data = name_of_study_data)
  }

  try(util_expect_data_frame(meta_data), silent = TRUE)

  case_insens <- util_is_na_0_empty_or_false(
    getOption(
      "dataquieR.study_data_colnames_case_sensitive",
      dataquieR.study_data_colnames_case_sensitive_default
    )
  )

  if (case_insens) {
    colnames(study_data) <-
      util_align_colnames_case(
        .colnames = colnames(study_data),
        .var_names = meta_data[[VAR_NAMES]]
      )
  }

  ci_in_study <- FALSE
  if (is.data.frame(meta_data) && !case_insens) {
    in_study <- meta_data[[VAR_NAMES]] %in% colnames(study_data)
    ci_in_study <- tolower(meta_data[[VAR_NAMES]]) %in%
      tolower(colnames(study_data))
  }
  if (identical(getOption(
    "dataquieR.ELEMENT_MISSMATCH_CHECKTYPE",
    dataquieR.ELEMENT_MISSMATCH_CHECKTYPE_default
  ), "subset_u")) {
    if (is.data.frame(meta_data)) {
      meta_data <- meta_data[in_study, , drop = FALSE]
    }
  }

  # try, meta_data my still be missing
  # note: we had the following twice, first w/o try, then w/ try. likely a bug
  try(
    meta_data <- util_prepare_item_level_metadata(
      meta_data = meta_data,
      label_col = label_col
    ),
    silent = TRUE
  )
  warning_pred_meta <- NULL
  if (!is.data.frame(meta_data) || !prod(dim(meta_data))) {
    try_ci <- ""
    if (!case_insens) {
      if (any(ci_in_study)) {
        try_ci <- sprintf(
          paste(
            "But maybe, if you enable case-insensitive mapping",
            "of meta_data on study data using %s, it could work?"
          ),
          sQuote(
            "options(dataquieR.study_data_colnames_case_sensitive = FALSE)"
          )
        )
      }
    }
    w <- paste(
      "No item level metadata matching study data found. Will guess",
      "some from the study data. This will not be very helpful, please",
      "consider passing an item level metadata file.",
      try_ci
    )
    if (requireNamespace("cli", quietly = TRUE)) {
      w <- cli::bg_red(cli::col_br_yellow(cli::ansi_toupper(w)))
      w <- gsub("%S", "%s", w)
    }
    util_warning(w,
      immediate = TRUE
    )
    predicted <- prep_study2meta(study_data, convert_factors = TRUE)
    meta_data <- predicted$MetaData
    study_data <- predicted$ModifiedStudyData
    warning_pred_meta <- paste(
      "No item-level metadata matching study data",
      "could be found, so it was guessed from the",
      "study data.", try_ci
    )
  } else {
    # strip rownames from metadata to prevent confusing the html_table function
    rownames(meta_data) <- NULL
  }
  meta_data_segment <- util_ensure_segment_metadata(
    meta_data_segment = meta_data_segment,
    meta_data = meta_data
  )
  try(util_expect_data_frame(meta_data_dataframe), silent = TRUE)
  if (!is.data.frame(meta_data_dataframe)) {
    util_message(
      "No dataframe level metadata %s found.",
      dQuote(meta_data_dataframe)
    )
    meta_data_dataframe <- util_dataframe_metadata_for_names(
      name_of_study_data,
      include_df_code = FALSE,
      include_df_id_vars = FALSE
    )
  } else {
    # strip rownames from metadata to prevent confusing the html_table function
    rownames(meta_data_dataframe) <- NULL
  }
  meta_data_cross_item <- util_ensure_cross_item_metadata(
    meta_data_cross_item
  )

  suppressWarnings(util_ensure_in(VAR_NAMES, names(meta_data),
    error = TRUE,
    err_msg =
      sprintf(
        "Did not find the mandatory column %%s in the %s.",
        sQuote("meta_data")
      )
  ))

  util_expect_scalar(label_col, check_type = is.character)
  util_ensure_in(label_col, names(meta_data),
    error = TRUE,
    err_msg =
      sprintf(
        "Did not find a label column (%s) named %%s in the %s. Did you mean %%s?", # nolint: line_length_linter.
        sQuote("label_col"),
        sQuote("meta_data")
      )
  )

  try(util_expect_data_frame(meta_data_item_computation), silent = TRUE)
  if (!is.data.frame(meta_data_item_computation)) {
    meta_data_item_computation <- data.frame()
  }

  cross_item_already_normalized <-
    identical(
      util_attr(meta_data_cross_item, "normalized", exact = TRUE),
      TRUE
    )

  prepared_label_modification_text <- NULL
  prepared_label_modification_table <- NULL
  if (!util_dataquieR_inputs_prepared(study_data, meta_data)) {
    prepared_inputs <- util_prepare_dataquieR_inputs(
      study_data = study_data,
      meta_data = meta_data,
      label_col = label_col,
      meta_data_cross_item = meta_data_cross_item,
      meta_data_item_computation = meta_data_item_computation,
      name_of_study_data = name_of_study_data
    )
    study_data <- prepared_inputs$study_data
    meta_data <- prepared_inputs$meta_data
    meta_data_item_computation <- prepared_inputs$meta_data_item_computation
    prepared_label_modification_text <-
      prepared_inputs$label_modification_text
    prepared_label_modification_table <-
      prepared_inputs$label_modification_table
  }

  # ensure that VAR_NAMES and labels exist, are unique and not too long
  mod_label <- util_ensure_label(
    meta_data = meta_data,
    label_col = label_col
  )
  if (!is.null(mod_label$label_modification_text)) {
    # There were changes in the metadata.
    meta_data <- mod_label$meta_data
    label_col <- mod_label$label_col
  }
  mod_label$label_modification_text <- trimws(paste(
    prepared_label_modification_text,
    mod_label$label_modification_text
  ))
  mod_label$label_modification_table <- rbind(
    prepared_label_modification_table,
    mod_label$label_modification_table
  )
  # Fill empty labels before later code maps to metadata label columns.
  meta_data <- util_fill_empty_label_columns(meta_data, label_col = label_col)

  if (!cross_item_already_normalized) {
    meta_data_cross_item <- util_normalize_cross_item(
      meta_data = meta_data,
      meta_data_cross_item = meta_data_cross_item,
      label_col = label_col
    )
  }

  all_vars <- (length(resp_vars) == 0)

  # remove unrelated rules, if resp_vars -----
  if (!all_vars && nrow(meta_data_cross_item) > 0) {
    meta_data_cross_item <- util_filter_cross_item_metadata(
      meta_data_cross_item = meta_data_cross_item,
      resp_vars = resp_vars,
      meta_data = meta_data,
      label_col = label_col
    )
  }
  # end remove unrelated rules -----


  util_expect_scalar(dimensions,
    allow_more_than_one = TRUE,
    allow_null = TRUE,
    check_type = is.character,
    error_message =
      sprintf(
        "The argument %s must be character or NULL",
        sQuote("dimensions")
      )
  )
  if (length(dimensions) == 0 || (length(dimensions) == 1 &&
        !is.na(dimensions) &&
        tolower(trimws(dimensions)) == "all")) {
    dimensions <- c("completeness", "consistency", "accuracy")
  } else {
    dimensions <- tolower(dimensions)
  }
  dimensions[dimensions %in% c("acc", "accuracy")] <- "Accuracy"
  dimensions[dimensions %in% c("con", "consistency")] <- "Consistency"
  dimensions[dimensions %in% c("com", "completeness")] <- "Completeness"
  dimensions[dimensions %in% c("int", "integrity")] <- "Integrity"
  dimensions[dimensions %in% c("des", "descriptors")] <- "Descriptors"
  .dimensions <-
    util_ensure_in(dimensions,
      c(
        "Completeness", "Consistency", "Accuracy", "Integrity",
        "Descriptors"
      ),
      error = FALSE,
      applicability_problem = TRUE
    )

  util_expect_scalar(resp_vars,
    allow_more_than_one = TRUE,
    allow_null = TRUE,
    check_type = is.character
  )

  md100 <- meta_data # metadata with study data

  miss_from_study <- (!(md100[[VAR_NAMES]] %in% (c(
    colnames(study_data),
    meta_data_item_computation$VAR_NAMES
  ))))

  only_nas <- vapply(setNames(md100[[VAR_NAMES]], nm = md100[[label_col]]),
    function(vn) {
      all(util_empty(study_data[[vn]]))
    },
    FUN.VALUE = logical(1)
  )

  if (any(miss_from_study)) {
    vars_not_found <- paste0(
      dQuote(paste0(
        md100[miss_from_study, label_col, drop = TRUE], " (",
        md100[miss_from_study, VAR_NAMES, drop = TRUE], ")"
      )),
      collapse = ", "
    )
    util_message(
      c(
        "Could not find the following variables in %s:",
        "%s.\nThese will be preliminarily removed from the %s."
      ),
      sQuote("study_data"),
      vars_not_found,
      sQuote("meta_data")
    )
    md100 <- md100[!miss_from_study, , drop = FALSE]
  }

  if (all_vars) {
    resp_vars <- md100[[label_col]]
  } else {
    resp_vars_m <- util_find_var_by_meta(
      resp_vars = resp_vars,
      meta_data = md100,
      label_col = label_col,
      # allowed_sources = ,
      target = label_col,
      ifnotfound = NA_character_
    )
    if (any(is.na(resp_vars_m))) {
      util_warning(
        c(
          "Could not find the following variables in %s:",
          "%s.\nThese will be removed."
        ),
        sQuote("meta_data"),
        paste0(
          dQuote(resp_vars[is.na(resp_vars_m)]),
          collapse = ", "
        )
      )
    }
    resp_vars <- resp_vars_m[!is.na(resp_vars_m)]
  }

  to_remove <-
    intersect(resp_vars, names(which(only_nas)))

  orig_ignore_empty_vars <- ignore_empty_vars

  if (length(resp_vars) == 0) {
    util_error("No response variables left.")
  }

  if (ignore_empty_vars == "auto") {
    ignore_empty_vars <- length(to_remove) / length(resp_vars) > .2 # 20%, see options for documentation, search for ignore_empty_vars20, if changing # nolint: line_length_linter.
  } else {
    ignore_empty_vars <- as.logical(ignore_empty_vars)
  }

  if (ignore_empty_vars) {
    util_warning(
      c(
        "%s was %s, so removing the following variables from the report,",
        "because they only feature empty values: %s"
      ),
      sQuote("ignore_empty_vars"),
      sQuote(orig_ignore_empty_vars),
      util_pretty_vector_string(to_remove, n_max = 6),
      immediate = TRUE
    )
    resp_vars <- setdiff(resp_vars, to_remove)
  } else if (length(to_remove) > 0) {
    util_message("Have variables only featuring empty data values: %s",
      util_pretty_vector_string(to_remove, n_max = 6),
      immediate = TRUE
    )
  }

  util_message("Pre-computing curated study data frames...")

  util_reset_cache()
  if (getOption("dataquieR.precomputeStudyData",
      default =
        dataquieR.precomputeStudyData_default
    )) {
    util_populate_study_data_cache(study_data, meta_data, label_col = LABEL)
  } else {
    util_purge_study_data_cache()
  }

  util_message("Pre-computing curated study data frames... done")

  util_expect_scalar(filter_indicator_functions,
    allow_more_than_one = TRUE,
    allow_null = TRUE,
    check_type = is.character
  )
  util_expect_scalar(exclude_indicator_functions,
    allow_more_than_one = TRUE,
    allow_null = TRUE,
    check_type = is.character
  )
  util_expect_scalar(filter_result_slots,
    allow_more_than_one = TRUE,
    allow_null = TRUE,
    check_type = is.character
  )

  scale_level <- util_amend_scale_level_once(
    study_data = study_data,
    meta_data = meta_data,
    label_col = label_col
  )
  meta_data <- scale_level$meta_data
  scale_level_predicted <- scale_level$predicted

  all_calls <- util_generate_calls(
    dimensions = dimensions,
    meta_data = meta_data,
    label_col = label_col,
    meta_data_segment = meta_data_segment,
    meta_data_dataframe = meta_data_dataframe,
    meta_data_cross_item = meta_data_cross_item,
    specific_args = specific_args,
    arg_overrides = list(...),
    filter_indicator_functions =
      filter_indicator_functions,
    exclude_indicator_functions =
      exclude_indicator_functions,
    resp_vars = resp_vars
  )

  tm <- system.time(
    r <- util_evaluate_calls(
      cores = cores,
      all_calls = all_calls,
      study_data = study_data,
      meta_data = meta_data,
      label_col = label_col,
      meta_data_segment = meta_data_segment,
      meta_data_dataframe = meta_data_dataframe,
      meta_data_cross_item = meta_data_cross_item,
      debug_parallel = debug_parallel,
      resp_vars = resp_vars,
      filter_result_slots = filter_result_slots,
      mode = mode,
      mode_args = mode_args,
      my_storr_object = my_storr_object,
      checkpoint_resumed = checkpoint_resumed,
      dt_adjust = dt_adjust,
      content_file = content_file
    )
  )

  start_from_call <- util_find_first_externally_called_functions_in_stacktrace()
  start_from_call <- length(sys.calls()) - start_from_call # refers to reverted sys.calls, so mirror the number # nolint: line_length_linter.
  if (is.na(start_from_call)) {
    start_from_call <- 1
  }
  cl <- NULL
  try(
    {
      cl <- sys.call(start_from_call)
    },
    silent = TRUE
  )

  call_override <- try(eval.parent(
    quote({
      if (exists(".dataquieR_report_call_override", inherits = TRUE)) {
        .dataquieR_report_call_override
      } else {
        NULL
      }
    })
  ), silent = TRUE)
  if (!inherits(call_override, "try-error") &&
      !is.null(call_override) &&
      length(call_override) == 1L) {
    cl <- call_override
  }

  # get call for dq_report_by
  get_call <- try(deparse(sys.call(-10)), silent = TRUE)
  if (!inherits(get_call, "try-error")) {
    if (startsWith(
      paste(deparse(sys.call(-10)), collapse = ""),
      "dq_report_by"
    )) {
      cl <- eval.parent(quote({
        if (exists("call_report_by_overview", inherits = TRUE)) {
          call_report_by_overview
        } else {
          call_report_by
        }
      }))
    }
  }

  p <- list(
    author = author,
    date = Sys.time(),
    call = cl,
    version = paste(packageName(), util_dataquieR_version()),
    R = R.version.string,
    os = osVersion,
    machine = paste(
      Sys.info()[["nodename"]],
      sprintf("(%s)", Sys.info()[["version"]]),
      Sys.info()["machine"]
    ),
    runtime = paste(round(tm[["elapsed"]], 1), "secs")
  )

  if (is.list(user_info)) {
    p <- c(user_info, p)
  }

  dq_report2_env <- environment()
  meta_data_hints <- list()
  capture <- function(cnd) {
    dq_report2_env$meta_data_hints <-
      c(dq_report2_env$meta_data_hints, list(cnd))
    if (inherits(cnd, "warning")) {
      invokeRestart("muffleWarning")
    }
    if (inherits(cnd, "message")) {
      invokeRestart("muffleMessage")
    }
  }

  suppressWarnings(suppressMessages(
    try(withCallingHandlers(meta_data <- util_validate_known_meta(meta_data),
        error = capture,
        warning = capture,
        message = capture
      ), silent = TRUE)
  ))


  if (scale_level_predicted) {
    suppressWarnings(suppressMessages(
      try(withCallingHandlers(
        util_message(
          c(
            "Missing some or all entries in %s column in item-level %s. Predicting", # nolint: line_length_linter.
            "it from the data -- please verify these predictions, they",
            "may be wrong and lead to functions claiming not to be",
            "reasonably applicable to a variable."
          ),
          sQuote(SCALE_LEVEL), "meta_data",
          applicability_problem = TRUE,
          intrinsic_applicability_problem = FALSE
        ),
        error = capture,
        warning = capture,
        message = capture
      ), silent = TRUE)
    ))
  }

  if (util_really_rstudio()) {
    rstudioapi::executeCommand("activateConsole")
  }

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

  repsum <- NA

  p$rep_id <- rep_id

  r <- util_attach_attr(r,
    properties = p,
    min_render_version = as.numeric_version("1.0.0"),
    translation_version = translation_version,
    warning_pred_meta = warning_pred_meta,
    label_modification_text = trimws(paste(
      notes_from_wrapper[["label_modification_text"]],
      mod_label$label_modification_text
    )),
    label_modification_table = rbind(
      notes_from_wrapper[["label_modification_table"]],
      mod_label$label_modification_table
    ),
    label_meta_data_hints = meta_data_hints,
    meta_data_item_computation = meta_data_item_computation,
    repsum = repsum,
    title = title,
    subtitle = subtitle
  )

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

    # Result objects are attached to the report rather than bulk-stored here.

    attr(r, "my_storr_object") <- my_storr_object
  }

  if (has_output_dir) {
    print.dataquieR_resultset2(r,
      dir = output_dir,
      view = FALSE, force_overwrite = TRUE
    )
    return(invisible(r))
  } else {
    return(r)
  }
}

.study_data_cache <- new.env(parent = emptyenv())
.study_data_cache_input_keys <- new.env(parent = emptyenv())
.study_data_cache_meta_data <- new.env(parent = emptyenv())
#' Internal helper: purge study data cache
#'
#' @noRd
util_purge_study_data_cache <- function() {
  rm(list = ls(.study_data_cache), envir = .study_data_cache)
  rm(list = ls(.study_data_cache_input_keys),
    envir = .study_data_cache_input_keys)
  rm(list = ls(.study_data_cache_meta_data),
    envir = .study_data_cache_meta_data)
}

# Historical cache-population metrics workflow removed here. Inspect with
# `git show dc9e18946f -- R/dq_report2.R` before restoring it.
#' Internal helper: populate study data cache
#'
#' @noRd
util_populate_study_data_cache <- function(study_data, meta_data, label_col, quick = getOption("dataquieR.study_data_cache_quick_fill", dataquieR.study_data_cache_quick_fill_default)) { # nolint: line_length_linter.
  util_purge_study_data_cache()
  util_purge_falsish_value_cache()
  invisible(lapply(study_data, util_is_na_0_empty_or_false))
  if (quick) {
    try(silent = TRUE, prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data, .label_col = label_col, .replace_hard_limits = TRUE, .replace_missings = TRUE, .adjust_data_type = TRUE, .amend_scale_level = TRUE)) # nolint: line_length_linter.
    try(silent = TRUE, prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data, .label_col = label_col, .replace_missings = FALSE)) # nolint: line_length_linter.
    try(silent = TRUE, prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data, .label_col = label_col)) # nolint: line_length_linter.
    try(silent = TRUE, prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data, .label_col = label_col, .allow_empty = TRUE)) # nolint: line_length_linter.
    try(silent = TRUE, prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data, .label_col = label_col, .replace_hard_limits = TRUE)) # nolint: line_length_linter.
    try(silent = TRUE, prep_prepare_dataframes(.study_data = study_data, .meta_data = meta_data, .label_col = label_col, .replace_missings = FALSE, .adjust_data_type = FALSE)) # nolint: line_length_linter.
  } else {
    combinations <- unlist(lapply(
      c(0, seq_along(.to_combine)),
      function(x) utils::combn(.to_combine, x, simplify = FALSE)
    ), recursive = FALSE)
    calls <- lapply(combinations, function(set_true) {
      res <- .call_template
      res[set_true] <- TRUE
      res
    })
    study_data_hash <- rlang::hash(study_data)
    meta_data_hash <- rlang::hash(meta_data)
    a <- lapply(
      lapply(lapply(calls, rlang::call_args), `[`, .to_combine),
      rlang::hash
    )
    names(calls) <-
      paste0(
        a, "@", study_data_hash,
        "@", meta_data_hash, "@", label_col
      )

    my_env <- new.env(parent = parent.env(environment()))
    my_env$study_data <- study_data
    my_env$meta_data <- meta_data
    my_env$label_col <- label_col

    # HINT: Do not touch my_env (use new.env(parent = my_env)) to
    #       keep the original study data for all cases.

    # Historical parallelMap cache prefill prototype removed here. Inspect with
    # `git show dc9e18946f -- R/dq_report2.R`.
    for_cache <- lapply(calls,
      function(cl, my_env) {
        try(util_attach_attr(eval(cl, envir = new.env(parent = my_env)), call = cl), # nolint: line_length_linter.
          silent = TRUE
        )
      },
      my_env = my_env
    )
    for_cache <- for_cache[vapply(for_cache, is.data.frame,
        FUN.VALUE = logical(1)
      )]
    list2env(for_cache, .study_data_cache)
  }
}

.call_template <- quote(prep_prepare_dataframes(
  .study_data = study_data,
  .meta_data = meta_data,
  .label_col = label_col,
  .replace_hard_limits = FALSE,
  .replace_missings = FALSE,
  .adjust_data_type = FALSE,
  .amend_scale_level = FALSE,
  .apply_factor_metadata = FALSE,
  .apply_factor_metadata_inadm = FALSE
))
.to_combine <- setdiff(
  names(.call_template),
  c("", ".study_data", ".meta_data", ".label_col")
)
