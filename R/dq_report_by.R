# nolint start: line_length_linter.
#' Generate a stratified full DQ report
#'
#' @inheritParams .template_function_indicator
#' @inheritParams .template_function_report_plan
#'
#' @param id_vars [variable] a vector containing the name/s of the variables
#'                            containing ids, to
#'                            be used to merge multiple data frames if provided
#'                            in `study_data` and to be add to referred vars
#' @param ... arguments to be passed through to [dq_report] or [dq_report2],
#'            including `cores`. In RStudio, avoid passing a caller-created
#'            cluster such as `cores = cl`; this can hang during HTML report
#'            finalization while thumbnail and embedded HTML files are written. Prefer
#'            passing a number, e.g., `cores = 4`, or a backend list, e.g.,
#'            `cores = list(mode = "socket", cpus = 4)`, so `dataquieR` can
#'            create and stop the cluster itself. Alternatively run the same
#'            command outside RStudio (on Windows start R from the Start menu,
#'            on macOS open Terminal and run R, on Linux run R in a terminal).
#'            To force a caller-owned cluster
#'            in RStudio, set
#'            `options(dataquieR.force_rstudio_user_cluster = TRUE)` or pass
#'            `advanced_options =
#'            list(dataquieR.force_rstudio_user_cluster = TRUE)`.
#' @param segment_column [variable attribute] name of a metadata attribute
#'                                             usable to split the report in
#'                                             sections of variables, e.g. all
#'                                             blood-pressure related variables.
#'                                             By default,
#'                                             reports are split by
#'                                             [STUDY_SEGMENT] if available and
#'                                             no segment_column nor
#'                                             strata_column or subgroup
#'                                             are defined.
#'                                             To create an un-split report
#'                                             please write explicitly
#'                                             the argument
#'                                             'segment_column = NULL'
#' @param strata_column [variable] name of a study variable to stratify the
#'                                    report by, e.g. the study centers.
#'                                    Both labels and `VAR_NAMES` are accepted.
#'                                    In case of NAs in the selected variable,
#'                                    a separate report containing the NAs
#'                                    subset will be created
#' @param strata_select [character] if given, the strata of strata_column
#'                                       are limited to the content of this
#'                                       vector. A character vector or a regular
#'                                       expression can be provided
#'                                       (e.g., "^a.*$"). This argument can not
#'                                       be used if no strata_column is
#'                                       provided
#' @param segment_select [character] if given, the levels of segment_column
#'                                       are limited to the content of this
#'                                       vector. A character vector or a
#'                                       regular expression (e.g., ".*_EXAM$")
#'                                       can be provided.
#'                                       This argument can not be used if no
#'                                       segment_column is provided.
#' @param input_dir [character] if given, the study data files that have
#'                              no path and that are not URL are searched in
#'                              this directory. Also `meta_data_v2` is searched
#'                              in this directory if no path is provided
#' @param advanced_options [list] options to set during report computation,
#'                                see [options()]
#' @param html_table_backend [character] HTML table backend to use when
#'                           `also_print` is `TRUE`. One of `"auto"`, `"DT2"`,
#'                           or `"DT"`.
#' @param output_dir [character] if given, the report objects are written to
#'                               this directory and are not retained in RAM.
#' @param dir [character] alias for `output_dir`.
#' @param missing_tables [character] the name of the data frame containing the
#'                                   missing codes, it can be a vector if more
#'                                   than one table is provided. Example:
#'                                   `c("missing_table1", "missing_table2")`.
#'                                   Use this when `item_level` metadata are
#'                                   provided separately from the workbook that
#'                                   contains the referenced code or missing
#'                                   list sheets. If a complete `meta_data_v2`
#'                                   workbook is loaded, item-level references
#'                                   in columns such as `CODE_LIST_TABLE` or
#'                                   `MISSING_LIST_TABLE` may use the sheet name
#'                                   only, e.g., `"tab1"`. If only the
#'                                   item-level metadata are supplied, the
#'                                   referenced table must already be present in
#'                                   the data-frame cache under the exact name
#'                                   used in the metadata. For tables loaded
#'                                   from a workbook, this can be the fully
#'                                   qualified workbook/sheet name, e.g.,
#'                                   `"meta_data_v2.xlsx|tab1"`, and that same
#'                                   name should be listed in `missing_tables`.
#' @param also_print [logical] if `output_dir` is not `NULL`, also create
#'                             `HTML` output for each report using
#'                             [print.dataquieR_resultset2()]
#'                               written to the path `output_dir`
#' @param disable_plotly [logical] do not use `plotly`, even if installed
#' @param selection_type [character] optional, can only be specified if a
#'                                              `strata_select` or
#'                                              `strata_exclude` is specified.
#'                                              If not present the
#'                                              function try to guess what the
#'                                              user typed as `strata_select` or
#'                                               `strata_exclude`.
#'                                              There are 3 options:
#'                                              `value` indicating that the
#'                                              stratum selected is a value and
#'                                              not a value_label.
#'                                              For example `"0"`;
#'                                              `v_label` indicating that the
#'                                              stratum specified is a label.
#'                                              For example `"male"`.
#'                                              `regex` indicating that the user
#'                                              specified strata using a regular
#'                                              expression. For example `"^Ber"`
#'                                              to select all strata starting
#'                                              with that letters
#' @param segment_exclude [character] optional, can only be specified if a
#'                                              `segment_column` is specified.
#'                                               The levels of `segment_column`
#'                                               will not include the content of
#'                                               this argument.
#'                                               A character vector or
#'                                               a regular  expression can be
#'                                               provided  (e.g., "^STU").
#' @param strata_exclude [character] optional, can only be specified if a
#'                                              `strata_column` is specified.
#'                                              The strata of `strata_column`
#'                                               will not include the content of
#'                                               this argument.
#'                                               A character vector or
#'                                               a regular  expression can be
#'                                               provided  (e.g., "^STU").
#' @param subgroup [character] optional, to define subgroups of cases. Rules are
#'                                      to be written as `REDCap` rules.
#'                                      Only VAR_NAMES are accepted in the rules.
#' @param item_computation_level [data.frame] alias for
#'                               `meta_data_item_computation`
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
#' @param view [logical] open the returned report
#'
#' @details
#' `dq_report_by()` accepts the canonical study data and metadata arguments used
#' by the indicator functions. In addition, report inputs can be given as file
#' paths, vectors of file paths, cached example-data names, or via the data-frame
#' level metadata. File names without a path are resolved relative to
#' `input_dir`, if provided.
#'
#' The returned `dataquieR_report_by` bundle is printable. Without an output
#' directory it contains the computed reports in memory. With `output_dir` or
#' `dir`, reports are flushed to `.dq2` files and removed from the returned
#' object. Such a disk-backed bundle remains printable only while that directory
#' and its files are readable. Use `print(result)` to render its collection
#' overview, or provide `also_print = TRUE` during computation.
#'
#' @return A printable `dataquieR_report_by` named list. Without an output
#'         directory its leaves are [dq_report2] reports. With an output
#'         directory its leaves are lightweight placeholders and the report
#'         data remain in the referenced `.dq2` files. The result is returned
#'         invisibly unless `view = TRUE`.
#'
#' @param force_overwrite [logical] force to overwrite `output_dir`, even if it
#'                                 exists
#' @param title [character] optional argument to specify the title for
#'                          the data quality report bundle
#' @param subtitle [character] optional argument to specify a subtitle for
#'                             the data quality report bundle
#' @param user_info [list] additional info stored with the report bundle,
#'                          e.g., comments, title, ...
#' @param author [character] author for the report bundle's documents.
#'
#' @seealso [dq_report]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # really long-running example.
#' prep_load_workbook_like_file("meta_data_v2")
#' rep <- dq_report_by("study_data",
#'   label_col =
#'     LABEL, strata_column = "CENTER_0"
#' )
#' rep <- dq_report_by("study_data",
#'   label_col = LABEL, strata_column = "CENTER_0",
#'   segment_column = NULL
#' )
#' unlink("/tmp/testRep/", force = TRUE, recursive = TRUE)
#' dq_report_by("study_data",
#'   label_col = LABEL, strata_column = "CENTER_0",
#'   segment_column = STUDY_SEGMENT, output_dir = "/tmp/testRep"
#' )
#' unlink("/tmp/testRep/", force = TRUE, recursive = TRUE)
#' dq_report_by("study_data",
#'   label_col = LABEL, strata_column = "CENTER_0",
#'   segment_column = NULL, output_dir = "/tmp/testRep"
#' )
#' dq_report_by("study_data",
#'   label_col = LABEL,
#'   segment_column = STUDY_SEGMENT, output_dir = "/tmp/testRep"
#' )
#' dq_report_by("study_data",
#'   label_col = LABEL,
#'   segment_column = STUDY_SEGMENT, output_dir = "/tmp/testRep",
#'   also_print = TRUE
#' )
#' dq_report_by(
#'   study_data = "study_data", meta_data_v2 = "meta_data_v2",
#'   advanced_options = list(
#'     dataquieR.study_data_cache_max = 0,
#'     dataquieR.study_data_cache_metrics = TRUE,
#'     dataquieR.study_data_cache_metrics_env = environment()
#'   ),
#'   cores = NULL, dimensions = "int"
#' )
#' dq_report_by(
#'   study_data = "study_data", meta_data_v2 = "meta_data_v2",
#'   advanced_options = list(dataquieR.study_data_cache_max = 0),
#'   cores = NULL, dimensions = "int"
#' )
#' }
# nolint end
dq_report_by <- function(study_data,
  item_level = "item_level",
  meta_data_segment = "segment_level",
  meta_data_dataframe = "dataframe_level",
  meta_data_cross_item = "cross-item_level",
  meta_data_item_computation = "item_computation_level",
  missing_tables = NULL,
  label_col,
  meta_data_v2,
  segment_column = NULL,
  strata_column = NULL,
  strata_select = NULL,
  selection_type = NULL,
  segment_select = NULL,
  segment_exclude = NULL,
  strata_exclude = NULL,
  subgroup = NULL,
  resp_vars = character(0),
  id_vars = NULL,
  advanced_options = list(),
  html_table_backend =
    getOption(
      "dataquieR.html_table_backend",
      dataquieR.html_table_backend_default
    ),
  storr_factory = NULL,
  amend = FALSE,
  checkpoint_resumed =
    getOption(
      "dataquieR.resume_checkpoint",
      dataquieR.resume_checkpoint_default
    ),
  ...,
  output_dir = NULL,
  input_dir = NULL,
  also_print = FALSE,
  force_overwrite = FALSE,
  disable_plotly = FALSE,
  view = TRUE,
  meta_data = item_level,
  cross_item_level,
  `cross-item_level`,
  segment_level,
  dataframe_level,
  item_computation_level,
  author = prep_get_user_name(),
  title = "Data quality report Bundle",
  subtitle = as.character(Sys.Date()),
  user_info = NULL,
  dir = NULL) {
  output_dir <- util_resolve_output_dir_alias(
    dir = dir,
    output_dir = output_dir,
    dir_missing = missing(dir),
    output_dir_missing = missing(output_dir)
  )
  has_output_dir <- !is.null(output_dir)
  if (missing(title) && has_output_dir) {
    title <- basename(output_dir)
  }
  by_call <- rlang::caller_call(0)

  util_defer_activate_rstudio_console(environment())

  .outer_by_env$outer_by <- list(
    i = NA,
    n = NA,
    msg = "Preparing computation..."
  )

  withr::defer(
    {
      .outer_by_env$outer_by <- NULL
    }
  )

  start_time <- Sys.time()

  rep_id <- util_make_report_id()

  if (!suppressWarnings(util_ensure_suggested("plotly",
        goal =
          "creating interactive figures",
        err = FALSE
      ))) {
    if (!isTRUE(disable_plotly)) {
      util_message("Without the package plotly, you miss interactive figures.")
      disable_plotly <- TRUE
    }
  }

  dots <- rlang::dots_list(...)

  util_stop_if_not(is.list(advanced_options))
  util_expect_scalar(html_table_backend, check_type = is.character)
  html_table_backend <- tolower(html_table_backend)
  html_table_backend <- util_match_arg(
    html_table_backend,
    c("auto", "dt2", "dt")
  )
  if ("cores" %in% names(dots)) {
    util_guard_rstudio_user_cluster(dots[["cores"]], advanced_options)
  }
  util_expect_scalar(view, check_type = is.logical)

  withr::local_options(c(
    list(
      dataquieR.CONDITIONS_WITH_STACKTRACE = FALSE,
      dataquieR.ERRORS_WITH_CALLER = FALSE,
      dataquieR.MESSAGES_WITH_CALLER = FALSE,
      dataquieR.WARNINGS_WITH_CALLER = FALSE,
      DT.warn.size = FALSE
    ),
    advanced_options
  ))

  # store the call to use it later for the technical info in the reports
  call_report_by <- paste(deparse(sys.call()), collapse = "")
  call_report_by_overview <- util_compact_dq_report_by_call_from_env(
    environment()
  )

  # Historical study-data-cache purge idea tracked in dataquieR issue 482.

  # Check the arguments (exception of segment_select and segment_exclude)----

  # check if both strata_column and strata_select are present
  if (!is.null(strata_select) && is.null(strata_column)) {
    util_error("strata_column is needed for selecting the strata")
  }

  # check if both strata_column and strata_exclude are present
  if (!is.null(strata_exclude) && is.null(strata_column)) {
    util_error("strata_column is needed for excluding the strata")
  }

  # check if both strata_select and strata_exclude are present
  # to use selection_type
  if (!is.null(selection_type) &&
      (is.null(strata_select) && is.null(strata_exclude))) {
    util_error(
      c(
        "selection_type can only be specified if",
        "strata_select or strata_exclude are present"
      )
    )
  }

  # check if the provided selection_type is acceptable
  if (!is.null(selection_type)) {
    util_stop_if_not(
      selection_type %in%
        c("value", "v_label", "regex"),
      label =
        'The selection_type can only be "value", "v_label", or "regex"'
    )
  }

  # check if subgroup rule is a string
  if (!is.null(subgroup)) {
    util_expect_scalar(
      arg_name = subgroup,
      allow_more_than_one = FALSE,
      check_type = is.character
    )
  }

  util_expect_scalar(force_overwrite, check_type = is.logical)

  util_expect_scalar(also_print, check_type = is.logical)
  if (has_output_dir) {
    util_expect_scalar(output_dir, check_type = is.character)
    output_entries <- if (dir.exists(output_dir)) {
      list.files(output_dir, all.files = TRUE, no.. = TRUE)
    } else {
      character()
    }
    if (also_print || length(output_entries)) {
      util_overwrite_if_requested(output_dir, force_overwrite)
    } else if (file.exists(output_dir) && !dir.exists(output_dir)) {
      util_error(
        "%s already exists as a file, not a directory",
        dQuote(output_dir)
      )
    } else if (!dir.exists(output_dir) &&
        !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)) {
      util_error("Could not create %s", dQuote(output_dir))
    }
    output_dir <- util_normalize_path(output_dir)
  }

  .hi <- .hp <- .hm <- NULL
  content_file <- NULL
  if (has_output_dir && also_print) {
    content_file <- file.path(output_dir, "index.html")
    list2env(util_init_html_progress(
      output_dir = output_dir,
      content_file = content_file,
      title = title,
      view = view,
      rep_id = rep_id
    ), envir = environment())
  }

  ### check input directory
  # if users specify an input_dir, check if it is a scalar and a character
  # string, and stop if the specified dir does not exists
  # provide an error if the dir do not exists
  if (!missing(input_dir)) {
    util_expect_scalar(input_dir, check_type = is.character)
    if (!dir.exists(input_dir)) {
      util_error(
        "%s does not exist. Provide an %s containing the study data",
        dQuote(input_dir),
        sQuote("input_dir")
      )
    }
  }

  # check if resp_vars is a string vector
  if (length(resp_vars) > 0) {
    util_expect_scalar(
      arg_name = resp_vars,
      allow_more_than_one = TRUE,
      check_type = is.character
    )
  } else {
    resp_vars_complete <- character(0)
  }


  # Check and import item_level metadata ----
  ### load meta_data_v2 and item_level metadata
  # in case of presence of meta_data_v2 purge the cache and load it
  if (!missing(meta_data_v2)) {
    util_message(
      "Have %s set, so I'll remove all loaded data frames",
      sQuote("meta_data_v2")
    )


    # save the grading rule-sets and formats and re-add to cache after purge
    grading_rl <- NULL
    if (getOption(
      "dataquieR.grading_rulesets",
      dataquieR.grading_rulesets_default
    ) %in%
      prep_list_dataframes()) {
      grading_rl <- prep_get_data_frame(getOption(
        "dataquieR.grading_rulesets",
        dataquieR.grading_rulesets_default
      ))
    }
    format_rl <- NULL
    if (options(
      "dataquieR.grading_formats" =
        dataquieR.grading_formats_default
    ) %in%
      prep_list_dataframes()) {
      format_rl <- prep_get_data_frame(options(
        "dataquieR.grading_formats" =
          dataquieR.grading_formats_default
      ))
    }
    prep_purge_data_frame_cache()
    if (!is.null(grading_rl)) {
      prep_add_data_frames(
        data_frame_list =
          setNames(list(grading_rl),
            nm = getOption(
              "dataquieR.grading_rulesets",
              dataquieR.grading_rulesets_default
            )
          )
      )
    }
    if (!is.null(format_rl)) {
      prep_add_data_frames(
        data_frame_list =
          setNames(list(format_rl),
            nm = options(
              "dataquieR.grading_formats" =
                dataquieR.grading_formats_default
            )
          )
      )
    }
    # try to import the metadata, and if not possible, try to add
    # the path if provided
    m <- try(prep_load_workbook_like_file(meta_data_v2), silent = TRUE)
    if (inherits(m, "try-error")) {
      # in case the name of the file is provided without a path and there is an
      # input_dir, the name is fixed adding the path at the beginning
      if (!is.null(input_dir)) {
        if (!grepl(.Platform$file.sep, meta_data_v2, fixed = TRUE)) {
          if (endsWith(input_dir, .Platform$file.sep)) {
            input_dir <- substr(input_dir, 1, nchar(input_dir) - 1)
          }
          meta_data_v2 <- file.path(input_dir, meta_data_v2)
        }
      }
      prep_load_workbook_like_file(meta_data_v2)
    }

    # check if item level metadata is in the cache and provide an error if not;
    # if it is not present predicts item-level from the data
    if (!is.data.frame(meta_data) &&
        (length(meta_data) != 1 || (!is.character(meta_data)) ||
            !exists(meta_data, .dataframe_environment()))) {
      w <- paste(
        "Did not find any sheet named %s in %s, is this",
        "really dataquieR version 2 metadata?"
      )
      if (requireNamespace("cli", quietly = TRUE)) {
        w <- cli::bg_red(cli::col_br_yellow(w))
      }
      util_warning(w, dQuote(meta_data),
        dQuote(meta_data_v2),
        immediate = TRUE
      )
    }
  }

  # check if meta_data (item_level) is a data frame,
  # if not look for it in the cache and in the file system
  util_expect_data_frame(meta_data)

  util_ck_arg_aliases()

  name_sd <- character(0)

  # Getting name of file indicated by user to add it as argument in
  # util_verify_names
  # (not including names from dataframe_level metadata)
  if (!missing(study_data)) {
    name_sd <- util_report_by_study_data_refs(
      study_data = study_data,
      study_data_expr = util_report_by_study_data_expr(
        substitute(study_data)
      ),
      input_dir = input_dir
    )
  } else {
    name_sd <- character(0)
  }


  util_verify_names(name_of_study_data = name_sd)
  rm(name_sd)


  ## back-compatibility for column names in item_level_metadata
  my_args <- list(...)
  if ("cause_label_df" %in% names(my_args)) {
    cause_label_df <- my_args$cause_label_df
  } else {
    cause_label_df <- rlang::missing_arg()
  }

  # check if meta_data (item_level) contains the column VAR_NAMES
  suppressWarnings(util_ensure_in(
    VAR_NAMES,
    names(meta_data),
    error = TRUE,
    err_msg =
      sprintf(
        "Did not find the mandatory column %%s in the %s.",
        sQuote("meta_data")
      )
  ))

  # define label_col as "LABEL" if they are not specified by users
  # if LABEL is missing then replace with VAR_NAMES
  if (missing(label_col) && (LABEL %in% names(meta_data))) {
    label_col <- LABEL
  } else if (missing(label_col) && !(LABEL %in% names(meta_data))) {
    label_col <- VAR_NAMES
  }

  # fix to rename columns from old metadata to new
  # (e.g., KEY_STUDY_SEGMENT > STUDY_SEGMENT)
  # even if cause_label_df is missing
  if (rlang::is_missing(cause_label_df)) {
    try(meta_data <- util_prepare_item_level_metadata(
      meta_data = meta_data,
      label_col = label_col
    ), silent = TRUE)
  } else {
    try(meta_data <- util_prepare_item_level_metadata(
      meta_data = meta_data,
      label_col = label_col,
      cause_label_df = cause_label_df
    ), silent = TRUE)
  }

  label_col_provided <- label_col
  # dq_report_by() must repair duplicate display labels before calling
  # dq_report2(), so split/overview labels stay consistent with sub-reports.
  mod_label <- util_ensure_label(
    meta_data = meta_data,
    label_col = label_col
  )
  if (!is.null(mod_label$label_modification_text)) {
    # There were changes in the metadata.
    meta_data <- mod_label$meta_data
    label_col <- mod_label$label_col
  }

  attr(meta_data, "normalized") <- TRUE
  attr(meta_data, "version") <- 2

  rm(my_args)

  meta_data <- util_ensure_variable_roles(
    meta_data = meta_data,
    label_col = label_col
  )

  util_stop_if_not(
    `Internal error, sorry: meta_data should be a data frame in dq_report_by. Please report` = # nolint: line_length_linter.
      is.data.frame(meta_data)
  )

  # if DATAFRAMES is present in the item_level metadata
  # check if it is a character vector.
  if (DATAFRAMES %in% colnames(meta_data)) {
    util_expect_data_frame(meta_data,
      col_names = list(DATAFRAMES = is.character)
    )
  }

  # Normalize identifier variables to their item-level variable names ----
  id_vars <- util_report_by_id_vars(
    id_vars = id_vars,
    meta_data = meta_data,
    label_col = label_col
  )


  # Resolve segment selection through the default STUDY_SEGMENT column ----
  segment_column_was_missing <- missing(segment_column)
  segment_column <- util_report_by_segment_column(
    meta_data = meta_data,
    segment_column = segment_column,
    segment_select = segment_select,
    segment_exclude = segment_exclude
  )

  # Prepare the remaining metadata levels ----
  metadata_levels <- util_report_by_metadata_levels(
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item,
    meta_data_segment = meta_data_segment,
    meta_data_dataframe = meta_data_dataframe,
    label_col = label_col,
    study_data_provided = !missing(study_data),
    study_data_is_data_frame = !missing(study_data) && is.data.frame(study_data), # nolint: line_length_linter.
    study_data_length = if (!missing(study_data)) length(study_data) else 0L,
    input_dir = input_dir
  )
  meta_data_cross_item <- metadata_levels$meta_data_cross_item
  meta_data_segment <- metadata_levels$meta_data_segment
  meta_data_dataframe <- metadata_levels$meta_data_dataframe

  # Load "meta_data_item_computation" metadata and complete VARIABLE_LIST ----
  # check if there is a computed_items metadata and if it is a data frame
  # in case is not present, create an empty data frame for computed_items
  try(util_expect_data_frame(meta_data_item_computation),
    silent = TRUE
  )
  if (!is.data.frame(meta_data_item_computation)) {
    util_message(sprintf(
      "No meta_data_item_computation %s found",
      dQuote(meta_data_item_computation)
    ))
    meta_data_item_computation <- data.frame(
      VAR_NAMES = character(0),
      COMPUTATION_RULE = character(0)
    )
  }

  if (VARIABLE_LIST %in% colnames(meta_data_cross_item)) {
    meta_data_cross_item[[VARIABLE_LIST_ORDER]] <-
      meta_data_cross_item[[VARIABLE_LIST]]
  }

  ### create VARIABLE_LIST entries from COMPUTATION_RULE entries
  needles_var_names <- unique(c(
    meta_data[[VAR_NAMES]],
    meta_data[[label_col]],
    meta_data[[LABEL]],
    meta_data[[LONG_LABEL]],
    meta_data[["ORIGINAL_VAR_NAMES"]],
    meta_data[["ORIGINAL_LABEL"]]
  ))
  meta_data_item_computation <- util_report_by_complete_computation_variables(
    meta_data_item_computation,
    variable_names = needles_var_names
  )

  # define vars_in_subgroup----
  vars_in_subgroup <- util_report_by_subgroup_variables(
    subgroup = subgroup,
    variable_names = needles_var_names,
    meta_data = meta_data,
    label_col = label_col
  )

  # Define needed objects for later use----
  # initialize split_segment to FALSE
  # it can be: FALSE if there is no splitting or TRUE otherwise
  split_segments <- FALSE


  # Prepare/filter item_level metadata when argument resp_vars is provided ----
  if (!missing(resp_vars)) {
    # if resp_vars is not VAR_NAMES but LABELS, first turn them to VAR_NAMES
    resp_vars <- vapply(resp_vars, FUN = function(x) {
      if (!x %in% meta_data[[VAR_NAMES]]) {
        x <- util_map_labels(
          x,
          meta_data = meta_data,
          from = label_col,
          to = VAR_NAMES
        )
        names(x) <- NULL
        return(x)
      } else {
        x
      }
    }, FUN.VALUE = character(1))

    cil_in_resp_vars <- util_filter_cross_item_metadata(
      meta_data_cross_item = meta_data_cross_item,
      resp_vars = resp_vars,
      meta_data = meta_data,
      label_col = label_col
    )


    # 2nd: extract all rules containing a resp_vars from item_computation_level
    # extracting the variable names from the column
    # VAR_NAMES in meta_data_item_computation
    comp_vars <- util_map_labels(
      meta_data_item_computation$VAR_NAMES,
      meta_data = meta_data,
      to = label_col,
      from = VAR_NAMES
    )
    # intersect vars in the rules with resp_vars
    computed_to_use <-
      vapply(lapply(comp_vars, intersect, vars),
        length,
        FUN.VALUE = integer(1)
      ) > 0

    # discard rules not affected by resp_vars
    computed_in_resp_vars <- meta_data_item_computation[
      computed_to_use, ,
      drop = FALSE
    ]

    rm(computed_to_use, comp_vars)

    # Obtain all the original plus referred variables and
    # the filtered item_level metadata.
    # All the referred variables added has a variable role = 'suppress'
    overview_referred <- util_referred_vars(
      resp_vars = resp_vars,
      id_vars = id_vars,
      vars_in_subgroup = vars_in_subgroup,
      label_col = label_col,
      meta_data = meta_data,
      meta_data_segment =
        meta_data_segment,
      meta_data_dataframe =
        meta_data_dataframe,
      meta_data_cross_item =
        cil_in_resp_vars,
      meta_data_item_computation =
        computed_in_resp_vars,
      strata_column = strata_column
    )
    meta_data_cross_item <- cil_in_resp_vars
    meta_data_item_computation <- computed_in_resp_vars
    resp_vars_complete <- overview_referred$vars_complete
    meta_data <- overview_referred$md_complete
    attr(meta_data, "normalized") <- TRUE
    attr(meta_data, "version") <- 2

    rm(overview_referred)

    # Modify also meta_data_item_computation to remove non necessary rows
    meta_data_item_computation <-
      meta_data_item_computation[meta_data_item_computation$VAR_NAMES %in%
        resp_vars_complete, , drop = FALSE]
  }


  # Define the split ----
  # if nothing specified, it tries to separate data by STUDY SEGMENT
  # (unless there is a subgroup, or strata_column, or
  # specifically stated by the user "segment_column = NULL"
  # if there is "segment_column = NULL" it is nowhere in the if statement,
  # and it does not create any split
  if ((segment_column_was_missing && is.null(segment_column)) &&
      is.null(strata_column) && is.null(subgroup)) {
    if (STUDY_SEGMENT %in% colnames(meta_data)) {
      segment_column <- STUDY_SEGMENT
    } else {
      util_error(
        paste0(
          "No information for split provided. ",
          "Missing both strata_column, segment_column, and subgroup.",
          " To have an unsplit report please set the ",
          "argument 'segment_column = NULL'"
        ),
        applicability_problem = TRUE
      )
    }
  } else if (is.null(segment_column) &&
      !is.null(strata_column)) {
    # if strata_column specified, check if the variable is in VAR_NAMES and
    # set a new object with the label if possible
    if (label_col_provided != VAR_NAMES && !is.null(strata_column)) {
      strata_column1 <- strata_column
      strata_column <- try(
        util_map_labels(
          strata_column,
          meta_data, VAR_NAMES,
          label_col
        ),
        silent = TRUE
      )
      # if strata_column is a VAR_NAME then there is an error instead of a
      # vector in strata_column, so replace with the original value
      if (!is.vector(strata_column)) {
        strata_column <- strata_column1
      }
      rm(strata_column1)
      if (!strata_column %in% meta_data[[VAR_NAMES]]) {
        util_error(
          paste0(
            "The strata_column provided does not correpond ",
            "to any variable in the item_level_metadata"
          ),
          applicability_problem = TRUE
        )
      }
      strata_column_label <- util_map_labels(
        strata_column,
        meta_data,
        label_col,
        VAR_NAMES
      )
    } else if (label_col_provided == VAR_NAMES && !is.null(strata_column)) {
      if (!strata_column %in% meta_data[VAR_NAMES]) {
        util_error(
          paste0(
            "The strata_column provided does not correpond ",
            "to any variable in the item_level_metadata"
          ),
          applicability_problem = TRUE
        )
      }
      strata_column_label <- util_map_labels(
        strata_column,
        meta_data, label_col,
        VAR_NAMES
      )
    }
    # if only segment_column is provided, check if the value corresponds to
    # a column in the item_level_metadata
  } else if (!is.null(segment_column) &&
      is.null(strata_column)) {
    if (!(segment_column %in% colnames(meta_data))) {
      util_error(
        "No metadata attribute %s found for segmenting DQ report.",
        dQuote(segment_column)
      )
    }
    # if both segment_column and strata_column are provided
    # check if the segment_column provided corresponds to a column
    # in the item_level_metadata and
    # check if the variable of strata_column is in VAR_NAMES and
    # set a new object with the label if possible
  } else if (!is.null(segment_column) &&
      !is.null(strata_column)) {
    if (!(segment_column %in% colnames(meta_data))) {
      util_error(
        "No metadata attribute %s found for segmenting DQ report.",
        dQuote(segment_column)
      )
    }
    if (label_col_provided != VAR_NAMES && !is.null(strata_column)) {
      strata_column1 <- strata_column
      strata_column <- try(
        util_map_labels(
          strata_column,
          meta_data, VAR_NAMES,
          label_col
        ),
        silent = TRUE
      )
      # if strata_column is a VAR_NAME then there is an error instead of a
      # vector in strata_column, so replace with the original value

      if (!is.vector(strata_column)) {
        strata_column <- strata_column1
      }
      rm(strata_column1)
      if (!strata_column %in% meta_data[, VAR_NAMES, drop = TRUE]) {
        util_error(
          paste0(
            "The strata_column provided does not correpond ",
            "to any variable in the item_level_metadata"
          ),
          applicability_problem = TRUE
        )
      }
      strata_column_label <- util_map_labels(
        strata_column,
        meta_data, label_col,
        VAR_NAMES
      )
    } else if (label_col_provided == VAR_NAMES && !is.null(strata_column)) {
      if (!strata_column %in% meta_data[, VAR_NAMES, drop = TRUE]) {
        util_error(
          paste0(
            "The strata_column provided does not correpond ",
            "to any variable in the item_level_metadata"
          ),
          applicability_problem = TRUE
        )
      }
      strata_column_label <- util_map_labels(
        strata_column,
        meta_data, label_col,
        VAR_NAMES
      )
    }
  }

  # Define the study data----
  # If the user starts from a prepared/mapped ds1, continue with the original
  # study data kept in attr(., "study_data"). This keeps dq_report_by aligned
  # with raw-data calls for strata selection and metadata matching, both of
  # which use VAR_NAMES. If the caller already passed a type-adjusted ds1, the
  # pre-adjustment values cannot be reconstructed at this point.
  if (!missing(study_data) &&
      is.data.frame(study_data) &&
      isTRUE(util_attr(study_data, "MAPPED", exact = TRUE))) {
    raw_study_data <- util_attr(study_data, "study_data", exact = TRUE)
    raw_study_data <- util_raw_study_data_attr_subset(
      ds1 = study_data,
      raw_study_data = raw_study_data,
      meta_data = meta_data
    )
    if (!is.data.frame(raw_study_data)) {
      util_error(
        c(
          "The prepared study data attribute %s is missing or stale.",
          "Please pass raw study data or rebuild the prepared data frame."
        ),
        sQuote("study_data"),
        applicability_problem = TRUE
      )
    }
    study_data <- raw_study_data
  }

  # if the study_data argument is specified by the user
  if (!missing(study_data)) {
    study_data_info <- util_report_by_collect_study_data(
      study_data = study_data,
      study_data_expr = util_report_by_study_data_expr(
        substitute(study_data)
      ),
      input_dir = input_dir
    )
    study_data <- study_data_info$study_data
    name_of_study_data <- study_data_info$name_of_study_data
    dataframe_names <- study_data_info$dataframe_names
    list_sd_columns <- study_data_info$list_sd_columns
    rm(study_data_info)
  } else if (missing(study_data) &&
      is.data.frame(meta_data_dataframe)) {
    ### case 3: no study data name provided by the user, use
    #meta_data_dataframe----
    # import the study_data names from meta_data_dataframe

    # Following 4 rowsNot needed, It is not possible to arrive here without a
    # valid DF_NAME
    # check if the column DF_NAME is not empty
    #    if (all(is.na(meta_data_dataframe$DF_NAME))) {
    #      util_error("Column %s in dataframe_level metadata can not be empty.",
    #                 sQuote("DF_NAME"))
    #    }

    # vector of the names present in the dataframe_level metadata
    dataframe_names <- meta_data_dataframe[, DF_NAME, drop = TRUE]

    # if there are information about the DATAFRAMES and DF_CODE
    if ((DF_CODE) %in% colnames(meta_data_dataframe) &&
        DATAFRAMES %in% colnames(meta_data)) {
      # Create a vector with the name of the study_data as before with
      # the actual path but also with the relative DF_CODE
      study_data_withcode <- meta_data_dataframe[, c(DF_NAME, DF_CODE),
        drop = FALSE
      ]
      list_sd_columns <- NULL
    } else {
      # if there are NO information about the DATAFRAMES and DF_CODE
      # import only headers of study data files
      list_sd_columns <- lapply(setNames(nm = dataframe_names), function(nm) {
        columns_df <-
          try(
            prep_get_data_frame(nm,
              column_names_only = TRUE,
              keep_types = TRUE
            ),
            silent = TRUE
          )
        if (util_is_try_error(columns_df)) {
          columns_df <- NULL
        }
        columns_df
      })
      # import works also with URLs in combination with n_max = 0 and nrows = 0
      list_sd_columns <- lapply(list_sd_columns, colnames)
      names(list_sd_columns) <- dataframe_names
    }
    # stop if no study data provided and no dataframe_level metadata available
    # or dataframe_level metadata is empty
  } else if (missing(study_data) &&
      (!is.data.frame(meta_data_dataframe) ||
          nrow(meta_data_dataframe) == 0)) {
    util_error(
      c(
        "Not possible to create reports as no study data and no",
        "dataframe level metadata %s with study",
        "data names are available"
      ),
      dQuote(meta_data_dataframe)
    )
  }

  # Define name_of_study_data to NULL if it does not exist yet----
  if (!exists("name_of_study_data")) {
    name_of_study_data <- NULL
  }


  # Prepare the metadata depending on the presence of a segment_column or
  # not----
  ### 1st case: the segment_column is present----
  if (!is.null(segment_column)) {
    # if there are empty entries in the column defined for the split, set
    # a new non-used segment name (e.g., na1) and use it for empty rows
    split_segments <- TRUE
    segment_info <- util_report_by_segment_names(
      meta_data = meta_data,
      segment_column = segment_column,
      segment_select = segment_select,
      segment_exclude = segment_exclude,
      label_col = label_col,
      label_col_provided = label_col_provided
    )
    meta_data <- segment_info$meta_data
    segment_names <- segment_info$segment_names
    rm(segment_info)


    # Filter cross-item level for the rules that can contain variables
    # in the segment (cross-item_level already normalized)
    # extract all rules containing a variable from the current evaluated segment
    cil_in_segment <- lapply(
      setNames(segment_names, nm = segment_names),
      function(segment) {
        vars <-
          meta_data[meta_data[[segment_column]] ==
            segment, VAR_NAMES]
        # replace var_names with labels
        vars <- util_map_labels(vars,
          meta_data = meta_data,
          to = label_col,
          from = VAR_NAMES
        )
        # extracting the variable names from the column
        # variable_list in cross-item metadata to have
        # a vector of variable names for each rule in the
        # list rules_vars
        rules_vars <-
          util_parse_assignments(
            meta_data_cross_item$VARIABLE_LIST,
            multi_variate_text = TRUE
          )
        # intersect vars in the rules with vars
        # in the segment to discard rules not
        # affecting the current segment
        rules_to_use <-
          vapply(lapply(rules_vars, intersect, vars),
            length,
            FUN.VALUE = integer(1)
          ) > 0
        meta_data_cross_item[rules_to_use, , drop = FALSE]
      }
    )

    # Filter computed items to extract all rules to compute
    # containing a VAR_NAMES from the current evaluated segment
    computed_in_segment <-
      lapply(
        setNames(segment_names, nm = segment_names),
        function(segment) {
          vars <-
            meta_data[meta_data[[segment_column]] ==
              segment, VAR_NAMES]
          # replace var_names with labels
          vars <- util_map_labels(vars,
            meta_data = meta_data,
            to = label_col,
            from = VAR_NAMES
          )
          # extracting the variable names from the column
          # VAR_NAMES in meta_data_item_computation
          comp_vars <- util_map_labels(
            meta_data_item_computation$VAR_NAMES,
            meta_data = meta_data,
            to = label_col,
            from = VAR_NAMES
          )
          # intersect vars in the rules with vars
          # in the segment to see which VAR_NAMES affect the
          # current segment
          computed_to_use <-
            vapply(lapply(comp_vars, intersect, vars),
              length,
              FUN.VALUE = integer(1)
            ) > 0
          # discard rules not affected by the
          # current segment
          meta_data_item_computation[computed_to_use, , drop = FALSE]
        }
      )

    # Select only the data frames interested by the current segment
    ### if there is a dataframe level metadata----
    if (is.data.frame(meta_data_dataframe) &&
        nrow(meta_data_dataframe) > 0) { # this is introduced in the case of
      # study_data argument filled and so the dataframe level will be ignored

      # check if the column DF_NAME is not empty
      if (all(is.na(meta_data_dataframe$DF_NAME))) {
        util_error(
          "Column %s in dataframe_level metadata can not be empty.",
          sQuote("DF_NAME")
        )
      }

      # extract the data frame per segment
      #### caseA: there are DF_CODE and DATAFRAMES: select the dataframes ----
      # based on the DF_CODE of the variables in the segment
      if ((DF_CODE) %in% colnames(meta_data_dataframe) &&
          DATAFRAMES %in% colnames(meta_data)) {
        dfr_in_segment <-
          lapply(setNames(segment_names, nm = segment_names), function(segment) { # nolint: line_length_linter.
            vars <-
              meta_data[meta_data[[segment_column]] ==
                segment, c(VAR_NAMES, DATAFRAMES), drop = FALSE]

            dfr_code_from_item_level <- unique(unname(unlist(
              util_parse_assignments(
                vars$DATAFRAMES,
                split_char = SPLIT_CHAR,
                multi_variate_text = TRUE
              )
            )))

            dfr_to_use <-
              meta_data_dataframe[meta_data_dataframe[[DF_CODE]] %in%
                dfr_code_from_item_level, , FALSE]
          })
      } else {
        #### caseB: there are no DF_CODE and DATAFRAMES ----
        dfr_in_segment <- util_report_by_dataframes_by_segment(
          segment_names = segment_names,
          meta_data = meta_data,
          segment_column = segment_column,
          list_sd_columns = list_sd_columns,
          dataframe_names = dataframe_names,
          meta_data_dataframe = meta_data_dataframe
        )
      }
    } else if (!is.data.frame(meta_data_dataframe) ||
        (is.data.frame(meta_data_dataframe) &&
            nrow(meta_data_dataframe) == 0)) {
      ### in case there is no dataframe metadata or it is empty----
      if (length(study_data) == 1 || is.data.frame(study_data)) {
        meta_data_dataframe <- util_dataframe_metadata_for_names(
          name_of_study_data,
          include_df_code = FALSE,
          include_df_id_vars = FALSE
        )
        dfr_in_segment <-
          lapply(setNames(segment_names, nm = segment_names), function(segment) { # nolint: line_length_linter.
            util_dataframe_metadata_for_names(name_of_study_data)
          })
      } else if (!is.data.frame(study_data) &&
          length(study_data) > 1) {
        meta_data_dataframe <- util_dataframe_metadata_for_names(
          dataframe_names
        )

        dfr_in_segment <-
          util_report_by_dataframes_by_segment(
            segment_names = segment_names,
            meta_data = meta_data,
            segment_column = segment_column,
            list_sd_columns = list_sd_columns,
            dataframe_names = dataframe_names,
            meta_data_dataframe = meta_data_dataframe
          )
      }
    }

    # Prepare segment level meta_data, selecting the row matching
    # the current segment
    # if the segment_column is study_segment, separate it by segment
    if (segment_column == STUDY_SEGMENT) {
      seg_in_segment <-
        lapply(setNames(segment_names, nm = segment_names), function(segment) {
          meta_data_segment[meta_data_segment[[STUDY_SEGMENT]] ==
              segment, , drop = FALSE]
        })
    } else {
      # in case the segment_column is not the segment, need to first
      # create a list with the corresponding segments for each segment_column
      if (!STUDY_SEGMENT %in% colnames(meta_data)) {
        # In case there is no STUDY_SEGMENT
        meta_data[[STUDY_SEGMENT]] <- "all"
      }
      segments_list <- meta_data[, c(segment_column, STUDY_SEGMENT),
        drop = FALSE
      ]
      segments_list <- unique(segments_list)
      segments_list <-
        lapply(setNames(segment_names, nm = segment_names), function(piece) {
          segments_list[segments_list[segment_column] ==
              piece, STUDY_SEGMENT, drop = TRUE]
        })
      seg_in_segment <-
        lapply(setNames(segment_names, nm = segment_names), function(x) {
          meta_data_segment[meta_data_segment[[STUDY_SEGMENT]] %in%
              c(segments_list[[x]]), , drop = FALSE]
        })
      # in case of segment_select not present in the data, there will be
      # elements $<NA> in the list. Remove them
      seg_in_segment <- seg_in_segment[!is.na(names(seg_in_segment))]
    }


    segment_items <- util_report_by_segment_items(
      segment_names = segment_names,
      meta_data = meta_data,
      segment_column = segment_column,
      resp_vars = if (!missing(resp_vars)) resp_vars else NULL,
      id_vars = id_vars,
      vars_in_subgroup = vars_in_subgroup,
      label_col = label_col,
      seg_in_segment = seg_in_segment,
      dfr_in_segment = dfr_in_segment,
      cil_in_segment = cil_in_segment,
      computed_in_segment = computed_in_segment,
      strata_column = strata_column
    )
    vars_in_segment <- segment_items$vars_in_segment
    md_in_segment <- segment_items$md_in_segment
    resp_vars_in_segment <- segment_items$resp_vars_in_segment
    rm(segment_items)
  } else {
    #### 2nd case: the segment_column is not present (NULL),----
    # include all variables and all metadata in category all_variables
    # create a list anyways but only with 1 element for all cross-item metadata
    cil_in_segment <- list(all_variables = meta_data_cross_item)

    # create a list anyways but only with 1 element for all computed vars md
    computed_in_segment <- list(all_variables = meta_data_item_computation)

    # Prepare metadata at the segment level
    meta_data_segment <- util_ensure_segment_metadata(
      meta_data_segment = meta_data_segment,
      meta_data = meta_data,
      validate_study_segment = TRUE
    )
    seg_in_segment <- list(all_variables = meta_data_segment)

    # prepare the dataframe_level metadata, put all in a list element
    # names "all_variables" if present, if not create an empty one
    if (is.data.frame(meta_data_dataframe)) {
      dfr_in_segment <- list(all_variables = meta_data_dataframe)
    } else {
      meta_data_dataframe <- util_dataframe_metadata_for_names(
        dataframe_names
      )
      dfr_in_segment <- list(all_variables = meta_data_dataframe)
    }

    # prepare the list of variables in a list containing all the variables
    # in the item_level metadata
    vars_in_segment <- list(all_variables = meta_data[[VAR_NAMES]])

    # create resp_vars_in_segment <- character(0)
    if (!missing(resp_vars)) {
      resp_vars_in_segment <- list(all_variables = resp_vars)
    } else {
      resp_vars_in_segment <- list(all_variables = character(0))
    }


    # Add all the meta_data (item_level)
    md_in_segment <- list(all_variables = meta_data)

    # Addition for progressing bar
    segment_names <- "all_variables"
  }


  # if no study data was specified by the user, set it to NULL -----
  # this fixes an issue in the following overall loop when it is missing
  if (missing(study_data)) {
    study_data <- NULL
  }


  # Calculate no. strata for the job progress bar----
  if (is.null(strata_column)) {
    n_strata <- 1
  } else {
    expected_strata <- util_report_by_expected_strata(
      meta_data = meta_data,
      strata_column = strata_column
    )


    # in case there is only a data frame as study_data
    if (length(list_sd_columns) == 1) {
      # if the dataframe already exists in the current environment assign it
      # otherwise import it
      tab1 <- util_expect_data_frame(names(list_sd_columns), dont_assign = TRUE)
      original_strata <- unique(tab1[[strata_column]])
      # check if all expected strata are present in the study data
      if (length(setdiff(names(expected_strata), original_strata)) > 0) {
        util_warning(
          c(
            "The stratum/strata %s is/are ",
            "not present in the study data"
          ),
          dQuote(names(expected_strata)[!(names(expected_strata) %in%
                  original_strata)])
        )
      }
      # check if in the study data there are strata not expected
      # removed NA from check
      if (length(setdiff(
        original_strata[!is.na(original_strata)],
        names(expected_strata)
      ) > 0)) {
        util_warning(
          "The stratum/strata %s is/are not present in the metadata",
          dQuote(original_strata[!(original_strata %in%
                  names(expected_strata))])
        )
      }
      n_strata <- length(original_strata)
      # in case the there are more dataframes as study_data
    } else if (length(list_sd_columns) > 1) {
      # obtain the names of the needed study data and select
      # only dataframes containing variables of this segment
      tab_to_import <-
        vapply(lapply(list_sd_columns, intersect, strata_column),
          length,
          FUN.VALUE = integer(1)
        ) > 0
      tab_to_import <- names(tab_to_import[tab_to_import %in% TRUE])

      if (length(tab_to_import) == 1) {
        tab1 <- util_expect_data_frame(tab_to_import, dont_assign = TRUE)
        original_strata <- unique(tab1[[strata_column]])
      } else if (length(tab_to_import) > 1) {
        tabs <- lapply(setNames(nm = tab_to_import),
          util_expect_data_frame,
          dont_assign = TRUE
        )
        tab1 <- Reduce(function(x, y) {
          merge(x, y, all = TRUE)
        }, tabs)
        original_strata <- unique(tab1[[strata_column]])
      }
      n_strata <- length(original_strata)
    } else if (is.null(list_sd_columns)) {
      # In case in which the DF_CODE and DATAFRAME are present
      # obtain the names of the needed study data and select
      # only dataframes containing variables of strata_column
      dtf_code_of_strata_col <- meta_data[
        meta_data[[VAR_NAMES]] %in%
          strata_column,
        c(VAR_NAMES, DATAFRAMES),
        drop = FALSE
      ]


      dfr_code_of_strata_col <- unique(unname(unlist(
        util_parse_assignments(
          dtf_code_of_strata_col$DATAFRAMES,
          split_char = SPLIT_CHAR,
          multi_variate_text = TRUE
        )
      )))

      tab_to_import <-
        study_data_withcode[study_data_withcode[[DF_CODE]] %in%
          dfr_code_of_strata_col, DF_NAME, drop = TRUE]


      if (length(tab_to_import) == 1) {
        tab1 <- util_expect_data_frame(tab_to_import, dont_assign = TRUE)
        original_strata <- unique(tab1[[strata_column]])
      } else if (length(tab_to_import) > 1) {
        tabs <- lapply(setNames(nm = tab_to_import),
          util_expect_data_frame,
          dont_assign = TRUE
        )
        tab1 <- Reduce(function(x, y) {
          merge(x, y, all = TRUE)
        }, tabs)
        original_strata <- unique(tab1[[strata_column]])
      }
      n_strata <- length(original_strata)
    }

    # Define the strata in case strata_select is used
    if (!is.null(strata_select)) {
      strata_select <- util_report_by_selection_values(strata_select)

      # if there is a strata_select argument
      if (!is.null(selection_type)) {
        if (selection_type == "value") {
          util_stop_if_not(any(strata_select %in% original_strata),
            label =
              paste0(
                "No values in the variable correspond ",
                "to the strata_select"
              )
          )
          original_strata <-
            original_strata[original_strata %in% strata_select]
          n_strata <- length(original_strata)
        } else if (selection_type == "v_label") {
          util_stop_if_not(any(strata_select %in% expected_strata),
            label =
              paste0(
                "No value label in the variable ",
                "correspond to the strata_select"
              )
          )
          expected_strata <-
            expected_strata[expected_strata %in% strata_select]
          original_strata <-
            original_strata[original_strata %in% names(expected_strata)]
          n_strata <- length(original_strata)
        } else if (selection_type == "regex") {
          name_pattern_strata <-
            original_strata[grepl(strata_select, original_strata)]
          name_pattern_strata2 <- expected_strata[grepl(
            strata_select,
            expected_strata
          )]

          name_pattern_strata <- unique(c(
            name_pattern_strata,
            names(name_pattern_strata2)
          ))

          util_stop_if_not(length(name_pattern_strata) > 0,
            label =
              paste0(
                "No value or value label in the variable",
                " correspond to the strata_select"
              )
          )
          n_strata <- length(name_pattern_strata)
        }
      } else {
        # if selection_type is null, try to guess the typed strata
        if (!any(strata_select %in% original_strata)) {
          # if the strata does not match the list from data
          if (!any(strata_select %in% expected_strata)) {
            # if the strata does not match the value list labels
            # if the argument is not present in the level names, check if it is
            # a pattern
            name_pattern_strata <-
              original_strata[grepl(strata_select, original_strata)]
            name_pattern_strata <- c(
              name_pattern_strata,
              expected_strata[grepl(
                strata_select,
                expected_strata
              )]
            )
            if (length(name_pattern_strata) == 0) {
              util_error(
                "%s does not corresponds to any strata",
                dQuote(strata_select)
              )
            } else {
              n_strata <- length(name_pattern_strata)
            }
          } else {
            # if the strata matches a value label
            expected_strata <-
              expected_strata[expected_strata %in% strata_select]
            original_strata <-
              original_strata[original_strata %in% names(expected_strata)]
            n_strata <- length(original_strata)
          }
        } else {
          # if the strata matches a value of the variable in the data
          original_strata <-
            original_strata[original_strata %in% strata_select]
          n_strata <- length(original_strata)
        }
      }
    }

    ## Define the strata in case strata_exclude is used
    if (!is.null(strata_exclude)) {
      strata_exclude <- util_report_by_selection_values(strata_exclude)
      # if there is a strata_exclude argument
      if (!is.null(selection_type)) {
        if (selection_type == "value") {
          util_stop_if_not(any(strata_exclude %in% original_strata),
            label =
              paste0(
                "No values in the variable correspond",
                " to the strata_exclude"
              )
          )
          original_strata <-
            original_strata[!(original_strata %in% strata_exclude)]
          n_strata <- length(original_strata)
        } else if (selection_type == "v_label") {
          util_stop_if_not(any(strata_exclude %in% expected_strata),
            label =
              paste0(
                "No value label in the variable ",
                "correspond to the strata_exclude"
              )
          )
          expected_strata <-
            expected_strata[!(expected_strata %in% strata_exclude)]
          original_strata <-
            original_strata[original_strata %in% names(expected_strata)]
          n_strata <- length(original_strata)
        } else if (selection_type == "regex") {
          unwanted_seg_orig <-
            original_strata[grepl(strata_exclude, original_strata)]
          unwanted_seg_label <- expected_strata[grepl(
            strata_exclude,
            expected_strata
          )]
          unwanted_seg_label <- original_strata[original_strata %in%
              names(unwanted_seg_label)]
          unwanted_seg <- unique(c(unwanted_seg_orig, unwanted_seg_label))
          rm(unwanted_seg_orig, unwanted_seg_label)

          name_pattern_strata <- setdiff(original_strata, unwanted_seg)
          util_stop_if_not(length(unwanted_seg) > 0,
            label =
              paste0(
                "No value or value label in the variable",
                " correspond to the strata_exclude"
              )
          )
          n_strata <- length(name_pattern_strata)
        }
      } else {
        # if selection_type is null, try to guess the typed strata
        if (!any(strata_exclude %in% original_strata)) {
          # if the strata does not match the list from data
          if (!any(strata_exclude %in% expected_strata)) {
            # if the strata does not match the value list labels
            # if the argument is not present in the level names, check if it is
            # a pattern
            unwanted_seg_orig <-
              original_strata[grepl(strata_exclude, original_strata)]
            unwanted_seg_label <- expected_strata[grepl(
              strata_exclude,
              expected_strata
            )]
            unwanted_seg_label <- original_strata[original_strata %in%
                names(unwanted_seg_label)]
            unwanted_seg <- unique(c(unwanted_seg_orig, unwanted_seg_label))
            rm(unwanted_seg_orig, unwanted_seg_label)

            name_pattern_strata <- setdiff(original_strata, unwanted_seg)
            if (length(unwanted_seg) == 0) {
              util_error(
                "%s does not corresponds to any strata",
                dQuote(strata_exclude)
              )
            } else {
              n_strata <- length(name_pattern_strata)
            }
          } else {
            # if the strata matches a value label
            expected_strata <-
              expected_strata[!(expected_strata %in% strata_exclude)]
            original_strata <-
              original_strata[original_strata %in% names(expected_strata)]
            n_strata <- length(original_strata)
          }
        } else {
          # if the strata matches a value of the variable in the data
          original_strata <-
            original_strata[!(original_strata %in% strata_exclude)]
          n_strata <- length(original_strata)
        }
      }
    }
  }

  # Create a job to report progress and give it a name ----
  util_setup_rstudio_job("dq_report_by",
    n = length(segment_names) * n_strata
  )
  # create an extra environment for the progress
  p <- new.env(parent = emptyenv())
  p$i <- 0
  p$N <- length(segment_names) * n_strata

  # OUTER list: split base on segment----
  # Return a list of lists of results. The outer list is for the split based on
  # segment_column e.g., study segments. The inner list is for the strata
  # (split based on the strata_column)
  overall_res <- mapply(
    # here starts the outer loop by segment_column (by segment or another
    # column)
    vars_in_segment = vars_in_segment[order(names(vars_in_segment))],
    cur_seg = sort(names(vars_in_segment)),
    meta_data = md_in_segment[order(names(md_in_segment))],
    meta_data_dataframe = dfr_in_segment[order(names(dfr_in_segment))],
    seg_in_segment = seg_in_segment[order(names(seg_in_segment))],
    MoreArgs = list(
      list_sd_columns = list_sd_columns,
      name_of_study_data = name_of_study_data,
      call_report_by = call_report_by,
      subgroup = subgroup,
      resp_vars_in_segment = resp_vars_in_segment,
      id_vars = id_vars,
      vars_in_subgroup = vars_in_subgroup
    ),
    SIMPLIFY = FALSE,
    FUN = function(vars_in_segment,
      cur_seg,
      meta_data,
      meta_data_dataframe,
      list_sd_columns,
      seg_in_segment,
      name_of_study_data,
      call_report_by,
      subgroup,
      resp_vars_in_segment,
      id_vars,
      vars_in_subgroup) {
      attr(meta_data, "normalized") <- TRUE
      attr(meta_data, "version") <- 2

      # filter resp_vars_in_segment for the right segment if present
      # in case there is only a data frame as study_data
      if (length(list_sd_columns) == 1) {
        dataframe_names_cur_seg <- names(list_sd_columns)
        # if the study_data = object
        if (is.data.frame(study_data)) {
          dataframes_cur_seg <- study_data
          name_files <-
            head(as.character(substitute(study_data)), 1)
        } else {
          # import the study data files matching the name
          dataframes_cur_seg <-
            util_expect_data_frame(names(list_sd_columns), dont_assign = TRUE)
          name_files <- paste(names(list_sd_columns))
        }

        # in case there is only one study data in the data frame level
        if (is.null(name_of_study_data)) {
          name_of_study_data <- name_files
        }
        # in case the there are more data frames as study_data
      } else if (length(list_sd_columns) > 1 ||
          is.null(list_sd_columns)) {
        dataframe_names_cur_seg <- util_report_by_segment_dataframe_names(
          vars_in_segment = vars_in_segment,
          meta_data = meta_data,
          list_sd_columns = list_sd_columns,
          study_data_withcode = study_data_withcode
        )


        dataframes_cur_seg <- util_report_by_load_segment_dataframes(
          dataframe_names = dataframe_names_cur_seg
        )

        # create a name for the study data merging all the
        # data frames names, after ordering and making them unique
        dataframe_names_cur_seg <- sort(unique(dataframe_names_cur_seg))
        name_of_study_data <- paste(dataframe_names_cur_seg, collapse = ", ")
      }


      dfr_in_segment <- util_report_by_segment_dataframe_metadata(
        meta_data_dataframe = meta_data_dataframe,
        dataframe_names = dataframe_names_cur_seg,
        segment_name = cur_seg
      )

      # Create sd_merge and filter for var in segment if needed----
      # if there is no need to merge files
      if (length(list_sd_columns) == 1) {
        sd_merged <- dataframes_cur_seg
        sd_merged <- util_report_by_keep_segment_vars(
          study_data = sd_merged,
          vars_in_segment = vars_in_segment
        )
      } else if (length(list_sd_columns) > 1 ||
          is.null(list_sd_columns)) {
        # there is need to merge files
        # if the files are specified from user (length(study_data) > 1) or
        # there is no argument study_data
        if (is.null(list_sd_columns)) {
          # if there is no argument study_data
          # get id vars of the dataframe level metadata for merging
          # (only from dataframe and NOT from segment)
          dfr_in_segment <- dfr_in_segment[[1]]
          id_variable <- util_report_by_segment_id_vars(
            dfr_in_segment = dfr_in_segment,
            id_vars = id_vars
          )

          if ((DF_CODE) %in% colnames(dfr_in_segment) &&
              DATAFRAMES %in% colnames(meta_data)) {
            # In case there is a DATAFRAMES and DF_CODE, filter the data frames
            # imported in dataframe_cur_seg,
            # so that the var_names comes from the right data frame
            dataframes_cur_seg <- util_report_by_filter_segment_dataframes(
              dataframes = dataframes_cur_seg,
              dataframe_names = dataframe_names_cur_seg,
              dfr_in_segment = dfr_in_segment,
              meta_data = meta_data,
              vars_in_segment = vars_in_segment,
              study_data_withcode = study_data_withcode,
              id_vars = id_vars
            )
          }
        } else {
          id_variable <- id_vars
        }

        # If a segment is empty, do not create a report
        if (length(dataframes_cur_seg) == 0) {
          util_warning(sprintf(
            "No data available for the segment %s",
            sQuote(cur_seg)
          ))
          return(NULL)
        }


        # Before merging select only variables in the current segment
        dataframes_cur_seg <- lapply(
          dataframes_cur_seg,
          util_report_by_keep_segment_vars,
          vars_in_segment = vars_in_segment
        )

        sd_merged <- util_report_by_merge_segment_dataframes(
          dataframes = dataframes_cur_seg,
          id_vars = id_variable
        )
      }

      # Filter sd_merged by subgroup keeping only needed records----
      sd_merged <- util_report_by_filter_subgroup(
        study_data = sd_merged,
        meta_data = meta_data,
        subgroup = subgroup
      )

      scale_level <- util_amend_scale_level_once(
        study_data = sd_merged,
        meta_data = meta_data,
        label_col = label_col
      )
      meta_data <- scale_level$meta_data
      rm(scale_level)

      # Run shared report preparation once for the whole current segment.
      # Missing-code amendments, computed-variable creation, and metadata
      # normalization are row-wise/metadata operations; doing them before the
      # strata split avoids repeating expensive work for every stratum while
      # still keeping memory use low because strata are stored as row indices.
      prepared_segment_inputs <- util_prepare_dataquieR_inputs(
        study_data = sd_merged,
        meta_data = meta_data,
        label_col = label_col,
        meta_data_cross_item = cil_in_segment[[cur_seg]],
        meta_data_item_computation = computed_in_segment[[cur_seg]],
        name_of_study_data = name_of_study_data,
        update_registry = FALSE
      )
      sd_merged <- prepared_segment_inputs$study_data
      meta_data <- prepared_segment_inputs$meta_data
      meta_data_item_computation_cur_seg <-
        prepared_segment_inputs$meta_data_item_computation
      rm(prepared_segment_inputs)

      # Define the strata_column ----
      # If no argument to split is present, create a list anyway with
      # all_observations. Store row indices instead of split data frames:
      # for large reports, split(data.frame) eagerly creates one data-frame
      # copy per stratum. The row-index list is tiny and lets us materialize
      # only the current stratum immediately before calling dq_report2().
      .sd_list <- util_report_by_strata_rows(
        study_data = sd_merged,
        strata_column = strata_column
      )
      if (!is.null(strata_column) &&
          strata_column %in% colnames(sd_merged)) {
        if (!is.null(strata_select) || !is.null(strata_exclude)) {
          expected_strata <- util_report_by_expected_strata(
            meta_data = meta_data,
            strata_column = strata_column
          )
        }

        ## filtering for the study_data_strata (vector of names or regexp)
        if (!is.null(strata_select)) {
          ## definition of selection_type and strata_select
          if (!is.null(selection_type)) {
            if (selection_type == "value") {
              # check if the typed levels exist in the segment_column possible
              # levels before to select it
              if (!any(strata_select %in% names(.sd_list))) {
                # stop if selection does not match any strata
                util_error(
                  c(
                    "No strata_column stratum matches the",
                    "provided names: %s"
                  ),
                  dQuote(strata_select)
                )
              } else {
                .sd_list <- .sd_list[strata_select]
                # in case empty, remove empty from list
                # and give a warning
                .sd_list <- .sd_list[!is.na(names(.sd_list))]
                if (length(strata_select) > length(.sd_list)) {
                  util_warning(c(
                    "One or more strata were not",
                    "present in the study_data"
                  ))
                }
              }
            } else if (selection_type == "v_label") {
              value_of_strata_select <-
                expected_strata[expected_strata %in% strata_select]
              value_of_strata_select <- names(value_of_strata_select)
              # check if the typed levels exist in the segment_column possible
              # levels before to select it
              if (length(value_of_strata_select) == 0 &&
                  !any(value_of_strata_select %in% names(.sd_list))) {
                # stop if selection does not match any strata
                util_error(
                  c(
                    "No strata_column stratum matches the",
                    "provided names: %s"
                  ),
                  dQuote(strata_select)
                )
              } else {
                .sd_list <- .sd_list[value_of_strata_select]
                # in case a strata is not present, remove empty from list
                # and give a warning
                .sd_list <- .sd_list[!is.na(names(.sd_list))]
                if (length(value_of_strata_select) > length(.sd_list)) {
                  util_warning(c(
                    "One or more strata were not",
                    "present in the study_data"
                  ))
                }
              }
            } else if (selection_type == "regex") {
              all_labels_in_sd_list <-
                expected_strata[names(expected_strata) %in% names(.sd_list)]
              value_of_strata_select <-
                all_labels_in_sd_list[grepl(strata_select, all_labels_in_sd_list)] # nolint: line_length_linter.
              if (length(names(.sd_list)[grepl(
                strata_select,
                names(.sd_list)
              )]) > 0) {
                # regex match at least one strata
                .sd_list <- .sd_list[grepl(strata_select, names(.sd_list))]
                .sd_list <- .sd_list[!is.na(names(.sd_list))]
              } else if (length(names(.sd_list)[names(.sd_list) %in%
                      names(value_of_strata_select)]) > 0) {
                .sd_list <- .sd_list[names(.sd_list)[names(.sd_list) %in%
                      names(value_of_strata_select)]]
                .sd_list <- .sd_list[!is.na(names(.sd_list))]
              } else {
                util_error(
                  c(
                    "No strata_column stratum matches the",
                    "provided regular expression: %s"
                  ),
                  dQuote(strata_select)
                )
              }
            }
          } else {
            # if  selection_type is null
            # give a warning and then try to find matches
            util_message(
              c(
                "The argument selection_type is not provided, ",
                "the function will try to find any matches with value, ",
                "value labels or possible regular expressions"
              )
            )
            # check if the strata_select matches a names(.sd_list)
            if (any(strata_select %in% names(.sd_list))) {
              .sd_list <- .sd_list[strata_select]
              # in case present, remove empty from list
              .sd_list <- .sd_list[!is.na(names(.sd_list))]
            } else if (any(strata_select %in% expected_strata)) {
              # in case the strata_select matches a label of names(.sd_list)
              labels_strata_select <- expected_strata[expected_strata %in%
                  strata_select]
              value_of_strata_select <- names(labels_strata_select)
              .sd_list <- .sd_list[value_of_strata_select]
              # in case present, remove empty from list
              .sd_list <- .sd_list[!is.na(names(.sd_list))]
            } else if (length(names(.sd_list)[grepl(
              strata_select,
              names(.sd_list)
            )]) > 0) {
              # regex matches a value
              .sd_list <- .sd_list[names(.sd_list)[grepl(
                strata_select,
                names(.sd_list)
              )]]
            } else {
              labels_strata_select <-
                expected_strata[grepl(strata_select, expected_strata)]

              if (length(labels_strata_select) > 0) {
                value_of_strata_select <- names(labels_strata_select)
                .sd_list <- .sd_list[value_of_strata_select]
                # in case present, remove empty from list
                .sd_list <- .sd_list[!is.na(names(.sd_list))]
              } else {
                # it does not maches any labels or anything else
                util_error(
                  c(
                    "No strata_column stratum matches the",
                    "provided names: %s"
                  ),
                  dQuote(strata_select)
                )
              }
            }
          }
        }

        ## remove the strata to exclude
        if (!is.null(strata_exclude)) {
          if (!is.null(selection_type)) {
            if (selection_type == "value") {
              # check if the typed levels exist in the segment_column possible
              # levels before to select it
              if (!any(strata_exclude %in% names(.sd_list))) {
                # stop if selection does not match any strata
                util_error(
                  c(
                    "No strata_column stratum matches the",
                    "strata to exclude: %s"
                  ),
                  dQuote(strata_exclude)
                )
              } else {
                # remove strata_exclude elements from list
                .sd_list <- .sd_list[names(.sd_list) %in% strata_exclude == FALSE] # nolint: line_length_linter.
                # stop if removing all strata
                util_stop_if_not(length(.sd_list) > 0,
                  label = paste0(
                    "No strata remain after",
                    " excluding selected ones"
                  )
                )
              }
            } else if (selection_type == "v_label") {
              value_of_strata_select <-
                expected_strata[expected_strata %in% strata_exclude]
              value_of_strata_select <- names(value_of_strata_select)
              # check if the typed levels exist in the segment_column possible
              # levels before to select it
              if (length(value_of_strata_select) == 0 &&
                  !any(value_of_strata_select %in% names(.sd_list))) {
                # stop if selection does not match any strata
                util_error(
                  c(
                    "No strata_column stratum matches the",
                    "strata_exclude: %s"
                  ),
                  dQuote(strata_exclude)
                )
              } else {
                .sd_list <- .sd_list[names(.sd_list) %in%
                    value_of_strata_select == FALSE]
                # stop if no strata remain
                if (length(.sd_list) == 0) {
                  util_error(
                    c(
                      "No strata_column stratum remains",
                      "after removing strata_exclude: %s"
                    ),
                    dQuote(strata_exclude)
                  )
                }
              }
            } else if (selection_type == "regex") {
              all_labels_in_sd_list <-
                expected_strata[names(expected_strata) %in% names(.sd_list)]
              value_of_strata_select <-
                all_labels_in_sd_list[grepl(
                  strata_exclude,
                  all_labels_in_sd_list
                )]
              if (length(names(.sd_list)[grepl(
                strata_exclude,
                names(.sd_list)
              )]) > 0) {
                # regex match at least one strata - remove the strata
                .sd_list <- .sd_list[!grepl(strata_exclude, names(.sd_list))]
                # stop if no strata remain
                if (length(.sd_list) == 0) {
                  util_error(
                    c(
                      "No strata_column stratum remains",
                      "after removing strata_exclude: %s"
                    ),
                    dQuote(strata_exclude)
                  )
                }
              } else if (length(names(.sd_list)[names(.sd_list) %in%
                      names(value_of_strata_select)]) > 0) {
                .sd_list <- .sd_list[names(.sd_list) %in%
                    names(value_of_strata_select) == FALSE]
              } else {
                util_error(
                  c(
                    "No strata_column stratum matches the",
                    "provided regular expression: %s"
                  ),
                  dQuote(strata_exclude)
                )
              }
            }
          } else {
            # if  selection_type is null
            # give a warning and then try to find matches
            util_warning(
              c(
                "The argument selection_type not provided, ",
                "the function will try to find any matches with value, ",
                "value labels or possible regular expressions"
              )
            )
            # check if the strata_exclude matches a names(.sd_list)
            if (any(strata_exclude %in% names(.sd_list))) {
              # remove strata_exclude elements from list
              .sd_list <- .sd_list[names(.sd_list) %in% strata_exclude == FALSE]
              # stop if removing all strata
              util_stop_if_not(length(.sd_list) > 0,
                label = "No strata remain after excluding selected ones"
              )
            } else if (any(strata_exclude %in% expected_strata)) {
              # in case the strata_exclude matches a label of names(.sd_list)
              value_of_strata_select <-
                expected_strata[expected_strata %in% strata_exclude]
              value_of_strata_select <- names(value_of_strata_select)
              # check if the typed levels exist in the segment_column possible
              # levels before to select it
              if (length(value_of_strata_select) == 0 &&
                  !any(value_of_strata_select %in% names(.sd_list))) {
                # stop if selection does not match any strata
                util_error(
                  c(
                    "No strata_column stratum matches the",
                    "strata_exclude: %s"
                  ),
                  dQuote(strata_exclude)
                )
              } else {
                .sd_list <- .sd_list[names(.sd_list) %in%
                    value_of_strata_select == FALSE]
                # stop if no strata remain
                if (length(.sd_list) == 0) {
                  util_error(
                    c(
                      "No strata_column stratum remains",
                      "after removing strata_exclude: %s"
                    ),
                    dQuote(strata_exclude)
                  )
                }
              }
            } else {
              # in case no matches found for value or v_label try regex
              all_labels_in_sd_list <-
                expected_strata[names(expected_strata) %in% names(.sd_list)]
              value_of_strata_select <-
                all_labels_in_sd_list[grepl(
                  strata_exclude,
                  all_labels_in_sd_list
                )]
              # if the regex matches a value
              if (length(names(.sd_list)[grepl(
                strata_exclude,
                names(.sd_list)
              )]) > 0) {
                # regex match at least one strata - remove the strata
                .sd_list <- .sd_list[!grepl(strata_exclude, names(.sd_list))]
                # stop if no strata remain
                if (length(.sd_list) == 0) {
                  util_error(
                    c(
                      "No strata_column stratum remains",
                      "after removing strata_exclude: %s"
                    ),
                    dQuote(strata_exclude)
                  )
                }
                # if the regex matches a v_label
              } else if (length(names(.sd_list)[names(.sd_list) %in%
                      names(value_of_strata_select)]) > 0) {
                .sd_list <- .sd_list[names(.sd_list) %in%
                    names(value_of_strata_select) == FALSE]
              } else {
                # if the regex also has no matches
                util_error(
                  c(
                    "No strata_column stratum matches the",
                    "provided strata_exclude: %s"
                  ),
                  dQuote(strata_exclude)
                )
              }
            }
          }
        }

        # convert names of the groups
        # to VAR_NAMES_actualGroupValues
        names(.sd_list) <- paste0(strata_column_label, "_", names(.sd_list))
      }

      # INNER loop starts here: split by strata_column ----
      mapply(
        sd = .sd_list,
        sdn = names(.sd_list),
        MoreArgs = list(
          md = meta_data,
          sd_merged = sd_merged,
          name_of_study_data = name_of_study_data,
          call_report_by = call_report_by,
          resp_vars_in_segment = resp_vars_in_segment,
          meta_data_item_computation_cur_seg =
            meta_data_item_computation_cur_seg
        ),
        SIMPLIFY = FALSE,
        FUN = function(sd,
          sdn,
          md,
          sd_merged,
          name_of_study_data,
          call_report_by,
          resp_vars_in_segment,
          meta_data_item_computation_cur_seg) {
          # import a list containing original study data (too long), and
          # new assigned names, in case it was created in a previous apply,
          # otherwise create an empty one
          if (exists("..INFO_SD_NAME_FOR_REPORT", .dataframe_environment())) {
            info_sd_name <- prep_get_data_frame("..INFO_SD_NAME_FOR_REPORT",
              keep_types = TRUE
            )
            info_sd_name <- as.list(info_sd_name)
          } else {
            info_sd_name <- list()
          }
          # Define a new name composed of study_data_current
          # stratum_current segment
          level_name <- sdn

          # message on progresses
          util_message(sprintf(
            "Segment %s, Stratum %s..",
            sQuote(cur_seg),
            sQuote(level_name)
          ))
          progress_msg(sprintf(
            "Segment %s, Stratum %s...",
            sQuote(cur_seg),
            sQuote(level_name)
          ))
          # update the progress bar
          p$i <- p$i + 1
          progress(100 * p$i / p$N)
          .outer_by_env$outer_by <- list(
            i = p$i,
            n = p$N,
            msg = sprintf(
              "Segment %s, Stratum %s...",
              sQuote(cur_seg),
              sQuote(level_name)
            )
          )

          # add the study data name that are too long to a list
          # containing original study data names and
          # new assigned names
          if (!is.null(name_of_study_data) && (
            startsWith(name_of_study_data, "https://") ||
              startsWith(name_of_study_data, "http://") ||
              startsWith(name_of_study_data, "ftp://") ||
              startsWith(name_of_study_data, "ftps://") ||
              startsWith(name_of_study_data, "dbx://") ||
              nchar(name_of_study_data) > 20
          )) {
            # if not already present in the list,
            # add the name to a general list for the overview
            if (!name_of_study_data %in%
                unlist(info_sd_name, use.names = FALSE)) {
              info_sd_name <-
                c(info_sd_name, setNames(as.list(name_of_study_data),
                  nm = paste0("SD", length(
                    info_sd_name
                  ) + 1)
                ))
            }

            # add the name to a local list for the report
            info_sd_name_vector <- setNames(names(info_sd_name), info_sd_name)
            info_sd_name_vector <-
              info_sd_name_vector[names(info_sd_name_vector) ==
                name_of_study_data]
            # turn it to a list to be used as arg. in dq_report2
            info_sd_name_per_report <-
              setNames(as.list(names(info_sd_name_vector)), info_sd_name_vector)
          } else {
            info_sd_name_vector <- NULL
            info_sd_name_per_report <- NULL
          }
          # use abbreviation SD# instead of study data file names in folder
          # names
          if (!is.null(info_sd_name_vector)) {
            name_of_study_data <- info_sd_name_vector[name_of_study_data]
          }

          # Define a new name composed of study_data_current
          # stratum_current segment
          sdn <- paste0(name_of_study_data, "_", sdn, "_", cur_seg)

          subtitle_report <- paste0("Study data: ", name_of_study_data)

          if (!is.null(subgroup)) {
            subtitle_report <- paste0(subtitle_report, " Subgroup: ", subgroup)
          }


          #     }
          # update the cache with the new list info_sd_name for the overview
          if (length(info_sd_name) != 0) {
            ..INFO_SD_NAME_FOR_REPORT <- data.frame(info_sd_name)
            prep_add_data_frames(
              "..INFO_SD_NAME_FOR_REPORT" =
                ..INFO_SD_NAME_FOR_REPORT
            )
          }
          title_report <-
            paste0("Data quality report on ", level_name, ".", cur_seg)

          # Created to later clean the cache to improve speed using function
          # prep_remove_from_cache
          old_dataframes <- prep_list_dataframes()

          # add missing tables specified as arguments
          if (!is.null(missing_tables)) {
            suppressMessages(lapply(
              missing_tables,
              FUN = function(x) {
                assign(x, get(x), envir = .dataframe_environment())
                invisible(.dataframe_environment())
              }
            ))
          }

          # in case of study_data being used as argument indicating the name of
          # the data frame, saved it here to prevent its change in the cache
          # during running the function dq_report2, and restored after
          if ("study_data" %in% names(.dataframe_environment())) {
            saved_study_data <- prep_get_data_frame("study_data",
              keep_types = TRUE
            )
          } else {
            saved_study_data <- NULL
          }

          if (!is.null(storr_factory)) {
            storr_factory_clone <- rlang::duplicate(storr_factory)
            environment(storr_factory_clone) <-
              rlang::env_clone(environment(storr_factory_clone))
            ns <- get("namespace", environment(storr_factory))
            ns <- paste0(ns, "/", sdn)
            assign("namespace", ns, environment(storr_factory_clone))
          } else {
            storr_factory_clone <- NULL
          }


          # If a segment is empty, do not create a report.
          if (is.null(sd) || length(sd) == 0) {
            util_warning(sprintf(
              "No data available to create the report %s",
              sQuote(title_report)
            ))
            return(NULL)
          }
          sd <- sd_merged[sd, , drop = FALSE]
          sd <- util_mark_dataquieR_inputs_prepared(sd)

          allowed_args <- names(formals(dataquieR::dq_report2))

          clean_dots <- dots[names(dots) %in% allowed_args]

          ui <- info_sd_name_per_report

          if (!is.null(user_info) && is.list(user_info)) {
            ui <- user_info
            ui[names(info_sd_name_per_report)] <- info_sd_name_per_report
          }

          args <- c(
            list(
              study_data = sd,
              meta_data = md,
              resp_vars = resp_vars_in_segment[[cur_seg]],
              meta_data_cross_item = cil_in_segment[[cur_seg]],
              meta_data_segment = seg_in_segment[[cur_seg]],
              meta_data_dataframe = dfr_in_segment[[1]],
              meta_data_item_computation = meta_data_item_computation_cur_seg,
              label_col = label_col,
              split_segments = split_segments,
              user_info = ui,
              title = title_report,
              subtitle = subtitle_report,
              storr_factory = storr_factory_clone,
              amend = amend,
              checkpoint_resumed = checkpoint_resumed,
              name_of_study_data = name_of_study_data,
              author = author
            ),
            clean_dots
          )

          args$output_dir <- NULL

          # create the different sub-reports ----
          r <- try(local({
            .dataquieR_report_call_override <- call_report_by_overview
            do.call(dq_report2, args)
          }))

          ### Check here if it is an error, put r <- NULL, empty report()
          attr(r, "label_modification_text") <- trimws(paste(
            util_attr(r, "label_modification_text", exact = TRUE),
            mod_label$label_modification_text
          ))

          attr(r, "label_modification_table") <-
            rbind(
              util_attr(r, "label_modification_table", exact = TRUE),
              mod_label$label_modification_table
            )


          # Define subgroup for technical information
          if (!is.null(subgroup)) {
            subgroup_info <- subgroup
          } else {
            subgroup_info <- "Not specified"
          }

          # Add information in the "Technical information" part of the report
          attr(r, "properties") <- c(
            util_attr(r, "properties", exact = TRUE),
            list(
              Study_data = name_of_study_data,
              Segment = cur_seg,
              Stratum = level_name,
              Subgroup = subgroup_info
            )
          )

          rm(sd_merged, sd, md, subgroup_info)
          # remove the files creating by the dq_report function from the cache
          new_dataframes <- prep_list_dataframes()
          names_to_remove <- setdiff(new_dataframes, old_dataframes)
          suppressMessages(prep_remove_from_cache(names_to_remove))
          # restore the study_data as mentioned before running the report
          suppressMessages(prep_remove_from_cache("study_data"))
          if (!is.null(saved_study_data)) {
            prep_add_data_frames(study_data = saved_study_data)
          }
          gc()

          attr(r, "report_by_info") <- list(
            sdn = sdn,
            level_name = level_name,
            name_of_study_data = name_of_study_data,
            cur_seg = cur_seg
          )
          report_output <- util_report_by_output(
            r = r,
            output_dir = output_dir,
            sdn = sdn,
            level_name = level_name,
            name_of_study_data = name_of_study_data,
            cur_seg = cur_seg,
            also_print = also_print,
            dots = dots,
            disable_plotly = disable_plotly,
            advanced_options = advanced_options,
            html_table_backend = html_table_backend,
            view_meta_data = meta_data,
            force_overwrite = force_overwrite
          )
          rm(r)
          gc()
          return(report_output)
        }
      )
    }
  )

  report_by_meta_names <- c(
    "strata_column",
    "segment_column",
    "strata_column_label",
    "subgroup",
    "mod_label",
    "disable_plotly",
    "advanced_options",
    "html_table_backend",
    "title",
    "start_time",
    "rep_id",
    "subtitle",
    "author",
    "user_info",
    "by_call",
    "call_report_by",
    "call_report_by_overview"
  )
  if (has_output_dir && dir.exists(output_dir)) {
    call_report_by_overview <- util_compact_dq_report_by_call_from_env(
      environment()
    )
    util_report_by_meta(
      output_dir = output_dir,
      names = report_by_meta_names
    )
  }

  # create the html overview page that links all sub-reports created -----
  if (has_output_dir && also_print) {
    util_create_report_by_overview(
      output_dir = output_dir
    )
  }
  prep_purge_data_frame_cache()

  report_files <- if (has_output_dir) {
    sort(list.files(output_dir, pattern = "^report_.*[.]dq2$"))
  } else {
    character()
  }
  if (has_output_dir) {
    stored_meta <- readRDS(file.path(output_dir, "report_by_meta.RDS"))
    stored_meta$report_files <- report_files
    saveRDS(stored_meta, file.path(output_dir, "report_by_meta.RDS"))
  }
  report_by_meta <- if (has_output_dir) {
    list()
  } else {
    util_report_by_meta_values(report_by_meta_names, environment())
  }
  overall_res <- util_new_dataquieR_report_by(
    overall_res,
    output_dir = output_dir,
    meta = report_by_meta,
    report_files = report_files
  )
  if (has_output_dir) {
    util_message(
      paste0(
        "The returned report bundle is disk-backed. Keep %s readable ",
        "to print the bundle later."
      ),
      dQuote(output_dir)
    )
  }

  if (!has_output_dir) {
    if (view) {
      return(overall_res)
    } else {
      return(invisible(overall_res))
    }
  } else {
    # util_view_file(content_file) is handled by the HTML progress exit hook.
    return(invisible(overall_res))
  }
}

# return a list of .hi, .hp, and .hm, the handles for the
# progress hooks for init, progress and progress-messages
# Use list2env() locally to bind the returned progress hooks if needed.
#' Internal helper: init html progress
#'
#' @noRd
util_init_html_progress <- function(output_dir, content_file, title, view,
  rep_id, start_time = Sys.time(), logo_rel = "logo.png") {
  util_seed_last_error()
  force(start_time)
  packageName <- utils::packageName()
  hook_store <- new.env(parent = emptyenv())
  hook_store$n <- Inf
  hook_store$percent <- 0
  hook_store$status <- "Initializing"
  hook_store$msg <- ""
  hook_store$finished <- FALSE
  completion_file <- file.path(output_dir, ".report", "render-complete")

  hook <- function(n = hook_store$n,
    percent = hook_store$percent,
    status = hook_store$status,
    msg = hook_store$msg) {
    if (!missing(n)) {
      hook_store$n <- n
    }
    if (!missing(percent)) {
      hook_store$percent <- percent
    }
    if (!missing(status)) {
      hook_store$status <- status
    }
    if (!missing(msg)) {
      hook_store$msg <- msg
    }
    util_write_index_html(
      content_file,
      util_index_loading_lines(
        title = title,
        message = hook_store$status,
        detail = hook_store$msg,
        reload_ms = 1200L,
        n = hook_store$n,
        percent = hook_store$percent,
        logo_rel = logo_rel
      )
    )
    util_write_renderinfo_js_json(
      output_dir = output_dir,
      rep_id = rep_id,
      start_time = start_time,
      end_time = Sys.time()
    )
  }

  .hi <- prep_register_progress_hook(
    type = "init",
    hook
  )
  withr::defer_parent(prep_deregister_progress_hook(.hi, verbose = FALSE))
  .hp <- prep_register_progress_hook(
    type = "progress",
    hook
  )
  withr::defer_parent(prep_deregister_progress_hook(.hp, verbose = FALSE))
  .hm <- prep_register_progress_hook(
    type = "msg",
    hook
  )
  withr::defer_parent(prep_deregister_progress_hook(.hm, verbose = FALSE))

  if (!dir.exists(file.path(output_dir, ".report"))) {
    if (!dir.create(file.path(output_dir, ".report"))) {
      util_error(
        "Could not create %s for HTML output",
        dQuote(file.path(output_dir, ".report"))
      )
    }
  }
  unlink(completion_file, force = TRUE)

  file.copy(
    system.file("logos",
      "dataquieR_48x48.png",
      package = packageName
    ),
    file.path(output_dir, ".report", "logo.png")
  )

  util_write_index_html(
    content_file,
    util_index_loading_lines(
      title = title,
      message = hook_store$status,
      detail = hook_store$msg,
      reload_ms = 1200L,
      n = hook_store$n,
      percent = hook_store$percent,
      logo_rel = logo_rel
    )
  )
  util_write_renderinfo_js_json(
    output_dir = output_dir,
    rep_id = rep_id,
    start_time = start_time,
    end_time = Sys.time()
  )
  withr::defer_parent({
    cl <- ""
    cl <- suppressWarnings(try(readLines(content_file), silent = TRUE))
    report_file <- file.path(output_dir, ".report", "report.html")
    report_written <- file.exists(report_file) &&
      isTRUE(file.info(report_file)$size > 0L)
    index_finished <- any(grepl(fixed = TRUE, "<!-- done -->", cl))
    if (report_written && !index_finished) {
      util_write_index_html(
        content_file,
        util_index_redirect_lines(
          title = title,
          target_rel = ".report/report.html",
          delay_ms = 300L,
          logo_rel = logo_rel
        )
      )
    } else if (!isTRUE(hook_store$finished) && !file.exists(completion_file) &&
        !index_finished) {
      util_write_index_html(
        content_file,
        util_index_error_lines(
          title = title,
          message = "Report was not created... it was cancelled or an error occurred.", # nolint: line_length_linter.
          logo_rel = logo_rel
        )
      )
      # Use dirname(content_file) and util_pretty_vector_string() locally when
      # debugging missing done markers.
      unlink(file.path(
        dirname(content_file), ".report", "renderinfo.js"
      )) # this is the marker for an error
    }
  })

  if (view) {
    if (util_works_in_rs_viewer(content_file)) {
      # RStudio has a deadlock, if the autorefresh runs in its viewer,
      # whatever, we have progressbars there, anyways.
      withr::defer_parent({
        cl <- ""
        cl <- suppressWarnings(try(readLines(content_file), silent = TRUE))
        if (!any(grepl(fixed = TRUE, "<!-- done -->", cl))) {
          if (!util_is_last_error_sentinel()) {
            p <- NULL
            try(p <- rlang::last_error(), silent = TRUE)
            rlang::abort("Cancelled.",
              parent = p
            )
          } else {
            util_error("Cancelled.")
          }
        } else {
          util_view_file(content_file)
        }
      })
    } else {
      util_view_file(content_file)
    }
  }
  list(
    .hi = .hi,
    .hp = .hp,
    .hm = .hm,
    .hf = function() {
      hook_store$finished <- TRUE
      if (!file.create(completion_file)) {
        util_error("Could not write report completion marker")
      }
      invisible(completion_file)
    }
  )
}

#' Internal helper: seed last error
#'
#' @noRd
util_seed_last_error <- function(context = "pipeline phase started") {
  cnd <- util_attr(try(
    rlang::abort(
      message = context,
      class = "my_last_error_sentinel"
    ),
    silent = TRUE
  ), "condition", exact = TRUE)
  rlang::entrace(cnd)
}

#' Internal helper: is last error sentinel
#'
#' @noRd
util_is_last_error_sentinel <- function() {
  err <- tryCatch(rlang::last_error(), error = function(e) NULL)
  inherits(err, "my_last_error_sentinel")
}
