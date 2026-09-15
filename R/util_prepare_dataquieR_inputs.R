#' Register the current primary study data
#'
#' Make the current report input available under `"study_data"` and the
#' user-facing `name_of_study_data` in the regular data-frame registry. This is
#' needed for pipeline code and dataframe-level integrity metadata that refer to
#' the supplied study data by name. Heavily curated study-data variants are not
#' registered here; those belong to `.study_data_cache`, where
#' `dataquieR.study_data_cache_max` can limit RAM use.
#'
#' @param study_data data frame to register.
#' @param name_of_study_data user-facing registry name.
#'
#' @return Invisibly returns the result of [prep_add_data_frames()].
#'
#' @noRd
util_register_primary_study_data <- function(study_data,
  name_of_study_data) {
  util_expect_data_frame(study_data, keep_types = TRUE)
  if (!isTRUE(util_attr(study_data, "dataquieR_data_frame_keep_types",
        exact = TRUE
      ))) {
    attr(study_data, "dataquieR_data_frame_keep_types") <- TRUE
  }

  # Keep the user-facing alias for dataframe-level integrity metadata. These
  # checks may refer to the originally supplied study-data name even if the
  # data frame was passed directly as an argument and not loaded from the cache.
  cache_names <- c(name_of_study_data, "study_data")
  cache_names <- unique(cache_names[nzchar(cache_names)])
  prep_add_data_frames(data_frame_list = setNames(
    rep(list(study_data), length(cache_names)),
    cache_names
  ))
}

#' Mark inputs that passed shared report preparation
#'
#' @param x data frame or `NULL`.
#'
#' @return `x`, with an internal preparation marker for data frames.
#'
#' @noRd
util_mark_dataquieR_inputs_prepared <- function(x) {
  if (is.data.frame(x)) {
    attr(x, "dataquieR_inputs_prepared") <- TRUE
  }
  x
}

#' Test whether shared report preparation already ran
#'
#' @inheritParams .template_function_developer
#'
#' @return `TRUE` if both data frames carry the internal preparation marker.
#'
#' @noRd
util_dataquieR_inputs_prepared <- function(study_data, meta_data) {
  identical(
    util_attr(study_data, "dataquieR_inputs_prepared", exact = TRUE),
    TRUE
  ) &&
    identical(
      util_attr(meta_data, "dataquieR_inputs_prepared", exact = TRUE),
      TRUE
    )
}

#' Internal helper: clean relevant var names
#'
#' @noRd
util_clean_relevant_var_names <- function(relevant_var_names) {
  if (is.null(relevant_var_names)) {
    return(character(0))
  }
  if (is.list(relevant_var_names)) {
    relevant_var_names <- unlist(
      lapply(relevant_var_names, util_clean_relevant_var_names),
      recursive = TRUE,
      use.names = FALSE
    )
  } else if (!is.character(relevant_var_names)) {
    return(character(0))
  }
  relevant_var_names[!util_empty(relevant_var_names)]
}

#' Internal helper: map relevant var names
#'
#' @noRd
util_map_relevant_var_names <- function(relevant_var_names,
  meta_data,
  label_col) {
  relevant_var_names <- util_clean_relevant_var_names(relevant_var_names)
  try(
    relevant_var_names <- util_find_var_by_meta(
      relevant_var_names,
      meta_data = meta_data,
      label_col = label_col,
      target = VAR_NAMES,
      allowed_sources = c(VAR_NAMES, LABEL, LONG_LABEL, label_col),
      ifnotfound = relevant_var_names
    ),
    silent = TRUE
  )
  unique(relevant_var_names)
}

#' Internal helper: prepare relevant meta data
#'
#' @noRd
util_prepare_relevant_meta_data <- function(meta_data,
  relevant_var_names,
  label_col) {
  relevant_var_names <- util_map_relevant_var_names(
    relevant_var_names = relevant_var_names,
    meta_data = meta_data,
    label_col = label_col
  )
  meta_data <- util_validate_relevant_meta_uniqueness(
    meta_data,
    relevant_var_names = relevant_var_names
  )
  list(meta_data = meta_data, relevant_var_names = relevant_var_names)
}

#' Internal helper: fill empty label columns
#'
#' @noRd
util_fill_empty_label_columns <- function(meta_data, label_col) {
  for (lcol in unique(c(label_col, LABEL, LONG_LABEL))) {
    if (is.data.frame(meta_data) && lcol %in% colnames(meta_data)) {
      empty_labels <- which(util_empty(meta_data[[lcol]]))
      meta_data[[lcol]][empty_labels] <- meta_data[[VAR_NAMES]][empty_labels]
    }
  }
  meta_data
}

#' Prepare item-level metadata for shared dataquieR input handling
#'
#' @inheritParams .template_function_developer
#' @param cause_label_df [data.frame] optional missing code table passed
#'   through to `prep_meta_data_v1_to_item_level_meta_data()`.
#'
#' @return Normalized item-level metadata with stable row names.
#'
#' @details This is the metadata-only part of the shared input preparation. It
#'   deliberately does not replace `prep_prepare_dataframes()`, which also
#'   prepares study data according to the caller-specific flags.
#'
#' @noRd
util_prepare_item_level_metadata <- function(meta_data, label_col,
  cause_label_df) {
  util_expect_data_frame(meta_data)
  util_expect_scalar(label_col, check_type = is.character)

  meta_data <- util_remove_identical_meta_rows(meta_data)
  has_direct_value_labels <- VALUE_LABELS %in% colnames(meta_data) &&
    any(!util_empty(meta_data[[VALUE_LABELS]]))
  if (!identical(util_attr(meta_data, "normalized", exact = TRUE), TRUE) ||
      !identical(util_attr(meta_data, "version", exact = TRUE), 2) ||
      has_direct_value_labels) {
    if (missing(cause_label_df)) {
      meta_data <- prep_meta_data_v1_to_item_level_meta_data(
        meta_data = meta_data,
        label_col = label_col,
        verbose = FALSE
      )
    } else {
      meta_data <- prep_meta_data_v1_to_item_level_meta_data(
        meta_data = meta_data,
        label_col = label_col,
        cause_label_df = cause_label_df,
        verbose = FALSE
      )
    }
  }
  meta_data <- util_normalize_clt(meta_data = meta_data)
  rownames(meta_data) <- NULL
  meta_data
}

#' Ensure valid item-level variable roles
#'
#' @inheritParams .template_function_developer
#'
#' @return Item-level metadata with valid `VARIABLE_ROLE` values.
#'
#' @noRd
util_ensure_variable_roles <- function(meta_data, label_col) {
  util_expect_data_frame(meta_data)
  util_expect_scalar(label_col, check_type = is.character)

  if (!(VARIABLE_ROLE %in% colnames(meta_data))) {
    util_message("No %s assigned in item level metadata. Defaulting to %s.",
      sQuote(VARIABLE_ROLE), dQuote(VARIABLE_ROLES$PRIMARY),
      applicability_problem = TRUE
    )
    meta_data[[VARIABLE_ROLE]] <- VARIABLE_ROLES$PRIMARY
  }

  which_invalid <- !(meta_data[[VARIABLE_ROLE]] %in% VARIABLE_ROLES)
  if (any(which_invalid)) {
    util_message(
      c(
        "The variables %s have no or an invalid %s assigned in item level",
        "metadata: %s are not in %s. Defaulting to %s."
      ),
      util_pretty_vector_string(
        n_max = 5,
        meta_data[which_invalid, label_col, drop = TRUE]
      ),
      sQuote(VARIABLE_ROLE),
      dQuote(util_pretty_vector_string(sort(unique(
        meta_data[which_invalid, VARIABLE_ROLE, drop = TRUE]
      )))),
      util_pretty_vector_string(VARIABLE_ROLES),
      dQuote(VARIABLE_ROLES$PRIMARY),
      applicability_problem = TRUE
    )
    meta_data[[VARIABLE_ROLE]][which_invalid] <- VARIABLE_ROLES$PRIMARY
  }

  meta_data
}

#' Ensure usable cross-item metadata
#'
#' @inheritParams .template_function_developer
#'
#' @return A data frame with the mandatory cross-item metadata columns.
#'
#' @noRd
util_ensure_cross_item_metadata <- function(meta_data_cross_item) {
  try(util_expect_data_frame(meta_data_cross_item), silent = TRUE)
  if (!is.data.frame(meta_data_cross_item)) {
    util_message(
      "No cross-item level metadata %s found",
      dQuote(meta_data_cross_item)
    )
    meta_data_cross_item <- data.frame(
      VARIABLE_LIST = character(0),
      CHECK_LABEL = character(0)
    )
  } else {
    rownames(meta_data_cross_item) <- NULL
  }

  meta_data_cross_item
}

#' Ensure usable segment metadata
#'
#' @inheritParams .template_function_developer
#' @param validate_study_segment whether to require a character
#'   `STUDY_SEGMENT` column when segment metadata are supplied.
#' @param add_segment_id_vars whether to add an empty `SEGMENT_ID_VARS` column.
#'
#' @return Segment-level metadata.
#'
#' @noRd
util_ensure_segment_metadata <- function(meta_data_segment,
  meta_data,
  validate_study_segment = FALSE,
  add_segment_id_vars = FALSE) {
  util_expect_data_frame(meta_data)
  util_expect_scalar(validate_study_segment, check_type = is.logical)
  util_expect_scalar(add_segment_id_vars, check_type = is.logical)

  if (validate_study_segment) {
    try(util_expect_data_frame(
      meta_data_segment,
      col_names = list(STUDY_SEGMENT = is.character)
    ), silent = TRUE)
  } else {
    try(util_expect_data_frame(meta_data_segment), silent = TRUE)
  }
  if (!is.data.frame(meta_data_segment)) {
    util_message(
      "No segment level metadata %s found",
      dQuote(meta_data_segment)
    )
    study_segments <- meta_data[[STUDY_SEGMENT]]
    if (is.null(study_segments)) {
      study_segments <- character(0)
    }
    meta_data_segment <- data.frame(
      STUDY_SEGMENT = unique(study_segments)
    )
  } else {
    rownames(meta_data_segment) <- NULL
  }

  if (add_segment_id_vars && !SEGMENT_ID_VARS %in% names(meta_data_segment)) {
    meta_data_segment[[SEGMENT_ID_VARS]] <- rep(
      NA_character_,
      nrow(meta_data_segment)
    )
  }

  meta_data_segment
}

#' Ensure a grading-ruleset column on entity-level metadata
#'
#' @param meta_data [data.frame] Metadata for one entity level.
#'
#' @return `meta_data` with a normalized character [GRADING_RULESET] column.
#'
#' @noRd
util_ensure_grading_ruleset_metadata <- function(meta_data) {
  util_expect_data_frame(meta_data)

  if (!(GRADING_RULESET %in% colnames(meta_data))) {
    meta_data[[GRADING_RULESET]] <- rep("0", nrow(meta_data))
  } else {
    meta_data[[GRADING_RULESET]] <- trimws(as.character(
      meta_data[[GRADING_RULESET]]
    ))
    meta_data[[GRADING_RULESET]][
      util_empty(meta_data[[GRADING_RULESET]])
    ] <- "0"
  }

  meta_data
}

#' Create empty dataframe-level metadata
#'
#' @param include_df_code whether to include a `DF_CODE` column.
#' @param include_df_id_vars whether to include a `DF_ID_VARS` column.
#'
#' @return An empty dataframe-level metadata table.
#'
#' @noRd
util_empty_dataframe_metadata <- function(include_df_code = TRUE,
  include_df_id_vars = TRUE) {
  util_dataframe_metadata_for_names(
    dataframe_names = character(0),
    include_df_code = include_df_code,
    include_df_id_vars = include_df_id_vars
  )
}

#' Create dataframe-level metadata from data frame names
#'
#' @param dataframe_names names of study data frames.
#' @param include_df_code whether to include a `DF_CODE` column.
#' @param include_df_id_vars whether to include a `DF_ID_VARS` column.
#'
#' @return Dataframe-level metadata with optional code and id columns.
#'
#' @noRd
util_dataframe_metadata_for_names <- function(dataframe_names,
  include_df_code = TRUE,
  include_df_id_vars = TRUE) {
  if (is.null(dataframe_names)) {
    dataframe_names <- character(0)
  }
  util_stop_if_not(
    "`dataframe_names` must be character" = is.character(dataframe_names)
  )
  util_expect_scalar(include_df_code, check_type = is.logical)
  util_expect_scalar(include_df_id_vars, check_type = is.logical)

  meta_data_dataframe <- data.frame(DF_NAME = dataframe_names)
  if (include_df_code) {
    meta_data_dataframe[[DF_CODE]] <- rep(
      NA_character_,
      nrow(meta_data_dataframe)
    )
  }
  if (include_df_id_vars) {
    meta_data_dataframe[[DF_ID_VARS]] <- rep(
      NA_character_,
      nrow(meta_data_dataframe)
    )
  }

  meta_data_dataframe
}

#' Keep cross-item rules relevant to selected report variables
#'
#' @inheritParams .template_function_developer
#'
#' @return The matching rows of `meta_data_cross_item`.
#'
#' @noRd
util_filter_cross_item_metadata <- function(meta_data_cross_item,
  resp_vars,
  meta_data,
  label_col) {
  util_expect_data_frame(meta_data_cross_item)
  util_expect_data_frame(meta_data)
  util_expect_scalar(label_col, check_type = is.character)

  if (!nrow(meta_data_cross_item) || !length(resp_vars)) {
    return(meta_data_cross_item)
  }
  util_expect_scalar(resp_vars,
    allow_more_than_one = TRUE,
    check_type = is.character
  )

  vars <- util_map_labels(
    resp_vars,
    meta_data = meta_data,
    to = label_col,
    from = VAR_NAMES,
    ifnotfound = NA_character_
  )
  rules_vars <- util_parse_assignments(
    meta_data_cross_item[[VARIABLE_LIST]],
    multi_variate_text = TRUE
  )
  rules_to_use <- vapply(
    lapply(rules_vars, intersect, vars),
    length,
    FUN.VALUE = integer(1)
  ) > 0

  meta_data_cross_item[rules_to_use, , drop = FALSE]
}

#' Apply cached missing-code rules to prepared report inputs
#'
#' @inheritParams .template_function_developer
#'
#' @return A list with `study_data` and `meta_data`.
#'
#' @noRd
util_apply_missing_code_rules_once <- function(study_data, meta_data,
  label_col) {
  util_expect_data_frame(study_data, keep_types = TRUE)
  util_expect_data_frame(meta_data)
  util_expect_scalar(label_col, check_type = is.character)

  if (!(MISSING_CODE_RULES %in% prep_list_dataframes())) {
    return(list(study_data = study_data, meta_data = meta_data))
  }

  missing_code_rules <- MISSING_CODE_RULES
  util_expect_data_frame(missing_code_rules)
  missing_code_rules_hash <- rlang::hash(missing_code_rules)
  if (identical(
    util_attr(study_data,
      "dataquieR_missing_code_rules_applied",
      exact = TRUE
    ),
    missing_code_rules_hash
  )) {
    return(list(study_data = study_data, meta_data = meta_data))
  }

  pamc <- try(prep_add_missing_codes(
    use_value_labels = FALSE,
    study_data = study_data,
    meta_data = meta_data,
    rules = missing_code_rules
  ), silent = TRUE)
  if (util_is_try_error(pamc)) {
    cnd <- util_condition_from_try_error(pamc)
    util_warning("Could not amend missing codes based on the sheet %s: %s",
      sQuote(MISSING_CODE_RULES),
      dQuote(conditionMessage(cnd)),
      applicability_problem = TRUE
    )
  } else {
    study_data <- pamc$ModifiedStudyData
    meta_data <- util_prepare_item_level_metadata(
      meta_data = pamc$ModifiedMetaData,
      label_col = label_col
    )
    attr(study_data, "dataquieR_missing_code_rules_applied") <-
      missing_code_rules_hash
  }

  list(study_data = study_data, meta_data = meta_data)
}

#' Internal helper: add computed study data once
#'
#' @noRd
util_add_computed_study_data_once <- function(study_data,
  meta_data,
  label_col,
  meta_data_item_computation,
  computed_signature) {
  computed_study_data <- try(
    {
      res <- prep_add_computed_variables(
        study_data = study_data,
        meta_data = meta_data,
        label_col = label_col,
        rules = meta_data_item_computation
      )
      msd <- res$ModifiedStudyData[, meta_data_item_computation$VAR_NAMES,
        drop = FALSE
      ]
      mapped <- identical(util_attr(res$ModifiedStudyData, "MAPPED",
          exact = TRUE
        ), TRUE)
      if (mapped) {
        mapped_lc <- util_attr(res$ModifiedStudyData, "label_col", exact = TRUE)
        colnames(msd) <- prep_map_labels(colnames(msd),
          meta_data = meta_data,
          to = VAR_NAMES,
          from = mapped_lc,
          ifnotfound = colnames(msd),
          warn_ambiguous = FALSE
        )
      }
      new_columns <- setdiff(colnames(msd), colnames(study_data))
      if (length(new_columns)) {
        study_data[, new_columns] <- msd[, new_columns, drop = FALSE]
      }
      attr(study_data, "dataquieR_computed_variables_applied") <-
        computed_signature
      study_data
    },
    silent = TRUE
  )
  if (util_is_try_error(computed_study_data)) {
    util_warning(
      "%s",
      conditionMessage(util_condition_from_try_error(computed_study_data))
    )
    study_data
  } else {
    computed_study_data
  }
}

#' Apply computed-variable metadata to prepared report inputs
#'
#' @inheritParams .template_function_developer
#'
#' @return A list with `study_data`, `meta_data`, and
#'   `meta_data_item_computation`.
#'
#' @noRd
util_apply_computed_variables_once <- function(study_data,
  meta_data,
  label_col,
  meta_data_cross_item,
  meta_data_item_computation) {
  util_expect_data_frame(study_data, keep_types = TRUE)
  util_expect_data_frame(meta_data)
  util_expect_scalar(label_col, check_type = is.character)

  if (!is.data.frame(meta_data_item_computation)) {
    meta_data_item_computation <- data.frame()
  }
  if (!is.data.frame(meta_data_cross_item)) {
    meta_data_cross_item <- data.frame()
  }

  uaci <- util_add_computed_internals(
    meta_data_item_computation,
    meta_data_cross_item,
    meta_data,
    label_col
  )
  meta_data_item_computation <- uaci$meta_data_item_computation
  meta_data <- uaci$meta_data

  if (is.data.frame(meta_data_item_computation) &&
      !!prod(dim(meta_data_item_computation))) {
    util_message("Computed items metadata defined. Computing them...")
    rownames(meta_data_item_computation) <- NULL
    computed_signature <- rlang::hash(list(
      rules = meta_data_item_computation,
      meta_data = meta_data
    ))
    computed_targets <- meta_data_item_computation[[VAR_NAMES]]
    computed_up_to_date <-
      identical(
        util_attr(study_data,
          "dataquieR_computed_variables_applied",
          exact = TRUE
        ),
        computed_signature
      ) &&
      all(computed_targets %in% colnames(study_data))
    if (!computed_up_to_date) {
      study_data <- util_add_computed_study_data_once(
        study_data = study_data,
        meta_data = meta_data,
        label_col = label_col,
        meta_data_item_computation = meta_data_item_computation,
        computed_signature = computed_signature
      )
    }
  } else {
    meta_data_item_computation <- NULL
  }

  list(
    study_data = study_data,
    meta_data = meta_data,
    meta_data_item_computation = meta_data_item_computation
  )
}

#' Prepare dataquieR report inputs consistently
#'
#' Normalize item-level metadata and apply report-level study-data preparation
#' steps that must be shared between `dq_report2()`, `dq_report_by()`, and
#' standalone indicator calls. The function is intentionally idempotent for
#' expensive steps: missing-code and computed-variable application are guarded
#' by signatures stored as attributes on `study_data`.
#'
#' @inheritParams .template_function_developer
#' @param name_of_study_data registry name for the primary study data.
#' @param update_registry if `TRUE`, register the current primary study data in
#'   the regular data-frame registry.
#' @param relevant_var_names variable names relevant for hard metadata
#'   uniqueness checks.
#'
#' @return A list with `study_data`, normalized `meta_data`, and
#'   `meta_data_item_computation`.
#'
#' @noRd
util_prepare_dataquieR_inputs <- function(study_data,
  meta_data,
  label_col,
  meta_data_cross_item = data.frame(),
  meta_data_item_computation = data.frame(),
  name_of_study_data = "study_data",
  update_registry = TRUE,
  relevant_var_names = NULL) {
  util_expect_data_frame(study_data, keep_types = TRUE)
  util_expect_data_frame(meta_data)
  util_expect_scalar(label_col, check_type = is.character)

  meta_data <- util_prepare_item_level_metadata(
    meta_data = meta_data,
    label_col = label_col
  )
  mod_label <- util_ensure_label(
    meta_data = meta_data,
    label_col = label_col
  )
  meta_data <- mod_label$meta_data
  label_col <- mod_label$label_col
  meta_data <- util_ensure_variable_roles(
    meta_data = meta_data,
    label_col = label_col
  )

  if (!is.null(relevant_var_names)) {
    relevant_meta_data <- util_prepare_relevant_meta_data(
      meta_data = meta_data,
      relevant_var_names = relevant_var_names,
      label_col = label_col
    )
    meta_data <- relevant_meta_data$meta_data
  }

  missing_code_inputs <- util_apply_missing_code_rules_once(
    study_data = study_data,
    meta_data = meta_data,
    label_col = label_col
  )
  study_data <- missing_code_inputs$study_data
  meta_data <- missing_code_inputs$meta_data

  computed_inputs <- util_apply_computed_variables_once(
    study_data = study_data,
    meta_data = meta_data,
    label_col = label_col,
    meta_data_cross_item = meta_data_cross_item,
    meta_data_item_computation = meta_data_item_computation
  )
  study_data <- computed_inputs$study_data
  meta_data <- computed_inputs$meta_data
  meta_data_item_computation <- computed_inputs$meta_data_item_computation

  study_data <- util_mark_dataquieR_inputs_prepared(study_data)
  meta_data <- util_mark_dataquieR_inputs_prepared(meta_data)
  meta_data_item_computation <-
    util_mark_dataquieR_inputs_prepared(meta_data_item_computation)

  if (update_registry) {
    util_register_primary_study_data(
      study_data = study_data,
      name_of_study_data = name_of_study_data
    )
  }

  list(
    study_data = study_data,
    meta_data = meta_data,
    meta_data_item_computation = meta_data_item_computation,
    label_modification_text = mod_label$label_modification_text,
    label_modification_table = mod_label$label_modification_table
  )
}
