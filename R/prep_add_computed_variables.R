# nolint start: line_length_linter.
#' Insert missing codes for `NA`s based on rules
#'
#' @inheritParams .template_function_indicator
#' @param rules [data.frame] with the columns:
#'   - `VAR_NAMES`: [VAR_NAMES] of the variable to compute
#'   - `COMPUTATION_RULE`: A rule in `REDcap` style (see, e.g.,
#'   [`REDcap` help](https://help.redcap.ualberta.ca/help-and-faq/project-best-practices/data-quality/example-data-quality-rules) and
#'   [`REDcap` how-to](https://docs.google.com/document/d/1l3nGBgqqPKi5PtMe75g7q0dny8QzGMd_/edit?tab=t.0))
#'   that defines, how to compute the new values
#'   - `FILE_PATH`: Path to a folder of structured text files (JSON/XML) for generating a variable with a file-based value
#'   - `MERGE_ID_VAR`: ID Variable for matching structured text files to variable rows
#'   - `ID_PATH_REGEX`: How to get an ID for matching from the file path (first regex match group).
#'   - `ID_ENTITY_PATH`: How to find an ID for matching in the JSON/XML file. A jq program or an XPath depending on file type.
#' @param use_value_labels [logical] In rules for factors, use the value labels,
#'                                   not the codes. Defaults to `TRUE`, if any
#'                                   `VALUE_LABELS` are given in the metadata.
#'
#' @return a `list` with the entry:
#'   - `ModifiedStudyData`: Study data with the new variables
#'
#' @export
#'
#' @examples
#' \dontrun{
#' study_data <- prep_get_data_frame("ship")
#' prep_load_workbook_like_file("ship_meta_v2")
#' meta_data <- prep_get_data_frame("item_level")
#' rules <- tibble::tribble(
#'   ~VAR_NAMES, ~COMPUTATION_RULE,
#'   "BMI", "[BODY_WEIGHT_0]/(([BODY_HEIGHT_0]/100)^2)",
#'   "R", "[WAIST_CIRC_0]/2/[pi]", # in m^3
#'   "VOL_EST", "[pi]*([WAIST_CIRC_0]/2/[pi])^2*[BODY_HEIGHT_0] / 1000", # in l
#' )
#' r <- prep_add_computed_variables(study_data, meta_data,
#'   label_col = "LABEL", rules, use_value_labels = FALSE
#' )
#' }
# nolint end
prep_add_computed_variables <- function(
  study_data,
  meta_data,
  label_col,
  rules,
  use_value_labels
) {
  prep_prepare_dataframes(
    .replace_missings = FALSE,
    .replace_hard_limits = FALSE,
    .adjust_data_type = FALSE,
    .amend_scale_level = FALSE,
    .allow_empty = TRUE
  )
  if (!is.data.frame(rules) || !nrow(rules)) {
    return(list(ModifiedStudyData = ds1))
  }

  util_expect_data_frame(
    rules,
    list(
      VAR_NAMES = function(x) {
        !any(x %in% colnames(study_data))
      },
      COMPUTATION_RULE = is.character
    )
  )

  if (any(!util_empty(rules[["FILE_PATH"]]))) {
    file_rules <- rules[!util_empty(rules[["FILE_PATH"]]), , drop = FALSE]
    ds1 <- .util_add_computed_file_paths(
      ds1,
      meta_data,
      label_col,
      file_rules
    )$ModifiedStudyData
    if (nrow(file_rules) < nrow(rules)) {
      rules <- rules[util_empty(rules[["FILE_PATH"]]), , drop = FALSE]
    } else {
      return(list(ModifiedStudyData = ds1))
    }
  }

  #  if(nrow(file_rules)){ TODO: ??
  #     if(!nrow(rules)) {
  #     }
  #  }

  if (DATA_PREPARATION %in% colnames(rules)) {
    rules[[DATA_PREPARATION]] <-
      vapply(
        lapply(
          lapply(
            lapply(
              lapply(
                util_parse_assignments(
                  rules[[DATA_PREPARATION]],
                  multi_variate_text = TRUE
                ),
                toupper
              ),
              trimws
            ),
            sort
          ),
          unique
        ),
        FUN = prep_deparse_assignments,
        mode = "string_codes",
        FUN.VALUE = character(1)
      )
    rules_split <- split(rules, rules[[DATA_PREPARATION]])
    res <- ds1
    for (my_rules in names(rules_split)) {
      to_apply <- unlist(
        util_parse_assignments(my_rules),
        recursive = TRUE,
        use.names = FALSE
      )
      curr_rules <- rules_split[[my_rules]]
      curr_rules[[DATA_PREPARATION]] <- NULL
      replace_missing_by <- ""
      if (
        sum(
          "MISSING_INTERPRET" %in% to_apply,
          "MISSING_LABEL" %in% to_apply,
          "MISSING_NA" %in% to_apply
        ) >
          1
      ) {
        util_warning(
          "Invalid %s in computation rules. Falling back to %s",
          sQuote(DATA_PREPARATION),
          dQuote("MISSING_NA")
        )
        to_apply <- to_apply[!startsWith(to_apply, "MISSING_")]
        to_apply <- c(to_apply, "MISSING_NA")
      }
      if ("MISSING_NA" %in% to_apply) {
        replace_missing_by <- "NA"
      }
      if ("MISSING_INTERPRET" %in% to_apply) {
        replace_missing_by <- "INTERPRET"
      }
      if ("MISSING_LABEL" %in% to_apply) {
        replace_missing_by <- "LABEL"
      }
      res <- .util_add_computed_variables(
        ds1 = res,
        meta_data = meta_data,
        label_col = label_col,
        rules = curr_rules,
        use_value_labels = ("LABEL" %in% to_apply),
        replace_missing_by = replace_missing_by,
        replace_limits = ("LIMITS" %in% to_apply)
      )$ModifiedStudyData
    }
    return(list(ModifiedStudyData = res))
  } else {
    return(.util_add_computed_variables(
      ds1 = ds1,
      meta_data = meta_data,
      label_col = label_col,
      rules = rules,
      use_value_labels = use_value_labels
    ))
  }
}

#' Add computed paths for structured-text files
#'
#' @param ds1 Study data.
#' @param meta_data Item-level metadata.
#' @param label_col The metadata column identifying study variables.
#' @param rules Item-computation rules.
#' @return A list containing the modified study data.
#' @noRd
.util_add_computed_file_paths <- function(ds1, meta_data, label_col, rules) {
  if (!all(has_metadata <- rules[[VAR_NAMES]] %in% meta_data[[VAR_NAMES]])) {
    no_metadata <- rules[!has_metadata, , drop = FALSE]
    util_message(
      paste0(
        "For some variables defined in the item-computation-level,",
        "no item-level meta data has been specified. The following, ",
        "variables are affected: %s"
      ),
      sQuote(no_metadata),
      applicability_problem = TRUE
    )
    rules <- rules[has_metadata, , drop = FALSE]
  }

  mismatch_checktype <- getOption(
    "dataquieR.ELEMENT_MISSMATCH_CHECKTYPE",
    dataquieR.ELEMENT_MISSMATCH_CHECKTYPE_default
  )

  merge_dfs <- mapply(
    FUN = function(
      file_path,
      var_name,
      id_path_regex,
      id_entity_path,
      merge_id_var
    ) {
      # TODO: ask if recursive makes sense
      fp <- file_path
      if (startsWith(fp, "http://") || startsWith(fp, "https://")) {
        fp <- util_load_folder_from_url(fp)
      }
      files <- list.files(
        fp,
        all.files = TRUE,
        recursive = TRUE,
        no.. = TRUE,
        full.names = TRUE
      )

      if (any(util_is_file_hidden(files))) {
        util_message(
          paste0("Some files for item computation level variable %s ",
            "are hidden"),
          dQuote(var_name)
        )
      }

      file_type <- .util_json_or_xml(files)
      if (!(all(file_type == "xml") || all(file_type == "json"))) {
        majority <- "json"
        if (sum(file_type == "xml") > sum(file_type == "json")) {
          majority <- "xml"
        }
        util_message(
          paste0("Files for item computation level variable %s mix JSON ",
            "and XML. Using the majority type (%s) and ignoring the rest."),
          dQuote(var_name),
          majority
        )
        files <- files[file_type == majority]
        file_type <- majority
      }
      file_type <- file_type[[1]]

      if (!util_empty(id_path_regex)) {
        if (!util_empty(id_entity_path)) {
          util_warning(
            paste0(
              "Both ID_PATH_REGEX and ID_ENTITY_PATH are given for ",
              var_name,
              "choosing the former for further processing."
            ),
            applicability_problem = TRUE
          )
        }
        id_getter <- .util_get_regex_file_id
        id_data <- id_path_regex
      } else {
        if (!util_empty(id_entity_path)) {
          id_getter <- .util_make_entity_file_id_getter(file_type[[1]])
          id_data <- id_entity_path
        } else {
          util_error(
            "None of ID_PATH_REGEX and ID_ENTITY_PATH are given for %s",
            var_name,
            applicability_problem = TRUE
          )
        }
      }
      ids <- sapply(
        files,
        function(file_path) {
          id_getter(file_path, id_data)
        },
        simplify = TRUE
      )
      to_merge <- data.frame(a = files, b = ids)

      # TODO: COS: no indicator seems to fit here. Discuss. System missingness?
      #       for util_message(integrity_indicator=(abbreviation from dq_obs)),
      #       when an ID cannot be extracted from a file.
      #   TODO: Data Extraction Error
      if (any(ids == "null") || any(util_empty(ids))) {
        util_warning(
          paste0(
            "Could not extract IDs from all available files,",
            "skipping the following files: %s"
          ),
          paste(
            to_merge[ids == "null" | util_empty(ids), , drop = FALSE]$a,
            collapse = "\n"
          ),
          integrity_indicator = "int_dsc_extraction"
        )
      }

      to_merge <- to_merge[ids != "null" & !util_empty(ids), , drop = FALSE]

      colnames(to_merge) <- c(var_name, merge_id_var) # e.g. v000043, v00001
      colnames(to_merge) <- util_map_labels(
        colnames(to_merge),
        from = VAR_NAMES,
        to = label_col,
        meta_data = meta_data
      )
      id_col <- colnames(to_merge)[[2]]

      if (
        !all(to_merge[[id_col]] %in% ds1[[id_col]]) &&
          (mismatch_checktype == "exact" || mismatch_checktype == "subset_u")
      ) {
        util_warning(
          paste0("Some IDs extracted from folder %s are absent from ",
            "the study data's ID column %s"),
          dQuote(file_path),
          dQuote(merge_id_var),
          integrity_indicator = "int_dsc_record"
        )
      }
      # TODO: Align the indicator exception handling with
      # int_unexpected_data_element_set.
      if (
        !all(ds1[[id_col]] %in% to_merge[[id_col]]) &&
          (mismatch_checktype == "exact" || mismatch_checktype == "subset_m")
      ) {
        util_warning(
          paste0("No file in folder %s matched some study-data IDs ",
            "in column %s"),
          dQuote(file_path),
          dQuote(merge_id_var),
          integrity_indicator = "int_dsc_record"
        )
      }

      to_merge
    },
    rules[["FILE_PATH"]],
    rules[[VAR_NAMES]],
    rules[["ID_PATH_REGEX"]],
    rules[["ID_ENTITY_PATH"]],
    rules[["MERGE_ID_VAR"]],
    SIMPLIFY = FALSE
  )

  data <- Reduce(
    function(prev, curr) {
      merge(
        prev,
        curr,
        all.x = TRUE,
        by.x = colnames(curr)[[2]],
        by.y = colnames(curr)[[2]]
      )
    },
    merge_dfs,
    ds1,
    simplify = FALSE
  )

  list(ModifiedStudyData = data)
}

#
# test if first non-whitespace character in file is a '<'
#' Detect the structured-text format of files
#'
#' @param file_path Character vector of file paths.
#' @return A character vector containing `"json"` or `"xml"` per file.
#' @noRd
.util_json_or_xml <- function(file_path) {
  vapply(
    file_path,
    function(file_path) {
      con <- file(file_path, open = "rb")
      res <- readChar(con, nchars = 64)
      while (util_empty(res)) {
        res <- readChar(con, nchars = 1024)
      }
      close(con)
      res <- trimws(res)
      if (substr(res, 1, 1) != "<") {
        return("json")
      }
      "xml"
    },
    FUN.VALUE = character(1)
  )
}

#
# launch a regex on a file name to extract an ID
#
#' Extract a file identifier with a regular expression
#'
#' @param file_path A file path to match.
#' @param regex The regular expression with an optional capture group.
#' @return The first capture group or the complete match.
#' @noRd
.util_get_regex_file_id <- function(file_path, regex) {
  res <- util_capture_group_regex(file_path, regex, as_data_frame = TRUE)
  # Return the first capture group if there is one. Otherwise the whole match.
  if ("group1" %in% colnames(res)) {
    res[["group1"]]
  } else {
    res[["match"]]
  }
}

#
# get an ID from data contained in a file
# this returns a different function depending on the file type and ensures
# availability of the required tools
#
#' Construct an identifier reader for structured-text files
#'
#' @param file_type The structured-text format, JSON or XML.
#' @return A function that reads an entity identifier from a file.
#' @noRd
.util_make_entity_file_id_getter <- function(file_type) {
  switch(file_type,
    json = {
      if (util_empty(Sys.which("jq"))) {
        util_error(paste0(
          "jq not found. To process JSON files, dataquieR requires jq. ",
          "Installation instructions: https://jqlang.org"
        ))
      }

      function(file_path, entity_path) {
        #
        # Keep warnings: broken files may recover, but without an ID they
        # cannot enter the dataset or be checked by int_invalid_syntax().
        #
        # TODO: how to display in report?
        # TODO: COS: find indicator for broken files.
        # Data set format error (add to dq_obs)
        #
        tryCatch(
          error = function(cnd) {
            util_warning(
              paste0("Error running jq on file %s. Ignoring the file. ",
                "Message: %s"),
              file_path,
              conditionMessage(cnd)
            )
            "null"
          },
          warning = function(cnd) {
            util_warning(
              paste0("Warning from jq on file %s. Ignoring the file. ",
                "Message: %s"),
              file_path,
              conditionMessage(cnd)
            )
            "null"
          },
          system(
            paste0("jq -r '", entity_path, "' ", file_path),
            intern = TRUE,
            ignore.stderr = TRUE
          )
        )
      }
    },
    xml = {
      if (util_empty(Sys.which("xmllint"))) {
        util_error(paste0(
          "Xmllint not found. To process XML files, dataquieR ",
          "requires xmllint. It ships with libxml2 and is usually ",
          "preinstalled on Linux/macOS. On Windows, install Rtools ",
          "(https://cran.r-project.org/bin/windows/Rtools)"
        ))
      }
      # TODO: how to display in report?
      function(file_path, entity_path) {
        # string(string( <xpath> )) works fine, too
        tryCatch(
          error = function(cnd) {
            util_warning(
              paste0("Error running xmllint on file %s. ",
                "Ignoring the file. Message: %s"),
              file_path,
              conditionMessage(cnd)
            )
            "null"
          },
          warning = function(cnd) {
            util_warning(
              paste0("Warning from xmllint on file %s. ",
                "Ignoring the file. Message: %s"),
              file_path,
              conditionMessage(cnd)
            )
            "null"
          },
          system(
            paste0("xmllint --xpath 'string(", entity_path, ")' ", file_path),
            intern = TRUE,
            ignore.stderr = TRUE
          )
        )
      }
    }
  )
}

#' Internal helper: util add computed variables
#'
#' @noRd
.util_add_computed_variables <- function(
  ds1,
  meta_data,
  label_col,
  rules,
  use_value_labels,
  replace_missing_by = "NA",
  replace_limits = TRUE
) {
  if (missing(use_value_labels)) {
    use_value_labels <- VALUE_LABELS %in%
      colnames(meta_data) &&
      any(!util_empty(meta_data[[VALUE_LABELS]]))
  }

  util_expect_scalar(use_value_labels, check_type = is.logical)

  compiled_rules <- lapply(
    setNames(
      nm = rules[[VAR_NAMES]],
      rules[[COMPUTATION_RULE]]
    ),
    util_parse_redcap_rule
  )

  rule_res <- mapply(
    SIMPLIFY = FALSE,
    rule = compiled_rules,
    nm = names(compiled_rules),
    function(rule, nm) {
      # Use util_message() locally to trace compiled rule sources.
      util_message("%s", nm)
      r <- try(util_eval_rule(
        rule = rule,
        ds1 = ds1,
        meta_data = meta_data,
        use_value_labels = use_value_labels,
        replace_missing_by = replace_missing_by,
        replace_limits = replace_limits
      ))
      if (util_is_try_error(r)) {
        rl <- "?"
        try(rl <- util_attr(rule, "src", exact = TRUE), silent = TRUE)
        if (length(rl) != 1) {
          rl <- "?"
        }
        util_warning(
          "Could not evaluate rule %s: %s, results are all NA",
          sQuote(rl),
          dQuote(conditionMessage(util_attr(r, "condition", exact = TRUE))),
          applicability_problem = TRUE
        )
        NA
      } else {
        r
      }
    }
  )

  ModifiedStudyData <- ds1

  ModifiedStudyData[, names(rule_res)] <- rule_res

  return(list(ModifiedStudyData = ModifiedStudyData))
}
