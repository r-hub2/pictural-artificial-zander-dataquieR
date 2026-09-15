# nolint start: line_length_linter.
#' Render a table summarizing dataquieR results
#'
#' @param x a report summary (`summary(r)`)
#' @param grouped_by define the columns of the resulting matrix. It can be either
#'                  "call_names", one column per function, or "indicator_metric",
#'                  one column per indicator or both
#'                  `c("call_names", "indicator_metric")`.
#'                  The last combination is the default
#' @param folder_of_report a named vector with the location of variable and call_names
#' @param var_uniquenames a data frame with the original variable names and the
#'                        unique names in case of reports created with dq_report_by
#'                        containing the same variable in several reports
#'                        (e.g., creation of reports by sex)
#' @inheritParams util_filter_repsum
#' @return something, `htmltools` can render
#'
#' @noRd
# nolint end
util_render_table_dataquieR_summary <- function(x,
  grouped_by =
    c(
      "call_names",
      "indicator_metric"
    ),
  folder_of_report = NULL,
  var_uniquenames = NULL,
  vars_to_include = c("study")) {
  # make check happy:
  ordered_call_names <- NULL
  ordered_var_names <- NULL
  html <- NULL

  # order grouped_by if more than one is present
  if (length(grouped_by) > 1) {
    grouped_by <- sort(grouped_by)
  }

  # start
  util_stop_if_not(inherits(x, "dataquieR_summary"))
  # this represent the current object
  this <- util_attr(x, "this", exact = TRUE)
  if (identical(vars_to_include, "variable_group")) {
    this <- rlang::env_clone(this)
  }
  variable_group_call_names <- this$variable_group_call_names
  if (is.null(variable_group_call_names)) {
    variable_group_call_names <- character(0)
  }
  summary_meta_data <- this$summary_meta_data
  if (is.null(summary_meta_data)) summary_meta_data <- this$meta_data
  # define an environment to be attached
  withr::with_environment(this, {
    for (n in names(this)) {
      assign(n, get(n, envir = this), envir = environment())
    }
    meta_data <- summary_meta_data
    result <- util_filter_repsum(
      result, vars_to_include, summary_meta_data,
      rownames_of_report, label_col,
      variable_group_call_names,
      study_var_names = this$meta_data[[VAR_NAMES]]
    )
    if (identical(vars_to_include, "variable_group")) {
      result <- util_summary_most_specific_group_metrics(result)
    }
    rownames_of_report <- util_attr(result, "rownames_of_report", exact = TRUE)
    labels <- this$labels # obviously not attached by with_environment
    # extract the colors from the grading_format file
    gg_colors <- util_get_colors()
    # rename the categories to have "Ok" or "Critical" instead of cat1 or cat5
    names(gg_colors) <- util_get_labels_grading_class()[names(gg_colors)]

    # For robustness: in case none of the assessments work, create an empty
    # result
    # if result is empty or contains only errors
    if (!prod(dim(result)) || all(
      startsWith(as.character(result$indicator_metric), "CAT_") |
        startsWith(as.character(result$indicator_metric), "MSG_")
    )) {
      # create a paragraph stating that there is no result
      res <- (htmltools::browsable(htmltools::tagList(htmltools::p(
        "Empty summary"
      ))))
      # create an empty dataframe
      result2 <- data.frame()
      # use var_names as labels
      label_col <- VAR_NAMES
    } else { # in the case of actual metrics and not only error messages
      # ===== Table and html text preparation ===
      result_full <- result

      # create a named vector with names = call_names and values = function_name
      function_name_mapping <- setNames(result_full$function_name,
        nm = result_full$call_names
      )

      # create the function name that corresponds to the actual call_names,
      # e.g., with con_hard_limits instead of con_limit_deviations
      result_full$function_name2 <-
        apply(
          result_full[, "call_names",
            drop = FALSE
          ],
          1,
          FUN = function(x) {
            util_map_by_largest_prefix(x,
              haystack = util_all_ind_functions()
            )
          }
        )

      # add a column with suffixes
      result_full <-
        dplyr::mutate(result_full,
          suffixes = ifelse(
            startsWith(
              result_full[["call_names"]],
              result_full[["function_name2"]]
            ),
            substr(
              result_full[["call_names"]],
              nchar(result_full[["function_name2"]]) +
                1 + 1,
              nchar(result_full[["call_names"]])
            ),
            # name + "_" (first +1), start is the next character (second +1)
            result_full[["call_names"]]
          )
        )

      if (identical(vars_to_include, "variable_group") &&
          ".variable_group_result_label" %in% colnames(result_full)) {
        detail_labels <- as.character(
          result_full[[".variable_group_result_label"]]
        )
        group_labels <- as.character(result_full[[LABEL]])
        has_distinct_detail <- !util_empty(detail_labels) &
          detail_labels != group_labels
        escaped_details <- htmltools::htmlEscape(detail_labels)
        result_full$values_readable[has_distinct_detail] <- paste0(
          result_full$values_readable[has_distinct_detail],
          "<br>Comparison: ", escaped_details[has_distinct_detail]
        )
      }

      # create a new column with indicator_metric and suffixes
      # if present (excluding MSG and cAT indicators)
      result_full <-
        dplyr::mutate(result_full,
          indicator_metric_suff =
          ifelse(startsWith(
            as.character(result_full$indicator_metric),
            "MSG_"
          ) |
            startsWith(
              as.character(result_full$indicator_metric),
              "CAT_"
            ),
          result_full[["indicator_metric"]],
          ifelse(!util_empty(result_full[["suffixes"]]),
            paste0(
              result_full[["indicator_metric"]],
              "_",
              result_full[["suffixes"]]
            ),
            result_full[["indicator_metric"]]
          )
          )
        )

      # Create 3 different objects containing errors, category of errors, and
      # the actual results with the indicator metrics

      # Start selecting everything that starts with MSG (Error message)
      result_err_msg <-
        result_full[
          startsWith(
            as.character(result_full$indicator_metric),
            "MSG_"
          ),
          c(
            VAR_NAMES, "call_names",
            "indicator_metric_suff",
            "class"
          ),
          drop = FALSE
        ]
      # modify the class to be "err_msg"
      result_err_msg <- dplyr::rename(result_err_msg, err_msg = "class")

      # Select everything starting with CAT (Error category)
      result_err_cat <-
        result_full[
          startsWith(
            as.character(result_full$indicator_metric),
            "CAT_"
          ),
          c(
            VAR_NAMES, "call_names",
            "indicator_metric_suff",
            "class"
          ),
          drop = FALSE
        ]
      # Rename the class for CAT errors as "err_cat"
      result_err_cat <- dplyr::rename(result_err_cat, err_cat = "class")

      # Remove all error messages (CAT and MSG) and obtain an object with actual
      # results (metrics) only
      result_full <-
        result_full[!startsWith(
          as.character(result_full$indicator_metric_suff),
          "CAT_"
        ), , drop = FALSE]
      result_full <-
        result_full[!startsWith(
          as.character(result_full$indicator_metric_suff),
          "MSG_"
        ), , drop = FALSE]
      # =====  Add or modify columns in the 3 objects before merging them ======

      # ===== prepare the object with the results =======

      # Add a column with the numeric value of class e.g., 1 to 5
      result_full$class_num <- result_full$class # save it first as numeric

      # Convert column "class" to factor (e.g., Critical, Ok, NA)
      class_categ <- util_get_labels_grading_class()
      result_full$class <- factor(result_full$class,
        # take the levels of the grading and the labels
        # from the grading_ruleset
        levels = names(class_categ),
        labels = class_categ,
        ordered = TRUE
      )
      rm(class_categ)

      # ===== prepare the object with the error messages =======

      # Remove empty rows and leave only rows with actual error messages
      result_err_msg <- result_err_msg[!util_empty(result_err_msg$err_msg), ,
        drop = FALSE
      ]

      # remove duplicated rows in case present
      result_err_msg <- dplyr::distinct(result_err_msg)

      # in case there are duplicated rows due to the message
      # "No results computed"
      # separate duplicates in a group and then remove the
      # "No results computed" duplicates before to add them back
      values_with_dupl <- subset(
        result_err_msg,
        stats::ave(seq_len(nrow(result_err_msg)),
          interaction(result_err_msg[, c(
            "indicator_metric_suff",
            "VAR_NAMES", "call_names"
          ), drop = TRUE]),
          FUN = length
        ) > 1
      )

      result_err_msg_without_duplicate <-
        util_anti_join_common(
          result_err_msg,
          values_with_dupl[, c(
            "indicator_metric_suff",
            "VAR_NAMES",
            "call_names",
            "err_msg"
          ), drop = FALSE]
        )
      values_with_dupl <-
        values_with_dupl[order(
          values_with_dupl[, "indicator_metric_suff", drop = TRUE],
          values_with_dupl[, "VAR_NAMES", drop = TRUE],
          values_with_dupl[, "call_names", drop = TRUE],
          values_with_dupl[, "err_msg", drop = TRUE]
        ), ]
      values_with_dupl <- values_with_dupl[!grepl(
        "No results computed",
        values_with_dupl$err_msg
      ), , drop = FALSE]
      result_err_msg <- rbind(
        result_err_msg_without_duplicate,
        values_with_dupl
      )
      result_err_msg <- result_err_msg %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(c(
          VAR_NAMES,
          "call_names",
          "indicator_metric_suff"
        )))) %>%
        dplyr::summarise(
          err_msg = paste(unique(get("err_msg")), collapse = "\n"),
          .groups = "drop"
        ) %>%
        as.data.frame()

      # from long to wide format
      # History: this used tidyr::spread() before switching to stats::reshape().
      result_err_msg <- stats::reshape(result_err_msg,
        idvar = c("VAR_NAMES", "call_names"),
        timevar = "indicator_metric_suff",
        direction = "wide"
      )

      names(result_err_msg)[startsWith(names(result_err_msg), "err_msg.")] <-
        substr(
          names(result_err_msg)[startsWith(
            names(result_err_msg),
            "err_msg."
          )],
          nchar("err_msg.") + 1,
          nchar(names(result_err_msg)[startsWith(
            names(result_err_msg),
            "err_msg."
          )])
        )

      # Robustness: in case of empty cells replace them with NAs
      result_err_msg[util_empty(result_err_msg)] <- NA

      # add tag <li> to each error message (if the column exists in the report)
      if ("MSG_anamat" %in% colnames(result_err_msg)) {
        result_err_msg <- dplyr::mutate(result_err_msg,
          MSG_anamat =
            ifelse(!util_empty(get("MSG_anamat")),
              paste0(
                "<li>",
                get("MSG_anamat"),
                "</li>"
              ),
              NA
            )
        )
      }

      if ("MSG_applicability" %in% colnames(result_err_msg)) {
        result_err_msg <-
          dplyr::mutate(result_err_msg,
            MSG_applicability =
            ifelse(!util_empty(get("MSG_applicability")),
              paste0(
                "<li>",
                get("MSG_applicability"),
                "</li>"
              ), NA
            )
          )
      }

      if ("MSG_error" %in% colnames(result_err_msg)) {
        result_err_msg <- dplyr::mutate(result_err_msg,
          MSG_error =
            ifelse(!util_empty(get("MSG_error")),
              paste0(
                "<li>",
                get("MSG_error"),
                "</li>"
              ), NA
            )
        )
      }

      # create a column with all error messages (if there are error messages)
      # FIXED: Here, error messages are copied to all variables.
      # We need to group by VAR_NAMES and call_names
      ky_cols <- c("VAR_NAMES", "call_names")
      dt_cols <- setdiff(colnames(result_err_msg), ky_cols)
      result_err_msg <- tapply(
        simplify = FALSE,
        result_err_msg, as.list(result_err_msg[, ky_cols, drop = TRUE]),
        function(x) {
          res <- unique(x[, ky_cols, drop = FALSE])
          if (nrow(x) == 0) {
            return("")
          }
          paste(
            unique(
              na.omit(unlist(x[, dt_cols, drop = TRUE], recursive = TRUE))
            ),
            collapse = "\n"
          )
        }
      )
      result_err_msg <- array2DF(result_err_msg,
        responseName = "All_err_msg",
        simplify = FALSE,
        allowLong = TRUE
      )

      # ===== prepare the object with the categories of errors =======

      # Remove empty rows and leave only rows with actual error messages
      result_err_cat <- result_err_cat[!util_empty(result_err_cat$err_cat), ,
        drop = FALSE
      ]
      # remove duplicated rows in case present
      result_err_cat <- dplyr::distinct(result_err_cat)

      # in case there are duplicated rows separate duplicates in
      # a group and then order by decreasing values of err_cat
      # then keep only worst (higher values) of err_cat
      values_with_dupl <- subset(
        result_err_cat,
        stats::ave(seq_len(nrow(result_err_cat)),
          interaction(result_err_cat[, c(
            "indicator_metric_suff",
            "VAR_NAMES", "call_names"
          ), drop = TRUE]),
          FUN = length
        ) > 1
      )

      result_err_cat_without_duplicate <-
        util_anti_join_common(
          result_err_cat,
          values_with_dupl[, c(
            "indicator_metric_suff",
            "VAR_NAMES",
            "call_names",
            "err_cat"
          ), drop = FALSE]
        )
      values_with_dupl$err_cat <- as.numeric(values_with_dupl$err_cat)
      # order by decreasing values of err_cat
      values_with_dupl <-
        values_with_dupl[order(
          values_with_dupl[, "indicator_metric_suff", drop = TRUE],
          values_with_dupl[, "VAR_NAMES", drop = TRUE],
          values_with_dupl[, "call_names", drop = TRUE],
          -values_with_dupl[, "err_cat", drop = TRUE]
        ), ]
      values_with_dupl <- # keep first row of duplicated values
        values_with_dupl[!duplicated(values_with_dupl[
          ,
          c(
            "indicator_metric_suff",
            "VAR_NAMES",
            "call_names"
          )
          , drop = TRUE]), ]
      result_err_cat <- rbind(
        result_err_cat_without_duplicate,
        values_with_dupl
      )

      # from long to wide format
      # History: this used tidyr::spread() before switching to stats::reshape().
      result_err_cat <- stats::reshape(result_err_cat,
        idvar = c("VAR_NAMES", "call_names"),
        timevar = "indicator_metric_suff",
        direction = "wide"
      )

      names(result_err_cat)[startsWith(names(result_err_cat), "err_cat.")] <-
        substr(
          names(result_err_cat)[startsWith(
            names(result_err_cat),
            "err_cat."
          )],
          nchar("err_cat.") + 1,
          nchar(names(result_err_cat)[startsWith(
            names(result_err_cat),
            "err_cat."
          )])
        )

      # Add column other_error_stop5 - logic T/F; if T, an unexpected error
      # has occurred.
      result_err_cat$other_error_stop5 <- result_err_cat$CAT_error == 5

      # Add column is_indicator - logic T/F; if F is not an indicator
      result_err_cat$is_indicator <-
        result_err_cat$CAT_indicator_or_descriptor == 1

      # ===== Merge the 3 object in one data frame =======

      # Merge results and errors in one object (errors become 4 extra columns)
      if (!VAR_NAMES %in% colnames(result_err_msg)) {
        result_err_msg[[VAR_NAMES]] <- rep(NA_character_, nrow(result_err_msg))
      }
      result_full <- merge(
        result_full,
        result_err_msg,
        by = c(VAR_NAMES, "call_names"), all = TRUE
      )

      if (!VAR_NAMES %in% colnames(result_err_cat)) {
        result_err_cat[[VAR_NAMES]] <- rep(NA_character_, nrow(result_err_cat))
      }
      result_full <- merge(
        result_full,
        result_err_cat,
        by = c(VAR_NAMES, "call_names"), all = TRUE
      )


      # ===== Modify the obtained dataframe =======

      # Sort result_full by indicator_metrics
      result_full <- result_full[order(result_full[["indicator_metric"]]), ,
        drop = FALSE
      ]
      result_full$other_error_stop5[
        is.na(result_full$other_error_stop5)
      ] <- FALSE
      missing_is_indicator <- is.na(result_full$is_indicator)
      result_full$is_indicator[missing_is_indicator] <-
        !startsWith(
          result_full$indicator_metric[missing_is_indicator], "CAT_"
        ) &
        !startsWith(
          result_full$indicator_metric[missing_is_indicator], "MSG_"
        )

      # For robustness: names in labels and colors should match, otherwise
      # there is inconsistency in the grading_ruleset.
      # Check if the names match
      if (!identical(names(labels), names(colors)) ||
          !all(paste0("cat", seq_len(5)) %in% names(colors))) {
        util_warning(
          c(
            "Could not find enough categories in custom format file,",
            "falling back to default"
          ),
          applicability_problem = TRUE,
          intrinsic_applicability_problem = FALSE
        )
        withr::local_options(list(
          dataquieR.grading_formats =
            system.file("grading_formats.xlsx",
              package = "dataquieR"
            ),
          dataquieR.grading_rulesets =
            system.file("grading_rulesets.xlsx",
              package = "dataquieR"
            )
        ))
        labels <- util_get_labels_grading_class()
        colors <- util_get_colors()
        names(labels) <- paste0("cat", names(labels))
        names(colors) <- paste0("cat", names(colors))
      }

      # `this$rowmaxes` describes the unfiltered combined summary. Recompute it
      # after selecting the requested entity so item and variable-group totals
      # cannot leak into each other's tables or links.
      this$rowmaxes <- util_compute_rowmaxes(
        result = result,
        labels = labels,
        colors = colors,
        order_of = this$order_of,
        filter_of = this$filter_of,
        labels_of_var_names_in_report =
          this$labels_of_var_names_in_report
      )

      if ("indicator_metric" %in% grouped_by) {
        result_full$indicator_metric[
          util_empty(result_full$indicator_metric)
        ] <-
          "EMPTY_OUTPUT"
        if ("indicator_metric_suff" %in% colnames(result_full)) {
          result_full$indicator_metric_suff[
            util_empty(result_full$indicator_metric_suff)
          ] <- "EMPTY_OUTPUT"
        }
      }

      # Add a column T/F to check if the indicator metric has a value
      result_full$have_value <- result_full$indicator_metric != "EMPTY_OUTPUT"

      # Add a column label
      result_full$LABEL <-
        this$labels_of_var_names_in_report[result_full$VAR_NAMES]

      # Add a column islab - with the category (e.g., "Ok") or "Not classified"
      result_full <-
        result_full %>% dplyr::mutate(islab = ifelse(!util_empty(class),
            as.character(class),
            "Not classified"
          ))

      # Add a logic vector checking if the result is reasonably possible
      if ("CAT_anamat" %in% colnames(result_full)) {
        result_full$not_applicable <- result_full[["CAT_anamat"]] != 1
        result_full$not_applicable[is.na(result_full$not_applicable)] <- FALSE
      }

      # Add a logic vector checking if there are all metadata to compute the
      # indicator, if the value is not 1 something is missing
      if ("CAT_applicability" %in% colnames(result_full)) {
        result_full$meta_data_sufficient <-
          result_full[["CAT_applicability"]] == 1
        result_full$meta_data_sufficient[
          is.na(result_full$meta_data_sufficient)
        ] <- TRUE
      }
      # Add a column with the categories as. cat1, cat2 and so on
      result_full$class_cat <- util_as_cat(result_full$class_num)

      # Add "class_color" with the corresponding color of the categories and
      # NAs.
      result_full$class_color <- NA
      result_full$class_color[is.na(result_full$class_color) &
          !is.na(result_full$class_cat) &
          result_full$is_indicator] <-
        colors[paste(result_full$class_cat[is.na(result_full$class_color) &
                !is.na(result_full$class_cat) &
                result_full$is_indicator])]

      if ("not_applicable" %in% colnames(result_full)) {
        result_full$class_color[is.na(result_full$class_color) &
            result_full$not_applicable &
            result_full$is_indicator] <- "#ffffff"
      }


      if ("meta_data_sufficient" %in% colnames(result_full)) {
        result_full$class_color[is.na(result_full$class_color) &
            !result_full$meta_data_sufficient &
            result_full$is_indicator] <- "#444444"
      }

      result_full$class_color[is.na(result_full$class_color) &
          (result_full$other_error_stop5 |
              result_full$is_indicator)] <- "#aaaaaa"

      result_full$class_color[is.na(result_full$class_color)] <- "#ffffff"

      # Add a column to determine the brightness of the text
      # (black/white depending on the background)
      result_full$fg_color <- vapply(result_full$class_color,
        FUN = util_get_fg_color,
        FUN.VALUE = character(1)
      )

      # create a column title_text - containing the title for the hover text
      result_full$title <-
        vapply(unique(result_full$call_names),
          util_alias2caption,
          long = FALSE,
          FUN.VALUE = character(1)
        )[result_full$call_names]

      result_full <- result_full %>%
        dplyr::mutate(title_text = paste0(get("title"), " of ", get("LABEL")))

      # create the text of the hover box: a level-4 header with nice caption
      # and a tag for classification (e.g. Critical)
      result_full$hover_text <- paste0(
        "<h4>", result_full[["title_text"]],
        "</h4> <h5>", result_full[["islab"]],
        "</h5>"
      )
      result_full <- result_full[
        , -which(names(result_full) == c("title_text")),
        drop = FALSE
      ]


      # create alternative title_text and hover_text if only indicator metric
      # is selected
      result_full$title_ind <- vapply(unique(result_full$indicator_metric),
        util_translate_indicator_metrics,
        FUN.VALUE =
          character(1)
      )[result_full$indicator_metric]
      needs_function_title <-
        util_empty(result_full[["title_ind"]]) |
        result_full[["indicator_metric"]] == "EMPTY_OUTPUT"
      result_full$title_ind[needs_function_title] <-
        result_full[["title"]][needs_function_title]

      result_full <- result_full %>% dplyr::mutate(
        title_text_ind =
          paste0(
            get("title_ind"),
            " of ",
            get("LABEL")
          )
      )

      # create the text of the hover box: a level-4 header with nice caption
      # and a tag for classification (e.g. Critical)
      result_full$hover_text_ind <-
        paste0(
          "<h4>", result_full[["title_text_ind"]],
          "</h4> <h5>", result_full[["islab"]],
          "</h5>"
        )

      result_full <- result_full[, -which(names(result_full) ==
            c("title_text_ind")),
        drop = FALSE
      ]

      # create a readable text for the indicator metric result
      # example: Unexpected location (Percentage (0 to 100)) = 0.00%: Ok
      result_full <-
        dplyr::mutate(result_full,
          speaking_name_indic_metric = ifelse(
            !util_empty(get("indicator_metric")),
            paste0(util_translate_indicator_metrics(
              result_full[["indicator_metric"]]
            )),
            NA
          )
        )

      if (identical(vars_to_include, "variable_group")) {
        result_full$indicator_metric_suff <- result_full$indicator_metric
      }

      result_full <-
        dplyr::mutate(result_full,
          speaking_name_indic_metric = ifelse(
            !util_empty(result_full[["suffixes"]]),
            paste0(
              result_full[["speaking_name_indic_metric"]],
              " ", result_full[["suffixes"]]
            ),
            result_full[["speaking_name_indic_metric"]]
          )
        )

      missing_display_value <- util_summary_display_value_missing(
        result_full[["value"]]
      )
      result_full[["value"]] <- as.character(result_full[["value"]])
      result_full[["value"]][missing_display_value] <- ""

      result_full <-
        dplyr::mutate(result_full,
          values_readable = ifelse(
            !util_empty(result_full[["value"]]),
            paste0(
              result_full[["speaking_name_indic_metric"]],
              " = ", result_full[["value"]],
              ifelse(
                !is.na(result_full[["class"]]),
                paste0(
                  ": ",
                  result_full[["class"]]
                ),
                ""
              ),
              " - ", result_full[["title"]]
            ),
            "No results available"
          )
        )


      result_full <- result_full[, -which(names(result_full) ==
            c("speaking_name_indic_metric")),
        drop = FALSE
      ]

      # define which should be in bold (there is a metric and it has a class)
      result_full <- dplyr::mutate(result_full,
        values_readable =
          ifelse(get("have_value") &
              !util_empty(get("class")),
            paste(
              "<strong>",
              get("values_readable"),
              "</strong>"
            ),
            get("values_readable")
          )
      )

      # create a link

      result_full <-
        util_add_links_to_summary_table(result_full,
          this,
          folder_of_report = folder_of_report,
          vars_to_include = vars_to_include
        )


      rf <- result_full[, c(
        LABEL, "call_names",
        "value", "fg_color", "href", "popup_href", "title", "title_ind"
      ),
      drop = FALSE
      ]
      rf$orig_label_col <- util_attr(
        result_full, "orig_label_col", exact = TRUE
      )

      result_full$ln <-
        apply(rf, 1,
          FUN = function(x) {
            # in case it is not a summary of a single report
            if (!is.null(folder_of_report)) {
              search_name <- gsub(
                "\\s+",
                "",
                paste0(x[["orig_label_col"]], ".", x[["call_names"]])
              )
              folder_report_keys <- gsub("\\s+", "", names(folder_of_report))
              name_of_r <- unique(unname(folder_of_report[
                folder_report_keys %in% search_name
              ]))
              if (!length(name_of_r)) {
                name_of_r <- ""
                prefix <- ""
              } else {
                prefix <-
                  paste0(name_of_r, "/.report/")
                name_of_r <- paste0(name_of_r, " -- ")
              }
            } else { # in case it is a summary of a single report
              name_of_r <- ""
              prefix <- ""
            }
            if (grepl("/\\.report/", x[["href"]], fixed = FALSE)) {
              name_of_r <- ""
              prefix <- ""
            }
            dlg_cap <- paste0(name_of_r, paste0(x[["title"]], x[["title_ind"]]))
            link <- htmltools::a(
              href = paste0(prefix, x[["href"]]),
              title = htmltools::HTML(as.character(
                x[["value"]]
              )),
              onclick = "event.preventDefault();",
              htmltools::HTML(as.character(x[["value"]]))
            )

            link$attribs$style <- c(
              link$attribs$style,
              "text-decoration:none;display:block;"
            )
            link$attribs$style <- paste0(
              link$attribs$style,
              sprintf(
                "color:%s;",
                x[["fg_color"]]
              )
            )
            as.character(link)
          }
        )

      result_full <- dplyr::mutate(result_full,
        ln =
          ifelse(util_empty(get("value")),
            gsub("(<a.*?[^/>]*>)[^/<]*(.*)",
              "\\1&nbsp;\\2",
              x = get("ln"),
              perl = TRUE
            ),
            get("ln")
          )
      )


      # extract href to a columns and remove href as an attribute in the link
      result_full$href <- gsub('<a.*? *href *= *"([^"]*)".*?>.*',
        "\\1",
        x = result_full$ln, perl = TRUE
      )
      result_full$ln <- gsub('(<a.*?) *href *= *"[^"]*"(.*?>)',
        "\\1\\2",
        x = result_full$ln, perl = TRUE
      )
      result_full$ln <- gsub('(<a.*?) *onclick *= *"[^"]*"(.*?>)',
        "\\1\\2",
        x = result_full$ln, perl = TRUE
      )

      # for all rows is_indicator == FALSE append something to All_err_msg
      result_full$descriptor_name <- result_full[["function_name"]]
      result_full$descriptor_name[util_empty(result_full$descriptor_name)] <-
        result_full[["title"]][util_empty(result_full$descriptor_name)]
      result_full$descriptor_name[util_empty(result_full$descriptor_name)] <-
        result_full[["call_names"]][util_empty(result_full$descriptor_name)]
      result_full <-
        dplyr::mutate(result_full,
          All_err_msg = ifelse(!get("is_indicator"),
            paste0(
              get("All_err_msg"),
              " ",
              '<span class="dataquieR-error-message"> ',
              get("descriptor_name"),
              " is a descriptor, only.",
              "</span>"
            ),
            get("All_err_msg")
          )
        )
      result_full[["descriptor_name"]] <- NULL

      result_full$All_err_msg <- gsub("(?m)^\\s*NA\\s*$\\n?", "",
        result_full$All_err_msg,
        perl = TRUE
      )
      result_full$All_err_msg[is.na(result_full$All_err_msg)] <- ""

      if (identical(vars_to_include, "variable_group")) {
        available_group_row <- util_summary_display_row_available(
          value = result_full[["value"]],
          indicator_metric = result_full[["indicator_metric"]],
          messages = result_full[["All_err_msg"]],
          classification = result_full[["class"]]
        )
        original_labels <- util_attr(
          result_full, "orig_label_col", exact = TRUE
        )
        if (length(original_labels) == nrow(result_full)) {
          original_labels <- original_labels[available_group_row]
        }
        result_full <- result_full[available_group_row, , drop = FALSE]
        attr(result_full, "orig_label_col") <- original_labels
      }

      # create hover addition containing error messages
      result_full <- dplyr::mutate(result_full,
        hover = ifelse(!util_empty(get("All_err_msg")),
          paste0(
            "<br>", "Messages: ",
            "<br>",
            "\n<ul>\n",
            get("All_err_msg"),
            "\n</ul>\n"
          ),
          paste0("")
        )
      )


      # initialize the cell text; it should never be observed
      result_full$celltext <- paste(result_full$VAR_NAMES,
        result_full$call_names,
        collapse = "/"
      )
      result_full$o <- NA # Initialize the order
      result_full$f <- htmltools::HTML("&nbsp;") # initialize the filters


      # define order and filter
      result_full$o <- this$order_of[paste(result_full$class_cat)]
      result_full$f <- this$filter_of[paste(result_full$class_cat)]

      # Complete text for the hover box
      result_full$text_complete_hover <- paste0(
        result_full$hover_text, "<br/>",
        result_full$values_readable,
        "<br/>",
        result_full$hover
      )
      result_full$text_complete_hover <-
        htmltools::htmlEscape(result_full$text_complete_hover,
          attribute = TRUE
        )
      # Open both single-report and overview results in the same result-only
      # popup. The target page remains available from the popup's report link.
      can_open_result <-
        !util_empty(result_full$popup_href) &
        !util_empty(result_full$href)
      result_full$href_sentence <- rep("", nrow(result_full))
      result_full$href_sentence[can_open_result] <- paste0(
        '"', mapply(
          util_summary_popup_handler,
          url = result_full$popup_href[can_open_result],
          link_url = result_full$href[can_open_result],
          title = paste0(
            result_full$title[can_open_result],
            result_full$title_ind[can_open_result]
          ),
          USE.NAMES = FALSE
        ), '"'
      )
      rf <- result_full[, c(
        LABEL, "href_sentence", "class_color",
        "o", "f",
        "text_complete_hover", "ln",
        "call_names",
        "value", "fg_color", "href", "popup_href", "title", "title_ind"
      ),
      drop = FALSE
      ]
      rf$orig_label_col <- util_attr(
        result_full, "orig_label_col", exact = TRUE
      )
      # create a column with the final html text
      try(result_full$html <-
        apply(rf, 1,
          FUN = function(x) {
            ln <- x[["ln"]]
            filter <- x[["f"]]
            sort_value <- x[["o"]]
            # in case it is not a summary of a single report
            if (!is.null(folder_of_report)) {
              search_name <- gsub(
                "\\s+",
                "",
                paste0(x[["orig_label_col"]], ".", x[["call_names"]])
              )
              folder_report_keys <- gsub("\\s+", "", names(folder_of_report))
              name_of_r <- unique(unname(folder_of_report[
                folder_report_keys %in% search_name
              ]))
              if (!length(name_of_r)) {
                name_of_r <- ""
                prefix <- ""
              } else {
                prefix <-
                  paste0(name_of_r, "/.report/")
                name_of_r <- paste0(name_of_r, " -- ")
              }
            } else { # in case it is a summary of a single report
              name_of_r <- ""
              prefix <- ""
            }
            if (grepl("/\\.report/", x[["href"]], fixed = FALSE)) {
              name_of_r <- ""
              prefix <- ""
            }
            dlg_cap <- paste0(name_of_r, paste0(x[["title"]], x[["title_ind"]]))

            if (!util_empty(x[["value"]]) &&
              util_summary_link_available(
                x[["href"]],
                x[["popup_href"]]
              )) { # NOT no results available and has a target
              onclick <- paste0(
                "onclick=\"",
                util_summary_popup_handler(
                  paste0(prefix, x[["popup_href"]]),
                  x[["href"]], # this has a prefix here, already.
                  dlg_cap
                ),
                "\""
              )
              cursor_style <- "cursor: pointer;"
            } else {
              onclick <- ""
              cursor_style <- ""
            }
            # Cell content is already escaped before this HTML assembly.
            text_for_html <- paste0(
              "<pre ",
              onclick,
              " style=",
              sprintf(
                '"height: 100%%; min-height: 2em; margin: 0em; padding: 0em; background: %s; %s text-align: center;"', # nolint: line_length_linter.
                x[["class_color"]], cursor_style
              ),
              " sort=\"",
              sort_value, "\"",
              " filter=\"",
              filter, "\"",
              " title=\"",
              x[["text_complete_hover"]], "\"> ",
              ln, "\n</pre>"
            )
            as.character(text_for_html)
          }
        ))

      ##### ==== In case the argument grouped_by is selected ========
      # if not selected, the default is to have both grouped_by values
      if (identical(sort(grouped_by), c("call_names", "indicator_metric"))) {
        result <- result_full[, -which(names(result_full) %in%
          c(
            "value_readable",
            "hover_text",
            "ln", "href",
            "hover",
            "text_complete_hover"
          )),
        drop = FALSE
        ]
        rm(result_full)
      } else if (grouped_by == "call_names" ||
          grouped_by == "indicator_metric") {
        result_full <- result_full[, c(
          "VAR_NAMES", "call_names",
          "class", "indicator_metric",
          "indicator_metric_suff", "suffixes",
          "value", "values_raw",
          "n_classes", "STUDY_SEGMENT",
          "function_name", "class_num",
          "All_err_msg", "LABEL",
          "class_cat", "class_color",
          "values_readable",
          "hover_text", "ln", "hover_text_ind",
          "href_sentence", "celltext", "o", "f"
        ),
        drop = FALSE
        ]

        # ==== Separate the case of call_names and indicator_metric ===

        if (grouped_by == "call_names") {
          ##### ==== grouped_by = "call_names" is selected ====

          # decreasing order of VAR_NAMES, call_names and class_num
          result_full <- result_full[order(result_full$VAR_NAMES,
              result_full$call_names,
              result_full$class_num,
              decreasing = TRUE
            ), , drop = FALSE]

          # create a code with VAR_NAMES and call_names
          result_full$code <- paste(
            result_full$VAR_NAMES,
            result_full$call_names
          )
        } else if (grouped_by == "indicator_metric") {
          ##### ==== grouped_by = "indicator_metric" is selected ====

          # decreasing order of VAR_NAMES, call_names and class_num
          result_full <- result_full[order(result_full$VAR_NAMES,
              result_full$indicator_metric_suff,
              result_full$class_num,
              decreasing = TRUE
            ), , drop = FALSE]

          # create a code with VAR_NAMES and indicator_metric
          result_full$code <- paste(
            result_full$VAR_NAMES,
            result_full$indicator_metric_suff
          )
        }

        has_result <- !util_empty(result_full$values_raw) |
          !util_empty(result_full$class_num)
        grouped_result_state <- util_summary_group_result_state(
          result_full$code,
          result_full$class_num,
          has_result
        )
        result_full$grouped_result_count <-
          grouped_result_state[, "result_count", drop = TRUE]
        result_full$grouped_worst_count <-
          grouped_result_state[, "worst_count", drop = TRUE]
        result_full$grouped_has_classification <-
          grouped_result_state[, "has_classification", drop = TRUE] > 0L

        ##### ==== The following part is the same for the 2 cases
        # grouped_by = "indicator_metric" or
        # grouped_by = "call_names"  =========

        # return only first occurrence in the compared vectors, to get only the
        # first row with the worst value
        primary_result_rows <- util_summary_primary_result_rows(
          result_full$code,
          has_result
        )
        result_call_name <- result_full[primary_result_rows, , drop = FALSE]

        if (identical(vars_to_include, "variable_group")) {
          multiple_results <- result_call_name$grouped_result_count > 1L
          tied_worst_results <- multiple_results &
            result_call_name$grouped_worst_count > 1L
          result_call_name$ln[tied_worst_results] <-
            util_summary_replace_link_text(
              result_call_name$ln[tied_worst_results],
              ifelse(
                result_call_name$grouped_has_classification[
                  tied_worst_results
                ],
                sprintf(
                  "%d tied results",
                  result_call_name$grouped_worst_count[tied_worst_results]
                ),
                sprintf(
                  "%d results",
                  result_call_name$grouped_result_count[tied_worst_results]
                )
              )
            )
          unique_worst_result <- multiple_results & !tied_worst_results
          result_call_name$hover_text_ind[unique_worst_result] <- paste0(
            result_call_name$hover_text_ind[unique_worst_result],
            "<p>",
            "The worst of ",
            result_call_name$grouped_result_count[unique_worst_result],
            " results is shown. The remaining results are listed below.",
            "</p>"
          )
          classified_tie <- tied_worst_results &
            result_call_name$grouped_has_classification
          result_call_name$hover_text_ind[classified_tie] <- paste0(
            result_call_name$hover_text_ind[classified_tie],
            "<p>",
            result_call_name$grouped_worst_count[classified_tie],
            " results share the worst available classification; all tied ",
            "results are listed below.</p>"
          )
          unclassified_tie <- tied_worst_results &
            !result_call_name$grouped_has_classification
          result_call_name$hover_text_ind[unclassified_tie] <- paste0(
            result_call_name$hover_text_ind[unclassified_tie],
            "<p>No grading rule classified these ",
            result_call_name$grouped_result_count[unclassified_tie],
            " results. Add grading rules for this indicator metric to show ",
            "an identifiable worst result; equal worst results remain listed ",
            "together.</p>"
          )
        }

        if (identical(vars_to_include, "variable_group")) {
          # Keep every non-primary group result exactly once. An anti-join is
          # not safe here because the selected row is modified before this
          # split.
          result_full <- result_full[-primary_result_rows, , drop = FALSE]
        } else {
          # Preserve the established item-level behavior.
          result_full <- util_anti_join_common(result_full, result_call_name)
        }

        # create a temporary object tO remove strong tag from values_readable
        list_other_indicators <- result_full[, c(
          "VAR_NAMES",
          "call_names",
          "code",
          "indicator_metric",
          "indicator_metric_suff",
          "suffixes",
          "class",
          "values_readable"
        ),
        drop = FALSE
        ]
        # TO remove strong tag : remove all rows without a result (class = NA)
        list_other_indicators <- dplyr::filter(
          list_other_indicators,
          get("values_readable") !=
            "No results available"
        )

        # TO remove strong tag: keep only a part of the string
        # First create a temporary data frame with the code and the worst class
        class_worst_result <- result_call_name[, c("code", "class"),
          drop = FALSE
        ]
        class_worst_result <-
          class_worst_result[!util_empty(class_worst_result$class), ,
            drop = FALSE
          ]
        colnames(class_worst_result)[colnames(class_worst_result) == "class"] <-
          "class_worst_result"
        # Then merge data frames to add the worst class in the list of other
        # indicators.
        list_other_indicators <- merge(list_other_indicators,
          class_worst_result,
          by = "code",
          all.x = TRUE
        )
        rm(class_worst_result)

        # Finally remove the strong tag only if the class is different from
        # the worst class
        list_other_indicators <-
          dplyr::mutate(list_other_indicators,
            values_readable =
            ifelse(class ==
                get("class_worst_result") |
                is.na(get("class_worst_result")),
              get("values_readable"),
              gsub("<strong> *(.*)<.*",
                "\\1",
                x = list_other_indicators$values_readable,
                perl = TRUE
              )
            )
          )

        # create a row with the content of multiple rows creating a bullet list
        # if there are other rows
        # first add the tag li to each row in values_readable
        if (nrow(list_other_indicators) != 0) {
          list_other_indicators$values_readable <-
            paste0(
              "<li>",
              list_other_indicators$values_readable,
              "</li>"
            )
          # Then i merged the rows by code (VAR_NAMES call_names)
          list_other_indicators <- list_other_indicators %>%
            dplyr::group_by(get("code")) %>%
            dplyr::summarise(values_readable = paste(get("values_readable"),
                collapse = " "
              ))
          # renamed to have the first column "code" and not get("code")
          colnames(list_other_indicators) <- c("code", "values_readable")

          # History: an earlier version wrapped these values in <ul>.
          # Keep the merged values below the bold indicator and color the cell.
          result_call_name <- merge(result_call_name, list_other_indicators,
            by = "code", all.x = TRUE
          )


          # create a column with all value_readable
          result_call_name$values_readable_complete <-
            apply(
              result_call_name[, names(result_call_name) %in%
                c(
                  "values_readable.x",
                  "values_readable.y"
                ), drop = TRUE],
              1, function(x) paste(na.omit(x), collapse = "\n")
            )
          # remove non useful columns
          result_call_name <-
            result_call_name[, -which(names(result_call_name) %in%
              c(
                "values_readable.x",
                "values_readable.y"
              )),
            drop = FALSE
            ]
          rm(list_other_indicators)
        }

        # Now create the new complete list of error messages
        list_other_errors <- result_full[,
          c(
            "VAR_NAMES",
            "call_names",
            "code",
            "indicator_metric",
            "indicator_metric_suff",
            "suffixes",
            "class",
            "All_err_msg"
          ),
          drop = FALSE
        ]
        # remove rows without an error message
        list_other_errors <-
          list_other_errors[!is.na(list_other_errors$All_err_msg), ,
            drop = FALSE
          ]
        # keep only non-repeated error messages for VAR_NAMES and call_names

        list_other_errors <- list_other_errors[, c("code", "All_err_msg"),
          drop = FALSE
        ]
        list_other_errors <- unique(list_other_errors[, c(
          "code",
          "All_err_msg"
        ),
        drop = FALSE
        ])

        # remove error messages matching the error message of the indicator
        # with the worst result
        # create a temporary dataframe
        errors_in_result_call_name <-
          result_call_name[, c("code", "All_err_msg"), drop = FALSE]

        # return rows of list_other_errors that do not have a match in
        # errors_in_result_call_name
        list_other_errors <-
          util_anti_join_common(list_other_errors, errors_in_result_call_name)
        rm(errors_in_result_call_name, result_full)

        # Then merge the rows by code (VAR_NAMES call_names)
        list_other_errors <- list_other_errors %>%
          dplyr::group_by(get("code")) %>%
          dplyr::summarise(All_err_msg = paste(get("All_err_msg"),
              collapse = " "
            ))
        # renamed to have the first column "code" and not get("code")
        colnames(list_other_errors) <- c("code", "All_err_msg")

        # Then add the error messages to the dataframe
        result_call_name <- merge(result_call_name, list_other_errors,
          by = "code", all.x = TRUE
        )

        # create a column with all error messages
        result_call_name$All_err_msg_complete <-
          apply(
            result_call_name[, names(result_call_name) %in%
                c("All_err_msg.x", "All_err_msg.y"), drop = TRUE],
            1, function(x) {
              paste(na.omit(x),
                collapse = "\n"
              )
            }
          )
        result_call_name$All_err_msg_complete <-
          gsub("(?m)^\\s*NA\\s*$\\n?", "",
            result_call_name$All_err_msg_complete,
            perl = TRUE
          )
        result_call_name$All_err_msg_complete[
          is.na(result_call_name$All_err_msg_complete)
        ] <- ""

        values_for_empty_output_reason <- result_call_name[["values_readable"]]
        if (is.null(values_for_empty_output_reason)) {
          values_for_empty_output_reason <-
            result_call_name[["values_readable_complete"]]
        }

        missing_empty_output_reason <-
          result_call_name[["indicator_metric"]] == "EMPTY_OUTPUT" &
          util_empty(result_call_name[["All_err_msg_complete"]]) &
          values_for_empty_output_reason == "No results available"
        result_call_name$All_err_msg_complete[missing_empty_output_reason] <-
          paste0(
            '<li><span class="dataquieR-message-message">',
            "Assessment not reasonable / not requested: no result was ",
            "produced for this variable/function combination. This can happen ",
            "when the metadata, for example the variable role, do not request ",
            "or permit this assessment.",
            "</span></li>"
          )

        # create hover containing the bullet point list of error messages
        result_call_name <-
          dplyr::mutate(result_call_name,
            hover =
            ifelse(!util_empty(get("All_err_msg_complete")),
              paste0(
                "<br>", "Messages: ", "<br>",
                "\n<ul>\n",
                get("All_err_msg_complete"),
                "\n</ul>\n"
              ),
              paste0("")
            )
          )
        # remove non useful columns
        result_call_name <-
          result_call_name[, -which(names(result_call_name) %in%
            c(
              "All_err_msg.x",
              "All_err_msg.y"
            )),
          drop = FALSE
          ]
        rm(list_other_errors)


        # == modify hover title if only indicator metric is selected

        if ("call_names" %in% grouped_by) {
          # complete text for the hover box
          result_call_name$text_complete_hover <-
            paste0(
              result_call_name$hover_text, "<br/>",
              result_call_name$values_readable, "<br/>",
              result_call_name$hover
            )
          result_call_name$text_complete_hover <-
            htmltools::htmlEscape(result_call_name$text_complete_hover,
              attribute = TRUE
            )
        } else if (grouped_by == "indicator_metric") {
          # complete text for the hover box
          result_call_name$text_complete_hover <-
            paste0(
              result_call_name$hover_text_ind, "<br/>",
              result_call_name$values_readable, "<br/>",
              result_call_name$hover
            )
          result_call_name$text_complete_hover <-
            htmltools::htmlEscape(result_call_name$text_complete_hover,
              attribute = TRUE
            )
        }

        # create the new html column
        try(result_call_name$html <-
          apply(
            result_call_name[, c(
              "href_sentence",
              "class_color",
              "o",
              "f",
              "text_complete_hover",
              "ln"
            ),
            drop =
              FALSE
            ],
            1,
            FUN = function(x) {
              ln <- x[["ln"]]
              filter <- x[["f"]]
              sort_value <- x[["o"]]
              clickable <- !util_empty(x[["href_sentence"]])
              onclick <- if (clickable) {
                paste0("onclick=", x[["href_sentence"]])
              } else {
                ""
              }
              cursor_style <- if (clickable) "cursor: pointer;" else ""
              # Cell content is already escaped before this HTML assembly.
              text_for_html <-
                paste0(
                  "<pre ",
                  onclick,
                  " style=",
                  sprintf(
                    '"height: 100%%; min-height: 2em; margin: 0em; padding: 0em; background: %s; %s text-align: center;"', # nolint: line_length_linter.
                    x[["class_color"]], cursor_style
                  ),
                  " sort=\"",
                  sort_value,
                  "\"",
                  " filter=\"",
                  filter,
                  "\"",
                  " title=\"",
                  x[["text_complete_hover"]],
                  "\"> ",
                  ln,
                  "\n</pre>"
                )
              as.character(text_for_html)
            }
          ))
        result <- result_call_name
      }

      # rename value as category and html as value
      result1 <- dplyr::rename(result, category = value, value = html)

      # ==== create final table column if call_names is in the argument
      empty_output_columns_tech <- character(0)
      if ("call_names" %in% grouped_by) {
        if (length(grouped_by) > 1) {
          result2 <- result1[, c(
            "VAR_NAMES",
            "call_names",
            "indicator_metric",
            "category",
            "values_raw",
            "class_num",
            "value"
          ),
          drop = FALSE
          ]
          result2$cl <- paste0(
            result2$call_names, "_",
            result2$indicator_metric
          )
          has_value_by_column <- tapply(
            !util_empty(result2$values_raw) |
              !util_empty(result2$class_num),
            result2$cl, any
          )
          empty_output_columns_tech <-
            names(has_value_by_column)[!has_value_by_column]

          result2 <- result2[, c("VAR_NAMES", "cl", "value"), drop = FALSE]

          # remove duplicated rows in case present
          result2 <- dplyr::distinct(result2)

          result2 <- stats::reshape(result2,
            idvar = c("VAR_NAMES"),
            timevar = "cl",
            direction = "wide"
          )

          names(result2)[startsWith(names(result2), "value.")] <-
            substr(
              names(result2)[startsWith(
                names(result2),
                "value."
              )],
              nchar("value.") + 1,
              nchar(names(result2)[startsWith(
                names(result2),
                "value."
              )])
            )
        } else {
          result2 <- result1[, c(
            "VAR_NAMES",
            "call_names",
            "indicator_metric",
            "category",
            "grouped_result_count",
            "value"
          ),
          drop = FALSE
          ]
          result2 <- dplyr::select(
            result2, "VAR_NAMES",
            paste0(grouped_by),
            "category",
            "grouped_result_count",
            "value"
          )
          names(result2) <- c(
            "VAR_NAMES", "cl", "category", "grouped_result_count", "value"
          )
          has_value_by_column <- tapply(
            result2$grouped_result_count > 0L,
            result2$cl, any
          )
          empty_output_columns_tech <-
            names(has_value_by_column)[!has_value_by_column]
          result2 <- result2[, c("VAR_NAMES", "cl", "value"), drop = FALSE]

          # remove dupliates in case present
          result2 <- dplyr::distinct(result2)

          result2 <- stats::reshape(result2,
            idvar = c("VAR_NAMES"),
            timevar = "cl",
            direction = "wide"
          )

          names(result2)[startsWith(names(result2), "value.")] <-
            substr(
              names(result2)[startsWith(
                names(result2),
                "value."
              )],
              nchar("value.") + 1,
              nchar(names(result2)[startsWith(
                names(result2),
                "value."
              )])
            )
        }

        # create a dataframe with unique call_names in the first column and
        # indicator_metrics in the second column
        mapping_of_cns <-
          expand.grid(
            call_names = unique(result$call_names),
            indicator_metric = unique(na.omit(result$indicator_metric)),
            stringsAsFactors = FALSE
          )


        # append a new column with function names
        mapping_of_cns$fname <-
          function_name_mapping[mapping_of_cns[["call_names"]]]
        # create a new column called colname containing the combination
        # selected for grouped-by. Example:
        # if only indicator_metric was selected, the column will contain only
        # indicator_metrics
        if (all(c("call_names", "indicator_metric") %in% grouped_by)) {
          mapping_of_cns$colname <- paste0(
            mapping_of_cns[["call_names"]], "_",
            mapping_of_cns[["indicator_metric"]]
          )
        } else if (all(c("call_names") %in% grouped_by)) {
          mapping_of_cns$colname <- paste0(mapping_of_cns[["call_names"]])
          # } else if(all(c("indicator_metric") %in% grouped_by)) {
          # History: indicator_metric-only colname used indicator_metric.
        } else {
          util_error("this group by is not yet supported")
        }
        # create a list of suffixes for each call_name and function name
        # example for acc_loess_observer, the suffix is observer

        mapping_of_cns$fname2 <-
          apply(mapping_of_cns[, "call_names", drop = FALSE],
            1,
            FUN = function(x) {
              util_map_by_largest_prefix(x,
                haystack =
                  util_all_ind_functions()
              )
            }
          )

        suffixes <- mapply(
          SIMPLIFY = FALSE,
          cn = mapping_of_cns[["call_names"]],
          fn = mapping_of_cns$fname2,
          FUN = function(cn, fn) {
            if (startsWith(cn, fn)) {
              substr(cn, nchar(fn) + 1 + 1, nchar(cn))
              # name + "_" (first +1), start is the next character (second +1)
            } else {
              cn
            }
          }
        )

        # get a table with the short-name, implementations, and more for each
        # function
        acronyms_tab <- util_get_concept_info("implementations")

        # === Define the title of the matrix columns =====
        # First select the short name of the function from "implementations" in
        # dq control file
        mapping_of_cns$acronyms <-
          util_map_labels(mapping_of_cns$fname2,
            acronyms_tab,
            # The matrix-column report title is preferred over legacy titles.
            to = "matrix_column_title_report",
            from = "function_R",
            ifnotfound = util_abbreviate(mapping_of_cns$fname)
          )
        # Prepare the suffixes
        suffixes <- gsub("_", " ", vapply(suffixes, as.character,
            FUN.VALUE = character(1)
          ))
        suffixes[!util_empty(suffixes)] <-
          paste0(
            ":",
            abbreviate(
              suffixes[!util_empty(suffixes)],
              minlength = 3
            )
          )

        # Prepare the indicator names
        mapping_of_cns$imtitle <-
          util_translate_indicator_metrics(mapping_of_cns[["indicator_metric"]],
            long = FALSE,
            short = FALSE
          )
        empty_imtitle <- util_empty(mapping_of_cns$imtitle)
        mapping_of_cns$imtitle[empty_imtitle] <-
          mapping_of_cns[["indicator_metric"]][empty_imtitle]

        mapping_of_cns$imtitle_short <-
          util_translate_indicator_metrics(mapping_of_cns[["indicator_metric"]],
            long = FALSE, short = TRUE
          )
        mapping_of_cns$imtitle_short[
          util_empty(mapping_of_cns$imtitle_short)
        ] <-
          mapping_of_cns[["indicator_metric"]][
            util_empty(mapping_of_cns$imtitle_short)
          ]
        has_empty_output <-
          mapping_of_cns$indicator_metric == "EMPTY_OUTPUT"
        mapping_of_cns$imtitle[has_empty_output] <-
          "No regular output"
        mapping_of_cns$imtitle_short[has_empty_output] <-
          "No regular output"

        # Create the actual column names for the matrix in column "coltitle"
        if (all(c("call_names", "indicator_metric") %in% grouped_by)) {
          mapping_of_cns$coltitle <- paste0(
            mapping_of_cns$acronyms, suffixes,
            ", ", mapping_of_cns$imtitle_short
          )
        } else if (all(c("call_names") %in% grouped_by)) {
          mapping_of_cns$coltitle <- paste0(mapping_of_cns$acronyms, suffixes)
          # } else if(all(c("indicator_metric") %in% grouped_by)) {
          # History: indicator_metric-only coltitle used imtitle plus suffixes.
        } else {
          util_error("this group by is not yet supported")
        }

        # Definition of the column-title hover text description in the final
        # matrix.
        #      if("call_names" %in% grouped_by){
        # in all other cases of grouped_by
        # create column-title hover text description in the final matrix
        mapping_of_cns$coldesc <- vapply(mapping_of_cns$fname,
          FUN.VALUE = character(1),
          util_col_description
        )

        # order the column based on the object ordered_call_names
        # First create an order based on call names e.g., 3
        mapping_of_cns$order_call <-
          setNames(seq_along(this$ordered_call_names),
            nm = this$ordered_call_names
          )[
            mapping_of_cns[["call_names"]]
          ]
        mapping_of_cns$order_slot <- util_order_of_indicator_metrics(
          mapping_of_cns[["indicator_metric"]]
        )

        # Then combine the 2 values e.g., 3009
        mapping_of_cns$order <-
          (mapping_of_cns$order_call * 10**
            ceiling(log(max(mapping_of_cns$order_slot, na.rm = TRUE),
                base = 10
              ))) + mapping_of_cns$order_slot

        # create an object with the order
        order_of_cols <- c(
          VAR_NAMES = 0,
          setNames(mapping_of_cns$order, nm = mapping_of_cns$colname)
        )

        # check if the columns of result2 match the values in order_of_cols
        util_stop_if_not(all(colnames(result2) %in% names(order_of_cols)))

        # reorder the result columns based on the order object just created
        result2 <-
          result2[, order(order_of_cols[colnames(result2)]), FALSE]


        # create a list containing each combination of the selected grouped_by
        # the correspondent description
        descs <- setNames(mapping_of_cns$coldesc,
          nm = mapping_of_cns$colname
        )[colnames(result2)]
        descs[util_empty(descs)] <- ""

        # rename the columns of result2 with the user friendly name
        colnames_tech <- colnames(result2)
        colnames(result2)[colnames(result2) %in% mapping_of_cns$colname] <-
          setNames(mapping_of_cns$coltitle,
            nm = mapping_of_cns$colname
          )[
            colnames(result2)[colnames(result2) %in%
              mapping_of_cns$colname]
          ]

        # creation of the final matrix in case indicator_metric is selected ===
      } else if (grouped_by == "indicator_metric") {
        result2 <- result1[, c(
          "VAR_NAMES",
          "indicator_metric_suff",
          "category",
          "value"
        ), drop = FALSE]
        has_value_by_column <- tapply(
          !util_empty(result2$category),
          result2$indicator_metric_suff, any
        )
        empty_output_columns_tech <-
          names(has_value_by_column)[!has_value_by_column]
        result2 <- result2[, c("VAR_NAMES", "indicator_metric_suff", "value"),
          drop = FALSE
        ]

        # remove dupliates in case present
        result2 <- dplyr::distinct(result2)

        result2 <- stats::reshape(result2,
          idvar = c("VAR_NAMES"),
          timevar = "indicator_metric_suff",
          direction = "wide"
        )

        names(result2)[startsWith(names(result2), "value.")] <-
          substr(
            names(result2)[startsWith(
              names(result2),
              "value."
            )],
            nchar("value.") + 1,
            nchar(names(result2)[startsWith(
              names(result2),
              "value."
            )])
          )

        # create a dataframe with unique call_names in the first column and
        # indicator_metrics in the second column
        mapping_of_cns <-
          expand.grid(
            call_names = unique(result$call_names),
            indicator_metric = unique(result$indicator_metric),
            stringsAsFactors = FALSE
          )


        # add a new column with the function name
        mapping_of_cns$function_name2 <-
          apply(
            mapping_of_cns[, "call_names",
              drop = FALSE
            ],
            1,
            FUN = function(x) {
              util_map_by_largest_prefix(x,
                haystack =
                  util_all_ind_functions()
              )
            }
          )

        # add a column with suffixes
        mapping_of_cns <-
          dplyr::mutate(mapping_of_cns,
            suffixes = ifelse(
              startsWith(
                mapping_of_cns[["call_names"]],
                mapping_of_cns[["function_name2"]]
              ),
              substr(
                mapping_of_cns[["call_names"]],
                nchar(mapping_of_cns[["function_name2"]]) +
                  1 + 1,
                nchar(mapping_of_cns[["call_names"]])
              ),
              # name + "_" (first +1), start is the next character (second +1)
              mapping_of_cns[["call_names"]]
            )
          )

        # create a new column with indicator_metric and suffixes if present
        mapping_of_cns <-
          dplyr::mutate(mapping_of_cns,
            indicator_metric_suff =
            ifelse(!util_empty(get("suffixes")),
              paste0(
                get("indicator_metric"), "_",
                get("suffixes")
              ),
              get("indicator_metric")
            )
          )

        # append a new column with function names
        mapping_of_cns$fname <-
          function_name_mapping[mapping_of_cns[["call_names"]]]
        # create a new column called colname containing the combination
        # selected for grouped-by. Example:
        # if only indicator_metric was selected, the column will contain only
        # indicator_metrics
        mapping_of_cns$colname <-
          paste0(mapping_of_cns[["indicator_metric"]])
        mapping_of_cns$colname_suff <-
          paste0(mapping_of_cns[["indicator_metric_suff"]])

        # Prepare the indicator names
        mapping_of_cns$imtitle <-
          util_translate_indicator_metrics(mapping_of_cns[["indicator_metric"]],
            long = FALSE, short = FALSE
          )
        empty_imtitle <- util_empty(mapping_of_cns$imtitle)
        mapping_of_cns$imtitle[empty_imtitle] <-
          mapping_of_cns[["indicator_metric"]][empty_imtitle]
        has_empty_output <-
          mapping_of_cns$colname == "EMPTY_OUTPUT" |
          mapping_of_cns$colname_suff == "EMPTY_OUTPUT" |
          mapping_of_cns$indicator_metric == "EMPTY_OUTPUT" |
          mapping_of_cns$indicator_metric_suff == "EMPTY_OUTPUT" |
          mapping_of_cns$suffixes == "EMPTY_OUTPUT"
        mapping_of_cns$imtitle[has_empty_output] <-
          "No regular output"
        mapping_of_cns$suffixes[has_empty_output] <-
          ""

        # Create the actual column names for the matrix in column "coltitle"
        mapping_of_cns$coltitle <- paste0(
          mapping_of_cns$imtitle, " ",
          mapping_of_cns$suffixes
        )
        mapping_of_cns$coltitle <- trimws(mapping_of_cns$coltitle)

        # Definition of the column-title hover text description in the final
        # matrix.
        # remove prefix from indicator name
        mapping_of_cns$indicators_no_prefix <-
          ifelse(mapping_of_cns$colname == "EMPTY_OUTPUT",
            "EMPTY_OUTPUT",
            gsub(
              ".*?_(.*)", "\\1",
              vapply(mapping_of_cns$colname,
                as.character,
                FUN.VALUE = character(1)
              )
            )
          )
        # import the dqi informations
        # The order follows order_nr and AbbreviationMetrics$order.
        dqi_info <- util_get_concept_info("dqi")
        # use the Definition from dqi to create the hover text description
        mapping_of_cns$coldesc <- NA_character_
        has_regular_indicator <-
          mapping_of_cns$indicators_no_prefix %in% dqi_info$abbreviation
        mapping_of_cns$coldesc[has_regular_indicator] <-
          util_map_labels(
            mapping_of_cns$indicators_no_prefix[
              has_regular_indicator
            ],
            dqi_info,
            to = "Definition",
            from = "abbreviation"
          )
        mapping_of_cns$coldesc[
          mapping_of_cns$colname == "EMPTY_OUTPUT"
        ] <-
          paste(
            "No regular indicator output is available for this column;",
            "only diagnostic information was produced. This commonly",
            "happens for applicability problems, for example if an",
            "indicator requires a group variable but no group variable",
            "was provided in the metadata. Check the cell hover text for",
            "the concrete message."
          )


        # order the columns
        # create an order based on indicator metric e.g., 9
        mapping_of_cns$order <- util_order_of_indicator_metrics(
          mapping_of_cns[["indicator_metric_suff"]]
        )

        # create an object with the order
        order_of_cols <- c(
          VAR_NAMES = 0,
          setNames(mapping_of_cns$order, nm = mapping_of_cns$colname_suff)
        )

        util_stop_if_not(all(colnames(result2) %in% names(order_of_cols)))

        # reorder the result columns based on the order object just created
        result2 <-
          result2[, order(order_of_cols[colnames(result2)]), FALSE]

        # create a list containing each combination of the selected grouped_by
        # the correspondent description
        descs <- setNames(mapping_of_cns$coldesc,
          nm = mapping_of_cns$colname_suff
        )[colnames(result2)]
        descs[util_empty(descs)] <- ""

        # rename the columns of result2 with the user friendly name
        colnames_tech <- colnames(result2)
        colnames(result2)[colnames(result2) %in% mapping_of_cns$colname_suff] <-
          setNames(mapping_of_cns$coltitle,
            nm = mapping_of_cns$colname_suff
          )[
            colnames(result2)[colnames(result2) %in%
              mapping_of_cns$colname_suff]
          ]
      }

      if (identical(vars_to_include, "variable_group")) {
        value_columns <- setdiff(colnames(result2), VAR_NAMES)
        for (value_column in value_columns) {
          missing_value <- util_summary_display_value_missing(
            result2[[value_column]]
          )
          result2[[value_column]][missing_value] <- ""
        }
      }

      variable_group_ids <- NULL
      variable_group_link_ids <- NULL
      # replace var_names with labels in the first column of the matrix
      if (VAR_NAMES %in% colnames(result2)) {
        Variables <- if (identical(vars_to_include, "variable_group")) {
          variable_group_ids <- as.character(result2[[VAR_NAMES]])
          variable_group_link_ids <- util_result_variable_group_ids(
            variable_group_ids,
            this$result
          )
          fallback_meta_data <- this$summary_meta_data
          if (!is.data.frame(fallback_meta_data) ||
              !all(c(VAR_NAMES, this$label_col) %in%
                  colnames(fallback_meta_data))) {
            fallback_meta_data <- meta_data
          }
          group_labels <- util_map_labels(
            variable_group_ids,
            meta_data = fallback_meta_data,
            to = this$label_col,
            from = VAR_NAMES,
            ifnotfound = variable_group_ids
          )
          labels_in_report <- this$labels_of_var_names_in_report
          resolved_labels <- if (length(labels_in_report)) {
            as.character(labels_in_report[variable_group_ids])
          } else {
            rep(NA_character_, length(variable_group_ids))
          }
          missing_labels <- is.na(group_labels) | util_empty(group_labels)
          group_labels[missing_labels] <- resolved_labels[missing_labels]
          group_labels
        } else {
          util_map_labels(result2[[VAR_NAMES]],
            meta_data = meta_data,
            to = this$label_col,
            from = VAR_NAMES,
            ifnotfound = result2[[VAR_NAMES]]
          )
        }
        Variables[is.na(Variables) | util_empty(Variables)] <-
          result2[[VAR_NAMES]][is.na(Variables) | util_empty(Variables)]
        # The requested label column is kept from the summary context.
        result2[[VAR_NAMES]] <- Variables
        if (identical(vars_to_include, "variable_group") &&
            this$label_col %in% colnames(result2)) {
          result2[[this$label_col]] <- NULL
        }
        # Keep the historical internal name until ordering and totals have been
        # added below. Several of those steps intentionally address `Variables`.
        colnames(result2)[colnames(result2) == VAR_NAMES] <- "Variables"
        if (!is.null(names(descs))) {
          names(descs)[names(descs) == VAR_NAMES] <- "Variables"
        }
        label_col <- this$label_col
      } else {
        label_col <- VAR_NAMES
      }

      # In case of summary for dq_report_by
      # Create the href for each cell "Total"
      if (!is.null(folder_of_report) &&
          !identical(vars_to_include, "variable_group")) {
        # In case there is more than one strata, and the name of the variables
        # are modified because repeated in several folders
        if (!is.null(var_uniquenames)) {
          # Prepare vars_folder_of_report: a named vector containing the new
          # name
          # of the variable (e.g., "sd1-SEX_0_1-ITEM_5_0"). The names of this
          # vector contains the name of the folder of the report
          vars_folder_of_report <- setNames(
            names(folder_of_report),
            folder_of_report
          )
          vars_folder_of_report <- gsub(
            "^(.*)\\.(.*)", "\\1",
            vars_folder_of_report
          )
          vars_folder_of_report <-
            vars_folder_of_report[!duplicated(vars_folder_of_report)]
          # Prepare a data frame with the different names of the same variable
          var_uniquenames <- do.call("rbind", var_uniquenames)
          rownames(var_uniquenames) <- NULL

          # Prepare a named vector of the variables (e.g., SEX_0) and the folder
          name_and_folder <- vars_folder_of_report
          simple_var_names <-
            var_uniquenames[var_uniquenames$new_names_with_label %in%
              name_and_folder, , drop = FALSE]
          name_and_folder <- setNames(simple_var_names$name_label,
            nm = names(name_and_folder)
          )

          # ix the names of this vector so that include the complete path to a
          # variable inside the folder
          names(vars_folder_of_report) <-
            mapply(
              x = prep_link_escape(name_and_folder),
              y = names(vars_folder_of_report),
              SIMPLIFY = FALSE,
              FUN = function(x, y) {
                z <- paste0(y, "/.report/VAR_", x, ".html#", x)
                return(z)
              }
            )

          # rename content of vars_folder_of_report, so that now contain
          # the new name e.g.,  "sd1-SEX_0_0-v00000" instead of
          # previous "sd1-SEX_0_0-CENTER_0" (to match content of this$rowmaxes)
          var_uniquenames_fil <-
            var_uniquenames[var_uniquenames$new_names_with_label %in%
              vars_folder_of_report, , drop = FALSE]
          vars_folder_of_report <-
            setNames(var_uniquenames_fil$new_names_with_varnames,
              nm = names(vars_folder_of_report)
            )

          # make unique the names a created named vector containing
          # this$rowmaxes$cell_text. The names comes now from the
          # column this$rowmaxes$VAR_NAMES, because
          # the names of this$rowmaxes$cell_text contain repeated variable names
          new_rowmaxes_cell_text <- this$rowmaxes$cell_text
          names(new_rowmaxes_cell_text) <- this$rowmaxes$VAR_NAMES

          # replace the href in rowmaxes used for links from var_names and
          # total cells
          this$rowmaxes$cell_text <-
            mapply(
              needle = setNames(nm = names(new_rowmaxes_cell_text)),
              MoreArgs = list(
                haystack = new_rowmaxes_cell_text,
                corresponding_value_for_needle = vars_folder_of_report
              ),
              SIMPLIFY = FALSE,
              FUN = function(needle, corresponding_value_for_needle,
                haystack) {
                haystack <- haystack[[as.character(needle)]]
                haystack <-
                  gsub('(?=VAR_)(.*)(?="\\sstyle)',
                    names(corresponding_value_for_needle[
                      corresponding_value_for_needle %in% needle
                    ]),
                    haystack,
                    perl = TRUE
                  )
                return(haystack)
              }
            )
        } else {
          # Create a named vector vars_folder_of_report containing the variable
          # name (e.g., "SEX_0"). The names are the folder of the variable
          vars_folder_of_report <- setNames(
            names(folder_of_report),
            folder_of_report
          )
          vars_folder_of_report <- gsub(
            "^(.*)\\.(.*)", "\\1",
            vars_folder_of_report
          )
          vars_folder_of_report <-
            vars_folder_of_report[!duplicated(vars_folder_of_report)]

          names(vars_folder_of_report) <-
            mapply(
              variable_nm = prep_link_escape(vars_folder_of_report),
              folder_for_href = names(vars_folder_of_report),
              SIMPLIFY = FALSE,
              FUN = function(variable_nm, folder_for_href) {
                z <- paste0(
                  folder_for_href, "/.report/VAR_",
                  variable_nm, ".html#", variable_nm
                )
                return(z)
              }
            )

          # replace the href in rowmaxes used for links from var_names and
          # Total cells
          this$rowmaxes$cell_text <-
            mapply(
              x = setNames(nm = names(this$rowmaxes$cell_text)),
              MoreArgs = list(
                z = this$rowmaxes$cell_text,
                y = vars_folder_of_report
              ),
              SIMPLIFY = FALSE,
              FUN = function(x, y, z) {
                z <- z[[as.character(x)]]

                z <- gsub('(?=VAR_)(.*)(?="\\sstyle)',
                  names(y[y %in% x]),
                  z,
                  perl = TRUE
                )
                return(z)
              }
            )
        }
      }

      this$rowmaxes$VAR_NAMES_mapped <- prep_map_labels(this$rowmaxes$VAR_NAMES,
        meta_data = meta_data,
        to = label_col,
        ifnotfound = this$rowmaxes$VAR_NAMES
      )

      ordered_var_names_mapped <- prep_map_labels(ordered_var_names,
        meta_data = meta_data,
        to = label_col,
        ifnotfound = ordered_var_names
      )

      result2 <- result2[
        util_order_by_order(
          result2$Variables,
          ordered_var_names_mapped
        ), ,
        drop = FALSE
      ]
      if (identical(vars_to_include, "variable_group") &&
          length(variable_group_ids) == length(Variables)) {
        variable_group_ids <- setNames(
          variable_group_ids,
          Variables
        )[result2$Variables]
        variable_group_link_ids <- setNames(
          variable_group_link_ids,
          Variables
        )[result2$Variables]
      }

      result2$Total <- setNames(this$rowmaxes$cell_text,
        nm = this$rowmaxes$VAR_NAMES_mapped
      )[result2$Variables]

      if (identical(vars_to_include, "variable_group") &&
          length(variable_group_ids) == nrow(result2)) {
        result2$Total <- util_relink_variable_group_total_cells(
          result2$Total,
          variable_group_link_ids,
          this$meta_data_cross_item
        )
        descs <- c(descs, "Total DQ for variable groups")
      } else {
        descs <- c(descs, "Total DQ for Items")
      }


      colnames_speaking_from_tech <- setNames(colnames(result2),
        nm = colnames_tech
      )
      empty_output_columns <-
        colnames_speaking_from_tech[empty_output_columns_tech]
      empty_output_columns <- empty_output_columns[
        empty_output_columns %in% colnames(result2)
      ]

      add_missing_empty_output_reason <- function(x) {
        needs_reason <- grepl("No results available", x, fixed = TRUE) &
          !grepl("Messages:", x, fixed = TRUE)
        if (!any(needs_reason)) {
          return(x)
        }
        reason <- paste0(
          "&lt;br&gt;Messages: &lt;br&gt;&#10;&lt;ul&gt;&#10;",
          "&lt;li&gt;&lt;span class=&quot;dataquieR-message-message&quot;&gt;",
          "Assessment not reasonable / not requested: no result was ",
          "produced for this variable/function combination. This can happen ",
          "when the metadata, for example the variable role, do not request ",
          "or permit this assessment.",
          "&lt;/span&gt;&lt;/li&gt;&#10;&lt;/ul&gt;&#10;"
        )
        x[needs_reason] <-
          sub("(&lt;br/&gt;)(\"\\s*>\\s*<a)",
            paste0("\\1", reason, "\\2"),
            x[needs_reason],
            perl = TRUE
          )
        x
      }
      for (empty_output_column in empty_output_columns) {
        result2[[empty_output_column]] <-
          add_missing_empty_output_reason(result2[[empty_output_column]])
      }

      clean_col_tag <- function(x) {
        unique(x[!is.na(x) & x %in% colnames(result2)])
      }

      metric_dimensions <- util_summary_metric_dimensions(colnames_tech)
      columns_for_dimension <- function(dimension) {
        technical_names <- names(metric_dimensions)[
          !is.na(metric_dimensions) & metric_dimensions == dimension
        ]
        unname(colnames_speaking_from_tech[technical_names])
      }

      summary_col_tags <- list(
        `All columns` = clean_col_tag(setdiff(
          colnames(result2),
          empty_output_columns
        )),
        `Integrity` = clean_col_tag(c(
          "Variables",
          setdiff(columns_for_dimension("Integrity"), empty_output_columns),
          "Total"
        )),
        `Completeness` = clean_col_tag(c(
          "Variables",
          setdiff(columns_for_dimension("Completeness"),
            empty_output_columns
          ),
          "Total"
        )),
        `Consistency` = clean_col_tag(c(
          "Variables",
          setdiff(columns_for_dimension("Consistency"),
            empty_output_columns
          ),
          "Total"
        )),
        `Accuracy` = clean_col_tag(c(
          "Variables",
          setdiff(columns_for_dimension("Accuracy"), empty_output_columns),
          "Total"
        ))
      )
      if (length(empty_output_columns) > 0) {
        summary_col_tags[["Empty output columns"]] <-
          util_attach_attr(
            clean_col_tag(c("Variables", empty_output_columns, "Total")),
            cssClass = "dq-hidden-col"
          )
      }

      summary_grading_args <- list(
        grading_cols = I(seq_len(ncol(result2) - 1L)),
        grading_order = util_get_labels_grading_class(),
        grading_colors = util_get_colors(),
        fg_colors = util_get_fg_color(util_get_colors())
      )

      # In case of summary for dq_report_by
      if (!is.null(folder_of_report)) {
        if (identical(vars_to_include, "variable_group")) {
          group_origins <- mapply(
            variable_group_ids,
            variable_group_link_ids,
            FUN = function(group_id, check_id) {
              suffix <- paste0("-", check_id)
              if (endsWith(group_id, suffix)) {
                substr(group_id, 1L, nchar(group_id) - nchar(suffix))
              } else {
                group_id
              }
            },
            USE.NAMES = FALSE
          )
          report_folders <- vapply(group_origins, function(origin) {
            candidates <- unique(unname(folder_of_report[
              startsWith(names(folder_of_report), paste0(origin, "-"))
            ]))
            if (length(candidates) == 1L) candidates else NA_character_
          }, character(1))
          href <- paste0(report_folders, "/.report/report.html")
          href[is.na(report_folders)] <- NA_character_
          href <- util_relink_variable_group_hrefs(
            href,
            variable_group_link_ids,
            this$meta_data_cross_item
          )
          href <- setNames(href, result2$Variables)
          filter <- setNames(result2$Variables, nm = result2$Variables)
          data <- setNames(result2$Variables, nm = result2$Variables)
        } else if (!is.null(var_uniquenames)) {
          # if more than 1 strata is present
          # create a named vector with as variable name the new name that
          # will appear on the table (it has extra spaces)
          # (e.g.,  "sd1-SEX_0_1 - QUEST_DT_0" instead of
          # "sd1-SEX_0_1-QUEST_DT_0") and as name
          # the folder containing the variable
          new_var_name_in_table <- vars_folder_of_report
          simple_var_names <-
            var_uniquenames[var_uniquenames$new_names_with_varnames %in%
              new_var_name_in_table, , drop = FALSE]

          new_var_name_in_table <-
            setNames(simple_var_names$result2_Variables_match,
              nm = names(new_var_name_in_table)
            )

          # create a new named vector href with the correct folder (as name) and
          # the new variable name to read in the table (with extra white spaces)
          href <-
            setNames(
              names(new_var_name_in_table[new_var_name_in_table %in%
                    result2$Variables]),
              nm = new_var_name_in_table[new_var_name_in_table %in%
                  result2$Variables]
            )
          filter <- setNames(result2$Variables, nm = result2$Variables)
          data <- setNames(result2$Variables, nm = result2$Variables)
        } else {
          # define the href, a named vector containing the folder of the
          # variable.
          # The names contain the variable name (e.g., "SEX_0")
          href <-
            setNames(
              names(vars_folder_of_report[vars_folder_of_report %in%
                    result2$Variables]),
              nm = vars_folder_of_report[vars_folder_of_report %in%
                  result2$Variables]
            )
          filter <- setNames(result2$Variables, nm = result2$Variables)
          data <- setNames(result2$Variables, nm = result2$Variables)
        }

        result2$Variables <-
          mapply(
            x = setNames(nm = result2$Variables),
            MoreArgs = (list(
              href = href,
              filter = filter,
              data = data
            )), SIMPLIFY = FALSE,
            FUN = function(x, href, filter, data) {
              text_for_html <-
                paste0(
                  "<pre ",
                  'onclick=\"\"',
                  " style=",
                  '"height: 100%; min-height: 2em; font-size: 16px; font-family: serif; font-style: normal; margin: 0em; color: #0066CC; padding: 0em; background: #ffffff; cursor: pointer; text-align: right;"', # nolint: line_length_linter.
                  " filter=\"", filter[x], "\">\n",
                  "<a href=\"", href[names(href) %in% x], '"',
                  'style=\"text-decoration:none;display:block;',
                  "\"> ",
                  data[x], "</a>\n</pre>"
                )
              as.character(text_for_html)
              return(text_for_html)
            }
          )

        if (identical(vars_to_include, "variable_group")) {
          colnames(result2)[colnames(result2) == "Variables"] <-
            "Variable group"
          if (!is.null(names(descs))) {
            names(descs)[names(descs) == "Variables"] <- "Variable group"
          }
          summary_col_tags <- lapply(summary_col_tags, function(columns) {
            columns[columns == "Variables"] <- "Variable group"
            columns
          })
        }

        tb <-
          util_suppress_warnings(util_html_table(
            result2,
            filter = "top", options = list(
              scrollCollapse = TRUE,
              scrollY = "75vh",
              order = list()
            ),
            is_matrix_table = TRUE, rotate_headers =
              (ncol(result2) > 5),
            link_variables = FALSE,
            cols_are_indicatormetrics = !TRUE,
            colnames_aliases2acronyms = FALSE,
            descs = descs,
            meta_data = meta_data,
            label_col = label_col,
            dl_fn = prep_link_escape(
              paste0(
                "Summary-",
                this[["title"]],
                "-",
                this[["subtitle"]]
              ),
              html = TRUE
            ),
            title = this[["title"]],
            messageTop = this[["subtitle"]],
            messageBottom = sprintf(
              "generated by %s %s",
              paste(packageName()),
              paste(packageVersion(
                packageName()
              ))
            ),
            hideCols = empty_output_columns,
            col_tags = summary_col_tags,
            additional_init_args = summary_grading_args
          ), classes = LONG_LABEL_EXCEPTION)
      } else {
        if (identical(vars_to_include, "variable_group")) {
          colnames(result2)[colnames(result2) == "Variables"] <-
            "Variable group"
          if (!is.null(names(descs))) {
            names(descs)[names(descs) == "Variables"] <- "Variable group"
          }
          summary_col_tags <- lapply(summary_col_tags, function(columns) {
            columns[columns == "Variables"] <- "Variable group"
            columns
          })
        }
        tb <- util_suppress_warnings(
          util_html_table(
            result2,
            filter = "top", options = list(
              scrollCollapse = TRUE,
              scrollY = "75vh",
              order = list()
            ),
            is_matrix_table = TRUE, rotate_headers =
              (ncol(result2) > 5),
            link_variables = !identical(
              vars_to_include,
              "variable_group"
            ),
            cols_are_indicatormetrics = !TRUE,
            colnames_aliases2acronyms = FALSE,
            descs = descs,
            meta_data = meta_data,
            label_col = label_col,
            dl_fn = prep_link_escape(
              paste0(
                "Summary-",
                this[["title"]],
                "-",
                this[["subtitle"]]
              ),
              html = TRUE
            ),
            title = this[["title"]],
            messageTop = this[["subtitle"]],
            messageBottom = sprintf(
              "generated by %s %s",
              paste(packageName()),
              paste(packageVersion(
                packageName()
              ))
            ),
            hideCols = empty_output_columns,
            col_tags = summary_col_tags,
            additional_init_args = summary_grading_args
          ),
          classes = LONG_LABEL_EXCEPTION
        )
      }

      jqui <- rmarkdown::html_dependency_jqueryui()
      jqui$stylesheet <- "jquery-ui.min.css"

      res <- htmltools::browsable(htmltools::tagList(
        rmarkdown::html_dependency_jquery(),
        jqui,
        html_dependency_tippy(),
        tb,
        htmltools::tags$script(paste0(
          "$(function() {",
          "if (window.dataquieRInitTippies) {",
          ' window.dataquieRInitTippies($(".table_result"));',
          "}",
          "})"
        )),
        htmltools::tags$script(paste0(
          "$(function() {",
          'if (!window.hasOwnProperty("dq_report2") || !window.dq_report2) {',
          'if (!window.hasOwnProperty("dq_report_by_overview") || !window.dq_report_by_overview) {', # nolint: line_length_linter.
          ' $("a").attr("href", "javascript:alert(\\"links work in dq_report2 reports, only.\\")")', # nolint: line_length_linter.
          "} else {",
          " ",
          "}",
          "}} )"
        ))
      ))
    }

    util_attach_attr(res,
      repsum_wide = util_attach_attr(result2,
        label_col = label_col
      ),
      this = this
    )
  })
}
