#' Mark a `dataquieR_result` for a special HTML layout
#'
#' The layout information is used by `util_pretty_print()` when wrapping
#' the rendered result into the outer `div.dataquieR_result`.  Currently
#' supported layouts are `"default"` (legacy behavior) and
#' `"2-columns-fig-left"` (plot and summary table combined in a single
#' result, shown side by side).
#'
#' @param dqr [dataquieR_result] object to tag.
#' @param layout [character] one of `"default"`, `"1-column-fig-top"`
#'                           (this is the default)
#'                           or `"2-columns-fig-left"`.
#'
#' @return `dqr`, invisibly, with the attribute `dq_layout` set.
#' @noRd
util_mark_result_layout <- function(
  dqr,
  layout = c(
    "default",
    "1-column-fig-top",
    "2-columns-fig-left"
  )
) {
  layout <- match.arg(layout)
  attr(dqr, "dq_layout") <- layout
  invisible(dqr)
}

#' Get the layout tag of a `dataquieR_result`
#'
#' @param dqr [dataquieR_result].
#'
#' @return [character] layout name, `"default"` if unset.
#' @noRd
util_get_result_layout <- function(dqr) {
  layout <- util_attr(dqr, "dq_layout", exact = TRUE)
  if (is.null(layout)) {
    layout <- "default"
    fn <- util_attr(dqr, "function_name", exact = TRUE)
    if (length(fn) == 1 && is.character(fn)) {
      lt <- suppressWarnings(try(
        unique(util_get_concept_info("implementations",
            get("function_R") == fn, "layout",
            drop = TRUE
          )),
        silent = TRUE
      ))
      if (length(lt == 1) && is.character(lt)) {
        layout <- lt
      }
    }
  }
  layout
}

#' Get the explicit title for a rendered result group
#'
#' @param dqr [dataquieR_result].
#'
#' @return `NULL` or an `htmltools` heading with optional description.
#' @noRd
util_result_group_heading <- function(dqr) {
  title <- util_attr(dqr, "dq_result_title", exact = TRUE)
  if (length(title) != 1 ||
      !is.character(title) ||
      is.na(title) ||
      !nzchar(title)) {
    return(NULL)
  }

  description <- util_attr(dqr, "dq_result_description", exact = TRUE)
  if (length(description) != 1 ||
      !is.character(description) ||
      is.na(description) ||
      !nzchar(description)) {
    description <- NULL
  } else {
    description <- htmltools::div(
      class = "infobutton",
      htmltools::HTML(description)
    )
  }
  htmltools::tagList(htmltools::h5(title), description)
}

#' Wrap inner HTML into a `dataquieR_result` container
#'
#' This helper centralizes the construction of the outer
#' `div.dataquieR_result` element.  It keeps legacy data attributes and
#' allows attaching additional CSS classes.
#'
#' @param inner `shiny.tag|shiny.tag.list` inner HTML to wrap.
#' @param nm [character] value for the `data-nm` attribute.
#' @param dqr [dataquieR_result] the original result object (used for
#'   the stored call).
#' @param errors [character] error messages.
#' @param warnings [character] warning messages.
#' @param messages [character] additional messages.
#' @param extra_classes [character] optional vector of extra CSS
#'   classes to add to the `dataquieR_result` container.
#' @param popup_nm [character] optional shared identifier for grouped report
#'   links, stored separately from `data-nm`.
#'
#' @return `shiny.tag` `div.dataquieR_result`.
#' @noRd
util_wrap_dqr_result <- function(
  inner,
  nm,
  dqr,
  errors,
  warnings,
  messages,
  extra_classes = NULL,
  popup_nm = NULL
) {
  cll <- util_attr(dqr, "call", exact = TRUE)
  if (is.language(cll)) {
    cll <- deparse(cll)
  }

  classes <- c("dataquieR_result", extra_classes)
  classes <- classes[!is.na(classes) & nzchar(classes)]
  classes <- paste(classes, collapse = " ")

  htmltools::div(
    title = "", # hover text already suppressed in legacy code
    class = classes,
    `data-call` = paste0(cll, collapse = "\n"),
    `data-stderr` = paste(errors, warnings, messages, collapse = "\n"),
    `data-nm` = nm,
    `data-popup-nm` = popup_nm,
    inner
  )
}

#' Convert single `dataquieR` result to an `htmltools` compatible object
#'
#' @param dqr [dataquieR_result] an output (indicator) from `dataquieR`
#' @param nm [character] the name used in the report, the alias name of the
#'                       function call plus the variable name
#' @param is_single_var [logical] we are creating a single variable overview
#'                                page or an indicator summary page
#' @param meta_data [meta_data]  the data frame that contains metadata
#'                               attributes of study data
#' @inheritParams .template_function_developer
#' @param use_plot_ly [logical] use `plotly`
#' @param dir [character] output directory for potential `iframes`.
#' @param ... further arguments passed through, if applicable
#' @param is_ssi [logical] use this is a result for the `SSI` part of the report
#' @param ssi_link_target [character] target page type for SSI computed
#'        variable links
#' @param rotate_for_one_row [logical] rotate one-row tables into key-value
#'        tables.
#' @param link_variables [logical] link variable labels to report pages.
#' @param popup_nm [character] optional shared result-group identifier used by
#'        report links. The regular `nm` remains unique for plot files and
#'        figure controls.
#'
#' @return `htmltools` compatible object with rendered `dqr`
#'
#' @noRd
util_pretty_print <- function(
  dqr, nm, is_single_var,
  meta_data,
  meta_data_cross_item = NULL,
  label_col,
  use_plot_ly,
  dir,
  ...,
  is_ssi = FALSE,
  ssi_link_target = c("role", "cross_item"),
  rotate_for_one_row = TRUE,
  link_variables = TRUE,
  popup_nm = NULL
) {
  variable_name_context <- FALSE
  ssi_link_target <- util_match_arg(ssi_link_target)
  util_expect_scalar(rotate_for_one_row, check_type = is.logical)
  group_id <- util_attr(dqr, CHECK_ID, exact = TRUE)
  group_label <- util_attr(dqr, CHECK_LABEL, exact = TRUE)
  has_group_id <- length(group_id) == 1L && !util_empty(group_id)
  has_group_label <- length(group_label) == 1L && !util_empty(group_label)

  if (use_plot_ly) {
    plot_figure <- util_plot_figure_plotly
  } else {
    plot_figure <- util_plot_figure_no_plotly
  }

  fkt <- util_map_by_largest_prefix(
    nm,
    haystack = util_all_ind_functions()
  )
  # Use browser() locally to inspect acc_loess_observer rendering.
  if (!inherits(dqr, "dataquieR_NULL")) { # if the function did not return NULL or trigger an error
    # is there a filter on the implementations sheet?
    outputs <- util_get_concept_info("implementations", get("function_R")
      == fkt, "Reportoutputs")[["Reportoutputs"]]
    if (length(outputs) == 1 && !util_empty(outputs)) {
      outputs <- names(util_parse_assignments(outputs)) ###
      if (!is_ssi) {
        if (is_single_var) {
          outputs <- grep("(I)",
            outputs,
            value = TRUE,
            invert = TRUE,
            fixed = TRUE
          )
        } else {
          outputs <- grep("(V)",
            outputs,
            value = TRUE,
            invert = TRUE,
            fixed = TRUE
          )
        }
        outputs <- gsub("(V)", "", outputs, fixed = TRUE)
        outputs <- gsub("(I)", "", outputs, fixed = TRUE)
        remaining_slots <- intersect(outputs, names(dqr))
        if (length(remaining_slots) == 0) {
          util_message(
            "Removed all outputs of %s according to %s",
            dQuote(fkt),
            sQuote("concept | implementations | Reportoutputs"),
            level = 10
          )
        }
        for (.slot in setdiff(names(dqr), remaining_slots)) {
          dqr[[.slot]] <- NULL
        }
        # dqr <- dqr[remaining_slots] loosses attrbiutes
      }
    }

    slot <- # stores the name of the result object returned by the indicator function being displayed # nolint: line_length_linter.
      head(intersect( # check if the preferred slots are in the result names
        preferred_slots, names(dqr)
      ), 1) # take the first available result name according to the preferred slots # nolint: line_length_linter.

    if (fkt %in% c(
      "con_limit_deviations", # check if we are working with a limits function
      "con_hard_limits",
      "con_soft_limits",
      "con_detection_limits"
    )) {
      if ("SummaryPlotList" %in% names(dqr)) { # add an exception, the histogram is preferred to the table (heat map)
        slot <- "SummaryPlotList"
      }
    }
    if (length(slot) > 0) { # if there is an output, check and collect all warnings/errors/messages # nolint: line_length_linter.
      errors <- util_attr(dqr, "error", exact = TRUE) # get and store the errors from the output
      warnings <- util_attr(dqr, "warning", exact = TRUE)
      messages <- util_attr(dqr, "message", exact = TRUE)

      errors <- errors[!vapply( # do not show errors about calls, that would not be possible at all (e.g., loess for only categorical scales) # nolint: line_length_linter.
        lapply(errors, util_attr, "intrinsic_applicability_problem", exact = TRUE),
        identical,
        TRUE,
        FUN.VALUE = logical(1)
      )]

      warnings <- warnings[!vapply( # do not show errors about calls, that would not be possible at all (e.g., loess for only categorical scales) # nolint: line_length_linter.
        lapply(warnings, util_attr, "intrinsic_applicability_problem", exact = TRUE),
        identical,
        TRUE,
        FUN.VALUE = logical(1)
      )]

      messages <- messages[!vapply( # do not show errors about calls, that would not be possible at all (e.g., loess for only categorical scales) # nolint: line_length_linter.
        lapply(messages, util_attr, "intrinsic_applicability_problem", exact = TRUE),
        identical,
        TRUE,
        FUN.VALUE = logical(1)
      )]

      # extract the condition messages, and ensure that multi-line messages are
      # in a single line
      errors <- vapply(lapply(errors, conditionMessage), paste,
        FUN.VALUE = character(1)
      )
      warnings <- vapply(lapply(warnings, conditionMessage), paste,
        FUN.VALUE = character(1)
      )
      messages <- vapply(lapply(messages, conditionMessage), paste,
        FUN.VALUE = character(1)
      )
      x <- dqr[[slot]] # extract the corresponding result according to slot

      x <- util_remove_dataquieR_result_class(x)

      if (endsWith(slot, "PlotList")) {
        if (length(x) > 1) { # check and warn if there is more than one entry in PlotList # nolint: line_length_linter.
          x <- mapply(
            SIMPLIFY = FALSE, pl_i = x, pl_nm = names(x),
            function(pl_i, pl_nm) {
              if (is.null(popup_nm) &&
                  all(grepl(".", nm, fixed = TRUE))) {
                .nm <- util_sub_string_left_from_.(nm)
              } else {
                .nm <- nm
              }
              as_py_i <- util_attr(dqr, "as_plotly", exact = TRUE)
              util_pretty_print(
                util_attach_attr(
                  list(SummaryPlot = pl_i),
                  as_plotly = as_py_i
                ),
                nm =
                  paste0(.nm, ".", pl_nm),
                # paste0("", head(names(pl_nm), 1)), # avoid the same name,
                # iframe FIG_.html won't work, if > 1
                is_single_var = is_single_var,
                meta_data = meta_data,
                meta_data_cross_item = meta_data_cross_item,
                label_col = label_col,
                use_plot_ly = use_plot_ly,
                dir = dir,
                link_variables = link_variables,
                popup_nm = popup_nm,
                ...
              )
            }
          )
          x <- lapply(x, function(p) {
            htmltools::span(style = "float:left;", p)
          })
          x <- htmltools::tagList(c(
            x,
            list(htmltools::br(
              style =
                "clear: both;"
            ))
          ))
        } else {
          if (length(x) == 1) {
            x <- x[[1]] # take the single plot from PLotList
          } else {
            x <- NULL
          }
        }
      }
      # check the class of x
      if (inherits(x, "ReportSummaryTable")) {
        cats <- setdiff(colnames(x), c("Variables", "N"))
        ncats <- length(cats)
        vars <- unique(x$Variables)
        nvars <- length(vars)
        if (ncats == 0 && nvars == 0) {
          x <- NULL
          slot <- "Plot" # fake, that we have a plot
        } else if (ncats == 0 || nvars == 0) {
          x <- NULL
          slot <- "Plot" # fake, that we have a plot
        } else if (ncats == 1 && nvars == 1) {
          val <- x[x$Variables == vars, cats, drop = TRUE]
          level_names <- util_report_summary_table_level_names(x)
          if (!!length(level_names)) {
            val <- level_names[as.character(val)]
            x <- htmltools::p(
              sprintf(
                "%s for %s is",
                sQuote(cats),
                dQuote(vars)
              ),
              htmltools::strong(htmltools::em(val))
            )
          } else if ("N" %in% colnames(x)) {
            x <- htmltools::p(
              sprintf(
                "%s for %s is",
                sQuote(cats),
                dQuote(vars)
              ),
              htmltools::strong(htmltools::em(val)),
              "(",
              scales::percent(val / x$N),
              ") out of",
              x$N
            )
          } else {
            x <- htmltools::p(
              sprintf(
                "%s for %s is",
                sQuote(cats),
                dQuote(vars)
              ),
              htmltools::strong(htmltools::em(val))
            )
          }
        } else {
          x <- util_new_report_summary_table(x,
            meta_data = meta_data,
            label_col = label_col
          )
          x <- print.ReportSummaryTable(x, view = FALSE, ...) # convert to ggplot # nolint: line_length_linter.
        }
      }
      x <- dq_lazy_unwrap(x)
      if (inherits(x, "dq_lazy_ggplot")) x <- prep_realize_ggplot(x)
      if (inherits(x, "ggplot") || inherits(x, "ggmatrix") ||
          inherits(x, "util_pairs_ggplot_panels") ||
          inherits(x, "svg_plot_proxy") ||
          "PlotlyPlot" %in% names(dqr) ||
          inherits(x, "ggplot_built")) {
        if (inherits(x, "ggplot") || inherits(x, "ggmatrix") ||
            inherits(x, "util_pairs_ggplot_panels") ||
            inherits(x, "svg_plot_proxy") ||
            inherits(x, "ggplot_built")) {
          ggthumb <- x
        } else {
          ggthumb <- NULL
        }
        x_is_plot <- TRUE

        # ensure, sizing hint sticks at the dqr, only
        list2env(util_fix_sizing_hints(dqr = dqr, x = x), envir = environment())

        if ("PlotlyPlot" %in% names(dqr)) {
          as_plotly <- "util_as_plotly_from_res"
        } else {
          as_plotly <- util_attr(dqr, "as_plotly", exact = TRUE)
        }
        if (!is.null(as_plotly) && exists(as_plotly, mode = "function")) {
          as_plotly <- get(as_plotly, mode = "function")
        }
        if (!inherits(x, "ggplot_built") &&
            !util_is_svg_object(x) &&
            use_plot_ly && is.function(as_plotly)) {
          withCallingHandlers(
            {
              x <- as_plotly(dqr) # , height = 480, width = 1040)
              if (inherits(x, "plotly")) {
                if (!identical(
                  util_attr(dqr, "dont_util_adjust_geom_text_for_plotly",
                    exact = TRUE
                  ),
                  TRUE
                )) {
                  x <- util_adjust_geom_text_for_plotly(x)
                }
                x <- util_decorate_plotly(x)
              }
            },
            warning = function(cond) { # suppress a waning caused by ggplotly for barplots # nolint: line_length_linter.
              if (startsWith(
                conditionMessage(cond),
                "'bar' objects don't have these attributes: 'mode'"
              ) ||
                startsWith(
                  conditionMessage(cond),
                  "'box' objects don't have these attributes: 'mode'"
                )) {
                invokeRestart("muffleWarning")
              }
              if (any(grepl("the mode", conditionMessage(cond)))) {
                invokeRestart("muffleWarning")
              }
            },
            message = function(cond) { # suppress a waning caused by ggplotly for barplots # nolint: line_length_linter.
              if (startsWith(
                conditionMessage(cond),
                "'bar' objects don't have these attributes: 'mode'"
              ) ||
                startsWith(
                  conditionMessage(cond),
                  "'box' objects don't have these attributes: 'mode'"
                )) {
                invokeRestart("muffleMessage")
              }
              if (any(grepl("the mode", conditionMessage(cond)))) {
                invokeRestart("muffleMessage")
              }
            }
          )
        } else {
          x <- plot_figure(x, sizing_hints = util_attr(dqr, "sizing_hints",
              exact = TRUE
            ))
        }
        # convert to plotly or base 64 plot image

        # to iframe?
        x <- util_iframe_it_if_needed(x,
          dir = dir, nm = nm, fkt = fkt,
          sizing_hints = util_attr(dqr, "sizing_hints",
            exact = TRUE
          ),
          ggthumb = ggthumb
        )
        # NOTE: If we have two figures in the same result, nm is not unique,
        # because the two figures may be displayed, both., but we have
        # currently only on figure per result, the other one can olny be a
        # table or stuff (see this function, abobve)
      } else {
        x_is_plot <- grepl("Plot", slot)
      }
      if (is.data.frame(x)) {
        # check if dataframe is empty
        if (!prod(dim(x))) {
          x <- NULL
        } else {
          rownames(x) <- NULL
          x <- util_apply_entity_grading_for_render(x)
          entity_grading_args <- util_attr(
            x,
            "entity_grading_args",
            exact = TRUE
          )
          x <- util_link_result_references(x)
          x <- util_html_table(x,
            hideCols = util_attr(x, "hideCols", exact = TRUE),
            meta_data = meta_data,
            meta_data_cross_item = meta_data_cross_item,
            label_col = label_col,
            dl_fn = nm,
            rotate_for_one_row = rotate_for_one_row,
            link_variables = link_variables,
            df_escape = TRUE,
            additional_init_args = entity_grading_args,
            ssi_link_target = ssi_link_target
          )
        }
      }
      if (length(x) > 0) {
        if (!inherits(x, "htmlwidget") && # check if output is compatible with htmltools
            !inherits(x, "shiny.tag") &&
            !inherits(x, "shiny.tag.list")) {
          x <- htmltools::p(
            sprintf(
              paste(
                "Cannot display objects of class(es) %s, yet.",
                "Please file a feature request."
              ),
              paste(dQuote(class(x)), collapse = ", ")
            )
          )
        }
      } else {
        x <- NULL
      }
    } else {
      x <- NULL
    }
  } else {
    x <- NULL
  }

  if (!is.null(x) || (exists("x_is_plot") && x_is_plot)) { # create and add tags and links
    if (all(grepl(".", nm, fixed = TRUE))) { # get the full name, which includes a dot
      anchor <- util_generate_anchor_tag(
        name = nm,
        order_context =
          ifelse(
            is_single_var,
            "variable",
            "indicator"
          )
      )
      # the link is most easily created here, but therefore in the wrong
      # position, so later it must be moved
      link <- if (has_group_label) {
        util_generate_anchor_link(
          name = nm,
          order_context = ifelse(is_single_var, "variable", "indicator"),
          title = group_label
        )
      } else {
        util_generate_anchor_link(
          name = nm,
          order_context = ifelse(is_single_var, "variable", "indicator")
        )
      }
      if (has_group_id) {
        link <- NULL
      }
    } else {
      anchor <- NULL
      link <- NULL
    }

    # add variable name to pages that are not single_vars and display multiple
    # variable in same page
    if (!is_single_var && all(grepl(".", nm, fixed = TRUE))) {
      href <- htmltools::tagGetAttribute(util_generate_anchor_link(
        name = nm,
        order_context =
          ifelse(
            is_single_var,
            "indicator", # link to the other world
            "variable"
          )
      ), attr = "href")
      if (!sub("^[^\\.]*\\.", "", nm) %in% meta_data[[label_col]]) {
        href <- NULL
      }
      var_label <- if (has_group_label) {
        group_label
      } else {
        sub("^[^\\.]*\\.", "", nm)
      }
      if (has_group_id) {
        group_href <- util_cross_item_hrefs(group_id, meta_data_cross_item)
        if (!is.na(group_href)) {
          href <- group_href
        }
      }
      caption <- var_label
      if (caption != "[ALL]") {
        variable_name_context <- TRUE
        caption <- htmltools::h5(htmltools::tags$a(href = href, var_label))
        if (!has_group_label && label_col != VAR_NAMES) {
          if (label_col != LONG_LABEL && LONG_LABEL %in% colnames(meta_data)) {
            long_label <- util_map_labels(
              var_label,
              meta_data,
              LONG_LABEL,
              label_col,
              ifnotfound = ""
            )
          } else {
            long_label <- ""
          }
          title <- util_map_labels(
            var_label,
            meta_data,
            VAR_NAMES,
            label_col,
            ifnotfound = NA_character_
          )
          if (length(title) == 1 &&
              is.character(title) &&
              !is.na(title)) {
            caption <- htmltools::h5(
              title = title,
              htmltools::tags$a(href = href, var_label),
              htmltools::br(),
              htmltools::tags$small(htmltools::tags$em(title)),
              htmltools::tags$em(long_label)
            )
          }
        }
      } else {
        caption <- NULL
      }
    } else {
      caption <- NULL
    }
    slot2 <- # stores the name of the result object returned by the indicator function being displayed # nolint: line_length_linter.
      head(intersect( # check if the preferred slots are in the result names
        setdiff(
          preferred_summary_slots,
          slot
        ), names(dqr)
      ), 1) # take the first available result name according to the preferred slots # nolint: line_length_linter.
    if (length(slot2) < 1) {
      slot2 <- NULL
    } else {
      slot2 <- slot2[[1]]
    }
    if (length(slot2)) {
      y <- dqr[[slot2]]
      if (!x_is_plot) {
        y <- NULL
      }
      if (is.data.frame(y)) {
        # check if dataframe is empty
        if (!prod(dim(y))) {
          y <- NULL
        } else {
          if (endsWith(slot2, "Table")) {
            y <- util_make_data_slot_from_table_slot(y)
          }
          rownames(y) <- NULL
          if (nrow(y) > 1 && (variable_name_context || is_single_var)) {
            if ("Variables" %in% colnames(y) && length(unique(y$Variables)) == 1) {
              y$Variables <- NULL
            }
          }
          y <- util_apply_entity_grading_for_render(y)
          entity_grading_args <- util_attr(
            y,
            "entity_grading_args",
            exact = TRUE
          )
          y <- util_link_result_references(y)
          y <- util_html_table(y,
            hideCols = util_attr(y, "hideCols", exact = TRUE),
            meta_data = meta_data,
            meta_data_cross_item = meta_data_cross_item,
            label_col = label_col,
            dl_fn = nm,
            rotate_for_one_row = rotate_for_one_row,
            link_variables = link_variables,
            df_escape = TRUE,
            additional_init_args = entity_grading_args,
            ssi_link_target = ssi_link_target
          )
        }
      }
      if (length(y) > 0) {
        if (!inherits(y, "htmlwidget") && # check if output is compatible with htmltools
            !inherits(y, "html") &&
            !inherits(y, "shiny.tag") &&
            !inherits(y, "shiny.tag.list")) {
          y <- htmltools::p(
            sprintf(
              paste(
                "Cannot display objects of class(es) %s, yet.",
                "Please file a feature request."
              ),
              paste(dQuote(class(y)), collapse = ", ")
            )
          )
        }
      } else {
        y <- NULL
      }
    } else {
      y <- NULL
    }
    if (is_ssi) {
      section_id <- util_attr(dqr, "dq_result_anchor", exact = TRUE)
      if (length(section_id) == 1 &&
          is.character(section_id) &&
          !is.na(section_id) &&
          nzchar(section_id)) {
        section_id <- htmltools::urlEncodePath(section_id)
        section_title <- util_attr(dqr, "dq_result_title", exact = TRUE)
        anchor <- htmltools::a(id = section_id)
        link <- htmltools::a(
          href = paste0("#", section_id),
          section_title
        )
      } else {
        anchor <- NULL
        link <- NULL
      }
    }

    ## NEW: decide how to wrap x and y into dataquieR_result containers
    layout <- util_get_result_layout(dqr)
    result_group_heading <- util_result_group_heading(dqr)

    if (!is.null(x) && !is.null(y) &&
        exists("x_is_plot") && x_is_plot &&
        identical(layout, "2-columns-fig-left")) {
      combined_inner <- htmltools::div(
        class = "dq-plot-table-grid",
        htmltools::div(class = "dq-plot-cell", x),
        htmltools::div(class = "dq-table-cell", y)
      )

      x <- util_wrap_dqr_result(
        combined_inner,
        nm = nm,
        popup_nm = popup_nm,
        dqr = dqr,
        errors = errors,
        warnings = warnings,
        messages = messages,
        extra_classes = "dq-plot-table-result"
      )
      y <- NULL
    } else {
      if (!is.null(x)) {
        x <- util_wrap_dqr_result(
          x,
          nm = nm,
          popup_nm = popup_nm,
          dqr = dqr,
          errors = errors,
          warnings = warnings,
          messages = messages
        )
      }
      if (!is.null(y)) {
        y <- util_wrap_dqr_result(
          y,
          nm = nm,
          popup_nm = popup_nm,
          dqr = dqr,
          errors = errors,
          warnings = warnings,
          messages = messages
        )
      }
    }

    if (!isTRUE(dynGet("old_called_in_pipeline",
          ifnotfound = .called_in_pipeline
        )) &&
        "FlaggedStudyData" %in% names(dqr)) {
      z <- util_html_table(
        dqr[["FlaggedStudyData"]],
        meta_data = meta_data,
        label_col = label_col,
        dl_fn = nm,
        rotate_for_one_row = TRUE,
        df_escape = TRUE
      )
      x <- htmltools::tagList(
        anchor = anchor,
        link = link,
        result_group_heading,
        caption,
        x,
        y,
        z
      )
    } else {
      x <- htmltools::tagList(
        anchor = anchor,
        link = link,
        result_group_heading,
        caption,
        x,
        y
      )
    }
    # the link is most easily added here, but still in the wrong position, so
    # later it must be moved
  }
  x
}

#' Internal helper: plotly add modebar buttons
#'
#' @noRd
util_plotly_add_modebar_buttons <- function(py, buttons) {
  if (is.null(py$x$config)) py$x$config <- list()
  if (is.null(py$x$config$modeBarButtonsToAdd)) py$x$config$modeBarButtonsToAdd <- list() # nolint: line_length_linter.

  py$x$config$modeBarButtonsToAdd <- c(py$x$config$modeBarButtonsToAdd, buttons)
  py
}

#' Adds layout and buttons to a `plotly`
#'
#' meant to work inside a report
#'
#' @param x a `plotly` object
#'
#' @returns decorated `htmltools` object
#' @noRd
util_decorate_plotly <- function(x) {
  if (!.called_in_pipeline) {
    return(x)
  }

  x <- plotly::layout(x,
    autosize = !FALSE,
    margin = list(
      autoexpand = !FALSE,
      r = 200,
      b = 100
    )
  )
  x <- plotly::config(x,
    responsive = TRUE,
    autosizable = TRUE,
    fillFrame = TRUE,
    displaylogo = FALSE,
    modeBarButtonsToRemove = list("toImage")
  )
  x <- util_plotly_add_modebar_buttons(
    x,
    list(
      list(
        name = "Save as image",
        icon = htmlwidgets::JS("Plotly.Icons.camera"),
        click = htmlwidgets::JS(paste(
          sep = "\n",
          "function(gd) {",
          "  plotlyDL(gd)",
          "}"
        ))
      ),
      list(
        name = "Restore initial Size",
        icon =
          htmlwidgets::JS("PlotlyIconshomeRED()"),
        click =
          htmlwidgets::JS(
            paste(
              sep = "\n",
              "function() {",
              "   if (togglePlotlyWindowZoom instanceof Function) {",
              "     togglePlotlyWindowZoom()",
              "   }",
              "}"
            )
          )
      ),
      list(
        name = "Print plot",
        icon = htmlwidgets::JS("PlotlyIconsprinter()"),
        click = htmlwidgets::JS(paste(
          sep = "\n",
          "function(gd) {",
          "  plotlyPrint(gd)",
          "}"
        ))
      ),
      list(
        name = "Download as PDF",
        icon = htmlwidgets::JS("PlotlyIconspdf()"),
        click = htmlwidgets::JS(paste(
          sep = "\n",
          "function(gd) {",
          "  downloadPlotlyAsPDF(gd, {orientation: 'landscape', dpi: 300});",
          "}"
        ))
      )
    )
  )
  util_ensure_suggested("htmlwidgets",
    goal = "Creating the Print Dialog for Plotly"
  )
  x <- htmlwidgets::onRender(x, "
                          function(el, x) {
                            if (typeof injectPlotlyPrintDialog === 'function') {
                              injectPlotlyPrintDialog();
                            }
                          }
                        ")
  x
}

# preferred order of the content of the report
preferred_slots <- c(
  "ReportSummaryTable",
  "PlotlyPlot",
  "SummaryPlot", "SummaryPlotList", "SummaryData",
  "OtherData", "ResultData",
  "SummaryTable",
  "DataframeData", "DataframeTable", "SegmentData",
  "SegmentTable", "OtherTable",
  "VariableGroupPlot", "VariableGroupPlotList",
  "VariableGroupData", "VariableGroupTable"
)


# preferred order of the content of the report
preferred_summary_slots <- c(
  "SummaryData",
  "OtherData", "ResultData",
  "SummaryTable",
  "DataframeData", "DataframeTable", "SegmentData",
  "SegmentTable", "OtherTable",
  "VariableGroupData", "VariableGroupTable"
)
