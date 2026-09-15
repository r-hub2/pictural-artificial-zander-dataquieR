# nolint start: line_length_linter.
#' Compute Pairwise Correlations
#'
#' works on variable groups (`cross-item_level`), which are expected to show
#' a Pearson correlation
#'
#' [Descriptor]
#'
#' @inheritParams .template_function_indicator
#'
#' @return a `list` with the slots:
#'   - `SummaryPlotList`: for each variable group a [ggplot2::ggplot] object with
#'                        pairwise correlation plots
#'   - `SummaryData`: table with columns `VARIABLE_LIST`, `cors`,
#'                    `max_cor`, `min_cor`
#'   - `SummaryTable`: like `SummaryData`, but machine readable and with
#'                     stable column names.
#' @export
#'
#' @importFrom stats cor
#'
#' @examples
#' \dontrun{
#' devtools::load_all()
#' prep_load_workbook_like_file("meta_data_v2")
#' des_scatterplot_matrix("study_data")
#' }
# nolint end
des_scatterplot_matrix <- function(label_col,
  study_data,
  item_level = "item_level",
  meta_data_cross_item = "cross-item_level",
  meta_data = item_level,
  meta_data_v2,
  cross_item_level,
  `cross-item_level`) {
  util_maybe_load_meta_data_v2()
  util_ck_arg_aliases()
  prep_prepare_dataframes(.apply_factor_metadata = TRUE)

  meta_data_cross_item <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item,
    label_col = label_col
  )
  VariableGroupPlotList <- lapply(which(
    useNames = TRUE,
    trimws(tolower(setNames(meta_data_cross_item$ASSOCIATION_METRIC,
          nm = meta_data_cross_item$CHECK_LABEL
        ))) %in%
      c("pearson", "spearman")
  ), function(vg) {
    rvs <-
      vapply(
        util_parse_assignments(meta_data_cross_item[vg, VARIABLE_LIST, drop = TRUE]), # nolint: line_length_linter.
        identity,
        FUN.VALUE = character(1)
      )
    rvs <- rvs[vapply(rvs, FUN = function(x) {
      !all(is.na(ds1[[x]]))
    }, FUN.VALUE = logical(1))]
    if (length(rvs) == 0) {
      r_py <- htmltools::div(
        htmltools::h3("No data available to create the plot")
      )
      r <- util_create_lean_ggplot(ggplot() +
        annotate("text",
          x = 0, y = 0,
          label = "No data available to create the plot"
        ) +
        theme(
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank()
        ))
      number_of_vars <- 0
      attr(r, "plotly") <- r_py
    } else {
      columnLabels <- prep_get_labels(rvs,
        item_level = meta_data,
        label_col = label_col,
        resp_vars_match_label_col_only = TRUE,
        label_class = "SHORT"
      )
      correlation_method <-
        trimws(tolower(meta_data_cross_item[vg, ASSOCIATION_METRIC, drop = TRUE])) # nolint: line_length_linter.
      .dt0 <- ds1[, rvs, drop = FALSE]
      cc <- complete.cases(.dt0)
      if (any(!cc, na.rm = TRUE)) {
        util_warning(
          "Found %d incomplete cases in %s. Discarding such rows for correlations.", # nolint: line_length_linter.
          sum(!cc), util_pretty_vector_string(columnLabels)
        )
      }
      .dt0 <- .dt0[cc, , drop = FALSE]
      r <- util_create_lean_ggplot(
        util_pairs_ggplot(.dt0,
          columnLabels = columnLabels,
          correlation_method = correlation_method
        ),
        .dt0 = .dt0,
        columnLabels = columnLabels,
        correlation_method = correlation_method
      )
      r_py <- util_pairs_plotly(.dt0,
        columnLabels = columnLabels,
        correlation_method = correlation_method
      )
      attr(r, "plotly") <- r_py
      number_of_vars <- length(rvs)
    }


    # Sizing information - a default
    attr(r, "sizing_hints") <- list(
      figure_type_id = "pairs_plot",
      number_of_vars = number_of_vars
    )
    return(r)
  })

  names(VariableGroupPlotList) <-
    as.character(meta_data_cross_item[
      trimws(tolower(setNames(meta_data_cross_item$ASSOCIATION_METRIC,
            nm = meta_data_cross_item$CHECK_LABEL
          ))) %in%
        c("pearson", "spearman"), CHECK_LABEL
      , drop = TRUE])
  VariableGroupTable <-
    util_rbind(data_frames_list = lapply(which(
      useNames = TRUE,
      trimws(tolower(setNames(meta_data_cross_item$ASSOCIATION_METRIC,
            nm = meta_data_cross_item$CHECK_LABEL
          ))) %in%
        c("pearson", "spearman")
    ), function(vg) {
      vl <- meta_data_cross_item[vg, VARIABLE_LIST, drop = TRUE]

      correlation_method <-
        trimws(tolower(meta_data_cross_item[vg, ASSOCIATION_METRIC, drop = TRUE])) # nolint: line_length_linter.

      cor_range <-
        util_parse_interval(
          trimws(tolower(meta_data_cross_item[vg, ASSOCIATION_RANGE, drop = TRUE])) # nolint: line_length_linter.
        )

      rvs <-
        vapply(
          util_parse_assignments(vl),
          identity,
          FUN.VALUE = character(1)
        )

      rvs <- rvs[vapply(rvs, FUN = function(x) {
        !all(is.na(ds1[[x]]))
      }, FUN.VALUE = logical(1))]
      if (length(rvs) == 0) {
        cors <- matrix(ncol = 0, nrow = 0)
      } else {
        cors <- cor(ds1[, rvs, drop = FALSE],
          use = "complete.obs",
          method = correlation_method
        )
      }

      cors_combs <-
        t(apply(expand.grid(rvs, rvs), 1, sort))

      cors_combs <-
        cors_combs[!duplicated(cors_combs), , drop = FALSE]

      cors_combs <-
        cors_combs[
          apply(cors_combs, 1, function(r) length(unique(r)) > 1), ,
          drop = FALSE
        ]

      if (!!prod(dim(cors_combs))) {
        cors_of_combs <- setNames(apply(cors_combs, 1, function(r) {
          cors[r[[1]], r[[2]], drop = TRUE]
        }), nm = apply(cors_combs, 1, function(r) {
          sprintf("cor(%s, %s)", r[[1]], r[[2]])
        }))
      } else {
        cors_of_combs <- setNames(numeric(0), nm = character(0))
      }

      data.frame(
        VARIABLE_LIST = vl,
        CHECK_ID = meta_data_cross_item[vg, CHECK_ID, drop = TRUE],
        CHECK_LABEL = meta_data_cross_item[vg, CHECK_LABEL, drop = TRUE],
        pretty_cors = prep_deparse_assignments(
          labels = format(util_round_to_decimal_places(cors_of_combs)),
          codes = names(cors_of_combs),
          mode = "string_codes"
        ),
        pretty_max_cor = format(max(cors_of_combs, na.rm = TRUE)),
        cors = prep_deparse_assignments(
          labels = cors_of_combs,
          codes = names(cors_of_combs),
          mode = "string_codes"
        ),
        max_cor = max(cors_of_combs, na.rm = TRUE),
        pretty_in_range = redcap_env$`in`(
          max(cors_of_combs, na.rm = TRUE),
          cor_range
        ),
        pretty_range = as.character(cor_range),
        in_range = redcap_env$`in`(
          max(cors_of_combs, na.rm = TRUE),
          cor_range
        )
      )
      # Historical correlation debug note removed here.
    }))
  pretty_columns <- grep(
    "^pretty_.*$",
    names(VariableGroupTable),
    value = TRUE
  )
  VariableGroupData <- VariableGroupTable[
    , c(VARIABLE_LIST, CHECK_ID, CHECK_LABEL, pretty_columns),
    drop = FALSE
  ]
  colnames(VariableGroupData) <- gsub("^pretty_", "", colnames(VariableGroupData)) # nolint: line_length_linter.
  util_attach_attr(list(
    VariableGroupPlotList = VariableGroupPlotList,
    VariableGroupTable = VariableGroupTable[, !startsWith(colnames(VariableGroupTable), "pretty_"), drop = FALSE], # nolint: line_length_linter.
    VariableGroupData = VariableGroupData
  ), "as_plotly" = "util_as_plotly_des_scatterplot_matrix")
}

#' Internal helper: as plotly des scatterplot matrix
#'
#' @noRd
util_as_plotly_des_scatterplot_matrix <- function(dqr) {
  util_ensure_suggested("plotly")
  if ("SummaryPlot" %in% names(dqr)) {
    return(util_attr(dqr$SummaryPlot, "plotly", exact = TRUE))
  }
  if (!identical(length(dqr$VariableGroupPlotList), 1L)) {
    print("FIXME")
    plotly::plot_ly() %>%
      plotly::add_text(
        x = 0.5, y = 0.5,
        text = "> 1 group in scatter plot matrix is not yet supported",
        textfont = list(size = 24, color = "red"),
        showlegend = FALSE
      ) %>%
      plotly::layout(
        xaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE), # nolint: line_length_linter.
        yaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE), # nolint: line_length_linter.
        margin = list(l = 0, r = 0, t = 0, b = 0)
      )
  } else {
    util_attr(dqr$VariableGroupPlotList[[1]], "plotly", exact = TRUE)
  }
}
