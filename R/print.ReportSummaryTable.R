#' print implementation for the class `ReportSummaryTable`
#'
#' @description
#' Use this function to print results objects of the class
#' `ReportSummaryTable`.
#'
#' @param relative deprecated
#' @param dt [logical] use the selected DT2 or DT table backend, if installed
#' @param fillContainer [logical] if `dt` is `TRUE`, control table size,
#'        filling the available widget container when enabled.
#' @param displayValues [logical] if `dt` is `TRUE`, also display the actual
#'                                values
#' @param view [logical] if `view` is `FALSE`, do not print but return the
#'                       output, only
#' @param drop [logical] if `drop` is `FALSE`, keep unused levels,
#'                       see [dataquieR.droplevels_ReportSummaryTable]
#' @param x `ReportSummaryTable` objects to print
#' @inheritParams acc_distributions
#' @param ... not used, yet
#'
#' @seealso base::print
#' @importFrom grDevices colorRamp rgb col2rgb
#' @importFrom ggplot2 expansion waiver scale_color_gradientn
#' @export
#' @return the printed object
print.ReportSummaryTable <- function(
  x, relative = lifecycle::deprecated(),
  dt = FALSE,
  fillContainer = FALSE,
  displayValues = FALSE,
  view = TRUE,
  drop =
    getOption(
      "dataquieR.droplevels_ReportSummaryTable",
      dataquieR.droplevels_ReportSummaryTable_default
    ),
  ...,
  flip_mode = "auto"
) {
  relative_arg_is_present <- lifecycle::is_present(relative)
  if (relative_arg_is_present) {
    explicit_relative <- relative
  }

  if (relative_arg_is_present) {
    # Signal the deprecation to the user
    lifecycle::deprecate_warn(
      "2.5.0",
      "dataquieR::print.ReportSummaryTable(relative = )"
    )
  }

  util_expect_scalar(drop,
    check_type = is.logical,
    error_message = "drop needs to be either TRUE or FALSE"
  )

  if (drop) {
    x <- droplevels(x)
  }

  # definition of colscale
  colscale <- c("#B2182B", "#92C5DE", "#2166AC")


  empty <-
    (!length(setdiff(colnames(x), c("Variables", "N")))) ||
    (!c("Variables") %in% colnames(x)) ||
    (!c("N") %in% colnames(x))
  if (empty) {
    if (!dt) {
      x <- ggplot() +
        annotate("text", x = 0, y = 0, label = "Empty result.") +
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
        )
      x <- util_set_size(x)
    } else {
      x <- htmltools::HTML("")
    }
    attr(x, "warning") <- "Empty result"
    class(x) <- union("dataquieR_result", class(x))
    if (view) {
      print(x)
    }
    attr(x, "from_ReportSummaryTable") <- TRUE
    return(invisible(x))
  }

  mfm <- missing(flip_mode)
  if (!mfm) oldfm <- flip_mode

  x <- util_apply_report_summary_table_grading(x)
  list2env(util_init_respum_tab(x), envir = environment())

  if (relative_arg_is_present) {
    relative <- explicit_relative
  }

  if (!mfm) flip_mode <- oldfm

  x <- util_new_report_summary_table(x)

  if (!dt && !is.null(util_attr(
    x,
    "segment_missingness_bar",
    exact = TRUE
  ))) {
    p <- util_plot_segment_missingness_report_summary_table(x)
    if (view) print(p)
    return(invisible(p))
  }

  hm <- x
  if (relative) {
    hm <- cbind.data.frame(
      Variables = hm$Variables,
      hm[, setdiff(colnames(hm), c("Variables", "N")),
        drop = FALSE
      ] /
        hm$N
    )
  } else {
    hm <- cbind.data.frame(
      Variables = hm$Variables,
      hm[, setdiff(colnames(hm), c("Variables", "N")),
        drop = FALSE
      ]
    )
  }

  # Historical ggplot sketch removed here. Inspect with
  # `git show b8f27bca62 -- R/print.ReportSummaryTable.R`.


  if (prod(dim(hm)) == 0) {
    if (dt) {
      html_table_backend <- util_html_table_backend(
        goal = "the option dt = TRUE"
      )
      use_dt2 <- identical(html_table_backend, "DT2")
      if (use_dt2) {
        w <- util_html_table_dt2_widget(data.frame())
      } else {
        w <- DT::datatable(data.frame())
      }
      if (view) print(w)
      attr(w, "from_ReportSummaryTable") <- TRUE
      return(invisible(x))
    } else {
      p <- ggplot()
      if (view) print(p)
      attr(p, "from_ReportSummaryTable") <- TRUE
      return(invisible(x))
    }
  }

  # `stats::reshape()` replaced the former `reshape::melt()` dependency.
  tb <- stats::reshape(
    data = hm, idvar = "Variables",
    varying = colnames(hm)[2:ncol(hm)],
    v.names = "value",
    times = colnames(hm)[2:ncol(hm)],
    direction = "long"
  )
  rownames(tb) <- NULL
  names(tb) <- c("Variables", "variable", "value")

  if (all(is.na(tb$value))) {
    tb <- tb[FALSE, , drop = FALSE]
  } else {
    tb <- tb[!is.na(tb$value), , drop = FALSE]
  }

  levs <- unique(tb$variable)

  if (length(levs[grep("^int_", levs)]) == 0 &&
      length(levs[grep("^com_", levs)]) == 0 &&
      length(levs[grep("^con_", levs)]) == 0 &&
      length(levs[grep("^acc_", levs)]) == 0) {
    tb$variable <- factor(tb$variable,
      levels = levs
    )
  } else {
    # Historical alphabetic sorting of levels removed here.
    levs_int <- levs[grep("^int_", levs)]
    levs_com <- levs[grep("^com_", levs)]
    levs_con <- levs[grep("^con_", levs)]
    levs_acc <- levs[grep("^acc_", levs)]

    levs_remaining <- setdiff(
      levs,
      Reduce(union, c(levs_int, levs_com, levs_con, levs_acc))
    )

    # order the factor levels
    tb$variable <- factor(tb$variable,
      levels = c(
        levs_int, levs_com,
        levs_con, levs_acc,
        levs_remaining
      )
    )
  }


  if (dt) {
    # https://stackoverflow.com/a/50406895
    html_table_backend <- util_html_table_backend(
      goal = "the option dt = TRUE"
    )
    use_dt2 <- identical(html_table_backend, "DT2")

    # if (!relative) {
    #   hm[, setdiff(colnames(hm), c("Variables", "N"))] <-
    #     hm[, setdiff(colnames(hm), c("Variables", "N")),
    #            drop = FALSE] / max(as.matrix(
    #              hm[, setdiff(colnames(hm), c("Variables", "N")),
    #                 drop = FALSE]
    #            ), na.rm = TRUE)
    # }
    mx <- max(as.matrix(
      hm[, setdiff(colnames(hm), c("Variables", "N")),
        drop = FALSE
      ]
    ), na.rm = TRUE)
    if (!is.finite(mx) || mx <= 0) {
      mx <- 1
    }
    colr <- colorRamp(colors = rev(colscale))
    colors_of_hm <- lapply(hm, function(values) {
      if (!all(is.numeric(values))) {
        return(values)
      }
      if (any(is.na(values))) {
        return(values)
      }
      if (continuous) {
        if (!relative) {
          v <- colr(values / mx)
        } else {
          v <- colr(values)
        }
      } else {
        if (length(level_names) > 0) {
          cc <- setNames(colcode, nm = level_names)
          v <- t(col2rgb(cc[level_names[as.character(values)]], alpha = TRUE))
        } else {
          v <- t(col2rgb(colcode[as.character(values)], alpha = TRUE))
        }
      }
      a <- apply(v, 1, function(cl) {
        paste0(
          "<span style=\"width:100%;display:block;text-align:center;",
          "color:",
          rgb(255 - cl[[1]],
            255 - cl[[2]],
            255 - cl[[3]],
            maxColorValue = 255.0
          ),
          ";",
          "overflow:hidden;background:",
          rgb(cl[[1]],
            cl[[2]],
            cl[[3]],
            maxColorValue = 255.0
          ),
          "\" title=\""
        )
      })
      b <- apply(v, 1, function(cl) {
        paste0(
          "\" sort=\""
        )
      })
      cc <- apply(v, 1, function(cl) {
        paste0(
          "\">"
        )
      })
      d <- apply(v, 1, function(cl) {
        paste0(
          "</span>"
        )
      })
      if (displayValues) {
        if (relative) {
          dv <- paste0(round(100 * values, 0), "%")
        } else {
          dv <- values
        }
      } else {
        dv <- "&nbsp;"
      }
      if (relative) {
        paste0(a, round(100 * values, 1), "%", b, values, cc, dv, d)
      } else {
        if (length(level_names) > 0) {
          hover <- level_names[as.character(values)]
        } else {
          hover <- round(values, 1)
        }
        paste0(a, hover, b, values, cc, dv, d)
      }
    })
    x[, names(colors_of_hm)] <- colors_of_hm

    colnames_html <- paste(
      "<div class=\"colheader\">",
      vapply(
        strsplit(
          colnames(x),
          "",
          fixed = TRUE
        ),
        function(letters) {
          paste0(
            "<span>",
            paste0(letters,
              collapse = ""
            ),
            "</span>"
          )
        },
        FUN.VALUE = character(1)
      ),
      "</div>"
    )

    options <- list(
      pageLength = nrow(x),
      columnDefs = list(
        list(
          targets = seq_len(ncol(x) - 1L),
          render = util_html_table_js("sort_heatmap_dt")
        )
      )
    )

    # https://www.pierrerebours.com/2017/09/custom-sorting-with-dt.html
    # https://datatables.net/manual/data/orthogonal-data
    # filter = "top" is not helpful
    if (use_dt2) {
      x_dt2 <- x
      colnames(x_dt2) <- colnames_html
      w <- util_html_table_dt2_widget(
        x_dt2,
        options = options,
        class = "ReportSummaryTable myDT matrixTable",
        fillContainer = fillContainer
      )
    } else {
      w <- DT::datatable(x,
        fillContainer = fillContainer,
        rownames = FALSE,
        options = options,
        class = "ReportSummaryTable myDT matrixTable",
        colnames = colnames_html,
        escape = FALSE
      )
    }


    # https://stackoverflow.com/a/35775262
    w$dependencies <- c(
      w$dependencies,
      list(html_dependency_vert_table())
    )

    if (fillContainer) {
      w <- htmlwidgets::onRender(w, "
        function(el, x) {
          el.classList.add('dq-matrix-fill-container');

          var pending = false;
          var retries = 60;

          function findWrapper() {
            return el.querySelector('.dt-container, .dataTables_wrapper') ||
              (el.closest && el.closest('.dt-container, .dataTables_wrapper'));
          }

          function getApi(table) {
            try {
              if (typeof dqGetDataTableApi === 'function') {
                var api = dqGetDataTableApi(table);
                if (api) return api;
              }
            } catch (e) {}
            try {
              if (window.jQuery && jQuery.fn && jQuery.fn.DataTable) {
                return jQuery(table).DataTable();
              }
            } catch (e) {}
            try {
              if (el._dt2 && el._dt2.columns) return el._dt2;
            } catch (e) {}
            return null;
          }

          function resetMatrixWidths(wrapper) {
            Array.prototype.forEach.call(
              wrapper.querySelectorAll(
                'table.matrixTable, .dt-scroll table, .dataTables_scroll table'
              ),
              function(table) {
                table.style.width = '100%';
                table.style.minWidth = '0';
                table.style.tableLayout = 'fixed';
                Array.prototype.forEach.call(
                  table.querySelectorAll('col'),
                  function(col) {
                    col.style.width = '';
                    col.style.minWidth = '0';
                    col.removeAttribute('width');
                  }
                );
              }
            );
          }

          function syncMatrixWidths(wrapper) {
            var body = wrapper.querySelector(
              '.dt-scroll-body, .dataTables_scrollBody'
            );
            var head = wrapper.querySelector(
              '.dt-scroll-head, .dataTables_scrollHead'
            );
            var headInner = wrapper.querySelector(
              '.dt-scroll-headInner, .dataTables_scrollHeadInner'
            );
            var bodyTable = body && body.querySelector('table');
            var headTable = head && head.querySelector('table');
            var width = body ? body.clientWidth : wrapper.clientWidth;

            if (!width || !isFinite(width) || width <= 0) return;

            wrapper.style.width = '100%';
            wrapper.style.maxWidth = '100%';
            if (body) {
              body.style.width = '100%';
              body.style.maxWidth = '100%';
              body.style.overflowX = 'auto';
            }
            if (headInner) {
              headInner.style.width = width + 'px';
            }
            Array.prototype.forEach.call([bodyTable, headTable], function(table) {
              if (!table) return;
              table.style.width = width + 'px';
              table.style.minWidth = '0';
              table.style.tableLayout = 'fixed';
              Array.prototype.forEach.call(
                table.querySelectorAll('col'),
                function(col) {
                  col.style.width = '';
                  col.style.minWidth = '0';
                  col.removeAttribute('width');
                }
              );
            });
          }

          function adjustFixedParts(api) {
            try {
              if (api && api.columns) api.columns.adjust();
            } catch (e) {}
            try {
              if (api && api.responsive) api.responsive.recalc();
            } catch (e) {}
            try {
              if (api && api.fixedColumns &&
                  api.fixedColumns().relayout) {
                api.fixedColumns().relayout();
              }
            } catch (e) {}
            try {
              if (api && api.fixedHeader) api.fixedHeader.adjust();
            } catch (e) {}
          }

          function resizeMatrixTable() {
            pending = false;
            var wrapper = findWrapper();
            if (!wrapper) {
              if (retries-- > 0) setTimeout(scheduleResize, 25);
              return;
            }

            var table = wrapper.querySelector('table.matrixTable');
            if (!table) {
              if (retries-- > 0) setTimeout(scheduleResize, 25);
              return;
            }

            var api = getApi(table);
            resetMatrixWidths(wrapper);
            adjustFixedParts(api);
            syncMatrixWidths(wrapper);
            window.requestAnimationFrame(function() {
              adjustFixedParts(api);
              syncMatrixWidths(wrapper);
            });
            setTimeout(function() {
              adjustFixedParts(api);
              syncMatrixWidths(wrapper);
            }, 100);
            setTimeout(function() {
              adjustFixedParts(api);
              syncMatrixWidths(wrapper);
            }, 350);
          }

          function scheduleResize() {
            if (pending) return;
            pending = true;
            window.requestAnimationFrame(resizeMatrixTable);
          }

          if (el._dqMatrixFillObserver) {
            el._dqMatrixFillObserver.disconnect();
          }
          if (typeof window.ResizeObserver === 'function') {
            el._dqMatrixFillObserver = new window.ResizeObserver(scheduleResize);
            el._dqMatrixFillObserver.observe(el);
            if (el.parentElement) {
              el._dqMatrixFillObserver.observe(el.parentElement);
            }
          }
          if (el._dqMatrixFillResizeHandler) {
            window.removeEventListener('resize', el._dqMatrixFillResizeHandler);
          }
          el._dqMatrixFillResizeHandler = scheduleResize;
          window.addEventListener('resize', scheduleResize);
          scheduleResize();
        }
      ")
    }

    if (view) print(w)

    attr(w, "from_ReportSummaryTable") <- TRUE

    return(invisible(w))

    # https://stackoverflow.com/a/46043032
  } else { # https://stackoverflow.com/a/64112567
    if (continuous &&
        (length(unique(tb$variable)) == 1 ||
            length(unique(tb$Variables)) == 1)) { # if only one dimension and real numbers, not categories, collapse the heatmap to a barchart # nolint: line_length_linter.
      if (length(unique(tb$Variables)) == 1) { # only one value in y direction
        x <- "variable"
        y <- "value"
        fill <- "value"
        if (relative) {
          if (suppressWarnings(max(tb$value, na.rm = TRUE)) > 0.1) {
            add_amount <- 0.04
          } else {
            add_amount <- 0.0004
          }
          rel_ax <- scale_y_continuous(
            labels = scales::percent,
            expand = expansion(add = c(
              0,
              add_amount
            ))
          )
        } else {
          rel_ax <- NULL
        }
      } else {
        x <- "Variables"
        y <- "value"
        fill <- "value"
        if (relative) {
          if (suppressWarnings(max(tb$value, na.rm = TRUE)) > 0.1) {
            add_amount <- 0.04
          } else {
            add_amount <- 0.0004
          }
          rel_ax <- scale_y_continuous(
            labels = scales::percent,
            expand = expansion(add = c(
              0,
              add_amount
            ))
          )
        } else {
          rel_ax <- NULL
        }
      }

      if (missing(flip_mode) && getOption(
        "dataquieR.flip_mode",
        dataquieR.flip_mode_default
      ) ==
        dataquieR.flip_mode_default) {
        # for bar charts, flip mode defaults to default (noflip)
        # Historical coord_cartesian fallback removed here.
        fli <- coord_flip()
      } else {
        fli <- util_coord_flip(
          w = length(unique(tb[[x]])),
          h = length(unique(tb[[y]]))
        )
      }

      is_flipped <- inherits(fli, "CoordFlip")

      if (is_flipped) {
        hjust <- 0
        vjust <- 0
      } else {
        hjust <- 0.5
        vjust <- 0
      }
      tb_copy <- tb
      tb_copy <- tb_copy[is.finite(tb_copy$value), , drop = FALSE]
      if (nrow(tb_copy) == 0) {
        p <- ggplot() +
          annotate("text", x = 0, y = 0, label = "Empty result.") +
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
          )
        p <- util_set_size(p)
        gtlb <- character(0)
      } else {
        if (relative) {
          scale_fill <- scale_fill_gradientn(
            colors = rev(colscale),
            labels = scales::percent
          )
          gtlb <- paste0(" ", round(tb_copy$value * 100, digits = 2), "%")
          texts <-
            util_create_lean_ggplot(
              geom_text(
                label = gtlb,
                hjust = hjust, vjust = vjust, size = 3.5
              ),
              gtlb = gtlb,
              hjust = hjust, vjust = vjust
            )
        } else {
          scale_fill <- scale_fill_gradientn(colors = rev(colscale))
          gtlb <- paste0(" ", round(tb_copy$value, digits = 2))
          texts <-
            util_create_lean_ggplot(
              geom_text(
                label = gtlb,
                hjust = hjust, vjust = vjust, size = 3.5
              ),
              gtlb = gtlb,
              hjust = hjust, vjust = vjust
            )
        }

        p <- util_create_lean_ggplot(
          ggplot(tb_copy, aes(
            x = .data[[x]], y = .data[[y]],
            fill = .data[[fill]]
          )) +
            geom_bar(
              stat = "identity", na.rm = TRUE,
              colour = "white", linewidth = 0.8
            ) + # https://github.com/tidyverse/ggplot2/issues/5051
            theme_minimal() +
            texts +
            fli +
            scale_fill +
            xlab("") +
            guides(fill = guide_legend(
              title = ""
              # Historical explicit legend grid settings removed here.
            )) +
            rel_ax +
            theme(
              legend.position = "bottom",
              axis.text.x = element_text(angle = 90, hjust = 0),
              axis.text.y = element_text(size = 10)
            ) +
            xlab("") +
            ylab(""),
          tb_copy = tb_copy,
          x = x,
          y = y,
          fill = fill,
          texts = texts,
          fli = fli,
          scale_fill = scale_fill,
          rel_ax = rel_ax
        )
      }


      if (suppressWarnings(util_ensure_suggested("plotly",
            goal = "generate interactive plots",
            err = FALSE
          ))) {
        attr(p, "py") <- local({
          tb_copy <- tb
          tb_copy <- tb_copy[is.finite(tb_copy$value), , drop = FALSE]
          marker_color <- try(ggplot2::ggplot_build(p)$data[[1]]$fill,
            silent = TRUE
          )
          if (util_is_try_error(marker_color) ||
              length(marker_color) != nrow(tb_copy)) {
            marker_color <- tb_copy[[fill]]
          }
          py <- plotly::plot_ly(tb_copy,
            x = tb_copy[[ifelse(is_flipped, y, x)]],
            y = tb_copy[[ifelse(is_flipped, x, y)]],
            type = "bar",
            marker = list(
              color = marker_color,
              line = list(color = "white", width = 0.8)
            )
          )
          gtlb_copy <- gtlb
          gtlb_copy <- gtlb_copy[is.finite(tb$value)]
          py <- plotly::style(py,
            text = gtlb_copy,
            textposition = "auto"
          )

          if (relative) {
            if (is_flipped) {
              py <- plotly::layout(py, xaxis = list(
                tickformat = ".2%",
                rangemode = "tozero"
              ))
            } else {
              py <- plotly::layout(py, yaxis = list(
                tickformat = ".2%",
                rangemode = "tozero"
              ))
            }
          }

          # Historical plotly bar-chart prototype removed here. Inspect with
          # `git show 2f89c36b44 -- R/print.ReportSummaryTable.R`.
          py
        })
      }

      if (relative) {
        fct <- 100
      } else {
        fct <- 1
      }

      attr(p, "sizing_hints") <- list(
        figure_type_id = "bar_chart",
        rotated = is_flipped,
        number_of_bars = ifelse(is.null(nrow(util_gg_get(p, "data"))), 0,
          nrow(util_gg_get(p, "data"))
        ),
        range = (fct * suppressWarnings(max(util_gg_get(p, "data")[[y]],
              na.rm = TRUE
            ))) -
          (fct * suppressWarnings(min(util_gg_get(p, "data")[[y]],
                na.rm = TRUE
              ))),
        no_char_vars = suppressWarnings(max(nchar(
          as.character(util_gg_get(p, "data")$Variables)
        ), na.rm = TRUE)),
        no_char_numbers = suppressWarnings(max(nchar(
          util_gg_get(p, "data")$value
        ), na.rm = TRUE)) # max no. numbers for tick labels
      )


      if (view) print(p)
      attr(p, "from_ReportSummaryTable") <- TRUE
      return(invisible(p))
    } else {
      if (continuous) {
        xlim <- as.character(tb$Variables)
        xlim <- xlim[!duplicated(xlim)]
        ylim <- as.character(tb$variable)
        # Historical dynamic plotly font-size prototype removed here. Inspect
        # with `git show 214dd76a7d -- R/print.ReportSummaryTable.R`.
        xsize <- 10
        ysize <- 10

        p <- util_create_lean_ggplot(
          ggplot(tb, aes(x = Variables, y = variable, colour = value, size = value)) + # nolint: line_length_linter.
            geom_point() + # scale_size_continuous(range = c(-1, 10)) + scale_x_discrete() +# breaks = 50 * seq_len(length(unique(tb$Variables)))) + # nolint: line_length_linter.
            # scale_x_discrete(guide = guide_axis(n.dodge = 5)) +
            labs(
              title = waiver(),
              subtitle = waiver(),
              x = "",
              y = ""
            ) +
            scale_x_discrete(limits = xlim) +
            # (if (nrow(hm) < ncol(hm)) coord_flip()) + # 7x5 standard of
            # rmarkdown, not din a4
            scale_color_gradientn(colors = rev(colscale)) +
            theme_minimal() +
            theme( # aspect.ratio=5*length(unique(tb$Variables))/7/length(unique(tb$Variables)), # nolint: line_length_linter.
              axis.text.x = element_text(
                angle = 35, hjust = 1,
                size = xsize
              ),
              axis.text.y = element_text(size = ysize)
            ),
          tb = tb,
          xlim = xlim,
          colscale = colscale,
          xsize = xsize,
          ysize = ysize
        )


        no_char_vars <- max(nchar(as.character(tb$Variables)))
        no_char_cat <- max(nchar(as.character(tb$variable)))


        fli <- util_coord_flip(p = p, w = nrow(hm), h = ncol(hm))
        is_flipped <- inherits(fli, "CoordFlip")
        p <- util_lazy_add_coord(p, fli)

        p <- util_set_size(p, 500, 300)

        if (view) print(p)

        attr(p, "from_ReportSummaryTable") <- TRUE

        size_info <- util_attr(x, "size_info", exact = TRUE)
        if (!is.null(size_info) &&
            size_info == "fix_size") {
          attr(p, "sizing_hints") <- list(
            figure_type_id = "dot_mat_fix",
            rotated = FALSE,
            number_of_vars = nrow(x),
            number_of_cat = 3,
            no_char_vars = no_char_vars,
            no_char_cat = no_char_cat
          )
        } else {
          attr(p, "sizing_hints") <- list(
            figure_type_id = "dot_mat",
            rotated = is_flipped,
            number_of_vars = nrow(x),
            number_of_cat = ncol(x) - 2,
            no_char_vars = no_char_vars,
            no_char_cat = no_char_cat
          )
        }


        return(invisible(p))
      } else {
        tb$value <- as.factor(tb$value)

        if (length(level_names) > 0) {
          levels(tb$value) <- level_names[levels(tb$value)]
          cc <- setNames(colcode, nm = level_names)
        } else {
          cc <- colcode
        }

        # Historical fallback color palette removed here.
        fli <- util_coord_flip(w = nrow(hm), h = ncol(hm))
        lcolcode <- length(colcode)
        p <- util_create_lean_ggplot(
          ggplot(tb, aes(
            x = variable, y = Variables,
            fill = value
          )) +
            geom_tile(colour = "white", linewidth = 0.8) + # https://github.com/tidyverse/ggplot2/issues/5051
            theme_minimal() +
            # (if (nrow(hm) > ncol(hm)) coord_flip()) +
            fli +
            scale_fill_manual(values = cc, name = " ") +
            # scale_fill_gradientn(colors = rev(colscale)) +
            # scale_x_discrete(position = "top") +
            xlab("") +
            ylab("") +
            guides(fill = guide_legend(
              ncol = 1, nrow = lcolcode,
              byrow = TRUE
            )) +
            theme(
              legend.position = "bottom",
              axis.text.x = element_text(angle = 90, hjust = 0, size = 10),
              axis.text.y = element_text(size = 10)
            ),
          tb = tb,
          fli = fli,
          cc = cc,
          lcolcode = lcolcode
        )
        if (view) print(p)

        attr(p, "from_ReportSummaryTable") <- TRUE

        if (isTRUE(util_attr(x, "render_as_plot", exact = TRUE))) {
          return(invisible(p))
        }
        return(invisible(x))
      }
    }
  }
}

#' @exportS3Method knitr::knit_print
knit_print.ReportSummaryTable <- print.ReportSummaryTable

#' Apply grading-rule colors when a ReportSummaryTable is rendered
#'
#' Tables opt in by attaching a `grading_context` containing metric values and
#' grading-rule-set references. The rules themselves are deliberately resolved
#' here so that changing the loaded rules affects later renders of an existing
#' result object.
#'
#' @param x a `ReportSummaryTable` object.
#'
#' @return `x` with render-time color attributes.
#' @noRd
util_apply_report_summary_table_grading <- function(x) {
  context <- util_attr(x, "grading_context", exact = TRUE)
  if (is.null(context)) {
    return(x)
  }

  required_context <- c(
    "indicator_metric", "entity", "values_raw", "var_names",
    "grading_rule_sets"
  )
  if (!all(required_context %in% names(context)) ||
      length(context$values_raw) != nrow(x) ||
      length(context$var_names) != nrow(x) ||
      length(context$grading_rule_sets) != nrow(x)) {
    return(x)
  }

  value_columns <- setdiff(colnames(x), c("Variables", "N"))
  if (length(value_columns) != 1L) {
    return(x)
  }

  summary_values <- data.frame(
    function_name = rep("com_segment_missingness", nrow(x)),
    indicator_metric = rep(context$indicator_metric, nrow(x)),
    values_raw = context$values_raw,
    call_names = rep("", nrow(x)),
    stringsAsFactors = FALSE
  )
  summary_values[[VAR_NAMES]] <- context$var_names
  summary_values$.row_id <- seq_len(nrow(summary_values))
  grading_meta_data <- data.frame(
    context$var_names,
    context$grading_rule_sets,
    stringsAsFactors = FALSE
  )
  colnames(grading_meta_data) <- c(VAR_NAMES, GRADING_RULESET)
  classes <- suppressWarnings(util_metrics_to_classes(
    summary_values,
    grading_meta_data,
    entity = context$entity
  ))
  classes <- classes[order(classes$.row_id), , drop = FALSE]

  colors <- unname(util_get_colors()[
    as.character(classes$class)
  ])
  colors[is.na(colors)] <- "#888888"
  class_labels <- unname(util_get_labels_grading_class()[
    as.character(classes$class)
  ])
  class_labels[is.na(class_labels)] <- "not classified"

  bar_context <- util_attr(x, "segment_missingness_bar", exact = TRUE)
  if (is.null(bar_context)) {
    return(x)
  }
  bar_context$colors <- colors
  bar_context$class_labels <- class_labels
  attr(x, "segment_missingness_bar") <- bar_context
  x
}

#' Choose a readable percentage axis for segment-missingness bars
#'
#' @param values [numeric] proportions of missing segments.
#'
#' @return A list with axis limit, breaks, labels, and truncation flag.
#' @noRd
util_segment_missingness_axis_spec <- function(values) {
  values <- values[is.finite(values) & values >= 0]
  maximum <- max(c(0, values))
  target <- min(1, max(0.01, 1.15 * maximum))
  candidates <- c(
    0.01, 0.02, 0.05, 0.10, 0.15, 0.20, 0.25,
    0.30, 0.40, 0.50, 0.60, 0.75, 1
  )
  limit <- candidates[which(candidates >= target)[[1L]]]
  breaks <- pretty(c(0, limit), n = 3)
  breaks <- breaks[breaks >= 0 & breaks < limit]
  breaks <- unique(c(breaks, limit))
  accuracy <- if (limit <= 0.02) 0.1 else 1
  labels <- scales::percent(breaks, accuracy = accuracy)

  list(
    limit = limit,
    breaks = breaks,
    labels = labels,
    truncated = limit < 1
  )
}

#' Label the focused endpoint of a segment-missingness axis
#'
#' @param axis_spec list returned by the axis-specification helper.
#' @param plotly [logical] create HTML labels for Plotly instead of plotmath.
#'
#' @return An expression or character vector of labels.
#' @noRd
util_segment_missingness_axis_labels <- function(axis_spec, plotly = FALSE) {
  labels <- axis_spec$labels
  if (!axis_spec$truncated || !length(labels)) {
    return(labels)
  }
  last_label <- length(labels)
  if (plotly) {
    labels[[last_label]] <- sprintf("<b>%s</b>", labels[[last_label]])
    return(labels)
  }
  labels <- as.list(labels)
  labels[[last_label]] <- bquote(bold(.(labels[[last_label]])))
  as.expression(labels)
}

#' Build grading-rule backgrounds for segment-missingness bars
#'
#' The backgrounds use the same interval membership and worst-class handling
#' as the result classification. Gaps between rules remain uncolored.
#'
#' @param x a `ReportSummaryTable` carrying segment-missingness metadata.
#' @param axis_limit [numeric] upper visible axis limit, as a proportion.
#'
#' @return A data frame with one row per visible colored interval.
#' @noRd
util_segment_missingness_grading_bands <- function(x, axis_limit) {
  bar_context <- util_attr(x, "segment_missingness_bar", exact = TRUE)
  grading_context <- util_attr(x, "grading_context", exact = TRUE)
  empty_bands <- data.frame(
    Variables = character(), lower = numeric(), upper = numeric(),
    fill = character(), class_label = character(),
    stringsAsFactors = FALSE
  )
  if (is.null(bar_context) || !is.finite(axis_limit) || axis_limit <= 0) {
    return(empty_bands)
  }

  visible_max <- 100 * axis_limit
  thresholds <- NULL
  if (identical(bar_context$color_mode, "grading_rules") &&
      !is.null(grading_context)) {
    grading_meta_data <- data.frame(
      grading_context$var_names,
      grading_context$grading_rule_sets,
      stringsAsFactors = FALSE
    )
    colnames(grading_meta_data) <- c(VAR_NAMES, GRADING_RULESET)
    thresholds <- util_get_thresholds(
      grading_context$indicator_metric,
      grading_meta_data
    )
  } else if (identical(bar_context$color_mode, "legacy_threshold") &&
      length(bar_context$threshold_value) == 1L &&
      length(bar_context$direction) == 1L) {
    threshold <- as.numeric(bar_context$threshold_value)
    if (is.finite(threshold)) {
      if (identical(bar_context$direction, "above")) {
        rules <- c(
          Normal = sprintf("[0;%s]", threshold),
          Critical = sprintf("(%s;Inf]", threshold)
        )
      } else {
        rules <- c(
          Critical = sprintf("[0;%s)", threshold),
          Normal = sprintf("[%s;Inf]", threshold)
        )
      }
      thresholds <- rep(list(rules), nrow(x))
      names(thresholds) <- as.character(x$Variables)
    }
  }
  if (is.null(thresholds)) {
    return(empty_bands)
  }

  grading_colors <- util_get_colors()
  grading_labels <- util_get_labels_grading_class()
  bands <- lapply(seq_len(nrow(x)), function(row) {
    variable <- as.character(x$Variables[[row]])
    rules <- thresholds[[variable]]
    if (!length(rules)) {
      return(NULL)
    }
    intervals <- lapply(rules, util_parse_interval)
    valid <- vapply(intervals, inherits, logical(1), what = "interval")
    intervals <- intervals[valid]
    rule_names <- names(rules)[valid]
    if (!length(intervals)) {
      return(NULL)
    }
    boundaries <- unlist(lapply(intervals, function(interval) {
      c(interval$low, interval$upp)
    }), use.names = FALSE)
    boundaries <- sort(unique(c(
      0, visible_max,
      boundaries[is.finite(boundaries) & boundaries > 0 &
          boundaries < visible_max]
    )))
    if (length(boundaries) < 2L) {
      return(NULL)
    }
    lower <- head(boundaries, -1L)
    upper <- tail(boundaries, -1L)
    midpoint <- lower + (upper - lower) / 2
    class_index <- vapply(midpoint, function(value) {
      matches <- which(vapply(intervals, function(interval) {
        isTRUE(redcap_env$`in`(value, interval))
      }, logical(1)))
      if (!length(matches)) NA_integer_ else tail(matches, 1L)
    }, integer(1))
    keep <- !is.na(class_index) & upper > lower
    if (!any(keep)) {
      return(NULL)
    }
    classes <- rule_names[class_index[keep]]
    if (identical(bar_context$color_mode, "legacy_threshold")) {
      colors <- c(Normal = "#2166AC", Critical = "#7f0000")[classes]
      labels <- classes
    } else {
      colors <- grading_colors[classes]
      labels <- grading_labels[classes]
    }
    data.frame(
      Variables = variable,
      lower = lower[keep] / 100,
      upper = upper[keep] / 100,
      fill = unname(colors),
      class_label = unname(labels),
      stringsAsFactors = FALSE
    )
  })
  bands <- util_rbind(data_frames_list = bands)
  if (!nrow(bands)) empty_bands else bands
}

#' Create subtle background colors from grading colors
#'
#' @param colors [character] colors understood by [grDevices::col2rgb()].
#'
#' @return A character vector of pale colors with the original hues.
#' @noRd
util_segment_missingness_band_colors <- function(colors) {
  result <- colors
  valid <- !is.na(colors) & nzchar(colors)
  if (!any(valid)) {
    return(result)
  }
  hsv_colors <- grDevices::rgb2hsv(
    grDevices::col2rgb(colors[valid]),
    maxColorValue = 255
  )
  result[valid] <- grDevices::hsv(
    h = hsv_colors["h", , drop = TRUE],
    s = pmin(0.08, hsv_colors["s", , drop = TRUE] * 0.15),
    v = 0.985
  )
  result
}

#' Plot a segment-missingness ReportSummaryTable as percentage bars
#'
#' @param x a `ReportSummaryTable` carrying `segment_missingness_bar` metadata.
#'
#' @return a [ggplot2::ggplot] object.
#' @noRd
util_plot_segment_missingness_report_summary_table <- function(x) {
  context <- util_attr(x, "segment_missingness_bar", exact = TRUE)
  value_column <- context$value_column
  util_stop_if_not(
    !is.null(value_column),
    value_column %in% colnames(x),
    length(context$colors) == nrow(x),
    length(context$class_labels) == nrow(x)
  )

  values <- x[[value_column]] / x$N
  plot_data <- data.frame(
    Variables = x$Variables,
    value = values,
    fill = context$colors,
    class_label = context$class_labels,
    stringsAsFactors = FALSE
  )
  plot_data <- plot_data[is.finite(plot_data$value), , drop = FALSE]
  plot_data$label <- scales::percent(plot_data$value, accuracy = 0.01)
  axis_spec <- util_segment_missingness_axis_spec(plot_data$value)
  grading_bands <- util_segment_missingness_grading_bands(
    x,
    axis_spec$limit
  )
  if (nrow(grading_bands)) {
    grading_bands$background_fill <-
      util_segment_missingness_band_colors(grading_bands$fill)
    grading_bands$midpoint <- (
      grading_bands$lower + grading_bands$upper
    ) / 2
    grading_bands$width <- grading_bands$upper - grading_bands$lower
  }

  legend_rows <- !duplicated(paste(
    plot_data$fill,
    plot_data$class_label,
    sep = "\r"
  ))
  legend_breaks <- plot_data$fill[legend_rows]
  legend_labels <- plot_data$class_label[legend_rows]
  if (identical(context$color_mode, "grading_rules")) {
    grading_labels <- unname(util_get_labels_grading_class())
    grading_colors <- unname(util_get_colors())
    present_grades <- grading_labels %in% legend_labels
    extra_labels <- !(legend_labels %in% grading_labels)
    legend_breaks <- c(
      grading_colors[present_grades],
      legend_breaks[extra_labels]
    )
    legend_labels <- c(
      grading_labels[present_grades],
      legend_labels[extra_labels]
    )
  }

  p <- ggplot(plot_data, aes(
    x = .data$Variables,
    y = .data$value,
    fill = .data$fill
  ))
  if (nrow(grading_bands)) {
    p <- p + geom_tile(
      data = grading_bands,
      aes(
        x = .data$Variables,
        y = .data$midpoint,
        height = .data$width,
        fill = .data$background_fill
      ),
      width = 0.9,
      inherit.aes = FALSE,
      show.legend = FALSE
    )
  }
  p <- p +
    geom_bar(
      stat = "identity",
      na.rm = TRUE,
      colour = "white",
      linewidth = 0.8,
      width = 0.62
    ) +
    geom_text(
      aes(label = .data$label),
      hjust = -0.1,
      vjust = 0.5,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_identity(
      name = NULL,
      breaks = legend_breaks,
      labels = legend_labels,
      guide = guide_legend(nrow = 1, byrow = TRUE)
    ) +
    scale_x_discrete(name = NULL) +
    scale_y_continuous(
      name = "Missing segments",
      breaks = axis_spec$breaks,
      labels = util_segment_missingness_axis_labels(axis_spec),
      limits = c(0, axis_spec$limit),
      expand = expansion(mult = c(0, 0))
    ) +
    coord_flip(clip = "off") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box.margin = ggplot2::margin(t = 0),
      axis.title.y = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.margin = ggplot2::margin(5.5, 18, 5.5, 5.5)
    )
  if (axis_spec$truncated) {
    p <- p + geom_segment(
      x = 0.55,
      xend = 0.78,
      y = axis_spec$limit,
      yend = axis_spec$limit,
      colour = "#6F6F6F",
      linewidth = 0.55,
      inherit.aes = FALSE
    )
  }

  if (suppressWarnings(util_ensure_suggested(
    "plotly",
    goal = "generate interactive plots",
    err = FALSE
  ))) {
    attr(p, "py") <- local({
      py <- plotly::plot_ly()
      if (nrow(grading_bands)) {
        for (band_fill in unique(grading_bands$background_fill)) {
          band_rows <- grading_bands$background_fill == band_fill
          band_data <- grading_bands[band_rows, , drop = FALSE]
          py <- plotly::add_trace(
            py,
            data = band_data,
            x = ~width,
            base = ~lower,
            y = ~Variables,
            type = "bar",
            orientation = "h",
            name = "",
            showlegend = FALSE,
            hoverinfo = "skip",
            width = 0.86,
            marker = list(color = band_fill, line = list(width = 0))
          )
        }
      }
      for (class_label in legend_labels) {
        class_rows <- plot_data$class_label == class_label
        class_data <- plot_data[class_rows, , drop = FALSE]
        py <- plotly::add_trace(
          py,
          data = class_data,
          x = ~value,
          y = ~Variables,
          type = "bar",
          orientation = "h",
          name = class_label,
          showlegend = TRUE,
          width = 0.58,
          text = ~label,
          textposition = "auto",
          hovertemplate = paste0(
            "%{y}: %{x:.2%}<br>",
            class_label,
            "<extra></extra>"
          ),
          marker = list(
            color = class_data$fill[[1L]],
            line = list(color = "white", width = 0.8)
          )
        )
      }
      plotly::layout(
        py,
        barmode = "overlay",
        showlegend = TRUE,
        xaxis = list(
          title = "Missing segments",
          tickvals = axis_spec$breaks,
          ticktext = util_segment_missingness_axis_labels(
            axis_spec,
            plotly = TRUE
          ),
          range = c(0, axis_spec$limit),
          rangemode = "tozero",
          showline = TRUE,
          linecolor = "#4D4D4D",
          linewidth = 1
        ),
        yaxis = list(
          title = "",
          categoryorder = "array",
          categoryarray = plot_data$Variables
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.34,
          yanchor = "top"
        ),
        shapes = if (axis_spec$truncated) {
          list(list(
            type = "line",
            xref = "x",
            x0 = axis_spec$limit,
            x1 = axis_spec$limit,
            yref = "paper",
            y0 = 0,
            y1 = 0.055,
            line = list(color = "#6F6F6F", width = 1.2)
          ))
        } else {
          list()
        },
        margin = list(b = 115)
      )
    })
  }

  attr(p, "segment_missingness_colors") <- context$color_mode
  attr(p, "sizing_hints") <- list(
    figure_type_id = "bar_chart",
    rotated = TRUE,
    number_of_bars = nrow(plot_data),
    range = 100 * diff(range(c(0, plot_data$value), na.rm = TRUE)),
    no_char_vars = suppressWarnings(max(
      nchar(as.character(plot_data$Variables)),
      na.rm = TRUE
    )),
    no_char_numbers = suppressWarnings(max(
      nchar(plot_data$label),
      na.rm = TRUE
    ))
  )
  attr(p, "from_ReportSummaryTable") <- TRUE
  p
}
