#' DT2 helpers for util_html_table
#'
#' These helpers collect the DataTables 2 specific translation layer for
#' `util_html_table()`. They are deliberately kept separate from the old DT
#' implementation while issue #776 replaces the widget backend.
#'
#' @keywords internal
#' @noRd
util_html_table_dt2_extensions <- function(searchBuilder = FALSE,
  buttons = TRUE,
  fixedColumns = TRUE,
  fixed_header = FALSE,
  responsive = TRUE,
  columnControl = FALSE,
  scroller = FALSE) {
  extensions <- character(0)

  if (isTRUE(buttons)) {
    extensions <- c(extensions, "Buttons")
  }
  if (isTRUE(fixedColumns)) {
    extensions <- c(extensions, "FixedColumns")
  }
  if (isTRUE(fixed_header)) {
    extensions <- c(extensions, "FixedHeader")
  }
  if (isTRUE(responsive)) {
    extensions <- c(extensions, "Responsive")
  }
  if (isTRUE(columnControl)) {
    extensions <- c(extensions, "ColumnControl")
  }
  if (isTRUE(searchBuilder)) {
    extensions <- c(extensions, "SearchBuilder")
  }
  if (isTRUE(scroller)) {
    extensions <- c(extensions, "Scroller")
  }

  unique(extensions)
}

#' @keywords internal
#' @noRd
util_html_table_dt2_filter_position <- function(filter = "none") {
  if (is.null(filter)) {
    return("none")
  }

  if (is.list(filter)) {
    filter <- filter[["position"]]
    if (is.null(filter)) {
      return("none")
    }
  }

  util_stop_if_not(
    "DT2 table filters need one position." =
      is.character(filter) && length(filter) == 1 && !is.na(filter)
  )
  util_stop_if_not(
    "DT2 table filters support the positions 'none', 'bottom', and 'top'." =
      filter %in% c("none", "bottom", "top")
  )
  filter
}

#' @keywords internal
#' @noRd
util_html_table_dt2_prepare_filter_data <- function(tb,
  copy_row_names_to_column) {
  class(tb) <- setdiff(class(tb), "dataquieR_result")
  if (is.matrix(tb)) {
    tb <- as.data.frame.matrix(tb, stringsAsFactors = FALSE)
  }
  if (isTRUE(copy_row_names_to_column)) {
    tb <- cbind.data.frame(
      data.frame(Variables = rownames(tb), stringsAsFactors = FALSE),
      tb
    )
    rownames(tb) <- NULL
  }
  tb
}

#' @keywords internal
#' @noRd
util_html_table_dt2_column_filter_control <- function(column) {
  data_type <- util_attr(column, DATA_TYPE, exact = TRUE)
  if (!is.null(data_type) && length(data_type) == 1L &&
      !any(is.na(data_type))) {
    data_type <- trimws(tolower(data_type))
    if (data_type %in% tolower(c(DATA_TYPES$DATETIME, DATA_TYPES$TIME))) {
      return("searchDateTime")
    }
    if (data_type %in% tolower(c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT))) {
      return("searchNumber")
    }
    if (identical(data_type, tolower(DATA_TYPE_NUMBER_PAREN))) {
      return("searchNumber")
    }
    if (util_html_table_dt2_is_number_paren_filter_column(column)) {
      return("searchNumber")
    }
    return(util_html_table_dt2_search_list_control())
  }

  if (inherits(column, c("Date", "POSIXct", "POSIXlt"))) {
    return("searchDateTime")
  }
  if (util_html_table_dt2_is_number_paren_filter_column(column)) {
    return("searchNumber")
  }
  if (util_html_table_dt2_is_discrete_filter_column(column)) {
    return(util_html_table_dt2_search_list_control())
  }
  if (is.numeric(column) || is.integer(column) ||
      util_html_table_dt2_is_numeric_filter_column(column)) {
    return("searchNumber")
  }

  util_html_table_dt2_search_list_control()
}

#' @keywords internal
#' @noRd
util_html_table_dt2_search_list_control <- function() {
  list(
    extend = "dropdown",
    text = "Filter",
    content = list(
      list(
        extend = "searchList",
        orthogonal = "dq_filter_label"
      )
    )
  )
}

#' @keywords internal
#' @noRd
util_html_table_dt2_is_discrete_filter_column <- function(column) {
  if (is.list(column) || is.factor(column)) {
    return(FALSE)
  }
  if (!is.character(column)) {
    return(FALSE)
  }
  if (length(unique(column)) > 10L) {
    return(FALSE)
  }
  !any(grepl("<\\s*[a-zA-Z][^>]*>", column, perl = TRUE))
}

#' @keywords internal
#' @noRd
util_html_table_dt2_is_number_paren_filter_column <- function(column) {
  util_html_table_is_number_paren_column(column)
}

#' @keywords internal
#' @noRd
util_html_table_dt2_is_numeric_filter_column <- function(column) {
  if (!is.character(column)) {
    return(FALSE)
  }

  values <- gsub("<[^>]*>", "", column)
  values <- trimws(values)
  values <- values[nzchar(values)]
  if (length(values) == 0) {
    return(FALSE)
  }

  values <- gsub("[[:space:]\u00a0\u202f,]", "", values, perl = TRUE)
  !anyNA(suppressWarnings(as.numeric(values)))
}

#' @keywords internal
#' @noRd
util_html_table_dt2_filter_column_defs <- function(data, target) {
  if (is.null(data)) {
    return(NULL)
  }

  lapply(seq_along(data), function(column_index) {
    column <- data[[column_index]]
    column_filter_def <- list(
      targets = column_index - 1L,
      columnControl = list(
        target = target,
        content = list(
          util_html_table_dt2_column_filter_control(column)
        )
      )
    )
    if (util_html_table_dt2_is_number_paren_filter_column(column) ||
      identical(
        tolower(util_attr(column, DATA_TYPE, exact = TRUE)),
        tolower(DATA_TYPE_NUMBER_PAREN)
      )) {
      column_filter_def[["type"]] <- "num"
      column_filter_def[["searchBuilderType"]] <- "num"
      column_filter_def[["searchBuilder"]] <- list(orthogonal = "filter")
    }
    column_filter_def
  })
}

#' @keywords internal
#' @noRd
util_html_table_dt2_filter_options <- function(options,
  filter = "none",
  data = NULL) {
  position <- util_html_table_dt2_filter_position(filter)
  if (identical(position, "none") ||
      !is.null(options[["columnControl"]])) {
    return(options)
  }

  target <- if (identical(position, "top")) 1 else "tfoot:0"

  # A second header/footer row mirrors the layout of DT's column filters.
  options[["columnControl"]] <- list(
    target = target,
    content = list("search")
  )

  column_defs <- util_html_table_dt2_filter_column_defs(data, target)
  if (!is.null(column_defs)) {
    options[["columnDefs"]] <- c(options[["columnDefs"]], column_defs)
  }

  options
}

#' @keywords internal
#' @noRd
util_html_table_dt2_layout <- function(searchBuilder = FALSE,
  buttons = TRUE) {
  top_start <- character(0)

  if (isTRUE(searchBuilder)) {
    top_start <- c(top_start, "searchBuilder")
  }
  if (isTRUE(buttons)) {
    top_start <- c(top_start, "buttons")
  }

  if (length(top_start) == 0) {
    top_start <- NULL
  }

  list(
    topStart = top_start,
    topEnd = NULL,
    bottomStart = NULL,
    bottomEnd = NULL
  )
}

#' @keywords internal
#' @noRd
util_html_table_dt2_normalize_button_extensions <- function(buttons) {
  aliases <- c(
    copy = "copyHtml5",
    excel = "excelHtml5",
    csv = "csvHtml5",
    pdf = "pdfHtml5"
  )

  normalize_one <- function(button) {
    if (is.character(button)) {
      matched <- button %in% names(aliases)
      button[matched] <- unname(aliases[button[matched]])
      return(button)
    }

    if (is.list(button)) {
      extend <- button[["extend"]]
      if (is.character(extend) && length(extend) == 1 &&
          extend %in% names(aliases)) {
        button[["extend"]] <- unname(aliases[extend])
      }
      if (!is.null(button[["buttons"]])) {
        button[["buttons"]] <-
          util_html_table_dt2_normalize_button_extensions(button[["buttons"]])
      }
    }

    button
  }

  if (is.null(buttons)) {
    return(buttons)
  }
  if (is.character(buttons)) {
    return(normalize_one(buttons))
  }
  if (is.list(buttons)) {
    return(lapply(buttons, normalize_one))
  }

  buttons
}

#' @keywords internal
#' @noRd
util_html_table_dt2_normalize_fixed_columns <- function(options) {
  fixed_columns <- options[["fixedColumns"]]
  if (is.null(fixed_columns) || identical(fixed_columns, FALSE)) {
    return(options)
  }

  if (identical(fixed_columns, TRUE)) {
    options[["fixedColumns"]] <- list(start = 1L, end = 0L)
    return(options)
  }

  if (!is.list(fixed_columns)) {
    return(options)
  }

  if (!is.null(fixed_columns[["leftColumns"]]) &&
      is.null(fixed_columns[["start"]])) {
    fixed_columns[["start"]] <- fixed_columns[["leftColumns"]]
  }
  if (!is.null(fixed_columns[["left"]]) &&
      is.null(fixed_columns[["start"]])) {
    fixed_columns[["start"]] <- fixed_columns[["left"]]
  }
  if (!is.null(fixed_columns[["rightColumns"]]) &&
      is.null(fixed_columns[["end"]])) {
    fixed_columns[["end"]] <- fixed_columns[["rightColumns"]]
  }
  if (!is.null(fixed_columns[["right"]]) &&
      is.null(fixed_columns[["end"]])) {
    fixed_columns[["end"]] <- fixed_columns[["right"]]
  }

  fixed_columns[["leftColumns"]] <- NULL
  fixed_columns[["rightColumns"]] <- NULL
  fixed_columns[["left"]] <- NULL
  fixed_columns[["right"]] <- NULL

  if (is.null(fixed_columns[["start"]])) {
    fixed_columns[["start"]] <- 1L
  }
  if (is.null(fixed_columns[["end"]])) {
    fixed_columns[["end"]] <- 0L
  }

  options[["fixedColumns"]] <- fixed_columns
  options
}

#' @keywords internal
#' @noRd
util_html_table_dt2_normalize_options <- function(options,
  searchBuilder = FALSE,
  buttons = TRUE,
  filter = "none",
  data = NULL) {
  if (is.null(options)) {
    options <- list()
  }
  util_stop_if_not("DT2 options must be a list." = is.list(options))

  dom <- options[["dom"]]
  if (is.null(dom)) {
    dom <- ""
  }

  if (is.null(options[["layout"]])) {
    options[["layout"]] <- util_html_table_dt2_layout(
      searchBuilder = searchBuilder || grepl("Q", dom),
      buttons = buttons || grepl("B", dom)
    )
  }

  if (isTRUE(searchBuilder || grepl("Q", dom)) &&
      is.null(options[["searchBuilder"]])) {
    options[["searchBuilder"]] <- list(liveSearch = FALSE)
  } else if (identical(options[["searchBuilder"]], TRUE)) {
    options[["searchBuilder"]] <- list(liveSearch = FALSE)
  }

  options[["buttons"]] <-
    util_html_table_dt2_normalize_button_extensions(options[["buttons"]])
  options <- util_html_table_dt2_normalize_fixed_columns(options)
  options[["dom"]] <- NULL
  util_html_table_dt2_filter_options(options, filter = filter, data = data)
}

#' @keywords internal
#' @noRd
util_html_table_backend <- function(
  backend = getOption(
    "dataquieR.html_table_backend",
    dataquieR.html_table_backend_default
  ),
  installed = c(
    DT2 = requireNamespace("DT2", quietly = TRUE),
    DT = requireNamespace("DT", quietly = TRUE)
  ),
  goal = "Generating nice tables"
) {
  backend <- if (is.null(backend)) "auto" else backend
  util_expect_scalar(backend, check_type = is.character)
  backend <- tolower(backend)
  backend <- util_match_arg(backend, c("auto", "dt2", "dt"))

  installed <- as.logical(installed[c("DT2", "DT")])
  names(installed) <- c("DT2", "DT")
  installed[is.na(installed)] <- FALSE

  if (identical(backend, "dt2") && installed[["DT2"]]) {
    return("DT2")
  }
  if (identical(backend, "dt") && installed[["DT"]]) {
    return("DT")
  }
  if (identical(backend, "auto")) {
    if (installed[["DT2"]]) {
      return("DT2")
    }
    if (installed[["DT"]]) {
      return("DT")
    }
  }

  if (identical(backend, "dt2")) {
    util_error("Missing the package %s to %s.", dQuote("DT2"), goal)
  } else if (identical(backend, "dt")) {
    util_error("Missing the package %s to %s.", dQuote("DT"), goal)
  } else {
    util_error(
      "Missing either package %s or package %s to %s.",
      dQuote("DT2"), dQuote("DT"), goal
    )
  }
}

#' @keywords internal
#' @noRd
util_html_table_dt2_widget <- function(tb,
  options = list(),
  extensions = NULL,
  class = NULL,
  width = "100%",
  height = NULL,
  elementId = NULL,
  fillContainer = FALSE) {
  util_ensure_suggested("DT2", "Generating DT2 tables")

  if (is.data.frame(tb) && is.null(options[["columns"]])) {
    display_columns <- colnames(tb)
    transport_columns <- sprintf(
      "__dataquieR_col_%03d",
      seq_along(display_columns)
    )
    colnames(tb) <- transport_columns
    options[["columns"]] <- unname(Map(
      function(data, title) {
        list(data = data, title = title, defaultContent = "")
      },
      data = transport_columns,
      title = display_columns
    ))
  }

  fillContainer <- isTRUE(fillContainer)
  if (fillContainer) {
    class <- paste(c(class, "fill-container"), collapse = " ")
    width <- "100%"
    height <- "100%"
    if (is.null(options[["scrollX"]])) {
      options[["scrollX"]] <- TRUE
    }
    if (is.null(options[["scrollY"]])) {
      options[["scrollY"]] <- "100px"
    }
    if (is.null(options[["autoWidth"]])) {
      options[["autoWidth"]] <- FALSE
    }
  }

  widget <- DT2::dt2(
    data = tb,
    style = "core",
    button_class = "dt-button",
    class = class,
    options = options,
    extensions = extensions,
    width = width,
    height = height,
    elementId = elementId
  )

  widget$x$fillContainer <- fillContainer
  if (fillContainer) {
    widget$sizingPolicy$viewer$fill <- TRUE
  }

  htmlwidgets::onRender(widget, "
    function(el, x) {
      el.classList.add('datatables');
      if (!x.fillContainer) return;

      el.classList.add('fill-container');
      el.style.width = '100%';
      el.style.minHeight = '0';
      el.style.boxSizing = 'border-box';

      var pending = false;
      var retries = 50;

      function hasExplicitHeight(node) {
        if (!node) return false;
        var inlineHeight = node.style && node.style.height;
        if (inlineHeight && inlineHeight !== 'auto') return true;
        if (node.classList && (
            node.classList.contains('html-fill-container') ||
            node.classList.contains('html-fill-item'))) {
          return true;
        }
        return false;
      }

      function hasExplicitHeightContext(node) {
        while (node && node !== document.body) {
          if (hasExplicitHeight(node)) return true;
          node = node.parentElement;
        }
        return false;
      }

      function positiveHeight(node) {
        if (!node) return 0;
        var height = node.getBoundingClientRect().height;
        return height > 0 ? height : 0;
      }

      function availableHeight(wrapper) {
        if (!hasExplicitHeightContext(el.parentElement)) {
          return 0;
        }
        var ownHeight = positiveHeight(el);
        var parentHeight = positiveHeight(el.parentElement);
        var wrapperHeight = positiveHeight(wrapper);
        var available = ownHeight;

        if (parentHeight > 0 && (available === 0 || wrapperHeight >= available - 1)) {
          available = parentHeight;
        }
        return available;
      }

      function resizeTable() {
        pending = false;
        var wrapper = el.querySelector('.dt-container, .dataTables_wrapper');
        var scroll = wrapper && wrapper.querySelector(
          '.dt-scroll, .dataTables_scroll'
        );
        var scrollBody = wrapper && wrapper.querySelector(
          '.dt-scroll-body, .dataTables_scrollBody'
        );
        var scrollHeadInner = wrapper && wrapper.querySelector(
          '.dt-scroll-headInner, .dataTables_scrollHeadInner'
        );

        if (!wrapper || !scrollBody) {
          if (retries-- > 0) setTimeout(scheduleResize, 25);
          return;
        }
        var available = availableHeight(wrapper);
        var usesScroller = x.options && x.options.scroller;

        if (available > 0) {
          scrollBody.style.maxHeight = 'none';
          el.style.height = '100%';
          var framingHeight = wrapper.getBoundingClientRect().height -
            scrollBody.getBoundingClientRect().height;
          var targetHeight = Math.max(0, available - framingHeight);

          if (Math.abs(scrollBody.getBoundingClientRect().height -
                       targetHeight) > 1) {
            scrollBody.style.height = targetHeight + 'px';
          }
        } else if (!usesScroller) {
          scrollBody.style.maxHeight = 'none';
          el.style.height = 'auto';
          scrollBody.style.height = '';
        } else {
          el.style.height = 'auto';
        }

        wrapper.style.width = '100%';
        wrapper.style.maxWidth = '100%';
        if (scroll) {
          scroll.style.width = '100%';
          scroll.style.maxWidth = '';
        }
        scrollBody.style.width = '100%';
        scrollBody.style.maxWidth = '100%';
        scrollBody.style.overflowX = 'auto';
        if (scrollHeadInner) {
          scrollHeadInner.style.width = '';
          Array.prototype.forEach.call(
            scrollHeadInner.querySelectorAll('table'),
            function(table) {
              table.style.marginLeft = '';
              table.style.width = '';
            }
          );
        }
        Array.prototype.forEach.call(
          wrapper.querySelectorAll('.dt-scroll-body table, .dataTables_scrollBody table'),
          function(table) {
            table.style.width = '';
          }
        );

        try {
          var api = el._dt2;
          if (api && api.columns) api.columns.adjust();
        } catch (e) {}
      }

      function scheduleResize() {
        if (pending) return;
        pending = true;
        window.requestAnimationFrame(resizeTable);
      }

      if (el._dataquieRFillObserver) {
        el._dataquieRFillObserver.disconnect();
      }
      if (typeof window.ResizeObserver === 'function') {
        el._dataquieRFillObserver = new window.ResizeObserver(scheduleResize);
        el._dataquieRFillObserver.observe(el);
        if (el.parentElement) {
          el._dataquieRFillObserver.observe(el.parentElement);
        }
      }

      if (el._dataquieRFillResizeHandler) {
        window.removeEventListener('resize', el._dataquieRFillResizeHandler);
      }
      el._dataquieRFillResizeHandler = scheduleResize;
      window.addEventListener('resize', scheduleResize);
      scheduleResize();
    }
  ")
}
