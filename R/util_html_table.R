# nolint start: line_length_linter.
#' The jack of all trades device for tables
#'
#' @param ... passed to the selected DT2 or DT table backend
#' @param tb the table as [data.frame]
#' @param columnDefs column specifications for the `datatables` JavaScript
#'                   object
#' @param autoWidth passed to the `datatables` JavaScript library
#' @param hideCols columns to hide (by name)
#' @param rowCallback passed to the `datatables` JavaScript library
#'                    (with default)
#' @param copy_row_names_to_column add a column 0 with `rownames`
#' @param tb_rownames number of columns from the left considered as row-names
#' @param meta_data the data dictionary for labels and similar stuff
#' @param options individually overwrites DataTables options passed to the
#'                selected table backend
#' @param link_variables considering row names being variables, convert
#'                      row names to links to the variable specific reports
#' @param rotate_headers rotate headers by 90 degrees
#' @param colnames column names for the table (defaults to `colnames(tb)`)
#' @param is_matrix_table create a heat map like table without padding
#' @param infer_column_alignments [logical] infer numeric/date alignment from
#'   table values when no type metadata is attached.
#' @param filter column-filter position (`"top"`, `"bottom"`, or `"none"`)
#' @param colnames_aliases2acronyms abbreviate column names considering being
#'                                  analysis matrix columns by their acronyms
#'                                  defined in square.
#' @param label_col label col used for mapping labels in case of
#'                  `link_variables` is used (that argument set to `TRUE` and
#'                  `Variables` or `VAR_NAMES` in `meta_data`)
#' @param fillContainer [logical] fill the available widget container
#' @param fixed_header [logical] keep the table header visible while scrolling
#' @param dl_fn file name for downloaded table -- see
#'      [https://datatables.net/reference/button/excel](https://datatables.net/reference/button/excel)
#' @param cols_are_indicatormetrics [logical] cannot be `TRUE`,
#'        `colnames_aliases2acronyms` is `TRUE`. `cols_are_indicatormetrics`
#'        controls, if the columns are really function calls or, if
#'        `cols_are_indicatormetrics` has been set to `TRUE`, the columns are
#'        indicator metrics.
#' @param rotate_for_one_row [logical] rotate one-row-tables
#' @param descs [character] descriptions of the columns for the hover-box shown
#'                          for the column names, if not missing, this overrides
#'                          the existing description stuff from known column
#'                          names. If you have an attribute "description" of the `tb`, then it
#'                          overwrites everything and appears as hover text
#' @param title [character] title for download formats, see
#'        [https://datatables.net/extensions/buttons/examples/html5/titleMessage.html](https://datatables.net/extensions/buttons/examples/html5/titleMessage.html)
#' @param messageTop  [character] subtitle for download formats, see
#'        [https://datatables.net/extensions/buttons/examples/html5/titleMessage.html](https://datatables.net/extensions/buttons/examples/html5/titleMessage.html)
#' @param messageBottom  [character] footer for download formats, see
#'        [https://datatables.net/extensions/buttons/examples/html5/titleMessage.html](https://datatables.net/extensions/buttons/examples/html5/titleMessage.html)
#' @param col_tags [list] if not `NULL`, a named `list()`, names are names used
#'                        to name a newly created  column-group hide/show
#'                        button, elements are column names belonging to each
#'                        column groups as defined by `colnames`
#' @param searchBuilder [logical] if `TRUE`, display a `searchBuilder`-Button.
#' @param init_search [list] object to initialize `searchBuilder`, see [`datatables.net`](https://datatables.net/reference/type/SearchBuilder.Criteria)
#' @param initial_col_tag [character] `col_tags` entry to activate initially
#' @param additional_init_args [list] if not missing or `NULL`, arguments passed to `JavaScript`.
#' @param additional_columnDefs [list] additional `columnDefs`, can be missing or `NULL`
#' @param df_escape [logical] apply `util_df_escape()` to the table
#' @param ssi_link_target [character] link SSI computed variables to their
#'        role pages (`"role"`) or cross-item pages (`"cross_item"`).
#'
#' @return the table to be added to an `rmd`/´`html` file as
#'         [htmlwidgets::htmlwidgets]
#' @seealso `util_formattable()`
#'
#' @family summary_functions
#' @concept html
#' @noRd
#'
# nolint end
util_html_table <- function(tb,
  filter = "top",
  columnDefs = NULL,
  autoWidth = FALSE,
  hideCols = character(0),
  rowCallback = util_html_table_js(
    "function(r,d) {$(r).attr('height', '2em')}"
  ),
  copy_row_names_to_column = !is.null(tb) &&
    length(rownames(tb)) == nrow(tb) &&
    !is.integer(util_attr(tb, "row.names", exact = TRUE)) &&
    !all(seq_len(nrow(tb))
      == rownames(tb)),
  link_variables = TRUE,
  tb_rownames = FALSE,
  meta_data,
  meta_data_cross_item = NULL,
  rotate_headers = FALSE,
  fillContainer = TRUE,
  fixed_header = FALSE,
  ...,
  colnames,
  descs,
  options = list(),
  is_matrix_table = FALSE,
  colnames_aliases2acronyms = is_matrix_table &&
    !cols_are_indicatormetrics,
  cols_are_indicatormetrics = FALSE,
  label_col = LABEL,
  dl_fn = "*",
  rotate_for_one_row = FALSE,
  title = dl_fn,
  messageTop = NULL,
  messageBottom = NULL,
  col_tags = NULL,
  searchBuilder = FALSE,
  initial_col_tag,
  init_search,
  additional_init_args,
  additional_columnDefs,
  kv_table = !is.null(tb) && isTRUE(util_attr(tb, "kv_table",
      exact = TRUE
    )),
  df_escape = FALSE,
  ssi_link_target = c("role", "cross_item"),
  infer_column_alignments = TRUE) {
  # caveat: the fixed columns filter may not work.

  if (is.null(tb) || nrow(tb) == 0 || ncol(tb) == 0) {
    return()
  }

  force(copy_row_names_to_column)
  force(kv_table)

  html_table_backend <- util_html_table_backend(
    goal = "Generating nice tables"
  )
  use_dt2 <- identical(html_table_backend, "DT2")
  util_ensure_suggested("htmlwidgets", "Generating nice tables")

  util_expect_scalar(rotate_for_one_row, check_type = is.logical)
  util_expect_scalar(cols_are_indicatormetrics, check_type = is.logical)
  util_expect_scalar(colnames_aliases2acronyms, check_type = is.logical)
  util_expect_scalar(is_matrix_table, check_type = is.logical)
  util_expect_scalar(df_escape, check_type = is.logical)
  util_expect_scalar(infer_column_alignments, check_type = is.logical)
  util_expect_scalar(fixed_header, check_type = is.logical)
  ssi_link_target <- util_match_arg(ssi_link_target)

  util_stop_if_not(!(cols_are_indicatormetrics && colnames_aliases2acronyms))

  #  if we are currently being knit get our metadata from knitr
  if (missing(meta_data)) {
    if (isTRUE(getOption("knitr.in.progress")) &&
        exists("report", envir = knitr::knit_global())) {
      # https://stackoverflow.com/a/33121933
      meta_data <- knitr::knit_global()[["report"]][["meta_data"]]
    }
  }

  plain_label <- NULL
  try(plain_label <- util_attr(tb[["Variables"]], "plain_label", exact = TRUE),
    silent = TRUE
  )

  dt_filter_data <- NULL
  dt2_filter_data <- NULL
  if (use_dt2) {
    dt2_filter_data <- util_html_table_dt2_prepare_filter_data(
      tb,
      copy_row_names_to_column
    )
  } else if (df_escape) {
    dt_filter_data <- util_html_table_prepare_data(
      tb,
      rotate_for_one_row,
      copy_row_names_to_column,
      df_escape = FALSE
    )
  }

  tb <- util_html_table_prepare_data(tb,
    rotate_for_one_row,
    copy_row_names_to_column,
    df_escape = df_escape
  )
  if (!use_dt2 && !df_escape) {
    # The filter needs the prepared pre-linking values. With no escaping, the
    # display preparation above is exactly that same representation.
    dt_filter_data <- tb
  }

  if (link_variables && any(c("Variables", VAR_NAMES) %in%
        base::colnames(tb)) && !missing(meta_data)) {
    result <- util_html_table_link_variables(
      tb = tb,
      descs = descs,
      meta_data = meta_data,
      meta_data_cross_item = meta_data_cross_item,
      label_col = label_col,
      ssi_link_target = ssi_link_target
    )
    tb <- result[["tb"]]
    if (!is.null(dt_filter_data)) {
      dt_filter_data <- util_html_table_align_filter_data(dt_filter_data, tb)
    }
    if (!is.null(dt2_filter_data)) {
      dt2_filter_data <- util_html_table_align_filter_data(dt2_filter_data, tb)
    }
    if (!is.null(result[["descs"]])) {
      descs <- result[["descs"]]
    }
  }

  # rotate ----
  if (rotate_for_one_row && nrow(tb) == 1) {
    tb <- util_table_rotator(tb)
    if (!is.null(dt_filter_data)) {
      dt_filter_data <- util_table_rotator(dt_filter_data)
    }
    if (!is.null(dt2_filter_data)) {
      dt2_filter_data <- util_table_rotator(dt2_filter_data)
    }
    # Historical key-value-table rotation fallback removed in commit 214dd76a7d.
  }

  # column attributes ----
  if (missing(colnames)) {
    colnames <- base::colnames(tb)
  }

  readable_text_cols <- util_html_table_readable_text_cols(colnames(tb))

  if (is.null(columnDefs)) {
    columnDefs <- util_html_table_default_column_defs(colnames(tb), hideCols)
  }

  number_paren_cols <- util_attr(tb, "number_paren_cols", exact = TRUE)

  columnDefs <- c(
    columnDefs,
    list(
      list(
        targets = readable_text_cols - 1,
        className = "dt-wrap max_60vw dq-readable-text"
      ),
      list(
        targets = (setdiff(seq_len(ncol(tb)), which(colnames(tb) %in% hideCols))) - 1, # nolint: line_length_linter.
        render = util_html_table_js("sort_vert_dt")
      )
    )
  )

  fnames <-
    vapply(colnames,
      util_map_by_largest_prefix,
      haystack = names(.manual$titles),
      FUN.VALUE = character(1)
    )

  ftitles <-
    vapply(fnames, function(fn) {
      r <- util_alias2caption(fn, long = TRUE)
      if (length(r) != 1) r <- NA_character_
      r
    }, FUN.VALUE = character(1))

  ftitles[is.na(ftitles)] <- colnames[is.na(ftitles)]
  fnames[is.na(fnames)] <- colnames[is.na(fnames)]

  ftitles <- vapply(lapply(ftitles, htmltools::h3),
    as.character,
    FUN.VALUE = character(1)
  )

  coltitles <-
    vapply(colnames, function(cn) {
      r <- util_alias2caption(cn, long = TRUE)
      if (length(r) != 1) r <- NA_character_
      r
    }, FUN.VALUE = character(1))


  if (colnames_aliases2acronyms) {
    suffixes <-
      mapply(
        SIMPLIFY = FALSE, cn = colnames, fn = fnames,
        FUN = function(cn, fn) {
          if (startsWith(cn, fn)) {
            substr(cn, nchar(fn) + 1 + 1, nchar(cn)) # name + "_" (first +1), start is the next character (second +1) # nolint: line_length_linter.
          } else {
            cn
          }
        }
      )
    acronyms <-
      util_map_labels(fnames,
        util_get_concept_info("implementations"),
        to = "dq_report2_short_title",
        from = "function_R",
        ifnotfound = util_abbreviate(fnames)
      )
    suffixes <- gsub("_", " ", vapply(suffixes, as.character,
        FUN.VALUE = character(1)
      ))
    suffixes[!util_empty(suffixes)] <- paste0(
      ":",
      abbreviate(
        suffixes[!util_empty(suffixes)],
        minlength = 3
      )
    )
    acronyms <- paste0(acronyms, suffixes)
    names(acronyms) <- colnames
  } else if (cols_are_indicatormetrics) {
    # Historical raw-column acronym fallback removed here.
    acronyms <- util_translate_indicator_metrics(colnames,
      short = TRUE,
      long = FALSE
    )
  } else {
    acronyms <- colnames
    names(acronyms) <- colnames
  }

  if (cols_are_indicatormetrics) {
    fdescs <- rep("TODO", length(colnames))
  } else {
    fdescs <- vapply(fnames, FUN.VALUE = character(1), util_function_description) # nolint: line_length_linter.
  }

  if (missing(descs)) {
    if (!is.null(util_attr(tb, "description", exact = TRUE))) {
      descs <- util_attr(tb, "description", exact = TRUE)
      descs <- descs[colnames]
      descs[is.na(descs)] <- ""
      names(descs) <- colnames
    } else if (cols_are_indicatormetrics) {
      descs <- rep("TODO", length(colnames))
    } else {
      descs <- vapply(colnames, FUN.VALUE = character(1), util_col_description)
    }
  } else {
    util_expect_scalar(descs,
      allow_more_than_one = TRUE, check_type =
        is.character
    )
    util_stop_if_not(
      `Need one description per column` =
        length(descs) == length(colnames)
    )
  }

  acs <- acronyms[colnames]

  descs <- htmltools::htmlEscape(descs, attribute = TRUE)
  fdescs <- htmltools::htmlEscape(fdescs, attribute = TRUE)
  acs <- htmltools::htmlEscape(acs, attribute = TRUE)

  if (all(is.na(acs)) && all(colnames == "")) {
    acs <- colnames
  }

  cssClass <- if (rotate_headers) c("vertDT") else "myDT"

  if (rotate_headers && !searchBuilder) {
    cn <- paste0(
      "<div class=\"colheader\" colname=\"",
      coltitles,
      "\" title=\"",
      paste(coltitles, descs, sep = "<br />\n\n"),
      "\">",
      vapply(
        strsplit(
          acs,
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
  } else if (!searchBuilder) {
    cn <- paste0(
      "<span colname=\"",
      coltitles,
      "\" title=\"",
      paste(coltitles, descs, sep = "<br />\n\n"),
      "\">",
      acs,
      "</span>"
    )
  } else {
    cn <- paste(coltitles)
  }

  # these quotes are needed to prevent Excel from guessing
  # a title like "2020-01-01" may be a date to be displayed
  # as its integer representation

  quote_ifnotnull <- function(v, default = v) {
    if (!is.null(v)) {
      return(dQuote(v))
    } else {
      return(default)
    }
  }

  title <- quote_ifnotnull(title)
  messageTop <- quote_ifnotnull(messageTop, "-")
  messageBottom <- quote_ifnotnull(messageBottom)

  messageTop <- util_html_table_js(sprintf(
    'function() { return setMsgTop.call(this, "%s"); }',
    gsub('"', '\\"', messageTop, fixed = TRUE)
  ))

  col_tags_filter_buttons <- list()
  if (!is.null(col_tags)) {
    for (ct in names(col_tags)) {
      css_class <- util_attr(col_tags[[ct]], "cssClass", exact = TRUE)
      in_group <- colnames %in% col_tags[[ct]]
      col_tags_filter_buttons <- c(col_tags_filter_buttons, list(list(
        extend = "colvisGroup",
        className = paste("buttons-colvisGroup", css_class),
        text = ct,
        show = which(in_group) - 1,
        hide = which(!in_group) - 1,
        name = ct
      )))
    }
  }

  if (!missing(additional_columnDefs) && !is.null(additional_columnDefs) &&
      is.list(additional_columnDefs)) {
    columnDefs <- c(columnDefs, additional_columnDefs)
  }

  if (kv_table && ncol(tb) == 2) { # key-value-table, align to the center
    columnDefs <-
      c(
        list(
          list(className = "dt-right", targets = 0),
          list(className = "dt-left", targets = 1)
        ),
        columnDefs
      )
  } else {
    columnDefs <- c(
      list(list(className = "dt-left", targets = "_all")), # sane default
      if (infer_column_alignments) util_get_datatables_alignments(tb),
      columnDefs
    )
  }
  # This is only an internal hand-off from data preparation to alignment.
  # Do not serialize it with the widget input.
  attr(tb, "dataquieR_html_table_data_types") <- NULL
  attr(dt_filter_data, "dataquieR_html_table_data_types") <- NULL
  attr(dt2_filter_data, "dataquieR_html_table_data_types") <- NULL

  .options <- list(
    dom = "Bt",
    buttons = c(
      list(
        list(
          extend = "colvis",
          collectionLayout = "fixed columns",
          postfixButtons = list(
            "colvisRestore",
            '<input class="search_curr_colvis_input" onkeyup="search_curr_colvis()"></input>', # nolint: line_length_linter.
            '<button onclick="search_curr_colvis()">Toggle</button>'
          ),
          columnText = util_html_table_js("function (dt, idx, title) {
              return dt.column(idx).header().textContent;
            }")
        )
      ),
      col_tags_filter_buttons,
      util_make_export_buttons(dl_fn, title, messageBottom, messageTop)
    ),
    # https://github.com/rstudio/DT/issues/29
    columnDefs = columnDefs,
    autoWidth = autoWidth,
    rowCallback = rowCallback,
    autoFill = TRUE,
    scrollX = TRUE,
    scrollY = "55vh",
    scrollCollapse = TRUE,
    paging = FALSE,
    responsive = TRUE,
    fixedColumns = list(leftColumns = 1 + tb_rownames) # https://stackoverflow.com/a/51623663 # nolint: line_length_linter.
  )
  if (fixed_header) {
    .options[["fixedHeader"]] <- list(
      header = TRUE,
      headerOffset = 0
    )
  }

  is <- aia <- ict <- "null"
  number_paren_filter_cols <-
    which(colnames(tb) %in% number_paren_cols) - 1L + as.integer(isTRUE(tb_rownames)) # nolint: line_length_linter.
  if (length(number_paren_filter_cols)) {
    if (rlang::is_missing(additional_init_args) || is.null(additional_init_args)) { # nolint: line_length_linter.
      additional_init_args <- list()
    }
    additional_init_args[["number_paren_filter_cols"]] <-
      as.integer(number_paren_filter_cols)
  }

  if (searchBuilder) {
    result <- util_configure_search_builder(.options, init_search, additional_init_args) # nolint: line_length_linter.
    .options <- result[[".options"]]
    is <- result[["init_search"]]
    aia <- result[["additional_init_args"]]
  } else if (!rlang::is_missing(additional_init_args) &&
      !is.null(additional_init_args)) {
    util_ensure_suggested("jsonlite", goal = "initialize table filters")
    aia <- jsonlite::toJSON(additional_init_args, auto_unbox = TRUE)
  }

  if (!missing(initial_col_tag)) {
    ict <- sprintf('"%s"', initial_col_tag)
  }

  .options[names(options)] <- options

  if (is_matrix_table) {
    cssClass <- paste(cssClass, "matrixTable", collapse = " ")
  }

  ids <- as.character(floor(runif(2, min = 0, max = .Machine$integer.max)))
  pref <- vapply(nchar(.Machine$integer.max) - nchar(ids),
    function(n) {
      paste0(rep("0", n), collapse = "")
    },
    FUN.VALUE = character(1)
  )
  my_id <- paste0("id", paste0(pref, ids), collapse = "")

  if (prep_is_translated(colnames(tb))) {
    colnames(tb) <- as.character(colnames(tb))
  }

  tb[] <- lapply(tb, unname)

  if (use_dt2) {
    dt2_filter_position <- util_html_table_dt2_filter_position(filter)
    dt2_options <- util_html_table_dt2_normalize_options(
      .options,
      searchBuilder = searchBuilder,
      buttons = TRUE,
      filter = dt2_filter_position,
      data = dt2_filter_data
    )
    dt2_options[["initComplete"]] <- util_html_table_js(sprintf(
      "function(settings, json) { return(dataquieRdtCallback({ 'initialColTag': %s, 'initSearch': %s, 'additional_init_args': %s, 'table': this.api() })); }", # nolint: line_length_linter.
      ict, is, aia
    ))

    tb_dt2 <- tb
    colnames(tb_dt2) <- cn
    dtable <- util_html_table_dt2_widget(
      tb_dt2,
      elementId = my_id,
      class = cssClass,
      options = dt2_options,
      extensions = util_html_table_dt2_extensions(
        searchBuilder = searchBuilder,
        buttons = TRUE,
        fixedColumns = TRUE,
        fixed_header = fixed_header,
        responsive = TRUE,
        columnControl = !is.null(dt2_options[["columnControl"]]),
        scroller = isTRUE(dt2_options[["scroller"]])
      ),
      fillContainer = fillContainer
    )
  } else {
    dtable <- DT::datatable(tb,
      elementId = my_id,
      callback =
        util_html_table_js(sprintf("return(dataquieRdtCallback({ 'initialColTag': %s, 'initSearch': %s, 'additional_init_args': %s, 'table': table }));", ict, is, aia)), # nolint: line_length_linter.
      escape = FALSE,
      class = cssClass,
      filter = filter,
      rownames = tb_rownames,
      extensions = c(
        "FixedColumns", # https://stackoverflow.com/a/51623663
        if (fixed_header) "FixedHeader",
        "Buttons",
        "SearchBuilder",
        if (isTRUE(.options[["scroller"]])) "Scroller",
        if (searchBuilder) "DateTime"
      ),
      options = .options,
      colnames = cn,
      fillContainer = fillContainer,
      ...
    )
    dtable <- util_html_table_patch_filters(
      dtable = dtable,
      filter_data = dt_filter_data,
      filter = filter,
      rownames = tb_rownames,
      number_paren_cols = number_paren_cols
    )
  }

  if (is_matrix_table) {
    dtable <- htmlwidgets::onRender(
      dtable,
      "function(el, x) {
          if (!window.jQuery) return;
          var $ = window.jQuery;

          function findWrapper() {
            // el is usually inside the wrapper; if not, fall back to closest wrapper in parents
            var $el = $(el);
            var $w = $el.closest('.dataTables_wrapper');
            return $w.length ? $w : $el.parent().closest('.dataTables_wrapper');
          }

          function syncIn($root) {
            // copy computed bg from pre -> td, then clear pre bg
            $root.find('tbody td > pre').each(function () {
              var pre = this;
              var td = pre.parentNode;
              if (!td) return;

              var bg = window.getComputedStyle(pre).backgroundColor;
              if (!bg || bg === 'transparent' || bg === 'rgba(0, 0, 0, 0)') return;

              td.style.backgroundColor = bg;
              pre.style.backgroundColor = 'transparent';
            });
          }

          function syncAll() {
            var $w = findWrapper();
            if ($w && $w.length) {
              // all tables within this widget's wrapper (main + clones)
              syncIn($w);
            } else {
              // fallback: only within el
              syncIn($(el));
            }
          }

          // Try immediately (may be too early, but cheap)
          syncAll();

          // Wait until the actual DataTable is initialized, then hook events
          var tries = 0;
          (function hook() {
            tries++;

            var $w = findWrapper();
            var $main = $w && $w.length ? $w.find('table.dataTable').first() : $(el).find('table.dataTable').first();

            if ($main.length && $.fn.dataTable && $.fn.dataTable.isDataTable($main[0])) {
              // Sync now and on redraw-ish events
              syncAll();
              $main.on('draw.dt column-visibility.dt column-reorder.dt responsive-resize.dt', syncAll);
              return;
            }

            if (tries < 100) setTimeout(hook, 50); // ~5s max
          })();
       }"
    )
  }

  js <- sprintf("if (!window.dtConfig) {
                  window.dtConfig = {}
                }
                window.dtConfig['%s'] = %s;", my_id, aia)

  dtable <- htmlwidgets::prependContent(
    dtable,
    htmltools::tags$script(util_html_table_js(js))
  )

  jqui <- rmarkdown::html_dependency_jqueryui()
  jqui$stylesheet <- "jquery-ui.min.css"

  dtable$dependencies <- c(
    dtable$dependencies,
    list(
      rmarkdown::html_dependency_jquery(),
      jqui,
      html_dependency_report_table() # always, since sort_vert is indep. from vert heads # nolint: line_length_linter.
    )
  )

  # Historical utilHtmlTableInit onRender hook removed in commit 214dd76a7d.

  htmltools::tagList(
    htmltools::div(class = "table_top_spacer"),
    htmltools::div(class = "table_result", dtable),
    htmltools::br(style = "clear: both")
  )
}

#' Find table columns that need readable text layout
#'
#' @param column_names final table column names after any variable-link
#'   expansion
#'
#' @return 1-based column positions
#' @keywords internal
#' @noRd
util_html_table_readable_text_cols <- function(column_names) {
  which(column_names %in% c(VAR_NAMES, "Variables", "Labels", LABEL))
}

#' Default DataTables width definitions for the final table layout
#'
#' @param column_names final table column names after any variable-link
#'   expansion
#' @param hideCols columns to hide by name
#'
#' @return default `columnDefs` entries
#' @keywords internal
#' @noRd
util_html_table_default_column_defs <- function(column_names,
  hideCols = character(0)) {
  readable_text_cols <- util_html_table_readable_text_cols(column_names)
  variable_text_cols <- which(column_names %in% c(VAR_NAMES, "Variables"))
  label_text_cols <- which(column_names %in% c("Labels", LABEL))
  visible_cols <- setdiff(seq_along(column_names), which(column_names %in%
        hideCols))

  list(
    list(
      width = "5em",
      targets = setdiff(visible_cols, readable_text_cols) - 1
    ),
    list(
      width = "14em",
      targets = variable_text_cols - 1
    ),
    list(
      width = "24em",
      targets = label_text_cols - 1
    ),
    list(
      visible = FALSE,
      searchable = FALSE,
      targets = which(column_names %in% hideCols) - 1
    )
  )
}

#' Configure DT options for searchBuilder use
#'
#' @param .options  DT options
#' @param init_search util_html_table()
#' @param additional_init_args see util_html_table()
#'
#' @return a list containing each of the args altered to fit searchBuilder use
#' @keywords internal
#' @noRd
util_configure_search_builder <- function(.options, init_search, additional_init_args) { # nolint: line_length_linter.
  .options[["searchBuilder"]] <- TRUE
  .options[["search"]] <- list(return = TRUE)
  .options[["dom"]] <- paste0("Q", .options[["dom"]])

  jsl <- util_ensure_suggested("jsonlite",
    goal = "search filter init in tables",
    err = FALSE
  )

  is <- aia <- "null"

  # return early without init search and additional args if we
  # are unable to generate the JSON
  if (!jsl) {
    return(list(.options = .options, init_search = is, additional_init_args = aia)) # nolint: line_length_linter.
  }

  if (!rlang::is_missing(init_search)) {
    is <- jsonlite::toJSON(init_search, auto_unbox = TRUE)
  }
  if (!rlang::is_missing(additional_init_args)) {
    aia <- jsonlite::toJSON(additional_init_args, auto_unbox = TRUE)
  }

  list(.options = .options, init_search = is, additional_init_args = aia)
}

#' Patch DT filter controls from non-display data
#'
#' DT decides whether to build a range slider from the R column storage type.
#' The display table can contain escaped HTML, links, or compact `N (%)`
#' strings, so we rebuild the generated filter row from a separate filter-data
#' copy while keeping the display data untouched.
#'
#' @return a DT widget
#' @keywords internal
#' @noRd
util_html_table_patch_filters <- function(dtable,
  filter_data,
  filter,
  rownames,
  number_paren_cols) {
  if (is.null(filter) || identical(filter, "none")) {
    return(dtable)
  }
  if (is.list(filter) && identical(filter[["position"]], "none")) {
    return(dtable)
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    return(dtable)
  }
  if (is.null(filter_data)) {
    return(dtable)
  }

  column_filters <- utils::getFromNamespace("columnFilters", "DT")(filter_data)
  number_paren_idx <- which(names(filter_data) %in% number_paren_cols)
  for (idx in number_paren_idx) {
    range <- util_html_table_number_paren_range(filter_data[[idx]])
    if (is.null(range)) {
      next
    }
    column_filters[[idx]] <- list(
      control = "slider",
      type = "number",
      params = list(
        min = range[["min"]],
        max = range[["max"]],
        scale = range[["scale"]]
      ),
      disabled = !isTRUE(range[["enabled"]])
    )
  }

  filter_options <- if (is.list(filter)) filter else list(position = filter)
  filter_row <- utils::getFromNamespace("columnFilterRow", "DT")(
    column_filters,
    options = filter_options
  )
  if (isTRUE(rownames)) {
    filter_row$children[[1]][[1]] <- htmltools::tags$td("")
  }
  dtable$x$filterHTML <- htmltools::renderTags(filter_row)$html
  dtable
}

#' Keep separate filter data structurally aligned with display data
#'
#' Some display transformations, e.g. linking variables, add or reorder columns
#' after the non-display filter data has been prepared. DataTables requires the
#' generated filter header to match the final table width exactly.
#'
#' @return a data.frame with the same columns as `display_data`
#' @keywords internal
#' @noRd
util_html_table_align_filter_data <- function(filter_data, display_data) {
  if (is.null(filter_data) || identical(names(filter_data), names(display_data))) { # nolint: line_length_linter.
    return(filter_data)
  }

  strip_display <- function(values) {
    values <- as.character(values)
    filter_attr <- regmatches(
      values,
      regexec("\\bfilter=\"([^\"]*)\"", values, perl = TRUE)
    )
    values <- vapply(seq_along(values), function(idx) {
      match <- filter_attr[[idx]]
      if (length(match) >= 2L) {
        return(match[[2L]])
      }
      value <- gsub("<[^>]*>", "", values[[idx]], perl = TRUE)
      trimws(value)
    }, FUN.VALUE = character(1), USE.NAMES = FALSE)
    values
  }

  aligned <- lapply(names(display_data), function(col) {
    if (col %in% names(filter_data)) {
      return(filter_data[[col]])
    }
    strip_display(display_data[[col]])
  })
  names(aligned) <- names(display_data)
  aligned <- as.data.frame(aligned,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  attr(aligned, "number_paren_cols") <-
    util_attr(display_data, "number_paren_cols", exact = TRUE)
  aligned
}

#' @keywords internal
#' @noRd
util_html_table_number_paren_range <- function(values) {
  values <- util_html_table_parse_number_paren(values, value = "secondary")
  values <- values[is.finite(values)]
  if (!length(values)) {
    return(NULL)
  }
  min_value <- min(values)
  max_value <- max(values)
  list(
    min = min_value,
    max = max_value,
    scale = 0,
    enabled = is.finite(min_value) && is.finite(max_value) && max_value > min_value # nolint: line_length_linter.
  )
}

#' @keywords internal
#' @noRd
util_html_table_is_number_paren_column <- function(column) {
  if (!is.character(column)) {
    return(FALSE)
  }

  values <- gsub("<[^>]*>", "", column)
  values <- trimws(values)
  values <- values[!is.na(values) & nzchar(values)]
  if (!length(values)) {
    return(FALSE)
  }

  all(is.finite(util_html_table_parse_number_paren(values, value = "primary"))) &&
    all(is.finite(util_html_table_parse_number_paren(values, value = "secondary"))) # nolint: line_length_linter.
}

#' @keywords internal
#' @noRd
util_html_table_parse_number_paren <- function(values,
  value = c(
    "primary",
    "secondary"
  )) {
  value <- match.arg(value)
  values <- gsub("<[^>]*>", "", as.character(values))
  values <- trimws(values)
  pattern <- paste0(
    "^([-+]?[0-9]+(?:[\\.,][0-9]+)?)",
    "[[:space:]]*\\([[:space:]]*",
    "([-+]?[0-9]+(?:[\\.,][0-9]+)?)%?",
    "[[:space:]]*\\)$"
  )
  matches <- regexec(pattern, values, perl = TRUE)
  pieces <- regmatches(values, matches)
  vapply(pieces, function(piece) {
    if (length(piece) < 3L) {
      return(NA_real_)
    }
    piece_index <- if (identical(value, "primary")) 2L else 3L
    suppressWarnings(as.numeric(gsub(",", ".", piece[[piece_index]], fixed = TRUE)))
  }, FUN.VALUE = numeric(1))
}

#' Makes links from the variables in the table
#'
#' @param tb  the table
#' @param descs  descriptions
#' @param meta_data  see util_html_table()
#' @param meta_data_cross_item  see util_html_table()
#' @param label_col  see util_html_table()
#'
#' @return a list containing the table and, if applicable, descs
#' @keywords internal
#' @noRd
util_html_table_link_variables <- function(tb, descs, meta_data,
  meta_data_cross_item = NULL, label_col,
  ssi_link_target = c("role", "cross_item")) {
  ssi_link_target <- util_match_arg(ssi_link_target)
  is_html_escaped <- (identical(
    util_attr(tb, "is_html_escaped", exact = TRUE),
    TRUE
  ))
  description <- util_attr(tb, "description", exact = TRUE)
  number_paren_cols <- util_attr(tb, "number_paren_cols", exact = TRUE)

  Variables <- intersect(base::colnames(tb), c("Variables", VAR_NAMES))
  Variables <- head(Variables, 1)
  plain_label <- util_attr(tb[[Variables]], "plain_label", exact = TRUE)

  from <- if (is.null(plain_label)) tb[[Variables]] else plain_label

  pretty_lb <- prep_get_labels(
    resp_vars = from,
    item_level = meta_data,
    label_col = label_col,
    label_class = "LONG",
    resp_vars_are_var_names_only = Variables == VAR_NAMES,
    resp_vars_match_label_col_only = Variables == "Variables"
  )

  if (Variables == VAR_NAMES &&
      all(c(VAR_NAMES, label_col) %in% names(meta_data))) {
    tb[[Variables]] <- util_map_labels(
      from,
      meta_data = meta_data,
      to = label_col,
      from = VAR_NAMES,
      ifnotfound = from
    )
  }

  map_label_col_to_if_exists <- function(to, default) {
    if (to %in% names(meta_data)) {
      util_map_labels(
        from,
        meta_data = meta_data,
        to = to,
        from = label_col,
        ifnotfound = from
      )
    } else {
      default
    }
  }

  vn <- map_label_col_to_if_exists(VAR_NAMES, default = from)
  lb <- map_label_col_to_if_exists(LABEL, default = from)
  llb <- map_label_col_to_if_exists(LONG_LABEL, default = from)

  if (Variables == VAR_NAMES) {
    href <- tb[[Variables]]
    data <- vn
    hover_title <- pretty_lb
  } else {
    href <- from
    data <- pretty_lb
    hover_title <- vn
  }

  .filter <- data

  href <- paste0(
    "VAR_",
    prep_link_escape(href, html = TRUE),
    ".html#",
    htmltools::urlEncodePath(prep_link_escape(as.character(href)))
  )

  ssi_href <- util_ssi_computed_variable_hrefs(
    vn,
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item
  )
  has_ssi_href <- !is.na(ssi_href) & nzchar(ssi_href)
  href[has_ssi_href] <- ssi_href[has_ssi_href]
  if (identical(ssi_link_target, "cross_item")) {
    ssi_cross_item_links <- util_ssi_computed_variable_cross_item_links(
      vn,
      meta_data = meta_data,
      meta_data_cross_item = meta_data_cross_item
    )
    has_cross_item_href <- !is.na(ssi_cross_item_links$href) &
      nzchar(ssi_cross_item_links$href)
    href[has_cross_item_href] <- ssi_cross_item_links$href[has_cross_item_href]
    data[has_cross_item_href] <- ssi_cross_item_links$label[has_cross_item_href]
    .filter[has_cross_item_href] <-
      ssi_cross_item_links$label[has_cross_item_href]
  }

  # if we have a plain_label attribute, we should consider
  # the entries in the Variables column HTML
  if (!is.null(plain_label)) {
    data <- lapply(tb[[Variables]], htmltools::HTML)
    hover_title <- prep_title_escape(plain_label, html = TRUE)
  } else {
    data <- prep_title_escape(data, html = TRUE)
    hover_title <- prep_title_escape(hover_title, html = TRUE)
  }

  onclick <- rep(
    "if (window.__dqPersistPopupHistory) window.__dqPersistPopupHistory();",
    length(href)
  )

  mk_hover_text <- function(hover_title, vn, lb, llb) {
    .m <- NULL
    try(
      {
        .m <- meta_data[meta_data[[VAR_NAMES]] == vn, , drop = FALSE]
        .m <- data.frame(
          fix.empty.names = FALSE,
          check.names = FALSE,
          ` ` = unlist(.m[1, , drop = TRUE])
        )
        .m <- .m[!is.na(.m[[1]]), , drop = FALSE]
        .m <- head(.m, 5)
        .m <- withr::with_options(
          list(knitr.kable.NA = ""),
          htmltools::HTML(
            knitr::kable(
              .m,
              "html"
            )
          )
        )
      },
      silent = TRUE
    )
    htmltools::div(
      style = htmltools::css(
        overflow_y = "scroll",
        max_height = "80vh"
      ),
      htmltools::h5(hover_title),
      htmltools::tags$ul(
        htmltools::tags$li(vn),
        htmltools::tags$li(lb),
        htmltools::tags$li(llb)
      ),
      .m
    )
  }

  # prep_get_labels() or l18n
  hover_text <- mapply(
    hover_title = hover_title,
    vn = vn,
    lb = lb,
    llb = llb,
    SIMPLIFY = FALSE,
    FUN = mk_hover_text
  )

  links <- mapply(
    href = href,
    target = "_top",
    onclick = onclick,
    title = hover_text,
    filter = .filter,
    data,
    SIMPLIFY = FALSE,
    FUN = htmltools::a
  )


  tb[[Variables]] <- (vapply(links,
      FUN = as.character,
      FUN.VALUE = character(1)
    ))

  var_col <- which(base::colnames(tb) == "Variables")
  if (length(var_col) == 1) {
    ..left <- seq_len(var_col - 1)
    ..right <- var_col + seq_len(ncol(tb) - var_col)
    tb <- cbind(tb[..left],
      Variables = vn,
      Labels = tb[["Variables"]],
      tb[..right]
    )

    if (!rlang::is_missing(descs)) {
      if (is.null(names(descs))) {
        descs <- c(
          descs[..left],
          "Variables",
          "Labels",
          descs[..right]
        )
      } else {
        descs <- c(descs[..left],
          Variables = "Variables",
          Labels = "Labels",
          descs[..right]
        )
      }
    }
  }

  if (rlang::is_missing(descs)) descs <- NULL

  attr(tb, "is_html_escaped") <- is_html_escaped
  attr(tb, "description") <- description
  attr(tb, "number_paren_cols") <- number_paren_cols
  attr(tb[[Variables]], "plain_label") <- plain_label

  list(tb = tb, descs = descs)
}

#' Prepares a table for use in util_html_table()
#'
#' @param tb   the table
#' @param rotate_for_one_row   see util_html_table()
#' @param copy_row_names_to_column  see util_html_table()
#' @param df_escape see util_html_table()
#'
#' @return the table
#' @keywords internal
#' @noRd
util_html_table_prepare_data <- function(tb,
  rotate_for_one_row,
  copy_row_names_to_column,
  df_escape) {
  class(tb) <- setdiff(class(tb), "dataquieR_result")

  is_html_escaped <- (identical(
    util_attr(tb, "is_html_escaped", exact = TRUE),
    TRUE
  ))
  description <- util_attr(tb, "description", exact = TRUE)
  plain_label <- NULL
  try(plain_label <- util_attr(tb[["Variables"]], "plain_label", exact = TRUE),
    silent = TRUE
  )

  if (is.matrix(tb)) {
    tb <- as.data.frame.matrix(tb, stringsAsFactors = FALSE)
  }

  # convert logical columns to character strings
  # Find all columns featuring any sort of logical also "TRUE" or so, and
  # convert it to logicals

  ts <- c("t", "true", "1", "+")
  fs <- c("f", "false", "0", "-")
  nas <- c("", "na", NA_character_)
  lgs <- unique(c(ts, fs, nas))

  is_log_col <- function(cl) {
    if (is.logical(cl)) {
      return(TRUE)
    }
    ..dt <- util_attr(cl, DATA_TYPE, exact = TRUE)
    if (!is.null(..dt) && !any(is.na(..dt))) {
      if (trimws(tolower(..dt)) %in% c("logical", "bool", "boolean")) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
    all(
      trimws(tolower(as.character(cl))) %in% lgs
    )
  }

  as_log_col <- function(cl) {
    if (!is.logical(cl)) {
      clc <- trimws(tolower(as.character(cl)))
      r <- logical(length(cl))
      r[clc %in% ts] <- TRUE
      r[clc %in% fs] <- FALSE
      r[clc %in% nas] <- NA # not really needed
      cl <- r
    }
    cl
  }

  log_cols <- vapply(tb, is_log_col, FUN.VALUE = logical(1))

  tb[, log_cols] <-
    vapply(tb[, log_cols, drop = FALSE],
      as_log_col,
      FUN.VALUE = logical(nrow(tb))
    )

  # round all numerical columns and convert to strings
  dtypes <- prep_datatype_from_data(tb, guess_character = TRUE)
  dtypes2 <- lapply(tb, util_attr, DATA_TYPE, exact = TRUE)
  if (any(vapply(dtypes2, length, FUN.VALUE = integer(1)) == 1)) {
    for (cl in names(dtypes2)) {
      dt2 <- dtypes2[[cl]]
      if (!is.null(dt2) && !any(is.na(dt2))) {
        dtypes[[cl]] <- dt2
      }
    }
  }
  fltcols <- !log_cols & dtypes %in% c(DATA_TYPES$FLOAT)
  flts_still_char <-
    vapply(tb[, fltcols, drop = FALSE], function(x) {
      x <- util_dt_normalize_cell_text(x, keep_first_number = TRUE)
      util_round_to_decimal_places(as.numeric(x))
    },
    FUN.VALUE = character(nrow(tb))
    )
  tb[, fltcols] <-
    as.numeric(ifelse(trimws(flts_still_char) == "NA", NA, trimws(flts_still_char)))
  intcols <- !log_cols & dtypes %in% c(DATA_TYPES$INTEGER)
  tb[, intcols] <- vapply(tb[, intcols, drop = FALSE], function(x) {
    if (all(suppressWarnings(!is.na(x) & !is.na(as.logical(x)) & is.na(as.integer(x))))) { # nolint: line_length_linter.
      x <- suppressWarnings(as.logical(x))
    }
    as.integer(x)
  }, FUN.VALUE = integer(nrow(tb)))

  number_paren_cols <- !log_cols & (
    dtypes %in% DATA_TYPE_NUMBER_PAREN |
      vapply(tb, util_html_table_is_number_paren_column, FUN.VALUE = logical(1))
  )
  number_paren_col_names <- names(tb)[number_paren_cols]
  txtcols <- !log_cols & dtypes %in% c(DATA_TYPES$STRING)
  if ("Variables" %in% names(txtcols)) {
    txtcols[["Variables"]] <- FALSE
  }
  if (VAR_NAMES %in% names(txtcols)) {
    txtcols[[VAR_NAMES]] <- FALSE
  }
  tb[, txtcols] <- lapply(tb[, txtcols, drop = FALSE], function(cl) {
    if (is.list(cl) && all(vapply(cl, length, FUN.VALUE = integer(1)) == 1L) &&
        all(vapply(lapply(cl, class), length, FUN.VALUE = integer(1)) == 1L) &&
        all(vapply(cl, is.character, FUN.VALUE = logical(1)))) {
      cl <- unlist(cl)
    }
    if (!is.list(cl) && !is.factor(cl) && length(unique(cl)) <= 10) {
      if (!any(grepl("<\\s*[a-zA-Z][^>]*>", cl, perl = TRUE))) {
        try(cl <- as.factor(cl), silent = TRUE)
      }
    }
    cl
  })

  if (copy_row_names_to_column) {
    tb <- cbind.data.frame(data.frame(
      Variables = rownames(tb),
      stringsAsFactors = FALSE
    ), tb)
    rownames(tb) <- NULL
    dtypes <- c(Variables = DATA_TYPES$STRING, dtypes)
  }

  attr(tb, "is_html_escaped") <- is_html_escaped
  attr(tb, "description") <- description
  attr(tb[["Variables"]], "plain_label") <- plain_label
  attr(tb, "number_paren_cols") <- number_paren_col_names

  if (df_escape) {
    tb <- util_df_escape(tb)
  }
  attr(tb, "dataquieR_html_table_data_types") <- dtypes

  tb
}


#' Creates extra button definitions for datatables
#'
#' @param dl_fn  download file name
#' @param title  title of all buttons
#' @param messageBottom  bottom message
#' @param messageTop  top message
#'
#' @return list of button definitions for the DT lib
#' @keywords internal
#' @noRd
util_make_export_buttons <- function(dl_fn, title, messageBottom, messageTop) {
  exportHeader <- util_html_table_js("function (data, columnIdx) {
                           var text = data == null ? \"\" : data + \"\";
                           if (!/^\\s*</.test(text)) {
                             return text;
                           }
                           var wrapper = $('<div/>').html(text);
                           var colname = wrapper.find('[colname]').first().attr('colname');
                           if (colname != null && colname !== '') {
                             return colname;
                           }
                           var label = wrapper.text().trim();
                           return label !== '' ? label : text;
                         }")

  mkFormatBody <- function(extend, extraJS = "") {
    util_html_table_js(paste0(
      "function (data, row, column, node) {
                      r = data + \"\";
                      if (r.match(/\"data:image\\//g)) {
                        r = \"not supported in ", extend, "\"
                      }
                      if (r.match(/<table *>/g)) {
                        r = \"not supported in ", extend, "\"
                      }
                      r = r.replaceAll(/\\<br *\\/?\\>/g,\"  \");",
      extraJS, "
                      return r;
                    }"
    ))
  }

  extraJS <- "r = $('<div />').append(r).text().trim();
              if (isNumeric(r.replaceAll(/\\s/g, ''))) {
                r = r.replaceAll(/\\s/g, '');
              }"

  mkExportOpts <- function(
    extend,
    extraJS = "",
    orthogonal = "filter",
    rows = util_html_table_js("rowFilter"),
    columns = ":visible",
    formatBody = mkFormatBody(extend, extraJS),
    formatHeader = exportHeader,
    extraFormat = list(),
    ...
  ) {
    c(list(
      orthogonal = orthogonal,
      rows = rows,
      columns = columns,
      format = c(list(
        body = force(formatBody),
        header = force(formatHeader)
      ), extraFormat)
    ), list(...))
  }

  mkButton <- function(
    extend,
    .title = title,
    .messageTop = messageTop,
    .messageBottom = messageBottom,
    exportOptions = mkExportOpts(extend),
    ...
  ) {
    c(list(
      extend = extend,
      title = .title,
      messageTop = .messageTop,
      messageBottom = .messageBottom,
      exportOptions = exportOptions
    ), list(...))
  }

  printBody <- util_html_table_js(
    "function (data) {
      return data == null ? \"\" : data + \"\";
    }"
  )

  list(
    mkButton(extend = "copy"),
    mkButton(
      extend = "excel",
      filename = dl_fn,
      customize = util_html_table_js("customize_excel"),
      exportOptions = mkExportOpts(
        extend = "excel",
        orthogonal = "display",
        extraJS = extraJS
      ),
      autoFilter = TRUE
    ),
    mkButton(
      extend = "csv",
      filename = dl_fn
    ),
    mkButton(
      extend = "pdf",
      filename = dl_fn,
      orientation = "landscape",
      customize = util_html_table_js("customize_pdf"),
      exportOptions = mkExportOpts(
        extend = "pdf",
        orthogonal = "display",
        extraJS = extraJS
      )
    ),
    mkButton(
      extend = "print",
      message = dl_fn,
      autoPrint = TRUE,
      customize = util_html_table_js("dqCustomizePrint"),
      exportOptions =
        mkExportOpts(
          stripHtml = FALSE,
          orthogonal = "display",
          formatBody = printBody
        )
    )
  )
}


#' Internal helper: dt normalize cell text
#'
#' @noRd
util_dt_normalize_cell_text <- function(x, keep_first_number = TRUE) {
  x <- trimws(gsub("&nbsp;", " ",
      gsub("(?is)<[^>]+>", "",
        gsub("(?is)<br\\s*/?>", "\n", as.character(x), perl = TRUE),
        perl = TRUE
      ),
      fixed = TRUE
    ))

  # Treat trailing percent as numeric-ish (e.g., "30%" -> "30")
  x <- sub("\\s*%\\s*$", "", x)

  if (!keep_first_number) {
    return(x)
  }

  num_pat <- "[-+]?((\\d{1,3}([ ,]\\d{3})+)|(\\d+))(\\.\\d+)?([eE][-+]?\\d+)?|[-+]?\\.\\d+([eE][-+]?\\d+)?" # nolint: line_length_linter.
  m <- regexpr(num_pat, x, perl = TRUE)
  trimws(ifelse(m > 0, regmatches(x, m), x))
}

#' Internal helper: get datatables alignments
#'
#' @noRd
util_get_datatables_alignments <- function(
  tb,
  prefer_right_for_datetime = TRUE,
  right_types = c(
    DATA_TYPES$INTEGER, DATA_TYPES$FLOAT,
    DATA_TYPE_NUMBER_PAREN
  ),
  center_types = DATA_TYPE_LOGICAL,
  ...
) {
  util_stop_if_not("`tb` must be a data frame" = is.data.frame(tb))
  known_types <- util_attr(tb, "dataquieR_html_table_data_types", exact = TRUE)

  Map(function(col, j) {
    align <- if (is.integer(col) || is.numeric(col)) {
      "dt-right"
    } else if (inherits(col, c("POSIXct", "POSIXt", "Date"))) {
      if (prefer_right_for_datetime) "dt-right" else "dt-center"
    } else if (is.logical(col)) {
      "dt-center"
    } else {
      dtype <- util_attr(col, DATA_TYPE, exact = TRUE)
      if (is.null(dtype) && !is.null(known_types) &&
          length(known_types) >= j && !is.na(known_types[[j]])) {
        dtype <- known_types[[j]]
      }
      if (is.null(dtype) || any(is.na(dtype))) {
        x <- util_dt_normalize_cell_text(col, TRUE)
        x <- x[!is.na(x) & x != ""]
        dtype <- if (!length(x)) {
          DATA_TYPES$INTEGER
        } else {
          prep_robust_guess_data_type(x, ...)
        }
      }
      if (dtype %in% right_types) {
        "dt-right"
      } else if (dtype %in% center_types) {
        "dt-center"
      } else if (dtype %in% c(DATA_TYPES$DATETIME, DATA_TYPES$TIME)) {
        if (prefer_right_for_datetime) "dt-right" else "dt-center"
      } else {
        "dt-left"
      }
    }
    list(className = align, targets = j - 1L)
  }, unname(tb), seq_along(tb))
}
