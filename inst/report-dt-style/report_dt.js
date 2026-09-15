 // https://stackoverflow.com/a/8747204
jQuery.expr[':'].icontains = function(a, i, m) {
  return jQuery(a).text().toUpperCase()
      .indexOf(m[3].toUpperCase()) >= 0;
};

window.dtConfig = {}

var dqLazyDashboardImageObserver = null;
var dqDeferredColumnFilterCounter = 0;

function dqLoadLazyDashboardImage(image) {
  var source = image.getAttribute("data-dq-lazy-src");
  if (!source) return;
  image.setAttribute("src", source);
  image.removeAttribute("data-dq-lazy-src");
  image.removeAttribute("data-dq-lazy-observed");
}

function dqInitLazyDashboardImages(root, refresh) {
  root = root || document;
  var images = root.querySelectorAll("img[data-dq-lazy-src]");
  if (!("IntersectionObserver" in window)) {
    Array.prototype.forEach.call(images, dqLoadLazyDashboardImage);
    return;
  }
  if (!dqLazyDashboardImageObserver) {
    dqLazyDashboardImageObserver = new IntersectionObserver(function(entries) {
      entries.forEach(function(entry) {
        if (!entry.isIntersecting) return;
        dqLoadLazyDashboardImage(entry.target);
        dqLazyDashboardImageObserver.unobserve(entry.target);
      });
    }, { rootMargin: "300px 0px" });
  }
  Array.prototype.forEach.call(images, function(image) {
    if (image.hasAttribute("data-dq-lazy-observed")) {
      if (!refresh) return;
      dqLazyDashboardImageObserver.unobserve(image);
    }
    image.setAttribute("data-dq-lazy-observed", "true");
    dqLazyDashboardImageObserver.observe(image);
  });
}

$(function() {
  dqInitLazyDashboardImages(document);
  $(document).on(
    "draw.dt column-visibility.dt responsive-resize.dt",
    function(event) {
      var root = event.target || document;
      window.requestAnimationFrame(function() {
        dqInitLazyDashboardImages(root, true);
      });
    }
  );
  $(document).on("scroll", ".dt-scroll-body", function() {
    var root = this;
    window.requestAnimationFrame(function() {
      dqInitLazyDashboardImages(root, true);
    });
  });
});

function dataquieRBuildDeferredColumnFilters(table, rowId) {
  var header = $(table.table().header());
  header.find("tr.dq-deferred-column-filters").remove();
  var row = $("<tr class='dq-deferred-column-filters'></tr>")
    .attr("id", rowId);
  table.columns(":visible").every(function() {
    var column = this;
    var columnIndex = column.index();
    var searchTimer = null;
    var label = $(column.header()).text().replace(/\s+/g, " ").trim();
    var input = $("<input type='search' autocomplete='off'>")
      .attr("data-dq-column-index", columnIndex)
      .attr("aria-label", "Filter " + label)
      .attr("placeholder", label)
      .val(column.search())
      .on("input", function() {
        var source = this;
        var value = this.value;
        window.clearTimeout(searchTimer);
        searchTimer = window.setTimeout(function() {
          if (column.search() !== value) {
            var selectionStart = source.selectionStart;
            var selectionEnd = source.selectionEnd;
            column.search(value, false, true).draw();
            var replacement = $(table.table().header()).find(
              "input[data-dq-column-index='" + columnIndex + "']"
            ).first();
            if (replacement.length && replacement.get(0) !== source) {
              replacement.trigger("focus");
              replacement.get(0).setSelectionRange(
                selectionStart,
                selectionEnd
              );
            }
          }
        }, 250);
      });
    $("<th></th>").append(input).appendTo(row);
  });
  header.prepend(row);
  return row;
}

function dataquieRSetButtonText(button, text) {
  var span = $(button).children("span").first();
  if (span.length) {
    span.text(text);
  } else {
    $(button).text(text);
  }
}

function dataquieRAppendButtonGroup(toolbar, nodes, className, label) {
  if (!nodes.length) return;
  if (toolbar.children().length) {
    $("<span class='dq-table-tool-separator' role='separator'></span>")
      .appendTo(toolbar);
  }
  nodes
    .addClass(className)
    .attr("data-dq-tool-group", label)
    .appendTo(toolbar);
}

function dataquieRInitDeferredColumnFilters(config) {
  var cfg = config.additional_init_args || {};
  if (!cfg.deferred_column_filters) return;
  var container = $(config.table.table().container());
  if (container.data("dqDeferredColumnFilters")) return;
  container.data("dqDeferredColumnFilters", true);
  var toolbar = $(config.table.buttons().container()).first();
  var buttons = toolbar.children().detach();
  var exportButtons = buttons.filter(
    ".buttons-copy, .buttons-excel, .buttons-csv, " +
    ".buttons-pdf, .buttons-print"
  );
  var visibilityButtons = buttons.filter(".buttons-colvis");
  var filterButtons = buttons.not(exportButtons).not(visibilityButtons);
  filterButtons.each(function() {
    var text = $(this).text().trim().toLowerCase();
    if (text === "init") dataquieRSetButtonText(this, "Initial columns");
    if (text === "all") dataquieRSetButtonText(this, "All columns");
  });
  var rowId = "dq-column-filters-" + (++dqDeferredColumnFilterCounter);
  var checkbox = $("<input type='checkbox'>")
    .attr("aria-controls", rowId)
    .on("change", function() {
      var row = $(config.table.table().header()).find("#" + rowId);
      if (this.checked) {
        row = dataquieRBuildDeferredColumnFilters(config.table, rowId);
        row.show();
      } else {
        row.hide();
      }
    });
  var checkboxLabel = $("<label class='dq-column-filter-toggle'></label>")
    .append(checkbox)
    .append(document.createTextNode(" Column filters"));
  dataquieRAppendButtonGroup(
    toolbar,
    visibilityButtons,
    "dq-column-visibility-tools",
    "Column visibility"
  );
  dataquieRAppendButtonGroup(
    toolbar,
    checkboxLabel,
    "dq-column-filter-toggle-tools",
    "Column filters"
  );
  dataquieRAppendButtonGroup(
    toolbar,
    filterButtons,
    "dq-content-filter-tools",
    "Content filters"
  );
  dataquieRAppendButtonGroup(
    toolbar,
    exportButtons,
    "dq-export-tools",
    "Export"
  );
  $(config.table.table().node()).on(
    "column-visibility.dt.dqDeferredColumnFilters",
    function() {
      if (checkbox.prop("checked")) {
        dataquieRBuildDeferredColumnFilters(config.table, rowId);
      }
    }
  ).on(
    "draw.dt.dqDeferredColumnFilters",
    function() {
      if (!checkbox.prop("checked")) return;
      var row = $(config.table.table().header()).find("#" + rowId);
      if (!row.length) {
        dataquieRBuildDeferredColumnFilters(config.table, rowId);
      }
    }
  );
}

function dqGetDataTableApi(rootEl) {
  if (!rootEl) return null;

  try {
    if (rootEl.table && typeof rootEl.table === "function") return rootEl;
  } catch (e) {}

  var $root = $(rootEl);
  var candidates = [];
  if ($root.is("table")) {
    candidates.push($root.get(0));
  }
  $root.find("table").each(function () {
    candidates.push(this);
  });
  candidates.push(rootEl);

  for (var i = 0; i < candidates.length; i++) {
    var node = candidates[i];
    if (!node) continue;

    if (node._dt2) return node._dt2;

    var $node = $(node);
    var api = $node.data("dt-api") || $node.data("DataTable");
    if (api) return api;

    try {
      if ($.fn.dataTable && $.fn.dataTable.isDataTable(node)) {
        return $node.DataTable();
      }
    } catch (e) {}
  }

  return null;
}

search_curr_colvis = function(event) {
  var evt=event||window.event;
  var cur_colvis = $(evt.target).parent();
  var input_el = cur_colvis.find("input.search_curr_colvis_input")
  var search_text = $(input_el).val();
  cur_colvis.find("button.buttons-columnVisibility").css("backgroundColor", "");
  cur_colvis.find("button.buttons-columnVisibility:icontains(" + search_text + ")").css(
    "backgroundColor", "#cccc55");
  if (evt instanceof KeyboardEvent) {
    if (evt.key == "Enter") {
      cur_colvis.find("button.buttons-columnVisibility:icontains(" + search_text + ")").click()
    }
  } else if (evt instanceof PointerEvent || evt instanceof MouseEvent) {
    cur_colvis.find("button.buttons-columnVisibility:icontains(" + search_text + ")").click()
  }
}

// https://stackoverflow.com/a/77266386
is_html = function(content) {
    let elem = document.createElement('p');
    elem.innerHTML = content;
    let res=elem.children.length > 0;
    elem = null;
    return(res);
}

escape_html = function(content) {
  var text = $("<div/>").html(content).text();
  return text;
}

function isNumeric(str) {
  str = str.replaceAll(/\s+/g, "")
  return str == "NaN" || !isNaN(Number(str))
}

function dqDataTableCellAttributes(data) {
  var string = data == null ? "" : data + "";
  var regex = new RegExp('[\\s\\r\\t\\n]*([a-z0-9\\-_]+)[\\s\\r\\t\\n]*=[\\s\\r\\t\\n]*([\'"])((?:\\\\\\2|(?!\\2).)*)\\2', 'ig');
  var attributes = {};
  var match;
  while ((match = regex.exec(string))) {
    attributes[match[1]] = match[3];
  }
  return attributes;
}

function dqDataTablePlainValue(data) {
  if (data == null) {
    return "";
  }
  if (is_html(data)) {
    return escape_html(data);
  }
  return data;
}

function dqDataTableParsedNumber(value) {
  if (value == null) {
    return null;
  }
  value = (value + "").trim();
  if (value === "") {
    return null;
  }
  value = value
    .replace(/\s+/g, "")
    .replace(/%$/, "")
    .replace(",", ".");
  if (!isNumeric(value)) {
    return null;
  }
  return Number(value);
}

function dqDataTableNumberParenValue(data) {
  var text = dqDataTablePlainValue(data);
  var number_paren = new RegExp(
    "^\\s*([-+]?[0-9]+(?:[\\.,][0-9]+)?)\\s*" +
    "\\(\\s*([-+]?[0-9]+(?:[\\.,][0-9]+)?)\\s*%?\\s*\\)\\s*$"
  );
  var match = number_paren.exec(text);
  if (!match) {
    return null;
  }

  var primary = dqDataTableParsedNumber(match[1]);
  var secondary = dqDataTableParsedNumber(match[2]);
  if (primary == null || secondary == null) {
    return null;
  }

  return {
    primary: primary,
    secondary: secondary,
    text: text
  };
}

function dqDataTableSearchValue(data) {
  var attributes = dqDataTableCellAttributes(data);
  var searchAttributes = ["filter", "search", "data-filter", "data-search"];
  for (var i = 0; i < searchAttributes.length; i++) {
    if (attributes.hasOwnProperty(searchAttributes[i])) {
      return attributes[searchAttributes[i]];
    }
  }

  var parsed = dqDataTableNumberParenValue(data);
  if (parsed != null) {
    return parsed.secondary + "";
  }

  return dqDataTablePlainValue(data);
}

function dqDataTableGradingValue(data) {
  var attributes = dqDataTableCellAttributes(data);
  var gradingAttributes = [
    "grading", "grade", "class", "data-grading", "data-grade",
    "data-class", "filter", "search", "data-filter", "data-search",
    "sort", "order", "data-sort", "data-order"
  ];
  for (var i = 0; i < gradingAttributes.length; i++) {
    if (attributes.hasOwnProperty(gradingAttributes[i])) {
      return attributes[gradingAttributes[i]] + "";
    }
  }

  if (data != null && is_html(data)) {
    return $("<div/>").html(data).text().trim();
  }
  return dqDataTablePlainValue(data) + "";
}

function dqDataTableFilterLabel(data) {
  if (data == null) {
    return "";
  }
  if (!is_html(data)) {
    return data;
  }

  var $wrapper = $("<div/>").html(data);
  $wrapper.find("script, style, iframe, object, embed, form, input, button, select, textarea").remove();
  $wrapper.find("*").each(function() {
    var attrs = Array.prototype.slice.call(this.attributes || []);
    for (var i = 0; i < attrs.length; i++) {
      var name = attrs[i].name;
      if (/^on/i.test(name) ||
          ["href", "target", "src", "srcset", "action"].indexOf(name.toLowerCase()) >= 0) {
        this.removeAttribute(name);
      }
    }
  });
  return $wrapper.html();
}

function dqDataTableSortValue(data) {
  var attributes = dqDataTableCellAttributes(data);
  var sortAttributes = ["sort", "order", "data-sort", "data-order"];
  for (var i = 0; i < sortAttributes.length; i++) {
    if (attributes.hasOwnProperty(sortAttributes[i])) {
      var sortValue = attributes[sortAttributes[i]];
      return isNumeric(sortValue) ? Number(sortValue) : sortValue;
    }
  }

  var parsed = dqDataTableNumberParenValue(data);
  if (parsed != null) {
    return parsed.secondary + Math.atan(parsed.primary) / Math.PI * 1e-7;
  }

  data = dqDataTablePlainValue(data);
  var num_one_or_with_paren = new RegExp(
    '^\\s*([0-9]+(?:[\\.,][0-9]+)?)\\s*(?:\\(\\s*[0-9]+(?:[\\.,][0-9]+)?\\s*%\\s*\\))?\\s*([a-zA-Z%]*)\\s*$'
  );
  var match = num_one_or_with_paren.exec(data);
  if (match) {
    return Number(match[1].replace(",", "."));
  }
  return data;
}

function dqDataTableApiFromMeta(meta) {
  if (!meta || !meta.settings) {
    return null;
  }
  if (window.jQuery && jQuery.fn && jQuery.fn.dataTable &&
      jQuery.fn.dataTable.Api) {
    return new jQuery.fn.dataTable.Api(meta.settings).table();
  }
  if (window.DataTable && window.DataTable.Api) {
    return new window.DataTable.Api(meta.settings).table();
  }
  return null;
}

function dqDataTableCellValue(table, row, colIdx) {
  if (Array.isArray(row)) {
    return row[colIdx];
  }
  if (row == null || typeof row !== "object") {
    return row;
  }

  var dataSrc = null;
  try {
    dataSrc = table.column(colIdx).dataSrc();
  } catch (e) {}

  if ((typeof dataSrc === "string" || typeof dataSrc === "number") &&
      row.hasOwnProperty(dataSrc)) {
    return row[dataSrc];
  }
  if (row.hasOwnProperty(colIdx)) {
    return row[colIdx];
  }
  return undefined;
}

function dqDataTableColumnHeaderNames(columnApi) {
  var header = columnApi.header();
  var $header = $(header);
  return [
    $header.text().trim(),
    $header.attr("colname"),
    $header.find("[colname]").attr("colname")
  ].filter(function (name, index, names) {
    return name != null && name !== "" && names.indexOf(name) === index;
  });
}

function dqDataTableColumnConfigName(table, cfg, colIdx) {
  if (!cfg || !cfg.grading_cols) {
    return null;
  }
  if (cfg.grading_cols.indexOf(colIdx) >= 0) {
    return colIdx;
  }

  var headerNames = dqDataTableColumnHeaderNames(table.column(colIdx));
  for (var i = 0; i < headerNames.length; i++) {
    if (cfg.grading_cols.indexOf(headerNames[i]) >= 0) {
      return headerNames[i];
    }
  }
  return null;
}

function dqDataTableConfig(table) {
  var id = $(table.table().container()).attr("id");
  if (id == null || !window.dtConfig) {
    return {};
  }
  return window.dtConfig[id] || {};
}

function dqDataTableColorHex(color) {
  if (color == null) return null;
  color = (color + "").trim();
  var hex = color.match(/^#?([0-9a-f]{3}|[0-9a-f]{6}|[0-9a-f]{8})$/i);
  if (hex) {
    hex = hex[1];
    if (hex.length === 3) {
      hex = hex.split("").map(function(character) {
        return character + character;
      }).join("");
    }
    return hex.length === 6 ? hex + "ff" : hex;
  }
  return rgba2hex(color).replace(/^#/, "");
}

function dqDataTableGradingStyle(cfg, data) {
  if (!cfg || !cfg.grading_order || !cfg.grading_colors) return null;
  var gradingIndex = cfg.grading_order.indexOf(dqDataTableGradingValue(data));
  if (gradingIndex < 0 || cfg.grading_colors[gradingIndex] == null) return null;
  return {
    background: dqDataTableColorHex(cfg.grading_colors[gradingIndex]),
    foreground: cfg.fg_colors && cfg.fg_colors[gradingIndex]
  };
}

function dqDataTableGradingFillColors(cfg) {
  if (!cfg || !cfg.grading_colors) return [];
  return unique(cfg.grading_colors.map(dqDataTableColorHex)).filter(
    function(color) {
      return color != null && color !== "00000000";
    }
  );
}

sort_vert_dt = function(data, type, row, meta) {
  /* https://www.pierrerebours.com/2017/09/custom-sorting-with-dt.html */
     if (type == 'dq_filter_label') {
        return(dqDataTableFilterLabel(data))
     } else if (type == 'filter') {
        return(dqDataTableSearchValue(data))
     } else if (type == 'sort') {
        var tb = dqDataTableApiFromMeta(meta)
        var id = tb == null ? null : $(tb.container()).attr("id");
        var cfg = id == null || !window.dtConfig ? null : window.dtConfig[id]
        if (!cfg) {
          cfg = {}
        }
        var column = tb == null ? null : dqDataTableColumnConfigName(tb, cfg, meta.col)
        if (column != null && cfg.grading_order) {
          if (cfg.secondary_order && cfg.secondary_order[column]) {
            var basis = 0.0;
            for (var i = 0; i < cfg.secondary_order[column].length; i++) {
              var basis_col_idx = cfg.secondary_order[column][i];
              if (dqDataTableColumnConfigName(tb, cfg, basis_col_idx) != null) {
                basis = basis + cfg.grading_order.length
              } else {
                basis = basis + tb.rows().count() + 1
              }
            }
            while (basis > 2 ** 16) {
              basis = basis / 2
            }
            var res = 0.0;
            for (var i = 0; i < cfg.secondary_order[column].length; i++) {
              var cur_col_idx = cfg.secondary_order[column][i];
              var cur_value = dqDataTableCellValue(tb, row, cur_col_idx);
              if (dqDataTableColumnConfigName(tb, cfg, cur_col_idx) != null) {
                var cur_order = cfg.grading_order.indexOf(dqDataTableGradingValue(cur_value))
                if (cur_order < 0) {
                  cur_order = cfg.grading_order.length;
                }
              } else {
                var colCnt = tb.column(cur_col_idx).data().toArray()
                  .map(dqDataTableSortValue)
                var cur_order = colCnt.sort().indexOf(dqDataTableSortValue(cur_value))
              }
              res = res * basis + cur_order;
            }

            return (1*res).toString().padStart(50, '0');
          }
          var order = cfg.grading_order.indexOf(dqDataTableGradingValue(data))
          return(order < 0 ? cfg.grading_order.length : order)
        }
        return(dqDataTableSortValue(data))
     } else if (type == 'type') {
        var tb = dqDataTableApiFromMeta(meta)
        var cfg = tb == null || !window.dtConfig ? null :
          window.dtConfig[$(tb.node()).closest(".datatables").attr("id")]
        if (!cfg) {
          cfg = {}
        }
        var column = tb == null ? null : dqDataTableColumnConfigName(tb, cfg, meta.col)
        if (column != null && cfg.grading_order) {
          return "xx"; // 1.0
        }

        if (dqDataTableNumberParenValue(data) != null) {
          return dqDataTableSortValue(data);
        }

        var string = data + "";
        string = string.trim()
        var num_one_or_with_paren = new RegExp(
          '^\\s*([0-9]+(?:[\\.,][0-9]+)?)\\s*(?:\\(\\s*[0-9]+(?:[\\.,][0-9]+)?\\s*%\\s*\\))?\\s*([a-zA-Z%]*)\\s*$'
        );
        var match = num_one_or_with_paren.exec(data);
        if (match) {
          return Number(match[1].replace(",", "."));
        }
        if (isNumeric(string.replace(/\s*%\s*$/, ""))) {
          return(Number(string.replace(/\s*%\s*$/, "")))
        }
        return(data)
     } else {
       return(data);
     }
}

// Convert filter widgets for all traffic lights to drop-downs:
// https://stackoverflow.com/a/28625937
$(function() {
/*
  var availableQS = [
    "",
    "grey",
    "green",
    "yellow",
    "red"
  ];
*/

  $(".matrixTable .dataTables_filter").hide();

  $(".matrixTable thead td input[type=search]:not(:first)").autocomplete({
//    source: availableQS,
   source: function(req, res) {
          let colIdx = this.element.parent().parent().index()
          let api = dqGetDataTableApi(this.element.closest(".datatables"));
          if (!api) {
            res([]);
            return;
          }
          let cats = $(api.column(colIdx).cells().nodes()).find("pre").map(function(x) { return $(this).attr("filter"); })
          cats = Array.from(new Set(cats)).sort(); // requires ES6, https://stackoverflow.com/a/36270406
          // https://stackoverflow.com/a/2405109
          let re = $.ui.autocomplete.escapeRegex(req.term);
          let matcher = new RegExp( "^" + re, "i" );
          let a = $.grep( cats, function(item, index){
          return matcher.test(item);
      });
      res(a)
    },
    select: function(event, ui) {
          let colIdx = $(this).parent().parent().index();
          //let tName = $(this).closest(".html-widget").find("table[aria-describedby]").first()[0].
          //    getAttribute("aria-describedby").replaceAll("_info", "");
          //let oTable = new $.fn.dataTable.Api("#" + tName)
          let oTable = dqGetDataTableApi(this.closest(".datatables"));
          if (!oTable) return false;
          let that = $(this);
          let my_value = ui.item.value;
          that.val(my_value);
          $(function() {
            oTable.column(colIdx).search(my_value).draw();
          })
          return true;
    }
  });
});

// XLSX with colors inspired by https://stackoverflow.com/a/73793683

function getFgColor(cl) {
  var cl1 = dqDataTableColorHex(cl)
  var r = Number("0x" + cl1.substr(0, 2))
  var g = Number("0x" + cl1.substr(2, 2))
  var b = Number("0x" + cl1.substr(4, 2))
  // var a = Number("0x" + cl1.substr(6, 2))
  var brightness = r * 0.299 + g * 0.587 + b * 0.114
  if (brightness > 160) {
    return "000000" + cl1.substr(6, 2)
  } else {
    return "ffffff" + cl1.substr(6, 2)
  }
}


function addCellColorStyles(styles, stylesCount, stylesDict, fillColors) {
  var fgColors = fillColors.map(getFgColor);
  // add font styles:
  let fontsCount = parseInt($( 'fonts', styles ).attr("count"), 10);
  fgColors.forEach((color) => {
    $( 'fonts', styles ).append( fontTmplt(color.toUpperCase()) );
  });
  $( 'fonts', styles ).attr("count", (fontsCount + fgColors.length).toString());

  // add fill styles:
  let fillsCount = parseInt($( 'fills', styles ).attr("count"), 10);
  fillColors.forEach((color) => {
    $( 'fills', styles ).append( fillTmplt(color.toUpperCase()) );
  });
  $( 'fills', styles ).attr("count", (fillsCount + fillColors.length).toString());

  var cellStyles = Array(fillColors.length).fill().map(
    function (element, index) {
      return { fontIdx: index + fontsCount, fillIdx: index + fillsCount };
  });

  // add cell styles:
  cellStyles.forEach(function (style, i) {
    var nm = fillColors[style.fontIdx - fontsCount].toUpperCase();
    $( 'cellXfs', styles ).append( cellXfTmplt(style.fontIdx,
      style.fillIdx, false, false) );
    stylesDict[nm] = stylesCount + 4 * i;
    $( 'cellXfs', styles ).append( cellXfTmplt(style.fontIdx,
      style.fillIdx, true, false) );
    stylesDict[nm + "pct"] = stylesCount + 4 * i + 1;
    $( 'cellXfs', styles ).append( cellXfTmplt(style.fontIdx,
      style.fillIdx, false, true) );
    stylesDict[nm + "int"] = stylesCount + 4 * i + 2;
    $( 'cellXfs', styles ).append( cellXfTmplt(style.fontIdx,
      style.fillIdx, true, true) );
    stylesDict[nm + "pctint"] = stylesCount + 4 * i + 3;

  });
  $( 'cellXfs', styles ).attr("count", (stylesCount +
                                    Object.keys(stylesDict).length).toString());
}

function highlightCells(that, cfg, sheet, stylesCount, stylesDict, colIndex,
                        rowIndex, excelLayout) {
  let isInt = Array(that.columns().nodes().length).fill().map(function (element, cl) {
    let is_int = true;
    that.columns(cl).data()[0].forEach(function(x) {
      if (is_int) {
        let txt = $('<div />').append(x).text();
        if (isNumeric(txt.replace(/\s*%\s*$/, "")) &&
            !Number.isInteger(Number(txt.replaceAll(/[\s%]/g, "").trim()))) {
          is_int = false;
        }
      }
    })
    return is_int;
  });
  rowIndex.dom2data.forEach(function(dataRowIdx, exportRowIdx) {
    colIndex.dom2data.forEach(function(dataColIdx, exportColIdx) {
    if (dqDataTableColumnConfigName(that, cfg, dataColIdx) == null) {
      return;
    }
    var xlRow = excelLayout.dataStartRow + exportRowIdx;
    var cellData = dqDataTableCellValue(
      that,
      that.row(dataRowIdx).data(),
      dataColIdx
    );
    var gradingStyle = dqDataTableGradingStyle(cfg, cellData);
    if (gradingStyle != null) {
      var bgColor = gradingStyle.background;
    // find out, if %
      var txt = $('<div />').append(cellData).text().trim()
      var pct = txt.endsWith("%");
      if (pct) {
        txt = txt.replace(/%\s*$/, "")
      }
      //var tpe = typeof tp ;
      var n = isNumeric(txt);

      let int
      if (n) {
        int = isInt[dataColIdx];
      } else {
        int = false;
      }

      if (bgColor !== "00000000") {
        let cellStyle;
        if (n && pct) {
          if (int) {
            cellStyle = stylesDict[bgColor.toUpperCase() + "pctint"]
          } else {
            cellStyle = stylesDict[bgColor.toUpperCase() + "pct"]
          }
        } else {
          if (int) {
            cellStyle = stylesDict[bgColor.toUpperCase() + "int"]
          } else {
            cellStyle = stylesDict[bgColor.toUpperCase()]
          }
        }
        let xlCol = createXlColLetter(exportColIdx);
        let xlRef = xlCol + xlRow;
        let cellSelector = 'c[r=' + xlRef + ']';
        let cell = $(cellSelector, sheet);
        if (cellStyle == null) {
          return;
        }
        if (cell.length == 0) { // cell is empty, but we want to have it for color
          // row can also be missing
          if ($("sheetData row[r="+xlRow+"]", sheet).length == 0) {
            // https://stackoverflow.com/a/29259101
            // Create non-jq element
            let rwel = $(sheet.createElement("row")); // not with jquery
            rwel.attr({"r": "" + xlRow}) // need to be 2nd step
            alert("should never happen, sorry. please report excel trouble #1")
            //
            // insertAfter xx
            $("sheetData", sheet).append(rwel)
          }
          let cel = $(sheet.createElement("c")); // not with jquery
          cel.attr({
            "t": "inlineStr",
            "r": xlRef,
            "s": cellStyle.toString()
          })
          let is = $(sheet.createElement("is")); // not with jquery
          is.attr({
            "xml:space": "preserve"
          })
          is.append(" ")
          cel.append(is)
          var all_cells_in_row =
            $("sheetData row[r="+xlRow+"] c[r]", sheet).map(function() {return idxFromColLetter(this.getAttribute("r").replaceAll(/[0-9]+$/g, "")) - 1})

          var where_to_append = getPositionForXLXML(all_cells_in_row.toArray(),
            exportColIdx);
          if (where_to_append == 0) {
            $("sheetData row[r="+xlRow+"]", sheet).prepend(cel)
          } else {
            var where = $("sheetData row[r="+xlRow+"] c:nth-child("+where_to_append+")", sheet);
            cel.insertAfter(where);
          }
        } else {
          cell.attr("s", cellStyle.toString());
        }
      }
    }
    });
  });
}

function getPositionForXLXML(list, element) {
  var index = list.findIndex(function(el) {
    return (element < el)
  });
  if (index == -1) {
    return list.length;
  } else {
    return index ;
  }
};

function computeExcelLayout(that, cfg) {
  var exportData = that.buttons.exportData(cfg.exportOptions || {});
  var exportInfo = that.buttons.exportInfo(cfg);
  var headerRows = 0;

  if (cfg.header !== false) {
    if (Array.isArray(exportData.headerStructure)) {
      headerRows = exportData.headerStructure.length;
    } else if (exportData.header) {
      // Buttons 2 / DataTables 1 exports one header row and does not expose
      // headerStructure.
      headerRows = 1;
    }
  }

  return {
    dataStartRow: (exportInfo.title ? 1 : 0) +
      (exportInfo.messageTop ? 1 : 0) + headerRows + 1,
    headerRows: headerRows
  };
}

// https://stackoverflow.com/a/9906193
function idxFromColLetter(l) {
  var base = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', i, j, result = 0;

  for (i = 0, j = l.length - 1; i < l.length; i += 1, j -= 1) {
    result += Math.pow(base.length, j) * (base.indexOf(l[i]) + 1);
  }

  return result;
};

// to build an Excel column letter reference from an
// integer (1 -> A, 2 -> B, 28 -> AB, and so on...);
function createXlColLetter( n ){
  var ordA = 'A'.charCodeAt(0);
  var ordZ = 'Z'.charCodeAt(0);
  var len = ordZ - ordA + 1;
  var s = "";
  while( n >= 0 ) {
    s = String.fromCharCode(n % len + ordA) + s;
    n = Math.floor(n / len) - 1;
  }
  return s;
}

// style templates
function fontTmplt(color) {
  color = rgba2argbxl(color);
  return `<font><sz val="11" /><name val="Calibri" /><color rgb="${ color}" /></font>`;
}
function fillTmplt(color) {
  color = rgba2argbxl(color);
  return `<fill><patternFill patternType="solid"><fgColor rgb="${ color}" /><bgColor indexed="64" /></patternFill></fill>`;
}
function cellXfTmplt(fontIdx, fillIdx, pct, int) {
  var numFmtId;
  if (pct) {
    if (int) {
      numFmtId = 9
    } else {
      numFmtId = 10
    }
  } else {
    if (int) {
      numFmtId = 1
    } else {
      numFmtId = 2
    }
  }
  return `<xf numFmtId="${numFmtId}" fontId="${fontIdx}" fillId="${fillIdx}" borderId="0" applyFont="1" applyFill="1" applyBorder="1" />`;
}

function unique(array) { // https://stackoverflow.com/a/10192255
    return $.grep(array, function(el, index) {
        return index === $.inArray(el, array);
    });
}

function addCellColorStylesPdf(styles, fillColors) {
  var fgColors = fillColors.map(getFgColor);
  fillColors.forEach(function (color, i) {
    var c = fgColors[i]
    var f = fillColors[i]
    styles["s" + color] = {
      'color': "#" + c.substr(0, 6),
      'fillColor': "#" + f.substr(0, 6)
    }
  })
}


function computeRowIndex(that) {
  var dom2data = that.rows({search: 'applied', order: 'applied'})
    .indexes().toArray().filter(function(rowIdx) {
      var row = that.row(rowIdx);
      return rowFilter(rowIdx, row.data(), row.node());
    });
  return {
    "dom2data": dom2data
  }
}

function computeColIndex(that) {
  var dom2data = that.columns(":visible").indexes().toArray();
  return {
    "dom2data": dom2data
  }
}

function dqDataTableCellNode(that, rowIdx, colIdx) {
  if (rowIdx === undefined || colIdx === undefined || rowIdx < 0 || colIdx < 0) {
    return null;
  }

  var cell = that.cell(rowIdx, colIdx);
  if (!cell) {
    return null;
  }

  if (typeof cell.node === "function") {
    return cell.node();
  }

  if (typeof cell.nodes === "function") {
    var nodes = cell.nodes();
    if (nodes && typeof nodes.toArray === "function") {
      nodes = nodes.toArray();
    }
    return nodes && nodes.length ? nodes[0] : null;
  }

  return null;
}

function estimate_pdf_size(pdf) {
    const the_table = pdf.content.find((x) => {return x.table != undefined;}).table.body;
    const columns = [...Array(the_table[0].length).keys()].map(() => Array(0));
    const canvas = document.createElement("canvas");
    const ctx = canvas.getContext("2d");
    const get_size = (row, text) => {
        const fontsize = row === 0 ? 11 : 6;
        ctx.font = `${fontsize}px arial`;
        return ctx.measureText(text).width;
    };
    the_table.forEach( (val, row) => {
        val.map( (inner, col) => {
            columns[col].push(get_size(row, inner.text));
        } );
    })

    const max_width = columns.map((v) => v.reduce((p,c) => Math.max(p,c))).reduce((p, c) => p + c, 0);
    console.log(`calculated width: ${max_width}`);
    return max_width * 0.9;
}


function customize_pdf(pdf, cfg, that) {
  var colIndex = computeColIndex(that);
  var rowIndex = computeRowIndex(that)
  var gradingCfg = dqDataTableConfig(that);
  var fillColors = dqDataTableGradingFillColors(gradingCfg);

  addCellColorStylesPdf(pdf.styles, fillColors)
  let tabEl =
    pdf.content.find(function (docEl) { return docEl.table != undefined});

  tabEl.table.body.forEach(function(row, rowIdx) {
    row.forEach(function(col, colIdx) {
      if (col.style != "tableHeader") {
        var dataColIdx = colIndex.dom2data[colIdx];
        if (dqDataTableColumnConfigName(that, gradingCfg, dataColIdx) == null) {
          return;
        }
        var cellData = dqDataTableCellValue(
          that,
          that.row(rowIndex.dom2data[rowIdx - 1]).data(),
          dataColIdx
        );
        var gradingStyle = dqDataTableGradingStyle(gradingCfg, cellData);
        if (gradingStyle != null && gradingStyle.background !== "00000000") {
          col.style = "s" + gradingStyle.background
        }
      }
    })
  })
  pdf.styles.tableHeader.fontSize = 8;
  pdf.defaultStyle.fontSize = 6;
  const width = estimate_pdf_size(pdf);
  //A3 in postscript points. see https://pdfkit.org/docs/paper_sizes.html
  if (width > 1190.55) {
      pdf.pageSize = {width: width, height: 841.89};
  } else {
      pdf.pageSize = "A3"
  }
  console.log(pdf);
}

function customize_excel(xlsx, cfg, that) {
  var sheet = xlsx.xl.worksheets["sheet1.xml"];
  var excelLayout = computeExcelLayout(that, cfg);
  $(sheet.documentElement).find("[r=A1]").attr("r", "B1")
  if (cfg.messageTop != null) {
    $(sheet.documentElement).find("[r=A2]").attr("r", "B2")
  }
  $(sheet.documentElement).find("mergeCells mergeCell").remove()
  customize_excel_new(xlsx, cfg, that, excelLayout)

  var freezePanes =
    '<sheetViews><sheetView tabSelected="1" workbookViewId="0"><pane xSplit="1" ySplit="' + (excelLayout.dataStartRow - 1) + '" topLeftCell="' + "B" + excelLayout.dataStartRow +'" activePane="bottomRight" state="frozen"/></sheetView></sheetViews>';
  var current = sheet.children[0].innerHTML;
  current = freezePanes + current;
  sheet.children[0].innerHTML = current;
}

function removeNull(sheet) {
  $(sheet.documentElement).find('t:contains("null")').filter(function() {
    return($(this).attr('xml:space') == "preserve")
  }).text("")
}

function dqCustomizePrint(win, cfg, that) {
  if (!that || typeof that.table !== "function") return;
  var gradingCfg = dqDataTableConfig(that);
  var colIndex = computeColIndex(that);
  var table = $(win.document.body).find("table").first();
  table.find("tbody tr").each(function() {
    $(this).children("td").each(function(exportColIdx) {
      var dataColIdx = colIndex.dom2data[exportColIdx];
      if (dqDataTableColumnConfigName(
          that, gradingCfg, dataColIdx) == null) return;
      var gradingStyle = dqDataTableGradingStyle(gradingCfg, $(this).html());
      if (gradingStyle == null) return;
      $(this).css({
        "background-color": "#" + gradingStyle.background.substr(0, 6),
        "color": gradingStyle.foreground ||
          "#" + getFgColor(gradingStyle.background).substr(0, 6),
        "-webkit-print-color-adjust": "exact",
        "print-color-adjust": "exact"
      });
    });
  });
}

function customize_excel_new(xlsx, cfg, that, excelLayout) {
  var colIndex = computeColIndex(that);
  var rowIndex = computeRowIndex(that)
  var gradingCfg = dqDataTableConfig(that);
  var fillColors = dqDataTableGradingFillColors(gradingCfg);

  // styles dictionary
  let stylesDict = new Object();

  // set up new styles:
  let styles = xlsx.xl['styles.xml'];
  let stylesCount = parseInt($( 'cellXfs', styles ).attr("count"), 10);
  addCellColorStyles(styles, stylesCount, stylesDict, fillColors);

  // use new styles:
  let sheet = xlsx.xl.worksheets['sheet1.xml'];
  //$( 'row c', sheet ).attr( 's', (stylesCount + 1).toString() );
  highlightCells(that, gradingCfg, sheet, stylesCount, stylesDict, colIndex,
                 rowIndex, excelLayout);
  removeNull(sheet);
}

// https://stackoverflow.com/a/49974627
function rgba2hex(orig) {
  var a, isPercent,
    rgb = orig.replace(/\s/g, '').match(/^rgba?\((\d+),(\d+),(\d+),?([^,\s)]+)?/i),
    alpha = (rgb && rgb[4] || "").trim(),
    hex = rgb ?
    (rgb[1] | 1 << 8).toString(16).slice(1) +
    (rgb[2] | 1 << 8).toString(16).slice(1) +
    (rgb[3] | 1 << 8).toString(16).slice(1) : orig;

  if (alpha !== "") {
    a = alpha;
  } else {
    a = 01;
  }
  // multiply before convert to HEX
  a = ((a * 255) | 1 << 8).toString(16).slice(1)
  hex = hex + a;

  return hex;
}

function rgba2argbxl(cl) {
  var a = "FF"
  if (cl.length > 6) {
    a = cl.slice(-2);
  }
  var rgb = cl.substr(0, 6);
  return a + rgb ;
}

function rowFilter(idx, data, node) {
  return ( $(node).attr('style') != 'display: none;')
}

function setMsgTop(cap) {
  var target = null;
  if (typeof event !== "undefined" && event && event.currentTarget) {
    target = event.currentTarget;
  }
  if (!target && this) {
    if (typeof this.node === "function") {
      target = this.node();
    } else if (this.node) {
      target = this.node;
    }
  }
  if (!target) {
    return cap;
  }
  var _target = $(target);
  var wrapper = _target.closest("[id]");
  var dt_id = wrapper.attr("id");
  var msgTop = dt_id ? sessionStorage.getItem(dt_id + ".ActiveFilter") : null;
  if (msgTop == null) {
    var group_buttons = wrapper.find(".buttons-colvisGroup");
    if (group_buttons.length > 0) {
      msgTop = $(group_buttons[0]).text();
    }
  }
  if (msgTop == null || msgTop === "") {
    return cap;
  }
  return cap + " -- " + msgTop;
}

function dqDataTableColumnNodesByNameOrHeader(table, columnName) {
  if (typeof columnName === "number") {
    return $(table.column(columnName).nodes());
  }
  var nodes = $();
  try {
    nodes = $(table.column(columnName + ":name").nodes());
  } catch (e) {
    nodes = $();
  }
  if (nodes.length > 0) {
    return nodes;
  }

  table.columns().every(function () {
    var headerNames = dqDataTableColumnHeaderNames(this);

    if (headerNames.indexOf(columnName) >= 0) {
      nodes = nodes.add($(this.nodes()));
    }
  });

  return nodes;
}

function dataquieRcolorize(config) {
    var tb = config.table.table()
    var cfg = config.additional_init_args
    if (!cfg) {
      cfg = {}
    }
    if (cfg.grading_cols && cfg.grading_order && cfg.grading_colors && cfg.fg_colors) {
      var grading_order = cfg.grading_order
      var grading_colors = cfg.grading_colors
      var fg_colors = cfg.fg_colors
      cfg.grading_cols.forEach(function(column) {
        dqDataTableColumnNodesByNameOrHeader(tb, column).each(function() {
          var cls = grading_order.indexOf(dqDataTableGradingValue($(this).html()));
          if (cls >= 0) {
            $(this).css("background-color", grading_colors[cls]);
            $(this).css("color", fg_colors[cls]);
          }
        })
      })
    }
}

function util_dt_sync_filter_alignment(rootEl) {
  if (!rootEl) return;

  // Accept either a DataTables API instance or a DOM root.
  var api = null;
  try {
    if (rootEl.table && typeof rootEl.table === "function") api = rootEl;
  } catch (e) {}

  if (!api) {
    var $root = $(rootEl);
    api = dqGetDataTableApi($root.get(0));
    if (!api) return;
  }

  var container = api.table().container();
  if (!container) return;

  $(container).find("table thead").each(function () {
    var thead = this;

    // Row 0 = header labels (TH), Row 1 = filters (TD in DT::filter='top')
    var headerRow = thead.querySelector("tr");
    if (!headerRow) return;

    var headerCells = headerRow.querySelectorAll("th");
    if (!headerCells || !headerCells.length) return;

    // Find the filter row: the first TR in THEAD that contains an input
    var filterRow = null;
    var rows = thead.querySelectorAll("tr");
    for (var r = 0; r < rows.length; r++) {
      if (rows[r].querySelector("input")) { filterRow = rows[r]; break; }
    }
    if (!filterRow) return;

    // Filter cells can be TD (DT) or TH (some setups)
    var filterCells = filterRow.querySelectorAll("td, th");
    if (!filterCells || !filterCells.length) return;

    var n = Math.min(headerCells.length, filterCells.length);
    for (var i = 0; i < n; i++) {
      var input = filterCells[i].querySelector("input");
      if (!input) continue;

      var align = window.getComputedStyle(headerCells[i]).textAlign;
      if (align) input.style.textAlign = align;
    }
  });
}

function dataquieRInitSearchBuilder(table, initSearch, attempts) {
  if (initSearch == null) return;

  attempts = typeof attempts === "number" ? attempts : 40;

  function rebuild() {
    try {
      if (table && table.searchBuilder &&
          typeof table.searchBuilder.rebuild === "function") {
        table.searchBuilder.rebuild(initSearch);
        return;
      }
    } catch (error) {
      if (attempts <= 0) {
        console.error("Could not initialize the dashboard search filters.",
                      error);
        return;
      }
    }

    if (attempts-- > 0) {
      setTimeout(rebuild, 25);
    } else {
      console.error("Could not initialize the dashboard search filters.");
    }
  }

  // DT2 creates layout features while initComplete is still on the stack.
  // Rebuilding SearchBuilder there aborts the widget constructor, so let the
  // DataTable finish first. The retries also cover slower legacy DT reports.
  setTimeout(rebuild, 0);
}

function dataquieRInitNumberParenFilters(table, config) {
  var cfg = config.additional_init_args || {};
  var columns = cfg.number_paren_filter_cols || [];
  if (!columns.length || !window.jQuery || !jQuery.fn ||
      !jQuery.fn.dataTable) {
    return;
  }

  var settings = table.settings()[0];
  if (!settings || settings._dataquieRNumberParenFilters) {
    return;
  }
  settings._dataquieRNumberParenFilters = {};

  function parseRange(value) {
    value = (value == null ? "" : value + "").trim();
    if (value === "") {
      return null;
    }
    var pieces = value.replace(/\s+/g, "").split("...");
    if (pieces.length === 1) {
      var exact = dqDataTableParsedNumber(pieces[0]);
      return exact == null ? null : { min: exact, max: exact };
    }
    if (pieces.length !== 2) {
      return null;
    }
    var min = pieces[0] === "" ? -Infinity :
      dqDataTableParsedNumber(pieces[0]);
    var max = pieces[1] === "" ? Infinity :
      dqDataTableParsedNumber(pieces[1]);
    if (min == null || max == null) {
      return null;
    }
    return { min: min, max: max };
  }

  function filterCells() {
    var $cells = $();
    $(table.table().container()).closest(".datatables, .html-widget, body")
      .first().find("thead tr").each(function() {
      var $row = $(this);
      if ($row.find("input, select").length) {
        $cells = $cells.add($row.children("td,th"));
      }
    });
    return $cells;
  }

  function filterCellsForColumn(column) {
    var $cells = $();
    $(table.table().container()).closest(".datatables, .html-widget, body")
      .first().find("thead tr").each(function() {
      var $row = $(this);
      if ($row.find("input, select").length) {
        $cells = $cells.add($row.children("td,th").eq(column));
      }
    });
    return $cells;
  }

  function readCell(rowData, column) {
    if ($.isArray(rowData)) {
      return rowData[column];
    }
    var columnDef = settings.aoColumns[column];
    if (!columnDef) {
      return null;
    }
    var dataKey = columnDef.data || columnDef.mData;
    return rowData[dataKey];
  }

  function readFilterSpec(column) {
    var $cells = filterCellsForColumn(column);
    var $cell = $cells.filter(function() {
      return ($(this).find("input").first().val() || "") !== "";
    }).first();
    if (!$cell.length) {
      $cell = $cells.filter(":visible").first();
    }
    if (!$cell.length) {
      $cell = $cells.first();
    }
    return {
      logic: $cell.find("select").first().val() || "equal",
      value: $cell.find("input").first().val() || ""
    };
  }

  function numericFilterValue(cellValue) {
    var parsed = dqDataTableNumberParenValue(cellValue);
    return parsed == null ? dqDataTableParsedNumber(cellValue) :
      parsed.secondary;
  }

  function matchesNumberParenFilter(cellValue, spec) {
    var logic = spec && spec.logic ? spec.logic : "equal";
    var value = numericFilterValue(cellValue);

    if (logic === "empty") {
      return value == null;
    }
    if (logic === "notEmpty") {
      return value != null;
    }
    if (!spec || spec.value == null || (spec.value + "").trim() === "") {
      return true;
    }

    var target = dqDataTableParsedNumber(spec.value);
    if (target == null) {
      var range = parseRange(spec.value);
      return range == null || (value != null &&
        value >= range.min && value <= range.max);
    }
    if (value == null) {
      return false;
    }

    if (logic === "notEqual") {
      return value !== target;
    }
    if (logic === "greater") {
      return value > target;
    }
    if (logic === "greaterOrEqual") {
      return value >= target;
    }
    if (logic === "less") {
      return value < target;
    }
    if (logic === "lessOrEqual") {
      return value <= target;
    }
    return value === target;
  }

  function filterOperatorLabel(logic) {
    var labels = {
      equal: "=",
      notEqual: "!=",
      greater: ">",
      greaterOrEqual: ">=",
      less: "<",
      lessOrEqual: "<=",
      empty: "empty",
      notEmpty: "not empty"
    };
    return labels[logic || "equal"] || "=";
  }

  function syncFilterControls(column, sourceCell) {
    var spec = sourceCell ? {
      logic: sourceCell.find("select").first().val() || "equal",
      value: sourceCell.find("input").first().val() || ""
    } : readFilterSpec(column);
    filterCellsForColumn(column).each(function() {
      var $cell = $(this);
      $cell.find("select").first().val(spec.logic);
      $cell.find(".dtcc-search-type-icon").first()
        .empty()
        .append($("<span/>", {
          "class": "dq-number-paren-operator",
          text: filterOperatorLabel(spec.logic),
          title: "Filter operator"
        }).css({
          "align-items": "center",
          "display": "inline-flex",
          "font-size": "var(--dtcc-search-icon_size)",
          "font-weight": "600",
          "height": "var(--dtcc-search-icon_size)",
          "justify-content": "center",
          "line-height": "var(--dtcc-search-icon_size)",
          "opacity": "var(--dtcc-search-icon_opacity)",
          "width": "var(--dtcc-search-icon_size)"
        }));
      $cell.find("input").first()
        .removeAttr("placeholder")
        .val(spec.value);
    });
    return spec;
  }

  function setColumnControlFilter(column, spec) {
    var columnApi = table.column(column);
    if (!columnApi || !columnApi.search || !columnApi.search.fixed) {
      return false;
    }
    columnApi.search.fixed("dtcc", "");
    if (!spec || (spec.value + "").trim() === "" &&
        spec.logic !== "empty" && spec.logic !== "notEmpty") {
      columnApi.search.fixed("dq-number-paren", "");
    } else {
      columnApi.search.fixed("dq-number-paren", function(searchData) {
        return matchesNumberParenFilter(searchData, spec);
      });
    }
    return true;
  }

  function applyFilter(column, sourceCell) {
    var spec = syncFilterControls(column, sourceCell);
    settings._dataquieRNumberParenFilters[column] = spec;
    setColumnControlFilter(column, spec);
    filterCellsForColumn(column).data("filter", false);
    table.draw();
  }

  $.fn.dataTable.ext.search.push(function(filterSettings, rowData) {
    if (filterSettings !== settings) {
      return true;
    }
    var active = settings._dataquieRNumberParenFilters || {};
    for (var column in active) {
      if (!Object.prototype.hasOwnProperty.call(active, column)) {
        continue;
      }
      var cellValue = readCell(rowData, Number(column));
      if (!matchesNumberParenFilter(cellValue, active[column])) {
        return false;
      }
    }
    return true;
  });

  function attachFilterControls() {
    columns.forEach(function(column) {
      var $cells = filterCellsForColumn(column);
      if (!$cells.length) {
        return;
      }
      function interceptColumnControl(event) {
        if (event.type === "keyup" && event.key && event.key !== "Enter") {
          return;
        }
        var $sourceCell = $(event.currentTarget).closest("td,th");
        event.stopImmediatePropagation();
        window.setTimeout(function() {
          applyFilter(column, $sourceCell);
        }, 0);
      }
      $cells.each(function() {
        var $cell = $(this);
        var $input = $cell.find("input").first();
        if (!$input.length || $input[0]._dataquieRNumberParenFilter) {
          return;
        }
        $input[0]._dataquieRNumberParenFilter = true;
        $cell.attr("data-dq-number-paren", "true");
        $input[0].addEventListener("input", interceptColumnControl, true);
        $input[0].addEventListener("change", interceptColumnControl, true);
        $input[0].addEventListener("keyup", interceptColumnControl, true);
        var select = $cell.find("select").first()[0];
        if (select) {
          select.addEventListener("input", interceptColumnControl, true);
          select.addEventListener("change", interceptColumnControl, true);
        }
        syncFilterControls(column);
        var $slider = $cell.find(".noUi-target").first();
        if ($slider.length) {
          $slider.on("set.dataquieRNumberParen change.dataquieRNumberParen",
            function() {
              window.setTimeout(function() {
                applyFilter(column, $cell);
              }, 0);
            });
        }
        $input.off(".dataquieRNumberParen").on(
          "input.dataquieRNumberParen change.dataquieRNumberParen keyup.dataquieRNumberParen",
          function() {
            window.setTimeout(function() {
              applyFilter(column, $cell);
            }, 0);
          }
        );
      });
    });
  }

  attachFilterControls();
  window.setTimeout(attachFilterControls, 0);
  window.setTimeout(attachFilterControls, 50);
  window.setTimeout(attachFilterControls, 250);
}

function dataquieRBridgeScrollBodyHeaderClicks(table) {
  if (!table || !table.table || typeof table.table !== "function" ||
      !window.jQuery) {
    return;
  }

  var $container = jQuery(table.table().container());
  if ($container.data("dataquieRScrollBodyHeaderBridge")) {
    return;
  }
  $container.data("dataquieRScrollBodyHeaderBridge", true);

  $container.on(
    "click.dataquieRScrollBodyHeaderBridge",
    ".dt-scroll-body thead th, .dt-scroll-body thead td, " +
      ".dataTables_scrollBody thead th, .dataTables_scrollBody thead td",
    function(event) {
      var cell = event.currentTarget;
      var row = cell && cell.parentNode;
      if (!row) {
        return;
      }

      var columnIndex = Array.prototype.indexOf.call(row.children, cell);
      if (columnIndex < 0) {
        return;
      }
      var orderIndex = columnIndex;
      try {
        if (table.column && typeof table.column === "function") {
          var visibleColumnIndex = table.column(columnIndex + ":visible").index();
          if (visibleColumnIndex != null && !isNaN(Number(visibleColumnIndex))) {
            orderIndex = Number(visibleColumnIndex);
          }
        }
      } catch (e) {}

      event.preventDefault();
      event.stopPropagation();

      var currentOrder = [];
      try {
        currentOrder = table.order();
      } catch (e) {}

      var current = currentOrder && currentOrder.length ? currentOrder[0] : null;
      var direction = "asc";
      if (current && Number(current[0]) === orderIndex &&
          (current[1] + "").toLowerCase() === "asc") {
        direction = "desc";
      }

      try {
        table.order([[orderIndex, direction]]).draw();
      } catch (e) {
        var $headCell = $container
          .find(".dt-scroll-head thead tr:first-child th, " +
                ".dt-scroll-head thead tr:first-child td, " +
                ".dataTables_scrollHead thead tr:first-child th, " +
                ".dataTables_scrollHead thead tr:first-child td")
          .eq(columnIndex);
        if ($headCell.length) {
          $headCell.trigger("click");
        }
      }
    }
  );
}

function dataquieRdtCallback(config) {
  dataquieRInitDeferredColumnFilters(config);
  util_dt_sync_filter_alignment(config.table);
  setTimeout(function () { util_dt_sync_filter_alignment(config.table); }, 0);
  if (typeof window.dataquieRApplyDtResponsiveCaps === "function") {
    window.dataquieRApplyDtResponsiveCaps(config.table);
  }
  setTimeout(function () {
    if (typeof window.dataquieRApplyDtResponsiveCaps === "function") {
      window.dataquieRApplyDtResponsiveCaps(config.table);
    }
  }, 0);

  var qs = new URLSearchParams(window.location.search);
  var filterCol = qs.get("dq_filter_col");
  var filterValue = qs.get("dq_filter_value");
  if (filterCol != null && filterValue != null) {
    var tableContainer = $(config.table.table().container());
    config.table.columns().every(function () {
      var header = $(this.header()).text().trim();
      if (header == filterCol) {
        var colIdx = this.index();
        tableContainer.find("thead input[type=search]").eq(colIdx)
          .val(filterValue)
          .trigger("keyup");
        if (this.search() != filterValue) {
          this.search(filterValue).draw();
        }
      }
    });
  }

  if (config.initialColTag != null)
    config.table.button(config.initialColTag + ":name").trigger()
  dataquieRInitSearchBuilder(config.table, config.initSearch)
  dataquieRInitNumberParenFilters(config.table, config)
  dataquieRBridgeScrollBodyHeaderClicks(config.table)
  if (!window.dtConfig) {
    window.dtConfig = {}
  }
  window.dtConfig[$(config.table.table().container()).attr("id")] = config.additional_init_args
  dataquieRcolorize(config)
  dataquieRMoveTotalRowsToBottom(config)
  var tableContainer = $(config.table.table().container());
  dataquieRInitTippies(tableContainer);
  config.table.on("draw.dt column-visibility.dt column-reorder.dt responsive-resize.dt", function () {
    setTimeout(function () {
      dataquieRcolorize(config);
      dataquieRMoveTotalRowsToBottom(config);
      dataquieRInitTippies(tableContainer);
    }, 0);
  });
  // window.tb = config.table
  return config.table ;
}

function dataquieRMoveTotalRowsToBottom(config) {
  var cfg = config.additional_init_args || {};
  var cols = cfg.total_last_cols;
  if (cols == null) return;
  if (!Array.isArray(cols)) cols = [cols];

  var tableContainer = $(config.table.table().container());
  tableContainer.find("tbody").each(function () {
    var tbody = this;
    var rows = Array.prototype.slice.call(tbody.rows || []);
    var totalRows = rows.filter(function (row) {
      return cols.some(function (col) {
        var cell = row.cells && row.cells[col];
        if (!cell) return false;
        return cell.textContent.replace(/\s+/g, " ").trim() === "Total";
      });
    });
    totalRows.forEach(function (row) {
      tbody.appendChild(row);
    });
  });
}

function dataquieRInitTippies(scope) {
  if (!window.jQuery || !window.tippy) return;

  var $scope = scope ? $(scope) : $(document);
  var $withTitle = $scope.find("[title]");
  if ($scope.is && $scope.is("[title]")) {
    $withTitle = $withTitle.add($scope);
  }

  $withTitle.each(function () {
    var title = this.getAttribute("title");
    if (!title || title.trim() === "") {
      this.removeAttribute("title");
      return;
    }
    this.setAttribute("data-tippy-content", title);
    this.removeAttribute("title");
  });

  var $withTippyContent = $scope.find("[data-tippy-content]");
  if ($scope.is && $scope.is("[data-tippy-content]")) {
    $withTippyContent = $withTippyContent.add($scope);
  }

  $withTippyContent.each(function () {
    var content = this.getAttribute("data-tippy-content");
    if (!content || content.trim() === "") return;
    if (this._tippy) {
      this._tippy.setContent(content);
      return;
    }
    tippy(this, {
      content: content,
      allowHTML: true,
      interactive: true,
      maxWidth: 650,
      appendTo: document.body,
      hideOnClick: true,
      trigger: "mouseenter focus"
    });
  });
}

function dataquieRInternalComputedToggleNode(node) {
  return node && node.jquery ? node.get(0) : node;
}

function dataquieRInternalComputedToggleName(value) {
  var holder = document.createElement("div");
  holder.innerHTML = value == null ? "" : String(value);
  return holder.textContent.trim();
}

function dataquieRInternalComputedToggleAction(e, dt, node) {
  var button = dataquieRInternalComputedToggleNode(node);
  var toggle = button ? button.querySelector("input") : null;
  if (!toggle) return;

  var target = e && e.target;
  if (target && target.closest &&
      target.closest(".dq-internal-computed-toggle")) {
    return;
  }
  toggle.checked = !toggle.checked;
  toggle.dispatchEvent(new Event("change", { bubbles: true }));
}

function dataquieRMetadataControls(button) {
  if (!button) return null;
  return button.closest(".dt-layout-row") ||
    button.closest(".dt-buttons") ||
    button.closest(".row");
}

function dataquieRMetadataFixedHeaderInit(dt, button, root) {
  if (!root || !button) return;

  var controls = dataquieRMetadataControls(button);
  if (!controls) return;

  controls.classList.add("dq-metadata-table-controls");

  var pending = false;
  var previousStickyTop = null;
  var previousHeaderOffset = null;
  var observer = null;

  function sync() {
    pending = false;
    var currentControls = dataquieRMetadataControls(button);
    if (currentControls && currentControls !== controls) {
      controls.classList.remove("dq-metadata-table-controls");
      controls = currentControls;
      controls.classList.add("dq-metadata-table-controls");
      if (observer) observer.observe(controls);
    }
    var navbar = document.querySelector(
      ".navbar-fixed-top, .navbar, nav"
    );
    var navbarBottom = navbar ? Math.max(
      0,
      Math.ceil(navbar.getBoundingClientRect().bottom)
    ) : 0;
    var controlsHeight = Math.ceil(
      controls.getBoundingClientRect().height
    );
    var headerOffset = navbarBottom + controlsHeight;

    if (navbarBottom !== previousStickyTop) {
      root.style.setProperty(
        "--dq-metadata-sticky-top",
        navbarBottom + "px"
      );
      previousStickyTop = navbarBottom;
    }

    if (headerOffset !== previousHeaderOffset) {
      if (dt.fixedHeader &&
          typeof dt.fixedHeader.headerOffset === "function") {
        dt.fixedHeader.headerOffset(headerOffset);
      }
      if (dt.fixedHeader &&
          typeof dt.fixedHeader.adjust === "function") {
        dt.fixedHeader.adjust();
      } else if (dt._fixedHeader &&
                 typeof dt._fixedHeader.headerOffset === "function") {
        dt._fixedHeader.headerOffset(headerOffset);
        if (typeof dt._fixedHeader.adjust === "function") {
          dt._fixedHeader.adjust();
        }
      }
      previousHeaderOffset = headerOffset;
    }
  }

  function scheduleSync() {
    if (pending) return;
    pending = true;
    window.requestAnimationFrame(sync);
  }

  if (typeof window.ResizeObserver === "function") {
    observer = new window.ResizeObserver(scheduleSync);
    observer.observe(controls);
    var navbar = document.querySelector(
      ".navbar-fixed-top, .navbar, nav"
    );
    if (navbar) observer.observe(navbar);
  }
  window.addEventListener("resize", scheduleSync);
  window.addEventListener("scroll", scheduleSync, { passive: true });
  dt.on(
    "draw.dt column-visibility.dt responsive-resize.dt",
    scheduleSync
  );
  dt.on("destroy.dt", function() {
    window.removeEventListener("resize", scheduleSync);
    window.removeEventListener("scroll", scheduleSync);
    if (observer) observer.disconnect();
  });
  window.requestAnimationFrame(function() {
    sync();
    window.requestAnimationFrame(scheduleSync);
  });
}

function dataquieRInternalComputedToggleInit(dt, node, config) {
  var button = dataquieRInternalComputedToggleNode(node);
  if (!button || !config) return;
  if (button.dataset.dqInternalComputedInitialized === "true") return;

  var toggle = button.querySelector("input");
  if (!toggle) return;

  var toggleLabel = toggle.closest(".dq-internal-computed-toggle");
  function keepToggleClickInsideLabel(event) {
    event.stopPropagation();
  }
  if (toggleLabel) {
    toggleLabel.addEventListener("click", keepToggleClickInsideLabel);
  }

  function initFixedHeader() {
    var currentButton = document.querySelector(
      "#" + config.rootId +
      " .dq-internal-computed-toggle-control"
    );
    if (!currentButton) return;
    var root = currentButton.closest("#" + config.rootId);
    dataquieRMetadataFixedHeaderInit(dt, currentButton, root);
  }
  window.requestAnimationFrame(initFixedHeader);

  var configuredNames = config.internalNames || [];
  if (!Array.isArray(configuredNames)) {
    configuredNames = [configuredNames];
  }
  var internalNames = new Set(configuredNames);
  var filterName = "dq-internal-computed-" + config.rootId;
  var tableNode = dt.table().node();
  var filter = null;
  var usesFixedSearch = dt.search &&
    typeof dt.search.fixed === "function";

  if (usesFixedSearch) {
    filter = function(searchText, data) {
      if (toggle.checked) return true;
      var firstValue = Array.isArray(data) ? data[0] :
        data[Object.keys(data)[0]];
      return !internalNames.has(
        dataquieRInternalComputedToggleName(firstValue)
      );
    };
    dt.search.fixed(filterName, filter);
  } else if (window.jQuery && $.fn.dataTable &&
      $.fn.dataTable.ext && $.fn.dataTable.ext.search) {
    filter = function(settings, data) {
      if (settings.nTable !== tableNode || toggle.checked) return true;
      return !internalNames.has(
        dataquieRInternalComputedToggleName(data[0])
      );
    };
    $.fn.dataTable.ext.search.push(filter);
  } else {
    return;
  }

  button.dataset.dqInternalComputedInitialized = "true";

  function apply() {
    dt.draw(false);
  }

  toggle.addEventListener("click", function(event) {
    event.stopPropagation();
  });
  toggle.addEventListener("change", apply);
  dt.on("destroy.dt", function() {
    if (toggleLabel) {
      toggleLabel.removeEventListener("click", keepToggleClickInsideLabel);
    }
    if (usesFixedSearch) {
      dt.search.fixed(filterName, null);
    } else {
      var index = $.fn.dataTable.ext.search.indexOf(filter);
      if (index !== -1) $.fn.dataTable.ext.search.splice(index, 1);
    }
  });
  window.requestAnimationFrame(function() {
    apply();
  });
}

$(function() {
  $(".buttons-colvisGroup").each(function(){
    $(this).on("click", function() {
      var _this = $(this);
      // var dt = _this.closest(".datatables").find("table").DataTable();
      var dt_id = _this.closest("[id]").attr("id");
      var filterText = _this.text();
      sessionStorage.setItem(dt_id + ".ActiveFilter", filterText);
    })
  })
});

(function ($) {
  var comboDown = false;

  function isMacPlatform() {
    var platform = "";
    try {
      platform = (navigator.userAgentData && navigator.userAgentData.platform) ||
        navigator.platform ||
        navigator.userAgent ||
        "";
    } catch (e) {
      platform = "";
    }
    return /Mac|iPhone|iPad/i.test(platform);
  }

  function isComboPressed(e) {
    var isMac = isMacPlatform();
    if (isMac && e.ctrlKey) return false;
    return e.altKey && e.shiftKey && (isMac ? e.metaKey : e.ctrlKey);
  }

  $(document).on("keydown", function (e) {
    if (comboDown) return;
    if (isComboPressed(e)) {
      comboDown = true;
      $("button.dq-hidden-col").addClass("dq-show").hide().fadeIn(150);
    }
  });

  $(document).on("keyup", function () {
    if (!comboDown) return;
    comboDown = false;
    $("button.dq-hidden-col").fadeOut(150, function () {
      $(this).removeClass("dq-show");
    });
  });

})(jQuery);
