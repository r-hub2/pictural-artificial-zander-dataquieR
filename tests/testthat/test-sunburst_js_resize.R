test_that("sunburst relayout uses the bounded chart container", {
  skip_on_cran()
  skip_if(Sys.which("node") == "", "node is not available")

  script_lines <- readLines(system.file(
    "menu", "script_toplevel.js", package = "dataquieR"
  ))
  start <- grep("^function relayout_visible_sunburst_widgets", script_lines)
  end <- grep("^//#endregion", script_lines)
  end <- end[end > start][1] - 1L
  sunburst_js <- paste(script_lines[start:end], collapse = "\n")

  probe <- paste(sunburst_js, "
var relayouts = [];
globalThis.document = { body: { body: true }, documentElement: { root: true } };
globalThis.Plotly = {
  Plots: { resize: function(gd) { gd.resized = true; } },
  relayout: function(gd, size) { relayouts.push({ gd: gd.name, size: size }); }
};

function rect(width, height) { return { width: width, height: height }; }
function classList(names) {
  return { contains: function(name) { return names.indexOf(name) !== -1; } };
}
function node(name, width, height, classes, parent, data) {
  return {
    name: name,
    data: data || [],
    parentElement: parent || null,
    classList: classList(classes || []),
    getBoundingClientRect: function() { return rect(width, height); }
  };
}

var page = node('page', 1536, 817, ['singlePage'], document.body);
var wrapper = node('wrapper', 1536, 620, [], page);
var sunburst = node('sunburst', 1536, 342, ['js-plotly-plot'], wrapper,
  [{ type: 'sunburst' }]);
var scatter = node('scatter', 1536, 342, ['js-plotly-plot'], wrapper,
  [{ type: 'scatter' }]);
var root = { querySelectorAll: function() { return [sunburst, scatter]; } };

relayout_visible_sunburst_widgets(root);
if (relayouts.length !== 1) throw new Error('expected one sunburst relayout');
if (relayouts[0].gd !== 'sunburst') throw new Error('relayout touched non-sunburst');
if (relayouts[0].size.width !== 1536) throw new Error('unexpected width');
if (relayouts[0].size.height !== 620) throw new Error('container height not used');
", sep = "\n")

  probe_file <- tempfile(fileext = ".js")
  writeLines(probe, probe_file)
  result <- system2("node", probe_file, stdout = TRUE, stderr = TRUE)
  expect_identical(attr(result, "status"), NULL)
})

test_that("wheel gestures above sunbursts scroll the report page", {
  skip_on_cran()
  skip_if(Sys.which("node") == "", "node is not available")

  script_lines <- readLines(system.file(
    "menu", "script_toplevel.js", package = "dataquieR"
  ))
  start <- grep("^function enable_sunburst_page_scroll", script_lines)
  end <- grep("^/\\*\\*", script_lines)
  end <- end[end > start][1] - 1L
  scroll_js <- paste(script_lines[start:end], collapse = "\n")

  probe <- paste(scroll_js, "
var listener = null;
var registrations = 0;
var scrollingElement = { scrollTop: 0 };
var container = {
  addEventListener: function(name, handler, options) {
    if (name !== 'wheel') throw new Error('unexpected event');
    if (!options.capture || options.passive !== false) {
      throw new Error('wheel listener must run before Plotly');
    }
    listener = handler;
    registrations += 1;
  }
};
var el = { closest: function() { return container; } };
globalThis.document = {
  scrollingElement: scrollingElement,
  documentElement: scrollingElement,
  body: scrollingElement
};
globalThis.window = {
  scrollBy: function(x, y) { scrollingElement.scrollTop += y; }
};

enable_sunburst_page_scroll(el);
enable_sunburst_page_scroll(el);
if (registrations !== 1) throw new Error('listener installed more than once');
var prevented = false;
listener({
  ctrlKey: false,
  deltaY: 120,
  preventDefault: function() { prevented = true; }
});
if (scrollingElement.scrollTop !== 120) throw new Error('page did not scroll');
if (!prevented) throw new Error('Plotly received the wheel gesture');
", sep = "\n")

  probe_file <- tempfile(fileext = ".js")
  writeLines(probe, probe_file)
  result <- system2("node", probe_file, stdout = TRUE, stderr = TRUE)
  expect_identical(attr(result, "status"), NULL)
})

test_that("split sunbursts keep independent drilldown history", {
  skip_on_cran()
  skip_if(Sys.which("node") == "", "node is not available")

  script_lines <- readLines(system.file(
    "menu", "script_toplevel.js", package = "dataquieR"
  ))
  start <- grep("^function sunburst_history_key", script_lines)
  end <- grep("^/\\*\\*", script_lines)
  end <- end[end > start][1] - 1L
  history_js <- paste(script_lines[start:end], collapse = "\n")

  probe <- paste(history_js, "
var split = {
  closest: function() {
    return { id: 'dq-variable-group-sunburst-contradictions' };
  }
};
var standalone = { closest: function() { return null; } };
if (sunburst_history_key(split, 'dq_sunburst_level') !==
    'dq_sunburst_level.dq-variable-group-sunburst-contradictions') {
  throw new Error('split key is not widget-specific');
}
if (sunburst_history_key(standalone, 'dq_sunburst_level') !==
    'dq_sunburst_level') {
  throw new Error('standalone compatibility key changed');
}
", sep = "\n")

  probe_file <- tempfile(fileext = ".js")
  writeLines(probe, probe_file)
  result <- system2("node", probe_file, stdout = TRUE, stderr = TRUE)
  expect_identical(attr(result, "status"), NULL)
})

test_that("sunburst mode, drilldown, and popup history restore together", {
  skip_on_cran()
  skip_if(Sys.which("node") == "", "node is not available")

  script_lines <- readLines(system.file(
    "menu", "script_toplevel.js", package = "dataquieR"
  ))
  start <- grep("^function sunburst_mode_history_key", script_lines)
  end <- grep("^function enable_sunburst_page_scroll", script_lines) - 1L
  switch_js <- paste(script_lines[start:end], collapse = "\n")

  probe <- paste(switch_js, "
function attrs(initial) {
  var values = initial || {};
  return {
    get: function(name) { return values[name] || null; },
    set: function(name, value) { values[name] = String(value); }
  };
}
function button(mode, label) {
  var a = attrs({ 'data-dq-sunburst-mode-button': mode });
  return {
    textContent: label,
    listeners: {},
    classList: { toggle: function(name, on) { this[name] = on; } },
    getAttribute: a.get,
    setAttribute: a.set,
    addEventListener: function(name, fn) { this.listeners[name] = fn; },
    focus: function() { this.focused = true; }
  };
}
function widget(mode) {
  return {
    mode: mode,
    classList: {
      contains: function(name) { return name === 'js-plotly-plot'; }
    },
    restores: [],
    __dqRestoreSunburstHistory: function() {
      var key = 'dq_sunburst_level.dq-variable-group-sunburst-' +
        (mode === 'contradiction' ? 'contradictions' : mode);
      this.restores.push({
        level: (history.state || {})[key] || '',
        popups: ((history.state || {}).dq_open_results || []).slice()
      });
    }
  };
}
function panel(mode, chart) {
  var a = attrs({ 'data-dq-sunburst-mode-panel': mode });
  return {
    hidden: false,
    getAttribute: a.get,
    setAttribute: a.set,
    querySelectorAll: function(selector) {
      return selector === '.js-plotly-plot' ? [chart] : [];
    }
  };
}
var otherButton = button('other', 'Other group checks');
var contradictionButton = button('contradiction', 'Contradiction checks');
var allButton = button('all', 'All group-level results');
var otherWidget = widget('other');
var contradictionWidget = widget('contradiction');
var allWidget = widget('all');
var otherPanel = panel('other', otherWidget);
var contradictionPanel = panel('contradiction', contradictionWidget);
var allPanel = panel('all', allWidget);
var status = { textContent: '' };
var rootAttrs = attrs({ 'data-dq-default-mode': 'other' });
var modeSwitch = {
  id: 'dq-variable-group-sunburst-mode-switch',
  getAttribute: rootAttrs.get,
  setAttribute: rootAttrs.set,
  querySelectorAll: function(selector) {
    return selector.indexOf('button') !== -1 ?
      [otherButton, contradictionButton, allButton] :
      [otherPanel, contradictionPanel, allPanel];
  },
  querySelector: function() { return status; }
};
var widgets = [otherWidget, contradictionWidget, allWidget];
var context = {
  querySelectorAll: function(selector) {
    if (selector === '.dq-sunburst-mode-switch') return [modeSwitch];
    if (selector === '.js-plotly-plot') return widgets;
    return [];
  }
};
var relayouts = [];
var eventListeners = {};
var entries = [{}];
var historyIndex = 0;
var pushes = 0;
var replacements = 0;
var popupPersists = 0;
globalThis.document = context;
globalThis.window = {
  addEventListener: function(name, fn) {
    if (!eventListeners[name]) eventListeners[name] = [];
    eventListeners[name].push(fn);
  },
  __dqPersistPopupHistory: function() { popupPersists += 1; }
};
globalThis.history = {
  state: entries[0],
  pushState: function(next) {
    entries = entries.slice(0, historyIndex + 1);
    entries.push(Object.assign({}, next));
    historyIndex += 1;
    this.state = entries[historyIndex];
    pushes += 1;
  },
  replaceState: function(next) {
    entries[historyIndex] = Object.assign({}, next);
    this.state = entries[historyIndex];
    replacements += 1;
  }
};
function emit(name) {
  (eventListeners[name] || []).forEach(function(fn) { fn(); });
}
function externalPush(changes) {
  history.pushState(Object.assign({}, history.state, changes));
}
function back() {
  if (historyIndex <= 0) throw new Error('cannot go back');
  historyIndex -= 1;
  history.state = entries[historyIndex];
  emit('popstate');
}
function schedule_visible_sunburst_relayout(panel) {
  relayouts.push('scheduled:' + panel.getAttribute(
    'data-dq-sunburst-mode-panel'
  ));
}
function relayout_visible_sunburst_widgets(panel) {
  relayouts.push('printed:' + panel.getAttribute(
    'data-dq-sunburst-mode-panel'
  ));
}

install_sunburst_history_restore_once();
initSunburstModeSwitches(context);
if (otherPanel.hidden || !contradictionPanel.hidden || !allPanel.hidden) {
  throw new Error('default mode is not exclusive');
}
var modeKey = 'dq_sunburst_mode.dq-variable-group-sunburst-mode-switch';
if (history.state[modeKey] !== 'other' || replacements !== 1) {
  throw new Error('default mode was not installed by replacement');
}
if (otherButton.getAttribute('aria-selected') !== 'true' ||
    contradictionButton.getAttribute('aria-selected') !== 'false') {
  throw new Error('default ARIA state is incorrect');
}
contradictionButton.listeners.click();
if (!otherPanel.hidden || contradictionPanel.hidden) {
  throw new Error('contradiction mode is not exclusive');
}
if (status.textContent !== 'Showing Contradiction checks.') {
  throw new Error('live status did not update');
}
if (contradictionButton.getAttribute('aria-selected') !== 'true') {
  throw new Error('selected tab was not announced');
}
if (pushes !== 1 || popupPersists !== 1 ||
    history.state[modeKey] !== 'contradiction') {
  throw new Error('mode transition did not create exactly one history entry');
}

var contradictionLevelKey =
  'dq_sunburst_level.dq-variable-group-sunburst-contradictions';
externalPush((function() {
  var change = {};
  change[contradictionLevelKey] = 'Consistency - Contradictions';
  return change;
})());
externalPush({ dq_open_results: ['dim_con.html#nm=rule_a'] });
allButton.listeners.click();
if (!otherPanel.hidden || !contradictionPanel.hidden || allPanel.hidden) {
  throw new Error('all-results mode is not exclusive');
}
if (status.textContent !== 'Showing All group-level results.') {
  throw new Error('all-results status did not update');
}
var pushesBeforeDuplicate = pushes;
allButton.listeners.click();
if (pushes !== pushesBeforeDuplicate) {
  throw new Error('active mode created a duplicate history entry');
}

var pushesBeforeRestore = pushes;
back();
if (otherPanel.hidden === false || contradictionPanel.hidden ||
    allPanel.hidden === false) {
  throw new Error('back did not restore contradiction mode');
}
var restored = contradictionWidget.restores[
  contradictionWidget.restores.length - 1
];
if (restored.level !== 'Consistency - Contradictions' ||
    restored.popups[0] !== 'dim_con.html#nm=rule_a') {
  throw new Error('mode did not restore its drilldown and popup state');
}
back();
if (((history.state || {}).dq_open_results || []).length !== 0 ||
    history.state[contradictionLevelKey] !== 'Consistency - Contradictions') {
  throw new Error('popup history did not restore independently');
}
back();
if ((history.state || {})[contradictionLevelKey]) {
  throw new Error('drilldown root was not restored');
}
back();
if (history.state[modeKey] !== 'other' || otherPanel.hidden) {
  throw new Error('original mode was not restored');
}
if (pushes !== pushesBeforeRestore) {
  throw new Error('history restoration recursively recorded entries');
}

allButton.listeners.click();
emit('beforeprint');
if (otherPanel.hidden || contradictionPanel.hidden || allPanel.hidden) {
  throw new Error('print does not expose all panels');
}
emit('afterprint');
if (!otherPanel.hidden || !contradictionPanel.hidden || allPanel.hidden) {
  throw new Error('screen mode was not restored after print');
}
", sep = "\n")

  probe_file <- tempfile(fileext = ".js")
  writeLines(probe, probe_file)
  result <- system2("node", probe_file, stdout = TRUE, stderr = TRUE)
  expect_identical(attr(result, "status"), NULL)
  expect_match(
    paste(script_lines, collapse = "\n"),
    "gd.__dqRestoreSunburstHistory = dq_restore",
    fixed = TRUE
  )

  css <- paste(readLines(system.file(
    "menu", "style_toplevel.css", package = "dataquieR"
  )), collapse = "\n")
  expect_match(css, "@media print", fixed = TRUE)
  expect_match(
    css,
    ".dq-sunburst-mode-panel[hidden]",
    fixed = TRUE
  )
})

test_that("a deliberate standalone resize activates Plotly", {
  skip_on_cran()
  skip_if(Sys.which("node") == "", "node is not available")

  script_lines <- readLines(system.file(
    "menu", "script_toplevel.js", package = "dataquieR"
  ))
  start <- grep("^function resize_scaler_div", script_lines)
  end <- grep("^/\\*\\*", script_lines)
  end <- end[end > start][1] - 1L
  resize_js <- paste(script_lines[start:end], collapse = "\n")

  probe <- paste(resize_js, "
var toggles = 0;
var scaler = { __dqResizeArmed: true, dataset: {} };
var initializedScalerDivs = new WeakSet([scaler]);
var dataquieR = { isReady: function() { return true; } };
var window = { dataquieR_single_result: true };
function util_scaler_install_user_resize_detector() {}
function tglePy() { toggles += 1; }
function $(el) {
  return { find: function() { return { length: 1, 0: { image: true } }; } };
}

resize_scaler_div([{ target: scaler, contentRect: { width: 500, height: 300 } }]);
if (toggles !== 1) throw new Error('standalone resize did not activate Plotly');
if (scaler.dataset.userResized !== '1') {
  throw new Error('standalone resize was not recorded');
}
", sep = "\n")

  probe_file <- tempfile(fileext = ".js")
  writeLines(probe, probe_file)
  result <- system2("node", probe_file, stdout = TRUE, stderr = TRUE)
  expect_identical(attr(result, "status"), NULL)
})
