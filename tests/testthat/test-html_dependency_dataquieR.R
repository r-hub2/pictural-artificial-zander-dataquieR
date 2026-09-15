skip_on_cran()

test_that("dataquieR HTML dependency no longer loads script2.js", {
  deps <- list(
    html_dependency_dataquieR(iframe = FALSE),
    html_dependency_dataquieR(iframe = TRUE)
  )

  scripts <- lapply(deps, `[[`, "script")

  expect_true(all(vapply(scripts, function(script) {
    "script.js" %in% script
  }, logical(1))))

  expect_false(any(vapply(scripts, function(script) {
    "script2.js" %in% script
  }, logical(1))))

  expect_false(file.exists(system.file("menu", "script2.js",
        package = "dataquieR"
      )))
})

test_that("report CSS defines one base font family", {
  css_file <- system.file("menu", "style_toplevel.css", package = "dataquieR")
  iframe_css_file <- system.file("menu", "style_iframe.css",
    package = "dataquieR"
  )
  report_dt_css_file <- system.file("report-dt-style", "report-dt-style.css",
    package = "dataquieR"
  )

  css <- readLines(css_file)
  iframe_css <- readLines(iframe_css_file)
  report_dt_css <- readLines(report_dt_css_file)
  base_font_var <- "--dq-base-font-family: Arial"
  base_font_use <- "font-family: var(--dq-base-font-family)"
  inherit_font <- "font-family: inherit !important"
  iframe_font <- "font-family: Arial, Helvetica, sans-serif"

  expect_true(any(grepl(base_font_var, css, fixed = TRUE)))
  expect_true(any(grepl(base_font_use, css, fixed = TRUE)))
  expect_true(any(grepl(inherit_font, report_dt_css, fixed = TRUE)))
  expect_true(any(grepl(iframe_font, iframe_css, fixed = TRUE)))
})

test_that("report content leaves room below the last resizable result", {
  css <- readLines(system.file(
    "menu", "style_toplevel.css", package = "dataquieR"
  ))

  expect_true(any(grepl("padding-bottom: 25vh", css, fixed = TRUE)))
  expect_true(any(grepl("@media print", css, fixed = TRUE)))
})

test_that("report navigation switches before the full menu can wrap", {
  css <- readLines(system.file(
    "menu", "style_toplevel.css", package = "dataquieR"
  ))
  script <- readLines(system.file(
    "menu", "script_toplevel.js", package = "dataquieR"
  ))

  expect_true(any(grepl("max-width: 1280px", css, fixed = TRUE)))
  expect_false(any(grepl("max-width: 1082px", css, fixed = TRUE)))
  expect_true(any(grepl(
    "#dq-reset-figures-btn",
    css,
    fixed = TRUE
  )))
  expect_true(any(grepl("right: 7em !important", css, fixed = TRUE)))
  expect_true(any(grepl(
    "DQ_RESPONSIVE_NAVBAR_QUERY = \"screen and (max-width: 1280px)\"",
    script,
    fixed = TRUE
  )))
  expect_false(any(grepl("max-width: 1082px", script, fixed = TRUE)))
})

test_that("metadata toggle behavior lives in report table resources", {
  css <- readLines(system.file(
    "report-dt-style", "report-dt-style.css", package = "dataquieR"
  ))
  script <- readLines(system.file(
    "report-dt-style", "report_dt.js", package = "dataquieR"
  ))

  expect_true(any(grepl(
    ".dq-internal-computed-metadata-table",
    css,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "dataquieRInternalComputedToggleAction",
    script,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "dataquieRInternalComputedToggleInit",
    script,
    fixed = TRUE
  )))
  expect_false(any(grepl("--dq-metadata-header-top", script, fixed = TRUE)))
  expect_false(any(grepl(
    ".dq-internal-computed-metadata-table .dt-scroll-head",
    css,
    fixed = TRUE
  )))
  expect_true(any(grepl("dt.fixedHeader.adjust()", script, fixed = TRUE)))
  expect_true(any(grepl(
    "dataquieRMetadataFixedHeaderInit",
    script,
    fixed = TRUE
  )))
  expect_true(any(grepl("currentControls", script, fixed = TRUE)))
  expect_true(any(grepl(
    "window.addEventListener(\"scroll\"",
    script,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "headerOffset !== previousHeaderOffset",
    script,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "new window.ResizeObserver(scheduleSync)",
    script,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "toggleLabel.addEventListener(\"click\", keepToggleClickInsideLabel)",
    script,
    fixed = TRUE
  )))
  expect_true(any(grepl("position: sticky", css, fixed = TRUE)))
  expect_true(any(grepl("$.fn.dataTable.ext.search", script, fixed = TRUE)))
})

test_that("DT resize keeps dt-cols max-width for small tables", {
  css <- readLines(system.file("menu", "style.css", package = "dataquieR"))
  script <- readLines(system.file("menu", "script.js", package = "dataquieR"))
  script_toplevel <- readLines(system.file(
    "menu", "script_toplevel.js", package = "dataquieR"
  ))
  report_dt <- readLines(system.file(
    "report-dt-style", "report_dt.js", package = "dataquieR"
  ))

  scroll_selector <- 'find(".dataTables_scroll, .dt-scroll")'
  old_selector <-
    ".dataTables_scroll, .dataTables_scrollHead, .dataTables_scrollBody"

  expect_true(any(grepl(".dt-cols-2 .dataTables_scroll", css, fixed = TRUE)))
  expect_true(any(grepl(".dt-cols-2 .dt-scroll", css, fixed = TRUE)))
  expect_true(any(grepl(".table_result .dt-scroll-head", css, fixed = TRUE)))
  expect_true(any(grepl(".dt-cols-2 .dt-scroll-headInner", css, fixed = TRUE)))
  expect_true(any(grepl("max-width: calc", css, fixed = TRUE) &
        grepl("!important", css, fixed = TRUE)))
  expect_true(any(grepl("margin-right: auto !important", css, fixed = TRUE)))

  expect_true(any(grepl(
    "div.dataTables_scrollBody, div.dt-scroll-body",
    script,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "window.dataquieRApplyDtResponsiveCaps",
    script,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "window.dataquieRApplyDtResponsiveCaps(config.table)",
    report_dt,
    fixed = TRUE
  )))
  for (script_lines in list(script, script_toplevel)) {
    expect_true(any(grepl(scroll_selector, script_lines, fixed = TRUE)))
    expect_true(any(grepl('maxWidth: ""', script_lines, fixed = TRUE)))
    expect_false(any(grepl(old_selector, script_lines, fixed = TRUE)))
  }
})

test_that("all result popups use the single-result hash implementation", {
  script_toplevel <- readLines(system.file(
    "menu", "script_toplevel.js", package = "dataquieR"
  ))

  expect_true(any(grepl(
    'const PREFIX_SHOW = "nm="',
    script_toplevel,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    'get("dq_popup")',
    script_toplevel,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    'attr("data-popup-nm") === nm',
    script_toplevel,
    fixed = TRUE
  )))
})

test_that("result context menus only offer copying when a call exists", {
  script_toplevel <- readLines(system.file(
    "menu", "script_toplevel.js", package = "dataquieR"
  ))

  expect_true(any(grepl(
    'attr("data-call") || ""',
    script_toplevel,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    'if (call !== "")',
    script_toplevel,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "button.dq-copy-r-call",
    script_toplevel,
    fixed = TRUE
  )))
})

test_that("clipboard buttons can confirm a successful copy at the trigger", {
  script_toplevel <- readLines(system.file(
    "menu", "script_toplevel.js", package = "dataquieR"
  ))

  expect_true(any(grepl(
    "e.trigger.dataset.clipboardSuccess",
    script_toplevel,
    fixed = TRUE
  )))
  expect_true(any(grepl('placement: "bottom"', script_toplevel, fixed = TRUE)))
})

test_that("report-scope tree behavior and styles use the report dependency", {
  script_toplevel <- readLines(system.file(
    "menu", "script_toplevel.js", package = "dataquieR"
  ))
  css <- readLines(system.file(
    "menu", "style_toplevel.css", package = "dataquieR"
  ))

  expect_true(any(grepl(
    "function initReportScopeTrees()",
    script_toplevel,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    'document.querySelectorAll(".dq-report-scope-tree-table")',
    script_toplevel,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    ".dq-report-scope-tree-table [title]",
    script_toplevel,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    'trigger: "click"',
    script_toplevel,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    'trigger: "mouseenter focus click"',
    script_toplevel,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "interactive: true",
    script_toplevel,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    'reference.addEventListener("keydown"',
    script_toplevel,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    'event.key === "Enter" || event.key === " "',
    script_toplevel,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "reference.click()",
    script_toplevel,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    'event.key === "Escape" && reference._tippy',
    script_toplevel,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "reference._tippy.hide()",
    script_toplevel,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    'closeButton.className = "dq-report-scope-tooltip-close"',
    script_toplevel,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    'closeButton.setAttribute("aria-label", "Close details")',
    script_toplevel,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "instance.hide()",
    script_toplevel,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    ".dq-report-scope-tree-grid",
    css,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    ".dq-report-scope-tooltip-close {",
    css,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    ".dq-report-scope-tree-coverage-secondary {",
    css,
    fixed = TRUE
  )))
  expect_true(any(grepl("@media (max-width: 900px)", css, fixed = TRUE)))
  expect_true(any(grepl("min-width: 58em", css, fixed = TRUE)))
  expect_true(any(grepl(".dq-assessment-scope {", css, fixed = TRUE)))
})
