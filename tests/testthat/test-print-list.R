test_that("print.list() falls back via NextMethod() for empty lists", {
  # print.default(list()) returns the object invisibly
  expect_invisible(print(list()))
})

test_that("print.list() falls back via NextMethod() during knitr", {
  withr::local_options(knitr.in.progress = TRUE)

  # If this branch is taken, the heavy code must NOT run; we guard it by mocking
  testthat::local_mocked_bindings(
    util_is_try_error = function(...) stop("should not be called"),
    util_ensure_suggested = function(...) stop("should not be called"),
    .package = "dataquieR"
  )

  expect_invisible(print(list(a = 1)))
})

test_that(
  paste0(
    "print.list() renders single-entity multi-result HTML ",
    "and respects view=FALSE"
  ),
  {
    # Two dummy "results" that share one entity
    mk_dqr <- function(cn) {
      dqr <- structure(list(), class = "dataquieR_result")
      attr(dqr, "call") <- structure(list(),
        entity = "AGE_GROUP_0",
        entity_name = "v00103"
      )
      attr(dqr, "cn") <- cn
      dqr
    }

    x <- list(mk_dqr("A"), mk_dqr(NULL))
    # Include cn NULL to cover the title/cn/nm fallback.

    report_dir <- withr::local_tempdir("print-list-report")
    testthat::local_mocked_bindings(
      local_tempdir = function(...) report_dir,
      .package = "withr"
    )

    # Keep this branch self-contained and non-interactive.
    testthat::local_mocked_bindings(
      util_ensure_suggested = function(pkg, ...) {
        # Pretend suggests are available so the code continues.
        TRUE
      },
      html_dependency_dataquieR = function(...) {
        # Return a minimal dependency-like object (or NULL is fine for tagList)
        NULL
      },
      html_dependency_jspdf = function(...) NULL,
      util_alias2caption = function(cn, long = TRUE, ...) {
        paste0("CAPTION-", cn)
      },
      util_pretty_print = function(dqr, nm, is_single_var, use_plot_ly,
        dir, ...) {
        expect_identical(dir, report_dir)
        # Minimal HTML output
        htmltools::div(class = "pp", paste("nm:", nm))
      },
      .package = "dataquieR"
    )

    # Avoid launching a browser / viewer
    expect_null(print(x, view = FALSE))
  }
)

test_that("print.list() combines pure dataquieR_result lists as mini reports", {
  mk_master <- function(group_var) {
    y <- structure(
      list(SummaryTable = data.frame(
        Variables = "SBP",
        Group = group_var,
        stringsAsFactors = FALSE
      )),
      class = c("dataquieR_result", "master_result", "list")
    )
    attr(y, "call") <- substitute(
      acc_margins(resp_vars = "SBP", group_vars = group_var),
      list(group_var = group_var)
    )
    attr(y, "function_name") <- "acc_margins"
    attr(y, "error") <- list()
    y
  }

  x <- list(
    "group_vars=OBS (GROUP_VAR_OBSERVER)" = mk_master("OBS"),
    "group_vars=DEV (GROUP_VAR_DEVICE)" = mk_master("DEV")
  )

  testthat::local_mocked_bindings(
    util_combine_res = function(all_of_f) {
      expect_equal(names(all_of_f), c("acc_margins.SBP", "acc_margins.SBP"))
      slot <- names(all_of_f[[1]])[[1]]
      setNames(
        list(setNames(
          list(data.frame(
            Variables = c("SBP", "SBP"),
            stringsAsFactors = FALSE
          )),
          slot
        )),
        "acc_margins"
      )
    },
    print.master_result = function(x, ...) {
      expect_s3_class(x, "master_result")
      expect_identical(
        util_attr(x, "function_name", exact = TRUE),
        "acc_margins"
      )
      expect_identical(util_attr(x, "cn", exact = TRUE), "acc_margins")
      expect_true(is.call(util_attr(x, "call", exact = TRUE)))
      expect_named(x, "SummaryTable")
      invisible(NULL)
    },
    .package = "dataquieR"
  )

  expect_invisible(print(x))
})

test_that("print.list() combines multiple summary plots as a mini report", {
  skip_on_cran()

  mk_master <- function(group_var) {
    y <- structure(
      list(SummaryPlot = htmltools::div(paste("plot", group_var))),
      class = c("dataquieR_result", "master_result", "list")
    )
    attr(y, "call") <- substitute(
      acc_margins(resp_vars = "SBP", group_vars = group_var),
      list(group_var = group_var)
    )
    attr(y, "function_name") <- "acc_margins"
    attr(y, "error") <- list()
    y
  }

  x <- list(
    "acc_margins.SBP" = mk_master("OBS"),
    "acc_margins.SBP" = mk_master("DEV")
  )

  testthat::local_mocked_bindings(
    util_combine_res = function(all_of_f) {
      expect_length(all_of_f, 2)
      list(
        "acc_margins.OBS" = list(SummaryPlot = htmltools::div("obs")),
        "acc_margins.DEV" = list(SummaryPlot = htmltools::div("dev"))
      )
    },
    util_ensure_suggested = function(pkg, ...) {
      expect_identical(pkg, "plotly")
      TRUE
    },
    util_plot_figure_plotly = function(p, ...) {
      htmltools::span(paste("converted", as.character(p$children[[1]])))
    },
    print.master_result = function(x, ...) {
      expect_s3_class(x, "master_result")
      expect_identical(
        util_attr(x, "function_name", exact = TRUE),
        "acc_margins"
      )
      expect_identical(util_attr(x, "cn", exact = TRUE), "acc_margins")
      expect_named(x, "SummaryPlot")
      expect_s3_class(x$SummaryPlot, "dataquieR_result")
      expect_s3_class(x$SummaryPlot, "shiny.tag.list")
      invisible(NULL)
    },
    .package = "dataquieR"
  )

  expect_invisible(print(x))
})

test_that("print.master_result() delegates outer result lists to print.list()", { # nolint: line_length_linter.
  y <- structure(list(), class = c("dataquieR_result", "master_result"))
  attr(y, "error") <- list()
  x <- structure(list(y, y), class = c("master_result", "list"))

  testthat::local_mocked_bindings(
    print.list = function(x, ...) {
      expect_identical(class(x)[[1]], "list")
      expect_s3_class(x, "master_result")
      invisible(NULL)
    },
    util_pretty_print = function(...) stop("should not be called"),
    .package = "dataquieR"
  )

  expect_invisible(print.master_result(x))
})

test_that("print.master_result() omits legacy limit tables before rendering", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("rmarkdown")

  report_dir <- withr::local_tempdir("print-master-result")
  testthat::local_mocked_bindings(
    local_tempdir = function(...) report_dir,
    .package = "withr"
  )

  x <- structure(
    list(
      ReportSummaryTable = data.frame(legacy = 1),
      SummaryTable = data.frame(current = 2)
    ),
    class = c("master_result", "list")
  )
  attr(x, "cn") <- "limits"
  attr(x, "nm") <- "Limit result"
  attr(x, "function_name") <- "con_limit_deviations"

  testthat::local_mocked_bindings(
    util_pretty_print = function(dqr, ...) {
      expect_null(dqr$ReportSummaryTable)
      htmltools::div("rendered limit result")
    },
    util_ensure_suggested = function(...) TRUE,
    util_write_iframe_results = function(...) invisible(NULL),
    util_copy_all_deps = function(...) list(deps = list()),
    .package = "dataquieR"
  )

  expect_invisible(print.master_result(x, view = FALSE))
})

test_that("print.master_result() opens rendered HTML through the viewer", {
  report_dir <- withr::local_tempdir("print-master-view")
  testthat::local_mocked_bindings(
    local_tempdir = function(...) report_dir,
    .package = "withr"
  )

  report_dir <- withr::local_tempdir("print-master-view")
  testthat::local_mocked_bindings(
    local_tempdir = function(...) report_dir,
    .package = "withr"
  )

  x <- structure(list(SummaryTable = data.frame(current = 2)),
    class = c("master_result", "list"))
  attr(x, "cn") <- "summary"
  attr(x, "nm") <- "Summary"

  state <- new.env(parent = emptyenv())
  withr::local_options(viewer = function(path) {
    state$path <- path
  })
  testthat::local_mocked_bindings(
    util_save_master_result_html = function(x, dir, template = "default", ...) {
      expect_s3_class(x, "master_result")
      expect_identical(template, "default")
      file.path(dir, "index.html")
    },
    .package = "dataquieR"
  )

  expect_invisible(print.master_result(x))
  expect_identical(state$path, "index.html")
})

test_that("questionnaire master results retain SSI outputs while rendering", {
  skip_on_cran()
  skip_if_not_installed("htmltools")
  skip_if_not_installed("rmarkdown")

  result <- structure(
    list(SummaryTable = data.frame(Variables = "Missing responses")),
    class = c("dataquieR_result", "master_result")
  )
  attr(result, "function_name") <- "con_ssi_range_check"
  attr(result, "dq_result_title") <- "Missing responses"
  x <- util_sectioned_master_result_from_result_list(
    list(`con_ssi_range_check.Missing responses` = result),
    title_mode = "ssi"
  )
  attr(x, "dq_questionnaire_result") <- TRUE

  testthat::local_mocked_bindings(
    util_pretty_print = function(dqr, is_ssi, ...) {
      expect_true(is_ssi)
      expect_true(.called_in_pipeline)
      expect_named(dqr, "SummaryTable")
      expect_null(util_attr(dqr, "dq_result_title", exact = TRUE))
      htmltools::div("rendered SSI result")
    },
    util_ensure_suggested = function(...) TRUE,
    util_write_iframe_results = function(...) invisible(NULL),
    util_copy_all_deps = function(...) list(deps = list()),
    .package = "dataquieR"
  )

  output_dir <- withr::local_tempdir()
  expect_s3_class(
    util_save_master_result_html(x, dir = output_dir),
    "shiny.tag.list"
  )
  expect_true(file.exists(file.path(output_dir, "logo.png")))
  expect_match(
    readChar(file.path(output_dir, "index.html"),
      nchars = file.info(file.path(output_dir, "index.html"))$size
    ),
    'href="logo.png"',
    fixed = TRUE
  )
})

test_that("print.master_result() reports unsupported knitr output modes", {
  skip_if_not_installed("knitr")

  report_dir <- withr::local_tempdir("print-master-knitr")
  testthat::local_mocked_bindings(
    local_tempdir = function(...) report_dir,
    .package = "withr"
  )

  x <- structure(list(SummaryTable = data.frame(current = 2)),
    class = c("master_result", "list"))
  attr(x, "cn") <- "summary"
  attr(x, "nm") <- "Summary"

  withr::local_options(knitr.in.progress = TRUE)
  testthat::local_mocked_bindings(
    util_save_master_result_html = function(...) "index.html",
    util_ensure_suggested = function(...) TRUE,
    .package = "dataquieR"
  )

  testthat::with_mocked_bindings(
    .package = "knitr",
    is_latex_output = function() TRUE,
    is_html_output = function() FALSE,
    pandoc_to = function() "latex",
    {
      expect_warning(
        expect_identical(print.master_result(x), ""),
        "not yet supported"
      )
    }
  )

  testthat::with_mocked_bindings(
    .package = "knitr",
    is_latex_output = function() FALSE,
    is_html_output = function() FALSE,
    pandoc_to = function() "docx",
    {
      expect_warning(
        expect_identical(print.master_result(x), ""),
        "not yet supported"
      )
    }
  )
})

test_that("util_combine_res merges disjoint result descriptions", {
  mk_result <- function(group_var) {
    result_data <- data.frame(
      Variables = "SBP",
      group = "a",
      margins = 1,
      stringsAsFactors = FALSE
    )
    names(result_data)[[2]] <- group_var
    attr(result_data, "description") <- c(
      setNames("Group: Observer/Device/...", group_var),
      margins = "Mean for the Group"
    )

    y <- structure(list(ResultData = result_data),
      class = c("dataquieR_result", "list")
    )
    attr(y, "call") <- substitute(
      acc_margins(resp_vars = "SBP", group_vars = group_var),
      list(group_var = group_var)
    )
    attr(y, "function_name") <- "acc_margins"
    attr(y, "cn") <- "acc_margins"
    attr(y, "error") <- list()
    attr(y, "warning") <- list()
    attr(y, "message") <- list()
    y
  }

  results <- list(
    "acc_margins.SBP" = mk_result("OBS_BP_0"),
    "acc_margins.SBP" = mk_result("DEV_BP_0")
  )

  combined <- expect_warning(util_combine_res(results), NA)
  description <- util_attr(combined[[1]]$ResultData, "description",
    exact = TRUE
  )

  expect_equal(
    description[c("OBS_BP_0", "DEV_BP_0", "margins")],
    c(
      OBS_BP_0 = "Group: Observer/Device/...",
      DEV_BP_0 = "Group: Observer/Device/...",
      margins = "Mean for the Group"
    )
  )
})

test_that("util_combine_res handles table metadata edge cases", {
  skip_on_cran()

  mk_result <- function(variable, description = NULL) {
    table <- data.frame(
      Variables = variable,
      value = 1,
      stringsAsFactors = FALSE
    )
    attr(table, "VAR_NAMES") <- variable
    attr(table, "description") <- description

    y <- structure(list(ReportSummaryTable = table),
      class = c("dataquieR_result", "list")
    )
    attr(y, "call") <- substitute(acc_margins(resp_vars = variable),
      list(variable = variable))
    attr(y, "function_name") <- "acc_margins"
    attr(y, "cn") <- "acc_margins"
    attr(y, "error") <- list()
    attr(y, "warning") <- list()
    attr(y, "message") <- list()
    y
  }

  results <- list(
    "acc_margins.SBP" = mk_result("SBP", c(value = "first")),
    "acc_margins.DBP" = mk_result("DBP", c(value = "second"))
  )

  expect_warning(
    combined <- util_combine_res(results),
    "incompatible results"
  )
  expect_equal(
    util_attr(combined[[1]][[1]], "VAR_NAMES"),
    c("SBP", "DBP")
  )
})

test_that("util_combine_res rejects invalid plain labels", {
  skip_on_cran()

  table <- data.frame(
    Variables = "SBP",
    value = 1,
    stringsAsFactors = FALSE
  )
  attr(table$Variables, "plain_label") <- c("Systolic", "Blood pressure")

  y <- structure(list(ReportSummaryTable = table),
    class = c("dataquieR_result", "list")
  )
  attr(y, "call") <- quote(acc_margins(resp_vars = "SBP"))
  attr(y, "function_name") <- "acc_margins"
  attr(y, "cn") <- "acc_margins"
  attr(y, "error") <- list()
  attr(y, "warning") <- list()
  attr(y, "message") <- list()

  expect_error(
    util_combine_res(list("acc_margins.SBP" = y)),
    "invalid plain label atts"
  )
})

test_that(
  paste0(
    "print.list() master_result: warns for stored errors, ",
    "then falls back via NextMethod()"
  ),
  {
    mk_master <- function(entity, with_err = FALSE) {
      y <- structure(list(), class = c("dataquieR_result", "master_result"))
      attr(y, "call") <- structure(list(), entity = entity, entity_name = "N")
      attr(y, "function_name") <- "acc_dummy"
      attr(y, "error") <- if (with_err) {
        list(simpleWarning("stored warning from result"))
      } else {
        list()
      }
      y
    }

    x <- list(
      mk_master("E1", TRUE),
      mk_master("E2", FALSE)
    )

    testthat::local_mocked_bindings(
      util_stop_if_not = function(...) invisible(TRUE),
      util_combine_res = function(...) stop("should not be called"),
      # IMPORTANT: fallback printing may call this via print.dataquieR_result()
      print.master_result = function(...) invisible(NULL),
      .package = "dataquieR"
    )

    # Reaches warning loop, then NextMethod() triggers default printing.
    expect_warning(
      print(x),
      "stored warning from result"
    )
  }
)
