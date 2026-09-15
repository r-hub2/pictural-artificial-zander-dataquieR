skip_on_cran()

test_that("renderinfo and iframe helper utilities use local file state", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  out_dir <- tempfile()
  dir.create(out_dir)

  rep_id <- dataquieR:::util_write_renderinfo_js_json(
    out_dir,
    rep_id = "rep-test",
    start_time = as.POSIXct("2026-07-20 10:00:00", tz = "UTC"),
    end_time = as.POSIXct("2026-07-20 10:03:00", tz = "UTC")
  )
  expect_identical(rep_id, "rep-test")

  json <- readLines(file.path(out_dir, ".report", "renderinfo.json"),
    warn = FALSE)
  js <- readLines(file.path(out_dir, ".report", "renderinfo.js"),
    warn = FALSE)
  expect_match(paste(json, collapse = "\n"), '"renderingTime": "3 min"',
    fixed = TRUE)
  expect_match(paste(json, collapse = "\n"), '"reportId": "rep-test"',
    fixed = TRUE)
  expect_match(paste(js, collapse = "\n"), "window.renderingData = ",
    fixed = TRUE)

  no_time_id <- dataquieR:::util_write_renderinfo_js_json(out_dir)
  expect_type(no_time_id, "character")
  no_time_json <- readLines(file.path(out_dir, ".report", "renderinfo.json"),
    warn = FALSE)
  expect_match(paste(no_time_json, collapse = "\n"),
    '"renderingTime": null',
    fixed = TRUE)

  tagged <- htmltools::tags$span("child")
  attr(tagged, "html_file") <- "child.html"
  nested <- htmltools::tagList(
    htmltools::tags$div("plain"),
    htmltools::tags$section(tagged)
  )
  found <- dataquieR:::util_collect_tag_attrs(nested)
  expect_length(found, 1)
  expect_identical(attr(found[[1]], "html_file"), "child.html")

  fs <- dataquieR:::util_check_shared_filesystem(out_dir, cl = NULL,
    cleanup = FALSE)
  expect_true(fs$shared)
  expect_true(fs$exists)
  expect_true(file.exists(fs$tmpFile))
  unlink(fs$tmpFile, force = TRUE)
})

test_that("loading page includes outer report-by progress fallbacks", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  outer_env <- dataquieR:::.outer_by_env # nolint
  old_outer <- outer_env$outer_by
  withr::defer({
    if (is.null(old_outer)) {
      rm("outer_by", envir = outer_env)
    } else {
      outer_env$outer_by <- old_outer
    }
  })

  outer_env$outer_by <- list(i = 2L, n = 4L, msg = "Segment <A>")
  valid_lines <- dataquieR:::util_index_loading_lines( # nolint
    title = "Title <A>",
    message = "Working",
    detail = "Detail & context",
    percent = 25,
    logo_rel = "logo.png"
  )
  valid_html <- paste(valid_lines, collapse = "\n")
  expect_match(valid_html, "Overall: 25% &mdash; step 2 / 4", fixed = TRUE)
  expect_match(valid_html, "Segment &lt;A&gt;", fixed = TRUE)
  expect_match(valid_html, "Detail &amp; context", fixed = TRUE)

  outer_env$outer_by <- list(i = "next", n = 0L, msg = "Waiting")
  fallback_lines <- dataquieR:::util_index_loading_lines( # nolint
    title = "Title",
    message = "Working",
    percent = NA_real_,
    logo_rel = ""
  )
  fallback_html <- paste(fallback_lines, collapse = "\n")
  expect_match(fallback_html, "outer-spinner", fixed = TRUE)
  expect_match(fallback_html, "Waiting", fixed = TRUE)
})

test_that("report render paths collect fixed report file names", {
  skip_on_cran()

  out_dir <- tempfile()
  paths <- dataquieR:::util_report_render_paths(out_dir)

  expect_identical(paths$content_dir, dataquieR:::util_normalize_path(out_dir))
  expect_identical(paths$report_dir, file.path(paths$content_dir, ".report"))
  expect_identical(paths$content_file,
    file.path(paths$content_dir, "index.html"))
  expect_identical(paths$content_lib_dir, file.path(paths$content_dir, "lib"))
  expect_identical(paths$logo, "logo.png")
  expect_identical(paths$report_logo_file,
    file.path(paths$report_dir, "logo.png"))
  expect_identical(paths$content_logo_rel, ".report/logo.png")
  expect_identical(paths$report_logo_rel, "logo.png")
  expect_identical(paths$anchor_js_file,
    file.path(paths$report_dir, "anchor_list.js"))
  expect_identical(paths$anchor_rds_file,
    file.path(paths$report_dir, "anchor_list.RDS"))
})

test_that("anchor list writer uses render path helper output", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  out_dir <- tempfile()
  paths <- dataquieR:::util_report_render_paths(out_dir)
  dir.create(paths$report_dir, recursive = TRUE)

  pages <- list(
    page_a.html = htmltools::tags$section(
      id = "section-a",
      htmltools::tags$span(id = "inner-a")
    )
  )

  all_ids <- dataquieR:::util_write_anchor_list(pages, paths)

  expect_true(file.exists(paths$anchor_js_file))
  expect_true(file.exists(paths$anchor_rds_file))
  expect_identical(readRDS(paths$anchor_rds_file), all_ids)
  expect_true("page_a.html#section-a" %in% all_ids)
  expect_true("page_a.html#inner-a" %in% all_ids)

  js <- readLines(paths$anchor_js_file, warn = FALSE)
  expect_match(paste(js, collapse = "\n"), "window.all_ids", fixed = TRUE)
  expect_match(paste(js, collapse = "\n"), "page_a.html#section-a",
    fixed = TRUE)
})

test_that("renderinfo writer falls back when atomic renames fail", {
  skip_on_cran()

  out_dir <- tempfile()
  dir.create(out_dir)

  testthat::with_mocked_bindings(
    .package = "base",
    file.rename = function(...) FALSE,
    {
      rep_id <- dataquieR:::util_write_renderinfo_js_json(
        out_dir,
        rep_id = "fallback"
      )
    }
  )

  report_dir <- file.path(out_dir, ".report")
  expect_identical(rep_id, "fallback")
  expect_true(file.exists(file.path(report_dir, "renderinfo.json")))
  expect_true(file.exists(file.path(report_dir, "renderinfo.js")))
  expect_false(file.exists(file.path(report_dir, "renderinfo.json.tmp")))
  expect_false(file.exists(file.path(report_dir, "renderinfo.js.tmp")))
})

test_that("renderinfo writer reports blocked report directories", {
  skip_on_cran()

  out_dir <- tempfile()
  dir.create(out_dir)
  writeLines("not a directory", file.path(out_dir, ".report"))

  expect_error(
    suppressWarnings(dataquieR:::util_write_renderinfo_js_json(out_dir)),
    "Could not create"
  )
})

test_that("iframe writer cluster setup falls back to serial modes", {
  skip_on_cran()
  skip_if_not_installed("parallel")

  expect_null(dataquieR:::util_start_iframe_writer_cluster(
    list(mode = "local", cpus = 2),
    max_tasks = 2
  ))
  expect_warning(
    expect_null(dataquieR:::util_start_iframe_writer_cluster(
      list(mode = "unknown", cpus = 2),
      max_tasks = 2
    )),
    "Unknown parallel mode"
  )
  expect_identical(
    dataquieR:::util_stop_iframe_writer_cluster(NULL),
    invisible(NULL)
  )

  seen <- new.env(parent = emptyenv())
  fake_cluster <- structure(list(), class = "cluster")
  testthat::with_mocked_bindings(
    .package = "parallel",
    makePSOCKcluster = function(cpus) {
      seen$psock_cpus <- cpus
      fake_cluster
    },
    {
      invalid_mode <- dataquieR:::util_start_iframe_writer_cluster(
        list(mode = character(), cpus = 1),
        max_tasks = 2
      )
    }
  )
  expect_identical(invalid_mode, fake_cluster)
  expect_identical(seen$psock_cpus, 1L)

  testthat::with_mocked_bindings(
    util_detect_cores = function() 2L,
    testthat::with_mocked_bindings(
      .package = "parallel",
      makePSOCKcluster = function(cpus) {
        seen$default_cpus <- cpus
        fake_cluster
      },
      {
        default_cpus <- dataquieR:::util_start_iframe_writer_cluster(
          list(mode = "socket", cpus = NA_real_),
          max_tasks = 3
        )
      }
    )
  )
  expect_identical(default_cpus, fake_cluster)
  expect_identical(seen$default_cpus, 2L)

  testthat::with_mocked_bindings(
    .package = "parallel",
    makeForkCluster = function(cpus) {
      seen$fork_cpus <- cpus
      fake_cluster
    },
    {
      fork_cluster <- dataquieR:::util_start_iframe_writer_cluster(
        list(mode = "multicore", cpus = 2),
        max_tasks = 1
      )
    }
  )
  expect_identical(fork_cluster, fake_cluster)
  expect_identical(seen$fork_cpus, 1L)
})

test_that("iframe writer falls back when workers cannot share files", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  out_dir <- tempfile()
  dir.create(out_dir)
  iframe <- htmltools::tags$div("iframe content")
  attr(iframe, "html_file") <- file.path(out_dir, "iframe.html")
  attr(iframe, "html_inner") <- htmltools::tags$div("inner content")

  calls <- new.env(parent = emptyenv())
  calls$written <- 0L
  fake_cluster <- structure(list(list()), class = "cluster")
  testthat::local_mocked_bindings(
    util_copy_all_deps = function(dir, pages, copy_dependencies) {
      list(rendered_pages = pages, deps = htmltools::tagList())
    },
    util_check_shared_filesystem = function(...) list(shared = FALSE),
    util_write_one = function(...) {
      calls$written <- calls$written + 1L
      invisible(NULL)
    }
  )

  expect_message(
    util_write_iframe_results(
      pages = list(iframe),
      progress_msg = function(...) invisible(NULL),
      progress = function(...) invisible(NULL),
      block_load_factor = 1,
      copy_dependencies = FALSE,
      template_file = "unused.html",
      dir = out_dir,
      cores = fake_cluster
    ),
    "File system not shared.*falling back to serial writing"
  )
  expect_identical(calls$written, 1L)
})

test_that("RStudio caller-owned cluster guard can warn or stop", {
  skip_on_cran()

  fake_cluster <- structure(list(), class = "cluster")

  testthat::local_mocked_bindings(
    util_really_rstudio = function() TRUE
  )

  expect_error(
    util_guard_rstudio_user_cluster(fake_cluster),
    "Using a caller-created parallel cluster"
  )

  withr::local_options(dataquieR.force_rstudio_user_cluster = TRUE)
  expect_warning(
    util_guard_rstudio_user_cluster(fake_cluster),
    "Using a caller-created parallel cluster"
  )

  expect_warning(
    util_guard_rstudio_user_cluster(fake_cluster,
      advanced_options = list(dataquieR.force_rstudio_user_cluster = TRUE)
    ),
    "Using a caller-created parallel cluster"
  )

  expect_identical(
    util_guard_rstudio_user_cluster(NULL),
    invisible(NULL)
  )
})

test_that("iframe writer writes HTML from template without thumbnail", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  out_dir <- tempfile()
  dir.create(out_dir)
  template_file <- file.path(out_dir, "template.html")
  writeLines(
    "<!DOCTYPE html><html><head>{{ deps }}</head><body>{{ content }}</body></html>", # nolint: line_length_linter.
    template_file
  )
  outfile <- file.path(out_dir, "child.html")
  thumb <- file.path(out_dir, "child.png")
  tag <- htmltools::span("outer")
  attr(tag, "html_file") <- outfile
  attr(tag, "thumbnail_path") <- thumb
  attr(tag, "ggthumb") <- util_compress(NULL)
  attr(tag, "html_inner") <- htmltools::span("inner")
  attr(tag, "thumbnail_args") <- list()

  expect_silent(util_write_one(
    tag,
    template_file = template_file,
    deps = htmltools::tagList()
  ))

  expect_true(file.exists(outfile))
  expect_false(file.exists(thumb))
  html <- paste(readLines(outfile, warn = FALSE), collapse = "\n")
  expect_match(html, "<span>inner</span>", fixed = TRUE)
})

test_that("iframe writer muffles known Plotly serialization warnings", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  out_dir <- tempfile()
  dir.create(out_dir)
  template_file <- file.path(out_dir, "template.html")
  writeLines("{{ content }}", template_file)

  make_tag <- function(outfile) {
    tag <- htmltools::span("outer")
    attr(tag, "html_file") <- outfile
    attr(tag, "thumbnail_path") <- file.path(out_dir, "thumb.png")
    attr(tag, "ggthumb") <- util_compress(NULL)
    attr(tag, "html_inner") <- htmltools::span("inner")
    attr(tag, "thumbnail_args") <- list()
    tag
  }

  testthat::with_mocked_bindings(
    .package = "htmltools",
    htmlTemplate = function(...) {
      warning("'bar' objects don't have these attributes: 'mode'")
      htmltools::HTML("<span>inner</span>")
    },
    {
      expect_silent(util_write_one(
        make_tag(file.path(out_dir, "template-warning.html")),
        template_file = template_file,
        deps = htmltools::tagList()
      ))
    }
  )

  write_result <- NULL
  testthat::with_mocked_bindings(
    .package = "base",
    cat = function(...) {
      warning("'box' objects don't have these attributes: 'mode'")
    },
    {
      write_result <- util_write_one(
        make_tag(file.path(out_dir, "write-warning.html")),
        template_file = template_file,
        deps = htmltools::tagList()
      )
    }
  )
  expect_null(write_result)
})

test_that("render compatibility metadata warns before template rendering", {
  skip_on_cran()

  result <- structure(
    list(SummaryTable = data.frame(x = 1)),
    class = "dataquieR_result"
  )
  report <- structure(
    list(call_a.x = result),
    class = "dataquieR_resultset2",
    all_calls = list(call_a.x = quote(call_a(x))),
    rn = "x",
    cn = "call_a",
    names = "call_a.x",
    matrix_list = structure(
      list(),
      row_indices = c(x = 1),
      col_indices = c(call_a = 1)
    )
  )
  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) invisible(TRUE),
    util_parallel_start = function(...) invisible(NULL),
    util_parallel_stop = function() invisible(NULL)
  )

  expect_warning(
    expect_error(
      print.dataquieR_resultset2(
        report,
        dir = tempfile(),
        template = "missing-template",
        view = FALSE,
        cores = NULL
      ),
      "Template .+ not found"
    ),
    "may be not rendered correctly"
  )

  attr(report, "min_render_version") <- as.numeric_version("1.0.0")
  attr(report, "translation_version") <- "0.0.0"
  expect_warning(
    expect_error(
      print.dataquieR_resultset2(
        report,
        dir = tempfile(),
        template = "missing-template",
        view = FALSE,
        cores = NULL
      ),
      "Template .+ not found"
    ),
    "translations may have changed"
  )
})

test_that("iframe writer delegates thumbnail creation when requested", {
  skip_on_cran()
  skip_if_not_installed("htmltools")
  skip_if_not_installed("ggplot2")

  out_dir <- tempfile()
  dir.create(out_dir)
  template_file <- file.path(out_dir, "template.html")
  writeLines(
    "<!DOCTYPE html><html><head>{{ deps }}</head><body>{{ content }}</body></html>", # nolint: line_length_linter.
    template_file
  )
  outfile <- file.path(out_dir, "child.html")
  thumb <- file.path(out_dir, "child.png")
  plot <- ggplot2::ggplot(
    data.frame(x = 1, y = 2),
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point()

  calls <- new.env(parent = emptyenv())
  calls$args <- list()
  testthat::local_mocked_bindings(
    util_save_with_inner_size = function(...) {
      calls$args[[length(calls$args) + 1L]] <- list(...)
      invisible(NULL)
    }
  )

  tag <- htmltools::span("outer")
  attr(tag, "html_file") <- outfile
  attr(tag, "thumbnail_path") <- thumb
  attr(tag, "ggthumb") <- util_compress(plot)
  attr(tag, "html_inner") <- htmltools::span("inner")
  attr(tag, "thumbnail_args") <- list(
    width = 2,
    height = 1,
    dpi = 72,
    rotated = TRUE
  )

  expect_silent(util_write_one(
    tag,
    template_file = template_file,
    deps = htmltools::tagList()
  ))

  expect_true(file.exists(outfile))
  expect_length(calls$args, 1L)
  expect_identical(calls$args[[1]]$filename, thumb)
  expect_equal(calls$args[[1]]$inner_width, 2)
  expect_equal(calls$args[[1]]$inner_height, 1)
  expect_equal(calls$args[[1]]$dpi, 72)
  expect_equal(calls$args[[1]]$base_text_factor, 40)
})

test_that("deferred parallel cleanup skips an already stopped backend", {
  called <- FALSE
  testthat::local_mocked_bindings(
    util_parallel_status = function() "stopped",
    util_parallel_stop = function() {
      called <<- TRUE
      invisible(NULL)
    }
  )

  util_deferred_parallel_stop()

  expect_false(called)
})

test_that("plot saving helper writes a sized ggplot image", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  file <- tempfile(fileext = ".png")
  withr::defer(unlink(file, force = TRUE))

  plot <- ggplot2::ggplot(
    data.frame(x = factor(c("left", "right")), y = c(1, 2)),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_col()

  expect_silent(util_save_with_inner_size(
    plot = plot,
    filename = file,
    inner_width = 2,
    inner_height = 1.5,
    dpi = 72
  ))
  expect_true(file.exists(file))
  expect_gt(file.info(file)$size, 0)
})

test_that("plot saving helper writes a sized patchwork image", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  file <- tempfile(fileext = ".png")
  withr::defer(unlink(file, force = TRUE))

  data <- data.frame(
    x = factor(c("left", "right")),
    y = c(1, 2)
  )
  left_plot <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_col()
  right_plot <- ggplot2::ggplot(data, ggplot2::aes(x = y, y = x)) +
    ggplot2::geom_point()
  patchwork_plot <- left_plot + right_plot + patchwork::plot_layout(
    widths = c(2, 1)
  )

  expect_silent(util_save_with_inner_size(
    plot = patchwork_plot,
    filename = file,
    inner_width = 3,
    inner_height = 1.5,
    dpi = 72
  ))
  expect_true(file.exists(file))
  expect_gt(file.info(file)$size, 0)
})

test_that("plot saving helper writes an error-thumbnail fallback", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  file <- tempfile(fileext = ".png")
  withr::defer(unlink(file, force = TRUE))
  unsupported_plot <- structure(
    list(),
    sizing_hints = list(figure_type_id = "pairs_plot")
  )

  expect_warning(
    util_save_with_inner_size(
      plot = unsupported_plot,
      filename = file,
      inner_width = 2,
      inner_height = 1,
      dpi = 72
    ),
    "Could not write"
  )
  expect_true(file.exists(file))
  expect_gt(file.info(file)$size, 0)
})
