skip_on_cran()

test_that("online references are generated without render-time network state", {
  expect_false(exists("util_online_ref_begin_render", mode = "function"))
  expect_false(exists("util_online_ref_end_render", mode = "function"))
  expect_false(exists("util_online_ref_check_url", mode = "function"))

  refs <- util_online_ref_candidates("com_item_missingness")

  expect_identical(
    refs$concept,
    paste0(
      "https://dataquality.qihs.uni-greifswald.de/",
      "VIN_com_item_missingness.html"
    )
  )
  expect_identical(
    refs$implementation,
    paste0(
      "https://dataquality.qihs.uni-greifswald.de/",
      "VIN_com_impl_item_missingness.html"
    )
  )
  expect_identical(
    util_online_ref("com_item_missingness"),
    refs$implementation
  )
  expect_identical(
    util_online_ref("com_item_missingness", target = "concept"),
    refs$concept
  )
})

test_that("online references reject report aliases", {
  skip_on_cran()

  expect_identical(
    util_online_ref_candidates("acc_cat_distributions"),
    list(
      concept = paste0(
        "https://dataquality.qihs.uni-greifswald.de/",
        "VIN_acc_cat_distributions.html"
      ),
      implementation = paste0(
        "https://dataquality.qihs.uni-greifswald.de/",
        "VIN_acc_impl_cat_distributions.html"
      )
    )
  )

  expect_error(
    util_online_ref_candidates("acc_cat_distributions_observer"),
    "Internal error, sorry, please report"
  )
  expect_error(
    util_online_ref_candidates("acc_distributions_ecdf_device"),
    "Internal error, sorry, please report"
  )
})

test_that("report rendering does not probe online references from R", {
  skip_if_not_installed("DT")
  skip_if_not_installed("stringdist")
  skip_if_not_installed("markdown")

  study_data <- data.frame(
    v0001 = c(1, 1, NA, 2, 2, 1),
    stringsAsFactors = FALSE
  )
  meta_data <- prep_create_meta(
    VAR_NAMES = "v0001",
    LABEL = "Local variable",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    VALUE_LABELS = "1 = yes | 2 = no",
    MISSING_LIST_TABLE = NA_character_,
    MISSING_LIST = SPLIT_CHAR,
    character.only = TRUE
  )

  report <- dq_report2(
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    dimensions = "Completeness",
    filter_indicator_functions = "^com_item_missingness$",
    filter_result_slots = "^SummaryTable$",
    cores = NULL
  )

  target <- withr::local_tempdir("dq-offline-render")

  expect_error(
    with_mocked_bindings(
      curlGetHeaders = function(...) {
        stop("rendering attempted an R-side network request", call. = FALSE)
      },
      suppressMessages(suppressWarnings(print(
        report,
        dir = target,
        view = FALSE,
        force_overwrite = TRUE
      ))),
      .package = "base"
    ),
    NA
  )
})

test_that("rendered online reference links carry browser-side state attributes", { # nolint: line_length_linter.
  template_dir <- system.file(
    "templates", "default",
    package = "dataquieR"
  )
  templates <- c("single_indicator.html", "single_indicator_with_menu.html")

  for (template in templates) {
    html <- readLines(file.path(template_dir, template), warn = FALSE)
    expect_true(any(grepl("class=\"dq-online-ref\"", html, fixed = TRUE)))
    expect_true(any(grepl("data-dq-online-ref-href", html, fixed = TRUE)))
    expect_true(any(grepl(
      "data-dq-online-ref-fallback-href",
      html,
      fixed = TRUE
    )))
    expect_true(any(grepl("data-dq-online-ref-state", html, fixed = TRUE)))
  }
})

test_that("browser-side online reference checks use CORS-readable requests", {
  script <- readLines(
    system.file("menu", "script.js", package = "dataquieR"),
    warn = FALSE
  )
  script <- paste(script, collapse = "\n")

  expect_true(grepl("function initOnlineReferenceLinks", script, fixed = TRUE))
  expect_true(grepl("function probeOnlineReference", script, fixed = TRUE))
  expect_true(grepl("data-dq-online-ref-fallback-href", script, fixed = TRUE))
  expect_true(grepl("data-dq-online-ref-active-href", script, fixed = TRUE))
  expect_true(grepl("mode: \"cors\"", script, fixed = TRUE))
  expect_true(grepl("credentials: \"omit\"", script, fixed = TRUE))
  expect_false(grepl("mode: \"no-cors\"", script, fixed = TRUE))
})
