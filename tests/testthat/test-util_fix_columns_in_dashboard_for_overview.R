skip_on_cran()

test_that("util_fix_columns_in_dashboard_for_overview(): keeps variable-name helper column", { # nolint: line_length_linter.
  skip_if_not_installed("jsonlite")

  dashboard <- data.frame(
    href = "VAR_a.html#a.call",
    value = "42",
    popup_href = "VAR_a.html#nm=call.a",
    title = "A: call",
    VAR_NAMES = "a",
    LABEL = "A",
    Figure = NA_character_,
    Graph = NA_character_,
    fq_VARNAME = "segment-a",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  dataquieR:::util_translated_colnames(dashboard) <- # nolint
    dataquieR:::util_translate(colnames(dashboard)) # nolint

  fixed <- dataquieR:::util_fix_columns_in_dashboard_for_overview( # nolint
    dashboard,
    image_dir = withr::local_tempdir("dashboard-images")
  )

  fixed_cols <- dataquieR:::util_untranslated_colnames(fixed) # nolint
  expect_s3_class(fixed, "data.frame")
  expect_true("fq_VARNAME" %in% fixed_cols)
  expect_false("href" %in% fixed_cols)
  expect_false("popup_href" %in% fixed_cols)
  expect_false("title" %in% fixed_cols)
})

test_that("util_fix_columns_in_dashboard_for_overview(): tolerates narrow dashboards", { # nolint: line_length_linter.
  skip_if_not_installed("jsonlite")

  dashboard <- data.frame(
    href = c("VAR_a.html#a.call", "VAR_b.html#b.call"),
    value = c("0", "1"),
    popup_href = c("VAR_a.html#nm=call.a", "VAR_b.html#nm=call.b"),
    title = c("A: call", "B: call"),
    VAR_NAMES = c("a", "b"),
    LABEL = c("A", "B"),
    fq_VARNAME = c("segment-a", "segment-b"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  dataquieR:::util_translated_colnames(dashboard) <- # nolint
    dataquieR:::util_translate(colnames(dashboard)) # nolint

  fixed <- dataquieR:::util_fix_columns_in_dashboard_for_overview( # nolint
    dashboard,
    image_dir = withr::local_tempdir("dashboard-images")
  )

  fixed_cols <- dataquieR:::util_untranslated_colnames(fixed) # nolint
  expect_s3_class(fixed, "data.frame")
  expect_equal(nrow(fixed), nrow(dashboard))
  expect_true(all(c("VAR_NAMES", "LABEL", "fq_VARNAME") %in% fixed_cols))
  expect_false(any(c("Figure", "Graph") %in% fixed_cols))
})

test_that("util_fix_columns_in_dashboard_for_overview(): falls back to names", { # nolint: line_length_linter.
  skip_if_not_installed("jsonlite")

  dashboard <- data.frame(
    href = "VAR_a.html#a.call",
    value = "42",
    popup_href = "VAR_a.html#nm=call.a",
    title = "A: call",
    VAR_NAMES = "a",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  dataquieR:::util_translated_colnames(dashboard) <- # nolint
    dataquieR:::util_translate(colnames(dashboard)) # nolint

  fixed <- dataquieR:::util_fix_columns_in_dashboard_for_overview( # nolint
    dashboard,
    image_dir = withr::local_tempdir("dashboard-images")
  )

  expect_identical(fixed$VAR_NAMES, "a")
})

test_that("util_fix_columns_in_dashboard_for_overview(): keeps empty tables", {
  skip_if_not_installed("jsonlite")

  dashboard <- data.frame(
    href = character(),
    VAR_NAMES = character(),
    stringsAsFactors = FALSE
  )

  fixed <- dataquieR:::util_fix_columns_in_dashboard_for_overview( # nolint
    dashboard,
    image_dir = withr::local_tempdir("dashboard-images")
  )

  expect_identical(fixed, dashboard)
})

test_that("util_extract_datauri_pngs(): writes and deduplicates embedded images", { # nolint: line_length_linter.
  skip_if_not_installed("jsonlite")

  image_dir <- withr::local_tempdir("dashboard-images")
  payload <- "AQIDBA=="
  png_uri <- paste0("data:image/png;base64,", payload)
  dashboard <- data.frame(
    Figure = c(
      paste0("<img src=\"", png_uri, "\">"),
      paste0("<span><img src='", png_uri, "'></span>")
    ),
    Other = c("plain", NA_character_),
    stringsAsFactors = FALSE
  )

  fixed <- dataquieR:::util_extract_datauri_pngs( # nolint
    dashboard,
    image_dir = image_dir
  )

  image_href <- file.path(
    basename(normalizePath(image_dir, winslash = "/", mustWork = FALSE)),
    "img_0001.png"
  )
  image_file <- file.path(image_dir, "img_0001.png")

  expect_identical(list.files(image_dir, pattern = "\\.png$"), "img_0001.png")
  expect_false(any(grepl("data:image/png;base64", fixed$Figure, fixed = TRUE)))
  expect_true(all(grepl(image_href, fixed$Figure, fixed = TRUE)))
  expect_true(grepl(
    paste0("src=\"", image_href, "\""),
    fixed$Figure[[1]],
    fixed = TRUE
  ))
  expect_true(grepl(
    paste0("src='", image_href, "'"),
    fixed$Figure[[2]],
    fixed = TRUE
  ))
  expect_identical(
    readBin(image_file, what = "raw", n = 4L),
    jsonlite::base64_dec(payload)
  )
})

test_that("util_extract_datauri_pngs(): ignores non-src data URI text", {
  skip_if_not_installed("jsonlite")

  image_dir <- withr::local_tempdir("dashboard-images")
  dashboard <- data.frame(
    Figure = "data:image/png;base64,AQIDBA==",
    stringsAsFactors = FALSE
  )

  fixed <- dataquieR:::util_extract_datauri_pngs( # nolint
    dashboard,
    image_dir = image_dir
  )

  expect_identical(fixed$Figure, dashboard$Figure)
  expect_identical(list.files(image_dir, pattern = "\\.png$"), character())
})
