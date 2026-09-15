skip_on_cran()

test_that("dataquieR_resultset2 class", {
  skip_on_cran() # slow, parallel, ...
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("storr")

  # Use a fixed db_dir and unlink() locally to inspect storr persistence.
  db_dir <- withr::local_tempdir()
  db_dir2 <- withr::local_tempdir()

  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx") # nolint: line_length_linter.

  study_data <- head(prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE), 100) # nolint: line_length_linter.
  meta_data <- prep_get_data_frame("item_level")

  mlt <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx| missing_table") # nolint: line_length_linter.

  prep_purge_data_frame_cache()

  prep_add_data_frames(`missing_table` = mlt)

  invisible(testthat::capture_output_lines(gc(full = TRUE, verbose = FALSE)))

  sd0 <- study_data[, 1:5]
  sd0$v00012 <- study_data$v00012
  md0 <- subset(meta_data, VAR_NAMES %in% colnames(sd0))
  md0$PART_VAR <- NULL
  md1 <- md0
  md1$LABEL <- c(
    "CENTER_0",
    "",
    "CENTER_0 DUPLICATE", # will become a duplicated label
    "CENTER_0", # direct duplication of the first label
    "Have you been physically vigorously active in the past 12 hours ('physically vigorously active' means at least 30 minutes of jogging or fast cycling, digging up your garden, carrying heavy objects weighing more than 10 kg for a long time, or similar physical activities)?", # very long label # nolint: line_length_linter.
    "Hybpvaitp1hpvamal3mojofcduygchowmt1kfaltospa"
  ) # legacy abbreviation-like label
  md1$VAR_NAMES[2] <- "yOvCzPY60JRjmrYb16Tsd6qMymal4B5Skw9rZ5PHSCtaBqOVglAKcguPkQhakampFJcC8xqLbZJs7kZUdKH804pbOmM5ORPVabrkEkVkiWbakWiixZ99NRYF6BP8SRxzNYY2tED7DjmhMUwk0t674RjH828jq9zoTJgDxYP6nEdHBxhmXJh0ClCPjGsi1q" # very long variable name that should get caught and not be used as label as it is # nolint: line_length_linter.
  colnames(sd0)[2] <- md1$VAR_NAMES[2]

  sf <- prep_create_storr_factory(
    namespace = "Test",
    db_dir = db_dir
  )

  suppressWarningsMatching(
    report <- dq_report2(sd0, md1,
      storr_factory =
        sf,
      label_col = VAR_NAMES,
      cores = NULL,
      dimensions =
        c(
          "Integrity",
          "Consistency"
        )
    ),
    "(Some variables have labels with more than 60 characters in.+|Unique labels are required|Lost 16.7% of the study data because of missing/not assignable metadata|Need.+VAR_NAMES.+discard.+|.+dummy names|Some variables have no label in .+LABEL)" # nolint: line_length_linter.
  )
  report0 <- report

  testidx <- head(which(names(report) == "des_summary_categorical.v00000"), 1)
  expect_length(testidx, 1)

  x0 <- force(report0[[testidx]]$SummaryTable)
  expect_s3_class(report0[["con_limit_deviations.v00004"]], "master_result")
  expect_null(report0[["con_limit_deviations.v0000xx4"]])


  suppressWarningsMatching(
    {
      report[[1]] <- report[[1]]
      report[["con_limit_deviations.v00004"]] <-
        report[["con_limit_deviations.v00004"]]

      expect_error(
        report[["con_limit_deviations.v0000xx4"]] <- NULL,
        regexp = "Extending reports is not supported"
      )
    },
    regexps = ".*inefficient.*"
  )

  x1 <- force(report[[testidx]]$SummaryTable)
  # Use save() locally to inspect x0, x1, and report in one RData file.

  expect_s3_class(report[["con_limit_deviations.v00004"]], "master_result")
  expect_null(report[["con_limit_deviations.v0000xx4"]])

  report0 <- prep_set_backend(report, NULL)
  report2 <- prep_set_backend(
    report0,
    prep_create_storr_factory(
      namespace = "Test",
      db_dir = db_dir2
    )
  )
  report3 <- prep_load_report_from_backend(
    namespace = "Test",
    db_dir = db_dir2
  )

  expect_equal(
    report0$`int_all_datastructure_dataframe.[ALL]`,
    report$`int_all_datastructure_dataframe.[ALL]`,
    ignore_attr = "system.time"
  )

  expect_equal(
    report2$`int_all_datastructure_dataframe.[ALL]`,
    report$`int_all_datastructure_dataframe.[ALL]`,
    ignore_attr = "system.time"
  )

  expect_equal(
    report3$`int_all_datastructure_dataframe.[ALL]`,
    report$`int_all_datastructure_dataframe.[ALL]`,
    ignore_attr = "system.time"
  )

  # Historical covr skip for this block removed here.
  #                   message = "Does not work, if instrumented")


  expect_s3_class(x0, "TableSlot")
  expect_s3_class(x1, "TableSlot")
})

test_that("storr helper functions", {
  skip_if_not_installed("storr")
  sf1 <- prep_create_storr_factory()
  so1 <- sf1()
  sf2 <- prep_create_storr_factory(namespace = "Test")
  so2 <- sf2()
  expect_equal(util_get_storr_att_namespace(so1), "objects.attributes")
  expect_equal(util_get_storr_att_namespace(so2), "Test.attributes")
  expect_equal(util_get_storr_stat_namespace(so1), "objects.status")
  expect_equal(util_get_storr_stat_namespace(so2), "Test.status")
  expect_equal(util_get_storr_summ_namespace(so1), "objects.summary")
  expect_equal(util_get_storr_summ_namespace(so2), "Test.summary")
})

test_that("dataquieR_resultset2 list backend supports simple accessors", {
  first <- structure(list(SummaryTable = data.frame(x = 1)),
    class = "dataquieR_result")
  second <- structure(list(SummaryTable = data.frame(y = 2)),
    class = "dataquieR_result")
  report <- structure(
    list(call_a.x = first, call_b.y = second),
    class = "dataquieR_resultset2",
    all_calls = list(call_a.x = quote(call_a(x)),
      call_b.y = quote(call_b(y))),
    rn = c("x", "y"),
    cn = c("call_a", "call_b"),
    names = c("call_a.x", "call_b.y")
  )

  expect_identical(report[[1]], first)
  expect_identical(report[["call_b.y"]], second)
  expect_identical(report$call_a.x, first)
  expect_error(report[[0]], "less than one element")
  expect_null(report[[NA_integer_]])
  expect_null(report[[NA_character_]])
  expect_null(report[["missing"]])

  expect_equal(names(report[1]), "call_a.x")
  expect_equal(names(report[c(TRUE, FALSE)]), "call_a.x")
  expect_error(report[list(1)], "numbers, logical vectors or names")
  expect_identical(unclass(as.list(report)), unclass(report))
})

test_that("dataquieR_resultset2 reports dimensions from matrix metadata", {
  skip_on_cran()

  make_result <- function(id) {
    structure(list(SummaryTable = data.frame(id = id)),
      class = "dataquieR_result")
  }

  report <- structure(
    list(call_a.x = make_result("ax"),
      call_a.y = make_result("ay"),
      call_b.x = make_result("bx")),
    class = "dataquieR_resultset2",
    all_calls = list(call_a.x = quote(call_a(x)),
      call_a.y = quote(call_a(y)),
      call_b.x = quote(call_b(x))),
    rn = c("x", "y", "x"),
    cn = c("call_a", "call_a", "call_b"),
    names = c("call_a.x", "call_a.y", "call_b.x"),
    matrix_list = structure(
      list(),
      row_indices = c(y = 2, x = 1),
      col_indices = c(call_b = 2, call_a = 1)
    )
  )

  expect_identical(resnames(report), "SummaryTable")
  expect_identical(dimnames(report), list(
    c("x", "y"),
    c("call_a", "call_b"),
    "SummaryTable"
  ))
  expect_identical(dim(report), c(2L, 2L, 1L))

  attr(report, "resnames") <- c("SummaryPlot", "SummaryTable")
  expect_identical(resnames(report), c("SummaryPlot", "SummaryTable"))
})

test_that("dataquieR_resultset2 subsets by row, column, result, and drop", {
  skip_on_cran()

  make_result <- function(id) {
    structure(
      list(
        SummaryTable = data.frame(Variables = id),
        ResultData = data.frame(raw = id)
      ),
      class = "dataquieR_result"
    )
  }

  report <- structure(
    list(
      call_a.x = make_result("ax"),
      call_a.y = make_result("ay"),
      call_b.x = make_result("bx")
    ),
    class = "dataquieR_resultset2",
    all_calls = list(
      call_a.x = quote(call_a(x)),
      call_a.y = quote(call_a(y)),
      call_b.x = quote(call_b(x))
    ),
    rn = c("x", "y", "x"),
    cn = c("call_a", "call_a", "call_b"),
    names = c("call_a.x", "call_a.y", "call_b.x")
  )

  subset <- report["x", c("call_b", "call_a")]
  expect_equal(names(subset), c("call_b.x", "call_a.x"))

  dropped <- report["x", "call_a", "SummaryTable", drop = TRUE]
  expect_s3_class(dropped, "data.frame")
  expect_equal(as.character(dropped$Variables), "ax")

  expect_error(report["x", c("call_a", "call_a")], "duplicated")
  expect_error(report["x", factor("call_a")], "not a vector")
  expect_error(report["x", "call_a", c("SummaryTable", "SummaryTable")],
    "duplicated")
})

test_that("dataquieR_resultset2 missing result-slot subsets stay empty", {
  skip_on_cran()

  make_result <- function(id) {
    structure(
      list(SummaryTable = data.frame(Variables = id)),
      class = "dataquieR_result"
    )
  }

  report <- structure(
    list(
      call_a.x = make_result("ax"),
      call_a.y = make_result("ay")
    ),
    class = "dataquieR_resultset2",
    all_calls = list(
      call_a.x = quote(call_a(x)),
      call_a.y = quote(call_a(y))
    ),
    rn = c("x", "y"),
    cn = c("call_a", "call_a"),
    names = c("call_a.x", "call_a.y")
  )

  empty_subset <- report[, "call_a", "MissingSlot"]
  expect_s3_class(empty_subset, "dataquieR_NULL")
  expect_s3_class(empty_subset, "dataquieR_result")
  expect_length(empty_subset, 2)

  dropped_empty <- report["x", "call_a", "MissingSlot", drop = TRUE]
  expect_s3_class(dropped_empty, "Slot")
  expect_s3_class(dropped_empty, "dataquieR_result")
  expect_s3_class(dropped_empty$MissingSlot, "dataquieR_NULL")
})

test_that("dataquieR_resultset2 list backend replacement stays bounded", {
  first <- structure(list(SummaryTable = data.frame(x = 1)),
    class = "dataquieR_result")
  second <- structure(list(SummaryTable = data.frame(y = 2)),
    class = "dataquieR_result")
  replacement <- structure(list(SummaryTable = data.frame(z = 3)),
    class = "dataquieR_result")
  report <- structure(
    list(call_a.x = first, call_b.y = second),
    class = "dataquieR_resultset2",
    all_calls = list(call_a.x = quote(call_a(x)),
      call_b.y = quote(call_b(y))),
    rn = c("x", "y"),
    cn = c("call_a", "call_b"),
    names = c("call_a.x", "call_b.y")
  )

  report[[1]] <- second
  expect_identical(report[[1]], second)

  report[["call_b.y"]] <- NULL
  expect_s3_class(unclass(report)[["call_b.y"]], "dataquieR_NULL")

  fresh_report <- structure(
    list(call_a.x = first, call_b.y = second),
    class = "dataquieR_resultset2",
    all_calls = list(call_a.x = quote(call_a(x)),
      call_b.y = quote(call_b(y))),
    rn = c("x", "y"),
    cn = c("call_a", "call_b"),
    names = c("call_a.x", "call_b.y")
  )

  replaced <- fresh_report
  replaced[] <- list(replacement)
  expect_identical(unclass(replaced)[[1]], replacement)
  expect_identical(unclass(replaced)[[2]], replacement)

  partly_replaced <- `.access_dq_rs2<-`(
    fresh_report,
    c(FALSE, TRUE),
    list(replacement)
  )
  expect_identical(unclass(partly_replaced)[[1]], first)
  expect_identical(unclass(partly_replaced)[[2]], replacement)

  expect_error(
    report[["new_call.z"]] <- first,
    "Extending reports is not supported"
  )
  expect_error(
    report[1, 1] <- list(first),
    "cannot write subsets"
  )
})

test_that("dataquieR_resultset2 list backend supports raw list access", {
  skip_on_cran()

  first <- structure(list(SummaryTable = data.frame(x = 1)),
    class = "dataquieR_result")
  second <- structure(list(SummaryTable = data.frame(y = 2)),
    class = "dataquieR_result")
  report <- structure(
    list(call_a.x = util_compress(first), call_b.y = second),
    class = "dataquieR_resultset2",
    all_calls = list(call_a.x = quote(call_a(x)),
      call_b.y = quote(call_b(y))),
    rn = c("x", "y"),
    cn = c("call_a", "call_b"),
    names = c("call_a.x", "call_b.y")
  )

  raw_result <- report[els = "call_a.x", as_raw = TRUE][[1]]
  expect_type(raw_result, "raw")
  expect_identical(util_decompress(raw_result), first)

  numeric_raw_result <- .access_dq_rs2(report, 1, as_raw = TRUE)[[1]]
  expect_type(numeric_raw_result, "raw")
  expect_identical(util_decompress(numeric_raw_result), first)

  logical_raw_result <- .access_dq_rs2(report, c(TRUE, FALSE),
    as_raw = TRUE
  )[[1]]
  expect_type(logical_raw_result, "raw")
  expect_identical(util_decompress(logical_raw_result), first)

  logical_result <- report[c(TRUE, FALSE)][[1]]
  expect_s3_class(logical_result, "dataquieR_result")
  expect_identical(logical_result, first)

  replacement <- structure(list(SummaryTable = data.frame(z = 3)),
    class = "dataquieR_result")
  report[els = c("call_a.x", "call_b.y")] <- list(replacement, first)

  expect_identical(report[["call_a.x"]], replacement)
  expect_identical(report[["call_b.y"]], first)
  expect_error(
    `.access_dq_rs2<-`(report, list("call_a.x"), list(replacement)),
    "numbers, logical vectors or names"
  )
  expect_error(
    .access_dq_rs2(report, list("call_a.x")),
    "numbers, logical vectors or names"
  )
})

test_that("dataquieR_resultset2 storr backend supports raw access", {
  skip_on_cran()
  skip_if_not_installed("storr")

  first <- structure(list(SummaryTable = data.frame(x = 1)),
    class = "dataquieR_result")
  second <- structure(list(SummaryTable = data.frame(y = 2)),
    class = "dataquieR_result")
  replacement <- structure(list(SummaryTable = data.frame(z = 3)),
    class = "dataquieR_result")
  make_call <- function(expr, var_name) {
    attr(expr, VAR_NAMES) <- var_name
    attr(expr, STUDY_SEGMENT) <- NA_character_
    expr
  }
  report <- structure(
    list(des_summary.x = first,
      con_limit_deviations.y = second),
    class = "dataquieR_resultset2",
    all_calls = list(des_summary.x = make_call(quote(des_summary(x)), "x"),
      con_limit_deviations.y = make_call(
        quote(con_limit_deviations(y)),
        "y"
      )),
    rn = c("x", "y"),
    cn = c("des_summary", "con_limit_deviations"),
    names = c("des_summary.x", "con_limit_deviations.y")
  )

  backend_report <- prep_set_backend(
    report,
    prep_create_storr_factory(
      namespace = "Test",
      db_dir = withr::local_tempdir()
    )
  )
  backend <- util_get_storr_object_from_report(backend_report)
  backend$set("des_summary.x", util_compress(first))

  raw_result <- backend_report[els = "des_summary.x", as_raw = TRUE][[1]]
  expect_type(raw_result, "raw")
  expect_identical(util_decompress(raw_result), first)
  expect_identical(backend_report[[1]], first)
  expect_identical(backend_report[["con_limit_deviations.y"]], second)
  expect_null(backend_report[["missing"]])

  backend$del("des_summary.x")
  repaired_missing <- backend_report[[1]]
  expect_s3_class(repaired_missing, "dataquieR_result")
  expect_true(length(util_attr(repaired_missing, "error", exact = TRUE)) > 0)

  expect_warning(as.list(backend_report), "as.list is inefficient")
  expect_no_warning(lapply(backend_report, identity))

  backend_report[[1]] <- replacement
  expect_identical(backend_report[[1]], replacement)
  backend_report[["con_limit_deviations.y"]] <- first
  expect_identical(backend_report[["con_limit_deviations.y"]], first)
  expect_error(
    backend_report[["new_result.z"]] <- replacement,
    "Extending reports is not supported"
  )
})

test_that("prep_set_backend guards occupied storr backends", {
  skip_on_cran()
  skip_if_not_installed("storr")

  first <- structure(list(SummaryTable = data.frame(x = 1)),
    class = "dataquieR_result")
  report <- structure(
    list(des_summary.x = first),
    class = "dataquieR_resultset2",
    all_calls = list(des_summary.x = quote(des_summary(x))),
    rn = "x",
    cn = "des_summary",
    names = "des_summary.x"
  )

  factory <- prep_create_storr_factory(
    namespace = "Test",
    db_dir = withr::local_tempdir()
  )
  occupied_backend <- factory()
  occupied_backend$set("occupied", 1)

  expect_error(
    prep_set_backend(report, factory),
    "Your storr-object is not empty"
  )
  expect_message(
    backend_report <- prep_set_backend(report, factory, amend = TRUE),
    "so I'll amend the storage object"
  )
  expect_s3_class(backend_report, "dataquieR_resultset2")
  expect_s3_class(util_get_storr_object_from_report(backend_report), "storr")
})

test_that("dataquieR_resultset2 summary FUN path uses report matrix names", {
  make_result <- function(id) {
    structure(list(SummaryTable = data.frame(id = id)),
      class = "dataquieR_result")
  }

  report <- structure(
    list(call_a.x = make_result("ax"),
      call_a.y = make_result("ay"),
      call_b.x = make_result("bx"),
      call_b.y = make_result("by")),
    class = "dataquieR_resultset2",
    all_calls = list(call_a.x = quote(call_a(x)),
      call_a.y = quote(call_a(y)),
      call_b.x = quote(call_b(x)),
      call_b.y = quote(call_b(y))),
    rn = c("x", "y", "x", "y"),
    cn = c("call_a", "call_a", "call_b", "call_b"),
    names = c("call_a.x", "call_a.y", "call_b.x", "call_b.y"),
    matrix_list = structure(
      list(),
      row_indices = c(x = 1, y = 2),
      col_indices = c(call_a = 1, call_b = 2)
    )
  )

  calls <- summary(
    report,
    aspect = "error",
    FUN = function(result, aspect, collapse, rn, cn) {
      paste(result$SummaryTable$id, aspect, collapse, rn, cn, sep = "|")
    },
    collapse = " / "
  )

  expect_equal(dim(calls), c(2L, 2L))
  expect_equal(rownames(calls), c("x", "y"))
  expect_equal(colnames(calls), c("call_a", "call_b"))
  expect_equal(calls["x", "call_a"], "ax|error| / |x|call_a")
  expect_equal(calls["y", "call_b"], "by|error| / |y|call_b")
})

test_that(
  "dataquieR_resultset2 summary FUN path returns empty matrix for no cells",
  {
    report <- structure(
      list(),
      class = "dataquieR_resultset2",
      all_calls = list(),
      rn = character(0),
      cn = character(0),
      names = character(0),
      matrix_list = structure(
        list(),
        row_indices = c(x = 1),
        col_indices = c(call_all = 1)
      )
    )

    calls <- summary(report, aspect = "error", FUN = function(...) "unused")

    expect_equal(dim(calls), c(0L, 2L))
    expect_equal(colnames(calls), c(VAR_NAMES, STUDY_SEGMENT))
  }
)

test_that("summary row maxima ignore process metrics", {
  skip_on_cran()

  result <- data.frame(
    VAR_NAMES = c("v1", "v1", "v2", "v2"),
    indicator_metric = c("value", "CAT_process", "value", "MSG_note"),
    class = c("cat2", "cat5", "cat4", "cat1"),
    stringsAsFactors = FALSE
  )
  labels <- list(cat1 = "green", cat2 = "yellow", cat3 = "orange",
    cat4 = "red", cat5 = "black", `NA` = "missing")
  colors <- list(cat1 = "#00ff00", cat2 = "#ffff00", cat3 = "#ff9900",
    cat4 = "#ff0000", cat5 = "#000000", `NA` = "#ffffff")
  order_of <- c(cat1 = 1L, cat2 = 2L, cat3 = 3L, cat4 = 4L, cat5 = 5L,
    `NA` = NA_integer_)
  filter_of <- list(cat1 = "green", cat2 = "yellow", cat3 = "orange",
    cat4 = "red", cat5 = "black", `NA` = "missing")
  label_map <- c(v1 = "Variable one", v2 = "Variable two")

  rowmaxes <- util_compute_rowmaxes(
    result = result,
    labels = labels,
    colors = colors,
    order_of = order_of,
    filter_of = filter_of,
    labels_of_var_names_in_report = label_map
  )

  expect_equal(rowmaxes$VAR_NAMES, c("v1", "v2"))
  expect_equal(as.character(rowmaxes$rowmax), c("cat2", "cat4"))
  expect_equal(unname(rowmaxes$label), c("yellow", "red"))
  expect_equal(unname(rowmaxes$color), c("#ffff00", "#ff0000"))
  expect_equal(unname(rowmaxes$order), c(2L, 4L))
  expect_named(rowmaxes$cell_text, c("Variable one", "Variable two"))
  expect_true(all(grepl("href=", rowmaxes$cell_text, fixed = TRUE)))
})

test_that("summary row maxima handle missing structural columns", {
  skip_on_cran()

  labels <- list(cat1 = "green", cat2 = "yellow", cat3 = "orange",
    cat4 = "red", cat5 = "black", `NA` = "missing")
  colors <- list(cat1 = "#00ff00", cat2 = "#ffff00", cat3 = "#ff9900",
    cat4 = "#ff0000", cat5 = "#000000", `NA` = "#ffffff")
  order_of <- c(cat1 = 1L, cat2 = 2L, cat3 = 3L, cat4 = 4L, cat5 = 5L,
    `NA` = NA_integer_)
  filter_of <- labels

  rowmaxes <- util_compute_rowmaxes(
    result = data.frame(indicator_metric = "value"),
    labels = labels,
    colors = colors,
    order_of = order_of,
    filter_of = filter_of,
    labels_of_var_names_in_report = c()
  )

  expect_true(is.na(rowmaxes$VAR_NAMES[[1]]))
  expect_true(is.na(rowmaxes$rowmax[[1]]))
  expect_named(rowmaxes$cell_text, "NA")
  expect_true(grepl("href=", rowmaxes$cell_text, fixed = TRUE))
})
