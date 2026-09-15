test_that("reflection works", {
  skip_on_cran() # snot really a problem, if not working. also obvious, if buggy. slow. # nolint: line_length_linter.
  util_load_manual()
  expect("titles" %in% names(..manual),
    failure_message = "load manual did not read titles"
  )
  expect_true(
    any(
      vapply(
        mget(setdiff(ls(.indicator_or_descriptor), "man_hash"),
          envir = .indicator_or_descriptor
        ), identity,
        FUN.VALUE = logical(1)
      )
    ),
    info = "Any Indicator in Package"
  )
})

test_that("util_fix_backticks normalizes cross-item Rd item names", {
  skip_on_cran()
  man_dir <- tempfile("man")
  dir.create(man_dir)
  rd_file <- file.path(man_dir, "example.Rd")
  writeLines(c(
    "\\name{example}",
    "\\arguments{",
    "\\item{cross-item_level}{old escaped item}",
    "\\item{other}{unchanged}",
    "}"
  ), rd_file, useBytes = TRUE)

  expect_identical(util_fix_backticks(man_dir), rd_file)
  fixed <- readLines(rd_file, warn = FALSE)
  expect_true(any(grepl("\\\\item\\{`cross-item_level`\\}", fixed)))
  expect_true(any(grepl("\\\\item\\{other\\}", fixed)))
})

test_that("util_load_manual rebuilds small manual reflection caches", {
  skip_on_cran()
  skip_if_not_installed("Rdpack")

  manual_before <- as.list(..manual, all.names = TRUE)
  indicator_before <- as.list(..indicator_or_descriptor, all.names = TRUE)
  on.exit({
    rm(list = ls(..manual, all.names = TRUE), envir = ..manual)
    list2env(manual_before, envir = ..manual)
    rm(
      list = ls(..indicator_or_descriptor, all.names = TRUE),
      envir = ..indicator_or_descriptor
    )
    list2env(indicator_before, envir = ..indicator_or_descriptor)
  }, add = TRUE)

  make_rd <- function(name, title, description) {
    rd_file <- tempfile(fileext = ".Rd")
    writeLines(c(
      sprintf("\\name{%s}", name),
      sprintf("\\title{%s}", title),
      sprintf("\\description{%s}", description)
    ), rd_file)
    tools::parse_Rd(rd_file)
  }

  docs <- list(
    acc_indicator = make_rd(
      "acc_indicator",
      "Indicator title",
      "Indicator description \\link{Indicator}."
    ),
    des_descriptor = make_rd(
      "des_descriptor",
      "Descriptor title",
      "Descriptor description \\link{Descriptor}."
    ),
    int_both = make_rd(
      "int_both",
      "Both title",
      "Conflicting description \\link{Indicator} and \\link{Descriptor}."
    ),
    con_neither = make_rd(
      "con_neither",
      "Neither title",
      "Description without classification."
    )
  )
  target <- tempfile(fileext = ".RData")
  target2 <- tempfile(fileext = ".RData")

  orig_ls <- base::ls
  fake_ls <- function(name, pos = -1L, envir = as.environment(pos),
    all.names = FALSE, pattern, sorted = TRUE) {
    if (!missing(pattern) && identical(pattern, "^(des|int|com|con|acc)_")) {
      return(names(docs))
    }
    if (missing(pattern)) {
      orig_ls(name = name, pos = pos, envir = envir, all.names = all.names,
        sorted = sorted)
    } else {
      orig_ls(name = name, pos = pos, envir = envir, all.names = all.names,
        pattern = pattern, sorted = sorted)
    }
  }

  with_mocked_bindings(
    ls = fake_ls,
    {
      testthat::local_mocked_bindings(
        util_ensure_suggested = function(pkg, ..., and_import = character(),
          err = TRUE) {
          if (length(and_import)) {
            assign("is_dev_package", function(...) FALSE, parent.frame())
          }
          TRUE
        }
      )
      with_mocked_bindings(
        Rdo_fetch = function(f, ...) docs[[f]],
        {
          expect_warning(
            expect_warning(
              util_load_manual(rebuild = TRUE, target = target,
                target2 = target2, man_hash = "tiny-hash"),
              "set to both"
            ),
            "not correctly available"
          )
        },
        .package = "Rdpack"
      )
    },
    .package = "base"
  )

  expect_true(file.exists(target))
  expect_true(file.exists(target2))
  expect_identical(..manual$titles$acc_indicator, "Indicator title")
  expect_identical(..manual$descriptions$des_descriptor,
    "Descriptor description Descriptor.")
  expect_true(get("acc_indicator", envir = ..indicator_or_descriptor))
  expect_false(get("des_descriptor", envir = ..indicator_or_descriptor))
  expect_false(get("int_both", envir = ..indicator_or_descriptor))
  expect_false(get("con_neither", envir = ..indicator_or_descriptor))
  expect_identical(..manual$man_hash, "tiny-hash")
  expect_identical(get("man_hash", envir = ..indicator_or_descriptor),
    "tiny-hash")
})

test_that("util_load_manual handles missing Rdpack reflection support", {
  skip_on_cran()

  manual_before <- as.list(..manual, all.names = TRUE)
  indicator_before <- as.list(..indicator_or_descriptor, all.names = TRUE)
  on.exit({
    rm(list = ls(..manual, all.names = TRUE), envir = ..manual)
    list2env(manual_before, envir = ..manual)
    rm(
      list = ls(..indicator_or_descriptor, all.names = TRUE),
      envir = ..indicator_or_descriptor
    )
    list2env(indicator_before, envir = ..indicator_or_descriptor)
  }, add = TRUE)

  target <- tempfile(fileext = ".RData")
  target2 <- tempfile(fileext = ".RData")
  fns <- c("acc_missing_doc", "des_missing_doc")
  orig_ls <- base::ls
  fake_ls <- function(name, pos = -1L, envir = as.environment(pos),
    all.names = FALSE, pattern, sorted = TRUE) {
    if (!missing(pattern) && identical(pattern, "^(des|int|com|con|acc)_")) {
      return(fns)
    }
    if (missing(pattern)) {
      orig_ls(name = name, pos = pos, envir = envir, all.names = all.names,
        sorted = sorted)
    } else {
      orig_ls(name = name, pos = pos, envir = envir, all.names = all.names,
        pattern = pattern, sorted = sorted)
    }
  }

  with_mocked_bindings(
    ls = fake_ls,
    {
      testthat::local_mocked_bindings(
        util_ensure_suggested = function(pkg, ..., and_import = character(),
          err = TRUE) {
          if (length(and_import)) {
            assign("is_dev_package", function(...) FALSE, parent.frame())
          }
          identical(pkg, "pkgload")
        }
      )
      expect_silent(util_load_manual(rebuild = TRUE, target = target,
          target2 = target2, man_hash = "no-rd"))
    },
    .package = "base"
  )

  expect_true(file.exists(target))
  expect_true(file.exists(target2))
  expect_identical(..manual$titles$acc_missing_doc, "")
  expect_identical(..manual$descriptions$des_missing_doc, "")
  expect_false(get("acc_missing_doc", envir = ..indicator_or_descriptor))
  expect_false(get("des_missing_doc", envir = ..indicator_or_descriptor))
})
