skip_on_cran()

test_that(".onLoad works", {
  skip_if_not_installed("pkgload")
  skip_on_cran()
  try(.onLoad(), silent = TRUE)
  expect_equal(
    intersect(
      names(WELL_KNOWN_META_VARIABLE_NAMES),
      names(pkgload::pkg_env("dataquieR"))
    ),
    names(WELL_KNOWN_META_VARIABLE_NAMES)
  )
})

test_that(".set_properties works", {
  p <- as.list(dataquieR.properties)
  withr::defer(list2env(p, dataquieR.properties))
  .set_properties(list(a = "b"))
  expect_identical(dataquieR.properties$a, "b")
})

test_that("with_pipeline works", {
  expect_false(.dq2_globs$.called_in_pipeline)
  with_pipeline(expect_true(.dq2_globs$.called_in_pipeline))
})

test_that(".called_in_pipeline2 assumes dq_report2 when caller is unresolved", {
  skip_on_cran()

  testthat::with_mocked_bindings(
    .package = "rlang",
    caller_call = function(n) {
      if (identical(n, 1L)) {
        quote(dq_report2())
      } else {
        NULL
      }
    },
    caller_fn = function(...) NULL,
    expect_true(.called_in_pipeline2())
  )
})

test_that("decorator helper gates respect testthat options", {
  skip_on_cran()

  withr::local_options(dataquieR.test_decorator = FALSE,
    dataquieR.test_interactive_wrapper = FALSE,
    dataquieR.testdebug = FALSE)

  expect_true(util_testthat_blocks_interactive_wrapper())
  expect_false(util_should_run_decorator_preps())

  withr::local_options(dataquieR.test_interactive_wrapper = TRUE,
    dataquieR.test_decorator = TRUE)

  expect_false(util_testthat_blocks_interactive_wrapper())
  expect_true(util_should_run_decorator_preps())
  with_pipeline(expect_false(util_should_run_decorator_preps()))
})

test_that("decorator relevant variable names are collected from environments", {
  skip_on_cran()

  env <- new.env(parent = emptyenv())
  env$resp_vars <- c("v1", NA_character_)
  env$group_vars <- list("group", c("", "stratum"))
  env$unrelated <- "ignored"

  expect_equal(
    util_decorator_relevant_var_names(
      env,
      variable_arg_names = c("resp_vars", "group_vars", "missing")
    ),
    c("v1", "group", "stratum")
  )

  expect_equal(
    util_decorator_relevant_var_names(env, variable_arg_names = "missing"),
    character(0)
  )
})

test_that("decorator metadata aliases are applied conservatively", {
  skip_on_cran()

  meta_data_cross <- data.frame(VAR_NAMES = "v1")
  env_args <- list(meta_data_cross = meta_data_cross)

  fallback <- util_apply_meta_data_cross_fallback(
    env_args,
    what_i_have = "meta_data_cross"
  )
  expect_identical(fallback$meta_data_cross_item, meta_data_cross)
  expect_false("meta_data_cross" %in% names(fallback))

  already_present <- util_apply_meta_data_cross_fallback(
    env_args,
    what_i_have = c("meta_data_cross", "meta_data_cross_item")
  )
  expect_identical(already_present, env_args)

  env <- list2env(list(meta_data = data.frame(VAR_NAMES = "v2")),
    parent = emptyenv()
  )
  aliased <- util_apply_decorator_meta_data_alias(
    env_args = list(),
    canonical = "item_level",
    aliases = "meta_data",
    call_arg_names = "meta_data",
    env = env
  )
  expect_identical(aliased$item_level, env$meta_data)
  expect_identical(aliased$.item_level_arg_name, "meta_data")

  expect_identical(
    util_apply_decorator_meta_data_alias(
      env_args = list(item_level = data.frame(VAR_NAMES = "v3")),
      canonical = "item_level",
      aliases = "meta_data",
      call_arg_names = c("item_level", "meta_data"),
      env = env
    ),
    list(item_level = data.frame(VAR_NAMES = "v3"))
  )
})

test_that("decorator label column resolution prefers explicit information", {
  skip_on_cran()

  env <- list2env(list(label_col = "CALLER_LABEL"), parent = emptyenv())
  expect_identical(
    util_resolve_decorator_label_col(
      label_col = "EXPLICIT",
      what_i_have = "label_col",
      item_level = data.frame(LABEL = "Label", VAR_NAMES = "v1"),
      env = env
    ),
    "EXPLICIT"
  )

  expect_identical(
    util_resolve_decorator_label_col(
      label_col = NA_character_,
      what_i_have = character(),
      item_level = data.frame(LABEL = "Label", VAR_NAMES = "v1"),
      env = env
    ),
    "CALLER_LABEL"
  )

  empty_env <- new.env(parent = emptyenv())
  expect_identical(
    util_resolve_decorator_label_col(
      label_col = NA_character_,
      what_i_have = character(),
      item_level = data.frame(LABEL = "Label", VAR_NAMES = "v1"),
      env = empty_env
    ),
    LABEL
  )
  expect_identical(
    util_resolve_decorator_label_col(
      label_col = NA_character_,
      what_i_have = character(),
      item_level = data.frame(VAR_NAMES = "v1"),
      env = empty_env
    ),
    VAR_NAMES
  )
  expect_identical(
    util_resolve_decorator_label_col(
      label_col = "FALLBACK",
      what_i_have = character(),
      item_level = NULL,
      env = empty_env
    ),
    "FALLBACK"
  )
  expect_identical(
    util_resolve_decorator_label_col(
      label_col = NA_character_,
      what_i_have = character(),
      item_level = NULL,
      env = empty_env
    ),
    LABEL
  )
})

test_that("function body helpers wrap and insert expressions", {
  skip_on_cran()

  f <- function(x) {
    x + 1
  }
  f2 <- util_funins(f, y <- x * 2)
  expect_equal(f2(3), 4)
  expect_match(paste(deparse(body(f2)), collapse = "\n"), "y <- x \\* 2")

  wrapper <- function(expr) {
    eval(expr) * 10
  }
  f3 <- util_funwrap(f, "wrapper")
  environment(f3) <- environment()

  expect_equal(f3(3), 40)
})

test_that("util_decorator composes preps and result wrapping", {
  skip_on_cran()

  f <- function(x) {
    x + 1
  }
  decorated <- util_decorator(f, "test_helper")

  expect_equal(decorated(3), 4)
  expect_match(
    paste(deparse(body(decorated)), collapse = "\n"),
    "....alt_call_res"
  )
})

test_that("first positional data frame can be rewritten as study_data", {
  skip_on_cran()

  f <- function(resp_vars, study_data = NULL) {
    util_first_arg_study_data_or_resp_vars()
  }

  study_data <- data.frame(v1 = 1:2)
  rewritten <- f(study_data)
  expect_true(is.call(rewritten))
  expect_identical(as.character(rewritten[[1]]), "f")
  expect_identical(rewritten$study_data, study_data)

  expect_null(f("v1"))
  expect_null(f(resp_vars = study_data))
})

test_that("minimum group variable levels default is option backed", {
  option_default <- quote(
    getOption(
      "dataquieR.min_group_var_levels",
      dataquieR.min_group_var_levels_default
    )
  )

  expect_identical(dataquieR.min_group_var_levels_default, 2)
  expect_equal(formals(acc_varcomp)$min_subgroups, option_default)
  expect_equal(formals(util_acc_varcomp)$min_subgroups, option_default)
  expect_equal(formals(util_varcomp_robust)$min_subgroups, option_default)
  expect_equal(formals(util_margins_ord)$min_subgroups, option_default)
  expect_match(
    paste(deparse(body(acc_margins)), collapse = "\n"),
    "dataquieR\\.min_group_var_levels"
  )

  withr::local_options(dataquieR.min_group_var_levels = 3)
  expect_equal(eval(formals(acc_varcomp)$min_subgroups), 3)
})

test_that("ordinal margins raise low group level minimum", {
  skip_if_not_installed("ordinal")

  ds1 <- data.frame(
    resp = ordered(rep(c("low", "mid", "high"), length.out = 6)),
    group = factor(rep(c("a", "b"), each = 3))
  )

  expect_message(
    expect_error(
      util_margins_ord(
        resp_vars = "resp",
        group_vars = "group",
        co_vars = NULL,
        min_subgroups = 2,
        ds1 = ds1,
        label_col = "VAR_NAMES"
      ),
      regexp = "2 < 3 levels"
    ),
    regexp = "ordinal::clmm\\(\\).*R/clmm\\.R:getREterms"
  )
})
