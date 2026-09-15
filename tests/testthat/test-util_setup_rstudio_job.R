skip_on_cran()

test_that("util_progress_timestamp() returns a compact human-readable timestamp", { # nolint: line_length_linter.
  timestamp <- dataquieR:::util_progress_timestamp(
    as.POSIXct("2026-07-02 12:34:56", tz = "UTC")
  )

  expect_identical(timestamp, "12:34")
})

test_that("progress hooks register, run, and de-register", {
  skip_on_cran()

  rm(list = ls(dataquieR:::.progress_hooks),
    envir = dataquieR:::.progress_hooks)
  on.exit(rm(list = ls(dataquieR:::.progress_hooks),
      envir = dataquieR:::.progress_hooks), add = TRUE)

  events <- new.env(parent = emptyenv())
  events$values <- character()

  progress_hook <- function(percent) {
    events$values <- c(events$values, sprintf("progress:%s", percent))
  }
  init_hook <- function(n) {
    events$values <- c(events$values, sprintf("init:%s", n))
  }
  msg_hook <- function(status, msg) {
    events$values <- c(events$values, sprintf("msg:%s:%s", status, msg))
  }

  progress_handle <- prep_register_progress_hook("progress", progress_hook)
  init_handle <- prep_register_progress_hook("init", init_hook)
  msg_handle <- prep_register_progress_hook("msg", msg_hook)

  util_call_progress_hooks("progress", percent = 42)
  util_call_progress_hooks("init", n = 3)
  util_call_progress_hooks("msg", status = "run", msg = "halfway")

  expect_setequal(
    events$values,
    c("progress:42", "init:3", "msg:run:halfway")
  )

  expect_true(prep_deregister_progress_hook(progress_handle))
  expect_true(prep_deregister_progress_hook(init_handle))
  expect_true(prep_deregister_progress_hook(msg_handle))
  expect_false(suppressMessages(
    prep_deregister_progress_hook(msg_handle, verbose = TRUE)
  ))
})

test_that("progress hooks reject incompatible signatures", {
  skip_on_cran()

  expect_error(
    prep_register_progress_hook("init", function(x) x),
    "formal argument"
  )
  expect_error(
    prep_register_progress_hook("progress", function(x) x),
    "formal argument"
  )
  expect_error(
    prep_register_progress_hook("msg", function(status) status),
    "formal arguments"
  )
})

test_that("progress hooks handle invalid registry entries", {
  skip_on_cran()

  rm(list = ls(dataquieR:::.progress_hooks),
    envir = dataquieR:::.progress_hooks)
  on.exit(rm(list = ls(dataquieR:::.progress_hooks),
      envir = dataquieR:::.progress_hooks), add = TRUE)

  .progress_hooks[["progress"]] <- list(broken = "not a function")
  expect_message(
    util_call_progress_hooks("progress", percent = 1),
    "should never"
  )

  expect_true(prep_deregister_progress_hook("broken"))
})

test_that("progress hook deregistration validates scalar arguments", {
  skip_on_cran()

  expect_error(prep_deregister_progress_hook(1), "handle")
  expect_error(prep_deregister_progress_hook("missing", verbose = "yes"),
    "verbose")
})

test_that("util_setup_rstudio_job validates required arguments and callbacks", {
  skip_on_cran()

  expect_error(
    util_setup_rstudio_job("missing n"),
    "arguemnt .n. is mandatory"
  )

  expect_error(
    withr::with_options(
      list(dataquieR.progress_fkt = function(percent) percent),
      util_setup_rstudio_job("bad progress", n = 1)
    ),
    "dataquieR.progress_fkt"
  )

  expect_error(
    withr::with_options(
      list(dataquieR.progress_msg_fkt = function(status) status),
      util_setup_rstudio_job("bad message", n = 1)
    ),
    "dataquieR.progress_msg_fkt"
  )

  expect_error(
    withr::with_options(
      list(dataquieR.progress_init_fkt = function(total) total),
      util_setup_rstudio_job("bad init", n = 1)
    ),
    "dataquieR.progress_init_fkt"
  )
})

test_that("util_setup_rstudio_job uses compatible custom callbacks", {
  skip_on_cran()

  run_setup <- function() {
    events <- new.env(parent = emptyenv())
    events$values <- character()
    progress_fkt <- function(percent, is_rstudio, is_shiny, is_cli, e) {
      events$values <- c(events$values, sprintf("progress:%s", percent))
    }
    progress_msg_fkt <- function(status, msg, is_rstudio, is_shiny, e) {
      events$values <- c(events$values, sprintf("msg:%s:%s", status, msg))
    }
    progress_init_fkt <- function(n) {
      events$values <- c(events$values, sprintf("init:%s", n))
    }

    withr::with_options(
      list(
        dataquieR.progress_fkt = progress_fkt,
        dataquieR.progress_msg_fkt = progress_msg_fkt,
        dataquieR.progress_init_fkt = progress_init_fkt
      ),
      util_setup_rstudio_job("custom job", n = 7)
    )
    progress(25)
    progress_msg("phase", "done")
    events$values
  }

  expect_equal(
    run_setup(),
    c("init:7", "msg:custom job:", "progress:25", "msg:phase:done")
  )
})

test_that("progress functions ignore invalid progress values while testing", {
  skip_on_cran()

  calls <- new.env(parent = emptyenv())
  calls$values <- character()
  progress_hook <- function(percent) {
    calls$values <- c(calls$values, sprintf("progress:%s", percent))
  }
  progress_handle <- prep_register_progress_hook("progress", progress_hook)
  on.exit(prep_deregister_progress_hook(progress_handle), add = TRUE)

  util_setup_rstudio_job("invalid progress", n = 1)
  progress(c(1, 2))
  progress(NA_real_)
  progress("50")
  progress(-1)
  progress(101)

  expect_equal(
    calls$values,
    c("progress:1", "progress:2", "progress:NA", "progress:50",
      "progress:-1", "progress:101")
  )
})

test_that("progress functions validate values before message fallback", {
  skip_on_cran()

  calls <- new.env(parent = emptyenv())
  calls$messages <- character()

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) FALSE,
    util_progress_timestamp = function(...) "12:34",
    util_message = function(...) {
      calls$messages <- c(calls$messages, paste(..., collapse = " "))
      invisible(NULL)
    }
  )

  run_setup <- function() {
    util_setup_rstudio_job("message fallback", n = 1)
    progress(c(1, 2))
    progress(NA_real_)
    progress("50")
    progress(-1)
    progress(101)
    progress(2)
    progress_msg("plain status")
  }

  run_setup()

  expect_identical(
    calls$messages,
    c(
      "|%s ### 12:34 message fallback",
      "|%s> ##",
      "|%s ### 12:34 plain status"
    )
  )
})

test_that("progress_msg preserves existing timestamps", {
  skip_on_cran()

  calls <- new.env(parent = emptyenv())
  calls$values <- character()
  msg_hook <- function(status, msg) {
    calls$values <- c(calls$values, sprintf("%s:%s", status, msg))
  }
  msg_handle <- prep_register_progress_hook("msg", msg_hook)
  on.exit(prep_deregister_progress_hook(msg_handle), add = TRUE)

  util_setup_rstudio_job("timestamped", n = 1)
  progress_msg("status", "[2026-07-20T12:00:00+0200] already timestamped")
  progress_msg("status", "12:34 already timestamped")

  expect_true(any(calls$values == paste(
    "status:[2026-07-20T12:00:00+0200] already timestamped"
  )))
  expect_true(any(calls$values == "status:12:34 already timestamped"))
})

test_that("progress hook calls report failing hooks", {
  skip_on_cran()

  rm(list = ls(dataquieR:::.progress_hooks),
    envir = dataquieR:::.progress_hooks)
  on.exit(rm(list = ls(dataquieR:::.progress_hooks),
      envir = dataquieR:::.progress_hooks), add = TRUE)

  bad_hook <- function(percent) {
    stop("hook failed")
  }
  prep_register_progress_hook("progress", bad_hook)

  expect_message(
    util_call_progress_hooks("progress", percent = 1),
    "Could not call a hook function"
  )
})

test_that("util_setup_rstudio_job keeps previous RStudio job handles intact", {
  skip_on_cran()

  events <- new.env(parent = emptyenv())
  events$values <- character()

  testthat::local_mocked_bindings(
    util_rstudio_job_api_available = function() TRUE,
    util_rstudio_job_add = function(job_name) {
      events$values <- c(events$values, sprintf("add:%s", job_name))
      "new-job"
    },
    util_rstudio_job_set_state = function(job, state) {
      events$values <- c(events$values, sprintf("state:%s:%s", job, state))
      invisible(NULL)
    },
    util_rstudio_job_set_progress = function(job, percent) {
      events$values <- c(events$values, sprintf("progress:%s:%s", job, percent))
      invisible(NULL)
    },
    util_rstudio_job_set_status = function(job, status) {
      events$values <- c(events$values, sprintf("status:%s:%s", job, status))
      invisible(NULL)
    },
    util_rstudio_job_add_output = function(job, output) {
      events$values <- c(events$values, sprintf("output:%s:%s", job, output))
      invisible(NULL)
    }
  )

  run_nested_setup <- function() {
    rstudiojob <- "old-job"

    util_setup_rstudio_job("replacement", n = 1)
    expect_identical(rstudiojob, "new-job")
  }

  run_nested_setup()

  expect_false(any(grepl("old-job", events$values, fixed = TRUE)))
  expect_true("add:replacement" %in% events$values)
  expect_true("state:new-job:succeeded" %in% events$values)
})
