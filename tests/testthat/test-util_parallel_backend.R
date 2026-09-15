skip_on_cran()

skip_if_no_parallel_backend_psock_cluster <- function() {
  cl <- try(parallel::makePSOCKcluster(1), silent = TRUE)
  if (inherits(cl, "try-error")) {
    skip("PSOCK clusters unavailable")
  }
  try(parallel::stopCluster(cl), silent = TRUE)
  try(parallel::setDefaultCluster(NULL), silent = TRUE)
}

test_that("util_parallel_backend: start/stop in local mode", {
  skip_on_cran()
  withr::defer(try(util_parallel_stop(), silent = TRUE))

  expect_silent(util_parallel_start(mode = "local"))
  expect_equal(util_parallel_status(), "started")
  expect_equal(util_parallel_get_options()$settings$mode, "local")

  util_parallel_stop()
  expect_equal(util_parallel_status(), "stopped")
})

test_that("util_parallel_map: sequential semantics match lapply/mapply", {
  withr::defer(try(util_parallel_stop(), silent = TRUE))
  util_parallel_start(mode = "local")

  # 1-iterable, no names
  out <- util_parallel_map(1:5, fun = function(x) x^2)
  expect_equal(out, as.list((1:5)^2))

  # honour names of the first iterable
  inp <- setNames(1:3, c("a", "b", "c"))
  out <- util_parallel_map(inp, fun = function(x) x + 10)
  expect_equal(out, list(a = 11, b = 12, c = 13))

  # multiple iterables (mapply-style)
  out <- util_parallel_map(1:3, 4:6, fun = function(a, b) a * b)
  expect_equal(out, list(4, 10, 18))

  # more.args is broadcast
  out <- util_parallel_map(1:3,
    fun = function(x, k) x + k,
    more.args = list(k = 100)
  )
  expect_equal(out, list(101, 102, 103))
})

test_that("util_parallel_map: impute.error catches per-call errors", {
  withr::defer(try(util_parallel_stop(), silent = TRUE))
  util_parallel_start(mode = "local")

  f <- function(x) if (x == 2) stop("boom") else x
  out <- util_parallel_map(1:3, fun = f, impute.error = identity)

  expect_equal(out[[1]], 1)
  expect_s3_class(out[[2]], "simpleError")
  expect_equal(out[[3]], 3)
})

test_that("util_parallel_export: no-op in local mode does not error", {
  withr::defer(try(util_parallel_stop(), silent = TRUE))
  util_parallel_start(mode = "local")
  x <- 42
  expect_silent(util_parallel_export("x"))
})

test_that("util_parallel_export and library ignore empty requests", {
  withr::defer(try(util_parallel_stop(), silent = TRUE))
  util_parallel_start(mode = "local")

  expect_silent(util_parallel_export())
  expect_silent(util_parallel_export(""))
  expect_silent(util_parallel_export(objnames = character()))
  expect_silent(util_parallel_library())
  expect_silent(util_parallel_library(""))
})

test_that("util_parallel_library: in local mode loads namespace locally", {
  withr::defer(try(util_parallel_stop(), silent = TRUE))
  util_parallel_start(mode = "local")
  # `stats` is base R, always available
  expect_silent(util_parallel_library("stats"))
})

test_that("util_parallel_get_options falls back to parallelMap.mode while stopped", { # nolint: line_length_linter.
  withr::defer(try(util_parallel_stop(), silent = TRUE))
  try(util_parallel_stop(), silent = TRUE)

  # Sanity: the backend is stopped.
  expect_equal(util_parallel_status(), "stopped")

  withr::with_options(list(
    parallelMap.mode = "snow",
    dataquieR.parallel.mode = NULL
  ), {
    # Even though .dq2_par$mode is "local" while stopped, callers like
    # util_generate_pages_from_report() must see the user-set "snow" mode.
    expect_equal(util_parallel_get_options()$settings$mode, "snow")
  })

  # While the backend is started, the option lookup is bypassed.
  withr::with_options(list(
    parallelMap.mode = "snow",
    dataquieR.parallel.mode = NULL
  ), {
    util_parallel_start(mode = "local")
    expect_equal(util_parallel_get_options()$settings$mode, "local")
    util_parallel_stop()
  })
})

test_that("util_parallel_start picks up dataquieR.parallel.mode option", {
  skip_on_cran()
  withr::defer(try(util_parallel_stop(), silent = TRUE))

  # When called without an explicit mode, util_parallel_start should fall
  # back to the option (which itself falls back to the parallelMap.* legacy
  # option, then to "local").
  withr::with_options(list(
    dataquieR.parallel.mode = "local",
    parallelMap.mode = NULL
  ), {
    util_parallel_start()
    expect_equal(util_parallel_get_options()$settings$mode, "local")
    util_parallel_stop()
  })
})

test_that("util_parallel_opt prioritizes new, legacy, and fallback values", {
  withr::with_options(list(
    dataquieR.parallel.mode = NULL,
    parallelMap.mode = NULL
  ), {
    expect_identical(util_parallel_opt("mode", "local"), "local")
  })

  withr::with_options(list(
    dataquieR.parallel.mode = NULL,
    parallelMap.mode = "legacy"
  ), {
    expect_identical(util_parallel_opt("mode", "local"), "legacy")
  })

  withr::with_options(list(
    dataquieR.parallel.mode = "current",
    parallelMap.mode = "legacy"
  ), {
    expect_identical(util_parallel_opt("mode", "local"), "current")
  })
})

test_that("util_parallel_backend keeps batch stub modes sequential", {
  skip_on_cran()
  withr::defer(try(util_parallel_stop(), silent = TRUE))

  withr::with_options(list(dataquieR.parallel.show.info = TRUE,
      dataquieR.parallel.load.balancing = FALSE), {
      util_parallel_start(mode = "BatchJobs", cpus = 3,
        load.balancing = FALSE)
      settings <- util_parallel_get_options()$settings

      expect_equal(settings$mode, "BatchJobs")
      expect_equal(settings$cpus, 3L)
      expect_true(settings$show.info)
      expect_false(settings$load.balancing)
      expect_null(parallel::getDefaultCluster())
      expect_equal(
        util_parallel_map(1:3, fun = function(x) x * 2),
        list(2, 4, 6)
      )
    })

  util_parallel_stop()
})

test_that("util_parallel_start normalizes invalid mode and cpu inputs", {
  skip_on_cran()
  withr::defer(try(util_parallel_stop(), silent = TRUE))

  expect_silent(util_parallel_start(mode = NULL, cpus = 1))
  expect_equal(util_parallel_get_options()$settings$mode, "local")
  util_parallel_stop()

  expect_warning(
    util_parallel_start(mode = "cloud", cpus = NA),
    "Unknown parallel mode"
  )
  expect_equal(util_parallel_status(), "started")
  expect_equal(util_parallel_get_options()$settings$mode, "local")
  expect_gte(util_parallel_get_cpus(), 1L)
})

test_that("util_parallel_map handles empty, simplified, and invalid inputs", {
  withr::defer(try(util_parallel_stop(), silent = TRUE))
  util_parallel_start(mode = "local")

  expect_equal(util_parallel_map(fun = identity), list())
  expect_equal(util_parallel_map(fun = identity, simplify = TRUE), logical(0))

  named_empty <- structure(integer(), names = character())
  expect_equal(
    util_parallel_map(named_empty, fun = identity),
    structure(list(), names = character())
  )

  expect_equal(
    util_parallel_map(1:3, fun = function(x) x + 1, simplify = TRUE),
    c(2, 3, 4)
  )

  expect_error(
    util_parallel_map(1:2, 1:3, fun = function(x, y) x + y),
    "same length"
  )
})

test_that("util_parallel_eval evaluates locally without a cluster", {
  withr::defer(try(util_parallel_stop(), silent = TRUE))
  util_parallel_start(mode = "local")

  x <- 41L
  expect_equal(util_parallel_eval(quote(x + 1L)), 42L)
})

test_that("util_parallel_backend: socket mode roundtrip", {
  skip_on_cran()
  skip_on_os("solaris")
  skip_if(
    .Platform$OS.type == "windows" && !interactive(),
    "PSOCK on Windows in non-interactive test envs is flaky"
  )
  skip_if_no_parallel_backend_psock_cluster()

  withr::defer(try(util_parallel_stop(), silent = TRUE))

  util_parallel_start(mode = "socket", cpus = 2)
  expect_equal(util_parallel_status(), "started")
  expect_equal(util_parallel_get_options()$settings$mode, "socket")
  expect_false(is.null(parallel::getDefaultCluster()))

  out <- util_parallel_map(1:4, fun = function(x) x + 1)
  expect_equal(out, list(2, 3, 4, 5))

  util_parallel_stop()
  expect_equal(util_parallel_status(), "stopped")
  expect_null(parallel::getDefaultCluster())
})

test_that("util_parallel_map forces more.args before socket dispatch", {
  skip_on_cran()
  skip_on_os("solaris")
  skip_if(
    .Platform$OS.type == "windows" && !interactive(),
    "PSOCK on Windows in non-interactive test envs is flaky"
  )
  skip_if_no_parallel_backend_psock_cluster()

  withr::defer(try(util_parallel_stop(), silent = TRUE))
  payload <- list(a = 1L, b = 2L)

  util_parallel_start(mode = "socket", cpus = 1)
  out <- util_parallel_map(1:2,
    fun = function(i, payload) payload[[i]],
    more.args = list(payload = payload),
    use.names = FALSE
  )

  expect_equal(out, list(1L, 2L))
})

test_that("util_parallel_start stops owned clusters before restarting", {
  skip_on_cran()
  skip_on_os("solaris")
  skip_if(
    .Platform$OS.type == "windows" && !interactive(),
    "PSOCK on Windows in non-interactive test envs is flaky"
  )
  skip_if_no_parallel_backend_psock_cluster()

  withr::defer(try(util_parallel_stop(), silent = TRUE))

  util_parallel_start(mode = "socket", cpus = 1)
  expect_false(is.null(parallel::getDefaultCluster()))

  expect_silent(util_parallel_start(mode = "local", cpus = 1))
  expect_equal(util_parallel_get_options()$settings$mode, "local")
  expect_null(parallel::getDefaultCluster())
})

test_that("util_parallel_map supports static socket dispatch", {
  skip_on_cran()
  skip_on_os("solaris")
  skip_if(
    .Platform$OS.type == "windows" && !interactive(),
    "PSOCK on Windows in non-interactive test envs is flaky"
  )
  skip_if_no_parallel_backend_psock_cluster()

  withr::defer(try(util_parallel_stop(), silent = TRUE))

  util_parallel_start(mode = "socket", cpus = 1, load.balancing = FALSE)
  expect_false(util_parallel_get_options()$settings$load.balancing)

  out <- util_parallel_map(1:3,
    fun = function(x, offset) {
      if (x == 2) {
        stop("boom")
      }
      x + offset
    },
    more.args = list(offset = 10L),
    impute.error = function(e) conditionMessage(e),
    use.names = FALSE
  )

  expect_equal(out, list(11L, "boom", 13L))
})

test_that("util_parallel_backend reuses caller-owned default clusters", {
  skip_on_cran()
  skip_on_os("solaris")
  skip_if(
    .Platform$OS.type == "windows" && !interactive(),
    "PSOCK on Windows in non-interactive test envs is flaky"
  )
  skip_if_no_parallel_backend_psock_cluster()

  cl <- parallel::makePSOCKcluster(1)
  withr::defer({
    try(util_parallel_stop(), silent = TRUE)
    try(parallel::setDefaultCluster(NULL), silent = TRUE)
    try(parallel::stopCluster(cl), silent = TRUE)
  })
  parallel::setDefaultCluster(cl)

  util_parallel_start(mode = "socket", cpus = 4)

  expect_equal(util_parallel_status(), "started")
  expect_equal(util_parallel_get_options()$settings$mode, "socket")
  expect_equal(util_parallel_get_cpus(), 1L)

  util_parallel_stop()
  expect_equal(util_parallel_status(), "stopped")
  expect_false(is.null(parallel::getDefaultCluster()))
})

test_that("util_parallel_backend exports and evaluates on socket workers", {
  skip_on_cran()
  skip_on_os("solaris")
  skip_if(
    .Platform$OS.type == "windows" && !interactive(),
    "PSOCK on Windows in non-interactive test envs is flaky"
  )
  skip_if_no_parallel_backend_psock_cluster()

  withr::defer(try(util_parallel_stop(), silent = TRUE))
  util_parallel_start(mode = "socket", cpus = 1)

  answer <- 41L
  util_parallel_export("answer")

  expect_equal(
    util_parallel_eval(quote(answer + 1L)),
    list(42L)
  )
})
