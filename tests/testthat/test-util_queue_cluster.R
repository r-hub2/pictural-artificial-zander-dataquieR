test_that("util_queue_cluster_setup works", {
  skip_on_cran() # runs parallel
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  q <- util_queue_cluster_setup(1, function(...) {}, FALSE, NULL)
  r1 <- q$compute_report(
    all_calls = list(
      a = call("ls"),
      b = call("search"),
      c = call("class", as.symbol("dt0"))
    ),
    worker = function(exp, env, nm, my_storr_object) {
      eval(exp, env)
    },
    step = 1
  )
  dt0 <- cars
  q$export("dt0")
  q$workerEval(function(lib) {
    require(lib, character.only = TRUE)
  }, list(lib = "ggplot2"))
  r2 <- q$compute_report(
    all_calls = list(
      a = call("ls"),
      b = call("search"),
      c = call("class", as.symbol("dt0"))
    ),
    worker = function(exp, env, nm, my_storr_object) {
      eval(exp, env)
    },
    step = 1
  )
  q <- NULL
  gc()
  expect_equal(setdiff(r2$b, r1$b), "package:ggplot2")
  expect_s3_class(r1$c, "try-error")
  expect_equal(
    conditionMessage(util_attr(r1$c, "condition", exact = TRUE)),
    "object 'dt0' not found"
  )
  expect_equal(r2$c, "data.frame")
  expect_equal(r1$a, r2$a)
  expect_equal(r1$a, c("e", "nm"))
})

test_that("util_queue_cluster_setup handles task lifecycle guardrails", {
  skip_on_cran() # starts a local callr worker
  skip_if_not_installed("R6")
  skip_if_not_installed("processx")
  skip_if_not_installed("callr")

  q <- util_queue_cluster_setup(1, function(...) {}, FALSE, NULL)
  withr::defer({
    q <- NULL
    gc()
  })

  expect_null(q$pop(0))
  expect_identical(
    q$push(function() list(value = 1), id = "same"),
    "same"
  )
  expect_error(
    q$push(function() 2, id = "same"),
    "Duplicate task id",
    fixed = TRUE
  )
  expect_equal(q$get_num_running(), 1)

  result <- q$pop(Inf)
  expect_identical(result$task_id, "same")
  expect_equal(result$result$value, 1)
  expect_true(q$is_idle())
  expect_equal(q$get_num_done(), 0)
})

test_that("util_queue_cluster_setup handles automatic ids and chunked calls", {
  skip_on_cran() # starts a local callr worker
  skip_if_not_installed("R6")
  skip_if_not_installed("processx")
  skip_if_not_installed("callr")

  progress_env <- new.env(parent = emptyenv())
  progress_env$values <- numeric()
  q <- util_queue_cluster_setup(1, function(x) {
    progress_env$values <- c(progress_env$values, x)
  }, FALSE, NULL)
  withr::defer({
    q <- NULL
    gc()
  })

  id <- q$push(function(x) list(value = x + 1L), list(x = 1L))
  expect_identical(id, ".1")
  expect_equal(q$get_num_waiting(), 0)
  expect_equal(q$get_num_running(), 1)
  expect_false(q$is_idle())
  expect_true(id %in% q$list_tasks()$id)

  result <- q$pop(Inf)
  expect_identical(result$task_id, ".1")
  expect_equal(result$result$value, 2L)
  expect_true(q$is_idle())

  calls <- list(
    one = quote(1L + 1L),
    two = quote(2L + 2L),
    three = quote(3L + 3L)
  )
  chunked <- q$compute_report(
    all_calls = calls,
    worker = function(expr, env, nm, my_storr_object) {
      eval(expr, envir = env)
    },
    step = 2
  )

  expect_equal(chunked, list(one = 2L, two = 4L, three = 6L))
  expect_true(any(progress_env$values == 100))
})
