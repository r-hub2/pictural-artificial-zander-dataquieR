skip_on_cran()

test_that("RStudio console activation runs after regular exit handlers", {
  events <- character(0)

  local_scope <- function() {
    util_defer_activate_rstudio_console(
      envir = environment(),
      active = TRUE,
      activate = function() events <<- c(events, "console")
    )
    withr::defer(events <<- c(events, "viewer"))
  }

  local_scope()

  expect_identical(events, c("viewer", "console"))
})

test_that("RStudio console activation is skipped outside RStudio", {
  activated <- FALSE

  local_scope <- function() {
    util_defer_activate_rstudio_console(
      envir = environment(),
      active = FALSE,
      activate = function() activated <<- TRUE
    )
  }

  local_scope()

  expect_false(activated)
})
