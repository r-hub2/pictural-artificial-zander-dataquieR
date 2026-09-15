test_that(
  "print.DataSlot returns browsable HTML invisibly when view is false",
  {
    skip_on_cran()

    html <- htmltools::tags$table(htmltools::tags$tr(htmltools::tags$td("x")))
    data <- structure(data.frame(x = 1), class = c("DataSlot", "data.frame"))

    testthat::local_mocked_bindings(
      util_html_table = function(x, ...) {
        expect_s3_class(x, "DataSlot")
        html
      }
    )

    printed <- expect_invisible(print.DataSlot(data, view = FALSE))

    expect_s3_class(printed, "shiny.tag")
    expect_match(
      htmltools::renderTags(printed)$html,
      "<td>x</td>",
      fixed = TRUE
    )
  }
)

test_that(
  "print.DataSlot returns NULL quietly when no HTML table is available",
  {
    skip_on_cran()

    data <- structure(data.frame(x = 1), class = c("DataSlot", "data.frame"))
    testthat::local_mocked_bindings(
      util_html_table = function(...) NULL
    )

    expect_null(expect_invisible(print.DataSlot(data, view = FALSE)))
  }
)

test_that("print.DataSlot prints HTML or delegates to knitr", {
  skip_on_cran()
  skip_if_not_installed("knitr")

  html <- htmltools::tags$table(htmltools::tags$tr(htmltools::tags$td("x")))
  data <- structure(data.frame(x = 1), class = c("DataSlot", "data.frame"))

  testthat::local_mocked_bindings(
    util_html_table = function(...) html,
    util_ensure_suggested = function(...) TRUE
  )

  expect_invisible(print.DataSlot(data))

  withr::local_options(knitr.in.progress = TRUE)
  testthat::with_mocked_bindings(
    .package = "knitr",
    knit_print = function(x, ...) {
      expect_false(inherits(x, "DataSlot"))
      "knit-data-slot"
    },
    {
      expect_identical(print.DataSlot(data), "knit-data-slot")
    }
  )
})

test_that("print.TableSlot converts to a DataSlot before rendering", {
  skip_on_cran()

  html <- htmltools::tags$table(htmltools::tags$tr(htmltools::tags$td("y")))
  table <- structure(
    data.frame(Variables = "v1", Metric = 1),
    class = c("TableSlot", "data.frame")
  )

  testthat::local_mocked_bindings(
    util_make_data_slot_from_table_slot = function(x, ...) {
      expect_s3_class(x, "TableSlot")
      structure(data.frame(Metric = 1), class = c("DataSlot", "data.frame"))
    },
    util_html_table = function(x, ...) {
      expect_s3_class(x, "DataSlot")
      html
    }
  )

  printed <- expect_invisible(print.TableSlot(table, view = FALSE))

  expect_s3_class(printed, "shiny.tag")
  expect_match(
    htmltools::renderTags(printed)$html,
    "<td>y</td>",
    fixed = TRUE
  )

  expect_invisible(print.TableSlot(table))
})

test_that("print.StudyDataSlot returns a tibble invisibly when view is false", {
  skip_on_cran()
  skip_if_not_installed("tibble")

  study_data <- structure(
    data.frame(id = 1L, value = "a"),
    class = c("StudyDataSlot", "data.frame")
  )

  printed <- expect_invisible(print.StudyDataSlot(study_data, view = FALSE))

  expect_s3_class(printed, "tbl_df")
  expect_identical(as.data.frame(printed), as.data.frame(study_data))
})

test_that(
  "print.Slot replays stored conditions and returns the next print value",
  {
    skip_on_cran()

    slot <- structure(
      data.frame(a = 1),
      class = c("Slot", "data.frame")
    )
    attr(slot, "message") <- list("slot message")
    attr(slot, "warning") <- list("slot warning")

    printed <- NULL
    expect_message(
      expect_warning(
        expect_invisible(printed <- print(slot, view = FALSE)),
        "slot warning"
      ),
      "slot message"
    )

    expect_s3_class(printed, "data.frame")
    expect_false(inherits(printed, "Slot"))
    expect_identical(printed$a, 1)
  }
)
