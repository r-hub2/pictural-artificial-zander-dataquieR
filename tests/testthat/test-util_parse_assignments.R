test_that("util_parse_assignments works", {
  skip_on_cran()
  expected <- list(
    "1" = "married", "2" = "single", "3" = "divorced",
    "4" = "widowed"
  )

  util_parse_assignments(
    "1 = married| \n 2 \t = \r  single|   3 =divorced|4=widowed"
  )

  expect_equal(
    util_parse_assignments(
      "1 = married| \n 2 \t = \r  single|   3 =divorced|4=widowed"
    ),
    expected
  )
})

test_that("util_parse_assignments covers local parser edge paths", {
  skip_on_cran()

  expect_equal(
    util_parse_assignments(c("", " ", NA)),
    setNames(list(), character(0))
  )
  expect_equal(
    util_parse_assignments(
      paste(SPLIT_CHAR, "<"),
      split_char = c(SPLIT_CHAR, "<"),
      split_on_any_split_char = TRUE
    ),
    setNames(list(), character(0))
  )

  quoted_single <- util_parse_assignments("1 = 'yes' | 2 = 'no'")
  expect_equal(quoted_single, c("yes", "no"))

  quoted_double <- util_parse_assignments('1 = "yes" | 2 = "no"')
  expect_equal(quoted_double, c("yes", "no"))

  list_input <- util_parse_assignments(list("1 = yes", "2 = no"))
  expect_equal(list_input, list("1" = "yes\nno"))

  multi <- util_parse_assignments(
    c("1 = yes | 2 = no", "low < medium < high"),
    split_char = c(SPLIT_CHAR, "<"),
    multi_variate_text = TRUE
  )
  expect_equal(unname(unlist(multi[[1]])), c("yes", "no"))
  expect_equal(names(multi[[1]]), c("1", "2"))
  expect_equal(attr(multi[[1]], "split_char", exact = TRUE), SPLIT_CHAR)
  expect_equal(unname(unlist(multi[[2]])), c("low", "medium", "high"))
  expect_equal(names(multi[[2]]), c("low", "medium", "high"))
  expect_equal(attr(multi[[2]], "split_char", exact = TRUE), "<")

  any_split <- util_parse_assignments(
    "1 = yes | 2 = no < 3 = maybe",
    split_char = c(SPLIT_CHAR, "<"),
    split_on_any_split_char = TRUE
  )
  expect_equal(any_split, list("1" = "yes", "2" = "no", "3" = "maybe"))
})

test_that("util_parse_assignments decodes HTML-escaped value labels", {
  skip_on_cran()
  skip_if_not_installed("textutils")

  decoded <- withr::with_options(
    list(dataquieR.VALUE_LABELS_htmlescaped = TRUE),
    util_parse_assignments("1 = yes &amp; no")
  )

  expect_equal(decoded, list("1" = "yes & no"))
})

test_that(paste(
  "util_parse_assignments equivalent with an old independent",
  "development from con inadmissible categories"
), {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.

  got <- lapply(lapply(
    setNames(meta_data[[VALUE_LABELS]],
      nm =
        meta_data$VAR_NAMES
    ),
    util_parse_assignments
  ), unlist) # get for all item

  got <- mapply(lapply(got, names), got, FUN = setNames) # swap names and values

  label_col <- LABEL
  rvs <- meta_data[[LABEL]]

  expected <-
    lapply(
      lapply(
        setNames(meta_data[[VALUE_LABELS]][meta_data[[label_col]] %in% rvs],
          nm = meta_data[[VAR_NAMES]][meta_data[[label_col]] %in% rvs]
        ),
        function(x) {
          lapply(
            trimws(unlist(strsplit(x, SPLIT_CHAR, fixed = TRUE))),
            function(x) {
              setNames(lapply(
                x,
                function(x) trimws(unlist(strsplit(x, "=", fixed = TRUE))[1])
              ), nm = lapply(
                x,
                function(x) {
                  y <- unlist(strsplit(x, "=", fixed = TRUE))
                  if (length(y) > 1) {
                    trimws(paste(y[-1], collapse = "="))
                  } else {
                    trimws(paste(y, collapse = "="))
                  } # in case of no name, use
                  # the value as the name
                }
              ))
            }
          )
        }
      ),
      unlist
    )

  expected[vapply(expected, identical, c("NA" = NA_character_),
      FUN.VALUE = logical(1)
    )] <- list(NULL)

  expect_equal(got, expected)
})
