test_that("util_rbind fills missing columns and preserves data types", {
  skip_on_cran()

  left <- data.frame(id = 1L, left_only = "left")
  right <- data.frame(id = 2L, right_only = "right")
  attr(left$id, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(right$id, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(left$left_only, DATA_TYPE) <- DATA_TYPES$STRING
  attr(right$right_only, DATA_TYPE) <- DATA_TYPES$STRING

  bound <- util_rbind(left, right)

  expect_named(bound, c("id", "left_only", "right_only"))
  expect_equal(as.vector(bound$id), c(1L, 2L))
  expect_equal(as.vector(bound$left_only), c("left", NA))
  expect_equal(as.vector(bound$right_only), c(NA, "right"))
  expect_identical(attr(bound$id, DATA_TYPE, exact = TRUE),
    DATA_TYPES$INTEGER)
  expect_true(is.na(attr(bound$left_only, DATA_TYPE, exact = TRUE)))
  expect_true(is.na(attr(bound$right_only, DATA_TYPE, exact = TRUE)))
})

test_that("util_rbind handles NULL entries and conflicting data types", {
  skip_on_cran()

  integer_id <- data.frame(id = 1L)
  float_id <- data.frame(id = 2)
  attr(integer_id$id, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(float_id$id, DATA_TYPE) <- DATA_TYPES$FLOAT

  bound <- util_rbind(
    NULL,
    integer_id,
    data_frames_list = list(NULL, float_id)
  )

  expect_equal(as.vector(bound$id), c(1, 2))
  expect_true(is.na(attr(bound$id, DATA_TYPE, exact = TRUE)))
})

test_that("util_rbind aligns translated names across different columns", {
  skip_on_cran()

  first <- data.frame(id = 1L, left = "first")
  second <- data.frame(id = 2L, right = "second")
  util_translated_colnames(first) <- structure(
    c("Identifier", "Left"),
    names = c("id", "left"),
    ns = "dashboard_table",
    lang = "",
    class = "dataquieR_translated"
  )
  util_translated_colnames(second) <- structure(
    c("Identifier", "Right"),
    names = c("id", "right"),
    ns = "dashboard_table",
    lang = "",
    class = "dataquieR_translated"
  )

  bound <- util_rbind(first, second)

  expect_identical(as.character(colnames(bound)),
    c("Identifier", "Left", "Right")
  )
  expect_identical(util_untranslated_colnames(bound),
    c("id", "left", "right")
  )
  expect_identical(as.vector(bound$Identifier), c(1L, 2L))
})
