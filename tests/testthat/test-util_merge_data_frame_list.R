test_that(
  "util_merge_data_frame_list merges by ids and collapses equal columns",
  {
    skip_on_cran()

    data_frames <- list(
      left = data.frame(
        id = c(1, 2),
        shared = c("x", "y"),
        left_only = c("l1", "l2")
      ),
      right = data.frame(
        id = c(2, 3),
        shared = c("y", "z"),
        right_only = c("r2", "r3")
      )
    )

    merged <- util_merge_data_frame_list(data_frames, "id")

    expect_equal(merged$id, c(1, 2, 3))
    expect_named(merged, c("id", "left_only", "right_only", "shared"))
    expect_equal(merged$shared, c("x", "y", "z"))
    expect_equal(merged$left_only, c("l1", "l2", NA))
    expect_equal(merged$right_only, c(NA, "r2", "r3"))
  }
)

test_that(
  "util_merge_data_frame_list keeps conflicting shared columns separate",
  {
    skip_on_cran()

    data_frames <- list(
      left = data.frame(id = c(1, 2), shared = c("x", "y")),
      right = data.frame(id = c(1, 2), shared = c("x", "different"))
    )

    merged <- util_merge_data_frame_list(data_frames, "id")

    expect_named(merged, c("id", "shared.left", "shared.right"))
    expect_equal(merged$shared.left, c("x", "y"))
    expect_equal(merged$shared.right, c("x", "different"))
  }
)

test_that("util_merge_data_frame_list ignores non-data-frame list entries", {
  skip_on_cran()

  data_frames <- list(
    left = data.frame(id = 1, value = "x"),
    ignored = "not a data frame",
    right = data.frame(id = 1, other = "y")
  )

  merged <- util_merge_data_frame_list(data_frames, "id")

  expect_named(merged, c("id", "value", "other"))
  expect_equal(merged$value, "x")
  expect_equal(merged$other, "y")
})
