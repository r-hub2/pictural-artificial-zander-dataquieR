test_that("prep_get_user_name prefers explicit option and environment names", {
  skip_on_cran()

  withr::local_options(FULLNAME = "Option User")
  withr::local_envvar(FULLNAME = "Environment User")

  expect_identical(prep_get_user_name(), "Option User")
})

test_that("prep_get_user_name uses environment names when option is empty", {
  skip_on_cran()

  withr::local_options(FULLNAME = "")
  withr::local_envvar(FULLNAME = "Environment User")

  expect_identical(prep_get_user_name(), "Environment User")
})

test_that("prep_get_user_name ignores non-scalar option names", {
  skip_on_cran()

  withr::local_options(FULLNAME = c("Option", "User"))
  withr::local_envvar(FULLNAME = "Environment User")

  expect_identical(prep_get_user_name(), "Environment User")
})

test_that("prep_get_user_name falls back to a scalar user name", {
  skip_on_cran()

  withr::local_options(FULLNAME = "")
  withr::local_envvar(FULLNAME = NA)

  user_name <- prep_get_user_name()

  expect_type(user_name, "character")
  expect_length(user_name, 1)
  expect_true(nzchar(user_name))
})
