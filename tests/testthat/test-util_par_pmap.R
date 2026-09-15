test_that("util_par_pmap works", {
  skip_on_cran() # slow and meant for parallel processing
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  plan <- dplyr::tribble(
    ~x, ~y,
    rnorm(1:10000), rnorm(1:10000),
    rnorm(1:10000), rnorm(1:10000),
    rnorm(1:10000), rnorm(1:10000),
    rnorm(1:10000), rnorm(1:10000),
    rnorm(1:10000), rnorm(1:10000),
    rnorm(1:10000), rnorm(1:10000),
    rnorm(1:10000), rnorm(1:10000),
    rnorm(1:10000), rnorm(1:10000),
    rnorm(1:10000), rnorm(1:10000),
    rnorm(1:10000), rnorm(1:10000)
  )
  r <- unlist(dataquieR:::util_par_pmap(plan, cor, cores = 1),
    recursive = FALSE
  )
  expect_vector(r, numeric(0), 10)
  expect_equal(
    ignore_attr = TRUE, mapply(plan$x, plan$y, FUN = cor),
    r
  )
  my_cor <- function(..., meta_data, study_data) {
    testthat::expect_true(exists("meta_data"))
    testthat::expect_true(exists("study_data"))
    cor(...)
  }
  s <- unlist(
    dataquieR:::util_par_pmap(
      .l = plan,
      .f = my_cor,
      meta_data = cars,
      study_data = cars,
      cores = list(
        mode = "socket",
        cpus = 1,
        logging = FALSE,
        load.balancing = TRUE
      ),
      use_cache = TRUE
    ),
    recursive = FALSE
  )
  expect_equal(s, r)
})
