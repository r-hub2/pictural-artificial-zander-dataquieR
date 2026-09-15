withr::local_options(
  dataquieR.lazy_plots_gg_compatibility = "FALSE",
  viewer = function(...) invisible(NULL),
  .local_envir = testthat::teardown_env()
)
