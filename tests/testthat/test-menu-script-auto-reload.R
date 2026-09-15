test_that("report auto reload is scoped and tolerates missing markers", {
  skip_on_cran()

  script_file <- system.file("menu", "script.js", package = "dataquieR")
  script <- paste(readLines(script_file, warn = FALSE), collapse = "\n")

  expect_match(
    script,
    'const KEY = "renderingData:" + infoUrl;',
    fixed = TRUE
  )
  expect_match(
    script,
    "new URL(INFO_SRC, window.location.href).pathname",
    fixed = TRUE
  )
  expect_false(grepl(
    "if (err) {\n        location.replace(RELOAD_URL",
    script,
    fixed = TRUE
  ))
})
