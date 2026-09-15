skip_on_cran()

test_that("the report bibliography includes the software review", {
  citations <- utils::readCitationFile(
    system.file("CITATION", package = "dataquieR"),
    list(Encoding = "UTF-8")
  )
  bibtex <- paste(format(citations, style = "bibtex"), collapse = "\n\n")

  expect_match(bibtex, "@Article{Marino2022", fixed = TRUE)
  expect_match(bibtex, "10.3390/app12094238", fixed = TRUE)
})
