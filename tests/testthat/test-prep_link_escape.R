test_that("prep_link_escape removes characters unsafe for anchors and file names", { # nolint: line_length_linter.
  labels <- c(
    "`Q1`: Größe <cm> / Pfad\\x | $income & \"quote\"? *",
    "Δ-baseline µg/m³ – café",
    .dq_unicode_label_samples(),
    "CON",
    "AUX",
    "NUL"
  )

  escaped <- prep_link_escape(labels)

  unsafe_chars <- c(
    "<", ">", ":", "\"", "/", "\\", "?", "*", "`", "$",
    "&", "@", "#", "_", "~", ";", "(", ")", " ", "|"
  )
  for (chr in unsafe_chars) {
    expect_false(any(grepl(chr, escaped, fixed = TRUE)))
  }
  expect_true(grepl("\uFF5C", escaped[[1]], fixed = TRUE))
  expect_true(grepl("Q", escaped[[1]], fixed = TRUE))
  expect_true(grepl("X", escaped[[1]], fixed = TRUE))
  expect_true(grepl("Größe", escaped[[1]], fixed = TRUE))
  expect_true(grepl("Δ-baseline", escaped[[2]], fixed = TRUE))
  unicode_escaped <- escaped[seq.int(3L, 2L + length(.dq_unicode_label_samples()))] # nolint: line_length_linter.
  expect_false(any(util_empty(unicode_escaped)))
  expect_equal(
    unicode_escaped,
    gsub("\\s+", "", .dq_unicode_label_samples(), perl = TRUE)
  )

  file_names <- paste0("VAR_", escaped, ".html")
  windows_devices <- c(
    "CON", "PRN", "AUX", "NUL",
    paste0("COM", 1:9), paste0("LPT", 1:9)
  )
  expect_false(any(toupper(tools::file_path_sans_ext(basename(file_names))) %in%
        windows_devices))
})
