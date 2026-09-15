skip_on_cran()

test_that("util_ensure_label shortens long labels readably", {
  withr::local_options(dataquieR.MAX_LABEL_LEN = 60)

  md <- data.frame(
    VAR_NAMES = c("mort_cvd", "mort_other"),
    LABEL = c(
      "Haupttodesursache Herz-Kreislauf-Erkrankung (ICD-10: I10-I79, R96)",
      "Haupttodesursache Herz-Kreislauf-Erkrankung (ICD-10: I10-I79, R97)"
    ),
    stringsAsFactors = FALSE
  )

  res <- suppressWarningsMatching(
    .util_ensure_label(md, LABEL),
    "Some variables have labels with more than 60 characters"
  )
  labels <- res$meta_data[[LABEL]]

  expect_true(all(nchar(labels) <= MAX_LABEL_LEN))
  expect_equal(length(unique(labels)), length(labels))
  expect_true(startsWith(
    labels[[1]],
    "mort_cvd: Haupttodesursache Herz-Kreislauf"
  ))
  expect_false(grepl("HaupttodesursacheHerzKreislauf",
      labels[[1]],
      fixed = TRUE
    ))
  expect_equal(res$meta_data[["ORIGINAL_LABEL"]], md[[LABEL]])
})

test_that("util_ensure_label keeps short labels unique after shortening", {
  withr::local_options(dataquieR.MAX_LABEL_LEN = 60)

  long_label <-
    "Haupttodesursache Herz-Kreislauf-Erkrankung (ICD-10: I10-I79, R96)"
  colliding_label <- .util_make_readable_short_labels(
    label = long_label,
    var_names = "mort_cvd",
    max_label_len = MAX_LABEL_LEN
  )
  md <- data.frame(
    VAR_NAMES = c("mort_cvd", "existing_short_label"),
    LABEL = c(long_label, colliding_label),
    stringsAsFactors = FALSE
  )

  res <- suppressWarningsMatching(
    .util_ensure_label(md, LABEL),
    "Some variables have labels with more than 60 characters"
  )
  labels <- res$meta_data[[LABEL]]

  expect_true(all(nchar(labels) <= MAX_LABEL_LEN))
  expect_equal(length(unique(labels)), length(labels))
  expect_true(startsWith(labels[[1]], "mort_cvd: "))
  expect_identical(labels[[2]], colliding_label)
})

test_that("unique human labels omit long variable-name anchors", {
  withr::local_options(dataquieR.MAX_LABEL_LEN = 60)

  long_var_name <- paste(rep("verylongvariablename", 8), collapse = "")
  md <- data.frame(
    VAR_NAMES = long_var_name,
    LABEL = "Have you been physically vigorously active in the past 12 hours?",
    stringsAsFactors = FALSE
  )

  res <- suppressWarningsMatching(
    .util_ensure_label(md, LABEL),
    "Some variables have labels with more than 60 characters"
  )
  label <- res$meta_data[[LABEL]]

  expect_true(nchar(label) <= MAX_LABEL_LEN)
  expect_true(startsWith(label, "Have you been"))
  expect_false(grepl("~", label, fixed = TRUE))
})

test_that("unique SSI labels omit technical hashes", {
  withr::local_options(dataquieR.MAX_LABEL_LEN = 60)

  md <- data.frame(
    VAR_NAMES = "IRV_European Social Survey 10",
    LABEL = paste0(
      "Intra-individual Response Variability.",
      "European Social Survey 10"
    ),
    stringsAsFactors = FALSE
  )

  res <- suppressWarningsMatching(
    .util_ensure_label(md, LABEL),
    "Some variables have labels with more than 60 characters"
  )

  expect_identical(
    res$meta_data[[LABEL]],
    "Intra-individual Response Variability.European Social..."
  )
  expect_false(grepl("~[[:xdigit:]]+$", res$meta_data[[LABEL]]))
})

test_that("util_ensure_label anchors genuinely colliding abbreviations", {
  withr::local_options(dataquieR.MAX_LABEL_LEN = 60)

  md <- data.frame(
    VAR_NAMES = c(
      paste0(strrep("very_long_internal_name_", 3), "one"),
      paste0(strrep("very_long_internal_name_", 3), "two")
    ),
    LABEL = c(
      paste0(strrep("Shared human-readable label ", 3), "one"),
      paste0(strrep("Shared human-readable label ", 3), "two")
    ),
    stringsAsFactors = FALSE
  )

  res <- suppressWarnings(.util_ensure_label(md, LABEL))

  expect_false(anyDuplicated(res$meta_data[[LABEL]]) > 0L)
  expect_true(all(nchar(res$meta_data[[LABEL]]) <= MAX_LABEL_LEN))
  expect_true(all(grepl("~[[:xdigit:]]{6}: ", res$meta_data[[LABEL]])))
})

test_that("util_ensure_label avoids unreadably short variable-name anchors", {
  withr::local_options(dataquieR.MAX_LABEL_LEN = 12)

  label <- "Haupttodesursache Herz-Kreislauf-Erkrankung"
  short_label <- .util_make_readable_short_labels(
    label = label,
    var_names = "mort_cvd",
    max_label_len = MAX_LABEL_LEN
  )

  expect_equal(short_label, "Haupttode...")
  expect_false(grepl("^[[:alnum:]]: ", short_label))
  expect_true(nchar(short_label) <= MAX_LABEL_LEN)
})

test_that("util_ensure_label keeps long VAR_NAMES unchanged for mapping", {
  withr::local_options(dataquieR.MAX_LABEL_LEN = 60)

  long_var_name <- paste(rep("verylongvariablename", 8), collapse = "")
  md <- data.frame(
    VAR_NAMES = long_var_name,
    LABEL = "Short label",
    stringsAsFactors = FALSE
  )

  res <- suppressWarningsMatching(
    .util_ensure_label(md, VAR_NAMES),
    "Some variables have labels with more than 60 characters"
  )

  expect_equal(res$meta_data[[VAR_NAMES]], long_var_name)
  expect_equal(res$label_col, "VAR_NAMES_1")
  expect_true(nchar(res$meta_data[[res$label_col]]) <= MAX_LABEL_LEN)
  expect_true(startsWith(res$meta_data[[res$label_col]], "verylongvaria~"))
})

test_that("util_ensure_label uses readable suffixes for small limits", {
  withr::local_options(dataquieR.MAX_LABEL_LEN = 8)

  md <- data.frame(
    VAR_NAMES = c("alpha_1", "alpha_2"),
    LABEL = c(
      "Alpha question text from an eCRF",
      "Alpha questionnaire item with a similar beginning"
    ),
    stringsAsFactors = FALSE
  )

  res <- suppressWarningsMatching(
    .util_ensure_label(md, LABEL),
    "Some variables have labels with more than 8 characters"
  )
  labels <- res$meta_data[[LABEL]]

  expect_true(all(nchar(labels) <= MAX_LABEL_LEN))
  expect_equal(length(unique(labels)), length(labels))
  expect_equal(labels, c("Alpha...", "Alpha 2"))
})

test_that("MAX_LABEL_LEN active bindings reject unreadably small options", {
  withr::local_options(dataquieR.MAX_LABEL_LEN = 2)

  expect_error(
    MAX_LABEL_LEN,
    "options\\(dataquieR.MAX_LABEL_LEN = .*dataquieR.MAX_LONG_LABEL_LEN"
  )
})

test_that("MAX_LONG_LABEL_LEN active binding rejects unreadably small options", { # nolint: line_length_linter.
  withr::local_options(dataquieR.MAX_LONG_LABEL_LEN = 2)

  expect_error(
    MAX_LONG_LABEL_LEN,
    "options\\(dataquieR.MAX_LABEL_LEN = .*dataquieR.MAX_LONG_LABEL_LEN"
  )
})

test_that("util_ensure_label errors on unreadably small length arguments", {
  expect_error(
    .util_make_readable_short_labels(
      label = "Alpha question text from an eCRF",
      var_names = "alpha_1",
      max_label_len = 2
    ),
    "Argument .max_label_len.*at least 5"
  )
})

test_that("prep_get_labels errors on unreadably small max_len argument", {
  expect_error(
    prep_get_labels(
      resp_vars = "alpha_1",
      item_level = data.frame(
        VAR_NAMES = "alpha_1",
        LABEL = "Alpha question text",
        stringsAsFactors = FALSE
      ),
      max_len = 2
    ),
    "Argument .max_len.*at least 5"
  )
})

test_that("shortened special-character labels remain anchor and file-name safe", { # nolint: line_length_linter.
  withr::local_options(dataquieR.MAX_LABEL_LEN = 60)

  md <- data.frame(
    VAR_NAMES = c("v0001", "v0002"),
    LABEL = c(
      paste(
        .dq_unicode_label_samples()[[1]],
        "Größe µg/m³ Δ-baseline: Haben Sie in den letzten 12 Monaten $income / Einkommen > 0 EUR angegeben?" # nolint: line_length_linter.
      ),
      paste(
        .dq_unicode_label_samples()[[11]],
        "Wurde der Wert <5> in C:\\temp\\ecrf dokumentiert oder spaeter korrigiert?" # nolint: line_length_linter.
      )
    ),
    stringsAsFactors = FALSE
  )

  res <- suppressWarningsMatching(
    .util_ensure_label(md, LABEL),
    "Some variables have labels with more than 60 characters"
  )
  labels <- res$meta_data[[LABEL]]

  expect_true(all(nchar(labels) <= MAX_LABEL_LEN))
  expect_equal(length(unique(labels)), length(labels))
  expect_true(any(grepl("한국어|𐌲𐌿𐍄𐌹𐍃𐌺", labels)))

  for (label in labels) {
    href <- htmltools::tagGetAttribute(
      util_generate_anchor_link(label, "Summary", "variable"),
      "href"
    )
    page <- sub("#.*$", "", href)
    id <- htmltools::tagGetAttribute(
      util_generate_anchor_tag(label, "Summary", "variable"),
      "id"
    )

    expect_true(startsWith(page, "VAR_"))
    expect_true(endsWith(page, ".html"))
    for (chr in c(
      "<", ">", ":", "\"", "/", "\\", "?", "*", "`", "$",
      "&", "@", "#", ";", "(", ")", " "
    )) {
      expect_false(grepl(chr, page, fixed = TRUE))
      expect_false(grepl(chr, id, fixed = TRUE))
    }
  }
})

test_that(
  "util_ensure_label replaces missing labels and removes empty var names",
  {
    meta_data <- data.frame(
      VAR_NAMES = c("v1", "v2", ""),
      LABEL = c("label 1", NA, "ignored"),
      stringsAsFactors = FALSE
    )

    res <- suppressWarnings(util_ensure_label(meta_data, LABEL))

    expect_equal(res$meta_data$VAR_NAMES, c("v1", "v2"))
    expect_equal(res$meta_data$LABEL, c("label 1", "v2"))
    expect_match(res$label_modification_text, "missing label")
    expect_equal(
      res$label_modification_table$Reason,
      "No label specified."
    )
  }
)

test_that("util_ensure_label preserves missing-label origin after shortening", {
  meta_data <- data.frame(
    VAR_NAMES = c("very_long_variable_name_alpha", "v2"),
    LABEL = c(NA_character_, "short"),
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(.util_ensure_label(
    meta_data,
    LABEL,
    max_label_len = 12
  ))

  expect_equal(result$meta_data$ORIGINAL_LABEL, c("(missing)", "short"))
  expect_true(nchar(result$meta_data$LABEL[[1]]) <= 12)
  expect_equal(
    result$label_modification_table$Reason,
    "No label specified. The label is too long."
  )
})

test_that("util_ensure_label avoids colliding missing-label fallbacks", {
  meta_data <- data.frame(
    VAR_NAMES = c("existing_label", "v2"),
    LABEL = c(NA_character_, "existing_label"),
    stringsAsFactors = FALSE
  )

  res <- suppressWarnings(.util_ensure_label(meta_data, LABEL))

  expect_false(anyDuplicated(res$meta_data[[LABEL]]) > 0L)
  expect_true(startsWith(res$meta_data[[LABEL]][[1]], "NO LABEL "))
  expect_equal(res$meta_data[[LABEL]][[2]], "existing_label")
  expect_equal(
    res$label_modification_table$Reason,
    "No label specified."
  )
})

test_that("util_ensure_label makes duplicated labels unique", {
  meta_data <- data.frame(
    VAR_NAMES = c("v1", "v2", "v3"),
    LABEL = c("same", "same", "other"),
    stringsAsFactors = FALSE
  )

  res <- suppressWarnings(util_ensure_label(meta_data, LABEL))

  expect_false(anyDuplicated(res$meta_data$LABEL) > 0L)
  expect_equal(res$meta_data$LABEL[1:2], c("v1: same", "v2: same"))
  expect_true(all(grepl(
    "Duplicated label", res$label_modification_table$Reason
  )))
})

test_that("util_ensure_label numbers repeated duplicate-label fallbacks", {
  meta_data <- data.frame(
    VAR_NAMES = rep("v", 4),
    LABEL = rep("same", 4),
    stringsAsFactors = FALSE
  )

  res <- suppressWarnings(.util_ensure_label(meta_data, LABEL))

  expect_equal(
    res$meta_data$LABEL,
    c(
      "v: same",
      "v: same DUPLICATE 1",
      "v: same DUPLICATE 2",
      "v: same DUPLICATE 3"
    )
  )
  expect_false(anyDuplicated(res$meta_data$LABEL) > 0L)
  expect_true(all(grepl(
    "Duplicated label",
    res$label_modification_table$Reason
  )))
})

test_that("util_ensure_label marks exactly two duplicate fallback labels", {
  meta_data <- data.frame(
    VAR_NAMES = rep("v", 2),
    LABEL = rep("same", 2),
    stringsAsFactors = FALSE
  )

  res <- suppressWarnings(.util_ensure_label(meta_data, LABEL))

  expect_equal(res$meta_data$LABEL, c("v: same", "v: same DUPLICATE"))
  expect_false(anyDuplicated(res$meta_data$LABEL) > 0L)
})

test_that("util_ensure_label avoids pre-existing duplicate fallback labels", {
  set.seed(1)
  meta_data <- data.frame(
    VAR_NAMES = c("v", "v", "already"),
    LABEL = c("same", "same", "v: same DUPLICATE"),
    stringsAsFactors = FALSE
  )

  res <- suppressWarnings(.util_ensure_label(meta_data, LABEL))

  expect_false(anyDuplicated(res$meta_data$LABEL) > 0L)
  expect_equal(res$meta_data$LABEL[[1]], "v: same")
  expect_true(startsWith(res$meta_data$LABEL[[2]], "v: same DUPLICATE "))
  expect_equal(res$meta_data$LABEL[[3]], "v: same DUPLICATE")
})

test_that("util_ensure_label shortens long labels while preserving originals", {
  meta_data <- data.frame(
    VAR_NAMES = c("very_long_variable_name_one", "very_long_variable_name_two"),
    LABEL = c(
      "This is a deliberately long label for the first variable",
      "This is a deliberately long label for the second variable"
    ),
    stringsAsFactors = FALSE
  )

  res <- suppressWarnings(
    .util_ensure_label(meta_data, LABEL, max_label_len = 20)
  )

  expect_true(all(nchar(res$meta_data$LABEL) <= 20))
  expect_true("ORIGINAL_LABEL" %in% names(res$meta_data))
  expect_equal(res$meta_data$ORIGINAL_LABEL, meta_data$LABEL)
  expect_match(res$label_modification_text, "abbreviated labels")
})

test_that("util_ensure_label resolves abbreviation collisions", {
  meta_data <- data.frame(
    VAR_NAMES = c("var_one", "var_two"),
    LABEL = c("Alpha beta gamma delta", "Alpha b..."),
    stringsAsFactors = FALSE
  )

  res <- suppressWarnings(
    .util_ensure_label(meta_data, LABEL, max_label_len = 10)
  )

  expect_equal(res$meta_data$LABEL, c("Alpha b 2", "Alpha b..."))
  expect_false(anyDuplicated(res$meta_data$LABEL) > 0L)
  expect_equal(res$meta_data$ORIGINAL_LABEL, meta_data$LABEL)
  expect_match(res$label_modification_text, "abbreviation")
})

test_that("util_ensure_label moves VAR_NAMES labels to a helper column", {
  meta_data <- data.frame(
    VAR_NAMES = c("v1", "v2"),
    LABEL = c("label 1", "label 2"),
    stringsAsFactors = FALSE
  )

  res <- util_ensure_label(meta_data, VAR_NAMES)

  expect_equal(res$label_col, "VAR_NAMES_1")
  expect_equal(res$meta_data$VAR_NAMES, c("v1", "v2"))
  expect_equal(res$meta_data$VAR_NAMES_1, c("v1", "v2"))
})

test_that("util_ensure_label also normalizes secondary label columns", {
  withr::local_options(dataquieR.MAX_LABEL_LEN = 18)

  meta_data <- data.frame(
    VAR_NAMES = c("v1", "v2"),
    LABEL = c("same", "same"),
    LONG_LABEL = c(
      "This is a very long first long label",
      "This is a very long second long label"
    ),
    LABEL_DE = c("", "Deutsches Label"),
    stringsAsFactors = FALSE
  )

  res <- suppressWarnings(util_ensure_label(meta_data, LABEL))

  expect_false(anyDuplicated(res$meta_data[[LABEL]]) > 0L)
  expect_false(any(util_empty(res$meta_data[["LABEL_DE"]])))
  expect_true(nchar(res$meta_data[["LABEL_DE"]][[1]]) <= MAX_LABEL_LEN)
  expect_match(res$label_modification_text, "duplicated labels")
})

test_that("util_ensure_label uses long-label limits for primary long labels", {
  meta_data <- data.frame(
    VAR_NAMES = c("v1", "v2"),
    LABEL = c("short label", "other label"),
    LONG_LABEL = c(strrep("long ", 80), "short"),
    stringsAsFactors = FALSE
  )

  res <- suppressWarnings(util_ensure_label(meta_data, LONG_LABEL))

  expect_equal(res$label_col, LONG_LABEL)
  expect_true(nchar(res$meta_data[[LONG_LABEL]][[1L]]) > MAX_LABEL_LEN)
  expect_true(nchar(res$meta_data[[LONG_LABEL]][[1L]]) <= MAX_LONG_LABEL_LEN)
  expect_match(res$label_modification_text, "abbreviation")
})

test_that("short-label helpers create readable unique labels", {
  shortened <- .util_make_readable_short_labels(
    label = c("Alpha beta gamma delta", "Alpha beta gamma delta"),
    var_names = c("first_variable", "second_variable"),
    max_label_len = 18
  )

  expect_length(shortened, 2)
  expect_false(anyDuplicated(shortened) > 0L)
  expect_true(all(nchar(shortened) <= 18))

  expect_equal(.util_shorten_var_name("abc", 10), "abc")
  expect_true(nchar(.util_shorten_var_name(
    "abcdefghijklmnopqrstuvwxyz",
    8
  )) <= 8)
  expect_equal(.util_shorten_var_name("abc", 0), "")
  expect_equal(.util_shorten_label_at_word("abc", 0), "")
  expect_equal(.util_add_short_label_suffix("abcdef", "S", 10), "abcdef S")
  expect_equal(.util_add_short_label_suffix("abcdef", "too-long", 5), "")
})

test_that("short-label helpers handle empty anchors and invalid inputs", {
  expect_error(
    .util_make_readable_short_labels(
      label = c("Alpha", "Beta"),
      var_names = "only_one_name",
      max_label_len = 12
    ),
    "label and var_names"
  )

  expect_equal(
    .util_make_readable_short_label(
      label = "Alpha beta gamma",
      var_name = "",
      max_label_len = 10
    ),
    "Alpha b..."
  )
  expect_equal(.util_shorten_var_name("", 3), "var")
  expect_equal(.util_shorten_label_at_word("abc def", 2), "ab")
})

test_that("short-label helpers cover deterministic fallback branches", {
  squished <- .util_squish_label("  alpha/beta\\gamma \n delta  ")
  expect_equal(squished, "alpha beta gamma delta")
  expect_equal(.util_squish_label(NA_character_), "")

  expect_equal(
    .util_make_readable_short_label(
      label = "",
      var_name = "very_long_variable_name",
      max_label_len = 14
    ),
    .util_shorten_label_at_word("very_long_variable_name", 14)
  )

  expect_equal(.util_shorten_var_name("abcdef", 1), substr(
    rlang::hash("abcdef"), 1, 1
  ))
  expect_equal(.util_shorten_label_at_word("!!! abc def", 7), "!!!...")
  expect_equal(
    .util_add_short_label_suffix("abc", "2",
      max_label_len = 6,
      min_stem_len = 4
    ),
    ""
  )

  expect_equal(
    .util_make_unique_short_label(
      x = "",
      label = "Alpha beta gamma",
      var_name = "fallback_var",
      seen_labels = c("", "Alpha 2"),
      max_label_len = 12
    ),
    "fallback_var"
  )

  expect_error(
    .util_make_unique_short_label(
      x = "",
      label = "Alpha beta gamma",
      var_name = "",
      seen_labels = c("", "var", "Alpha 2"),
      max_label_len = 4
    ),
    "Could not create recognizable"
  )
})

test_that(
  paste0(
    "short-label helpers use hash suffixes after ",
    "deterministic fallbacks"
  ),
  {
    x <- "Alpha..."
    var_name <- "fallback_var"
    max_label_len <- 14L
    seen_labels <- c(
      x,
      vapply(
        as.character(2:1001),
        function(suffix) {
          .util_add_short_label_suffix(
            x = x,
            suffix = suffix,
            max_label_len = max_label_len,
            min_stem_len = 4L
          )
        },
        FUN.VALUE = character(1)
      ),
      .util_shorten_var_name(var_name, max_label_len)
    )

    label <- .util_make_unique_short_label(
      x = x,
      label = "Alpha beta gamma delta",
      var_name = var_name,
      seen_labels = seen_labels,
      max_label_len = max_label_len
    )

    expect_false(label %in% seen_labels)
    expect_true(nchar(label) <= max_label_len)
    expect_match(label, "\\[[[:xdigit:]]+\\]$")
  }
)
