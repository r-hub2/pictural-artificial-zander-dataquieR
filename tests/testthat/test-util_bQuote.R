skip_on_cran()

test_that("util_bQuote quotes names with formula-hostile characters", {
  nm <- c(
    "resp`tick",
    "cov\\slash",
    "income$month",
    "group label (1)",
    "Größe µg/m³",
    .dq_unicode_label_samples()
  )
  dat <- data.frame(
    setNames(
      c(
        list(
          seq_len(8),
          seq(2, 16, by = 2),
          rep(c(0, 1), 4),
          rep(c(1, 2), each = 4),
          rep(c(1, 3), 4)
        ),
        rep(list(rep(c(2, 4), 4)), length(nm) - 5L)
      ),
      nm
    ),
    check.names = FALSE
  )

  fmla <- as.formula(paste0(
    util_bQuote(nm[[1]]),
    " ~ ",
    paste(util_bQuote(nm[-1]), collapse = " + ")
  ))

  expect_silent(stats::lm(fmla, data = dat))
  expect_true(all(nm %in% all.vars(fmla)))
})

test_that("util_bQuote keeps empty and missing inputs stable", {
  skip_on_cran()

  expect_identical(util_bQuote(character(0)), character(0))
  expect_identical(
    util_bQuote(c(NA_character_, "a`b", "c\\d")),
    c(NA_character_, "`a\\`b`", "`c\\\\d`")
  )
})
