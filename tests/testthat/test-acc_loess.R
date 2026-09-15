acc_loess_fortests_urls <- c(
  meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData", # nolint: line_length_linter.
  study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData" # nolint: line_length_linter.
)

acc_loess_fortests_state <- new.env(parent = emptyenv())
acc_loess_fortests_state$available <- NA
acc_loess_fortests_state$failed_url <- NA_character_

acc_loess_can_read_url <- function(url) {
  con <- NULL
  tryCatch({
    con <- base::url(url)
    open(con, open = "rb")
    length(readBin(con, what = "raw", n = 1L)) == 1L
  }, error = function(e) {
    FALSE
  }, warning = function(e) {
    FALSE
  }, finally = {
    if (!is.null(con) && isOpen(con)) {
      close(con)
    }
  })
}

skip_if_acc_loess_fortests_unavailable <- function() {
  testthat::skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")

  if (is.na(acc_loess_fortests_state$available)) {
    for (url in acc_loess_fortests_urls) {
      if (!isTRUE(acc_loess_can_read_url(url))) {
        acc_loess_fortests_state$available <- FALSE
        acc_loess_fortests_state$failed_url <- url
        break
      }
    }
    if (is.na(acc_loess_fortests_state$available)) {
      acc_loess_fortests_state$available <- TRUE
    }
  }

  if (!isTRUE(acc_loess_fortests_state$available)) {
    testthat::skip(sprintf(
      "External acc_loess fixture is unavailable: %s",
      acc_loess_fortests_state$failed_url
    ))
  }
}

acc_loess_fortests_fixture <- local({
  fixture <- NULL

  function() {
    if (is.null(fixture)) {
      meta_data <- prep_get_data_frame(acc_loess_fortests_urls[["meta_data"]])
      study_data <- prep_get_data_frame(
        acc_loess_fortests_urls[["study_data"]],
        keep_types = TRUE
      )
      fixture <- list(
        study_data = study_data,
        meta_data = prep_scalelevel_from_data_and_metadata(
          study_data = study_data,
          meta_data = meta_data
        )
      )
    }

    fixture
  }
})

test_that("acc_loess works without label_col and catches wrong inputs", {
  skip_on_cran() # slow

  # Use testthat::local_reproducible_output() when debugging locally.
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  require_english_locale_and_berlin_tz()
  skip_on_cran() # slow test
  skip_if_translated()
  skip_if_acc_loess_fortests_unavailable()
  fixture <- acc_loess_fortests_fixture()
  meta_data <- fixture$meta_data
  study_data <- fixture$study_data
  expect_warning(
    expect_error(
      {
        res1 <-
          acc_loess(
            resp_vars = "v00014", study_data = study_data,
            meta_data = meta_data
          )
      },
      regexp = paste(".*Argument time_vars is NULL"),
      perl = TRUE
    ),
    regexp = paste(
      "Missing argument .+time_vars.+ without default value.",
      "Setting to NULL. As a dataquieR developer"
    ),
    perl = TRUE
  )

  expect_warning(
    expect_error(
      {
        res1 <-
          acc_loess(
            resp_vars = "v00014", study_data = study_data,
            meta_data = meta_data, group_vars = "v00016"
          )
      },
      regexp = paste(".*Argument time_vars is NULL"),
      perl = TRUE
    ),
    regexp = paste(
      "Missing argument .+time_vars.+ without default value.",
      "Setting to NULL. As a dataquieR developer"
    ),
    perl = TRUE
  )

  sd1 <- study_data
  sd1[["v00017"]][1:1000] <- NA
  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "v00014", study_data = sd1, # continuous variable
        meta_data = meta_data, group_vars = "v00016",
        time_vars = "v00017"
      ),
    regexp = "Due to missing values in v00016 or v00017, N = 1243 observations were excluded. Due to missing values in v00014, N = 82 observations were excluded" # nolint: line_length_linter.
  )

  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "v00008", # categorical variable
        study_data = study_data,
        meta_data = meta_data,
        group_vars = "v00011",
        time_vars = "v00013"
      ),
    regexp = sprintf(
      "(%s|%s|%s)",
      paste(
        "Due to missing values in v00011 or v00013,",
        "N = 218 observations were excluded. Due to missing",
        "values in v00008, N = 270 observations were",
        "excluded additionally."
      ),
      "Recoded 1 variable.s.",
      paste(
        "Levels of the group_var with too few observations",
        "were discarded .level USR_599.."
      )
    )
  )

  sd1 <- study_data
  sd1[["v00017"]] <- as.character(sd1[["v00017"]])
  sd1[["v00017"]][1:1000] <- "2001-02-29"
  sd1[["v00013"]] <- as.character(sd1[["v00013"]])
  sd1[["v00013"]][1:1000] <- "2001-02-29"
  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "v00014", study_data = sd1,
        meta_data = meta_data, group_vars = "v00016",
        time_vars = "v00017"
      ),
    regexp = sprintf(
      "(%s|%s)",
      paste(
        "Data type transformation of .v00017. introduced",
        "1000 additional missing values"
      ),
      paste(
        "Due to missing values in v00016 or v00017,",
        "N = 1243 observations were excluded. Due to",
        "missing values in v00014, N = 82 observations",
        "were excluded"
      )
    ),
    perl = TRUE
  )
  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "v00008", study_data = sd1,
        meta_data = meta_data, group_vars = "v00011",
        time_vars = "v00013"
      ),
    regexp = sprintf(
      "(%s|%s|%s|%s)",
      paste(
        "Data type transformation of .v00013. introduced",
        "1000 additional missing values"
      ),
      paste(
        "Due to missing values in v00011 or v00013,",
        "N = 1150 observations were excluded. Due to missing",
        "values in v00008, N = 211 observations were",
        "excluded additionally."
      ),
      "Recoded 1 variable.s.",
      paste(
        "Levels of the group_var with too few observations",
        "were discarded .level USR_599.."
      )
    )
  )

  expect_error(
    res1 <-
      acc_loess(
        resp_vars = "v00014", study_data = study_data,
        meta_data = meta_data, group_vars = "v00016",
        min_obs_in_subgroup = 1:2,
        time_vars = "v00017"
      )
    ,
    regexp =
      paste("Need exactly one element in argument min_obs_in_subgroup, got 2"),
    perl = TRUE
  )

  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "v00014", study_data = study_data,
        meta_data = meta_data, group_vars = "v00016",
        resolution = Inf,
        min_obs_in_subgroup = 30,
        time_vars = "v00017"
      )
    ,
    regexp = sprintf(
      "(%s|%s)",
      paste("Argument resolution is not specified correctly and is set to 80 instead"), # nolint: line_length_linter.
      paste("Due to missing values in v00016 or v00017, N = 308 observations were excluded. Due to missing values in v00014, N = 131 observations were excluded") # nolint: line_length_linter.
    ),
    perl = TRUE
  )

  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "v00014", study_data = study_data,
        meta_data = meta_data, group_vars = "v00016",
        resolution = "12",
        min_obs_in_subgroup = 30,
        time_vars = "v00017"
      )
    ,
    regexp = "Due to missing values in v00016 or v00017, N = 308 observations were excluded. Due to missing values in v00014, N = 131 observations were excluded" # nolint: line_length_linter.
  )

  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "v00014", study_data = study_data,
        meta_data = meta_data, group_vars = "v00016",
        resolution = complex(imaginary = 12),
        min_obs_in_subgroup = 30,
        time_vars = "v00017"
      )
    ,
    regexp = sprintf(
      "(%s|%s)",
      "Argument resolution is not specified correctly and is set to 80 instead",
      "Due to missing values in v00016 or v00017, N = 308 observations were excluded. Due to missing values in v00014, N = 131 observations were excluded" # nolint: line_length_linter.
    )
  )

  suppressWarnings(
    expect_error(
      res1 <-
        acc_loess(
          resp_vars = "v00014", study_data = study_data,
          meta_data = meta_data, group_vars = "v00016",
          mark_time_points = "x",
          min_obs_in_subgroup = 30,
          time_vars = "v00017"
        )
      ,
      regexp =
        paste("Argument mark_time_points must be logical"),
      perl = TRUE
    )
  )

  suppressWarnings(
    expect_error(
      res1 <-
        acc_loess(
          resp_vars = "v00014", study_data = study_data,
          meta_data = meta_data, group_vars = "v00016",
          mark_time_points = TRUE,
          comparison_lines = 42,
          min_obs_in_subgroup = 30,
          time_vars = "v00017"
        )
      ,
      regexp =
        paste(
          ".+comparison_lines.+ needs to be a list of arguments",
          "as specified in the documentation."
        ),
      perl = TRUE
    )
  )

  sd0 <- study_data
  sd0$v00017 <- "XXXX"
  suppressMessages(
    expect_error(
      res1 <-
        acc_loess(
          resp_vars = "v00014", study_data = sd0,
          meta_data = meta_data, group_vars = "v00016",
          mark_time_points = TRUE,
          min_obs_in_subgroup = 30,
          time_vars = "v00017"
        )
      ,
      regexp = "Variable .v00017.+time_vars. has only NA observations",
      perl = TRUE
    )
  )

  suppressWarnings(
    expect_error(
      res1 <-
        acc_loess(
          resp_vars = "v00014", study_data = study_data,
          meta_data = meta_data, group_vars = "v00016",
          mark_time_points = "TRUE",
          min_obs_in_subgroup = 30,
          time_vars = "v00017"
        )
      ,
      regexp = "Argument mark_time_points must be logical"
    )
  )

  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "v00014", study_data = study_data,
        meta_data = meta_data, group_vars = "v00016",
        min_obs_in_subgroup = NA,
        time_vars = "v00017"
      )
  )

  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "v00014", study_data = study_data,
        meta_data = meta_data, group_vars = "v00016",
        min_obs_in_subgroup = "x",
        time_vars = "v00017"
      )
    ,
    regexp = sprintf(
      "(%s|%s)",
      "Argument resolution is not specified correctly and is set to 80 instead",
      "Due to missing values in v00016 or v00017, N = 308 observations were excluded. Due to missing values in v00014, N = 131 observations were excluded" # nolint: line_length_linter.
    )
  )

  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "v00014", study_data = study_data,
        meta_data = meta_data, group_vars = "v00016",
        time_vars = "v00017", plot_format = "BOTH"
      )
    ,
    regexp = "Due to missing values in v00016 or v00017, N = 308 observations were excluded. Due to missing values in v00014, N = 131 observations were excluded" # nolint: line_length_linter.
  )

  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    2
  )
  expect_lt(
    suppressWarnings(abs(mean(
      as.numeric(
        as.matrix(res1$SummaryPlotList$Loess_fits_combined$data)
      ),
      na.rm = TRUE
    ) - 21.82126)), 50
  )
})

test_that("acc_loess works with label_col", {
  skip_on_cran() # slow

  # Use testthat::local_reproducible_output() when debugging locally.
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  require_english_locale_and_berlin_tz()
  skip_if_acc_loess_fortests_unavailable()
  fixture <- acc_loess_fortests_fixture()
  meta_data <- fixture$meta_data
  study_data <- fixture$study_data
  expect_warning(
    expect_error(
      {
        res1 <-
          acc_loess(
            resp_vars = "CRP_0", study_data = study_data,
            meta_data = meta_data,
            label_col = LABEL
          )
      },
      regexp = paste(".*Argument time_vars is NULL"),
      perl = TRUE
    ),
    regexp = paste(
      "Missing argument .+time_vars.+ without default value.",
      "Setting to NULL. As a dataquieR developer"
    ),
    perl = TRUE
  )

  expect_warning(
    expect_error(
      {
        res1 <-
          acc_loess(
            resp_vars = "CRP_0", study_data = study_data,
            meta_data = meta_data, group_vars = "DEV_NO_0",
            label_col = LABEL
          )
      },
      regexp = paste(".*Argument time_vars is NULL"),
      perl = TRUE
    ),
    regexp = paste(
      "Missing argument .+time_vars.+ without default value.",
      "Setting to NULL. As a dataquieR developer"
    ),
    perl = TRUE
  )

  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "CRP_0", study_data = study_data,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0",
        label_col = LABEL, plot_format = "BOTH"
      ),
    regexp = "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded" # nolint: line_length_linter.
  )

  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    2
  )
  skip_on_cran()
  expect_lt(
    suppressWarnings(abs(mean(
      as.numeric(
        as.matrix(res1$SummaryPlotList$Loess_fits_combined$data)
      ),
      na.rm = TRUE
    ) - 21.82126)), 50
  )

  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "VO2_CAPCAT_0", # categorical variable
        study_data = study_data,
        meta_data = meta_data,
        group_vars = "USR_VO2_0",
        time_vars = "EXAM_DT_0",
        label_col = LABEL
      ),
    regexp = sprintf(
      "(%s|%s|%s)",
      paste(
        "Due to missing values in USR_VO2_0 or EXAM_DT_0,",
        "N = 218 observations were excluded. Due to missing",
        "values in VO2_CAPCAT_0, N = 270 observations were",
        "excluded additionally."
      ),
      "Recoded 1 variable.s.",
      paste(
        "Levels of the group_var with too few observations",
        "were discarded .level USR_599.."
      )
    )
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_false(
    inherits(try(ggplot_build(res1$SummaryPlotList$VO2_CAPCAT_0)), "try-error")
  )

  sd0 <- study_data
  sd0[["v00007"]][which(sd0[["v00011"]] == "USR_211")] <- 0 # one subgroup has constant values # nolint: line_length_linter.
  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "ASTHMA_0", # categorical variable
        study_data = sd0,
        meta_data = meta_data,
        group_vars = "USR_VO2_0",
        time_vars = "EXAM_DT_0",
        label_col = LABEL
      ),
    regexp = sprintf(
      "(%s|%s|%s)",
      paste(
        "Due to missing values in USR_VO2_0 or EXAM_DT_0,",
        "N = 218 observations were excluded. Due to missing",
        "values in ASTHMA_0, N = 259 observations were",
        "excluded additionally."
      ),
      "Recoded 1 variable.s.",
      paste(
        "Levels of the group_var with too few observations",
        "were discarded .level USR_599.."
      )
    )
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_false(
    inherits(try(ggplot_build(res1$SummaryPlotList$ASTHMA_0)), "try-error")
  )

  md0 <- meta_data
  md0[[SCALE_LEVEL]][md0[[LABEL]] == "VO2_CAPCAT_0"] <- SCALE_LEVELS$ORDINAL
  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "VO2_CAPCAT_0", # categorical variable
        study_data = study_data,
        meta_data = md0,
        group_vars = "USR_VO2_0",
        time_vars = "EXAM_DT_0",
        label_col = LABEL
      ),
    regexp = sprintf(
      "(%s|%s|%s)",
      paste(
        "Due to missing values in USR_VO2_0 or EXAM_DT_0,",
        "N = 218 observations were excluded. Due to missing",
        "values in VO2_CAPCAT_0, N = 270 observations were",
        "excluded additionally."
      ),
      "Recoded 1 variable.s.",
      paste(
        "Levels of the group_var with too few observations",
        "were discarded .level USR_599.."
      )
    )
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_false(
    inherits(try(ggplot_build(res1$SummaryPlotList$VO2_CAPCAT_0)), "try-error")
  )

  sd0 <- study_data
  sd0[["v00014"]][which(sd0[["v00016"]] == 4)] <- 2.5 # one subgroup has constant values # nolint: line_length_linter.
  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "CRP_0",
        study_data = sd0,
        meta_data = meta_data,
        group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0",
        label_col = LABEL
      ),
    regexp = "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 121 observations were excluded" # nolint: line_length_linter.
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_false(
    inherits(try(ggplot_build(res1$SummaryPlotList$CRP_0)), "try-error")
  )
})

test_that("acc_loess output matches", {
  skip_on_cran() # slow

  skip_if_acc_loess_fortests_unavailable()
  # Use testthat::local_reproducible_output() when debugging locally.
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  require_english_locale_and_berlin_tz()
  fixture <- acc_loess_fortests_fixture()
  meta_data <- fixture$meta_data
  study_data <- fixture$study_data
  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "CRP_0", study_data = study_data,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0",
        label_col = LABEL, plot_format = "BOTH"
      ),
    regexp = "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded" # nolint: line_length_linter.
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    2
  )
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  expect_doppelganger2(
    "loess facets plot for CRP_0 ok",
    res1$SummaryPlotList$Loess_fits_facets
  )
  expect_doppelganger2(
    "loess combined plot for CRP_0 ok",
    res1$SummaryPlotList$Loess_fits_combined
  )

  set.seed(32)
  nt <- 1000 # = nrow(study_data)/3
  p1 <- 0.3 # baseline probability
  p2 <- 0.7 # probability for the deviating examiner (at the end of the observation period or for a subset of observations) # nolint: line_length_linter.
  sd0 <- study_data
  sd0[["v00011"]] <- rep(c("USR_321", "USR_590", "USR_213"), 1000)
  set.seed(32)
  sd0[["v00007"]][which(sd0[["v00011"]] == "USR_321")] <-
    rbinom(n = nt, size = 1, prob = p1) # only noise
  sd0[["v00007"]][which(sd0[["v00011"]] == "USR_590")] <-
    rbinom(
      n = nt, size = 1,
      prob = (p2 - p1) / (nt - 1) * 1:nt + (p1 * nt - p2) / (nt - 1)
    )
  # increase over time
  sd0[["v00007"]][which(sd0[["v00011"]] == "USR_213")] <-
    c(
      rbinom(n = floor(1 / 3 * nt), size = 1, prob = p1),
      rbinom(n = floor(1 / 3 * nt), size = 1, prob = p2),
      rbinom(n = nt - 2 * floor(1 / 3 * nt), size = 1, prob = p1)
    )
  # switch between p1 and p2
  md0 <- meta_data
  md0[["RECODE_CASES"]] <- ""
  md0[["RECODE_CONTROL"]] <- ""
  md0[["RECODE_CASES"]][md0$VAR_NAMES == "v00007"] <- "yes"
  md0[["RECODE_CONTROL"]][md0$VAR_NAMES == "v00007"] <- "no"

  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "ASTHMA_0", # categorical variable
        study_data = sd0,
        meta_data = md0,
        group_vars = "USR_VO2_0",
        time_vars = "EXAM_DT_0",
        label_col = LABEL
      )
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_false(
    inherits(try(ggplot_build(res1$SummaryPlotList$ASTHMA_0)), "try-error")
  )
})

test_that("acc_loess min_obs_in_subgroups with label_col", {
  skip_on_cran() # slow

  skip_if_acc_loess_fortests_unavailable()
  # Use testthat::local_reproducible_output() when debugging locally.
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  require_english_locale_and_berlin_tz()
  fixture <- acc_loess_fortests_fixture()
  meta_data <- fixture$meta_data
  study_data <- fixture$study_data
  expect_message2(
    expect_error(
      {
        res1 <-
          acc_loess(
            resp_vars = "CRP_0", study_data = study_data,
            meta_data = meta_data, group_vars = "DEV_NO_0",
            time_vars = "LAB_DT_0",
            label_col = LABEL, min_obs_in_subgroup = 999
          )
      },
      regexp = paste("No data left after data preparation."),
      perl = TRUE
    ),
    regexp =
      sprintf(
        "(%s|%s)",
        "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded.", # nolint: line_length_linter.
        paste(
          "Levels of the group_var with too few observations",
          "were discarded .levels 1, 2, 3, 4, 5.+"
        )
      ),
    perl = TRUE
  )
})

test_that("acc_loess with co-vars output matches", {
  skip_on_cran() # slow

  skip_if_acc_loess_fortests_unavailable()
  # Use testthat::local_reproducible_output() when debugging locally.
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  require_english_locale_and_berlin_tz()
  fixture <- acc_loess_fortests_fixture()
  meta_data <- fixture$meta_data
  study_data <- fixture$study_data

  sd0 <- study_data
  sd0$v00003[1:10] <- NA
  sd0$v00002[11:20] <- NA
  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "CRP_0", study_data = sd0,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0", co_vars = c("AGE_0", "SEX_0"),
        label_col = LABEL
      ),
    regexp = "Due to missing values in DEV_NO_0, AGE_0, SEX_0 or LAB_DT_0, N = 327 observations were excluded. Due to missing values in CRP_0, N = 130 observations were excluded" # nolint: line_length_linter.
  )

  sd0 <- study_data
  sd0$v00014 <- as.factor(sd0$v00014)
  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "CRP_0", study_data = sd0,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0", co_vars = c("AGE_0", "SEX_0"),
        label_col = LABEL, plot_format = "BOTH"
      ),
    regexp = "Due to missing values in DEV_NO_0, AGE_0, SEX_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded" # nolint: line_length_linter.
  )

  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    2
  )
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  expect_doppelganger2(
    "loess facets plot for CRP_0 with Covars ok",
    res1$SummaryPlotList$Loess_fits_facets
  )
  expect_doppelganger2(
    "loess combined plot for CRP_0 with Covars ok",
    res1$SummaryPlotList$Loess_fits_combined
  )

  nt <- 1000 # = nrow(study_data)/3
  p1 <- 0.3 # baseline probability
  p2 <- 0.7 # probability for the deviating examiner (at the end of the observation period or for a subset of observations) # nolint: line_length_linter.
  p3 <- 0.5 # deviation due to the co_var (categorical)
  sd0 <- study_data
  sd0[["v00011"]] <- rep(c("USR_321", "USR_590", "USR_213"), 1000)
  sd0[["v00002"]] <- rep(c(0, 1), 1500)
  set.seed(32)
  sd0[["v00007"]][which(sd0[["v00011"]] == "USR_321" & sd0[["v00002"]] == 0)] <-
    rbinom(n = nt / 2, size = 1, prob = p1) # only noise
  sd0[["v00007"]][which(sd0[["v00011"]] == "USR_321" & sd0[["v00002"]] == 1)] <-
    rbinom(n = nt / 2, size = 1, prob = p1 * p3) # only noise, with co-var effect # nolint: line_length_linter.
  sd0[["v00007"]][which(sd0[["v00011"]] == "USR_590")] <-
    rbinom(
      n = nt, size = 1,
      prob = ((p2 - p1) / (nt - 1) * 1:nt + (p1 * nt - p2) / (nt - 1)) *
        (1 - sd0[["v00002"]][which(sd0[["v00011"]] == "USR_590")] * (1 - p3))
    )
  # increase over time
  sd0[["v00007"]][which(sd0[["v00011"]] == "USR_213" & sd0[["v00002"]] == 0)] <-
    c(
      rbinom(n = floor(1 / 3 * nt / 2), size = 1, prob = p1),
      rbinom(n = floor(1 / 3 * nt / 2), size = 1, prob = p2),
      rbinom(n = nt / 2 - 2 * floor(1 / 3 * nt / 2), size = 1, prob = p1)
    )
  # switch between p1 and p2
  sd0[["v00007"]][which(sd0[["v00011"]] == "USR_213" & sd0[["v00002"]] == 1)] <-
    c(
      rbinom(n = floor(1 / 3 * nt / 2), size = 1, prob = p1 * p3),
      rbinom(n = floor(1 / 3 * nt / 2), size = 1, prob = p2 * p3),
      rbinom(n = nt / 2 - 2 * floor(1 / 3 * nt / 2), size = 1, prob = p1 * p3)
    )
  md0 <- meta_data
  md0[["RECODE_CASES"]] <- ""
  md0[["RECODE_CONTROL"]] <- ""
  md0[["RECODE_CASES"]][md0$VAR_NAMES == "v00007"] <- "yes"
  md0[["RECODE_CONTROL"]][md0$VAR_NAMES == "v00007"] <- "no"

  # Historical sex-specific categorical plot checks removed here.

  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "ASTHMA_0", # categorical variable
        study_data = sd0,
        meta_data = md0,
        group_vars = "USR_VO2_0",
        time_vars = "EXAM_DT_0",
        co_vars = "SEX_0",
        label_col = LABEL
      )
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_false(
    inherits(try(ggplot_build(res1$SummaryPlotList$ASTHMA_0)), "try-error")
  )
})

test_that("acc_loess works for all time ranges", {
  skip_on_cran() # slow

  skip_if_acc_loess_fortests_unavailable()
  # Use testthat::local_reproducible_output() when debugging locally.
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  require_english_locale_and_berlin_tz()
  skip_on_cran() # slow test
  fixture <- acc_loess_fortests_fixture()
  meta_data <- fixture$meta_data
  study_data <- fixture$study_data
  sd0 <- study_data
  v <- subset(meta_data, LABEL == "LAB_DT_0", VAR_NAMES, TRUE)
  sd0[[v]] <- min(sd0[[v]], na.rm = TRUE)
  expect_error(
    suppressWarnings(res1 <-
      acc_loess(
        resp_vars = "CRP_0", study_data = sd0,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0",
        label_col = LABEL
      )),
    regexp = paste(
      "Variable .+LAB_DT_0.+time_vars.+has fewer distinct values",
      "than required"
    )
  )

  sd0 <- study_data
  sd0[["v00017"]] <- sample(sd0[["v00017"]][c(100, 700, 1500)],
    size = nrow(sd0), replace = TRUE
  ) # LAB_DT_0
  expect_silent(
    suppressMessages(res1 <-
      acc_loess(
        resp_vars = "CRP_0", study_data = sd0,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0",
        label_col = LABEL,
        comparison_lines = list(
          type = "quartiles",
          color = "red",
          linetype = 3
        )
      ))
  )

  sd0 <- study_data
  sd0[["v00017"]][1:3] <- sd0[["v00017"]][1:3] - 2000 * 60 * 60 * 24 * 365
  # extreme gap, unnoticed typo in year (inspired from real-world data case)
  md0 <- meta_data
  md0[["HARD_LIMITS"]][md0[["VAR_NAMES"]] == "v00017"] <- NA
  sd0[["v00013"]][1:3] <- sd0[["v00013"]][1:3] - 2000 * 60 * 60 * 24 * 365
  md0[["HARD_LIMITS"]][md0[["VAR_NAMES"]] == "v00013"] <- NA
  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "CRP_0", study_data = sd0,
        meta_data = md0, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0",
        label_col = LABEL,
        comparison_lines = list(
          type = "quartiles",
          color = "red",
          linetype = 3
        )
      ),
    regexp = "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded" # nolint: line_length_linter.
  )
  expect_false(
    inherits(try(ggplot_build(res1$SummaryPlotList$CRP_0)), "try-error")
  )

  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "v00008", # categorical variable
        study_data = sd0, meta_data = md0,
        group_vars = "v00011",
        time_vars = "v00013"
      ),
    regexp = sprintf(
      "(%s|%s|%s)",
      paste(
        "Due to missing values in v00011 or v00013,",
        "N = 218 observations were excluded. Due to missing",
        "values in v00008, N = 270 observations were",
        "excluded additionally."
      ),
      "Recoded 1 variable.s.",
      paste(
        "Levels of the group_var with too few observations",
        "were discarded .level USR_599.."
      )
    )
  )
  expect_false(
    inherits(try(ggplot_build(res1$SummaryPlotList$v00008)), "try-error")
  )


  sd0 <- study_data
  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "CRP_0", study_data = sd0,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0",
        resolution = 0.1,
        label_col = LABEL
      ),
    regexp = sprintf(
      "(%s|%s)",
      "Argument resolution is not specified correctly and is set to 80 instead",
      "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded" # nolint: line_length_linter.
    )
  )

  expect_silent(
    suppressMessages(res1 <-
      acc_loess(
        resp_vars = "CRP_0", study_data = sd0,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0",
        resolution = 10,
        label_col = LABEL
      ))
  )

  expect_silent(
    suppressMessages(res1 <-
      acc_loess(
        resp_vars = "CRP_0", study_data = sd0,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0",
        resolution = 10000,
        label_col = LABEL
      ))
  )

  set.seed(1024)
  sd0 <- data.frame(
    "v00014" = c(
      rnorm(4000, mean = 10, sd = 1),
      rnorm(4000, mean = 20, sd = 10),
      rnorm(12, mean = 5, sd = 1),
      rnorm(50, mean = 5, sd = 1)
    ),
    "v00016" = c(
      rep(1, 4000),
      rep(2, 4000),
      rep(3, 12),
      rep(4, 50)
    ),
    "v00017" = c(
      sample(study_data$v00017,
        size = 8012, replace = TRUE
      ),
      rep(min(study_data$v00017, na.rm = TRUE), 25),
      rep(max(study_data$v00017, na.rm = TRUE), 25)
    )
  )
  md0 <- meta_data[which(meta_data$VAR_NAMES %in% colnames(sd0)), ]
  expect_message2(
    acc_test <-
      acc_loess(
        resp_vars = "v00014", study_data = sd0,
        meta_data = md0, group_vars = "v00016",
        time_vars = "v00017",
        plot_observations = TRUE
      ),
    regexp = sprintf(
      "(%s|%s|%s)",
      paste(
        "Not all entries in .KEY_STUDY_SEGMENT. are found in .VAR_NAMES.+",
        "Could convert 0 of these 3 to match .VAR_NAMES.+standard.+This",
        "may be caused by providing subsets of .meta_data.+"
      ),
      "Due to missing values in",
      paste(
        "Levels of the group_var with too few observations were",
        "discarded .levels 3, 4.+"
      )
    ),
    perl = TRUE
  )
  expect_true("SummaryPlotList" %in% names(acc_test))
  expect_lt(
    suppressWarnings(abs(mean(
      as.numeric(
        as.matrix(acc_test$SummaryPlotList$v00014$data)
      ),
      na.rm = TRUE
    ) - mean(sd0$v00014))), 50
  )

  sd0 <- study_data
  g <- subset(meta_data, LABEL == "DEV_NO_0", VAR_NAMES, TRUE)
  set.seed(42)
  # Eleven or more groups use R's standard colors.
  sd0[[g]][!is.na(sd0[[g]])] <-
    sample(
      x = 11, size = sum(!is.na(sd0[[g]])),
      replace = TRUE
    )
  expect_message2(
    res0 <-
      acc_loess(
        resp_vars = "CRP_0", study_data = sd0,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0",
        label_col = LABEL, plot_format = "BOTH"
      )
  )

  # Eight or fewer groups use the dataquieR colors.
  sd0[[g]][!is.na(sd0[[g]])] <-
    sample(
      x = 8, size = sum(!is.na(sd0[[g]])),
      replace = TRUE
    )
  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "CRP_0", study_data = sd0,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0",
        label_col = LABEL, plot_format = "BOTH"
      )
  )

  g <- ggplot2::ggplot_build(res1$SummaryPlotList$Loess_fits_combined)
  got1 <- sort(unique(g$data[[1]][["colour"]]))
  hex_code <- sort(c( # the dataquieR colors
    "#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#8C510A"
  ))
  expect_equal(got1, hex_code)

  g <- ggplot2::ggplot_build(res0$SummaryPlotList$Loess_fits_combined)
  got0 <- sort(unique(g$data[[1]][["colour"]]))
  skip_if_not_installed("scales")
  ggs_default <- sort(scales::hue_pal()(11))

  expect_equal(got0, ggs_default)
})

test_that("acc_loess output matches plot_format=auto", {
  skip_on_cran() # slow

  skip_if_acc_loess_fortests_unavailable()
  # Use testthat::local_reproducible_output() when debugging locally.
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  require_english_locale_and_berlin_tz()
  fixture <- acc_loess_fortests_fixture()
  meta_data <- fixture$meta_data
  study_data <- fixture$study_data
  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "CRP_0", study_data = study_data,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0",
        label_col = LABEL, plot_format = "AUTO"
      ),
    regexp = "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded" # nolint: line_length_linter.
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    1
  )
  sd1 <- study_data
  set.seed(42)
  sd1$v00016 <- sample(1:20, size = nrow(sd1), replace = TRUE)
  expect_message2(
    res2 <-
      acc_loess(
        resp_vars = "CRP_0", study_data = sd1,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0",
        label_col = LABEL, plot_format = "AUTO"
      ),
    regexp = "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 60 observations were excluded. Due to missing values in CRP_0, N = 241 observations were excluded" # nolint: line_length_linter.
  )
  expect_true("SummaryPlotList" %in% names(res2))
  expect_equal(
    length(res2$SummaryPlotList),
    1
  )
  skip_on_cran()
  # Travis used to fail these vdiffr expectations.
  skip_if_not_installed("vdiffr")
  expect_doppelganger2(
    "loess plot for CRP_0 AUTO1 ok",
    res1$SummaryPlotList$CRP_0
  )
  expect_doppelganger2(
    "loess plot for CRP_0 AUTO2 ok",
    res2$SummaryPlotList$CRP_0
  )
})

test_that("acc_loess output matches plot_format=combined", {
  skip_on_cran() # slow

  skip_if_acc_loess_fortests_unavailable()
  # Use testthat::local_reproducible_output() when debugging locally.
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  require_english_locale_and_berlin_tz()
  fixture <- acc_loess_fortests_fixture()
  meta_data <- fixture$meta_data
  study_data <- fixture$study_data
  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "CRP_0", study_data = study_data,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0",
        label_col = LABEL, plot_format = "COMBINED"
      ),
    regexp = "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded" # nolint: line_length_linter.
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    1
  )
  skip_on_cran()
  # Travis used to fail these vdiffr expectations.
  skip_if_not_installed("vdiffr")
  expect_doppelganger2(
    "loess combined plot for CRP_0 COMBINED ok",
    res1$SummaryPlotList$CRP_0
  )
})

test_that("acc_loess output matches plot_format=facets", {
  skip_on_cran() # slow

  skip_if_acc_loess_fortests_unavailable()
  # Use testthat::local_reproducible_output() when debugging locally.
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  require_english_locale_and_berlin_tz()
  fixture <- acc_loess_fortests_fixture()
  meta_data <- fixture$meta_data
  study_data <- fixture$study_data
  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "CRP_0", study_data = study_data,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0",
        label_col = LABEL, plot_format = "FACETS"
      ),
    regexp = "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded" # nolint: line_length_linter.
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    1
  )
  skip_on_cran()
  # Travis used to fail these vdiffr expectations.
  skip_if_not_installed("vdiffr")
  expect_doppelganger2(
    "loess facets plot for CRP_0 FACETS ok",
    res1$SummaryPlotList$CRP_0
  )
})

test_that("acc_loess output matches plot_format=both", {
  skip_on_cran() # slow

  skip_if_acc_loess_fortests_unavailable()
  # Use testthat::local_reproducible_output() when debugging locally.
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  require_english_locale_and_berlin_tz()
  fixture <- acc_loess_fortests_fixture()
  meta_data <- fixture$meta_data
  study_data <- fixture$study_data
  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "CRP_0", study_data = study_data,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0",
        label_col = LABEL, plot_format = "BOTH"
      ),
    regexp = "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded" # nolint: line_length_linter.
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_equal(
    length(res1$SummaryPlotList),
    2
  )
  skip_on_cran()
  # Travis used to fail these vdiffr expectations.
  skip_if_not_installed("vdiffr")
  expect_doppelganger2(
    "loess facets plot for CRP_0 BOTH ok",
    res1$SummaryPlotList$Loess_fits_facets
  )
  expect_doppelganger2(
    "loess combined plot for CRP_0 BOTH ok",
    res1$SummaryPlotList$Loess_fits_combined
  )
})

test_that("acc_loess output matches plot_format=invalid1", {
  skip_on_cran() # slow

  skip_if_acc_loess_fortests_unavailable()
  # Use testthat::local_reproducible_output() when debugging locally.
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  require_english_locale_and_berlin_tz()
  fixture <- acc_loess_fortests_fixture()
  meta_data <- fixture$meta_data
  study_data <- fixture$study_data
  expect_error(
    res1 <-
      acc_loess(
        resp_vars = "CRP_0", study_data = study_data,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0",
        label_col = LABEL, plot_format = "invalid"
      ),
    regexp = "Argument plot_format must match the predicate"
  )
})

test_that("acc_loess output matches plot_format=invalid2", {
  skip_on_cran() # slow

  skip_if_acc_loess_fortests_unavailable()
  # Use testthat::local_reproducible_output() when debugging locally.
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  require_english_locale_and_berlin_tz()
  fixture <- acc_loess_fortests_fixture()
  meta_data <- fixture$meta_data
  study_data <- fixture$study_data
  expect_error(
    res1 <-
      acc_loess(
        resp_vars = "CRP_0", study_data = study_data,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0",
        label_col = LABEL, plot_format = 1:10
      ),
    regexp = "Need exactly one element in argument plot_format, got 10"
  )
})

test_that("acc_loess works without a grouping variable", {
  skip_on_cran() # slow

  # Use testthat::local_reproducible_output() when debugging locally.
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  require_english_locale_and_berlin_tz()
  skip_if_translated()
  skip_if_acc_loess_fortests_unavailable()
  fixture <- acc_loess_fortests_fixture()
  meta_data <- fixture$meta_data
  study_data <- fixture$study_data
  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "v00014", # continuous variable
        study_data = study_data,
        meta_data = meta_data,
        time_vars = "v00017",
        label_col = LABEL
      ),
    regexp = "Due to missing values in LAB_DT_0, N = 60 observations were excluded. Due to missing values in CRP_0, N = 241 observations were excluded" # nolint: line_length_linter.
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_false(
    inherits(try(ggplot_build(res1$SummaryPlotList$CRP_0)), "try-error")
  )

  expect_error(
    res1 <- suppressWarnings(suppressMessages(
      acc_loess(
        resp_vars = "v00014", # continuous variable
        study_data = study_data,
        meta_data = meta_data,
        group_vars = NA,
        time_vars = "v00017",
        co_vars = c("v00006", "v00010"),
        label_col = LABEL
      )
    )),
    NA
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_false(
    inherits(try(ggplot_build(res1$SummaryPlotList$CRP_0)), "try-error")
  )

  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "v00008", # categorical variable
        study_data = study_data,
        meta_data = meta_data,
        time_vars = "v00013",
        label_col = LABEL
      ),
    regexp = sprintf(
      "(%s|%s)",
      paste(
        "Due to missing values in EXAM_DT_0,",
        "N = 60 observations were excluded. Due to missing",
        "values in VO2_CAPCAT_0, N = 345 observations were",
        "excluded additionally."
      ),
      "Recoded 1 variable.s."
    )
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_false(
    inherits(try(ggplot_build(res1$SummaryPlotList$VO2_CAPCAT_0)), "try-error")
  )

  expect_error(
    res1 <- suppressWarnings(suppressMessages(
      acc_loess(
        resp_vars = "v00008", # categorical variable
        study_data = study_data,
        meta_data = meta_data,
        group_vars = NA,
        time_vars = "v00013",
        co_vars = c("v00006", "v00010"),
        label_col = LABEL
      )
    )),
    NA
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_false(
    inherits(try(ggplot_build(res1$SummaryPlotList$VO2_CAPCAT_0)), "try-error")
  )
})

test_that("optional features for acc_loess work as expected", {
  skip_on_cran() # slow

  skip_if_acc_loess_fortests_unavailable()
  # Use testthat::local_reproducible_output() when debugging locally.
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  require_english_locale_and_berlin_tz()
  fixture <- acc_loess_fortests_fixture()
  meta_data <- fixture$meta_data
  study_data <- fixture$study_data
  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "CRP_0", study_data = study_data,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0", co_vars = c("AGE_0", "SEX_0"),
        label_col = LABEL, mark_time_points = TRUE,
        plot_observations = TRUE, min_bandwidth = 0.5
      ),
    regexp = "Due to missing values in DEV_NO_0, AGE_0, SEX_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded" # nolint: line_length_linter.
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_false(
    inherits(try(ggplot_build(res1$SummaryPlotList$CRP_0)), "try-error")
  )

  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "CRP_0", study_data = study_data,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0", co_vars = c("AGE_0", "SEX_0"),
        label_col = LABEL, enable_GAM = TRUE
      ),
    regexp = "Due to missing values in DEV_NO_0, AGE_0, SEX_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded" # nolint: line_length_linter.
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_false(
    inherits(try(ggplot_build(res1$SummaryPlotList$CRP_0)), "try-error")
  )

  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "v00008", # categorical variable
        study_data = study_data,
        meta_data = meta_data,
        time_vars = "v00013",
        label_col = LABEL, enable_GAM = TRUE
      ),
    regexp = sprintf(
      "(%s|%s)",
      paste(
        "Due to missing values in EXAM_DT_0,",
        "N = 60 observations were excluded. Due to missing",
        "values in VO2_CAPCAT_0, N = 345 observations were",
        "excluded additionally."
      ),
      "Recoded 1 variable.s."
    )
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_false(
    inherits(try(ggplot_build(res1$SummaryPlotList$VO2_CAPCAT_0)), "try-error")
  )

  sd0 <- study_data
  sd0[["v00007"]][which(sd0[["v00011"]] == "USR_211")] <- 0 # one subgroup has constant values # nolint: line_length_linter.
  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "ASTHMA_0", # categorical variable
        study_data = sd0,
        meta_data = meta_data,
        group_vars = "USR_VO2_0",
        time_vars = "EXAM_DT_0",
        label_col = LABEL,
        exclude_constant_subgroups = TRUE,
        min_bandwidth = 0.5,
        min_proportion = 0.05
      ),
    regexp = sprintf(
      "(%s|%s|%s|%s)",
      paste(
        "Due to missing values in USR_VO2_0 or EXAM_DT_0,",
        "N = 218 observations were excluded. Due to missing",
        "values in ASTHMA_0, N = 259 observations were",
        "excluded additionally."
      ),
      "Recoded 1 variable.s.",
      paste(
        "Levels of the group_var with too few observations",
        "were discarded .level USR_599.."
      ),
      paste(
        "Levels of the group_var with constant values",
        "were discarded .level USR_211.."
      )
    )
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_false(
    inherits(try(ggplot_build(res1$SummaryPlotList$ASTHMA_0)), "try-error")
  )

  sd0 <- study_data
  sd0[["v00014"]][which(sd0[["v00016"]] == 4)] <- 2.5 # one subgroup has constant values # nolint: line_length_linter.
  expect_message2(
    res1 <-
      acc_loess(
        resp_vars = "CRP_0",
        study_data = sd0,
        meta_data = meta_data,
        group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0",
        label_col = LABEL,
        exclude_constant_subgroups = TRUE
      ),
    regexp = sprintf(
      "(%s|%s)",
      paste(
        "Due to missing values in DEV_NO_0 or LAB_DT_0,",
        "N = 308 observations were excluded.",
        "Due to missing values in CRP_0, N = 121",
        "observations were excluded additionally"
      ),
      paste(
        "Levels of the group_var with constant values",
        "were discarded .level 4.."
      )
    )
  )
  expect_true("SummaryPlotList" %in% names(res1))
  expect_false(
    inherits(try(ggplot_build(res1$SummaryPlotList$CRP_0)), "try-error")
  )
})
