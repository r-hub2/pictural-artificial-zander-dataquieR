test_that("prep_min_obs_level works", {
  skip_on_cran() # function not used, yet
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  label_col <- LABEL
  prep_prepare_dataframes(.internal = TRUE)
  expect_message2(
    x <- prep_min_obs_level(ds1,
      group_vars = "USR_BP_0",
      min_obs_in_subgroup = 50
    ),
    regexp =
      paste(
        "The following levels: .+USR_559.+ have < 50",
        "observations and are disregarded"
      ),
    perl = TRUE,
  )
  expect_equal(nrow(study_data) - nrow(x), 29)


  expect_error(
    x <- prep_min_obs_level(ds1,
      group_vars = character(0),
      min_obs_in_subgroup = 50
    ),
    regexp =
      paste(".+group_vars.+ is required to name exactly one variable."),
    perl = TRUE
  )

  expect_error(
    x <- prep_min_obs_level(ds1,
      group_vars = NULL,
      min_obs_in_subgroup = 50
    ),
    regexp =
      paste(".+group_vars.+ is required to be a character.* argument."),
    perl = TRUE
  )

  expect_message2(
    expect_error(
      x <- prep_min_obs_level(ds1,
        group_vars = letters,
        min_obs_in_subgroup = 50
      ),
      regexp =
        paste(".+group_vars.+ = .+a.+ is not a variable."),
      perl = TRUE
    ),
    regexp = sprintf(
      "(%s)",
      paste("Subsets based only on one variable possible.")
    ),
    perl = TRUE
  )

  expect_message2(
    x <- prep_min_obs_level(ds1,
      group_vars = "USR_BP_0",
      min_obs_in_subgroup = NA
    ),
    regexp = sprintf(
      "(%s|%s)",
      paste(
        "The following levels: .+USR_559.+ have < 30",
        "observations and are disregarded"
      ),
      paste(
        "argument .+min_obs_in_subgroup.+ was missing,",
        "not of length 1 or NA, setting to its default, 30"
      )
    ),
    perl = TRUE
  )
  expect_equal(nrow(study_data) - nrow(x), 29)

  ds1_dot <- ds1
  ds1_dot$USR_BP_0 <- NA
  expect_message2(
    expect_error(
      x <- prep_min_obs_level(ds1_dot,
        group_vars = "USR_BP_0",
        min_obs_in_subgroup = NA
      ),
      regexp = paste(
        "For .+group_vars.+ = .+USR_BP_0.+,",
        "observations cannot be counted."
      ),
      perl = TRUE
    ),
    regexp = sprintf(
      "(%s|%s)",
      paste(
        "The following levels: .+USR_559.+ have < 30",
        "observations and are disregarded"
      ),
      paste(
        "argument .+min_obs_in_subgroup.+ was missing,",
        "not of length 1 or NA, setting to its default, 30"
      )
    ),
    perl = TRUE
  )

  ds1_dot <- subset(ds1, USR_BP_0 != "USR_559")
  expect_silent(
    x <- prep_min_obs_level(ds1_dot,
      group_vars = "USR_BP_0",
      min_obs_in_subgroup = 50
    )
  )
  expect_equal(nrow(ds1_dot) - nrow(x), 0)
})

test_that("prep_min_obs_level filters local subgroups", {
  skip_on_cran()

  study_data <- data.frame(
    id = seq_len(5),
    observer = c("A", "A", "B", "B", "C"),
    value = c(1, 2, 3, 4, 5)
  )

  expect_message2(
    filtered <- prep_min_obs_level(
      study_data,
      group_vars = "observer",
      min_obs_in_subgroup = 2
    ),
    "The following levels: .+C.+ have < 2 observations"
  )
  expect_equal(filtered$id, 1:4)

  expect_silent(
    unchanged <- prep_min_obs_level(
      study_data,
      group_vars = "observer",
      min_obs_in_subgroup = 1
    )
  )
  expect_identical(unchanged, study_data)

  expect_message2(
    expect_message2(
      defaulted <- prep_min_obs_level(
        study_data,
        group_vars = "observer",
        min_obs_in_subgroup = NA
      ),
      "setting to its default, 30"
    ),
    "The following levels: .+A.+B.+C.+ have < 30 observations"
  )
  expect_equal(nrow(defaulted), 0)
})
