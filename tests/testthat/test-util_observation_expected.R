util_observation_expected_fixture <- function() {
  study_data <- data.frame(
    part_study = c(1, 1, 0, NA),
    part_exam = c(1, 0, 1, 1),
    value = c(10, 20, 30, 40)
  )

  meta_data <- data.frame(
    VAR_NAMES = c("part_study", "part_exam", "value"),
    LABEL = c("Participation in study", "Participation in exam", "Value"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    PART_VAR = c("part_study", "part_study", "part_exam")
  )

  list(study_data = study_data, meta_data = meta_data)
}

test_that("util_all_intro_vars_for_rv maps participation hierarchy", {
  skip_on_cran()

  fixture <- util_observation_expected_fixture()

  expect_identical(
    util_all_intro_vars_for_rv(
      "value",
      fixture$study_data,
      fixture$meta_data,
      label_col = VAR_NAMES,
      expected_observations = "ALL"
    ),
    character()
  )
  expect_identical(
    unname(util_all_intro_vars_for_rv(
      "value",
      fixture$study_data,
      fixture$meta_data,
      label_col = VAR_NAMES,
      expected_observations = "SEGMENT"
    )),
    "part_exam"
  )
  expect_identical(
    unname(util_all_intro_vars_for_rv(
      "value",
      fixture$study_data,
      fixture$meta_data,
      label_col = VAR_NAMES,
      expected_observations = "HIERARCHY"
    )),
    c("part_study", "part_exam")
  )
  expect_identical(
    unname(util_all_intro_vars_for_rv(
      "Value",
      fixture$study_data,
      fixture$meta_data,
      label_col = LABEL,
      expected_observations = "HIERARCHY"
    )),
    c("Participation in study", "Participation in exam")
  )
})

test_that("util_observation_expected applies ALL, SEGMENT, and HIERARCHY", {
  skip_on_cran()

  fixture <- util_observation_expected_fixture()

  expect_identical(
    util_observation_expected(
      "value",
      fixture$study_data,
      fixture$meta_data,
      label_col = VAR_NAMES,
      expected_observations = "ALL"
    ),
    c(TRUE, TRUE, TRUE, TRUE)
  )
  expect_identical(
    util_observation_expected(
      "value",
      fixture$study_data,
      fixture$meta_data,
      label_col = VAR_NAMES,
      expected_observations = "SEGMENT"
    ),
    c(TRUE, FALSE, TRUE, TRUE)
  )
  expect_identical(
    util_observation_expected(
      "value",
      fixture$study_data,
      fixture$meta_data,
      label_col = VAR_NAMES,
      expected_observations = "HIERARCHY"
    ),
    c(TRUE, FALSE, FALSE, FALSE)
  )
})

test_that(
  "util_all_intro_vars_for_rv warns for unknown participation metadata",
  {
    skip_on_cran()

    fixture <- util_observation_expected_fixture()
    fixture$meta_data[fixture$meta_data[[VAR_NAMES]] == "value", PART_VAR] <-
      "missing_part"

    expect_warning(
      intro_vars <- util_all_intro_vars_for_rv(
        "value",
        fixture$study_data,
        fixture$meta_data,
        label_col = VAR_NAMES,
        expected_observations = "SEGMENT"
      ),
      "must contain names of study variables"
    )
    expect_identical(intro_vars, character())
  }
)

test_that("util_observation_expected assumes missing participation variables", {
  skip_on_cran()

  fixture <- util_observation_expected_fixture()
  fixture$meta_data <- rbind(
    fixture$meta_data,
    data.frame(
      VAR_NAMES = "missing_part",
      LABEL = "Missing participation",
      DATA_TYPE = DATA_TYPES$INTEGER,
      SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
      PART_VAR = "missing_part"
    )
  )
  fixture$meta_data[fixture$meta_data[[VAR_NAMES]] == "value", PART_VAR] <-
    "missing_part"

  expect_message(
    result <- util_observation_expected(
      "value",
      fixture$study_data,
      fixture$meta_data,
      label_col = VAR_NAMES,
      expected_observations = "SEGMENT"
    ),
    "all observations are expected"
  )
  expect_identical(result, c(TRUE, TRUE, TRUE, TRUE))
})

test_that("util_count_expected_observations aggregates expected rows", {
  skip_on_cran()

  fixture <- util_observation_expected_fixture()

  expect_identical(
    util_count_expected_observations(
      c("part_exam", "value"),
      fixture$study_data,
      fixture$meta_data,
      label_col = VAR_NAMES,
      expected_observations = "ALL"
    ),
    c(part_exam = 4L, value = 4L)
  )
  expect_identical(
    util_count_expected_observations(
      c("part_exam", "value"),
      fixture$study_data,
      fixture$meta_data,
      label_col = VAR_NAMES,
      expected_observations = "SEGMENT"
    ),
    c(part_exam = 2L, value = 3L)
  )
  expect_identical(
    util_count_expected_observations(
      c("part_exam", "value"),
      fixture$study_data,
      fixture$meta_data,
      label_col = VAR_NAMES,
      expected_observations = "HIERARCHY"
    ),
    c(part_exam = 2L, value = 1L)
  )
})
