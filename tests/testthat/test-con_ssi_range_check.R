test_that("con_ssi_range_check to summarize and plot ranges for ssi", {
  skip_on_cran()

  set.seed(25)
  x <- 1:5
  s1 <- sample(x, size = 10, replace = TRUE)
  x <- 1:4
  s2 <- sample(x, size = 10, replace = TRUE)

  study_data <- data.frame(
    scaleA1 = s1,
    scaleA2 = s1,
    scaleA3 = s2,
    scaleA4 = s1,
    scaleA5 = s2
  )
  set.seed(120)
  vec1 <- sample(3:10, size = 198, replace = TRUE)
  vec1 <- c(vec1, 35, 38)
  start_base <- as.POSIXct("2025-03-02 09:00:00", tz = "UTC")
  cum_secs <- cumsum(vec1)
  start_times <- c(start_base, start_base + head(cum_secs, -1))
  end_times   <- start_base + cum_secs

  new_study_data <- data.frame(
    START_TIME = start_times,
    END_TIME = end_times
  )
  study_data <- cbind(study_data, new_study_data)

  md_1 <- prep_study2meta(study_data)
  md_1$JUMP_LIST <- "|"
  md_1$TIME_VAR <- NA
  md_1$TIME_VAR <- c(rep("START_TIME", 5), "", "")
  md_1$TIME_VAR_END <- c(rep("END_TIME", 5), "", "")

  cil <- data.frame(
    VARIABLE_LIST =
      c("scaleA1 | scaleA2 | scaleA3 |scaleA4 |scaleA5"),
    CHECK_LABEL = c("all_questionnaire"),
    MAXIMUM_LONG_STRING = "[;3)",
    TOTRESPT = "[0; 5]"
  )


  rep1 <- dq_report2(
    study_data = study_data,
    meta_data = md_1,
    meta_data_cross_item = cil,
    dimensions = "int", cores = NULL
  )

  result <- unclass(rep1$`con_ssi_range_check.Maximum Long String`)

  expect_equal(
    result$SummaryTable$NUM_ssc_mls,
    20
  )
  expect_equal(
    result$SummaryData$Variables,
    "Maximum Long String.all_questionnaire"
  )
  expect_equal(as.numeric(result$SummaryData$N), 200)
  expect_identical(
    attr(result$SummaryData$N, DATA_TYPE),
    DATA_TYPES$INTEGER
  )
  expect_equal(
    as.numeric(result$SummaryData$"Observational units removed"),
    0
  )
  expect_identical(
    attr(result$SummaryData$"Observational units removed", DATA_TYPE),
    DATA_TYPES$INTEGER
  )
  expect_match(
    attr(result$SummaryData, "description")[[
      "Observational units removed"
    ]],
    "computed metric value",
    fixed = TRUE
  )

  miss_cil <- data.frame(
    VARIABLE_LIST = "scaleA1 | scaleA2 | scaleA3 | scaleA4 | scaleA5",
    CHECK_LABEL = "all_questionnaire",
    MISS_RESP = "[;1)"
  )
  miss_report <- dq_report2(
    study_data = study_data,
    meta_data = md_1,
    meta_data_cross_item = miss_cil,
    dimensions = "int",
    cores = NULL
  )
  miss_result <- unclass(
    miss_report$`con_ssi_range_check.Missing responses`
  )
  expect_named(
    miss_result$SummaryTable,
    c("Variables", "NUM_scc_miss", "PCT_scc_miss", "FLG_scc_miss")
  )
  expect_equal(miss_result$SummaryTable$NUM_scc_miss, 0)
  jitter_layers <- Filter(function(layer) {
    inherits(layer$position, "PositionJitter")
  }, miss_result$SummaryPlotList[[1]]$layers)
  expect_length(jitter_layers, 1L)
  expect_identical(jitter_layers[[1]]$position$seed, 1L)

  total_response_time <- rep1[[
    "con_ssi_range_check.Total Response Time.all_questionnaire"
  ]]
  expect_s3_class(total_response_time, "master_result")
  expect_false(inherits(total_response_time, "dataquieR_NULL"))
  expect_identical(
    as.character(total_response_time$SummaryData$Variables),
    "Total Response Time.all_questionnaire"
  )

  cil$TOTRESPT <- "[20;30]"
  report_without_within_segment <- suppressWarnings(dq_report2(
    study_data = study_data,
    meta_data = md_1,
    meta_data_cross_item = cil,
    dimensions = "int",
    cores = NULL
  ))
  total_response_time <- report_without_within_segment[[
    "con_ssi_range_check.Total Response Time.all_questionnaire"
  ]]
  expect_s3_class(total_response_time, "master_result")
  expect_false(inherits(total_response_time, "dataquieR_NULL"))

})
