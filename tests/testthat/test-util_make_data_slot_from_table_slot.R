test_that(
  "util_make_data_slot_from_table_slot selects and translates columns",
  {
    skip_on_cran()

    table <- data.frame(
      Variables = "x",
      DF_NAME = "df1",
      PCT_com_qum_nonresp = 12.345,
      NUM_com_qum_nonresp = 2,
      OTHER = "ignored",
      check.names = FALSE
    )
    class(table) <- c("TableSlot", class(table))

    data <- util_make_data_slot_from_table_slot(table)

    expect_s3_class(data, "DataSlot")
    expect_false(inherits(data, "TableSlot"))
    expect_equal(
      colnames(data),
      c(
        "Variables", "Dataframe",
        "Non-response rate (Percentage (0 to 100))",
        "Non-response rate (Number)"
      )
    )
    expect_equal(as.character(data[[3]]), "12.35%")
    expect_equal(
      util_attr(data[[3]], DATA_TYPE, exact = TRUE),
      DATA_TYPES$FLOAT
    )
    expect_equal(data[[4]], 2, ignore_attr = TRUE)
    expect_equal(
      util_attr(data[[4]], DATA_TYPE, exact = TRUE),
      DATA_TYPES$INTEGER
    )
  }
)

test_that(
  "util_make_data_slot_from_table_slot keeps flag metric type untouched",
  {
    skip_on_cran()

    table <- data.frame(
      Variables = "x",
      FLG_com_qum_nonresp = TRUE,
      check.names = FALSE
    )

    data <- util_make_data_slot_from_table_slot(table)

    expect_s3_class(data, "DataSlot")
    expect_equal(as.character(data[[2]]), "TRUE")
    expect_null(util_attr(data[[2]], DATA_TYPE, exact = TRUE))
  }
)

test_that(
  "util_make_data_slot_from_table_slot handles empty percentage columns",
  {
    skip_on_cran()

    table <- data.frame(
      Variables = "x",
      PCT_com_qum_nonresp = NA_real_,
      check.names = FALSE
    )

    data <- util_make_data_slot_from_table_slot(table)

    expect_s3_class(data, "DataSlot")
    expect_equal(as.character(data[[2]]), NA_character_)
    expect_equal(
      util_attr(data[[2]], DATA_TYPE, exact = TRUE),
      DATA_TYPES$FLOAT
    )
  }
)

test_that(
  paste0(
    "util_make_data_slot_from_table_slot returns empty ",
    "DataSlot if nothing matches"
  ),
  {
    skip_on_cran()

    table <- data.frame(OTHER = "ignored", stringsAsFactors = FALSE)

    data <- util_make_data_slot_from_table_slot(table)

    expect_s3_class(data, "DataSlot")
    expect_equal(ncol(data), 0L)
    expect_equal(nrow(data), 1L)
  }
)
