skip_on_cran()

test_that(
  "util_map_to_other_metrics rewrites computed-role metrics in pipeline",
  {
    sumtab <- data.frame(
      Variables = "miss response",
      NUM_con_rvv_inum = 3,
      PCT_con_rvv_inum = 50,
      OTHER = 99,
      check.names = FALSE
    )
    meta_data <- data.frame(
      LABEL = "miss response",
      COMPUTED_VARIABLE_ROLE = COMPUTED_VARIABLE_ROLES$MISS_RESP
    )
    add_attribs <- new.env(parent = emptyenv())

    unchanged <- util_map_to_other_metrics(
      sumtab,
      meta_data = meta_data,
      label_col = "LABEL",
      add_attribs = add_attribs
    )
    expect_identical(unchanged, sumtab)
    expect_equal(ls(add_attribs), character())

    mapped <- with_pipeline(util_map_to_other_metrics(
      sumtab,
      meta_data = meta_data,
      label_col = "LABEL",
      add_attribs = add_attribs
    ))

    expect_named(mapped, c("Variables", "NUM_scc_miss", "PCT_scc_miss"))
    expect_equal(mapped$NUM_scc_miss, 3)
    expect_equal(mapped$PCT_scc_miss, 50)
    expect_equal(
      add_attribs[[COMPUTED_VARIABLE_ROLE]],
      c("miss response" = "MISS_RESP")
    )
  }
)

test_that(
  "util_map_to_other_metrics leaves unmapped summary tables unchanged",
  {
    sumtab <- data.frame(
      Variables = c("a", "b"),
      NUM_con_rvv_inum = c(1, 2),
      check.names = FALSE
    )
    meta_data <- data.frame(
      LABEL = c("a", "b"),
      COMPUTED_VARIABLE_ROLE = c(COMPUTED_VARIABLE_ROLES$MISS_RESP, NA)
    )
    add_attribs <- new.env(parent = emptyenv())

    mapped <- with_pipeline(util_map_to_other_metrics(
      sumtab,
      meta_data = meta_data,
      label_col = "LABEL",
      add_attribs = add_attribs
    ))

    expect_identical(mapped, sumtab)
    expect_equal(ls(add_attribs), character())
  }
)
