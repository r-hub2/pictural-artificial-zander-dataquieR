test_that(
  "dq_lazy_unwrap leaves ordinary and unsupported S7-like objects unchanged",
  {
    skip_on_cran()

    ordinary <- list(payload = "plot")
    expect_identical(dq_lazy_unwrap(ordinary), ordinary)

    s7_like_without_slot <- structure(
      list(payload = "plot"),
      class = "S7_object"
    )
    expect_identical(dq_lazy_unwrap(s7_like_without_slot), s7_like_without_slot)
  }
)
