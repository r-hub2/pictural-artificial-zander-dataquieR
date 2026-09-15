skip_on_cran()

test_that(
  "util_generate_anchor_tag builds stable variable and indicator anchors",
  {
    expect_identical(
      as.character(util_generate_anchor_tag("A B", "acc_margins", "variable")),
      "<a id=\"AB.acc_margins\"></a>"
    )
    expect_identical(
      as.character(util_generate_anchor_tag("A B", "acc_margins", "indicator")),
      "<a id=\"acc_margins.AB\"></a>"
    )
    expect_identical(
      as.character(util_generate_anchor_tag(
        name = "acc_margins.[ALL]",
        order_context = "indicator"
      )),
      "<a id=\"acc_margins\"></a>"
    )
    expect_identical(
      as.character(util_generate_anchor_tag(
        name = "only_variable",
        order_context = "variable"
      )),
      "<a id=\"only_variable\"></a>"
    )
    expect_identical(
      as.character(util_generate_anchor_tag(
        name = "only_indicator",
        order_context = "indicator"
      )),
      "<a id=\"onlyindicator\"></a>"
    )
    expect_identical(
      as.character(util_generate_anchor_tag(
        name = "[ALL].only_variable",
        order_context = "variable"
      )),
      "<a id=\"onlyvariable\"></a>"
    )
  }
)

test_that(
  "util_generate_anchor_link builds stable variable and indicator links",
  {
    expect_identical(
      as.character(util_generate_anchor_link("A B", "acc_margins", "variable")),
      "<a href=\"VAR_AB.html#AB.acc_margins\">Distribution across</a>"
    )
    expect_identical(
      as.character(util_generate_anchor_link("A B",
          "acc_margins",
          "indicator")),
      "<a href=\"dim_acc_acc_margins.html#acc_margins.AB\">A B</a>"
    )
    expect_identical(
      as.character(util_generate_anchor_link("A B",
          "con_limit_deviations",
          "indicator",
          title = "Limits")),
      "<a href=\"dim_con.html#con_limit_deviations.AB\">Limits</a>"
    )
    expect_identical(
      as.character(util_generate_anchor_link(
        name = "acc_margins.[ALL]",
        order_context = "indicator"
      )),
      "<a href=\"dim_acc_acc_margins.html#acc_margins\">acc_margins</a>"
    )
    expect_identical(
      as.character(util_generate_anchor_link(
        name = "[ALL].v1",
        order_context = "variable",
        title = "All checks"
      )),
      "<a href=\"VAR_v1.html#v1\">All checks</a>"
    )
  }
)

test_that("util_generate_anchor_link rejects incomplete name-only links", {
  expect_error(
    util_generate_anchor_link(name = "acc_margins",
      order_context = "variable"),
    "need to know the variable"
  )
  expect_error(
    util_generate_anchor_link(name = "v1",
      order_context = "indicator"),
    "need to know the indicator"
  )
})
