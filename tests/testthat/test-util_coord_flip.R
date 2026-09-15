test_that("util_coord_flip respects explicit and automatic flip modes", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  call_coord_flip <- function(flip_mode = "noflip", w, h, p) {
    ref_env <- environment()
    if (missing(p)) {
      util_coord_flip(w = w, h = h, ref_env = ref_env)
    } else {
      util_coord_flip(w = w, h = h, p = p, ref_env = ref_env)
    }
  }

  expect_s3_class(call_coord_flip("noflip", 1, 2), "CoordCartesian")
  expect_s3_class(call_coord_flip("flip", 1, 2), "CoordFlip")
  expect_s3_class(call_coord_flip("auto", 3, 1), "CoordFlip")
  expect_s3_class(call_coord_flip("auto", 1, 3), "CoordCartesian")

  discrete_plot <- ggplot2::ggplot(
    data.frame(x = factor(c("a", "b")), y = factor(c("c", "d"))),
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point()

  expect_s3_class(call_coord_flip("auto", p = discrete_plot), "CoordCartesian")
})

test_that("util_coord_flip uses caller defaults and global options", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  old_flip_mode <- getOption("dataquieR.flip_mode")
  on.exit(options(dataquieR.flip_mode = old_flip_mode), add = TRUE)

  call_coord_flip <- function(flip_mode = "flip") {
    ref_env <- environment()
    util_coord_flip(w = 1, h = 2, ref_env = ref_env)
  }

  expect_s3_class(call_coord_flip("default"), "CoordFlip")

  options(dataquieR.flip_mode = "noflip")
  expect_s3_class(call_coord_flip(), "CoordCartesian")
})

test_that("util_coord_flip falls back when auto mode lacks dimensions", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  call_coord_flip <- function(flip_mode = "auto") {
    ref_env <- environment()
    util_coord_flip(ref_env = ref_env)
  }

  expect_s3_class(call_coord_flip("auto"), "CoordCartesian")
})

test_that("util_coord_flip requires a flip_mode caller", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  call_without_flip_mode <- function() {
    ref_env <- environment()
    util_coord_flip(w = 1, h = 2, ref_env = ref_env)
  }

  expect_error(
    call_without_flip_mode(),
    "can only be called from a function with the"
  )

  expect_error(
    util_coord_flip(w = 1, h = 2, ref_env = new.env(parent = emptyenv())),
    "ref_env outside the"
  )
})

test_that("util_lazy_add_coord follows lazy plot options", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  old_lazy <- getOption("dataquieR.lazy_plots")
  old_compat <- getOption("dataquieR.lazy_plots_gg_compatibility")
  on.exit(options(
    dataquieR.lazy_plots = old_lazy,
    dataquieR.lazy_plots_gg_compatibility = old_compat
  ), add = TRUE)

  base_plot <- ggplot2::ggplot(data.frame(x = 1:2, y = 1:2),
    ggplot2::aes(x, y))
  coord <- ggplot2::coord_flip()

  expect_s3_class(util_lazy_add_coord(base_plot, coord), "ggplot")

  options(
    dataquieR.lazy_plots = TRUE,
    dataquieR.lazy_plots_gg_compatibility = FALSE
  )
  lazy_plot <- util_create_lean_ggplot(
    {
      base_plot
    },
    base_plot = base_plot,
    .lazy = TRUE
  )
  lazy_with_coord <- util_lazy_add_coord(lazy_plot, coord)
  expect_s3_class(lazy_with_coord, "dq_lazy_ggplot")
  expect_s3_class(prep_realize_ggplot(lazy_with_coord)$coordinates,
    "CoordFlip")

  options(dataquieR.lazy_plots = "bad")
  expect_warning(
    fallback <- util_lazy_add_coord(lazy_plot, coord),
    "Cannot use option dataquieR.lazy_plots"
  )
  expect_s3_class(fallback, "dq_lazy_ggplot")
})
