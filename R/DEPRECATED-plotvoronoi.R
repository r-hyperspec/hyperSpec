#' @name DEPRECATED-plotvoronoi
#' @concept deprecated
#'
#' @title (DEPRECATED)
#'        Plotting spectra
#'
#' @description
#' These \pkg{hyperSpec} functions are **deprecated** and not maintained any
#' more. You should not use these.
#' Currently they are present due to back-compatibility reasons and will be
#' removed in the next release of the package.
#' Please, use the suggested alternative functions instead:
#'
#' - [hyperSpec::plot_voronoi()]
#'
#'
#' @param ... arguments to [hyperSpec::plot_voronoi()].
#'
#' @include raster.R
#' @export
plotvoronoi <- function(...) {
  hySpc_deprecated("plot_voronoi")
  plot_voronoi(...)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(plotvoronoi) <- function() {
  context("Deprecated functions")

  test_that("plotvoronoi() is deprecated", {
    plot_d <- function() plotvoronoi(faux_cell[, , 1003])
    expect_warning(vdiffr::expect_doppelganger("plotvoronoi", plot_d), "deprecated")
  })
}
