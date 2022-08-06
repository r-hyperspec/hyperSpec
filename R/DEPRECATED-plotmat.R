#' @name DEPRECATED-plotmat
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
#' - [hyperSpec::plot_matrix()]
#'
#'
#' @param ... arguments to [hyperSpec::plot_matrix()].
#'
#' @include raster.R
#' @export
plotmat <- function(...) {
  hySpc_deprecated("plot_matrix")
  plot_matrix(...)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(plotmat) <- function() {
  context("Deprecated functions")

  test_that("plotmat() is deprecated", {
    plot_d <- function() plotmat(faux_cell)
    expect_warning(vdiffr::expect_doppelganger("plotmat", plot_d), "deprecated")
  })
}
