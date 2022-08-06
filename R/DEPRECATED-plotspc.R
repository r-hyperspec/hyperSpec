#' @name DEPRECATED-plotspc
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
#' - [hyperSpec::plot_spc()]
#'
#'
#' @param ... arguments to [hyperSpec::plot_spc()].
#'
#' @include raster.R
#' @export
plotspc <- function(...) {
  hySpc_deprecated("plot_spc")
  plot_spc(...)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(plotspc) <- function() {
  context("Deprecated functions")

  test_that("plotspc() is deprecated", {
    plot_d <- function() plotspc(flu)
    expect_warning(vdiffr::expect_doppelganger("plotspc", plot_d), "deprecated")
  })
}
