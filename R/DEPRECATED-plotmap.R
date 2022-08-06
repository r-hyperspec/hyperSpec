#' @name DEPRECATED-plotmap
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
#' - [hyperSpec::plot_map()]
#'
#'
#' @param ... arguments to [hyperSpec::plot_map()].
#'
#' @include raster.R
#' @export
plotmap <- function(...) {
  hySpc_deprecated("plot_map")
  plot_map(...)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(plotmap) <- function() {
  context("Deprecated functions")

  test_that("plotmap() is deprecated", {
    expect_error(expect_warning(plotmap(), "deprecated"))
  })
}
