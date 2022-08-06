#' @name DEPRECATED-plotc
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
#' - [hyperSpec::plot_c()]
#'
#'
#' @param ... arguments to [hyperSpec::plot_c()].
#'
#' @include raster.R
#' @export
plotc <- function(...) {
  hySpc_deprecated("plot_c")
  plot_c(...)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(plotc) <- function() {
  context("Deprecated functions")

  test_that("plotc() is deprecated", {
    expect_error(expect_warning(plot_c(), "deprecated"))
  })
}
