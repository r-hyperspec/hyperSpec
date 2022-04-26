#' @name DEPRECATED-raster
#' @concept deprecated
#'
#' @title (DEPRECATED)
#'        Find an evenly spaced grid for x
#'
#' @description
#' These \pkg{hyperSpec} functions are **deprecated** and not maintained any
#' more. You should not use these.
#' Currently they are present due to back-compatibility reasons and will be
#' removed in the next release of the package.
#' Please, use the suggested alternative functions instead:
#'
#' - [hyperSpec::raster_make()]
#' - [hyperSpec::raster_fit()]
#'
#'
#' @param ... arguments to [hyperSpec::raster_make()] and
#'            [hyperSpec::raster_fit()].
#'
#' @include raster.R
#' @export
makeraster <- function(...) {
  hySpc_deprecated("raster_make")
  raster_make(...)
}

#' @rdname DEPRECATED-raster
#' @export
fitraster <- function(...) {
  hySpc_deprecated("raster_fit")
  raster_fit(...)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(makeraster) <- function() {
  context("Deprecated functions")

  test_that("makeraster() and fitraster() are deprecated", {
    expect_error(expect_warning(makeraster(), "deprecated"))
    expect_error(expect_warning(fitraster(), "deprecated"))

    x <- c(sample(1:20, 10), (0:5) + 0.5)
    expect_warning(makeraster(x, x[1], 2), "deprecated")
    expect_warning(fitraster(x), "deprecated")
  })
}
