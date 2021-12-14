#' @name DEPRECATED-spc.identify
#' @concept deprecated
#'
#' @title (DEPRECATED)
#'        Identifying spectra and spectral data points
#'
#' @description
#' These \pkg{hyperSpec} functions are **deprecated** and not maintained any
#' more. You should not use these.
#' Currently they are present due to back-compatibility reasons and will be
#' removed in the next release of the package.
#' Please, use the suggested alternative functions instead:
#'
#' - [hyperSpec::identify_spc()]
#'
#'
#' @param ... arguments to [hyperSpec::identify_spc()].
#'
#' @include identify_spc.R
#' @export
spc.identify <- function(...) {
  hySpc_deprecated("identify_spc")
  identify_spc(...)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(spc.identify) <- function() {
  context("Deprecated functions")

  test_that("spc.identify() is deprecated", {
    expect_error(expect_warning(spc.identify(), "deprecated"))
  })
}
