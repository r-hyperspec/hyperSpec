#' @name DEPRECATED-pooled.cov
#' @concept deprecated
#'
#' @title (DEPRECATED)
#'        Pooled covariance matrix
#'
#' @description
#' These \pkg{hyperSpec} functions are **deprecated** and not maintained any
#' more. You should not use these.
#' Currently they are present due to back-compatibility reasons and will be
#' removed in the next release of the package.
#' Please, use the suggested alternative functions instead:
#'
#' - [hyperSpec::cov_pooled()]
#'
#'
#' @param ... arguments to [hyperSpec::cov_pooled()]
#'
#' @include cov_pooled.R
#' @export
pooled.cov <- function(...) {
  hySpc_deprecated("cov_pooled")
  cov_pooled(...)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(pooled.cov) <- function() {
  context("Deprecated functions")

  test_that("pooled.cov is deprecated", {
    # Test that is deprecated
    expect_error(expect_warning(pooled.cov(), "deprecated"))

    # Test that works
    expect_warning(rez <- pooled.cov(faux_cell, faux_cell$region))
    expect_equal(rez, cov_pooled(faux_cell, faux_cell$region))
  })
}
