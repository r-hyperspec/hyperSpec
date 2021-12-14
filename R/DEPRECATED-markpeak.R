#' @name DEPRECATED-markpeak
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
#' - [hyperSpec::mark_peak()]
#'
#'
#' @param ... arguments to [hyperSpec::mark_peak()].
#'
#' @include mark_peak.R
#' @export
markpeak <- function(...) {
  hySpc_deprecated("mark_peak")
  mark_peak(...)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(markpeak) <- function() {
  context("Deprecated functions")

  test_that("markpeak() is deprecated", {
    expect_error(expect_warning(markpeak(), "deprecated"))
  })
}
