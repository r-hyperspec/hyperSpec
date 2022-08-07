#' @name DEPRECATED-stacked.offsets
#' @concept deprecated
#'
#' @title (DEPRECATED)
#'        Calculate y-axis offsets
#'
#' @description
#' These \pkg{hyperSpec} functions are **deprecated** and not maintained any
#' more. You should not use these.
#' Currently they are present due to back-compatibility reasons and will be
#' removed in the next release of the package.
#' Please, use the suggested alternative functions instead:
#'
#' - [hyperSpec::calculate_offsets()]
#'
#'
#' @param ... arguments to [hyperSpec::calculate_offsets()].
#'
#' @export
stacked.offsets <- function(...) {
  hySpc_deprecated("calculate_offsets")
  calculate_offsets(...)
}

# Unit tests -----------------------------------------------------------------
hySpc.testthat::test(stacked.offsets) <- function() {
  context("Deprecated functions")

  test_that("stacked.offsets() is deprecated", {
    expect_warning(stacked.offsets(flu), "deprecated")
  })
}
