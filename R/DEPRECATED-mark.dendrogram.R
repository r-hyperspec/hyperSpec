#' @name DEPRECATED-mark.dendrogram
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
#' - [hyperSpec::mark_groups_in_dendrogram()]
#'
#'
#' @param ... arguments to [hyperSpec::mark_groups_in_dendrogram()].
#'
#' @include mark_groups_in_dendrogram.R
#' @export
mark.dendrogram <- function(...) {
  hySpc_deprecated("mark_groups_in_dendrogram")
  mark_groups_in_dendrogram(...)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(mark.dendrogram) <- function() {
  context("Deprecated functions")

  test_that("mark.dendrogram() is deprecated", {
    expect_error(expect_warning(mark.dendrogram(), "deprecated"))
  })
}
