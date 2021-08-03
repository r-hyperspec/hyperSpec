
# Function -------------------------------------------------------------------

#' @name DEPRECATED-options
#' @concept Deprecated
#'
#' @title (DEPRECATED)
#'        Options for package \pkg{hyperSpec}
#'
#' @description
#'
#' These functions are **deprecated**.
#' Use [hy_get_option()], [hy_get_options()], or [hy_set_options()] instead.
#'
#' @param ... Passed to [hy_get_option()], [hy_get_options()],
#'        or [hy_set_options()].
#'
#' @export

hy.getOptions <- function(...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hySpc_deprecated("hy_get_options")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hy_get_options(...)
}


#' @rdname DEPRECATED-options
#' @export
hy.getOption <- function(...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hySpc_deprecated("hy_get_option")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hy_get_option(...)
}


#' @rdname DEPRECATED-options
#' @export
hy.setOptions <- function(...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hySpc_deprecated("hy_set_options")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hy_set_options(...)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(hy.setOptions) <- function() {
  context("hy.setOptions")

  test_that("deprecated",  expect_warning(hy.getOption(), "deprecated"))
  test_that("deprecated",  expect_warning(hy.getOptions(), "deprecated"))
  test_that("deprecated",  expect_warning(hy.setOptions(), "deprecated"))

}
