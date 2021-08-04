
# Function -------------------------------------------------------------------

#' @name DEPRECATED-chk.hy
#' @concept Deprecated
#'
#' @title (DEPRECATED)
#'        Validate `hyperSpec` objects
#'
#' @description
#'
#' This function is **deprecated**.
#' Use [assert_hyperSpec()] or [is_hyperSpec()].
#'
#' @param ... Passed to [hy_get_option()], [hy_get_options()],
#'        or [hy_set_options()].
#'
#' @export

chk.hy <- function(object) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hySpc_deprecated("assert_hyperSpec")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  assert_hyperSpec(object)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(assert_hyperSpec) <- function() {
  context("assert_hyperSpec")

  test_that("deprecated",  expect_warning(chk.hy(flu), "deprecated"))
}
