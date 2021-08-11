
# Function -------------------------------------------------------------------

#' @name DEPRECATED-chk.hy
#' @concept deprecated
#'
#' @title (DEPRECATED)
#'        Validate `hyperSpec` objects
#'
#' @description
#'
#' This function is **deprecated**.
#' Use [assert_hyperSpec()] or [is_hyperSpec()].
#'
#' @param object See argument `object` in [assert_hyperSpec()].
#'
#' @export

chk.hy <- function(object) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hySpc_deprecated("assert_hyperSpec")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  assert_hyperSpec(object)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(chk.hy) <- function() {
  context("chk.hy")

  test_that("deprecated",  expect_warning(chk.hy(flu), "deprecated"))
}
