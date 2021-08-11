
# Function -------------------------------------------------------------------

#' @name DEPRECATED-isample
#' @concept deprecated
#'
#' @title (DEPRECATED)
#'        Return vector of row indices
#'
#' @description
#'
#' This function is **deprecated**.
#' Use [sample(..., index = TRUE)][sample()] instead.
#'
#' @param ... Arguments to [hyperSpec::sample()].
#'
#' @export

isample <- function(...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hySpc_deprecated("sample(..., index = TRUE)")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sample(..., index = TRUE)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(isample) <- function() {
  context("isample")

  test_that("deprecated",  expect_warning(isample(flu), "deprecated"))
}
