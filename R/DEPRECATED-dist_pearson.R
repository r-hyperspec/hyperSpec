#' @name DEPRECATED-pearson.dist
#' @concept Deprecated
#'
#' @title (DEPRECATED)
#'        Distance based on Pearson's \eqn{R^2}{R squared}
#'
#' @description
#'
#' This function is **deprecated**.
#' Use [dist_pearson()] instead.
#'
#' @param ... Passed to [dist_pearson()].
#'
#' @export

pearson.dist <- function(...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hySpc_deprecated("dist_pearson")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dist_pearson(...)
}


hySpc.testthat::test(pearson.dist) <- function() {
  context("pearson.dist")

  test_that(
    "deprecated",
    expect_warning(pearson.dist(flu), "deprecated")
  )
}
