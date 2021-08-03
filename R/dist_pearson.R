#' Distance based on Pearson's \eqn{R^2}{R squared}
#'
#' The calculated distance is
#' \eqn{D^2 = \frac{1 - COR(`x`')}{2}}{D^2 = (1 - COR(x')) / 2}
#'
#' The distance between the rows of `x` is calculated.
#' The possible values range
#' from 0 (perfectly correlated)
#' over 0.5 (uncorrelated)
#' to 1 (perfectly anti-correlated).
#'
#' @param x a matrix
#'
#' @return distance matrix (distance object)
#'
#' @author C. Beleites
#'
#' @seealso [stats::as.dist()]
#' @references S. Theodoridis and K. Koutroumbas: Pattern Recognition, 3rd ed., p. 495
#'
#' @export
#'
#' @keywords cluster
#' @concept stats
#'
#' @examples
#'
#' dist_pearson(flu[[]])
#' dist_pearson(flu)
dist_pearson <- function(x) {
  x <- as.matrix(x)

  ## center & scale *row*s
  ## (n - 1) factor cancels out between variance scaling and calculating correlation
  x <- x - rowMeans(x)
  x <- x / sqrt(rowSums(x^2))

  if (hy_get_option("gc")) gc()
  x <- tcrossprod(x)

  ## keep only lower triagonal
  if (hy_get_option("gc")) gc()
  x <- as.dist(x)

  if (hy_get_option("gc")) gc()
  0.5 - x / 2
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(dist_pearson) <- function() {
  context("dist_pearson")

  test_that("dist_pearson against manual calculation", {
    expect_equivalent(
      dist_pearson(flu),
      as.dist(0.5 - cor(t(as.matrix(flu))) / 2)
    )
  })
}

## benchmark
# function() {
#   m <- sample(faux_cell, 10000)[[]]
#   microbenchmark(
#     cor = as.dist(0.5 - cor(t(as.matrix(m))) / 2),
#     tcross = dist_pearson(m),
#     times = 10L
#   )
# }
