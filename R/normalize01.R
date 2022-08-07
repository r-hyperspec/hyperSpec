# @title normalization for mixed colors

# Set generic ----------------------------------------------------------------

#' Normalize numbers to interval \[0, 1\]
#'
#' The input `x` is mapped to \[0, 1\] by subtracting the minimum and
#' subsequently dividing by the maximum. If all elements of `x` are equal,
#'  1 is returned.
#'
#' @name normalize_01
#'
#' @param x  Object (e.g., vector) with values to transform.
#' @param tolerance Tolerance level for determining what is 0 and 1.
#' @param ... additional parameters such as `tolerance` handed down.
#'
#' @return object (e.g., vector) with `x` values mapped to the interval \[0, 1\].
#'
#' @author C. Beleites
#'
#' @concept manipulation
#' @seealso [hyperSpec::wl_eval()], [hyperSpec::vanderMonde()]
#'
#' @export
#'
setGeneric("normalize_01", function(x, ...) standardGeneric("normalize_01"))


# Function -------------------------------------------------------------------

.normalize_01_mat <- function(x, tolerance = hy_get_option("tolerance")) {
  m <- apply(x, 1, min)
  x <- sweep(x, 1, m, `-`)
  m <- apply(x, 1, max)
  x <- sweep(x, 1, m, `/`)
  x[m < tolerance, ] <- 1
  x
}

#' @rdname normalize_01
#' @export
#'
setMethod(normalize_01, signature(x = "matrix"), .normalize_01_mat)


# Function -------------------------------------------------------------------

.normalize_01_num <- function(x, tolerance = hy_get_option("tolerance")) {
  x <- x - min(x)

  m <- max(x)
  if (m < tolerance) {
    rep(1, length(x))
  } else {
    x / m
  }
}

#' @rdname normalize_01
#' @export
#'
setMethod("normalize_01", signature(x = "numeric"), .normalize_01_num)


# Function -------------------------------------------------------------------

.normalize_01_hy <- function(x, ...) {
  validObject(x)
  x@data$spc <- normalize_01(unclass(x@data$spc), ...)
  x
}

#' @rdname normalize_01
#' @export
#'
setMethod(normalize_01, signature(x = "hyperSpec"), .normalize_01_hy)


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(normalize_01) <- function() {
  context("normalize_01")

  test_that("random numbers", {
    x <- runif(10, min = -1e3, max = 1e3)
    tmp.x <- normalize_01(x)

    expect_equivalent(min(normalize_01(x)), 0)
    expect_equivalent(max(normalize_01(x)), 1)

    expect_equivalent(normalize_01(x), (x - min(x)) / diff(range(x)))
  })

  test_that("0, 1, constant", {
    expect_equivalent(normalize_01(1), 1)
    expect_equivalent(normalize_01(0), 1)
    expect_equivalent(normalize_01(5), 1)
    expect_equivalent(normalize_01(rep(5, 3L)), rep(1, 3L))
  })


  test_that("matrix method", {
    m <- matrix(runif(12), 3)
    m[3, ] <- 7

    tmp.m <- normalize_01(m)

    expect_equal(apply(tmp.m, 1, max), c(1, 1, 1))
    expect_equal(apply(tmp.m, 1, min), c(0, 0, 1))
  })

  test_that("hyperSpec method", {
    tmp.hy <- normalize_01(-vanderMonde(flu, 1))

    expect_equal(apply(tmp.hy[[]], 1, min), 1:0)
    expect_equal(apply(tmp.hy[[]], 1, max), c(1, 1))
  })
}
