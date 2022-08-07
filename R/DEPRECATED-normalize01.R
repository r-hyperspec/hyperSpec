#' @name DEPRECATED-normalize01
#' @concept deprecated
#'
#' @title (DEPRECATED)
#'        Normalize numbers to interval \[0, 1\]
#'
#' @description
#' These \pkg{hyperSpec} functions are **deprecated** and not maintained any
#' more. You should not use these.
#' Currently they are present due to back-compatibility reasons and will be
#' removed in the next release of the package.
#' Please, use the suggested alternative functions instead:
#'
#' - [hyperSpec::normalize_01()]
#'
#'
#' @param ... arguments to [hyperSpec::normalize_01()].
#'
#' @export
# Set generic ----------------------------------------------------------------
setGeneric("normalize01", function(x, ...) {
  hySpc_deprecated("normalize_01", old = "normalize01")
  standardGeneric("normalize01")
})

# Function -------------------------------------------------------------------
.normalize01_mat <- function(x, tolerance = hy_get_option("tolerance")) {
  .normalize_01_mat(x = x, tolerance = tolerance)
}
#' @rdname DEPRECATED-normalize01
#' @export
setMethod(normalize01, signature(x = "matrix"), .normalize01_mat)

# Function -------------------------------------------------------------------
.normalize01_num <- function(x, tolerance = hy_get_option("tolerance")) {
  .normalize_01_num(x = x, tolerance = tolerance)
}
#' @rdname DEPRECATED-normalize01
#' @export
setMethod("normalize01", signature(x = "numeric"), .normalize01_num)

# Function -------------------------------------------------------------------
.normalize01_hy <- function(x, ...) {
  .normalize_01_hy(x = x, ...)
}
#' @rdname DEPRECATED-normalize01
#' @export
setMethod(normalize01, signature(x = "hyperSpec"), .normalize01_hy)

# Unit tests -----------------------------------------------------------------
hySpc.testthat::test(normalize01) <- function() {
  context("Deprecated functions")

  test_that("normalize01() is deprecated", {
    # Vector
    expect_warning(
      expect_equal(normalize01(1:20), normalize_01(1:20)),
      "deprecated"
    )
    # Matrix
    mat <- matrix(runif(12), 3)
    expect_warning(
      expect_equal(normalize01(mat), normalize_01(mat)),
      "deprecated"
    )
    # hyperSpec
    expect_warning(
      expect_equal(normalize01(flu), normalize_01(flu)),
      "deprecated"
    )
  })
}
