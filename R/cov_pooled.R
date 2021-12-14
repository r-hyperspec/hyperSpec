# Function -------------------------------------------------------------------

.cov_h_ <- function(x, y, use, method) {
  validObject(x)

  cov(x@data$spc, use = use, method = method)
}

#' Covariance matrices for `hyperSpec` objects
#'
#' @rdname cov
#'
#' @param x `hyperSpec` object.
#' @param y Not supported.
#' @param use,method handed to [stats::cov()]
#'
#' @return Covariance `matrix` of size `nwl(x)` x `nwl(x)`.
#'
#' @concept stats
#'
#' @author C. Beleites
#'
#' @export
#'
#' @seealso [stats::cov()]
#'
#' @examples
#' image(cov(faux_cell))
setMethod("cov", signature = signature(x = "hyperSpec", y = "missing"), .cov_h_)


# Function -------------------------------------------------------------------

#' @rdname cov
#'
#' @param ... Ignored.
#' @param regularize (numeric):
#'        Regularization of the covariance matrix.
#'        Set `0` to switch off.
#'        Default is `1e-5 * max(abs(cov_p))`, where `cov_p` is a pooled
#'        covariance matrix before regularization.
#'
#' [cov_pooled()] calculates pooled covariance like, e.g., in LDA.
#' @param groups Factor indicating the groups.
#'
#' @export
#'
#' @examples
#' pcov <- cov_pooled(faux_cell, faux_cell$region)
#' plot(pcov$means)
#' image(pcov$COV)
cov_pooled <- function(x, groups, ..., regularize = NULL) {
  assert_hyperSpec(x)
  validObject(x)

  if (!is.factor(groups)) {
    stop("groups must be a factor")
  }

  x <- x[!is.na(groups)]
  groups <- groups[!is.na(groups)]

  means <- aggregate(x, groups, "mean") # TODO: speed up?

  cov_p <- cov(x@data$spc - means@data$spc[as.numeric(groups), , drop = FALSE])

  # regularization
  if (is.null(regularize)) regularize <- 1e-5 * max(abs(cov_p))
  cov_p <- cov_p + diag(regularize, nrow(cov_p))

  # Return:
  list(COV = cov_p, means = means)
}

# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.cov_h_) <- function() {
  context("Covariance")

  n_wl <- nwl(faux_cell)
  obj <- cov(faux_cell)

  # Properties
  test_that("properties of cov()", {
    expect_true(is.matrix(obj))
    expect_type(obj, "double")
    expect_equal(dim(obj), c(n_wl, n_wl))
  })

  # Contents
  test_that("contents of cov()", {
    expect_equivalent(obj[1, 1],   cov(faux_cell[[, ,  1, wl.index = TRUE]]))
    expect_equivalent(obj[9, 9],   cov(faux_cell[[, ,  9, wl.index = TRUE]]))
    expect_equivalent(obj[20, 20], cov(faux_cell[[, , 20, wl.index = TRUE]]))
    expect_equivalent(obj[50, 50], cov(faux_cell[[, , 50, wl.index = TRUE]]))
  })
}


hySpc.testthat::test(cov_pooled) <- function() {
  context("Pooled covariance")

  obj <- cov_pooled(faux_cell, faux_cell$region)

  n_wl <- nwl(faux_cell)
  n_means <- length(unique(faux_cell$region))

  test_that("cov_pooled() object", {

    expect_true(is.list(obj))
    expect_equal(names(obj), c("COV", "means"))
  })

  # $means
  test_that("cov_pooled()$means", {
    expect_s4_class(obj$means, "hyperSpec")
    expect_equal(nrow(obj$means), n_means)
    expect_equal(nwl(obj$means), n_wl)

    expect_equivalent(
      tapply(faux_cell$spc[, 1], faux_cell$region, mean),
      obj$means$spc[, 1]
    )
    expect_equivalent(
      tapply(faux_cell$spc[, 9], faux_cell$region, mean),
      obj$means$spc[, 9]
    )
    expect_equivalent(
      tapply(faux_cell$spc[, 20], faux_cell$region, mean),
      obj$means$spc[, 20]
    )
    expect_equivalent(
      tapply(faux_cell$spc[, 50], faux_cell$region, mean),
      obj$means$spc[, 50]
    )
  })

  # $COV
  test_that("cov_pooled()$COV", {
    expect_true(is.matrix(obj$COV))
    expect_equal(dim(obj$COV), c(n_wl, n_wl))

    # FIXME: the contents (values) of covariance matrix must be tested too
    #
  })
}
