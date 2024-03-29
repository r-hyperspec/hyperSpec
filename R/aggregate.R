# Function -------------------------------------------------------------------

.aggregate <- function(x,
                       by = stop("by is needed"),
                       FUN = stop("FUN is needed."),
                       ...,
                       out.rows = NULL,
                       append.rows = NULL,
                       by.isindex = FALSE) {
  validObject(x)

  if (!is.list(by) || !by.isindex) {
    by <- split(seq(x, index = TRUE), by, drop = TRUE)
  }

  ## main work here is to avoid calling stats::aggregate as there splitting and
  ## rearranging is involved. That is slow with the spectra.

  # try a guess how many rows the result will have
  if (is.null(out.rows)) {
    tmp <- .apply_workhorse(
      data = x@data[by[[1]], , drop = FALSE], MARGIN = 2,
      FUN = FUN, ...
    )

    out.rows <- nrow(tmp) * length(by)
  }

  data <- x@data[rep(1, out.rows), , drop = FALSE] # preallocate memory
  data <- cbind(data, .aggregate = NA)
  col.aggregate <- ncol(data)

  r <- 1 # keeping track of the actually filled rows

  for (i in seq(along = by)) {
    tmp <- .apply_workhorse(
      data = x@data[by[[i]], , drop = FALSE], MARGIN = 2,
      FUN = FUN, ...
    )

    prows <- nrow(tmp) - 1

    ## TODO: try out whether this really helps
    if (r + prows > out.rows) {
      if (is.null(append.rows)) {
        append.rows <- max(100, ceiling(1 - (i / length(by)) * out.rows))
      }
      out.rows <- max(append.rows + out.rows, r + prows)
      data <- rbind(data, data[rep(1, out.rows - nrow(data)), , drop = FALSE])
      warning(
        "At", i, "of", length(by),
        "levels: Output data.frame too small. Consider using an",
        "appropriate value for out.rows to speed up calculations."
      )
    }

    if (prows >= 0) {
      data[r:(r + prows), -col.aggregate] <- tmp
      data[r:(r + prows), col.aggregate] <- i

      r <- r + prows + 1
    }
  }

  x@data <- data[seq_len(r - 1), , drop = FALSE]
  x@data[, col.aggregate] <-
    factor(x@data[, col.aggregate], levels = seq_along(by))

  if (!is.null(names(by)) && !any(is.na(names(by)))) {
    levels(x@data[, col.aggregate]) <- names(by)
  }

  x
}


#' Aggregate `hyperSpec` objects
#'
#' Compute summary statistics for subsets of a `hyperSpec` object.
#'
#' [aggregate()] applies `FUN` to each of the subgroups given by `by`.
#' It combines the functionality of [stats::aggregate()], [base::tapply()],
#' and [stats::ave()] for `hyperSpec` objects.
#'
#' [aggregate()] avoids splitting `x@data`.
#'
#' `FUN` does not need to return exactly one value.  The number of
#' returned values needs to be the same for all wavelengths (otherwise the
#' result could not be a matrix), see the examples.
#'
#' If the initially pre-allocated `data.frame` turns out to be too small,
#' more rows are appended and a warning is issued.
#'
#' @name aggregate
#' @rdname aggregate
#' @aliases aggregate,hyperSpec-method
#'          ave,hyperSpec-method
#'
#' @docType methods
#'
#' @param x `hyperSpec` object.
#' @param by grouping for the rows of `x@@data`. \cr
#'        Either a list containing an index vector for each of the subgroups
#'        or a vector that can be `split` in such a list.
#' @param FUN function to compute the summary statistics
#' @param out.rows number of rows in the resulting `hyperSpec` object,
#'        for memory pre-allocation.
#' @param append.rows If more rows are needed, how many should be appended? \cr
#'        Defaults to 100 or an estimate based on the percentage of groups that
#'        are still to be done, whatever is larger.
#' @param by.isindex If a list is given in `by`: does the list already
#'        contain the row indices of the groups? If `FALSE`, the list in
#'        `by` is computed first (as in [stats::aggregate()]).
#' @param ... further arguments passed to `FUN`
#'
#'
#' @return A `hyperSpec` object with an additional column `@data$.aggregate`
#'         tracing which group the rows belong to.
#'
#'
#' @author C. Beleites
#' @seealso [base::tapply()], [stats::aggregate()],
#'
#'   [stats::ave()]
#'
#' @keywords methods category array
#' @concept manipulation
#' @concept stats
#'
#' @include hyperspec-class.R
#' @import stats
#' @export
#'
#' @examples
#' region.means <- aggregate(faux_cell, faux_cell$region, mean_pm_sd)
#' plot(region.means,
#'   stacked = ".aggregate", fill = ".aggregate",
#'   col = palette_matlab_dark(3)
#' )
#'
#' ## make some "spectra"
#' spc <- new(
#'   "hyperSpec",
#'   spc = sweep(matrix(rnorm(10 * 20), ncol = 20), 1, (1:10) * 5, "+")
#' )
#'
#' ## 3 groups
#' color <- c("red", "blue", "black")
#' by <- as.factor(c(1, 1, 1, 1, 1, 1, 5, 1, 2, 2))
#' by
#' plot(spc, "spc", col = color[by])
#'
#' ## Example 1: plot the mean of the groups
#' plot(aggregate(spc, by, mean), "spc",
#'   col = color, add = TRUE,
#'   lines.args = list(lwd = 3, lty = 2)
#' )
#'
#' ## Example 2: FUN may return more than one value (here: 3)
#' plot(aggregate(spc, by, mean_pm_sd), "spc",
#'   col = rep(color, each = 3), lines.args = list(lwd = 3, lty = 2)
#' )
#'
#' ## Example 3: aggregate even takes FUN that return different numbers of
#' ##            values for different groups
#' plot(spc, "spc", col = color[by])
#'
#' weird.function <- function(x) {
#'   if (length(x) == 1) {
#'     x + 1:10
#'   } else if (length(x) == 2) {
#'     NULL
#'   } else {
#'     x[1]
#'   }
#' }
#'
#' agg <- aggregate(spc, by, weird.function)
#' agg$.aggregate
#' plot(agg, "spc",
#'   add = TRUE, col = color[agg$.aggregate],
#'   lines.args = list(lwd = 3, lty = 2)
#' )
setMethod("aggregate", signature = signature(x = "hyperSpec"), .aggregate)


# Unit tests -----------------------------------------------------------------

#' @import hySpc.testthat
hySpc.testthat::test(.aggregate) <- function() {
  context("aggregate")
  test_that("faux_cell region means", {
    region.means <- aggregate(faux_cell, faux_cell$region, mean_pm_sd)
    expect_true(all(is.na(region.means$y)))
    expect_true(all(is.na(region.means$x)))

    expect_equal(region.means$region, region.means$.aggregate)

    for (region in levels(faux_cell$region)) {
      expect_equivalent(
        region.means[[region.means$region == region, ]],
        apply(faux_cell[[faux_cell$region == region, ]], 2, mean_pm_sd)
      )
    }
  })

  test_that("FUN returning different numbers of values for different groups", {
    spc <- new("hyperSpec", spc = sweep(
      matrix(rnorm(10 * 20), ncol = 20), 1, (1:10) * 5,
      "+"
    ))
    by <- as.factor(c(1, 1, 1, 1, 1, 1, 5, 1, 2, 2))

    weird.function <- function(x) {
      if (length(x) == 1) {
        x + 1:10
      } else if (length(x) == 2) {
        NULL
      } else {
        x[1]
      }
    }

    expect_warning(agg <- aggregate(spc, by, weird.function))
    agg <- aggregate(spc, by, weird.function, out.rows = 20L)

    expect_equal(
      agg$.aggregate,
      structure(c(1L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L),
        .Label = c("1", "2", "5"), class = "factor"
      )
    )
  })
}
