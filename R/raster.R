#' @title Find an evenly spaced grid for x
#' @description
#' `raster_make()` fits the data to the specified raster.
#'
#' `raster_fit()` tries different raster parameter and returns the raster that
#' covers most of the `x` values: The differences between the values of `x` are
#' calculated (possible step sizes). For each of those step sizes, different
#' points are tried (until all points have been  covered by a raster) and the
#' parameter combination leading to the best coverage (i.e. most points on the
#' grid) is not used.
#'
#' Note that only differences between the sorted values of x are considered as
#' step size.
#'
#' @param x Numeric to be fitted with a raster.
#' @param startx Starting point ("origin") for calculation of the raster.
#' @param d Step size of the raster.
#' @param tol Tolerance for rounding to new levels: elements of x within `tol`
#'        of the distance between the levels of the new grid are rounded to the
#'        new grid point.
#' @param newlevels Levels of the raster.
#'
#' @return List with elements:
#' \item{x}{the values of `x`, possibly rounded to the raster values}
#' \item{levels}{the values of the raster}
#'
#' @importFrom utils tail
#' @export
#'
#' @concept manipulation
#'
#' @author Claudia Beleites
#'
#' @examples
#' x <- c(sample(1:20, 10), (0:5) + 0.5)
#' raster <- raster_make(x, x[1], 2)
#' raster
#' plot(x)
#' abline(h = raster$levels, col = "#00000040")
#'
#' ## unoccupied levels
#' missing <- setdiff(raster$levels, raster$x)
#' abline(h = missing, col = "red")
#'
#' ## points actually on the raster
#' onraster <- raster$x %in% raster$levels
#' points(which(onraster), raster$x[onraster], col = "blue", pch = 20)
raster_make <- function(x, startx, d, newlevels, tol = 0.1) {
  if (missing(newlevels)) {
    # make sure to cover the whole data range + 1 point
    newlevels <- c(
      rev(seq(startx, min(x, na.rm = TRUE) - d, by = -d)[-1]),
      seq(startx, max(x, na.rm = TRUE) + d, by = d)
    )
  }

  inew <- approx(newlevels, seq_along(newlevels), x)$y

  # rounding
  rinew <- round(inew)
  wholenum <- abs(inew - rinew) < tol

  xnew <- x
  xnew[wholenum] <- newlevels[rinew[wholenum]]


  list(
    x = xnew,

    ## usually: drop outside levels 1 and length (newlevels)
    levels = newlevels[min(rinew[wholenum]):max(rinew[wholenum])]
  )
}

#' @rdname raster_make
#' @export
#'
#' @concept manipulation
#'
#' @examples
#'
#' raster <- raster_fit(x)
#' raster
#' plot(x)
#' abline(h = raster$levels, col = "#00000040")
#'
#' ## unoccupied levels
#' missing <- setdiff(raster$levels, raster$x)
#' abline(h = missing, col = "red")
#'
#' ## points actually on the raster
#' onraster <- raster$x %in% raster$levels
#' points(which(onraster), raster$x[onraster], col = "blue", pch = 20)
#'
#' x <- c(sample(1:20, 10), (0:5) + 0.45)
#' raster <- raster_fit(x)
#' raster
#' plot(x)
#' abline(h = raster$levels, col = "#00000040")
#'
#' ## unoccupied levels
#' missing <- setdiff(raster$levels, raster$x)
#' abline(h = missing, col = "red")
#'
#' ## points actually on the raster
#' onraster <- raster$x %in% raster$levels
#' points(which(onraster), raster$x[onraster], col = "blue", pch = 20)
raster_fit <- function(x, tol = 0.1) {
  levels <- sort(unique(x))

  if (length(levels) == 1L) {
    return(list(x = x, levels = levels))
  }

  dx <- sort(unique(diff(levels)))

  # reduce by rounding?
  dx <- c(dx[!diff(dx) < tol], tail(dx, 1))

  dx <- rev(dx)

  max.covered <- 0

  for (d in dx) {
    totry <- order(x)
    while (length(totry) > 0L) {
      # cat ("totry: ", totry, "\n")
      startx <- x[totry[1]]
      # cat ("startx: ", startx, "\n")

      # cat ("fit: ", c (startx, d), "\n")
      raster <- raster_make(x, startx, d, tol = tol)
      tmp <- sum(raster$x %in% raster$levels, na.rm = TRUE)
      # cat ("     ", tmp, "\n")
      if (tmp > max.covered) {
        max.covered <- tmp
        fit <- raster
      }

      if (max.covered == length(x)) {
        break
      }

      totry <- totry[!raster$x[totry] %in% raster$levels]
    }
  }

  fit
}
