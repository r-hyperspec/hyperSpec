# Function -------------------------------------------------------------------

#' Identifying spectra and spectral data points
#'
#' Function [identify_spc()] allows identifing the spectrum and the wavelength
#' of a point in a plot produced by [plot_spc()].
#'
#' @rdname identify_spc
#'
#' @aliases identify_spc
#'          format_label_ispc_wl
#'          format_label_wl_only
#'          locate_spc_point_clicked
#'          locate_spc_point_max
#'          locate_spc_point_min
#'          locate_spc_point_parabola_max
#'
#' @details
#' Function [identify_spc()] first finds the spectrum with a point closest
#' to the clicked  position (see [locator()][graphics::locator()]).
#' The distance to the clicked  point is evaluated relative to the size
#' of the tolerance window.
#'
#' In a second step, `max.fn` searches for the actual point to label
#' within the specified wavelength window of that spectrum. This allows to
#' label maxima (or minima) without demanding too precise clicks. Currently,
#' the following functions to determine the precise point:
#'
#' - [locate_spc_point_clicked()]
#'   uses the clicked wavelength together with its spectral intensity;
#' - [locate_spc_point_max()]
#'   the point with the highest intensity in the wavelength window;
#' - [locate_spc_point_min()]
#'   the point with the lowest intensity in the wavelength window;
#' - [locate_spc_point_parabola_max()]
#'   maximum of a parabola fit through the point with highest intensity
#'   and the two surrounding points.
#'
#'
#' `point.fn` is called with the arguments
#' `wl` containing the considered wavelength window,
#' `spc` the  respective intensities of the closest spectrum, and
#' `wlclick` the  wavelength that was clicked.
#' They return a vector of two elements (wavelength and intensity).
#'
#' As a last step, a label for the point produced by `formatter` and plotted
#' using [text()][graphics::text()]. Currently, the following `formatter`s are
#' available:
#'  \tabular{ll}{
#'  [format_label_ispc_wl()] \tab spectrum number, wavelength \cr
#'  [format_label_wl_only()]  \tab wavelength \cr
#' }
#'
#' `formatter` functions receive the number of the spectrum `ispc`,
#' the wavelength `wl`, and the spectral intensity `spc` and produce
#' a character variable suitable for labeling. The predefined formatters
#' surround the label text by spaces in order to easily have an appropriate
#' offset from the point of the spectrum.
#'
#' The warning issued if no spectral point is inside the tolerance window may
#' be switched of by `warn = FALSE`. In that case, the click will produce
#' a row of `NA`s in the resulting data.frame.
#'
#' [identify_spc()] uses option `debuglevel` to determine whether debugging
#' output  should be produced.
#' - `debuglevel == 2` will plot the tolerance window for every clicked point,
#' - `debuglevel == 1` will plot the tolerance window only if no data point was
#' inside.
#'
#' See ['hyperSpec' options][hyperSpec::options] for details about retrieving
#' and setting options.
#'
#' You may want to adjust the plot's `ylim` to ensure that the labels are
#' not clipped. As a dirty shortcut, `xpd = NA` may help.
#'
#'
#' @param x either the abscissa coordinates or the list returned by [plot_spc()]
#'
#' @param y the ordinate values. Giving `y` will override any values from `x$y`.
#' @param wavelengths the wavelengths for the data points.
#'        Giving `wavelengths` will override any values from `x$wavelengths`.
#' @param tol.wl,tol.spc tolerance in wavelength and spectral intensity to
#'        search around the clicked point. See details.
#' @param point.fn `function(wl, spc, wlclick)` to determine the actual
#'        point to label, see details.
#' @param formatter `function(i, wl, spc)` that produces the labels.
#'        If `NULL`, no labels are displayed.
#' @param ... passed to [graphics::text()] in order to produce the
#'        labels
#' @param  cex,adj,srt see [graphics::par()]
#'
#' @param warn Should the user be warned if no point is in the considered
#'             window? In addition, see the discussion of option `debuglevel` in
#'             the details.
#'
#'             If `FALSE`, the resulting data.frame will have a row of `NA`s
#'             instead.
#'
#' @param delta `locate_spc_point_parabola_max` fits the parabola in the window wlclick
#'   \eqn{\pm}{+-} delta points.
#'
#' @return [identify_spc()] returnsa `data.frame` with columns:
#'  \item{ispc}{spectra indices of the identified points, i.e. the rows of the
#'               `hyperSpec` object that was  plotted.
#'
#'  If `ispc` is given, `ispc [i]` is returned rather than `i`.
#' }
#'  \item{wavelengths}{the wavelengths of the identified points.}
#'  \item{spc}{the intensities of the identified points.}
#'
#'
#' @author C. Beleites
#'
#' @seealso
#' - [graphics::locator()];
#' - [plot_spc()];
#' - ['hyperSpec' options()][hyperSpec::options];
#' - [map.identify()], [map.sel.poly()]
#'
#' @keywords iplot
#' @concept plotting
#' @concept plotting tools
#'
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#'
#' ispc <- sample(nrow(laser), 10)
#' ispc
#'
#' identified <- identify_spc(plot_spc(laser[ispc]))
#'
#' ## convert to the "real" spectra indices
#' ispc[identified$ispc]
#' identified$wl
#' identified$spc
#'
#' ## allow the labels to be plotted into the plot margin
#' identify_spc(plot_spc(laser[ispc]), ispc = ispc, xpd = NA)
#'
#' identify_spc(plot_spc(paracetamol,
#'   xoffset = 1100,
#'   wl.range = c(600 ~ 1700, 2900 ~ 3150)
#' ),
#' formatter = format_label_wl_only
#' )
#'
#' ## looking for minima
#' identify_spc(
#'   plot(-paracetamol, wl.reverse = TRUE),
#'   point.fn = locate_spc_point_min, adj = c(1, 0.5)
#' )
#'
#' }}
identify_spc <- function(x, y = NULL,
                         wavelengths = NULL,
                         ispc = NULL,
                         tol.wl = diff(range(x)) / 200,
                         tol.spc = diff(range(y)) / 50,
                         point.fn = locate_spc_point_max, # function to find the maximum
                         formatter = format_label_ispc_wl, # NULL: suppress labels
                         ...,
                         cex = 0.7,
                         adj = c(0, 0.5),
                         srt = 90, # for the label text
                         warn = TRUE) {
  if (!interactive()) {
    stop("identify_spc works only on interactive graphics devices.")
  }

  if (is.list(x)) {
    if (is.null(wavelengths)) {
      wavelengths <- x$wavelengths
    }
    if (is.null(y)) {
      y <- x$y
    }
    x <- x$x
  }

  debuglevel <- hy_get_option("debuglevel")

  if ((length(x) != length(y)) | (length(x) != length(wavelengths))) {
    stop("x, y, and wavelength need to have the same length.")
  }

  if (is.null(ispc)) {
    ispc <- row(y)
  } else {
    ispc <- ispc[row(y)]
  }

  pts <- data.frame(ispc = rep(NA, 50), wl = NA, spc = NA)
  pos <- 1

  while (!is.null(tmp <- locator(n = 1))) {
    # return wl_min / wl_max for outside pts.:
    wl <- approx(x, wavelengths, tmp$x, rule = 2)$y

    if (debuglevel == 2L) {
      points(tmp$x, tmp$y, pch = ".", col = "red")
      rect(tmp$x - tol.wl, tmp$y - tol.spc, tmp$x + tol.wl, tmp$y + tol.spc,
        border = "red", col = NA
      )
    }

    # window to search for the closest spectrum:
    i.window <- wavelengths >= wl - tol.wl &
      wavelengths <= wl + tol.wl &
      y >= tmp$y - tol.spc &
      y <= tmp$y + tol.spc

    if (!any(i.window)) {
      if (warn) {
        warning("No spectra in specified window.")
      } else {
        pos <- pos + 1
      }

      if (debuglevel == 1L) {
        points(tmp$x, tmp$y, pch = ".", col = "red")
        rect(tmp$x - tol.wl, tmp$y - tol.spc, tmp$x + tol.wl, tmp$y + tol.spc,
          border = "red", col = NA
        )
      }
    } else {

      ## find spectrum closest to clicked point.
      ## x and y distances are scaled according to tolerance.
      tmp <- ((wl - wavelengths[i.window]) / tol.wl)^2 +
        ((tmp$y - y[i.window]) / tol.spc)^2
      tmp <- which(i.window)[which.min(tmp)]

      pts[pos, "ispc"] <- ispc[tmp] # closest spectrum;
      # this will grow the data.frame if necessary
      # no time concern with hand-clicked points

      ## search for the max (min) of spectrum pt within tmp$x +- tol.wl
      i.window <- which(ispc == ispc[tmp] &
        wavelengths >= wl - tol.wl &
        wavelengths <= wl + tol.wl)

      pts[pos, 2:3] <- point.fn(
        wl = wavelengths[i.window],
        spc = y[i.window],
        wlclick = wl
      )

      ## label the point
      if (!is.null(formatter)) {
        lab <- formatter(pts[pos, 1], pts[pos, 2], pts[pos, 3])

        text(approx(wavelengths, x, pts[pos, 2], rule = 2),
          pts[pos, 3],
          labels = lab, cex = cex, adj = adj, srt = srt, ...
        )
      }

      pos <- pos + 1
    }
  }

  pts[seq_len(pos - 1), ]
}


# Function -------------------------------------------------------------------

#' @rdname identify_spc
#'
#' @param wl The wavelength to label.
#' @param spc The intensity to label.
#' @param wlclick The clicked wavelength.
#'
#' @export
locate_spc_point_max <- function(wl, spc, wlclick) {
  i <- which.max(spc)
  c(wl = wl[i], spc = spc[i])
}

#' @rdname identify_spc
#' @export
locate_spc_point_clicked <- function(wl, spc, wlclick) {
  i <- round(approx(wl, seq_along(wl), wlclick, rule = 2)$y)
  c(wl = wl[], spc = spc[i])
}

#' @rdname identify_spc
#' @export
locate_spc_point_min <- function(wl, spc, wlclick) {
  i <- which.min(spc)
  c(wl = wl[i], spc = spc[i])
}

#' @rdname identify_spc
#' @export
locate_spc_point_parabola_max <- function(wl, spc, wlclick, delta = 1L) {
  i <- which.max(spc)

  ## points (wl [i], spc [i])
  if (i > 1L && i < length(wl)) {
    i <- i + (-delta:delta)
    i <- i %in% seq_along(wl) # make sure the indices exist

    p <- outer(wl[i], 0:2, "^") # Vandermonde matrix
    p <- qr.solve(p, spc[i])

    i <- -p[2] / p[3] / 2

    ## lines (wl, outer (wl, 0 : 2, "^") %*% p, col = "red")
    c(wl = i, spc = sum(p * c(1, i, i^2)))
  } else {
    c(wl = wl[i], spc = spc[i])
  }
}


# Function -------------------------------------------------------------------

#' @rdname identify_spc
#'
#' @param ispc If a selection of spectra was plotted, their indices can be
#'        given in `ispc`. In this case `ispc[i]` is returned rather than `i`.
#' @param digits How many digits of the wavelength should be displayed?
#'
#' @export
format_label_ispc_wl <- function(ispc, wl, spc, digits = 3) {
  sprintf(" %i, %s ", ispc, format(wl, digits = digits))
}

#' @rdname identify_spc
#' @export
format_label_wl_only <- function(ispc, wl, spc, digits = 3) {
  sprintf(" %s ", format(wl, digits = digits))
}
