#################################################################################
###
###  plot_map - plot spectral maps
###
###  plots intensity or extra data column over 2 extra data columns

## TODO: check whether function should be applied or not



#' Plot a map and identify/select spectra in the map
#'
#' [lattice::levelplot()] functions for hyperSpec objects.
#' An image or map of a summary value of each spectrum is plotted.
#' Spectra may be identified by mouse click.
#'
#' The `model` can contain the special column name `.wavelength` to specify
#' the wavelength axis.
#'
#' `plot_map`, `map.identify`, and the `levelplot` methods internally use the same
#' gateway function to [lattice::levelplot()]. Thus `transform.factor` can be used
#' with all of them and the panel function defaults to [lattice::panel.levelplot.raster()]
#' for all three. Two special column names, `.rownames` and `.wavelength` may be used.
#'
#' `levelplot` plots the spectra matrix.
#'
#' `plot_voronoi` calls `plot_map` with different default settings, namely the
#' panel function defaults to [latticeExtra::panel.voronoi()].
#' [latticeExtra::panel.voronoi()] depends on either of the packages \pkg{interp} or \pkg{deldir}
#' being installed. For further information, please consult the help page of
#' [latticeExtra::panel.voronoi()].
#'
#' `map.identify()` calls `plot_map()` and `plot_voronoi()`, respectively and waits for
#' (left) mouse clicks on points. Other mouse clicks end the input.
#'
#' Unlike [lattice::panel.identify()], the indices returned by `map.identify` are in
#' the same order as the points were clicked. Also, multiple clicks on the same point are returned
#' as multiple entries with the same index.
#'
#' `map.identify` uses option `debuglevel` similar to [identify_spc()]:
#' `debuglevel == 1` will plot the tolerance window if no data point was inside (and
#' additionally labels the point) while `debuglevel == 2` will always plot the tolerance
#' window.
#'
#' The `map.sel.*` functions offer further interactive selection, see
#' [map.sel.poly()].
#'
#' @rdname levelplot
#' @aliases plot_map plot_voronoi levelplot,formula,hyperSpec-method
#'   levelplot,hyperSpec,missing-method map.identify
#' @param object,data the `hyperSpec` object
#' @param model,x formula specifying the columns of object that are to be
#'   displayed by [lattice::levelplot()]
#' @param func,func.args Before plotting, `plot_map` applies function
#'   `func` with the arguments given in the list `func.args` to each
#'   of the spectra. Thus a single summary value is displayed for each of the
#'   spectra.
#'
#' This can be suppressed manually by setting `func` to NULL. It is automatically suppressed if
#' `.wavelength` appears in the formula.
#' @param voronoi Should the plot for identifying spectra by mouse click be
#'   produced by `plot_map` (default) or `plot_voronoi`?
#' @param ... further arguments are passed down the call chain, and finally
#'   to [lattice::levelplot()]
#' @return `map.identify` returns a vector of row indices into
#'   `object` of the clicked points.
#'
#' The other functions return a lattice object.
#' @author C. Beleites
#' @seealso `vignette(plotting)`, `vignette(hyperSpec)`
#'
#' [plot()]
#' @export
#'
#' @keywords hplot
#' @concept plotting
#' @concept plot generation
#'
#' @examples
#' \dontrun{
#' vignette(plotting)
#' vignette(hyperSpec)
#' }
#'
#' levelplot(spc ~ y * x, faux_cell[, , 1003]) # properly rotated
#' plot_map(faux_cell[, , 1003])
#'
#' # plot spectra matrix
#' levelplot(spc ~ .wavelength * t, laser, contour = TRUE, col = "#00000080")
#' # see also plot_matrix
#'
#' plot_map(faux_cell, region ~ x * y)
#'
#' # Voronoi plots
#' smpl <- sample(faux_cell, 300)
#' plot_map(smpl, region ~ x * y)
#'
#' plot_voronoi(smpl, region ~ x * y)
#' @importFrom utils modifyList
plot_map <- function(object, model = spc ~ x * y,
                     func = mean, func.args = list(), ...) {
  assert_hyperSpec(object)
  validObject(object)

  if (!is.null(func) & !any(grepl("[.]wavelength", model))) {
    object <- do.call(apply, c(list(object, 1, func), func.args))
  }

  dots <- modifyList(
    list(aspect = "iso"),
    list(...)
  )

  dots <- c(list(x = model, data = object), dots)

  do.call(.levelplot, dots)
}
