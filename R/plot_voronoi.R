########################################################################## ~
###
###  plot_voronoi - plot spectral maps with irregular point pattern
###
###  plots intensity or extra data column over 2 extra data columns
###
########################################################################## ~


# Function -------------------------------------------------------------------

#' @rdname levelplot
#'
#' @param use.tripack (DEPRECATED) See [latticeExtra::panel.voronoi] for
#'        details.
#' @param mix (DEPRECATED) This argument is deprecated due to deprecation of
#'        argument `use.tripack`.
#'
#'
#' @seealso [latticeExtra::panel.voronoi()]
#'
#' @concept plotting
#' @concept plot generation
#'
#' @include levelplot.R
#'
#' @importFrom latticeExtra panel.voronoi
#' @importFrom lattice prepanel.default.levelplot
#' @importFrom utils modifyList
#'
#' @export
#'
plot_voronoi <- function(object, model = spc ~ x * y,
                         use.tripack = "DEPRECATED", mix = "DEPRECATED", ...) {
  if (!requireNamespace("latticeExtra")) {
    stop("package latticeExtra is needed for Voronoi plots.")
  }

  if (!missing(use.tripack)) {
    warning(
      "Argument 'use.tripack' is deprecated and ignored. ",
      "See ?latticeExtra::panel.voronoi ",
      "for more details."
    )
  }

  if (!missing(mix)) {
    warning(
      "Argument 'mix' is deprecated and ignored due to deprecation of ",
      "'use.tripack'. ",
      "On deprecation of 'use.tripack', see ?latticeExtra::panel.voronoi "
    )
  }

  dots <- modifyList(
    list(
      object = object,
      model = model,
      panel = panel.voronoi,
      prepanel = prepanel.default.levelplot,
      pch = 19, cex = .25,
      col.symbol = "#00000020",
      border = "#00000020"
    ),
    list(...)
  )
  do.call(plot_map, dots)
}

# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(plot_voronoi) <- function() {
  context("plot_voronoi")

  test_that("plot_voronoi() produces no errors or warnings", {
    # Just check that no errors occur
    expect_silent(plot_voronoi(faux_cell, region ~ y * x))

  })

  test_that("plot_voronoi() produces warnings", {
    # Test deprecated arguments
    expect_warning(
      plot_voronoi(faux_cell, region ~ y * x, use.tripack = FALSE),
      "Argument 'use.tripack' is deprecated and ignored."
    )
    expect_warning(
      plot_voronoi(faux_cell, region ~ y * x, use.tripack = TRUE),
      "Argument 'use.tripack' is deprecated and ignored."
    )
    expect_warning(
      plot_voronoi(faux_cell, region ~ y * x, mix = FALSE),
      "Argument 'mix' is deprecated and ignored."
    )
    expect_warning(
      plot_voronoi(faux_cell, region ~ y * x, mix = TRUE),
      "Argument 'mix' is deprecated and ignored."
    )
  })
}
