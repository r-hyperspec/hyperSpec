#' Plot spectra matrix
#'
#' Plots the spectra matrix.
#'
#' If package \pkg{plotrix} is available, a color legend is plotted to the right.
#' The right margin is set to at least 5 lines.
#' @param object hyperSpec object
#' @param y character giving the name of the extra data column to label the y axis.
#' @param ylab y axis label, defaults to `"row"` and the label of the extra data column used
#' for the y axis, respectively.
#' @param col see  [graphics::image()]
#' @param ... further parameters for [graphics::image()]
#' @param contour should [graphics::contour()] be called instead of
#' [graphics::image()]?
#'
#' @author Claudia Beleites
#' @seealso  [graphics::image()], [graphics::contour()], [hyperSpec::levelplot()]
#'
#' @concept plotting
#' @concept plot generation
#'
#' @export
#'
#' @examples
#' plot_matrix(laser, col = palette_alois(100))
#'
#' plot(laser, "mat")
#'
#' plot_matrix(laser)
#' plot_matrix(laser, contour = TRUE, add = TRUE)
#'
#' ## use different y axis labels
#'
#' plot_matrix(laser, "t")
#'
#' plot_matrix(laser, laser$t / 3600, ylab = "t / h")
#' @importFrom utils modifyList
plot_matrix <- function(object, y = ".row", ylab, col = palette_alois(20), ...,
                        contour = FALSE) {
  assert_hyperSpec(object)
  validObject(object)
  object <- wl_sort(object)

  if (is.character(y)) {
    if (missing(ylab)) {
      ylab <- switch(y,
        .row = "row",
        labels(object, y)
      )
    }

    y <- switch(y,
      .row = seq_len(nrow(object)),
      object@data[[y]]
    )
  }

  dots <- modifyList(
    list(
      x = wl(object),
      y = y,
      z = t(object[[]]),
      xlab = labels(object, ".wavelength"),
      ylab = ylab,
      col = col
    ),
    list(...)
  )


  if (contour) {
    do.call("contour", dots)
  } else {
    ## leave at least 4 lines right margin
    mar <- par()$mar
    if (mar[4] < 5) {
      par(mar = c(mar[1:3], 5))
    }

    do.call("image", dots)
    par(mar = mar)

    ## color legend
    if (requireNamespace("plotrix", quietly = TRUE)) {
      usr <- par()$usr

      dx <- diff(usr[1:2])

      plotrix::color.legend(usr[2] + 0.05 * dx,
        usr[3],
        usr[2] + 0.10 * dx,
        usr[4],
        pretty(range(object, na.rm = TRUE)),
        col,
        align = "rb", gradient = "y"
      )
    } else {
      warning("package 'plotrix' not available: omitting legend.")
    }
  }
}


hySpc.testthat::test(plot_matrix) <- function() {
  context("plot_matrix")

  test_that("non-increasing wavelength axis", {
    tmp <- flu
    tmp[[]] <- tmp[[, , max ~ min]]
    tmp@wavelength <- rev(tmp@wavelength)

    expect_silent(plot_matrix(tmp))
  })

  ## TODO: vdiffr
}
