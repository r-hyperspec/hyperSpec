
# Function -------------------------------------------------------------------

.options <- list(
  debuglevel = 0L,
  gc = FALSE,
  file.remove.emptyspc = TRUE,
  file.keep.name = TRUE,
  tolerance = sqrt(.Machine$double.eps),
  wl.tolerance = sqrt(.Machine$double.eps),
  plot.spc.nmax = 25,
  ggplot.spc.nmax = 10
)

#' Options for package \pkg{hyperSpec}
#'
#' Functions to access and set \pkg{hyperSpec}'s options.
#'
#' Currently, the following options are defined:
#' \tabular{llll}{
#' **Name**          \tab **Default Value (range)**      \tab **Description**                               \tab **Used by**\cr
#' debuglevel           \tab 0 (1L 2L 3L)                      \tab amount of debugging information produced         \tab [identify_spc()] [map.identify()]\cr
#'                      \tab                                   \tab                                                  \tab various file import functions\cr
#'                      \tab                                   \tab                                                  \tab [spc_fit_poly_below()]\cr
#' gc                   \tab FALSE                             \tab triggers frequent calling of gc ()               \tab [read.ENVI()], `new ("hyperSpec")`\cr
#' file.remove.emptyspc \tab TRUE                              \tab remove empty spectra directly on file import     \tab various file import functions\cr
#' file.keep.name       \tab TRUE                              \tab always create filename column                    \tab various file import functions\cr
#' tolerance            \tab `sqrt (.Machine$double.eps)` \tab tolerance for numerical comparisons              \tab [normalize01()], file import: `file.remove.emptyspc`\cr
#' wl.tolerance         \tab `sqrt (.Machine$double.eps)` \tab tolerance for comparisons of the wavelength axis \tab [all.equal()], [collapse()], [rbind()]\cr
#' plot.spc.nmax        \tab 25                                \tab number of spectra to be plotted by default       \tab [plot_spc()]\cr
#' ggplot.spc.nmax      \tab 10                                \tab                                                  \tab [`qplotspc()`](https://r-hyperspec.github.io/hySpc.ggplot2/reference/qplotspc.html)\cr
#' }
#'
#' `hy_set_options` will discard any values that were given without a  name.
#'
#' @rdname options
#' @param ... `hy_set_options`: pairs of argument names and values.
#'
#' `hy_get_options`: indices (or names) of the options.
#' @return
#' \tabular{ll}{
#' `hy_get_options` \tab returns a list of all options\cr
#' `hy_set_options` \tab invisibly returns a list with the options \cr
#' `hy_get_option`  \tab returns the value of the requested option \cr
#' }
#' @author C. Beleites
#'
#' @export
#'
#' @keywords misc
#' @concept utils
#'
#' @examples
#'
#' hy_get_options()
hy_get_options <- function(...) {
  dots <- c(...)
  if (length(dots) == 0L) {
    .options
  } else {
    .options[dots]
  }
}

# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(hy_get_options) <- function() {
  context("hy_get_options")

  test_that("proper return", {
    hy.opts <- get(".options", asNamespace("hyperSpec"))
    expect_equal(hy_get_options(), hy.opts)

    expect_equal(
      hy_get_options("debuglevel"),
      hy.opts["debuglevel"]
    )

    .options <- list()
    expect_equal(hy_get_options(), hy.opts)
  })
}


# Function -------------------------------------------------------------------

#' @rdname options
#' @export
#'
#' @concept utils
#'
#' @param name the name of the option
hy_get_option <- function(name) {
  .options[[name]]
}

# Function -------------------------------------------------------------------

#' @rdname options
#' @export
#'
#' @concept utils
#'
#' @importFrom utils modifyList
hy_set_options <- function(...) {
  new <- list(...)

  ## if called with list in 1st argument, use that list
  if (length(new) == 1 && is.list(new[[1]])) {
    new <- new[[1]]
  }

  names <- nzchar(names(new))

  if (!all(names) || length(names) != length(new)) {
    warning("options without name are discarded: ", which(!names))
  }

  opts <- modifyList(.options, new[names])

  opts$tolerance <- .checkpos(opts$tolerance, "tolerance")
  opts$wl.tolerance <- .checkpos(opts$wl.tolerance, "wl.tolerance")

  assign(".options", opts, envir = asNamespace("hyperSpec"))

  invisible(opts)
}

## check particular options that should exist and be finite and strictly positive
.checkpos <- function(opt, name) {
  if (length(opt) != 1L || !is.finite(opt) || opt < .Machine$double.eps) {
    warning(name, " must be a strictly positive finite number => set to .Machine$double.eps (", .Machine$double.eps, ").")
    opt <- .Machine$double.eps
  }

  opt
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(hy_set_options) <- function() {
  context("hy_set_options")

  old <- hy_get_options()
  on.exit(hy_set_options(old))

  test_that("new option and proper return value", {
    expect_equal(hy_set_options(bla = 1)$bla, 1)
    expect_equal(hy_get_option("bla"), 1)
  })

  test_that("setting", {
    tmp <- hy_set_options(debuglevel = 27)
    expect_equal(tmp$debuglevel, 27)

    tmp <- hy_set_options(list(debuglevel = 20))
    expect_equal(tmp$debuglevel, 20)

    tmp <- hy_set_options(debuglevel = 27, tolerance = 4)
    expect_equal(tmp$debuglevel, 27)
    expect_equal(tmp$tolerance, 4)

    tmp <- hy_set_options(list(debuglevel = 20, tolerance = 5))
    expect_equal(tmp$debuglevel, 20)
    expect_equal(tmp$tolerance, 5)
  })

  test_that("restrictions on tolerances", {
    for (o in c("tolerance", "wl.tolerance")) {
      expect_warning(hy_set_options(structure(list(0), .Names = o)))
      expect_equal(hy_get_option(o), .Machine$double.eps, label = o)

      hy_set_options(structure(list(1), .Names = o))
      expect_equal(hy_get_option(o), 1)
      expect_warning(hy_set_options(structure(list(-1), .Names = o)))
      expect_equal(hy_get_option(o), .Machine$double.eps, label = o)

      hy_set_options(structure(list(1), .Names = o))
      expect_equal(hy_get_option(o), 1)
      expect_warning(hy_set_options(structure(list(NA), .Names = o)))
      expect_equal(hy_get_option(o), .Machine$double.eps, label = o)
    }

    expect_warning(hy_set_options(tolerance = NULL))
    expect_equal(hy_get_option("tolerance"), .Machine$double.eps)

    expect_warning(hy_set_options(wl.tolerance = NULL))
    expect_equal(hy_get_option("wl.tolerance"), .Machine$double.eps)
  })


  test_that("options must be named", {
    tmp.a <- hy_get_options()
    expect_warning(tmp.b <- hy_set_options(1))
    expect_equal(tmp.a, tmp.b)
  })
}
