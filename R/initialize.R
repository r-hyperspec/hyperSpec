
#' Create a `hyperSpec` object
#'
#' To create a new `hyperSpec` object, the following functions can be used:
#' - [new()] (i.e., `new("hyperSpec", ...)`);
#' - `new_hyperSpec()`.
#'
#' @note
#'
#' A `hyperSpec` object is an S4 object, so its initialization is carried out
#' by calling `new("hyperSpec", ...)`. Function `new_hyperSpec()` is just
#' a convenience function.
#'
#' @docType methods
#'
#' @name initialize
#' @rdname initialize
#'
#' @aliases
#'  initialize
#'  initialize,hyperSpec-method
#'  create
#'  create,hyperSpec-method
#'  new
#'  new,hyperSpec-method
#'  new_hyperSpec
#'
#' @param spc (`matrix` or convertible to `matrix`)  \cr
#'        A spectra matrix with spectra in rows and wavelength intensities in
#'        columns.
#'
#'  The `spc` does not need to be an R `matrix`, but must be an object
#'  convertible to a matrix via `I(as.matrix(spc))`.
#'
#' @param data (`data.frame`)  \cr
#'        A `data.frame` with extra (non-spectroscopic) data in columns.
#'        The data frame may also contain a special column `spc` with a `matrix`
#'        of spectroscopic data.
#'        (Such single column that contains matrix can be created with
#'        `data.frame(spc = I(as.matrix(spc)))`.
#'        However, it will usually be more convenient if the spectra are given
#'        via argument `spc`.)
#'
#' @param wavelength (numeric vector)  \cr
#'        The wavelengths corresponding to the columns of `spc`.
#'
#'  If no wavelengths are given, an appropriate vector is derived from the
#'  column the column names of `data$spc`. If this is not possible,
#'  `1:ncol(data$spc)` is used instead.
#'
#' @param labels A named `list`:
#' - list's element names should containing one or more names of `data`
#'   columns as well as special name `.wavelength` for `wavelength`s ).
#' - list's element values should contain the labels for the indicated
#'   names usually either in a for of character strings or
#'   [plotmath][grDevices::plotmath()] expressions.
#'   (The labels should be given in a form ready
#'   for the text-drawing functions, see [grDevices::plotmath()]).
#'
#' If `label` is not given, a list containing `NULL` for each of the
#' columns of `data` and `wavelength` is used.
#'
#' @param gc (logical) \cr Use garbage collection.
#'       If option `gc` is `TRUE`, the initialization will have frequent calls
#'       to [base::gc()], which can help to avoid swapping or running out of
#'       memory. The default value of `gc` can be set via [hy_set_options()].
#'
#' @param log This parameter is currently **ignored**. It is present due to
#'        backwards compatibility.
#'
#' @param .Object A new `hyperSpec` object.
#'
#'
#' @author C.Beleites, V. Gegzna
#' @seealso
#'
#' - [methods::new()] for more information on creating and initializing S4 objects.
#' - [grDevices::plotmath()] on expressions for math annotations as for slot `label`.
#' - [hy_set_options()] setting `hyperSpec` options.
#'
#' @keywords methods datagen
#' @concept hyperSpec conversion
#'
#' @examples
#'
#' new("hyperSpec")
#' new_hyperSpec()
#'
#' spc <- matrix(rnorm(12), ncol = 4)
#' new("hyperSpec", spc = spc)
#' new_hyperSpec(spc = spc)
#'
#' new("hyperSpec",
#'   data = data.frame(x = letters[1:3]),
#'   spc = spc
#' )
#'
#' colnames(spc) <- 600:603
#' new("hyperSpec", spc = spc) # wavelength taken from colnames (spc)
#'
#' # given wavelengths precede over colnames of spc
#' new("hyperSpec", spc = spc, wavelength = 700:703)
#'
#' # specifying labels
#' h <- new("hyperSpec",
#'   spc = spc, data = data.frame(pos = 1:3),
#'   label = list(
#'     spc = "I / a.u.",
#'     .wavelength = expression(tilde(nu) / cm^-1),
#'     pos = expression("/"(x, mu * m))
#'   )
#' )
#'
#' plot(h)
#' plot_c(h, spc ~ pos)
NULL


# Functions ------------------------------------------------------------------

#' @include paste_row.R
#' @noRd
.initialize <- function(.Object, spc = NULL, data = NULL, wavelength = NULL,
                        labels = NULL, gc = hy_get_option("gc"),
                        log = "ignored") {
  # Do the small stuff first, so we need not be too careful about copies

  # The wavelength axis
  if (!is.null(wavelength) && !is.numeric(wavelength)) {
    stop("wavelength is not numeric but ", class(wavelength), ".")
  }

  if (!is.null(spc)) {
    if (is.null(dim(spc))) {
      nwl <- length(spc)
      if (gc) base::gc()
      dim(spc) <- c(1, nwl)
      if (gc) base::gc()
    } else {
      nwl <- ncol(spc)
    }
  } else if (!is.null(data$spc)) {
    nwl <- ncol(data$spc)
  } else {
    nwl <- 0
  }

  if (is.null(spc) && is.null(data) && !is.null(wavelength) &&
    is.numeric(wavelength) && is.vector(wavelength)) {
    spc <- matrix(NA_real_, ncol = length(wavelength), nrow = 0)
  }

  if (is.null(wavelength)) {
    # Guess from spc's colnames
    if (!is.null(spc)) {
      wavelength <- as.numeric(colnames(spc))
    }

    if (length(wavelength) == 0L || any(is.na(wavelength))) {
      wavelength <- as.numeric(colnames(data$spc))
    }

    if (length(wavelength) == 0L || any(is.na(wavelength))) {
      wavelength <- seq_len(nwl)
    } # guessing didn't work
  } else if (!is.numeric(wavelength)) {
    stop("wavelength must be numeric.")
  }
  .Object@wavelength <- wavelength

  # Column + wavelength axis labels
  if (is.null(labels) || length(labels) == 0L) {
    cln <- c(colnames(data), ".wavelength")
    if (!any(grepl("spc", cln))) {
      cln <- c(cln, "spc")
    }
    labels <- vector("list", length(cln))
    names(labels) <- cln
    rm(cln)
  }

  # Transform labels into expressions
  .make.expression <- function(x) {
    if (is.language(x) && !is.expression(x)) {
      class(x) <- "expression"
    } else if (is.character(x)) {
      x <- as.expression(x)
    }
    x
  }

  labels <- lapply(labels, .make.expression)

  .Object@label <- labels

  rm(labels)
  if (gc) base::gc()

  if (!is.null(data$spc) && !(is.null(spc))) {
    warning("Spectra in data are overwritten by argument spc.")
  }

  # Deal with spectra
  if (is.null(spc) && is.null(data$spc)) {
    spc <- structure(numeric(0), .Dim = c(0L, 0L))
  }

  if (!is.null(spc) && !is.matrix(spc)) {
    spc <- as.matrix(spc)
    if (ncol(spc) == 1L) {
      spc <- t(spc)
    }
  }

  if (!is.null(spc) && !is.numeric(spc) && !all(is.na(spc))) {
    dim <- dim(spc)
    spc <- suppressWarnings(as.numeric(spc))
    if (all(is.na(spc))) {
      stop("spectra matrix needs to be numeric or convertable to numeric")
    } else {
      warning("spectra matrix is converted from ", class(data$spc), " to numeric.")
    }

    dim(spc) <- dim
  }

  if (gc) base::gc()

  if (!is.null(spc)) {
    attr(spc, "class") <- "AsIs" # I seems to make more than one copy
    if (gc) base::gc()
  }

  # Deal with extra data
  if (is.null(data)) {
    data <- data.frame(spc = spc)
  } else if (!is.null(spc)) {
    if (nrow(data) == 1 && nrow(spc) > 1) {
      data <- data[rep(1, nrow(spc)), , drop = FALSE]
    }

    data$spc <- spc
  }

  rm(spc)
  if (gc) base::gc()

  attr(data$spc, "class") <- NULL # More than one copy!?
  if (gc) base::gc()

  .Object@data <- data
  if (gc) base::gc()


  .Object <- .spc_fix_colnames(.Object) # For consistency with .wl<-

  # Finally: check whether we got a valid hyperSpec object
  validObject(.Object)

  .Object
}

#' @rdname initialize
#' @export
new_hyperSpec <- function(spc = NULL, data = NULL, wavelength = NULL,
                          labels = NULL, gc = hy_get_option("gc"),
                          log = "ignored") {
  new("hyperSpec", spc = spc, data = data, wavelength = wavelength,
    labels = labels, gc = gc)
}

#' @rdname initialize
setMethod("initialize", "hyperSpec", .initialize)


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.initialize) <- function() {
  context(".initialize / new (\"hyperSpec\")")

  test_that("empty hyperSpec object", {
    expect_equal(dim(new("hyperSpec")), c(nrow = 0L, ncol = 1L, nwl = 0L))
  })

  test_that("vector for spc", {
    h <- new("hyperSpec", spc = 1:4)
    expect_equal(h@data$spc, matrix(1:4, nrow = 1, dimnames = list(NULL, 1:4)))
    expect_equal(as.numeric(colnames(h@data$spc)), 1:4)
    expect_equal(dim(h), c(nrow = 1L, ncol = 1L, nwl = 4L))
    expect_equal(h@wavelength, 1:4)
  })

  test_that("matrix for spc", {
    spc <- matrix(c(1:12), nrow = 3)
    h <- new("hyperSpec", spc = spc)

    expect_equivalent(h@data$spc, spc)
    expect_equal(dimnames(h@data$spc), list(NULL, as.character(1:4)))
    expect_equal(dim(h@data$spc), dim(spc))

    expect_equal(dim(h), c(nrow = 3L, ncol = 1L, nwl = 4L))
    expect_equal(h@wavelength, 1:4)
  })

  spc <- matrix(c(1:12), nrow = 3)
  test_that("matrix with numbers in colnames for spc", {
    colnames(spc) <- c(600, 601, 602, 603)
    h <- new("hyperSpec", spc = spc)
    expect_equal(h@data$spc, spc)
    expect_equal(dim(h), c(nrow = 3L, ncol = 1L, nwl = 4L))
    expect_equal(h@wavelength, c(600, 601, 602, 603))
  })

  colnames(spc) <- c(600, 601, 602, 603)
  test_that("spc and data given", {
    h <- new("hyperSpec", spc = spc, data = data.frame(x = 3))
    expect_equal(h@data$spc, spc)
    expect_equal(dim(h), c(nrow = 3L, ncol = 2L, nwl = 4L))
    expect_equal(h@wavelength, c(600, 601, 602, 603))
    expect_equal(h@data$x, rep(3, 3L))
  })

  test_that("spc and data given, data has $spc column (which should be overwritten with warning)", {
    expect_warning(h <- new("hyperSpec", spc = spc, data = data.frame(spc = 11:13)))
    expect_equal(h@data$spc, spc)
    expect_equal(dim(h), c(nrow = 3L, ncol = 1L, nwl = 4L))
    expect_equal(h@wavelength, c(600, 601, 602, 603))
  })

  test_that("spc and data given, different numbers of rows", {
    expect_error(new("hyperSpec", spc = spc, data = data.frame(x = 11:12)))
  })

  test_that("only data given, data has $spc column with `I()`-protected matrix", {
    h <- new("hyperSpec", data = data.frame(spc = I(spc)))
    expect_equal(h@data$spc, spc)
    expect_equal(dim(h), c(nrow = 3L, ncol = 1L, nwl = 4L))
    expect_equal(h@wavelength, c(600, 601, 602, 603))
  })

  test_that("spc is data.frame", {
    h <- new("hyperSpec", spc = as.data.frame(spc))
    expect_equal(h@data$spc, spc)
    expect_equal(dim(h), c(nrow = 3L, ncol = 1L, nwl = 4L))
  })

  test_that("uncommon spectra matrix class that can be converted to numeric", {
    expect_warning(new("hyperSpec", flu > 100))
  })

  test_that("spectra matrix class cannot be converted to numeric", {
    expect_error(new("hyperSpec", matrix(letters[1:6], 3)))
  })

  test_that("error if wavelength is not numeric", {
    expect_error(new("hyperSpec", spc = NA, wavelength = letters[1:3]))
  })


  test_that("gc option", {
    option <- hy_get_option("gc")
    on.exit(hy_set_options(gc = option))
    hy_set_options(gc = TRUE)

    spc <- new("hyperSpec", spc = flu[[]])
    expect_equal(spc[[]], flu[[]])
  })

  test_that("hyperSpec initializes with wavelength only", {
    # One wavelength
    expect_silent(hy_obj_1 <- new("hyperSpec", wavelength = 1))
    expect_equal(nwl(hy_obj_1), 1)
    expect_equal(nrow(hy_obj_1), 0)
    expect_equal(ncol(hy_obj_1), 1)
    expect_equal(colnames(hy_obj_1), "spc")

    # 100 wavelengths
    expect_silent(hy_obj_2 <- new("hyperSpec", wavelength = 1:100))
    expect_equal(nwl(hy_obj_2), 100)
    expect_equal(nrow(hy_obj_2), 0)
    expect_equal(ncol(hy_obj_2), 1)
    expect_equal(colnames(hy_obj_2), "spc")
  })

  test_that("hyperSpec initializes with gc", {
    expect_silent(hy_obj_1a <- new("hyperSpec", wavelength = 1:100, gc = FALSE))
    expect_silent(hy_obj_2a <- new("hyperSpec", wavelength = 1:100, gc = TRUE))
    expect_equal(hy_obj_1a, hy_obj_2a)

    expect_silent(hy_obj_1b <- new("hyperSpec", data.frame(), gc = FALSE))
    expect_silent(hy_obj_2b <- new("hyperSpec", data.frame(), gc = TRUE))
    expect_equal(hy_obj_1b, hy_obj_2b)

    expect_silent(hy_obj_1c <- new("hyperSpec", NA, gc = FALSE))
    expect_silent(hy_obj_2c <- new("hyperSpec", NA, gc = TRUE))
    expect_equal(hy_obj_1c, hy_obj_2c)
  })


  test_that('new_hyperSpec() and new("hyperSpec") give identical results', {
    expect_equal(new_hyperSpec(), new("hyperSpec"))
    expect_equal(new_hyperSpec(spc = 1:4), new("hyperSpec", spc = 1:4))
    expect_equal(
      new_hyperSpec(spc = spc, data = data.frame(x = 11:13)),
      new("hyperSpec", spc = spc, data = data.frame(x = 11:13))
    )
  })
}
