# Function -------------------------------------------------------------------

#' Create label from units for wavelength axis
#'
#' This function takes the name of units for wavelength axis and creates an
#' R expression which can be used as a label for `@wavelength` axis for
#' `hyperSpec` object.
#'
#' @param wl_units (character):
#'        A name of unit for wavelength axis.
#' @param greek (logical):
#'        Should Greek symbols be preferred in the output expression?
#' @param warn (logical):
#'        Should a warning be issued if a value of `wl_units` is not recognized?
#' @param fail (numeric):
#'        Should an error be issued and execution of code be stopped if a value
#'        of `wl_units` is not recognized?
#' @param null_ok (logical):
#'        Should value `NULL` be accepted as `wl_units`.
#'        This argument is passed to [.wl_fix_unit_name()].
#'
#' @author V. Gegzna
#'
#' @concept wavelengths
#' @include wl_convert_units.R
#'
#' @examples
#' wl_create_label_from_units("nm")
#' wl_create_label_from_units("nm", greek = TRUE)
#'
#' wl_create_label_from_units("1/cm", greek = TRUE)
#'
wl_create_label_from_units <- function(wl_units, greek = FALSE, warn = TRUE,
                                       fail = FALSE, null_ok = FALSE) {

  u_fixed <- .wl_fix_unit_name(wl_units, null_ok = null_ok)

  if (greek) {
    # At first, suffix "_greek" is removed if present to avoid duplication
    u_fixed <- paste0(grep("_greek", "", u_fixed), "_greek")
  }

  switch(u_fixed,

    nm               = expression("Wavelength, nm"),
    nm_greek         = expression(lambda / nm),

    invcm            = expression(Wavenumber / cm^-1), # expression("Wavenumber, 1/cm"),
    invcm_greek      = expression(tilde(nu) / cm^-1),

    wn_shift         = expression(Wavenumber ~ shift / cm^-1), # wavenumber_shift
    wn_shift_greek   = expression(Delta * tilde(nu) / cm^-1),

    raman            = expression(Raman ~ shift / cm^-1),
    raman_greek      = expression(Delta * tilde(nu) / cm^-1),

    ev               = expression("Energy, eV"),
    ev_greek         = expression("E / eV"),

    freq             = expression("Frequency, THz"),
    freq_greek       = expression(nu / THz),

    # Otherwise:
    {
      if (fail) {
        stop("The value of 'wl_units' is not identified: ", wl_units)
      }

      if (warn) {
        warning(
          "The value of 'wl_units' is not identified: '",
          wl_units,
          "'. So it will be returned as an output."
        )
      }

      wl_units
    }
  )
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(wl_create_label_from_units) <- function() {
  context("wl_create_label_from_units")

  test_that("wl_create_label_from_units() works", {
    expect_error(wl_create_label_from_units(), 'argument "wl_units" is missing')
    expect_silent(wl_create_label_from_units("nm"))
  })

  test_that("wl_create_label_from_units() works with `'nm", {
    # nm Greek
    expect_silent(lbl <- wl_create_label_from_units("nm", greek = TRUE))
    expect_equal(class(lbl), "expression")
    expect_equal(lbl, expression(lambda / nm))

    # nm Text
    expect_silent(lbl <- wl_create_label_from_units("nm", greek = FALSE))
    expect_equal(class(lbl), "expression")
    expect_equal(lbl, expression("Wavelength, nm"))
  })


  # FIXME: does not work yet:

  # expect_warning(
  #   wl_create_label_from_units("WARNING!"),
  #   "The value of 'wl_units' is not identified:"
  # )
}

