# Function -------------------------------------------------------------------

#' @include wl_convert_units.R
#' @author V. Gegzna
wl_create_label_by_units <- function(wl_units, greek = FALSE, warn = TRUE,
                                     fail = FALSE) {

  u_fixed <- .wl_fix_unit_name(wl_units)

  if (greek) {
    u_fixed <- paste0(u_fixed, "_greek")
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

hySpc.testthat::test(wl_create_label_by_units) <- function() {
  context("wl_create_label_by_units")

  test_that("wl_create_label_by_units() works", {
    expect_error(wl_create_label_by_units(), 'argument "wl_units" is missing')
    expect_silent(wl_create_label_by_units("nm"))
  })

  test_that("wl_create_label_by_units() works with `'nm", {
    # nm Greek
    expect_silent(lbl <- wl_create_label_by_units("nm", greek = TRUE))
    expect_equal(class(lbl), "expression")
    expect_equal(lbl, expression(lambda / nm))

    # nm Text
    expect_silent(lbl <- wl_create_label_by_units("nm", greek = FALSE))
    expect_equal(class(lbl), "expression")
    expect_equal(lbl, expression("Wavelength, nm"))
  })


  # FIXME: does not work yet:

  # expect_warning(
  #   wl_create_label_by_units("WARNING!"),
  #   "The value of 'wl_units' is not identified:"
  # )
}

