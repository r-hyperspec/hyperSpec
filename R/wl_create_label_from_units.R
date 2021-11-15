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
#' @param on_failure (string):
#'        The type of behavior in case unrecognized value of `wl_units` is passed:
#' - `"fail"` -- the code is stopped with an error message.
#' - `"warn"` -- a warning is issued and the value of `wl_units` is converted to an
#'               expression and returned as an output.
#' - `"pass"` -- the value of `wl_units` is converted to an expression and returned
#'               as an output and no error nor warning is issued.
#' @param null_ok (logical):
#'        Should value `NULL` be accepted as `wl_units`.
#'        This argument is passed to [.wl_fix_unit_name()].
#'
#' @author V. Gegzna
#'
#' @concept wavelengths
#' @include wl_convert_units.R
#' @include hyperspec-class.R
#'
#' @examples
#' wl_create_label_from_units("nm")
#' wl_create_label_from_units("nm", greek = TRUE)
#'
#' wl_create_label_from_units("1/cm", greek = TRUE)
#'
wl_create_label_from_units <- function(wl_units, greek = FALSE,
                                       on_failure = "warn", null_ok = FALSE) {

  on_failure <- match.arg(tolower(on_failure), c("fail", "warn", "pass"))

  u_fixed <- .wl_fix_unit_name(wl_units, null_ok = null_ok, on_failure = "pass")

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

    freq             = expression("Frequency, THz"), # FIXME: why `freq` and not THz?
    freq_greek       = expression(nu / THz),

    # Otherwise:
    {
      msg <- "The value of 'wl_units' is not identified: "
      switch(
        on_failure,
        fail = stop(msg, wl_units),
        warn = {
          warning(
            msg, wl_units, "\n",
            "So it will be converted to an expression and ",
            "returned as the output."
          )
          as.expression(units)
        },
        pass = as.expression(units)
      )
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

  test_that("wl_create_label_from_units() works with 'nm'", {
    # nm Greek
    expect_silent(lbl <- wl_create_label_from_units("nm", greek = TRUE))
    expect_equal(class(lbl), "expression")
    expect_equal(lbl, expression(lambda / nm))

    # nm Text
    expect_silent(lbl <- wl_create_label_from_units("nm", greek = FALSE))
    expect_equal(class(lbl), "expression")
    expect_equal(lbl, expression("Wavelength, nm"))
  })


  test_that("wl_create_label_from_units() fails correnctly", {

    expect_equal(
      wl_create_label_from_units("WARNING!", on_failure = "pass"),
      "WARNING!"
    )

    expect_warning(
      wl_create_label_from_units("WARNING!", on_failure = "warn"),
      "The value of 'wl_units' is not identified:"
    )

    expect_error(
      wl_create_label_from_units("WARNING!", on_failure = "fail"),
      "The value of 'wl_units' is not identified:"
    )
  })
}
