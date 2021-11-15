# Function -------------------------------------------------------------------

#' Create label from units for wavelength axis
#'
#' This function takes the name of units for wavelength axis and creates an
#' R expression which can be used as a label for `@wavelength` axis for
#' `hyperSpec` object.
#'
#' @param unit (character):
#'        A name of unit for wavelength axis.
#' @param greek (logical):
#'        Should Greek symbols be preferred in the output expression?
#' @param on_failure (string):
#'        The type of behavior in case unrecognized value of `unit` is passed:
#' - `"fail"` -- the code is stopped with an error message.
#' - `"warn"` -- a warning is issued and the value of `unit` is converted to an
#'               expression and returned as an output.
#' - `"pass"` -- the value of `unit` is converted to an expression and returned
#'               as an output and no error nor warning is issued.
#' @param null_ok (logical):
#'        Should value `NULL` be accepted as `unit`.
#'        This argument is passed to [.wl_fix_unit_name()].
#'
#' @author V. Gegzna
#'
#' @concept wavelengths
#' @include wl_convert_units.R
#'
#' @export
#'
#' @examples
#' wl_create_label_from_units("nm")
#' wl_create_label_from_units("nm", greek = TRUE)
#'
#' wl_create_label_from_units("1/cm", greek = TRUE)
#'
wl_create_label_from_units <- function(unit, greek = FALSE,
                                       on_failure = "warn", null_ok = FALSE) {

  if (missing(unit)) stop("argument \"unit\" is missing")
  on_failure <- match.arg(tolower(on_failure), c("fail", "warn", "pass"))

  u_fixed <- .wl_fix_unit_name(unit, null_ok = null_ok, on_failure = "pass")

  if (greek) {
    # At first, suffix "_greek" is removed if present to avoid duplication
    u_fixed <- paste0(u_fixed, grep("_greek", "", u_fixed), "_greek")
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
      msg <- "The value of 'unit' is not recognized: "

      switch(on_failure,

        fail = stop(msg, unit),

        warn = {
          warning(
            msg, unit, "\n",
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
    expect_silent(wl_create_label_from_units("nm"))
  })

  test_that("wl_create_label_from_units() works with 'nm' (Greek)", {
    # nm Greek
    expect_silent(lbl <- wl_create_label_from_units("nm", greek = TRUE))
    expect_equal(class(lbl), "expression")
    expect_equal(lbl, expression(lambda / nm))
  })

  test_that("wl_create_label_from_units() works with 'nm' (text)", {
    # nm Text
    expect_silent(lbl <- wl_create_label_from_units("nm", greek = FALSE))
    expect_equal(class(lbl), "expression")
    expect_equal(lbl, expression("Wavelength, nm"))
  })


  test_that("wl_create_label_from_units() fails correnctly", {

    expect_equal(
      wl_create_label_from_units("WARNING!", on_failure = "pass"),
      as.expression("WARNING!")
    )

    expect_warning(
      wl_create_label_from_units("WARNING!", on_failure = "warn"),
      "The value of 'unit' is not recognized:"
    )

    expect_error(
      wl_create_label_from_units("WARNING!", on_failure = "fail"),
      "The value of 'unit' is not recognized:"
    )

    expect_error(wl_create_label_from_units(), "argument \"unit\" is missing")
  })
}
