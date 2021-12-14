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
#' - `"pass as-is"` -- the same as `"pass"`, just output is not converted into
#'               an expression.
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
  on_failure <- match.arg(tolower(on_failure), c("fail", "warn", "pass", "pass as-is"))

  u_fixed <- .wl_fix_unit_name(unit, null_ok = null_ok, on_failure = "pass")

  if (greek) {
    # At first, suffix "_greek" is removed if present to avoid duplication
    u_fixed <- paste0(u_fixed, grep("_greek", "", u_fixed), "_greek")
  }

  switch(u_fixed,

    nm               = expression("Wavelength, nm"),
    nm_greek         = expression(list(lambda, nm)),

    invcm            = expression(list(Wavenumber, cm^-1)), # expression("Wavenumber, 1/cm"),
    invcm_greek      = expression(list(tilde(nu), cm^-1)),

    # Possible alternative is expression(list(Wavenumber ~ shift, cm^-1))
    raman            = expression(list(Raman ~ shift, cm^-1)),
    raman_greek      = expression(list(Delta * tilde(nu), cm^-1)),

    ev               = expression("Energy, eV"),
    ev_greek         = expression("E, eV"),

    freq             = expression("Frequency, THz"), # FIXME: why `freq` and not THz?
    freq_greek       = expression(list(nu, THz)),

    # Otherwise:
    {
      msg <- paste0(
        "Value '", unit,
        "' of argument 'unit' cannot be converted to any standard value."
      )

      switch(on_failure,

        fail = stop(msg),

        warn = {
          warning(
            msg, "\n",
            "So it will be converted to an expression and ",
            "returned as function's output."
          )
          as.expression(unit)
        },

        pass = as.expression(unit),

        "pass as-is" = unit
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

  test_that("wl_create_label_from_units() works with unit names (Greek)", {
    # nm Greek
    expect_silent(lbl <- wl_create_label_from_units("nm", greek = TRUE))
    expect_equal(class(lbl), "expression")
    expect_equal(lbl, expression(list(lambda, nm)))

    expect_equal(
      wl_create_label_from_units("invcm", greek = TRUE),
      expression(list(tilde(nu), cm^-1))
    )

    expect_equal(
      wl_create_label_from_units("Raman shift", greek = TRUE),
      expression(list(Delta * tilde(nu), cm^-1))
    )

    expect_equal(
      wl_create_label_from_units("eV", greek = TRUE),
      expression("E, eV")
    )

    expect_equal(
      wl_create_label_from_units("THz", greek = TRUE),
      expression(list(nu, THz))
    )
  })

  test_that("wl_create_label_from_units() works with unit names (text)", {
    # nm Text
    expect_silent(lbl <- wl_create_label_from_units("nm", greek = FALSE))
    expect_equal(class(lbl), "expression")
    expect_equal(lbl, expression("Wavelength, nm"))

    expect_equal(
      wl_create_label_from_units("invcm", greek = FALSE),
      expression(list(Wavenumber, cm^-1))
    )

    expect_equal(
      wl_create_label_from_units("Raman shift", greek = FALSE),
      expression(list(Raman ~ shift, cm^-1))
    )

    expect_equal(
      wl_create_label_from_units("eV", greek = FALSE),
      expression("Energy, eV")
    )

    expect_equal(
      wl_create_label_from_units("THz", greek = FALSE),
      expression("Frequency, THz")
    )

  })


  test_that("wl_create_label_from_units() fails correnctly", {

    expect_error(wl_create_label_from_units(), "argument \"unit\" is missing")

    expect_error(
      wl_create_label_from_units("WARNING!", on_failure = "fail"),
      "'unit' cannot be converted to any standard value"
    )

    expect_warning(
      wl_create_label_from_units("WARNING!", on_failure = "warn"),
      "'unit' cannot be converted to any standard value"
    )

    expect_equal(
      wl_create_label_from_units("WARNING!", on_failure = "pass"),
      as.expression("WARNING!")
    )

    expect_equal(
      wl_create_label_from_units("WARNING!", on_failure = "pass as-is"),
      "WARNING!"
    )

  })
}

