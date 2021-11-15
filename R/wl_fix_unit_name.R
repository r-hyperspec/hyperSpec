#' Bring argument to a conventional name
#'
#' Fix a string with unit name (for wavelength axis) into a sting that
#' can be used as a standardized argument value for other \pkg{hyperSpec}
#' functions, e.g., [wl_convert_units()].
#'
#'
#' @param unit (sting):
#'        A name, abbreviation or shot description of unit for wavelength axis.
#'
#' @param null_ok (logical):
#'        If `unit = NULL` and `null_ok = TRUE`, the function will not fail and
#'        will return `NULL`.
#'
#' @param on_failure (string):
#'        The type of behavior in case unrecognized value of `unit` is passed
#'        (i.e., in case of failure to convert to a standardized value):
#' - `"fail"` -- the code is stopped with an error message.
#' - `"warn"` -- a warning is issued and the value of `unit` is returned as
#'               an output.
#' - `"pass"` -- the value of `unit` is returned as an output and no error nor
#'               warning is issued.
#'
#'
#' @return
#' The function returns one of these "standard" values:
#' - `"raman"` for Raman shift in relative 1/cm;
#' - `"invcm"` for inverted centimeters (1/cm);
#' - `"nm"`    for nanometers (nm);
#' - `"ev"`    for electron volts (eV);
#' - `"freq"`  for frequency (THz);
#' - `"px"`    for pixels.
#'
#'
#' @seealso [wl_convert_units()]
#' @author R. Kiselev, V. Gegzna
#'
#' @export
#'
#' @examples
#' .wl_fix_unit_name("wavelength")


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.wl_fix_unit_name) <- function() {
  context(".wl_fix_unit_name")

  test_that(".wl_fix_unit_name() works", {
    expect_equal(.wl_fix_unit_name("raman"), "raman")
    expect_equal(.wl_fix_unit_name("raman shift"), "raman")
    expect_equal(.wl_fix_unit_name("raman_shift"), "raman")
    expect_equal(.wl_fix_unit_name("raman.shift"), "raman")
    expect_equal(.wl_fix_unit_name("invcm"), "invcm")
    expect_equal(.wl_fix_unit_name("nm"), "nm")
    expect_equal(.wl_fix_unit_name("ev"), "ev")
    expect_equal(.wl_fix_unit_name("freq"), "freq")
    expect_equal(.wl_fix_unit_name("px"), "px")
    expect_equal(.wl_fix_unit_name("file"), "file")

    expect_error(.wl_fix_unit_name("dDd"), "Unknown unit type")
    expect_equal(.wl_fix_unit_name("dDd", on_failure = "pass"), "dDd")
    expect_warning(.wl_fix_unit_name("dDd", on_failure = "warn"), "dDd")

    expect_error(.wl_fix_unit_name(NULL), "argument is of length zero")
    expect_silent(.wl_fix_unit_name(NULL, null_ok = TRUE))
  })

  # TODO (tests): add more specific tests.
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
