#' @name DEPRECATED-spc.identify
#' @concept deprecated
#'
#' @title (DEPRECATED)
#'        Identifying spectra and spectral data points
#'
#' @description
#' These \pkg{hyperSpec} functions are **deprecated** and not maintained any
#' more. You should not use these.
#' Currently they are present due to back-compatibility reasons and will be
#' removed in the next release of the package.
#' Please, use the suggested alternative functions instead:
#'
#' - [hyperSpec::identify_spc()]
#' - [hyperSpec::format_label_wl_only()]
#' - [hyperSpec::format_label_ispc_wl()]
#' - [hyperSpec::locate_spc_point_parabola_max()]
#' - [hyperSpec::locate_spc_point_min()]
#' - [hyperSpec::locate_spc_point_max()]
#' - [hyperSpec::locate_spc_point_clicked()]
#'
#'
#' @param ... arguments to [hyperSpec::identify_spc()] and other functions.
#'
#' @include identify_spc.R
#' @export
spc.identify <- function(...) {
  hySpc_deprecated("identify_spc")
  identify_spc(...)
}

#' @rdname DEPRECATED-spc.identify
#' @export
spc.label.wlonly <- function(...) {
  hySpc_deprecated("format_label_wl_only")
  format_label_wl_only(...)
}

#' @rdname DEPRECATED-spc.identify
#' @export
spc.label.default <- function(...) {
  hySpc_deprecated("format_label_ispc_wl")
  format_label_ispc_wl(...)
}

#' @rdname DEPRECATED-spc.identify
#' @export
spc.point.sqr <- function(...) {
  hySpc_deprecated("locate_spc_point_parabola_max")
  locate_spc_point_parabola_max(...)
}

#' @rdname DEPRECATED-spc.identify
#' @export
spc.point.min <- function(...) {
  hySpc_deprecated("locate_spc_point_min")
  locate_spc_point_min(...)
}

#' @rdname DEPRECATED-spc.identify
#' @export
spc.point.max <- function(...) {
  hySpc_deprecated("locate_spc_point_max")
  locate_spc_point_max(...)
}

#' @rdname DEPRECATED-spc.identify
#' @export
spc.point.default <- function(...) {
  hySpc_deprecated("locate_spc_point_clicked")
  locate_spc_point_clicked(...)
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(spc.identify) <- function() {
  context("Deprecated functions")

  test_that("spc.identify() is deprecated", {
    expect_error(expect_warning(spc.identify(), "deprecated"))
    expect_error(expect_warning(spc.label.wlonly(), "deprecated"))
    expect_error(expect_warning(spc.label.default(), "deprecated"))
    expect_error(expect_warning(spc.point.sqr(), "deprecated"))
    expect_error(expect_warning(spc.point.min(), "deprecated"))
    expect_error(expect_warning(spc.point.max(), "deprecated"))
    expect_error(expect_warning(spc.point.default(), "deprecated"))
  })
}
