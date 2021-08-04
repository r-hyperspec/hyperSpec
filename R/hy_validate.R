
#' @concept utils

.validate <- function(object) {
  ncol <- ncol(object@data$spc)

  if (is.null(ncol)) {
    ncol <- 0
  }

  if (length(object@wavelength) != ncol) {
    return("Length of wavelength vector differs from number of data points per spectrum.")
  }

  TRUE
}


#' Check and validate `hyperSpec` objects
#'
#' Check whether an object is a [hyperSpec][hyperSpec::hyperSpec-class()]
#' object and validate the object.
#'
#' @aliases validObject validObject,hyperSpec-method chk.hy
#'
#' @param object The object to check.
#'
#' @return `TRUE` if the check passes, otherwise stop with an error.
#'
#' @author C. Beleites
#'
#' @seealso [methods::validObject()], [base::inherits()]
#'
#' @keywords methods
#' @concept utils
#'
#' @export
#'
#' @examples
#' chk.hy(faux_cell)
#' validObject(faux_cell)
chk.hy <- function(object) {
  if (!is(object, "hyperSpec")) {
    stop("no hyperSpec object")
  }

  TRUE
}

