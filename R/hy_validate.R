
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


# Function -------------------------------------------------------------------

#' Check and validate `hyperSpec` objects
#'
#' Check whether an object is a [hyperSpec][hyperSpec::hyperSpec-class()]
#' object and validate the object.
#'
#' @name is_hyperSpec
#' @aliases is_hyperSpec assert_hyperSpec
#'          validObject validObject,hyperSpec-method
#'
#' @param object The object to check.
#'
#'
#' @return `TRUE` if the check passes, otherwise stop with an error.
#'
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
#' is_hyperSpec(faux_cell)
#'
#' assert_hyperSpec(faux_cell)
#'
#' validObject(faux_cell)
is_hyperSpec <- function(object) {
  inherits(object, "hyperSpec")
}


#' @rdname is_hyperSpec
#' @export
assert_hyperSpec <- function(object) {
  if (!is_hyperSpec(object)) {
    stop(
      "Not a 'hyperSpec' object! \n",
      "Class(es) of the object: ", paste0(class(object), collapse = ", ")
    )
  }

  TRUE
}


# Unit tests -----------------------------------------------------------------

# FIXME: add unit tests
