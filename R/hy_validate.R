
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
#' @author C. Beleites, V. Gegzna
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

hySpc.testthat::test(is_hyperSpec) <- function() {
  local_edition(3)

  test_that("is_hyperSpec() works", {
    expect_true(is_hyperSpec(flu))
    expect_false(is_hyperSpec(1:5))
  })

  test_that("assert_hyperSpec() works", {
    expect_true(assert_hyperSpec(flu))
    expect_error(assert_hyperSpec(1:5), "Not a 'hyperSpec' object!")
    expect_error(assert_hyperSpec(1L:5L), "integer")
    expect_error(assert_hyperSpec("A"), "character")
  })
}
