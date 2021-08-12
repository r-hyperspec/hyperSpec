# Function -------------------------------------------------------------------

.sample_h <- function(x, size, replace = FALSE, prob = NULL, index = FALSE) {
  validObject(x)

  if (missing(size)) size <- nrow(x) # normal default does not work!

  rows_i <-
    sample.int(nrow(x@data), size = size, replace = replace, prob = prob)

  if (isTRUE(index)) {
    rows_i
  } else {
    x[rows_i]
  }
}

#' Sample or permute rows of `hyperSpec`, `data.frame`, or `matrix`
#'
#' [hyperSprc::sample()] takes a random sample (drawn with or  without
#' replacement) of rows from the object `x`.
#'
#' @name sample
#' @rdname sample
#' @aliases sample.hyperSpec
#'          sample,hyperSpec-method
#'
#' @docType methods
#'
#' @param x `hyperSpec` object, `data.frame` or `matrix` to sample rows from.
#'
#' @param size (integer): Positive integer giving the number of spectra (rows)
#'        to choose. Id missing, defaults to number of rows in the object:
#'        `size = nrow(x)`.
#'
#' @param replace (logical): Should sampling be with replacement?
#'
#' @param prob (numeric): A vector of probability weights for obtaining the
#'        elements of the vector being sampled.
#'
#' @param index (logical): If `FALSE`, object of `class(x)` is returned,
#'        if `TRUE`, numeric vector is returned.
#'
#'
#' @return
#' - If `index = FALSE`, function returns a `hyperSpec` object, `data.frame` or
#'   `matrix` (object of the same class as `x`) with `size` rows.
#' - If `index = TRUE`, function returns a vector with row indices of size
#'   `size` suitable for subsetting rows of `x`.
#'
#' @seealso [base::sample()]
#'
#'
#' @author C. Beleites, V. Gegzna
#'
#' @keywords methods distribution
#' @concept stats
#'
#' @export
#'
#' @examples
#' set.seed(2021)
#'
#' sample(flu, 3)
#'
#' sample(flu, 3, index = TRUE)
#'
#' sample(flu, 3, replace = TRUE, index = TRUE)
#'
#' sample(flu, 8, replace = TRUE, index = TRUE)
#'
#'
#' plot(flu, col = "darkgray")
#' plot(sample(flu, 3), col = "red", add = TRUE)
#'
#' plot(flu, col = "darkgray")
#' plot(sample(flu, 3, replace = TRUE),
#'   col = "#0000FF80", add = TRUE,
#'   lines.args = list(lwd = 2)
#' )
setMethod("sample", signature = signature(x = "hyperSpec"), .sample_h)

# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.sample_h) <- function() {
  context(".sample")

  test_that("defaults", {
    tmp <- sample(flu)
    expect_equal(tmp[order(tmp$c)], flu)

    set.seed(101)
    expect_equal(sample(flu)$c, c(0.05, 0.3, 0.1, 0.15, 0.25, 0.2))
  })

  test_that("size", {
    expect_length(sample(flu, size = 3), 3L)
  })

  test_that("prob", {
    expect_equal(sample(flu, size = 1, prob = c(1, rep(0, 5))), flu[1L])
  })

  test_that("replace", {
    expect_equal(
      sample(flu, size = 3, replace = TRUE, prob = c(1, rep(0, 5))),
      flu[rep(1L, 3)]
    )
  })


  test_that("defaults, index = TRUE", {
    expect_equal(sort(sample(flu, index = TRUE)), 1:nrow(flu))

    set.seed(101)
    expect_equal(sample(flu, index = TRUE), c(1L, 6L, 2L, 3L, 5L, 4L))
  })

  test_that("size, index = TRUE", {
    expect_length(sample(flu, size = 3, index = TRUE), 3L)
  })

  test_that("prob, index = TRUE", {
    expect_equal(
      sample(flu, size = 1, prob = c(1, rep(0, 5)), index = TRUE),
      1L
    )
  })

  test_that("replace, index = TRUE", {
    probs <- c(1, rep(0, 5))

    expect_equal(
      sample(flu, size = 3, replace = TRUE, prob = probs, index = TRUE),
      rep(1L, 3)
    )
  })
}


# Function -------------------------------------------------------------------

.sample.data.frame <- function(x, size, replace = FALSE, prob = NULL,
                               index = FALSE, drop = FALSE) {
  if (missing(size)) size <- nrow(x)
  rows_i <- sample.int(nrow(x), size = size, replace = replace, prob = prob)

  if (isTRUE(index)) {
    rows_i
  } else {
    x[rows_i, , drop = drop]
  }
}

#' @rdname sample
#' @aliases sample.data.frame
#'          sample,data.frame-method
#'
#' @param drop (logical): See [base::drop()], by default, do not drop dimensions
#'        of the result. Applicable only if `index = FALSE`.
#'
#' @concept stats
#'
#' @export
#'
#' @examples
#'
#' sample(cars, 2)
#'
#' sample(cars, 2, index = TRUE)
setMethod("sample", signature = signature(x = "data.frame"), .sample.data.frame)


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.sample.data.frame) <- function() {
  context("sample data.frame")
  test_that("data.frame", {
    set.seed(101)
    tmp <- sample(iris)
    expect_equal(rownames(tmp), c(
      "73", "57", "95", "148", "61", "59", "99", "128", "131", "32",
      "9", "96", "144", "98", "60", "147", "145", "14", "97", "45",
      "117", "42", "64", "90", "43", "146", "125", "130", "58", "85",
      "84", "133", "8", "72", "20", "6", "88", "39", "10", "74", "89",
      "26", "140", "139", "37", "81", "135", "44", "138", "109", "108",
      "3", "111", "116", "66", "65", "142", "28", "22", "80", "93",
      "30", "25", "127", "103", "18", "50", "17", "86", "110", "34",
      "150", "112", "106", "2", "15", "100", "62", "7", "52", "56",
      "129", "101", "4", "143", "122", "79", "55", "149", "41", "114",
      "12", "21", "94", "120", "113", "105", "54", "31", "77", "118",
      "38", "136", "92", "19", "23", "16", "67", "134", "47", "35",
      "69", "63", "75", "5", "121", "132", "126", "27", "48", "87",
      "137", "13", "11", "102", "123", "24", "51", "46", "82", "40",
      "115", "1", "119", "141", "33", "70", "68", "83", "91", "29",
      "36", "78", "107", "76", "104", "71", "49", "53", "124"
    ))
    expect_equal(dim(tmp), dim(iris))
    expect_equal(tmp, iris[as.numeric(rownames(tmp)), ])
  })

  test_that("data.frame, index = TRUE", {
    set.seed(101)
    tmp <- sample(iris, index = TRUE)
    expect_length(tmp, nrow(iris))
    expect_equal(tmp[1:3], c(73, 57, 95))
  })
}


# Function -------------------------------------------------------------------

.sample.matrix <- function(x, size, replace = FALSE, prob = NULL,
                           index = FALSE, drop = FALSE) {
  if (missing(size)) size <- nrow(x)
  rows_i <- sample.int(nrow(x), size = size, replace = replace, prob = prob)

  if (isTRUE(index)) {
    rows_i
  } else {
    x[rows_i, , drop = drop]
  }
}

#' @rdname sample
#' @aliases sample.matrix
#'          sample,matrix-method
#'
#' @concept stats
#'
#' @export
#' @examples
#'
#' sample(matrix(1:24, 6), 2)
#'
#' sample(matrix(1:24, 6), 2, index = TRUE)
setMethod("sample", signature = signature(x = "matrix"), .sample.matrix)


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(.sample.matrix) <- function() {
  context(".sample.matrix")
  test_that("matrix", {
    set.seed(101)
    tmp <- sample(flu[[]])
    expect_equal(dim(tmp), dim(flu[[]]))
    expect_equal(tmp[c(1L, 3L, 4L, 6L, 5L, 2L), ], flu[[]])
  })

  test_that("matrix, index = TRUE", {
    set.seed(101)
    tmp <- sample(flu[[]], index = TRUE)
    expect_equal(length(tmp), nrow(flu[[]]))
    expect_equal(tmp,  c(1, 6, 2, 3, 5, 4))
  })
}
