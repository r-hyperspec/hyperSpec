##' line/word/character count of ASCII files
##'
##' `wc()` is defunct and will be removed from hyperSpec in future. Consider using [count_lines()] instead for line counting.
##' @seealso [count_lines()]
##' @export
##' @author C. Beleites
wc <- function (){
  .Defunct(new = "count_lines",
              msg = "wc() is now defunct and has been removed.")
}

