# Helper function ------------------------------------------------------------

#' Search if machine has IP
#'
#' This function searches for IP address in IP config.
#' If there is IP, it is assumed that the machine has Internet connection.
#' Otherwise it is assumed that machine is offline.
#'
#' @return `TRUE` if IP is found or `FALSE` otherwise.
#'
#' @noRd
.is_ip_found <- function() {
  ip_config_cmd <- switch(.Platform$OS.type,
    "windows" = "ipconfig",
    "ifconfig"
  )
  any(grep("(\\d{1,3}[.]){3}(\\d{1,3})$", system(ip_config_cmd, intern = TRUE)))
}


# Function -------------------------------------------------------------------

#' List available *R hyperSpec* family packages
#'
#' @description
#' Get the names of *R hyperSpec* family packages that are available on GitHub
#' organization [**`r-hyperspec`**](https://r-hyperspec.github.io/)
#' (the official homepage of these packages).
#'
#' This function requires Internet connection.
#'
#' @details
#' If your machine is connected to the Internet and you receive issues due to
#' overused limit of anonymous connections to GitHub, you may run alternative
#' code that employs GitHub PAT (personal access token) for authentication.
#' You have to have GitHub account (you can register here: https://github.com/).
#' Then, on how to set your GitHub PAT, see sections "10.1.2 How to get a PAT?",
#' "10.2.1 Call an R function to store your credentials", and (if you need more
#' information) others on this chapter
#' https://happygitwithr.com/credential-caching.htm and set PAT.
#' Then use this code:
#'
#' ```r
#' # install.packages("gh")
#' gh_api_response <- gh::gh("GET /users/r-hyperspec/repos?per_page=100")
#' repo_names <- vapply(gh_api_response, "[[", "", "name")
#' package_names <- grep("^hyperSpec|^hySpc[.]", repo_names, value = TRUE)
#' package_names
#' ```
#'
#' @return Character vector with the names of the packages.
#' @export
#'
#' @concept utils
#'
#' @author V. Gegzna
#'
#' @examples
#' \dontrun{\donttest{
#' hy_list_available_hySpc_packages()
#' }}
hy_list_available_hySpc_packages <- function() {
  if (.is_ip_found()) {
    tryCatch(
      {
        # Gets data via GitHub API
        # On some machines the answer is a single-line text, on others -
        # "beautified" JSON code that spans several rows.
        gh_api_response <- readLines(
          "https://api.github.com/orgs/r-hyperspec/repos?per_page=100",
          warn = FALSE
        )
      },
      error = function(e) {
        # If connection fails due to other reasons
        stop(
          "Website https://api.github.com/ cannot be reached at the moment. \n",
          "You may have reached the daily limit of annonymous connections to",
          " GitHub. \n",
          "The original error message: \n",
          e,
          call. = FALSE
        )
        message(e)
      }
    )

    # convert JSON into a single string with unnecessary whitespace removed:
    response_as_single_line <- paste(trimws(gh_api_response), collapse = "")

    # Parse downloaded data:
    one_line_per_repo <- strsplit(response_as_single_line, "}}")[[1]]
    pattern <- '(?<="name":")(hyperSpec|hySpc[.].*?)(?=",)'
    matches <- regexpr(pattern = pattern, text = one_line_per_repo, perl = TRUE)
    package_names <- regmatches(one_line_per_repo, m = matches)
    package_names
  } else {
    # If connection fails due to being offline (i.e., without IP):
    stop(
      "Website https://api.github.com/ cannot be reached at the moment. \n",
      "Please, check your Internet connection."
    )
  }
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(hy_list_available_hySpc_packages) <- function() {
  context("hy_list_available_hySpc_packages")

  test_that("hy_list_available_hySpc_packages() works", {
    testthat::skip_if_offline()

    # FIXME: The lines below should be fixed in the future
    # Skip on GihHub Actions (as it usually fails to connect to GH on macOS):
    testthat::skip_on_ci()

    expect_silent(pkgs <- hy_list_available_hySpc_packages())
    expect_is(pkgs, "character")
    expect_true(length(pkgs) > 5)
    expect_true(all(grepl("^hySpc[.]|^hyperSpec$", pkgs)))
  })
}
