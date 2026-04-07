#' Check if a package is installed and load it
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated because all required packages are declared in
#' the package DESCRIPTION file and are automatically available. There is no
#' need to check or load them at runtime.
#'
#' @param package_name A string with the name of the package to check and load.
#'
#' @return Invisibly returns `TRUE` if the package namespace is available.
#' @import utils
#'
#' @examples
#' check_and_load_package("httr2")
#' @export
check_and_load_package <- function(package_name) {
  cli::cli_warn(c(
    "{.fn check_and_load_package} is deprecated.",
    "i" = "All required packages are declared in DESCRIPTION and loaded automatically."
  ))
  invisible(requireNamespace(package_name, quietly = TRUE))
}

#' Handle HTTP response from Server
#'
#' This function processes the response from the WebDAV server, checking for errors.
#'
#' @param response The response object from an `httr2` request.
#'
#' @return The processed response object if successful, or an error if the request failed.
#'
#' @export
handle_response <- function(response) {
  status <- httr2::resp_status(response)
  if (status >= 400) {
    cli::cli_abort("HTTP request failed with status: {status}")
  }
  response
}
