#' @title Get path to example file
#'
#' @description
#' `r lifecycle::badge("stable")` \cr
#' CitaviR comes bundled with some example files in its `inst/extdata`
#' directory. This function makes them easy to access.
#'
#' @param file Name of file. If `NULL`, all example files will be listed.
#' @export
#' @examples
#' example_file()
#' example_file("3dupsin5refs.xlsx")
#' example_file("3dupsin5refs/3dupsin5refs.ctv6")

example_file <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "CitaviR"), recursive = TRUE)
  } else {
    system.file("extdata", file, package = "CitaviR", mustWork = TRUE)
  }
}
