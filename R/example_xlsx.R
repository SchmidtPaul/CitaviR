#' @title Get path to example xlsx exported from Citavi
#'
#' @description
#' `r lifecycle::badge("stable")` \cr
#' CitaviR comes bundled with some example files in its `inst/extdata`
#' directory. This function makes them easy to access.
#'
#' @param file Name of file. If `NULL`, all example files will be listed.
#' @export
#' @examples
#' example_xlsx()
#' example_xlsx("3dupsin5refs.xlsx")

example_xlsx <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "CitaviR"))
  } else {
    system.file("extdata", file, package = "CitaviR", mustWork = TRUE)
  }
}
