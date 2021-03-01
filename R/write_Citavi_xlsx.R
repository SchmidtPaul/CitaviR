#' @title Export
#'
#' @param CitDat A tibble.
#' @param read_path The path/xlsx file name that was used for \code{\link[CitaviR]{read_Citavi_xlsx}}.
#' @param write_path path/xlsx file name
#' @param ... Other arguments passed to the [openxlsx::write.xlsx()] function.
#'
#' @details
#' `r lifecycle::badge("maturing")`
#'
#' @examples
#' \dontrun{
#' your_read_path <- "data/yourCitaviExport.xlsx"
#'
#' read_Citavi_xlsx(path = your_path) %>%
#'    find_obvious_duplicates() %>%
#'    write_Citavi_xlsx(read_path = your_read_path)
#'
#' read_Citavi_xlsx(path = your_path) %>%
#'    find_obvious_duplicates() %>%
#'    write_Citavi_xlsx(write_path = "output/yourNewFile.xlsx")
#' }
#'
#' @importFrom openxlsx write.xlsx
#' @export

write_Citavi_xlsx <- function(CitDat, read_path = NULL, write_path = NULL, ...) {

  if (is.null(read_path) & is.null(write_path)) {
    stop("You must provide a path!")
  }

  if (!is.null(read_path) & !is.null(write_path)) {
    stop("You must not provide more than one path!")
  }

  if (is.character(read_path)) {
    last_dot   <- regexpr("\\.[^\\.]*$", read_path)[1]
    write_path <- substr(read_path, 1, last_dot - 1)
    write_path <- paste0(write_path, "_R_out.xlsx")
  }

  openxlsx::write.xlsx(x = CitDat, file = write_path, ...)

}
