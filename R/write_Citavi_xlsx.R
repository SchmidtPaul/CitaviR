#' @title Export
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' #'
#' @param CitDat A tibble of processed Citavi data TO DO
#' @param read_path The path used for \code{\link[CitaviR]{read_Citavi_xlsx}}.
#'
#'
#' @examples
#' \dontrun{
#' your_path <- "data/yourCitaviExport.xlsx"
#' read_Citavi_xlsx(your_path) %>%
#'    find_obvious_duplicates %>%
#'    handle_obvious_duplcates %>%
#'    write_Citavi_xlsx(your_path)
#' }
#'
#' @return nothing ?! TO DO
#' @importFrom openxlsx write.xlsx
#' @export

write_Citavi_xlsx <- function(CitDat, read_path = NULL) { # TO DO: Optionally choose export path

  if (is.null(read_path)) {
    stop("You did not provide a path to the imported Excel file")
  } else {
    last_dot    <- regexpr("\\.[^\\.]*$", read_path)[1]
    export_path <- substr(read_path, 1, last_dot - 1)
    export_path <- paste0(export_path, "_R_out.xlsx")
  }

  openxlsx::write.xlsx(x = CitDat, file = export_path)

}
