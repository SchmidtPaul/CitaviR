#' @title Importing Excel files created via Citavi Export
#'
#' @description
#' `r lifecycle::badge("maturing")`
#' Currently this only works for files that were generated while Citavi
#' was set to "English" or "German" so that column names are "Short title" or "Kurztitel" etc.
#'
#' @param path Path to the xls/xlsx file [created with Citavi via export to Excel](https://www1.citavi.com/sub/manual6/en/index.html?exporting_to_excel.html).
#' @param keepMarksCols If TRUE (default) it will keep and rename the first three
#' nameless columns of the imported excel file. These columns are automatically
#' created by the Citavi export and contain information about the attachment/paper-clip marker,
#' [the red flag marker and the blue circle marker](https://www1.citavi.com/sub/manual6/en/index.html?using_labels.html). If kept, the columns are renamed
#' `has_attachment`, `red_flag` and `blue_circle`.
#' If FALSE, these three columns are deleted.
#' @param useYearDerived If TRUE (default) the
#' [special column "Year derived"](https://www1.citavi.com/sub/manual6/en/index.html?cse_using_special_components.html)
#' (DE: ["Jahr ermittelt"](https://www1.citavi.com/sub/manual6/de/index.html?cse_using_special_components.html)) is renamed to "year" (DE:"Jahr") and thus replaces
#' the original basic column "year" (DE: "Jahr") which may have also been created
#' via the Citavi export.
#' @param setSuggestedColOrder If TRUE (default) columns a reordered in a suggested order.
#' @param ... Other arguments passed to the [readxl::read_xlsx()] function.
#'
#' @examples
#' path <- example_xlsx("3dupsin5refs.xlsx") # use this package's example xlsx file
#' read_Citavi_xlsx(path)
#'
#' \dontrun{
#' CitDat <- read_Citavi_xlsx("data/yourCitaviExport.xlsx")
#' }
#'
#' @return A tibble containing the information of the xls/xlsx file [created with Citavi via export to Excel](https://www1.citavi.com/sub/manual6/en/index.html?exporting_to_excel.html).
#' @importFrom readxl read_xlsx
#' @import dplyr
#' @export

read_Citavi_xlsx <- function(path = NULL, keepMarksCols = TRUE, useYearDerived = TRUE, setSuggestedColOrder = TRUE, ...) {

  if (is.null(path)) {
    stop("You did not provide a path to the Excel file.\n  If you want to use an example file provided in this package instead, try\n  read_Citavi_xlsx(example_xlsx('3dupsin5refs.xlsx'))")
  }

  # import ------------------------------------------------------------------
  suppressMessages( # because usually the first three columns (=MarksCols) have no names so we get message that they are automatically renamed "...1", "...2" and "...3".
    CitDat <- readxl::read_xlsx(path = path, ...)
  )

  # keepMarksCols: handle first 3 columns with marks ----------------
  if (all(names(CitDat)[1:3] == c("...1", "...2", "...3"))) {
    if (keepMarksCols) {
      CitDat <- CitDat %>%
        rename(
          "has_attachment" = 1,
          "red_flag" = 2,
          "blue_circle" = 3
        )
    } else {
      CitDat <- CitDat %>%
        select(-c("...1", "...2", "...3"))
    }
  }


  # useYearDerived: use "Jahr ermittelt" instead of "Jahr"  ---------
  if (useYearDerived) {
    year_key <-
      c(
        "Jahr" = "Jahr ermittelt",
        "Year" = "Year derived"
      )

    if (any(year_key %in% names(CitDat))) {
      CitDat <-
        CitDat[, -which(names(CitDat) %in% names(year_key))] %>% # delete original "year" if present
        rename(year_key[year_key %in% names(CitDat)]) # rename "year derived" to "year"
    }
  }


  # setSuggestedColOrder: order columns by default -----------------------------
  if (setSuggestedColOrder) {
    first_cols <- c(
      "ID",
      "Kurztitel", "Short title",
      "Titel", "Title",
      "Jahr", "Year"
    )

    last_cols <- c(
      "has_attachment",
      "red_flag",
      "blue_circle"
    )

    CitDat <- CitDat %>%
      relocate(any_of(first_cols[first_cols %in% names(CitDat)])) %>%
      relocate(any_of(last_cols[last_cols %in% names(CitDat)]), .after = last_col())
  }

  CitDat
}
