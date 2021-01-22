#' @title Importing Excel files created via Citavi Export
#'
#' @description Currently this only works for files that were generated while Citavi
#' was set to "English" or "German" so that column names are "Short title" or "Kurztitel" etc.
#'
#' @param path Path to the xls/xlsx file \href{https://www1.citavi.com/sub/manual6/en/index.html?exporting_to_excel.html}{created with Citavi via export to Excel}.
#' @param keepMarksCols If TRUE (default) it will keep and rename the first three
#' nameless columns of the imported excel file. These columns are automatically
#' created by the Citavi export and contain information about the attachment/paper-clip marker,
#' \href{https://www1.citavi.com/sub/manual6/en/index.html?using_labels.html}{the red flag marker and the blue circle marker}. If kept, the columns are renamed
#' \code{has_attachment}, \code{red_flag} and \code{blue_circle}.
#' If FALSE, these three columns are deleted.
#' @param useYearDerived If TRUE (default) the
#' \href{https://www1.citavi.com/sub/manual6/en/index.html?cse_using_special_components.html}{special column "Year derived"}
#' (DE: \href{https://www1.citavi.com/sub/manual6/de/index.html?cse_using_special_components.html}{"Jahr ermittelt"}) is renamed to "year" (DE:"Jahr") and thus replaces
#' the original basic column "year" (DE: "Jahr") which may have also been created
#' via the Citavi export.
#' @param setSuggestedColOrder If TRUE (default) columns a reordered in a suggested order.
#' @param ... Other arguments passed to the \code{\link{readxl::read_xlsx()}} function.
#'
#' @examples
#' \dontrun{
#' CitDat <- read_Citavi_xlsx("data/myCitaviExport.xlsx")
#' }
#'
#' @return A tibble containing the information of the xls/xlsx file \href{https://www1.citavi.com/sub/manual6/en/index.html?exporting_to_excel.html}{created with Citavi via export to Excel}.
#' @export

read_Citavi_xlsx <- function(path, keepMarksCols = TRUE, useYearDerived = TRUE, setSuggestedColOrder = TRUE, ...) {

  # import ------------------------------------------------------------------
  suppressMessages( # because usually the first three columns (=MarksCols) have no names so we get message that they are automatically renamed "...1", "...2" and "...3".
    CitDat <- readxl::read_xlsx(path = path, ...)
  )

  # keepMarksCols: handle first 3 columns with marks ----------------
  if ( all(names(CitDat)[1:3] == c("...1", "...2", "...3")) ) {

    if (keepMarksCols) {

      CitDat <- CitDat %>%
        rename("has_attachment" = 1,
               "red_flag"       = 2,
               "blue_circle"    = 3)

    } else {

      CitDat <- CitDat %>%
        select(-c("...1", "...2", "...3"))

    }

  }


  # useYearDerived: use "Jahr ermittelt" instead of "Jahr"  ---------
  if (useYearDerived) {

    year_key <-
      c("Jahr" = "Jahr ermittelt",
        "Year" = "Year derived")

    if ( any(year_key %in% names(CitDat)) ) {

      CitDat <- CitDat %>%
        select(-matches(names(year_key)), everything()) %>%  # delete original "year" if present
        rename(year_key[year_key %in% names(CitDat)])  # rename "year derived" to "year"

    }

  }


  # setSuggestedColOrder: order columns by default -----------------------------
  if (setSuggestedColOrder) {

    first_cols <- c("ID",
                    "Kurztitel", "Short title",
                    "Titel", "Title",
                    "Jahr", "Year")

    last_cols  <- c("has_attachment",
                    "red_flag",
                    "blue_circle")

    CitDat <- CitDat %>%
      relocate(any_of(first_cols[first_cols %in% names(CitDat)])) %>%
      relocate(any_of(last_cols[last_cols  %in% names(CitDat)]), .after = last_col())
  }

  CitDat
}
