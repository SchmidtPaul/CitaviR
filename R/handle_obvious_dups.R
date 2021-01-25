#' @title Handle obvious duplicates
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Currently this only works for files that were generated while Citavi
#' was set to "English" so that column names are "Short Title" etc.
#'
#' @param CitDat A dataframe returned by \code{\link[CitaviR]{find_obvious_dups}}.
#' @param colsOfInterest Can be "refInfo", "all" or a custom character vector with column names. (TO DO)
#'
#' @examples
#' path <- example_xlsx("3dupsin5refs.xlsx")
#' read_Citavi_xlsx(path) %>%
#'    find_obvious_dups() %>%
#'    handle_obvious_dups()
#'
#' @return A tibble where information was spread across obvious duplicates. (TO DO)
#' @importFrom tidyr fill
#' @import dplyr
#' @export

handle_obvious_dups <- function(CitDat, colsOfInterest = "refInfo") { # TO DO: better name?

  if (colsOfInterest[1] == "refInfo") {
    colsOfInterest <- c("PubMed ID", "Online address") # TO DO: more default columns of interest!
  }
  if (colsOfInterest[1] == "all") {
    # TO DO: all columns except Title etc.
  }

  colsOfInterestHere <- colsOfInterest[colsOfInterest %in% names(CitDat)]

  CitDat <- CitDat %>%
    group_by(.data$clean_title) %>%
    tidyr::fill(colsOfInterestHere, .direction = "up") %>% # TO DO: more sophisticated. What if multiple entries?
    ungroup()

  CitDat

}
