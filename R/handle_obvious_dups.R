#' @title Handle obvious duplicates
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Currently this only works for files that were generated while Citavi
#' was set to "English" so that column names are "Short Title" etc.
#'
#' @param CitDat A dataframe returned by \code{\link[CitaviR]{find_obvious_dups}}.
#' @param colsToWriteInto Can be "refInfo", "all" or a custom character vector with column names. (TO DO)
#' @param nameDupCat What should the duplicate category be named? (TO DO)
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

handle_obvious_dups <- function(CitDat, colsToWriteInto = "refInfo", nameDupCat = NULL) { # TO DO: better name?

  # Which columns to handle -------------------------------------------------
  if (colsToWriteInto[1] == "refInfo") {
    colsToWriteInto <- c("Online address", "PubMed ID") # TO DO: more default columns of interest!
    colsCatGroKey   <- c("Categories") #, "Group", "Keyword") # TO DO: Add Group & Keyword possibility!
  }
  if (colsToWriteInto[1] == "all") {
    # TO DO: all columns except Title etc.
  }

  # colsToWriteInto ---------------------------------------------------------
  colsToWriteIntoHere <- colsToWriteInto[colsToWriteInto %in% names(CitDat)]

  CitDat <- CitDat %>%
    group_by(.data$clean_title) %>%
    tidyr::fill(colsToWriteIntoHere, .direction = "up") %>% # TO DO: more sophisticated. What if multiple entries?
    ungroup()

  # colsCatGroKey -----------------------------------------------------------
  colsCatGroKeyHere <- colsCatGroKey[colsCatGroKey %in% names(CitDat)]

  # collapse unique categories per clean_title_id
  CitDat <- CitDat %>%
    group_by(.data$clean_title_id) %>%
    mutate_at(
      .vars = vars(colsCatGroKeyHere),
      .funs = ~ case_when(.data$has_obv_dup == TRUE ~ paste(unique(.), collapse = "; "),
                          TRUE ~ .)
    ) %>%
    ungroup()

  if (!is.null(nameDupCat)) {
    # overwrite collapsed categories with "nameDupCat" string
    CitDat <- CitDat %>%
      mutate_at(
        .vars = vars(colsCatGroKeyHere),
        .funs = ~ case_when(
          .data$has_obv_dup == TRUE &
            .data$obv_dup_id != "dup_01" ~ paste(nameDupCat),
          TRUE ~ .
        )
      )

  }

  CitDat

}
