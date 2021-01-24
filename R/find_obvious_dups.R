#' @title Identify obvious duplicates based on title and year
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Currently this only works for files that were generated while Citavi
#' was set to "German" so that column names are "Kurztitel" etc.
#'
#' @param CitDat A dataframe holding the information of the xls/xlsx file
#' [created with Citavi via export to Excel](https://www1.citavi.com/sub/manual6/en/index.html?exporting_to_excel.html).
#' Thus, this dataframe may have been imported via \code{\link[CitaviR]{read_Citavi_xlsx}}. The following columns
#' \bold{must be present} in the dataframe: \code{ID}, \code{Title}, \code{Year}.
#'
#' @examples
#' \dontrun{
#' path <- example_xlsx("3dupsin5refs.xlsx") # use this package's example xlsx file
#' read_Citavi_xlsx(path) %>%
#'    find_obvious_dups()
#' }
#'
#' @return A tibble containing three additional columns:
#' \code{clean_title_id}, \code{has_obv_dup} and \code{obv_dup_id}.
#' @importFrom janitor make_clean_names
#' @importFrom stringr str_pad str_remove_all
#' @import dplyr
#' @export

find_obvious_dups <- function(CitDat) {

  clean_title <- NULL

  # if (!("ID" %in% names(CitDat))) { # TO DO: %not_in%
    ID <- NULL
  # } else {stop("'ID' column is missing!")}
  #
  # if (!("Title" %in% names(CitDat)==FALSE)) { # TO DO: %not_in%
    Title <- NULL
  # } else {stop("'Title' column is missing!")}
  #
  # if (!("Year" %in% names(CitDat)==FALSE)) { # TO DO: %not_in%
    Year <- NULL
  # } else {stop("'Year' column is missing!")}


  temp <- CitDat[, c("ID", "Title", "Year")] %>%
    arrange("Title")

  # clean_title -------------------------------------------------------------
  temp <- temp %>%
    mutate(
      clean_title =
        paste(Title, Year, "END") %>%     # combine Title & Year column and end string with "END"
        janitor::make_clean_names() %>%   # break down string to "_"-character, numbers, and letters
        stringr::str_remove_all("_\\d+$") # remove numbers at end of string created by make_clean_names()
    )

  # clean_title & obv_dup ID ------------------------------------------------
  clean_title_id_padding <-
    temp %>% pull(clean_title) %>% n_distinct() %>% log(10) %>% ceiling() + 1

  obv_dup_id_padding <-
    temp %>% count(clean_title) %>% pull(n) %>% max() %>% log(10) %>% ceiling() + 1

  temp <- temp %>%
    group_by(clean_title) %>%
    mutate(clean_title_id =
             cur_group_id() %>%
             stringr::str_pad(width = clean_title_id_padding, pad = "0") %>%
             paste0("ct_", .data)) %>%
    mutate(has_obv_dup = case_when(n()  > 1 ~ TRUE,
                                   n() == 1 ~ FALSE)) %>%
    mutate(obv_dup_id =
             1:n() %>%
             stringr::str_pad(width = obv_dup_id_padding, pad = "0") %>%
             paste0("dup_", .data)) %>%
    ungroup()


  # merge -------------------------------------------------------------------
  CitDat <- left_join(
    x = CitDat,
    y = temp,
    by = c("ID", "Title", "Year")
  )

  CitDat

}
