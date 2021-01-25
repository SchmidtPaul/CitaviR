#' @title Identify obvious duplicates based on title and year
#'
#' @description
#' `r lifecycle::badge("maturing")`
#' Currently this only works for files that were generated while Citavi
#' was set to "English" so that column names are "Short Title" etc.
#'
#' @param CitDat A dataframe holding the information of the xls/xlsx file
#' [created with Citavi via export to Excel](https://www1.citavi.com/sub/manual6/en/index.html?exporting_to_excel.html).
#' Thus, this dataframe may have been imported via \code{\link[CitaviR]{read_Citavi_xlsx}}. The following columns
#' \bold{must be present} in the dataframe: \code{ID}, \code{Title}, \code{Year}.
#' @param dupInfoAfterID If TRUE (default), the newly created columns
#' \code{clean_title}, \code{clean_title_id}, \code{has_obv_dup} and \code{obv_dup_id}
#' are moved right next to the \code{ID} column.
#'
#' @examples
#' path <- example_xlsx("3dupsin5refs.xlsx")
#' read_Citavi_xlsx(path) %>%
#'    find_obvious_dups()
#'
#' @return A tibble containing three additional columns:
#' \code{clean_title_id}, \code{has_obv_dup} and \code{obv_dup_id}.
#' @importFrom janitor make_clean_names
#' @importFrom stringr str_pad str_remove_all
#' @importFrom utils tail
#' @import dplyr
#' @export

find_obvious_dups <- function(CitDat, dupInfoAfterID = TRUE) {

  col_names <- c("ID", "Title", "Year")

  # ID, Title & Year present? -----------------------------------------------
  for (col_name_i in col_names) {
    if (col_name_i %not_in% names(CitDat)) {
      stop(paste(col_name_i, "column is missing!"))
    }
  }

  # reduce for now and sort -------------------------------------------------
  temp <- CitDat[, col_names] %>%
    arrange(col_names[2])

  # clean_title -------------------------------------------------------------
  temp <- temp %>%
    mutate(
      clean_title =
        paste(.data$Title, .data$Year, "END") %>% # combine Title & Year column and end string with "END"
        janitor::make_clean_names() %>%   # break down string to "_"-character, numbers, and letters
        stringr::str_remove_all("_\\d+$") # remove numbers at end of string created by make_clean_names()
    )

  # clean_title & obv_dup ID ------------------------------------------------
  clean_title_id_padding <-
    temp %>% pull(.data$clean_title) %>% n_distinct() %>% log(10) %>% ceiling() + 1

  obv_dup_id_padding <-
    temp %>% count(.data$clean_title) %>% pull(n) %>% max() %>% log(10) %>% ceiling() + 1

  temp <- temp %>%
    group_by(.data$clean_title) %>%
    mutate(clean_title_id =
             paste0(
               "ct_",
               cur_group_id() %>%
                 stringr::str_pad(width = clean_title_id_padding, pad = "0")
             )) %>%
    mutate(has_obv_dup = case_when(n()  > 1 ~ TRUE,
                                   n() == 1 ~ FALSE)) %>%
    mutate(obv_dup_id =
             paste0(
               "dup_",
               1:n() %>%
                 stringr::str_pad(width = obv_dup_id_padding, pad = "0")
             )) %>%
    ungroup()


  # merge -------------------------------------------------------------------
  CitDat <- left_join(
    x = CitDat,
    y = temp,
    by = col_names
  )

  # dupInfoAfterID ----------------------------------------------------------
  if (dupInfoAfterID) {
    CitDat <- CitDat %>%
      relocate(any_of(tail(names(CitDat), 4)), .after = col_names[1])
  }

  CitDat

}
