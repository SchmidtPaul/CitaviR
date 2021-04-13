#' @title Identify obvious duplicates based on title and year
#'
#' @param CitDat A dataframe/tibble returned by \code{\link[CitaviR]{read_Citavi_xlsx}}.
#' The following columns \bold{must be present}: \code{ID}, \code{Title}, \code{Year}.
#' @param dupInfoAfterID If TRUE (default), the newly created columns
#' \code{clean_title}, \code{clean_title_id}, \code{has_obv_dup} and \code{obv_dup_id}
#' are moved right next to the \code{ID} column. Additionally, the \code{ID} column is
#' moved to the first position.
#' @param preferDupsWithPDF If TRUE (default), obvious duplicates are sorted by their info
#' in columns \code{has_attachment} and/or \code{Locations} (given they are present in the dataset).
#' After sorting, duplicates with the most occurences of \code{".pdf"} in \code{Locations} and a
#' \code{TRUE} in \code{has_attachment} are first and will thus be chosen as \code{dup_01}.
#'
#' @details
#' `r lifecycle::badge("maturing")` \cr
#' Currently this only works for files that were generated while Citavi
#' was set to "English" so that column names are "Short Title" etc.
#'
#' @examples
#' example_path <- example_file("3dupsin5refs/3dupsin5refs.ctv6")
#' read_Citavi_ctv6(example_path) %>%
#'    find_obvious_dups() %>%
#'    dplyr::select(clean_title:obv_dup_id)
#'
#' @return A tibble containing four additional columns:
#' \code{clean_title}, \code{clean_title_id}, \code{has_obv_dup} and \code{obv_dup_id}.
#' @importFrom janitor make_clean_names
#' @importFrom stringr str_count
#' @importFrom stringr str_pad str_remove_all
#' @importFrom utils tail
#' @import dplyr
#' @export

find_obvious_dups <- function(CitDat, dupInfoAfterID = TRUE, preferDupsWithPDF = TRUE) {

  if ("StaticIDs" %in% names(CitDat)) {
    CitDat <- CitDat %>% rename("RefID" = "StaticIDs")
    OriginalRefIDLabel <- "StaticIDs"
  } else {
    CitDat <- CitDat %>% rename("RefID" = "ID")
    OriginalRefIDLabel <- "ID"
  }

  required_cols <- c("RefID", "Title", "Year")

  # RefID, Title & Year present? --------------------------------------------
  for (col_name_i in required_cols) {
    if (col_name_i %not_in% names(CitDat)) {
      stop(paste(col_name_i, "column is missing!"))
    }
  }


  # clean_title -------------------------------------------------------------
  CitDat <- CitDat %>%
    mutate(
      clean_title =
        paste(.data$Title, .data$Year, "END") %>% # combine Title & Year column and end string with "END"
        janitor::make_clean_names() %>%   # break down string to "_"-character, numbers, and letters
        stringr::str_remove_all("_\\d+$") # remove numbers at end of string created by make_clean_names()
    )


  # if titles have (pdf) attachments, prefer those (for dup_01) -------------
  if (preferDupsWithPDF) {
    if (all("has_attachment" %in% names(CitDat) & "Locations" %not_in% names(CitDat))) {
      CitDat <- CitDat %>% arrange(.data$clean_title,
                                   desc(.data$has_attachment))
    }

    if (all(c("has_attachment", "Locations") %in% names(CitDat))) {
      CitDat <- CitDat %>% arrange(.data$clean_title,
                                   desc(stringr::str_count(.data$Locations, ".PDF|.pdf")),
                                   desc(.data$has_attachment))
    }
  }


  # clean_title & obv_dup ID ------------------------------------------------
  clean_title_id_padding <-
    CitDat %>% pull(.data$clean_title) %>% n_distinct() %>% log(10) %>% ceiling() + 1

  obv_dup_id_padding <-
    CitDat %>% count(.data$clean_title) %>% pull(n) %>% max() %>% log(10) %>% ceiling() + 1

  CitDat <- CitDat %>%
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


  # dupInfoAfterID ----------------------------------------------------------
  if (dupInfoAfterID) {
    CitDat <- CitDat %>%
      relocate("RefID", dplyr::everything()) %>%
      relocate(c(
        "clean_title",
        "clean_title_id",
        "has_obv_dup",
        "obv_dup_id"
      ),
      .after = "RefID")
  }


  # Original RefID label ----------------------------------------------------
  names(CitDat)[names(CitDat) == "RefID"] <- OriginalRefIDLabel


  # return tibble -----------------------------------------------------------
  CitDat

}
