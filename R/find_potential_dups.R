#' @title Identify potential duplicates based on title and year
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Currently this only works for files that were generated while Citavi
#' was set to "English" so that column names are "Short Title" etc.
#'
#' @param CitDat A dataframe/tibble returned by \code{\link[CitaviR]{find_obvious_dups}}
#' or \code{\link[CitaviR]{handle_obvious_dups}}.
#' @param minSimilarity Minimum similarity (between 0 and 1). Default is 0.6. (TO DO)
#' @param potDupAfterObvDup If TRUE (default), the newly created column
#' \code{pot_dup_id} is moved right next to the \code{obv_dup_id} column.
#'
#' @examples
#' path <- example_xlsx("3dupsin5refs.xlsx")
#' read_Citavi_xlsx(path) %>%
#'    find_obvious_dups() %>%
#'    find_potential_dups()
#'
#' @return A tibble containing one new column: \code{pot_dup_id}.
#' @importFrom RecordLinkage levenshteinSim
#' @importFrom scales percent
#' @importFrom stringr str_detect
#' @importFrom stringr str_pad
#' @importFrom tidyr pivot_longer
#' @importFrom utils combn
#' @import dplyr
#' @export

find_potential_dups <- function(CitDat, minSimilarity = 0.6, potDupAfterObvDup = TRUE) {

  ct <- CitDat %>%
    filter(as.integer(gsub("dup_","",.data$obv_dup_id)) == 1) %>% # dup_01 or dup_001 or dup_0001 ...
    pull(.data$clean_title)
  ct_padding <- ct %>% n_distinct() %>% log(10) %>% ceiling() + 1


  # calculate similarity = Levenshtein distance -----------------------------
  similarities <-
    as.data.frame(t(combn(ct, 2))) %>% # get unique combinations of clean_titles
    mutate(similarity =
             # Calculate similarity (=Levenshtein distance) between strings of possibly different lengths
             stringdist::stringdist(.data$V1, .data$V2, method = "lv") / (nchar(.data$V1) + nchar(.data$V2) / 2))

  # create clean_title_similarity -------------------------------------------
  similarities <- similarities %>% # similarity per pair
    filter(.data$similarity >= minSimilarity) %>% # keep only those with a minimum similarity
    arrange(.data$similarity, .desc = TRUE) %>%
    group_by(.data$V1, .data$V2) %>%
      mutate(similarityRank = cur_group_id()) %>% # similarity rank
    ungroup() %>%
    mutate(pot_dup_id = paste0(
      "potdup_",
      stringr::str_pad(.data$similarityRank, ct_padding, pad = "0"),
      " ",
      scales::percent(.data$similarity, accuracy = 0.1),
      " similarity"
    )) %>%
    select(-.data$similarity, -.data$similarityRank) %>%
    pivot_longer(cols = .data$V1:.data$V2,
                 values_to = "clean_title",
                 names_to = NULL)


  # join with CitDat --------------------------------------------------------
  CitDat <-
    left_join(x = CitDat, y = similarities, by = "clean_title") %>%
    mutate(pot_dup_id = if_else(
      as.integer(gsub("dup_","",.data$obv_dup_id)) > 1,  # not dup_01 or dup_001 or dup_0001 ...
      NA_character_,
      .data$pot_dup_id
    ))


  # potDupAfterObvDup -------------------------------------------------------
  if (potDupAfterObvDup) {
    CitDat <- CitDat %>%
      relocate("pot_dup_id", .after = "obv_dup_id")
  }


  # return tibble -----------------------------------------------------------
  CitDat
}
