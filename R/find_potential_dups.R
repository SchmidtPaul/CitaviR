#' @title Identify potential duplicates based on title and year
#'
#' @description
#' `r lifecycle::badge("maturing")`
#' Currently this only works for files that were generated while Citavi
#' was set to "English" so that column names are "Short Title" etc.
#'
#' @param CitDat A dataframe/tibble returned by \code{\link[CitaviR]{find_obvious_dups}}
#' or \code{\link[CitaviR]{handle_obvious_dups}}.
#' @param minSimilarity Minimum similarity (between 0 and 1). Default is 0.6. (TO DO)
#' @param potDupAfterObvDup If TRUE (default), the newly created column
#' \code{pot_dup_id} is moved right next to the \code{obv_dup_id} column.
#' @param maxNumberOfComp Maximum number of clean_title similarity calculations to be made.
#' It is set to 1,000,000 by default (which corresponds to ~ 1414 clean_titles). TO DO: Document while-loop.
#'
#' @examples
#' path <- example_xlsx("3dupsin5refs.xlsx")
#' read_Citavi_xlsx(path) %>%
#'    find_obvious_dups() %>%
#'    find_potential_dups()
#'
#' @return A tibble containing one new column: \code{pot_dup_id}.
#' @importFrom RcppAlgos comboGeneral
#' @importFrom scales percent
#' @importFrom stats na.omit
#' @importFrom stringdist stringdist
#' @importFrom stringr str_length
#' @importFrom stringr str_pad
#' @importFrom tidyr pivot_longer
#' @import dplyr
#' @import tictoc
#' @export

find_potential_dups <- function(CitDat, minSimilarity = 0.6, potDupAfterObvDup = TRUE, maxNumberOfComp = 1000000) {

  myf <- function(x){format(x, digits = 0, big.mark = ",")}


  # combinations of clean_title ---------------------------------------------
  ct <- CitDat %>% pull(.data$clean_title) %>% unique # should be equal to filtering for dup_01
  pot_dup_id_padding <- ct %>% n_distinct() %>% log(10) %>% ceiling() + 1

  ct_similar <- RcppAlgos::comboGeneral(v = ct, 2) %>% # slower alternative: combn()
    as_tibble(.name_repair = ~c("ct1", "ct2")) %>%
    mutate(ct1nchar = stringr::str_length(.data$ct1), # number of characters in string
           ct2nchar = stringr::str_length(.data$ct2)) # slower alternative: nchar()


  # Too many combinations? --------------------------------------------------
  NumberOfComp <- nrow(ct_similar)
  ncharDiffCutoff <- 42 # Don't panic!

  if (NumberOfComp > maxNumberOfComp) {

    cat("clean_title comparisons =", myf(NumberOfComp), ">", myf(maxNumberOfComp), "= maxNumberOfComp",
        "\n  Trying to ignore comparisons with large differences in character length:\n   ")

    while (NumberOfComp > maxNumberOfComp) {
      cat(ncharDiffCutoff, "")
      ncharDiffCutoff <- ncharDiffCutoff - 1
      ct_similar <-
        ct_similar %>% filter(abs(.data$ct1nchar - .data$ct2nchar) <= ncharDiffCutoff)
      NumberOfComp <- nrow(ct_similar)

      if (ncharDiffCutoff == 1) {
        stop("You must set a larger maxNumberOfComp!")
      }
    }

    cat("\n   clean_title comparisons with a character length difference >",
        ncharDiffCutoff, "are ignored.\n")
  }

  cat("clean_title comparisons =", myf(NumberOfComp), "<", myf(maxNumberOfComp), "= maxNumberOfComp\n",
      "  calculating similarity...\n")

  tictoc::tic("   calculating similarity complete") # TO DO: Progress bar without losing efficiency?
  ct_similar <- ct_similar %>%
    mutate(
      similarity = 1 -
        stringdist::stringdist(.data$ct1, .data$ct2, method = "lv") /
        pmax(.data$ct1nchar, .data$ct2nchar)
      # RecordLinkage::levenshteinSim(str1 = .data$ct1, str2 = .data$ct2), # slower alternative
    )
  tictoc::toc()


  # create clean_title_similarity -------------------------------------------
  ct_similar <- ct_similar %>%                    # similarity per pair
    filter(.data$similarity >= minSimilarity) %>% # keep only those with a minimum similarity
    arrange(desc(.data$similarity)) %>%           # highest similarity ...
    mutate(similarityRank = 1:n()) %>%            # ... gets best rank
    mutate(pot_dup_id = paste0(                   # "potdup_001 (99.9% similarity)" etc.
      "potdup_", stringr::str_pad(.data$similarityRank, pot_dup_id_padding, pad = "0"),
      " (", scales::percent(.data$similarity, accuracy = 0.1), " similarity)"
    )) %>%
    # columns served their purpose
    select(-.data$similarity,
           -.data$similarityRank,
           -.data$ct1nchar,
           -.data$ct2nchar) %>%
    # convert wide: [ct1 ct2 pot_dup_id] to long: [ct pot_dup_id]
    tidyr::pivot_longer(
      cols = .data$ct1:.data$ct2,
      values_to = "clean_title",
      names_to = NULL
    ) %>%
    # in case there are multiple pot_dup_id per clean_title: collapse
    group_by(.data$clean_title) %>%
    mutate(pot_dup_id = paste(stats::na.omit(.data$pot_dup_id), collapse =
                                "; ")) %>%
    ungroup() %>%
    unique()


  # join with CitDat --------------------------------------------------------
  CitDat <-
    left_join(x = CitDat, y = ct_similar, by = "clean_title") %>%
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
