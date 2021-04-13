#' @title Identify potential duplicates based on title and year
#'
#' @param CitDat A dataframe/tibble returned by \code{\link[CitaviR]{find_obvious_dups}}
#' or \code{\link[CitaviR]{handle_obvious_dups}}.
#' @param minSimilarity Minimum similarity (between 0 and 1). Default is 0.6. (TO DO)
#' @param potDupAfterObvDup If TRUE (default), the newly created column
#' \code{pot_dup_id} is moved right next to the \code{obv_dup_id} column.
#' @param maxNumberOfComp Maximum number of clean_title similarity calculations to be made.
#' It is set to 1,000,000 by default (which corresponds to ~ 1414 clean_titles). TO DO: Document while-loop.
#' @param quiet If \code{TRUE}, all output will be suppressed.
#'
#' @details
#' `r lifecycle::badge("maturing")` \cr
#' Currently this only works for files that were generated while Citavi
#' was set to "English" so that column names are "Short Title" etc.
#'
#' @examples
#' example_path <- example_file("3dupsin5refs/3dupsin5refs.ctv6")
#' CitDat <- read_Citavi_ctv6(example_path) %>%
#'    find_obvious_dups() %>%
#'    find_potential_dups()
#'
#' CitDat %>%
#'    dplyr::select(clean_title_id, obv_dup_id, pot_dup_id)
#'
#' # check similarity yourself - it's a single typo:
#' CitDat %>%
#'    dplyr::select(clean_title)
#'
#' @return A tibble containing one new column: \code{pot_dup_id}.
#'
#' @importFrom RcppAlgos comboGeneral
#' @importFrom scales percent
#' @importFrom stats na.omit
#' @importFrom stringdist stringdist
#' @importFrom stringr str_length
#' @importFrom stringr str_pad
#' @importFrom tidyr pivot_longer
#' @import crayon
#' @import dplyr
#'
#' @export

find_potential_dups <- function(CitDat, minSimilarity = 0.6, potDupAfterObvDup = TRUE, maxNumberOfComp = 1000000, quiet = FALSE) {

  bignum <- function(x){format(x, digits = 0, big.mark = ",", scientific = FALSE)}


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

    # Msg: Too many comparisons
    if (!quiet) {
      cat(
        blue("clean_title comparisons ="),
        bignum(NumberOfComp),
        red(">"),
        bignum(maxNumberOfComp),
        blue(
          "= maxNumberOfComp\n",
          "  Trying to ignore comparisons with large differences in character length:"
        ),
        "\n   "
      )
    }

    # Reduce until not too many comparisons
    while (NumberOfComp > maxNumberOfComp) {
      if (!quiet) {
        cat(blue(ncharDiffCutoff, ""))
      }
      ncharDiffCutoff <- ncharDiffCutoff - 1
      ct_similar <-
        ct_similar %>% filter(abs(.data$ct1nchar - .data$ct2nchar) <= ncharDiffCutoff)
      NumberOfComp <- nrow(ct_similar)

      if (ncharDiffCutoff == 1) {
        stop("You must set a larger maxNumberOfComp!")
      }
    }

    # Msg: Suitable reduction
    if (!quiet) {
      cat(
        "\n",
        blue(
          "   clean_title comparisons with a character length difference >",
          ncharDiffCutoff,
          "are ignored."
        ),
        "\n"
      )
    }
  }

  # Msg: Not too many comparisons
  if (!quiet) {
    cat(
      blue("clean_title comparisons ="),
      bignum(NumberOfComp),
      green("<"),
      bignum(maxNumberOfComp),
      blue("= maxNumberOfComp"),
      "\n"
    )
  }


  # calculate similarity ----------------------------------------------------
  if (!quiet) {
    cat(blue("   calculating similarity now..."), "\n")
  }

  sta <- Sys.time() # TO DO: Progress bar without losing efficiency?
  ct_similar <- ct_similar %>%
    mutate(
      similarity = 1 -
        stringdist::stringdist(.data$ct1, .data$ct2, method = "lv") /
        pmax(.data$ct1nchar, .data$ct2nchar)
      # RecordLinkage::levenshteinSim(str1 = .data$ct1, str2 = .data$ct2), # slower alternative
    )
  end <- Sys.time()

  # Msg: Similarity calculation took this long
  if (!quiet) {
    cat(blue(
      "   calculating similarity done:",
      round(end - sta, 1),
      "sec elapsed"
    ),
    "\n")
  }

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
