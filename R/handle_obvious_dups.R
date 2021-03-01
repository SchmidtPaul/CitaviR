#' @title Handle obvious duplicates
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Currently this only works for files that were generated while Citavi
#' was set to "English" so that column names are "Short Title" etc.
#'
#' @param CitDat A tibble returned by \code{\link[CitaviR]{find_obvious_dups}}.
#' @param fieldsToHandle A character vector with all column/field names that should be handled.
#' Note that this does not include "Categories", "Groups" and "Keywords".
#' @param nameDupCategories Name that "Categories" of obvious duplicates should be set to. See details below.
#' @param nameDupGroups Name that "Groups" of obvious duplicates should be set to. See details below.
#' @param nameDupKeywords Name that "Keywords" of obvious duplicates should be set to. See details below.
#'
#' @details
#' \code{nameDupCategories}, \code{nameDupGroups} and \code{nameDupKeywords} are all \code{NA_character_}
#' by default. If a character string is provided for one of them, the respective column
#' (i.e. Categories, Groups or Keywords) is handled. This means that whenever obvious duplicates are present,
#' all unique entries are collapsed into \code{dup_01}, while \code{dup_02}, \code{dup_03} etc. are set
#' to the provided character string.
#'
#' @examples
#' path <- example_xlsx("3dupsin5refs.xlsx")
#' CitDat <- read_Citavi_xlsx(path) %>%
#'    find_obvious_dups()
#'
#' # before
#' CitDat %>%
#'    dplyr::select(clean_title, clean_title_id, obv_dup_id, "DOI name", "PubMed ID")
#'
#' # after
#' CitDat %>%
#'    handle_obvious_dups(fieldsToHandle = c("DOI name", "PubMed ID")) %>%
#'    dplyr::select(clean_title, clean_title_id, obv_dup_id, "DOI name", "PubMed ID")
#'
#' @return A tibble where information from obvious duplicates was brought together for \code{dup_01}, respectively.
#' @importFrom purrr map
#' @importFrom stringr str_detect
#' @importFrom stringr str_split
#' @importFrom tidyr fill
#' @importFrom tidyr separate_rows
#' @importFrom tidyr unite
#' @import dplyr
#' @export

handle_obvious_dups <- function(CitDat, fieldsToHandle = NULL, nameDupCategories = NA_character_, nameDupGroups = NA_character_, nameDupKeywords = NA_character_) { # TO DO: better name?

  # stop if nothing to be handled -------------------------------------------
  if (is.null(all_of(fieldsToHandle)) & all(is.na(c(nameDupCategories, nameDupGroups, nameDupKeywords)))) {
    stop("At least one of 'fieldsToHandle', 'nameDupCategories', 'nameDupGroups' or 'nameDupKeywords' must not be NULL/NA.")
  }


  # fields ------------------------------------------------------------------
  if (is.character(all_of(fieldsToHandle))) {
    if (any(all_of(fieldsToHandle) %not_in% names(CitDat))) {
      stop("At least one of the 'fieldsToHandle' you gave is missing in the dataset.")
    }


    # fields - online address -------------------------------------------------
    if ("Online address" %in% fieldsToHandle) {

      # combine "Online address" and "Locations" column
      URLs <- CitDat %>%
        group_by(.data$clean_title_id) %>%
        summarise_at(
          .vars = vars(.data$`Online address`, .data$Locations),
          .funs = function(x)
            paste(x[!is.na(x)], collapse = "; ")
        ) %>%
        ungroup() %>%
        tidyr::unite(
          col = "URL",
          .data$`Online address`,
          .data$Locations,
          na.rm = TRUE,
          sep = "; "
        )

      # separate and rank all URLs
      URLs <- URLs %>%
        tidyr::separate_rows(.data$URL, sep = "; ") %>%
        filter(stringr::str_detect(.data$URL, "http:|https:")) %>%
        mutate(
          prefer_pts = case_when(
            stringr::str_detect(.data$URL, "doi.org")              ~ 100, # best choice
            stringr::str_detect(.data$URL, "onlinelibrary.wiley")  ~  50,
            stringr::str_detect(.data$URL, "ncbi.nlm.nih.gov")     ~  -1,
            stringr::str_detect(.data$URL, "epistemonikos.org")    ~ -90,
            stringr::str_detect(.data$URL, "search.ebscohost")     ~ -91,
            stringr::str_detect(.data$URL, "scopus")               ~ -92,
            stringr::str_detect(.data$URL, "search.ebscohost")     ~ -93,
            TRUE ~ 0
          )
        )

      # per clean_title_id: keep only first/best URL
      URLs <- URLs %>%
        arrange(.data$clean_title_id, desc(.data$prefer_pts)) %>%
        group_by(.data$clean_title_id) %>%
        slice(1) %>%
        ungroup() %>%
        mutate(obv_dup_id = nth(sort(CitDat$obv_dup_id), 1)) %>%
        select(-.data$prefer_pts)

      # replace `Online address` with best URL for dup_01
      CitDat <- left_join(
        x = CitDat,
        y = URLs,
        by = c("clean_title_id", "obv_dup_id")
      ) %>%
        mutate(`Online address` = .data$URL, .keep = "unused")

    }


    # fields - all else -------------------------------------------------------
    fieldsToHandle <- fieldsToHandle[fieldsToHandle %not_in% "Online address"]

    CitDat <- CitDat %>%
      group_by(.data$clean_title) %>%
      tidyr::fill(all_of(fieldsToHandle), .direction = "up") %>% # TO DO: What if multiple entries? Do we care?
      ungroup()

  }


  # categories/groups/keywords ----------------------------------------------
  CatGroKey <- data.frame(
    collapse = rep(FALSE, 3),
    nameDup  = c(nameDupCategories, nameDupGroups, nameDupKeywords)
  )
  row.names(CatGroKey) <- c("Categories", "Groups", "Keywords")

  for (CatGroKey_i in row.names(CatGroKey)) {
    if (!is.na(CatGroKey[CatGroKey_i, "nameDup"])) {
      if (CatGroKey_i %not_in% names(CitDat)) {
        stop(paste0("There is no column named '", CatGroKey_i , "' to handle."))
      } else {

        # collapse sorted unique categories/groups/keywords per clean_title_id
        CitDat <- CitDat %>%
          group_by(.data$clean_title_id) %>%
          mutate_at(
            .vars = vars(all_of(CatGroKey_i)),
            .funs = ~ if_else(.data$has_obv_dup == TRUE,
                              paste(unique(.), collapse = "; "),
                              .)
          ) %>%
          ungroup() %>%
          mutate_at(
            .vars = vars(all_of(CatGroKey_i)),
            .funs = function(x) {
              x %>%
                # split into elements
                stringr::str_split(pattern = "; ") %>%
                # collapse sorted and unique elements back togehter
                purrr::map(
                  .f = function(y) {
                    y[!is.na(y)] %>%
                      unique %>% sort %>% paste(collapse = "; ")
                  }
                ) %>% unlist %>%
                # TO DO: Is this necessary? (= remove "; "at beginning / empty first entry )
                gsub(pattern = "^\\; ", replacement = "")
            }
          )

        # for duplicates: overwrite collapsed categories/groups/keywords with nameDup
        CitDat <- CitDat %>%
          mutate_at(
            .vars = vars(all_of(CatGroKey_i)),
            .funs = ~ if_else(
              .data$has_obv_dup == TRUE &
                as.integer(gsub("dup_","",.data$obv_dup_id)) > 1, # not dup_01 or dup_001 or dup_0001 ...
              paste(CatGroKey[CatGroKey_i, "nameDup"]),
              .
            )
          )

      }
    }
  }

  # return tibble -----------------------------------------------------------
  CitDat

}
