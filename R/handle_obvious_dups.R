#' @title Handle obvious duplicates
#'
#' @description
#' `r lifecycle::badge("experimental")`
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
#' read_Citavi_xlsx(path) %>%
#'    find_obvious_dups() %>%
#'    handle_obvious_dups(fieldsToHandle = c("Online address", "PubMed ID"))
#'
#' @return A tibble where information from obvious duplicates was brought together for \code{dup_01}, respectively.
#' @importFrom tidyr fill
#' @import dplyr
#' @export

handle_obvious_dups <- function(CitDat, fieldsToHandle = NULL, nameDupCategories = NA_character_, nameDupGroups = NA_character_, nameDupKeywords = NA_character_) { # TO DO: better name?

  # stop if nothing to be handled -------------------------------------------
  if (is.null(fieldsToHandle) & all(is.na(c(nameDupCategories, nameDupGroups, nameDupKeywords)))) {
    stop("At least one of 'fieldsToHandle', 'nameDupCategories', 'nameDupGroups' or 'nameDupKeywords' must not be NULL/NA.")
  }

  # handle fields -----------------------------------------------------------
  if (is.character(fieldsToHandle)) {
    if (any(fieldsToHandle %not_in% names(CitDat))) {
      stop("At least one of the 'fieldsToHandle' you gave is missing in the dataset.")
    }

    CitDat <- CitDat %>%
      group_by(.data$clean_title) %>%
      tidyr::fill(fieldsToHandle, .direction = "up") %>% # TO DO: more sophisticated. What if multiple entries?
      ungroup()

  }


  # handle categories/groups/keywords ---------------------------------------
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

        # collapse unique categories/groups/keywords per clean_title_id
        CitDat <- CitDat %>%
          group_by(.data$clean_title_id) %>%
          mutate_at(
            .vars = vars(CatGroKey_i),
            .funs = ~ if_else(.data$has_obv_dup == TRUE,
                              paste(unique(.), collapse = "; "),
                              .)
          ) %>%
          ungroup()

        # for duplicates: overwrite collapsed categories/groups/keywords with nameDup
        CitDat <- CitDat %>%
          mutate_at(
            .vars = vars(CatGroKey_i),
            .funs = ~ if_else(
              .data$has_obv_dup == TRUE & .data$obv_dup_id != "dup_01",
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
