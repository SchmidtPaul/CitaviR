#' @title Detect the language that the abstract or other fields are written in
#'
#' @param CitDat A dataframe/tibble possibly returned by \code{\link[CitaviR]{read_Citavi_xlsx}}.
#' @param fieldsToDetectIn Character vector with names of fields whose text
#' language should be detected. Default is \code{c("Abstract")}. When multiple fields are given
#' (e.g. \code{c("Abstract", "Title")}), they are combined into a single string whose language
#' is then detected.
#' @param wantedLanguage Character vector with names of languages that are desired. Default is
#' \code{c("english")}. If not set to \code{NULL}, a new column \code{det_lang_wanted}
#' is created, which is \code{TRUE} if the detected language in \code{det_lang} is a wanted
#' language.
#'
#' @details
#' `r lifecycle::badge("experimental")` \cr
#' Currently this only works for files that were generated while Citavi
#' was set to "English" so that column names are "Short Title" etc.
#'
#' @examples
#' path <- example_xlsx("3dupsin5refs.xlsx")
#' read_Citavi_xlsx(path) %>%
#'    detect_language() %>%
#'    dplyr::select(Title, Abstract, det_lang, det_lang_wanted)
#'
#' @return A tibble containing at least one additional column: \code{det_lang}.
#' @importFrom textcat textcat
#' @importFrom tidyr unite
#' @import dplyr
#' @export
#'
detect_language <- function (CitDat, fieldsToDetectIn = c("Abstract"), wantedLanguage = c("english")) {


  # collapse fieldsToDetectIn -----------------------------------------------
  CitDat <- CitDat %>%
    tidyr::unite(
      fieldsToDetectIn,
      col = "StringToDetect",
      sep = "; ",
      remove = FALSE,
      na.rm = TRUE
    ) %>%
    mutate(StringToDetect = if_else(.data$StringToDetect == "", NA_character_, .data$StringToDetect))


  # det_lang ----------------------------------------------------------------
  CitDat <- CitDat %>%
    mutate(det_lang = textcat::textcat(.data$StringToDetect)) %>%
    select(-.data$StringToDetect)


  # det_lang_wanted ---------------------------------------------------------
  if (!is.null(wantedLanguage)) {
    CitDat <- CitDat %>%
      mutate(det_lang_wanted = if_else(is.na(.data$det_lang),
                                       NA,
                                       .data$det_lang %in% wantedLanguage))
  }


  # return tibble -----------------------------------------------------------
  CitDat
}
