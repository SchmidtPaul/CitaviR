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
#' The underlying core function determining the language is \code{textcat::textcat()}.
#'
#' @examples
#' \dontrun{
#' CitDat <- CitaviR::diabetesprevalence %>%
#'   dplyr::slice(1952:1955, 4390:4393)
#'
#' CitDat %>%
#'   detect_language() %>%
#'   dplyr::select(Abstract, det_lang, det_lang_wanted)
#' }
#' @return A tibble containing at least one additional column: \code{det_lang}.
#' @importFrom textcat textcat
#' @importFrom tidyr unite
#' @import dplyr
#' @export
#'
detect_language <- function (CitDat, fieldsToDetectIn = c("Abstract"), wantedLanguage = c("english")) {

  # stop if empty arguments -------------------------------------------
  if (is.null(all_of(fieldsToDetectIn))) {
    stop("'fieldsToDetectIn' must not be NULL/NA.")
  }


  # check if fieldsToDetectIn are present -----------------------------------
  if (!all_of(fieldsToDetectIn %in% names(CitDat))) {
    stop(paste("Could not be found in dataset column names:\n",
               fieldsToDetectIn[fieldsToDetectIn %not_in% names(CitDat)]))
  }


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
