suppressWarnings(
  diabetesprevalence <-
    CitaviR::read_Citavi_xlsx(
      path = "data-raw/diabetesprevalence.xlsx",
      keepMarksCols = TRUE,
      useYearDerived = TRUE,
      setSuggestedColOrder = TRUE,
      setSuggestedColTypes = TRUE
    )
)

usethis::use_data(diabetesprevalence, overwrite = TRUE, internal = FALSE)
