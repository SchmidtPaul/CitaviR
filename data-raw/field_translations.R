field_translations <- readxl::read_excel("data-raw/field translations.xlsx")

usethis::use_data(field_translations, overwrite = TRUE, internal = TRUE)
