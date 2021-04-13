test_that("not a CitDat object", {
  expect_error(mtcars %>% handle_obvious_dups(fieldsToHandle = "mpg"))
})

# Tests based on 3dupsin5refs ---------------------------------------------
CitDat_before <- example_file('3dupsin5refs.xlsx') %>% read_Citavi_xlsx() %>% find_obvious_dups()
CitDat_after  <- CitDat_before %>% handle_obvious_dups(fieldsToHandle = "PubMed ID")

test_that("nothing to handle", {
  expect_error(CitDat_before %>% handle_obvious_dups())
})

test_that("it filled up PubMed ID for dup_01", {
  expect_equal(
    CitDat_before %>% filter(clean_title_id == "ct_02") %>% pull("PubMed ID"),
    c(NA_character_, "31248886")
  )

  expect_equal(
    CitDat_after %>% filter(clean_title_id == "ct_02") %>% pull("PubMed ID"),
    c("31248886", "31248886")
  )

})
