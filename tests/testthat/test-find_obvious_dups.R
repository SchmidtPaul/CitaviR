test_that("no warning or message", {
  expect_silent( temp <- example_xlsx('3dupsin5refs.xlsx') %>% read_Citavi_xlsx() %>% find_obvious_dups() )
})


# Tests based on 3dupsin5refs ---------------------------------------------
CitDat_before <- example_xlsx('3dupsin5refs.xlsx') %>% read_Citavi_xlsx()
CitDat_after  <- CitDat_before %>% find_obvious_dups()

test_that("adds 4 columns", {
  expect_equal( ncol(CitDat_after) - 4, ncol(CitDat_before) )
  })

test_that("no NAs in new columns", {
  expect_false(any(
    is.na(CitDat_after$clean_title),
    is.na(CitDat_after$clean_title_id),
    is.na(CitDat_after$has_obv_dup),
    is.na(CitDat_after$obv_dup_id)
  ))
})

test_that("if obv_dup_id!='dup_01' then has_obv_dup==TRUE", {
  expect_true(all(
    CitDat_after %>%
      dplyr::filter(obv_dup_id != "dup_01") %>%
      pull(has_obv_dup)
  ))
})




