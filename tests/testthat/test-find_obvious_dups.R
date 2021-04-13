# no warning or message ---------------------------------------------------
test_that("no warning or message", {
  expect_silent( temp <- example_file('3dupsin5refs.xlsx') %>% read_Citavi_xlsx() %>% find_obvious_dups() )
})


CitDat_before <- example_file('3dupsin5refs.xlsx') %>% read_Citavi_xlsx()
CitDat_after  <- CitDat_before %>% find_obvious_dups()


# adds 4 columns ----------------------------------------------------------
test_that("adds 4 columns", {
  expect_equal( ncol(CitDat_after) - 4, ncol(CitDat_before) )
  })



# no NAs in new columns ---------------------------------------------------
test_that("no NAs in new columns", {
  expect_false(any(
    is.na(CitDat_after$clean_title),
    is.na(CitDat_after$clean_title_id),
    is.na(CitDat_after$has_obv_dup),
    is.na(CitDat_after$obv_dup_id)
  ))
})


# if obv_dup_id!='dup_01' then has_obv_dup==TRUE --------------------------
test_that("if obv_dup_id!='dup_01' then has_obv_dup==TRUE", {
  expect_true(all(
    CitDat_after %>%
      dplyr::filter(obv_dup_id != "dup_01") %>%
      pull(has_obv_dup)
  ))
})


# At least one of ID, Title or Year is missing ----------------------------
test_that("At least one of ID, Title or Year is missing", {
  expect_error(
    CitDat_before[, "ID"] %>% find_obvious_dups()
  )
})


# prefers "has_attachment" when no "Locations" ----------------------------
CitDat_after2 <- CitDat_after %>%
  mutate(has_attachment = if_else(obv_dup_id == "dup_01",
                                  FALSE,
                                  TRUE)) %>%
  rename(obv_dup_id_old = obv_dup_id) %>%
  select(-Locations) %>%
  find_obvious_dups() %>%
  select(has_attachment, obv_dup_id_old, everything())

test_that('prefers "has_attachment" when no "Locations"', {
  expect_true(CitDat_after2 %>% filter(obv_dup_id_old == "dup_02") %>% pull(has_attachment))
  expect_false(all(
    CitDat_after2 %>% filter(obv_dup_id_old == "dup_01") %>% pull(has_attachment)
  ))
})

