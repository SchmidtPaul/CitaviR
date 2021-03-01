path_3dupsin5refs <- example_xlsx("3dupsin5refs.xlsx")

test_that("error if no path provided", {
  expect_error(write_Citavi_xlsx())
})

test_that("error if two paths provided", {
  expect_error(write_Citavi_xlsx(
    read_path = path_3dupsin5refs,
    write_path = path_3dupsin5refs
  ))
})

test_that("xlsx file actually created", {

  last_dot  <- regexpr("\\.[^\\.]*$", path_3dupsin5refs)[1]
  temp_path <- substr(path_3dupsin5refs, 1, last_dot - 1)
  temp_path <- paste0(temp_path, "_R_out.xlsx")

  if (file.exists(temp_path)) {file.remove(temp_path)}

  read_Citavi_xlsx(path = path_3dupsin5refs) %>%
    write_Citavi_xlsx(read_path = path_3dupsin5refs)

  expect_true(file.exists(temp_path))

  if (file.exists(temp_path)) {file.remove(temp_path)}

})


