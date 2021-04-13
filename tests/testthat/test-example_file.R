test_that("NULL path returns names of example files", {
  example_files <- example_file()
  expect_true("3dupsin5refs.xlsx" %in% example_files)
  expect_type(example_files, "character")
})

test_that("providing example file name returns full path", {
  expect_true(file.exists(example_file("3dupsin5refs.xlsx")))
})
