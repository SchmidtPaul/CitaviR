test_that("missing path gives error", {
  expect_error(read_Citavi_xlsx())

})


# Tests based on 3dupsin5refs ---------------------------------------------
path <- example_xlsx('3dupsin5refs.xlsx')

test_that("basic read_xlsx [for 3dupsin5refs]", {
  raw  <- suppressMessages(readxl::read_xlsx(path))
  dat  <- read_Citavi_xlsx(
    path,
    keepMarksCols = FALSE,
    useYearDerived = FALSE,
    setSuggestedColOrder = FALSE
  )

  expect_equal(dat, raw[,-c(1:3)])
  expect_true(is.data.frame(dat))
  expect_true(nrow(dat) > 0)
  expect_true(ncol(dat) > 0)

})

test_that("keepMarksCols [for 3dupsin5refs]", {
  raw  <- suppressMessages(readxl::read_xlsx(path))
  dat_FALSE <- read_Citavi_xlsx(path, keepMarksCols = FALSE)
  dat_TRUE  <- read_Citavi_xlsx(path, keepMarksCols = TRUE)

  raw_names <- c("...1", "...2", "...3")
  new_names <- c("has_attachment", "red_flag", "blue_circle")

  expect_true(all(raw_names %in% names(raw)))
  expect_false(any(new_names %in% names(raw)))

  expect_false(any(raw_names %in% names(dat_FALSE)))
  expect_false(any(new_names %in% names(dat_FALSE)))

  expect_false(any(raw_names %in% names(dat_TRUE)))
  expect_true(all(new_names %in% names(dat_TRUE)))

  expect_true(ncol(raw)      - 4 == ncol(dat_FALSE))
  expect_true(ncol(raw)      - 1 == ncol(dat_TRUE))
  expect_true(ncol(dat_TRUE) - 3 == ncol(dat_FALSE))

})

test_that("useYearDerived [for 3dupsin5refs]", {
  raw  <- suppressMessages(readxl::read_xlsx(path))
  dat_FALSE <- read_Citavi_xlsx(path, useYearDerived = FALSE)
  dat_TRUE  <- read_Citavi_xlsx(path, useYearDerived = TRUE)

  expect_true("Year" %in% names(raw))
  expect_true("Year derived" %in% names(raw))

  expect_true("Year" %in% names(dat_FALSE))
  expect_true("Year derived" %in% names(dat_FALSE))

  expect_true("Year" %in% names(dat_TRUE))
  expect_false("Year derived" %in% names(dat_TRUE))

  expect_true(ncol(raw)           == ncol(dat_FALSE))
  expect_true(ncol(raw)       - 1 == ncol(dat_TRUE))
  expect_true(ncol(dat_FALSE) - 1 == ncol(dat_TRUE))

})

test_that("setSuggestedColOrder [for 3dupsin5refs]", {
  raw  <- suppressMessages(readxl::read_xlsx(path))
  dat_FALSE <- read_Citavi_xlsx(path, setSuggestedColOrder = FALSE)
  dat_TRUE  <- read_Citavi_xlsx(path, setSuggestedColOrder = TRUE)

  expect_true(ncol(dat_TRUE) == ncol(dat_FALSE))
  expect_true(all(names(dat_TRUE) %in% names(dat_FALSE)))
  expect_true(all(names(dat_FALSE) %in% names(dat_TRUE)))

  expect_false(all(names(dat_TRUE) == names(dat_FALSE)))

})
