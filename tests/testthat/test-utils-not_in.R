test_that("is opposite of %in%", {
  a <- letters[1:3]
  b <- letters[2:4]

  expect_equal(a %in% b, ! a %not_in% b)
})
