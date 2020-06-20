context("cols_exist() checks")


test_df <- data.frame(a = 1:4, `b` = LETTERS[5:8], Z = rep(TRUE, 4))

test_that("cols_exist() correctly returns TRUE", {
  
  expect_equal(cols_exist(test_df, "a"), TRUE)
  expect_equal(cols_exist(test_df, c("b", "Z")), TRUE)
  
})

test_that("cols_exist() correctly returns errors", {
  
  expect_error(cols_exist(test_df, "A"))
  expect_error(cols_exist(test_df, c("b", "z")))
  expect_error(cols_exist(test_df, NA), "cols argument supplied to cols_exist\\(\\) is NA, NULL or length 0")
  expect_error(cols_exist(test_df, NULL), "cols argument supplied to cols_exist\\(\\) is NA, NULL or length 0")
  expect_error(cols_exist(test_df, character(0)), "cols argument supplied to cols_exist\\(\\) is NA, NULL or length 0")
  
})
