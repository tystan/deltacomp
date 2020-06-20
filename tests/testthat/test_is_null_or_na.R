context("is_null_or_na() checks")


test_df <- data.frame(a = 1:4, `b` = LETTERS[5:8], Z = rep(TRUE, 4))

test_that("is_null_or_na() correctly returns FALSE", {
  
  expect_equal(is_null_or_na(test_df), FALSE)
  expect_equal(is_null_or_na(c("b", "Z")), FALSE)
  expect_equal(is_null_or_na(character(1)), FALSE)
  expect_equal(is_null_or_na(logical(1)), FALSE)
  expect_equal(is_null_or_na(c("b", "Z", character(0))), FALSE)
  
})

test_that("is_null_or_na() correctly returns TRUE", {
  
  expect_equal(is_null_or_na(NA), TRUE)
  expect_equal(is_null_or_na(NULL), TRUE)
  expect_equal(is_null_or_na(character(0)), TRUE)
  expect_equal(is_null_or_na(logical(0)), TRUE)
  expect_equal(is_null_or_na(c("b", "Z", NA)), TRUE)
  
})
