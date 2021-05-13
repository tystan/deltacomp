context("create_seq_bin_part() checks")

test_that("create_seq_bin_part() states error if non-numerics or multiple values are used", {
  
  expect_error(create_seq_bin_part("b"))
  expect_error(create_seq_bin_part(c(2,3)))
  
})
