context("create_seq_bin_part() checks")

test_that("create_seq_bin_part() throws error if wrong inputs", {
  
  expect_error(create_seq_bin_part("b"))
  expect_error(create_seq_bin_part(c(2,3)))
  
})


