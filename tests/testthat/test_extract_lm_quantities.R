context("extract_lm_quantities() checks")

x <- runif(10)
y <- 3 * x + 7 + rnorm(10)
example_lm1 <- lm(y ~ x)

test_that("extract_lm_quantities() correctly throws errors for bad input", {
  
  expect_error(
    extract_lm_quantities(example_lm1, "alpha")
    )
  
  expect_error(
    extract_lm_quantities(y~x, "alpha")
  )
  
})

test_that("extract_lm_quantities() is a list", {
  
  expect_output(str(extract_lm_quantities(example_lm1)), "List of 5")
  
  expect_output(str(extract_lm_quantities(example_lm1)), "List of 5", fixed=T)
  
})



