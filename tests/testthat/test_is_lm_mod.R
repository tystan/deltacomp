context("is_lm_mod() checks")

library(stats)

set.seed(1234)
x <- runif(10)
y <- 3 * x + 7 + rnorm(10)

example_lm1 <- lm(y ~ x)

# not the usual use of lm() but returns an object with one class "lm"
example_lm2 <- aov(y ~ x)
# class(example_lm2)

# taken from ?glm
## Dobson (1990) Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
d.AD <- data.frame(treatment, outcome, counts)
example_lm3 <- glm(counts ~ outcome + treatment, family = poisson())
# class(example_lm3)

# from ?nls (non-linear least squares)
## using a selfStart model
DNase1 <- subset(DNase, Run == 1)
example_non_lm <- nls(density ~ SSlogis(log(conc), Asym, xmid, scal), DNase1)
summary(example_non_lm)
# class(example_non_lm)


test_that("is_lm_mod() correctly returns TRUE", {
  
  expect_equal(is_lm_mod(example_lm1), TRUE)
  expect_equal(is_lm_mod(example_lm2), TRUE)
  expect_equal(is_lm_mod(example_lm3), TRUE)
  
})

test_that("is_lm_mod() correctly returns FALSE", {
  
  expect_equal(is_lm_mod(as.formula(y ~ x)), FALSE)
  expect_equal(   is_lm_mod(example_non_lm), FALSE)
  expect_equal(             is_lm_mod(NULL), FALSE)
  expect_equal(          is_lm_mod(3145345), FALSE)
  expect_equal(            is_lm_mod("abc"), FALSE)
  expect_equal(               is_lm_mod(NA), FALSE)
  
})
