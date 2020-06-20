context("check_strictly_positive_vals() checks")


data(fairclough)
fa_y <- "z_bmi"
fa_comps <- c("sleep", "sed", "lpa", "mvpa")
fa_covars <- c("decimal_age", "sex")
fa_deltas <- 15 / (24 * 60)
fa_comparisons <- "one-v-one"
fa_alpha <- 0.05

f1 <- fairclough
f1$lpa[5] <- 1e-4

test_that("check_strictly_positive_vals() correctly OKs good input", {
  
  expect_equal(
    check_strictly_positive_vals(dataf = fairclough, fa_comps),
    invisible(TRUE)
  )

  expect_equal(
    check_strictly_positive_vals(dataf = f1, fa_comps),
    invisible(TRUE)
  )
  
})

f2 <- f3 <- fairclough
f2$lpa[5] <- 1e-7
f3$sleep[100] <- -1


test_that("check_strictly_positive_vals() correctly throws errors for bad input", {
  
  expect_error(
    check_strictly_positive_vals(dataf = f2, fa_comps),
    "Values less than"
  )
  expect_error(
    predict_delta_comps(dataf = f2, fa_y, fa_comps, fa_covars, fa_deltas, fa_comparisons, fa_alpha),
    "Values less than"
  )
  
  
  expect_error(
    check_strictly_positive_vals(dataf = f3, fa_comps),
    "Values less than"
  )
  expect_error(
    predict_delta_comps(dataf = f3, fa_y, fa_comps, fa_covars, fa_deltas, fa_comparisons, fa_alpha),
    "Values less than"
  )
  
  
  
})

