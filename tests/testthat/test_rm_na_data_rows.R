context("rm_na_data_rows() checks")


data(fairclough)
fa_y <- "z_bmi"
fa_comps <- c("sleep", "sed", "lpa", "mvpa")
fa_covars <- c("decimal_age", "sex")
fa_deltas <- 15 / (24 * 60)
fa_comparisons <- "one-v-one"
fa_alpha <- 0.05

test_that("rm_na_data_rows() correctly leaves input", {
  
  expect_equal(
    nrow(rm_na_data_rows(dataf = fairclough, c(fa_y, fa_comps, fa_covars))),
    nrow(fairclough)
  )
  
  expect_equal(
    ncol(rm_na_data_rows(dataf = fairclough, c(fa_y, fa_comps, fa_covars))),
    length(c(fa_y, fa_comps, fa_covars))
  )
  
})

f1 <- f2 <- f3 <- fairclough[1:10, ]
f1$lpa[c(5, 10, 6, 2)] <- NA
f2$lpa <- NA
f3$sex[1:3] <- NA


test_that("rm_na_data_rows() correctly removes NAs", {
  
  expect_equal(
    rm_na_data_rows(dataf = f1, c(fa_y, fa_comps, fa_covars)),
    f1[-c(5, 10, 6, 2), c(fa_y, fa_comps, fa_covars)]
  )
  expect_error(
    predict_delta_comps(dataf = f1, fa_y, fa_comps, fa_covars, fa_deltas, fa_comparisons, fa_alpha),
    "The number of rows in dataf"
  )
  
  expect_equal(
    rm_na_data_rows(dataf = f2, c(fa_y, fa_comps, fa_covars)),
    f2[0,  c(fa_y, fa_comps, fa_covars)]
  )  
  expect_error(
    predict_delta_comps(dataf = f2, fa_y, fa_comps, fa_covars, fa_deltas, fa_comparisons, fa_alpha),
    "dataf supplied must"
  )
  
  expect_equal(
    rm_na_data_rows(dataf = f3, c(fa_y, fa_comps, fa_covars)),
    f3[-(1:3),  c(fa_y, fa_comps, fa_covars)]
  )  
  
})

