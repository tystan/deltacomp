context("check_input_args() catches bad input checks")


data(fairclough)
fa_y <- "z_bmi"
fa_comps <- c("sleep", "sed", "lpa", "mvpa")
fa_covars <- c("decimal_age", "sex")
fa_deltas <- 15 / (24 * 60)
fa_comparisons <- "one-v-one"
fa_alpha <- 0.05

test_that("predict_delta_comps() correctly throws errors via check_input_args() for bad input", {
  
  expect_error(
    predict_delta_comps(dataf = NULL, fa_y, fa_comps, fa_covars, fa_deltas, fa_comparisons, fa_alpha),
    "dataf supplied must be a data.frame"
  )
  expect_error(
    predict_delta_comps(dataf = c(1, NA), fa_y, fa_comps, fa_covars, fa_deltas, fa_comparisons, fa_alpha),
    "dataf supplied must be a data.frame"
  )
  
  expect_error(
    predict_delta_comps(dataf = fairclough[0, ], fa_y, fa_comps, fa_covars, fa_deltas, fa_comparisons, fa_alpha),
    "dataf supplied must be a non-empty data.frame"
  )  
  
  expect_error(
    predict_delta_comps(dataf = fairclough, c(1, NA), fa_comps, fa_covars, fa_deltas, fa_comparisons, fa_alpha),
    "Please supply a character string for the outcome column in dataf"
  )
  expect_error(
    predict_delta_comps(dataf = fairclough, NULL, fa_comps, fa_covars, fa_deltas, fa_comparisons, fa_alpha),
    "Please supply a character string for the outcome column in dataf"
  )
  expect_error(
    predict_delta_comps(dataf = fairclough, 2, fa_comps, fa_covars, fa_deltas, fa_comparisons, fa_alpha),
    "Please supply a character string for the outcome column in dataf"
  )
  
  expect_error(
    predict_delta_comps(dataf = fairclough, fa_y, NA, fa_covars, fa_deltas, fa_comparisons, fa_alpha),
    "The provided compositional column names must be a non-empty vector"
  )
  expect_error(
    predict_delta_comps(dataf = fairclough, fa_y, numeric(0), fa_covars, fa_deltas, fa_comparisons, fa_alpha),
    "The provided compositional column names must be a non-empty vector"
  )
  expect_error(
    predict_delta_comps(dataf = fairclough, fa_y, NULL, fa_covars, fa_deltas, fa_comparisons, fa_alpha),
    "The provided compositional column names must be a non-empty vector"
  )
  
  expect_error(
    predict_delta_comps(dataf = fairclough, fa_y, 1, fa_covars, fa_deltas, fa_comparisons, fa_alpha),
    "Please supply a character string"
  )
  
  expect_error(
    predict_delta_comps(dataf = fairclough, fa_y, "sed", fa_covars, fa_deltas, fa_comparisons, fa_alpha),
    "At least two compositional components"
  )
  
  expect_error(
    predict_delta_comps(dataf = fairclough, fa_y, fa_comps, 1, fa_deltas, fa_comparisons, fa_alpha),
    "Please supply a character string of the covariate"
  )
  expect_error(
    predict_delta_comps(dataf = fairclough, fa_y, fa_comps, TRUE, fa_deltas, fa_comparisons, fa_alpha),
    "Please supply a character string of the covariate"
  )
  
  expect_error(
    predict_delta_comps(dataf = fairclough, fa_y, fa_comps, fa_covars, -1.1, fa_comparisons, fa_alpha),
    "deltas must be specified as positive and negative proportions"
  )
  expect_error(
    predict_delta_comps(dataf = fairclough, fa_y, fa_comps, fa_covars, +1.1, fa_comparisons, fa_alpha),
    "deltas must be specified as positive and negative proportions"
  )
  
  expect_error(
    predict_delta_comps(dataf = fairclough[1:6, ], fa_y, fa_comps, fa_covars, fa_deltas, fa_comparisons, fa_alpha),
    "The number of rows.*"
  )

  
})

