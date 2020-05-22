context("predict_delta_comps() using comparisons = 'one-v-one' option checks")


data(fairclough)


# A big thank you to Dr Naruki Kitano for his output to perform this check
truth_df <-
read.table(header = FALSE, fill = TRUE, stringsAsFactors = FALSE, text = "
sleep   sed   0.051824 -0.027833  0.131480    
sleep   lpa  -0.014592 -0.106113  0.076928    
sleep  mvpa   0.920707  0.541286  1.300129   *
  sed sleep  -0.051815 -0.131805  0.028175    
  sed   lpa  -0.065677 -0.126638 -0.004716   *
  sed  mvpa   0.869622  0.509535  1.229710   *
  lpa sleep   0.012169 -0.078545  0.102883    
  lpa   sed   0.064723  0.005043  0.124404   *
  lpa  mvpa   0.933607  0.535037  1.332176   *
 mvpa sleep  -0.507611 -0.718621 -0.296600   *
 mvpa   sed  -0.455056 -0.644928 -0.265185   *
 mvpa   lpa  -0.521472 -0.751990 -0.290955   *
")

colnames(truth_df) <- c("comp+", "comp-", "delta_pred", "ci_lo", "ci_up", "sig")
truth_df <- truth_df[order(truth_df[["comp+"]], truth_df[["comp-"]]), ]
truth_df


# keeping rows 1 - 166 was used in testing for whatever reason
fairclough <- fairclough[1:166, ]
# tail(fairclough)

deltacomp_df <-
  predict_delta_comps(
    dataf = fairclough,
    y = "z_bmi",
    comps = c("sleep","sed","lpa","mvpa"),
    covars = c("decimal_age","sex"),
    deltas = 15 / (24 * 60), # just 15 min intervals
    comparisons = "one-v-one",
    alpha = 0.05,
    verbose = FALSE
  )

# remove cols not tested
deltacomp_df <- deltacomp_df[, !(colnames(deltacomp_df) %in% c("alpha", "delta"))]
deltacomp_df <- deltacomp_df[order(deltacomp_df[["comp+"]], deltacomp_df[["comp-"]]), ]
deltacomp_df

test_that("predict_delta_comps() correctly returns 'one-vs-one' data.frame on fairclough data", {
  
  expect_equal(colnames(truth_df), colnames(deltacomp_df))
  expect_equal(truth_df[["comp+"]], deltacomp_df[["comp+"]])
  expect_equal(truth_df[["comp-"]], deltacomp_df[["comp-"]])
  expect_equal(truth_df[["sig"]], deltacomp_df[["sig"]])
  expect_equal(truth_df[["delta_pred"]], deltacomp_df[["delta_pred"]], tolerance = .00001, scale = 1)
  expect_equal(truth_df[["ci_lo"]], deltacomp_df[["ci_lo"]], tolerance = .00001, scale = 1)
  expect_equal(truth_df[["ci_up"]], deltacomp_df[["ci_up"]], tolerance = .00001, scale = 1)
  
})

