context("predict_delta_comps() using comparisons = 'one-v-one' option checks")


# thank you to the author on the below website for this permutation funciton:
# https://blog.ephorie.de/learning-r-permutations-and-combinations-with-base-r
perm <- function(v) {
  n <- length(v)
  if (n == 1) v
  else {
    X <- NULL
    for (i in 1:n) X <- rbind(X, cbind(v[i], perm(v[-i])))
    X
  }
}




data(fairclough)
# tail(fairclough)

realloc_mthd <- "prop-realloc"
covar_nms <- c("decimal_age", "sex")
comp_nms <- c("sleep", "sed", "lpa", "mvpa")
D <- length(comp_nms)
outc_nm <- "z_bmi"

# a matrix of 4! rows and 4 columns of all 1:4 permutations (row-wise)
comp_perms <- perm(1:D)
np <- nrow(comp_perms)
predictions_df <- data.frame(analytical = numeric(np), numerical = numeric(np))

delta1 <- 15 / (24 * 60)

for (i in 1:np) {

  # we will cycle through all permutations of the components specified in the model
  this_comp_order <- comp_nms[comp_perms[i, ]]
  
  deltacomp_df <-
    predict_delta_comps(
      dataf = fairclough,
      y = outc_nm, 
      comps = this_comp_order, 
      covars = covar_nms,
      deltas = delta1, 
      comparisons = realloc_mthd
    )
  
  
  numeric_y_delta <- deltacomp_df[deltacomp_df$`comp+` == this_comp_order[1], "delta_pred"]
  predictions_df$numerical[i] <- numeric_y_delta
  #str(deltacomp_df)
  
  
  # how to get the arithmetic mean below but we want geometric simplex mean
  # x1 <- mean(fair_data_ilrs[, this_comp_order[1]]) 
  # geometric/simplex mean:
  x1 <- attr(deltacomp_df, "mean_pred")[1, this_comp_order[1]]
  r <- delta1 / x1
  (x1 + delta1) / x1 == 1 + r
  s <- r * x1 / (1 - x1)
  
  fair_data_ilrs <- attr(deltacomp_df, "dataf")
  head(fair_data_ilrs)
  fair_data_ilrs <- fair_data_ilrs[, !(colnames(fair_data_ilrs) %in% this_comp_order)]
  fair_lm <- lm(as.formula(paste(outc_nm, "~ .")), data = fair_data_ilrs)
  
  b1 <- coefficients(fair_lm)["ilr1"]
  names(b1) <- NULL # remove vector names
  analytical_y_delta <- b1 * log((1 + r) / (1 - s)) * sqrt((D - 1) / D)
  
  predictions_df$analytical[i] <- analytical_y_delta



}



test_that("predict_delta_comps() correctly returns 'prop-realloc' data.frame on fairclough data (delta = +15min)", {


  expect_equal(predictions_df[["analytical"]], predictions_df[["numerical"]], tolerance = .00001, scale = 1)


})



data(fat_data)
# tail(fat_data)

realloc_mthd <- "prop-realloc"
covar_nms <- c("sibs",  "parents", "ed")
comp_nms <- c("sl","sb","lpa","mvpa")
D <- length(comp_nms)
outc_nm <- "fat"
# a matrix of 4! rows and 4 columns of all 1:4 permutations (row-wise)
comp_perms <- perm(1:D)
np <- nrow(comp_perms)
predictions_df <- data.frame(analytical = numeric(np), numerical = numeric(np))

delta1 <- 30 / (24 * 60)

for (i in 1:np) {
  
  # we will cycle through all permutations of the components specified in the model
  this_comp_order <- comp_nms[comp_perms[i, ]]
  
  deltacomp_df <-
    predict_delta_comps(
      dataf = fat_data,
      y = outc_nm, 
      comps = this_comp_order, 
      covars = covar_nms,
      deltas = delta1, 
      comparisons = realloc_mthd
    )
  
  
  numeric_y_delta <- deltacomp_df[deltacomp_df$`comp+` == this_comp_order[1], "delta_pred"]
  predictions_df$numerical[i] <- numeric_y_delta
  #str(deltacomp_df)
  
  
  # how to get the arithmetic mean below but we want geometric simplex mean
  # x1 <- mean(fat_data_ilrs[, this_comp_order[1]]) 
  # geometric/simplex mean:
  x1 <- attr(deltacomp_df, "mean_pred")[1, this_comp_order[1]]
  r <- delta1 / x1
  (x1 + delta1) / x1 == 1 + r
  s <- r * x1 / (1 - x1)
  
  fat_data_ilrs <- attr(deltacomp_df, "dataf")
  head(fat_data_ilrs)
  fat_data_ilrs <- fat_data_ilrs[, !(colnames(fat_data_ilrs) %in% this_comp_order)]
  fat_lm <- lm(as.formula(paste(outc_nm, "~ .")), data = fat_data_ilrs)
  
  b1 <- coefficients(fat_lm)["ilr1"]
  names(b1) <- NULL # remove vector names
  analytical_y_delta <- b1 * log((1 + r) / (1 - s)) * sqrt((D - 1) / D)
  
  predictions_df$analytical[i] <- analytical_y_delta
  
  
  
}



test_that("predict_delta_comps() correctly returns 'prop-realloc' data.frame on fat_data data (delta = +30min)", {
  
  
  expect_equal(predictions_df[["analytical"]], predictions_df[["numerical"]], tolerance = .00001, scale = 1)
  
  
})



delta1 <- -20 / (24 * 60)

for (i in 1:np) {
  
  # we will cycle through all permutations of the components specified in the model
  this_comp_order <- comp_nms[comp_perms[i, ]]
  
  deltacomp_df <-
    predict_delta_comps(
      dataf = fat_data,
      y = outc_nm, 
      comps = this_comp_order, 
      covars = covar_nms,
      deltas = delta1, 
      comparisons = realloc_mthd
    )
  
  
  numeric_y_delta <- deltacomp_df[deltacomp_df$`comp+` == this_comp_order[1], "delta_pred"]
  predictions_df$numerical[i] <- numeric_y_delta
  #str(deltacomp_df)
  
  
  # how to get the arithmetic mean below but we want geometric simplex mean
  # x1 <- mean(fat_data_ilrs[, this_comp_order[1]]) 
  # geometric/simplex mean:
  x1 <- attr(deltacomp_df, "mean_pred")[1, this_comp_order[1]]
  r <- delta1 / x1
  (x1 + delta1) / x1 == 1 + r
  s <- r * x1 / (1 - x1)
  
  fat_data_ilrs <- attr(deltacomp_df, "dataf")
  head(fat_data_ilrs)
  fat_data_ilrs <- fat_data_ilrs[, !(colnames(fat_data_ilrs) %in% this_comp_order)]
  fat_lm <- lm(as.formula(paste(outc_nm, "~ .")), data = fat_data_ilrs)
  
  b1 <- coefficients(fat_lm)["ilr1"]
  names(b1) <- NULL # remove vector names
  analytical_y_delta <- b1 * log((1 + r) / (1 - s)) * sqrt((D - 1) / D)
  
  predictions_df$analytical[i] <- analytical_y_delta
  
  
  
}



test_that("predict_delta_comps() correctly returns 'prop-realloc' data.frame on fat_data data (delta = -20min)", {
  
  
  expect_equal(predictions_df[["analytical"]], predictions_df[["numerical"]], tolerance = .00001, scale = 1)
  
  
})



