


get_se_y0_star <- function(x0_star, s_e, XtX_inv) {

  matrix(s_e * sqrt(diag(x0_star %*% XtX_inv %*% t(x0_star))), ncol = 1)

}




# testing

# y0_star <- x0_star %*% lm_quants$beta_hat
# se_y0_star2 <- matrix(0, ncol = 1, nrow = n_preds)
# test_star2 <- matrix(0, ncol = 1, nrow = n_preds)
# for (i in 1:n_preds) {
#   x0_star_i <- x0_star[i, , drop = FALSE]
#   test <- x0_star_i %*% lm_quants$XtX_inv %*% t(x0_star_i)
#   test_star2[i, ] <- test
#   se_y0_star2[i, ] <- lm_quants$s_e * sqrt(test)
# }
# 
# test_star <- diag(x0_star %*% lm_quants$XtX_inv %*% t(x0_star))
# test_star == test_star2
# 
# se_y0_star <- lm_quants$s_e * sqrt(diag(x0_star %*% lm_quants$XtX_inv %*% t(x0_star)))
# se_y0_star == se_y0_star2





