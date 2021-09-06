#' Extract critical quantities from a lm object (for confidence interval calculations)
#'
#' @param lm_X a lm object
#' @param alpha level of significance. Defaults to 0.05.
#'
#' @return
#' A list containing the \code{lm} model matrix (\code{dmX}), 
#' the inverse of \code{t(dmX) x dmX} (\code{XtX_inv}), 
#' the standard error (\code{s_e}),
#' the estimated single column beta matrix (\code{beta_hat}), and
#' the critical value of the relevant degrees of freedom t-dist (\code{crit_val}).
#' @export
#'
# @examples
# data(fat_data)
# lm_fat <- lm(fat ~ sl, data = fat_data)
# extract_lm_quantities(lm_fat)
extract_lm_quantities <- function(lm_X, alpha = 0.05) {

  # get the design matrix from the LM
  dmX <- model.matrix(lm_X)
  # (X^T X)^{-1}
  XtX_inv <- solve(t(dmX) %*% dmX)
  # the resid standard error
  s_e <- sqrt(sum(residuals(lm_X) ^ 2) / df.residual(lm_X))
  # crit val with 95% conf of relevant t-dist
  crit_val <- qt(1 - alpha / 2, df.residual(lm_X))
  # beta estimates
  beta_hat <- matrix(coefficients(lm_X), ncol = 1)
  
  return(list(
    dmX = dmX,
    XtX_inv = XtX_inv,
    s_e = s_e,
    beta_hat = beta_hat,
    crit_val = crit_val
  ))

}
