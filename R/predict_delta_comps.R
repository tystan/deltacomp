#' Get predictions from compositional ilr multiple linear regression model
#'
#' @author Ty Stanford <tystan@gmail.com>
#' @description Provided the data (containing outcome, composiitional compoents and covariates), fit a ilr multiple linear regression model and provide predictions from reallocating compositional values pairwise amunsnst the components model.
#' @param dataf A \code{data.frame} containing data
#' @param y Name (as string) of outcome in \code{dataf}
#' @param comps Character vector of names of compositions in \code{dataf}
#' @param covars Optional. Character vector of covariates names  (non-comp variables) in \code{dataf}. Defaults to NULL.
#' @param deltas A vector of time-component changes (as proportions of compositions , i.e., values between -1 and 1). Optional. Changes in compositions to be computed pairwise. Defaults to 0, 10 and 20 minutes as a proportion of the 1440 minutes in a day.
#' @param comparisons Currently three choices: "one-v-one" or "prop-realloc" (default). Currently "one-v-all"  isn't properly implemented.
#' @param alpha Optional. Level of significance. Defaults to 0.05.
#' @export
#' @examples
#' predict_delta_comps(
#'   dataf=fat_data,
#'   y = "fat",
#'   comps = c("sl", "sb", "lpa", "mvpa"),
#'   covars = c("sibs", "parents", "ed"),
#'   deltas = seq(-60, 60, by = 5) / (24 * 60),
#'   comparisons = "one-v-one",
#'   alpha = 0.05
#' )
#'

predict_delta_comps <- function(
  dataf, # data.frame of data
  y, # character name of outcome in dataf
  comps, # character vector of names of compositions in dataf
  covars = NULL, # character vector of names of covariates (non-comp variables) in dataf
  deltas = c(0, 10, 20) / (24 * 60), # changes in compositions to be computed pairwise
  comparisons = c("prop-realloc", "one-v-one")[1],
  alpha = 0.05
){

  # set up function constants
  comparisons <- get_comp_type(comparisons) 
  
  n <- nrow(dataf)
  n_comp <- length(comps)
  n_delta <- length(deltas)
  n_covar <- ifelse(is.null(covars), 0, length(covars))
  
  if (any(abs(deltas) > 1)) {
    stop("deltas must be specified as positive and negative proportions of a composition. i.e., values in (-1, 1).")
  }
  
  dataf <- rm_na_data_rows(dataf, c(y, comps, covars))
  
  # standardise comps
  dataf <- standardise_comps(dataf, comps)
  
  # get the mean of the compositions on the geometric scale
  mean_comps <- 
    compositions::mean.acomp(
      compositions::acomp(
        dataf[, comps]
      ), 
      robust = FALSE
    )
  
  if(!all.equal(1, sum(mean_comps), tolerance = 1e-5))
    stop("Calculated mean composition does not sum to 1")
  
  #### As covariates are fit linearly and are constant within prediction, they cancel out
  # (i.e., we don't compare age=10 vs age=15, keeping everything else constant
  # as we read that off the coefficients table)
  ### However, we may be interested in the prediction for the mean value of compositions 
  ### and covariates
  m_cov <- NULL
  mean_X <- as.data.frame(t(mean_comps))
  if (n_covar > 0) {
    m_cov <- get_avg_covs(dataf, covars)
    # testing:
    # print(tibble::as_tibble(m_cov))
    mean_X <- cbind(mean_X, m_cov)
    # print(tibble::as_tibble(mean_X))
  }
  
  # create sequential binary partition
  sbp <- create_seq_bin_part(n_comp)
  # The orthonormal transformation matrix
  psi <- compositions::gsi.buildilrBase(sbp)
  # add ilr coords to dataset
  dataf <- append_ilr_coords(dataf, comps, psi)
  mean_X <- append_ilr_coords(mean_X, comps, psi)
  # print(tibble::as_tibble(mean_X))
  
  # create dataset X only consisting of outcome, ilr coords and covars
  ilr_names <- paste0("ilr", 1:(n_comp - 1))
  X <- dataf[, colnames(dataf) %in% c(y, ilr_names, covars)] 
  # fit model
  lm_X <- fit_lm(y, X)
  
  mean_pred <- get_mean_pred(lm_X, mean_X, alpha = alpha)
  # extract linear model quantities required for further calculations
  lm_quants <- extract_lm_quantities(lm_X, alpha = alpha)
  
  delta_mat <- get_delta_mat(deltas, comparisons, comps, mean_comps)
  n_preds <- nrow(delta_mat)
  poss_comps <- get_all_comparison_mat(deltas, comparisons, comps, mean_comps)
  
  m_comps <- matrix(rep(mean_comps, n_preds), nrow = n_preds, byrow = TRUE)
  m_delta <- m_comps + delta_mat
  
  m_delta_less_0 <- rowSums(m_delta < 0)
  if(any(m_delta_less_0 > 0)) {
    warning(
      paste(
        "By using the supplied deltas, there are NEGATIVE compositional",
        "values so these predictions are non-sensical.",
        "Consider using smaller delta values or proportional changes in compositions."
      )
    )
  }
  
  if(!all.equal(rep(1, n_preds), rowSums(m_delta), tolerance = 1e-5))
    stop("Calculated mean composition does not sum to 1")
  
  
  ilr_means <- compositions::ilr(m_comps, V = psi)
  ilr_delta <- compositions::ilr(m_delta, V = psi)
  
  # get rid of "rmult" class, revert to "matrix", to allow for easier matrix manipulation
  attr(ilr_delta, "class") <- NULL 
  attr(ilr_means, "class") <- NULL 
  
  x0_star <- get_x0_star(lm_quants$dmX, n_preds, ilr_names, ilr_delta, ilr_means)
  y0_star <- x0_star %*% lm_quants$beta_hat
  se_y0_star <- get_se_y0_star(x0_star, lm_quants$s_e, lm_quants$XtX_inv)
  
  
  # get labels and deltas for reallocations
  realloc_nms <- get_realloc_nms(comps, comparisons, poss_comps) 
  delta_list <- get_pred_deltas(delta_mat, realloc_nms)
  
  
  preds <- 
    cbind(
      as.data.frame(realloc_nms, stringsAsFactors = FALSE),
      delta_list,
      alpha,
      get_pred_bounds(y0_star, lm_quants$crit_val, se_y0_star, bound =  0),
      get_pred_bounds(y0_star, lm_quants$crit_val, se_y0_star, bound = -1),
      get_pred_bounds(y0_star, lm_quants$crit_val, se_y0_star, bound =  1)
    )
  
  colnames(preds) <-
    c("comp+", "comp-", "delta", "alpha", "delta_pred", "ci_lo", "ci_up")
  
  
  preds$sig <- ifelse(preds$ci_lo <= 0 & preds$ci_up >= 0, "", "*")
  
  # data.frame to return
  ret_obj <- preds
  class(ret_obj) <- c(class(preds), "deltacomp_obj")
  # add info from function call
  attr(ret_obj, "dataf") <- dataf
  attr(ret_obj, "y") <- y
  attr(ret_obj, "comps") <- comps
  attr(ret_obj, "covars") <- covars
  attr(ret_obj, "deltas") <- deltas
  attr(ret_obj, "comparisons") <- comparisons
  attr(ret_obj, "alpha") <- alpha
  attr(ret_obj, "irl_basis") <- psi
  attr(ret_obj, "mean_pred") <- mean_pred
  
  return(ret_obj)

}
