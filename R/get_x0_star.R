

get_x0_star <- function(dmX, n_preds, ilr_names, ilr_delta, ilr_means) {

  # get single row of design matrix
  dmX0 <- dmX[1, ]
  x0_star <- 
    matrix(
      dmX0,
      ncol = ncol(dmX),
      nrow = n_preds, 
      byrow = TRUE, 
      dimnames = list(NULL, colnames(dmX))
    )
  x0_star[, ilr_names] <- ilr_delta - ilr_means
  x0_star[, !(colnames(x0_star) %in% ilr_names)] <- 0
  
  return(x0_star)

}
