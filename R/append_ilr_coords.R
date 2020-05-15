
append_ilr_coords <- function(dataf, comps, psi) {

  n <- nrow(dataf)
  n_c <- length(comps)
  ilr_comps <- compositions::ilr(dataf[, comps], V = psi)
  ilr_comps <- as.data.frame(ilr_comps[1:n, ])
  ilr_names <- paste0("ilr", 1:(n_c - 1))
  colnames(ilr_comps) <- ilr_names
  
  dataf <- cbind(ilr_comps, dataf)
  return(dataf)

}
