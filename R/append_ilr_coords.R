
append_ilr_coords <- function(dataf, comps, psi) {

  n <- nrow(dataf)
  n_c <- length(comps)
  ilr_comps <- compositions::ilr(dataf[, comps, drop = FALSE], V = psi)
  if (n == 1) { # special case where "rmult" is a vector not data.frame
    ilr_comps <- as.data.frame(matrix(ilr_comps, nrow = 1))
  } else {
    ilr_comps <- as.data.frame(ilr_comps[1:n, , drop = FALSE])
  }
  ilr_names <- paste0("ilr", 1:(n_c - 1))
  colnames(ilr_comps) <- ilr_names
  
  dataf <- cbind(ilr_comps, dataf)
  return(dataf)

}
