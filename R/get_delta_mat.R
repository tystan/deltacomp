

get_delta_mat <- function(deltas, comparisons, comps, mean_comps) {

  n_delta <- length(deltas)
  poss_comps0 <- create_comparison_matrix(comparisons, comps, mean_comps)
  
  delta_mat <- NULL
  for(d in 1:n_delta) {
    delta_mat <- rbind(delta_mat, deltas[d] * poss_comps0)
  }

  return(delta_mat)
  
}
