


get_all_comparison_mat <- function(deltas, comparisons, comps, mean_comps) {
  
  n_delta <- length(deltas)
  poss_comps0 <- create_comparison_matrix(comparisons, comps, mean_comps)
  
  poss_comps <- NULL
  for(d in 1:n_delta) {
    poss_comps <- rbind(poss_comps, poss_comps0)
  }
  
  return(poss_comps)
  
}
