


get_realloc_nms <- function(comps, comparisons, poss_comps) {

  n_p <- nrow(poss_comps)
  realloc_nms <- matrix("", nrow = n_p, ncol = 2) # make labels for reallocations
  
  if (comparisons == "one-v-all") {
    
    for (i in 1:n_p)
      realloc_nms[i, ] <- c(comps[poss_comps[i,] == 1], "equal-realloc")
    
  } else if (comparisons == "prop-realloc") {
    
    for (i in 1:n_p)
      realloc_nms[i, ] <- c(comps[poss_comps[i,] == 1], "proportional-realloc")
    
  } else { # comparisons = "one-v-one", DEFAULT
    
    for (i in 1:n_p)
      realloc_nms[i, ] <- c(comps[poss_comps[i,] == 1] , comps[poss_comps[i,] == -1])
    
  }

  return(realloc_nms)

}