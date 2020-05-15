

# comparisons = "one-v-all"
# comparisons = "prop-realloc"
# comparisons = "one-v-one"

create_comparison_matrix <- function(comparisons, comps, mean_comps) {

  K <- poss_comps0 <- NULL
  n_comp <- length(comps)
  
  if (comparisons == "one-v-all") {
    
    # number of combinations is:
    # K = n_comps as only one combination per composition
    K <- n_comp
    poss_comps0 <- matrix(0, nrow = K, ncol = n_comp, dimnames = list(NULL, comps))
    
    for (k in 1:K) {
      poss_comps0[k, k] <- 1
      poss_comps0[k, -k] <- -1 / (K - 1) # equal distribution of allocation to other comps
    }
    
  } else if (comparisons == "prop-realloc") {
    
    ### same as "one-vs-all" except the "1 / (K - 1)"s replaced by weighted means to sum to 1
    # number of combinations is:
    # K = n_comps as only one combination per composition
    K <- n_comp
    poss_comps0 <- matrix(0, nrow = K, ncol = n_comp, dimnames = list(NULL, comps))
    
    for (k in 1:K) {
      poss_comps0[k, k] <- 1
      other_mean_comps <- mean_comps[-k]
      sum_other <- sum(other_mean_comps)
      other_mean_comps <- other_mean_comps / sum_other
      if (!all.equal(1, sum(other_mean_comps), tolerance = 1e-5)) { # proportional realloc has not worked...
        stop("Error in proportional re-allocaiton")
      }
      poss_comps0[k, -k] <- -other_mean_comps # proportional distribution of allocation to other comps
    }
    
  } else { # comparisons = "one-v-one", DEFAULT
    # number of combinations is:
    # K = number of positions the 1 can be in multiplied by
    #     number of positions left for the -1 can be in
    K <- n_comp * (n_comp - 1)
    poss_comps0 <- matrix(0, nrow = K, ncol = n_comp, dimnames = list(NULL, comps))
    
    k <- 0
    for (i in 1:n_comp) for (j in 1:n_comp) if (i != j) {
      k <- k + 1
      poss_comps0[k, c(i, j)] <- c(1, -1)
    }
    
  }
  
  realloc_sum <- rowSums(poss_comps0)
  theoretical_sums <- rep(0, K)
  if (!all.equal(theoretical_sums, realloc_sum, tolerance = 1e-5)) { 
    # proportional realloc has not worked, should sum to 0
    stop("Error in re-allocaiton matrix: rows should sum to 0")
  }
  
  return(poss_comps0)
  
}