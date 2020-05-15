

create_seq_bin_part <- function(n_comp) {

  # sequential binary partition for 3 comp vars can be
  # (1, -1, -1), (0, 1, -1)
  # sequential binary partition for 4 comp vars can be
  # (1, -1, -1, -1), (0, 1, -1, -1), (0, 0, 1, -1)
  # etc
  base_zeros <- rep(0, n_comp)
  
  sbp <- matrix(0, nrow = n_comp, ncol = n_comp - 1)
  for (j in 1:(n_comp - 1)) {
    this_col <- base_zeros
    this_col[j] <- 1
    this_col[(j + 1):n_comp] <- -1
    sbp[, j] <- this_col
  }
  # can also do ilrBase(D = n_comp) etc

  return(sbp)

}