

#' Create a sequential binary partition indication matrix
#'
#' @param n_comp the number of compositional variables to partition
#'
#' @return
#' A \code{n_comp} by \code{n_comp - 1} matrix where each column relates to one partition.
#' 
#' The partitions are made so that the numerator (\code{1} values) for the \code{i}th column is in the \code{i}th row. 
#' All values below the \code{1} in the column are set to \code{-1} (the denominator).
#' 
#' The generated sequential binary partition for 3 comp vars is \code{(1, -1, -1), (0, 1, -1)}.
#' 
#' The generated sequential binary partition for 4 comp vars is \code{(1, -1, -1, -1), (0, 1, -1, -1), (0, 0, 1, -1)}.
#' 
#' etc
#' @export
#'
#' @examples
#' create_seq_bin_part(3)
#' create_seq_bin_part(4)
#' create_seq_bin_part(10)
create_seq_bin_part <- function(n_comp) {


  base_zeros <- rep(0, n_comp)
  
  sbp <- matrix(0, nrow = n_comp, ncol = n_comp - 1)
  for (j in 1:(n_comp - 1)) {
    this_col <- base_zeros
    this_col[j] <- 1
    this_col[(j + 1):n_comp] <- -1
    sbp[, j] <- this_col
  }
  # alternative ilr bases can be made via ilrBase(D = n_comp) etc

  return(sbp)

}