
#' Create ilr basis matrix (V)
#'
#' @param n_comp the number of compositional variables 
#'
#' @return
#' A \code{n_comp} by \code{n_comp - 1} matrix where each column relates to one ilr variable
#' 
#' The ilr basis made so that the numerator (\code{+} values) for the \code{i}th column is in the \code{i}th row. 
#' All values below the \code{+} value in the column are set to \code{-1} (the denominator).
#' 
#' The ilr basis for 3 compositional vars is 
#' {\code{(2, -1, -1)/sqrt(6), (0, 1, -1)/sqrt(2)}}.
#' 
#' The ilr basis for 4 comp vars is 
#' {\code{(3, -1, -1, -1)/sqrt(12), (0, 2, -1, -1)/sqrt(6), (0, 0, 1, -1)/sqrt(2)}}.
#' 
#' etc
#' @export
#'
# @examples
# create_v_mat(3)
# create_v_mat(4)
# create_v_mat(10)


create_v_mat <- function(n_comp) {
  
  base_zeros <- rep(0, n_comp)
  
  v_mat <- matrix(0, nrow = n_comp, ncol = n_comp - 1)
  for (j in 1:(n_comp - 1)) {
    this_col <- base_zeros
    this_col[(j + 1):n_comp] <- -1
    this_col[j] <- n_comp - j # -sum(this_col[(j + 1):n_comp])
    v_mat[, j] <- this_col
  }
  # alternative ilr bases can be made via compositions::ilrBase(D = n_comp) etc
  
  # now need to normalise columns to length 1
  vec_len <- sqrt(colSums(v_mat ^ 2))
  for (j in 1:(n_comp - 1)) {
    v_mat[, j] <- v_mat[, j] / vec_len[j]
  }
  
  return(v_mat)
  
}