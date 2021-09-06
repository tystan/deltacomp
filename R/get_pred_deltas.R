



get_pred_deltas <- function(delta_mat, realloc_nms) {
  
  n_p <- nrow(delta_mat)
  
  if (n_p != nrow(realloc_nms))
    stop("Internal error: delta_mat and realloc_nms have differnt row numbers. Please contact package maintainer.")
  
  delta <- NULL
  for(i in 1:n_p) 
    delta <- c(delta, delta_mat[i, realloc_nms[i, 1]]) # note col 1 of realloc names is the comp of interest
  
  delta

}
