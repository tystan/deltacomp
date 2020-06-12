check_strictly_positive_vals <- function(dataf, comps, tol = 1e-6) {
  
  vals <- dataf[, comps]
  
  if (any(vals < tol)) {
    stop(
      "Values less than ", tol, " detected in the compositions. ",
      "Please check your data for negative values or values close to zero. ",
      "Negative values are non-sensical and values that are 0 or close to 0 stop sensible geometric averaging."
    )
  }
  
  
}










