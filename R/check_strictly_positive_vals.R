#' Check if compositional variable are strictly greater than 0
#'
#' @param dataf data.frame containing composition variables
#' @param comps character vector of composition variable names in dataf
#' @param tol a numeric value that compositional values are expected to be greater or equal than. 1e-6 is deafult
#'
#' @return
#' If any compositional values are found to be strictly less than \code{tol} and erro is thrown. 
#' Returns \code{TRUE} invisibly otherwise.
#' @export
#'
# @examples
# data(fat_data)
# check_strictly_positive_vals(fat_data, c("lpa", "sl"), tol = 1e-6) 
# # not run (throws error):
# # check_strictly_positive_vals(data.frame(a = rnorm(10)), "a", tol = 1e-6) 
check_strictly_positive_vals <- function(dataf, comps, tol = 1e-6) {
  
  vals <- dataf[, comps]
  
  if (any(vals < tol)) {
    stop(
      "Values less than ", tol, " detected in the compositions. ",
      "Please check your data for negative values or values close to zero. ",
      "Negative values are non-sensical and values that are 0 or close to 0 stop sensible geometric averaging."
    )
  }
  
  return(invisible(TRUE))

}










