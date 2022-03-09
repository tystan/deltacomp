
#' fit linear model based on input data.frame
#'
#' @param y_str a string representation of the column in \code{X} that is the outcome
#' @param X a data.frame or matrix that contains the predictor and outcome variables
#' @param verbose if \code{TRUE} (default), a model summary will be printed to the console
#'
#' @return 
#' A \code{lm} object where the \code{y_str} column has been regressed against the remaining
#' columns of \code{X} (with an intercept term as well).
#' 
#' @export
#'
# @examples
# data(fairclough)
# fit_lm("bmi", fairclough[, c("bmi", "sex", "decimal_age")])
fit_lm <- function(y_str, X, verbose = TRUE) {

  lm_formula <- as.formula(paste(y_str, "~ ."))
  
  options(warn = 2) # make warnings errors to be caught
  lm_X <- try( {
    lm(lm_formula, data = X)
  }, silent = TRUE )
  options(warn = 0) # make warnings warnings again
  
  # check successful fit
  if (is_lm_mod(lm_X)) {
    
    if (verbose) {
      cat("---\nSummary of the linear model:\n---\n")
      print(summary(lm_X))
      cat("---\n\n")
    }
    
    return(lm_X)
    
  } else {
    
    warning("### Model fitting unsuccessful, halting function as calculations will be unreliable ###")
    stop(attr(lm_X, "condition")) # return error msg as string
    
  }
  
}
