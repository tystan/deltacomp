
#' Title
#'
#' @param y_str 
#' @param X 
#'
#' @return
#' @export
#'
#' @examples
fit_lm <- function(y_str, X) {


  lm_formula <- as.formula(paste(y_str, "~ ."))
  
  options(warn = 2) # make warnings errors to be caught
  lm_X <- try( {
    lm(lm_formula, data = X)
  }, silent = TRUE )
  options(warn = 0) # make warnings warnings again
  
  # check sucessful fit
  if (is_lm_mod(lm_X)) {
    
    cat("---\nSummary of the linear model:\n---\n")
    print(summary(lm_X))
    return(lm_X)
    
  } else {
    
    warning("### Model fitting unsuccessful, halting function as calculations will be unreliable ###")
    stop(attr(lm_X, "condition")) # return error msg as string
    
  }
  
}