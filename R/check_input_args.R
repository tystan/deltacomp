
check_input_args <- function(dataf, y, comps, covars, deltas) {
  
  
  if (!is.data.frame(dataf)) {
    stop("dataf supplied must be a data.frame.")
  } else if (nrow(dataf) < 1) {
    stop("dataf supplied must be a non-empty data.frame.")
  }
  
  if (is_null_or_na(y) | length(y) != 1 | !is.character(y)) {
    stop("Please supply a character string for the outcome column in dataf.")
  }
  
  if (is_null_or_na(comps)) {
    stop("The provided compositional column names must be a non-empty vector.")
  } else if (!is.character(comps)) {
    stop("Please supply a character string of the compositional component column names in dataf.")
  } else if (length(comps) < 2) {
    stop("At least two compositional components are required to create an ilr linear regression.")
  }
  
  if (!is_null_or_na(covars) & !is.character(covars)) {
    stop("Please supply a character string of the covariate column names in dataf (optionally NULL or NA for no covariates).")
  } 
  
  if (any(abs(deltas) > 1)) {
    stop("deltas must be specified as positive and negative proportions of a composition. i.e., values in (-1, 1).")
  }
  
  n_covars <- ifelse(is_null_or_na(covars), 0, length(covars))
  if (nrow(dataf) <= (length(comps) + n_covars)) {
    stop("The number of rows in dataf should far exceed the number of predictors (compositional variables and covariates). Reconsider your model specification.")
  }
  
  
  return(invisible(TRUE))
  
  
}

