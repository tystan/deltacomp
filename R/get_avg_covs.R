
get_avg_covs <- function(dataf, covars) {
  
  n_cov <- length(covars)
  m_cov <- vector(mode = "list", length = n_cov)
  names(m_cov) <- paste0("m_", covars)
  
  for (j in 1:n_cov) { # testing; j <- 1
    
    this_covar <- dataf[, covars[j]]
    
    if (is.factor(this_covar)) {
      levs <- levels(this_covar)
      m_temp <- median(as.integer(this_covar))
      m_cov[[j]] <- this_covar[this_covar == levs[m_temp]][1]
    } else if (is.numeric(this_covar)) {
      m_cov[[j]] <- mean(this_covar)
    } else {
      stop(paste0(
        "Covariate misspecification in data: ",
        "please have all covariates specified as either factors or numeric variables\n"
      ))
    }
    
    cat(
      "The 'average' case to be used for prediction of covariate",
      covars[j] , "is", 
      as.character(m_cov[[j]]), "\n"
    )
    
  }
  
  return(m_cov)
  
}




