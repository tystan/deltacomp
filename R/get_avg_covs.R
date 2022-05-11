

get_avg_covs <- function(dataf, covars) {
  
  n_cov <- length(covars)
  m_cov <- vector(mode = "list", length = n_cov)
  names(m_cov) <- covars
  
  cat("---\n")
  for (j in 1:n_cov) { # testing; j <- 1
    
    this_covar <- dataf[[covars[j]]] # dataf[, covars[j]]: this won't be a vector for tibbles
    
    cat("* The 'average' case to be used for prediction of covariate", covars[j] , "is ")
    if (is.factor(this_covar)) {
      levs <- levels(this_covar)
      m_temp <- median(as.integer(this_covar))
      m_cov[[j]] <- this_covar[this_covar == levs[m_temp]][1]
      cat(as.character(m_cov[[j]]), "(factor)\n")
    } else if (is.numeric(this_covar)) {
      m_cov[[j]] <- mean(this_covar)
      cat(trimws(sprintf("%9.2f", m_cov[[j]])), "(numeric)\n")
    } else {
      stop(paste0(
        "Covariate misspecification in data: ",
        "please have all covariates specified as either factors or numeric variables\n"
      ))
    }
    
  }
  cat("---\n\n") # give extra space to separate output in console

  return(as.data.frame(m_cov))
  
}




