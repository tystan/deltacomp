


standardise_comps <- function(dataf, comps) {
  
  # standardise comps
  comp_totals <- rowSums(dataf[, comps])
  comps_to_use <- compositions::acomp(dataf[, comps])

  cat(
    "---\nThese are the quartiles of the summed compositions",
    "(ideally all equal, otherwise calculations will be error prone):\n"
  )
  print(quantile(comp_totals, seq(0, 1, by = 0.25)))
  cat("---\n\n")
  dataf[, comps] <- as.data.frame(comps_to_use) 
  
  ## equally could use: dataf[, comps] / matrix(comp_totals, ncol = n_comp, nrow = n)
  # i.e.,note that these are equal
  # as.data.frame(compositions::acomp(dataf[,comps])) - 
  # dataf[, comps] / matrix(comp_totals, ncol = n_comp, nrow = n)
  
  return(dataf)
  
}