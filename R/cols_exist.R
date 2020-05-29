cols_exist <- function(dataf, cols) {
  
  col_fnd <- cols %in% colnames(dataf)
  
  if (!all(col_fnd)) {
    stop(paste(cols[which(!col_fnd)], collapse = ","), "are/is not column(s) in the supplied dataf")
  }
  
  
}
