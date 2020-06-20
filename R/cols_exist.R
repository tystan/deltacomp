cols_exist <- function(dataf, cols) {
  
  if (is_null_or_na(cols)) {
     stop("cols argument supplied to cols_exist() is NA, NULL or length 0.")
  }

  col_fnd <- cols %in% colnames(dataf)
  
  if (!all(col_fnd)) {
    stop(paste(cols[which(!col_fnd)], collapse = ","), " are/is not column(s) in the supplied dataf")
  }
  
  return(invisible(TRUE))
  
}
