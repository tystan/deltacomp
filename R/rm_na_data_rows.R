
# @examples 
# data(fat_data)
# fd_w_nas <- fat_data[1:10, ]
# fd_w_nas[1, "fat"] <- NA
# fd_w_nas[3, "sl"] <- NA
# fd_w_nas[5, "ed"] <- NA
# fd_w_nas
# rm_na_data_rows(fd_w_nas, c("fat", "sl", "sb", "lpa", "mvpa", "sibs", "parents", "ed"))


rm_na_data_rows <- function(dataf, cols) {
  
  # check we've got valid columns
  cols_exist(dataf, cols)
  
  dataf_thin <- dataf[, cols]
  
  na_sums <- apply(dataf_thin, 1, function(x) sum(is.na(x)))
  if (any(na_sums > 0)) {
    w_na_sums <- which(na_sums > 0)
    cat("*** Rows", paste(w_na_sums, collapse = ","), "have NA values. ***\n")
    cat("Printing these rows before removing them:\n")
    print(dataf_thin[w_na_sums, ])
    dataf_thin <- dataf_thin[!na_sums, ]
    cat("Deletion of rows", paste(w_na_sums, collapse = ","), "successful\n")
  }

  return(dataf_thin)
  
}
