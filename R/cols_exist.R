#' Check whether columns exist in a data.frame
#'
#' @param dataf a data.frame
#' @param cols character vector of columns to be checked in \code{dataf}
#'
#' @return
#' An error if all \code{cols} not present in \code{dataf}. 
#' Returns \code{TRUE} invisibly otherwise.
#' @export
#'
# @examples
# data(fat_data)
# cols_exist(fat_data, c("lpa", "sl")) 
# # not run (throws error):
# # cols_exist(fat_data, "a") 
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
