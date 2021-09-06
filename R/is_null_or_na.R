

#' Catch NULL, empty and objects containing NAs
#'
#' @param x object to be tested
#'
#' @return
#' Boolean. If object is NULL, empty or contains NA then TRUE returned. FALSE otherwise.
#' @export
#'
# @examples
# is_null_or_na(NULL)
# is_null_or_na(integer(0))
# is_null_or_na(c(1, NA))
# is_null_or_na(10)
is_null_or_na <- function(x) {
  
  if (is.null(x)) {
    return(TRUE)
  } else if (length(x) < 1) {
    return(TRUE)
  } else if (any(is.na(x))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

