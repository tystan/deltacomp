

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

