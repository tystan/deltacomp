

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
is_lm_mod <- function(x) {
  return("lm" %in% class(x))
}