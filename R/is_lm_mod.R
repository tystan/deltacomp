

#' Is object that is returned from \code{lm()}?
#'
#' @param x object to be tested
#'
#' @return
#' Boolean TRUE or FALSE
#' @export
#'
# @examples
# data(fairclough)
# is_lm_mod(fit_lm("bmi", fairclough[, c("bmi", "sex", "decimal_age")]))
# is_lm_mod(1) # FALSE

is_lm_mod <- function(x) {
  return("lm" %in% class(x))
}