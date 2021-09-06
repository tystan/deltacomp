




#' Is object that is returned from \code{pred_delta_comps()}?
#'
#' @param x object to be tested
#'
#' @return
#' Boolean TRUE or FALSE
#' @export
#'
# @examples
# is_deltacomp_obj(1) # FALSE
 
is_deltacomp_obj <- function(x) {
  # need to add test for NULL to return FALSE
  return("deltacomp_obj" %in% class(x))
}





