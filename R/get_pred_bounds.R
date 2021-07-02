
#' Title
#'
#' @param y0_star 
#' @param crit_val 
#' @param se_y0_star 
#' @param bound 
#'
#' @return
#' @export
#'
#' @examples
get_pred_bounds <- function(y0_star, crit_val, se_y0_star, bound = 0) {
  y0_star + bound * crit_val * se_y0_star
}
