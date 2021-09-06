

get_pred_bounds <- function(y0_star, crit_val, se_y0_star, bound = 0) {
  y0_star + bound * crit_val * se_y0_star
}
