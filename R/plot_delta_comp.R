
globalVariables(c(
  "ci_lo", "ci_up", "comp+", "delta", "delta_pred"
))


#' Get predictions from compositional ilr multiple linear regression model
#'
#' @author Ty Stanford <tystan@gmail.com>
#' @description Provided the data (containing outcome, composiitional compoents and covariates), fit a ilr multiple linear regression model and provide predictions from reallocating compositional values pairwise amunsnst the components model.
#'
#' @param dc_obj A \code{deltacomp_obj} object returned from the function \code{predict_delta_comps}
#' @param comp_total A numeric scalar that is the original units of the composition to make the x-axis the original scale instead of in the range \code{[min(delta), max(delta)]} in (-1, 1).
#' @param graph.sys Graphing system within R. This defaults to "ggplot2" but can be one out of c("base","ggplot2").
#' @param units_lab Character string of the units of the compositions relating to \code{comp_total} to add to the x-axis label
#'
#' @export
#' @examples 
#' data(fairclough)
#' 
#' deltacomp_df <-
#'   predict_delta_comps(
#'     dataf = fairclough,
#'     y = "z_bmi",
#'     comps = c("sleep","sed","lpa","mvpa"),
#'     covars = c("decimal_age","sex"),
#'     deltas =  seq(-20, 20, by = 5) / (24 * 60), 
#'     comparisons = "prop-realloc",
#'     alpha = 0.05
#'   )
#' class(deltacomp_df)
#' 
#' plot_delta_comp(
#'   dc_obj = deltacomp_df,
#'   comp_total = 24 * 60,
#'   units_lab = "min"
#' )
#' 
#' deltacomp_df <-
#'   predict_delta_comps(
#'     dataf = fairclough,
#'     y = "z_bmi",
#'     comps = c("sleep","sed","lpa","mvpa"),
#'     covars = c("decimal_age","sex"),
#'     deltas =  seq(-20, 20, by = 5) / (24 * 60), 
#'     comparisons = "one-v-one",
#'     alpha = 0.05
#'   )
#' 
#' plot_delta_comp(
#'   dc_obj = deltacomp_df,
#'   comp_total = 24 * 60,
#'   units_lab = "min"
#' )



plot_delta_comp <- function(dc_obj, comp_total = NULL, units_lab = NULL, 
                            graph.sys = c("base", "ggplot2")[2]) {

  if (!is_deltacomp_obj(dc_obj)) {
    stop("Input needs to be a deltacomp object. i.e., data.frame returned by predict_delta_comps().")
  }
  
  if (!is.null(comp_total)) {
    dc_obj[["delta"]] <- dc_obj[["delta"]] * comp_total
  }
  
  x_lab_add <- ""
  if (!is.null(units_lab)) {
    x_lab_add <- paste0(" (", units_lab, ")")
  }
  
  alph <- attr(dc_obj, "alpha")
  ci_lev <- sprintf("%2.0f", 100 * (1 - alph))
  lab_ci <- paste0("Predicted\nchange in\noutcome w/ \n", ci_lev, "% CI for\ndelta(comp)")
  delts <- attr(dc_obj, "deltas")
  outc <- attr(dc_obj, "y")
  comps <- attr(dc_obj, "comps")
  comparisons <- attr(dc_obj, "comparisons")
  
  if (length(delts) < 2) {
    warning(
      "plot_delta_comp(): Less than 2 distinct delta values provided, confidence intervals will not be plotted",
      "(two or more x values are required to plot a CI polygon/ribbon)"
    )
  }
  
  # make sure olevels are ordered by the order they are specified in the function call
  dc_obj$`comp+` <- factor(dc_obj$`comp+`, levels = comps)
  levels(dc_obj$`comp+`) <- paste0(comps, "+Delta")
  if (comparisons == "one-v-one") {
    dc_obj$`comp-` <- factor(dc_obj$`comp-`, levels = comps)
    levels(dc_obj$`comp-`) <- paste0(comps, "-Delta")
  }
  
  if (graph.sys == "ggplot2") {
    ggp <-
      ggplot(dc_obj) +
      geom_vline(xintercept = 0, col = "grey60") +
      geom_hline(yintercept = 0, col = "grey60") +
      geom_line(aes(x = delta, y = delta_pred, col = `comp+`)) +
      geom_point(aes(x = delta, y = delta_pred, col = `comp+`), size = 1) +
      geom_ribbon(aes(x = delta, ymin = ci_lo, ymax = ci_up, fill = `comp+`), alpha = 0.3) + 
      theme_bw() +
      labs(
        x = paste0("Change/delta in composition", x_lab_add),
        y = paste0("Predicted change in outcome (", outc, ") with ", ci_lev, "% CI")
      ) +
      theme(legend.position = "none")
    
    
    if (comparisons == "one-v-one") {
      ggp <- ggp +
        facet_grid(`comp-` ~ `comp+`, labeller = label_parsed)
    } else {
      ggp <- ggp +
        facet_wrap(~ `comp+`, labeller = label_parsed)
    } 
  } else if (graph.sys == "base") {
    # to do....
    # PLOT
    par(las = 1, mfrow = c(2,2))
    for (part in unique(dc_obj$`comp+`)) {
      thisPlot = which(dc_obj$`comp+` == part)
      plot(x = dc_obj$delta[thisPlot], y = dc_obj$delta_pred[thisPlot], 
           type = "o", pch = 19,
           main = bquote(.(gsub("+Delta", "", part, fixed = T)) + Delta),
           font.main=1,
           col = colors[4], lwd = 3, 
           xlim = range(dc_obj$delta),
           ylim = range(c(dc_obj$ci_lo, dc_obj$ci_up)),
           ylab = paste0("Predicted change in outcome (", outc, ") with ", ci_lev, "% CI"), 
           xlab = paste0("Change/delta in composition", x_lab_add))
      polygon(x = c(dc_obj$delta[thisPlot], rev(dc_obj$delta[thisPlot])), 
              y = c(dc_obj$ci_lo[thisPlot], rev(dc_obj$ci_up[thisPlot])),
              border = NA, col = myfunctions::add_alpha("red", alpha), fillOddEven = FALSE)
      abline(h = 0)
    }
  }
  
  return(ggp)


}




