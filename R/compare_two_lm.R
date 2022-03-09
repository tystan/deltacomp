
#' Statistical test of the collective significance of the ilr variables
#'
#' @param y_str a string representation of the column in \code{X1} (and \code{X2}) 
#' that is the outcome
#' @param X1 a data.frame or matrix that contains a subset of the predictor variables 
#' in \code{X2} and outcome variable
#' @param X2 a data.frame or matrix that contains the predictor variables and outcome 
#' variable
#'
#' @return 
#' Returns \code{NULL} invisibly. The ANOVA analysis is printed to the console, that is,
#' the statistical test of whether the additional predictors in \code{X2} improve the 
#' model significantly from the model with only the subset of predictors in \code{X1}.
#' 
#' @export
#'
# @examples
# data(fairclough)
# compare_two_lm(
#   "bmi",
#   fairclough[, c("bmi", "sex")],
#   fairclough[, c("bmi", "sex", "decimal_age")]
# )
# # compare to lm of superset
# summary(fit_lm(
#   "bmi",
#   fairclough[, c("bmi", "sex", "decimal_age")]
# )) # same p-val

# compare_two_lm(
#   "bmi",
#   fairclough[, c("bmi"), drop = FALSE],
#   fairclough[, c("bmi", "sex", "decimal_age")]
# )
# summary(lm(bmi ~ 1 + ., data = fairclough[, c("bmi"), drop = FALSE]))

### not run
# compare_two_lm(
#   "bmi",
#   fairclough[, c("bmi", "sex", "imd_decile")],
#   fairclough[, c("bmi", "sex", "decimal_age")]
# )
# compare_two_lm(
#   "bmi",
#   fairclough[, c("bmi", "sex", "decimal_age")],
#   fairclough[, c("bmi", "sex")]
# )
# compare_two_lm(
#   "bmi",
#   fairclough[, c("bmi", "sex")],
#   fairclough[, c("bmi", "sex")]
# )

compare_two_lm <- function(y_str, X1, X2) {
  
  x1_nms <- colnames(X1)
  x2_nms <- colnames(X2)
  
  if (!all(x1_nms %in% x2_nms)) {
    stop("X1 data is not a subset of X2 data, check all columns in X1 exist in X2")
  }
  
  superset_vars <- x2_nms[!(x2_nms %in% x1_nms)]
  if (length(superset_vars) < 1) {
    stop("X2 data is not a superset of the columns in X1")
  }
  superset_str <- paste(superset_vars, collapse = ", ")
  
  lm_X1 <- fit_lm(y_str, X1, verbose = FALSE)
  lm_X2 <- fit_lm(y_str, X2, verbose = FALSE)    

  cat("---\nStatistical test of the collective significance of the variables ")
  cat(paste0("{", superset_str, "}:"), "\n---\n")
  print(anova(lm_X1, lm_X2))
  cat("---\n\n")
  
  return(invisible(NULL))
 
}
