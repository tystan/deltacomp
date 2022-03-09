


#' Print the ilr transformation of provided composition parts to console
#'
#' @param comps a character vector of compositional parts
#'
#' @return 
#' a character vector of representing the ilr transformation of the \code{comps}
#' is returned invisibly as the function's purpose is simply to 
#' print to the R console
#' 
#' @export
#'
# @examples
# print_ilr_trans(c("sl", "sb", "lpa", "mvpa"))
# # note spaces are changed to underscores to remove ambiguity in ilr formulas
# ilrs_v <- print_ilr_trans(paste("V", 1:6)) 
# ilrs_v
print_ilr_trans <- function(comps) {
  
  n_comp <- length(comps)
  ilr_strings <- rep("", n_comp - 1)
  # change spaces to underscores to remove ambiguity in formulas
  comps <- gsub(" ", "_", comps, fixed = TRUE)
  
  for (i in 1:(n_comp - 1)) {
    
    const_numer_i <- n_comp - i
    const_denom_i <- n_comp - (i - 1)
    const_i <- paste0("sqrt(", const_numer_i, "/", const_denom_i, ")")
    ilr_numer_i <- comps[i]
    ilr_denom_i <- paste(comps[(i + 1):n_comp], collapse = " * ")
    if ((i + 1) < n_comp) {
      ilr_denom_i <- paste0("(", ilr_denom_i, ")^(1/", const_numer_i, ")")
    }
    ilr_strings[i] <- 
      paste0(
        "ilr", i, " = ", 
        const_i, " * ln(", ilr_numer_i, " / ", ilr_denom_i, ")"
      )
    
  }
  
  cat("---\nThe ilr transformation used is defined with:\n---\n")
  cat(paste(ilr_strings, collapse = "\n"))
  cat("\n\n(note `ln` is log base e/the natural logarithm)\n---\n\n")
  
  return(invisible(ilr_strings))
  
}
