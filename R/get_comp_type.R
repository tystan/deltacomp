


get_comp_type <- function(comparisons) {
  
  comp_opts <- c("one-v-one", "one-v-all", "prop-realloc")
  
  if (!(length(comparisons) == 1)) {
    stop("only specify one comparison method at a time")
  }
  
  # match input argument to valid values
  analysis_to_use <- pmatch(comparisons, comp_opts)
  # remove mismatches
  analysis_to_use <- analysis_to_use[!is.na(analysis_to_use)]
  # use strings not indexes for use later on
  analysis_to_use <- comp_opts[analysis_to_use]
  
  if (length(analysis_to_use) == 0)
    stop(
      "No valid comparisons option specified. Valid transformations are",
      paste(comp_opts, collapse = "|")
    )
  
  cat(
    "---\nComparison type being used in post-hoc calculations: '", 
    analysis_to_use, 
    "'\n---\n\n", 
    sep = ""
  )
  
  return(analysis_to_use)
  
}




