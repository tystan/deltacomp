# deltacomp
Functions to analyse compositional data and produce confidence intervals for relative increases and decreases in the compositional components

## Background
Please see [Dumuid et al. (2017)](https://doi.org/10.1177/0962280217710835) for details.

## Example usage

```R
library(devtools) # see http://cran.r-project.org/web/packages/devtools/README.html
devtools::install_github('tystan/deltacomp')
library(deltacomp)
### see help file to run example
?get_plus_minus_changes

get_plus_minus_changes(
    dataf = fat_data,
    y = "fat",
    comps = c("sl", "sb", "lpa", "mvpa"),
    covars = c("sibs", "parents", "ed"),
    deltas = seq(-60, 60, by = 5) / (24 * 60),
    comparisons = "one-v-one",
    alpha = 0.05,
    verbose = FALSE
)

# OR

get_plus_minus_changes(
    dataf = fat_data,
    y = "fat",
    comps = c("sl", "sb", "lpa", "mvpa"),
    covars = c("sibs", "parents", "ed"),
    deltas = seq(-60, 60, by=5) / (24 * 60),
    comparisons = "one-v-all",
    alpha = 0.05,
    verbose = FALSE
)

```


## Output

Output is a data.frame that can be turned into the plot below:

![](https://github.com/tystan/deltacomp/blob/master/delta_comps.png)
