
# Change notes

The current version (v0.2.0, updated 2020-05-15) is currently in beta testing with the following list of to-dos:

* complete general testing
* unit tests and commenting
* add plotting function
    
Changes since last version (v0.1.0):

* the main function `get_plus_minus_changes()` has been more sensibly renamed to `predict_delta_comps()`
* refactor and modularisation of `predict_delta_comps()`
* the three types of reallocation (`prop-realloc`, `one-v-one` or `one-v-all`) are now working (based on testing so far, unit tests pending)
* includes more checks to throw errors for obvious malfunctions



# deltacomp
Functions to analyse compositional data and produce predictions (with confidence intervals) for relative increases and decreases in the compositional components

## Background

For an outcome variable $Y$, $D$ compositional variables ($x_1, ..., x_D$) and $C$ covariates ($z_1, ..., z_C$); this package fits the compoistional data analysis model

$$
Y_i = \beta_0 + \beta_1 ilr_{1,i} + ... + \beta_{D-1} ilr_{D-1,i}+ \alpha_1 z_{1,i} + ... + \alpha_C z_{C,i} + e_i
$$

where $ilr_{j,i}$ are the isometric log ratio variables derived from the $D$ compositional variables ($x_1, ..., x_D$). The package then makes predictions in alterations of the time-use variables (the linearly dependennt set of compositional components) based on this model. 

Please see [Dumuid et al. (2017a)](https://doi.org/10.1177/0962280217710835) for more details on compositional data analysis.

For information on outcome prediction with time-use exchange between two compositional components (i.e., the `comparisons = "one-vs-one"` option of the `predict_delta_comps()` function), please see
[Dumuid et al. (2017b)](https://doi.org/10.1177%2F0962280217737805).


Information on outcome prediction with time-use exchange between all compositional components (the `comparisons = "prop-realloc"` and `comparisons = "one-vs-all"` options of the `predict_delta_comps()` function), please see ...
gamma


## Example usage

```R
library(devtools) # see https://www.r-project.org/nosvn/pandoc/devtools.html
devtools::install_github('tystan/deltacomp')
library(deltacomp)
### see help file to run example
?predict_delta_comps

predict_delta_comps(
    dataf = fat_data,
    y = "fat",
    comps = c("sl", "sb", "lpa", "mvpa"),
    covars = c("sibs", "parents", "ed"),
    deltas = seq(-60, 60, by = 5) / (24 * 60),
    comparisons = "prop-realloc",
    alpha = 0.05,
    verbose = FALSE
)

# OR

predict_delta_comps(
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

predict_delta_comps(
    dataf = fat_data,
    y = "fat",
    comps = c("sl", "sb", "lpa", "mvpa"),
    covars = c("sibs", "parents", "ed"),
    deltas = seq(-60, 60, by = 5) / (24 * 60),
    comparisons = "one-v-all",
    alpha = 0.05,
    verbose = FALSE
)

```


## Output

Output is a `data.frame` that can be turned into the plot below:

![](https://github.com/tystan/deltacomp/blob/master/delta_comps.png)
