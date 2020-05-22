
# Change notes

The current version (v0.2.0, updated 2020-05-15) is currently in beta testing with the following list of to-dos:

* complete general testing
* add testing for `NA` values in input datasets
* unit tests and commenting
* add plotting function
    
Changes since last version (v0.1.0):

* the main function `get_plus_minus_changes()` has been more sensibly renamed to `predict_delta_comps()`
* refactor and modularisation of `predict_delta_comps()`
* the three types of reallocation (`prop-realloc`, `one-v-one` or `one-v-all`) are now working (based on testing so far, unit tests pending)
* includes more checks to throw errors for obvious malfunctions
* the mean composition now correctly uses the geometric mean (on the simplex) and not the naive arithmetic mean of the compositions



# The `deltacomp` package

Functions to analyse compositional data and produce predictions (with confidence intervals) for relative increases and decreases in the compositional components

## Background

For an outcome variable `Y`, *D* compositional variables (`x_1, ..., x_D`) and *C* covariates (`z_1, ..., z_C`); this package fits the compoistional data analysis model (notation inexact):


`Y = b_0 + b_1 ilr_1 + ... + b_{D-1} ilr_{D-1} + a_1 z_1 + ... + a_C z_C + e`

where `ilr_i` are the isometric log ratio variables derived from the *D* compositional variables (`x_1, ..., x_D`), `b_0, ...,  b_{D-1}, a_1, ..., a_C` are parameters to be estimated and `e` is the error. The package then makes predictions in alterations of the time-use variables (the linearly dependennt set of compositional components) based on this model. 


Information on outcome prediction with time-use exchange between one component and the remaining compositional components proportionally (`comparisons = "prop-realloc"` option of the `predict_delta_comps()` function), please see [Dumuid et al. (2017a)](https://doi.org/10.1177/0962280217710835).

For information on outcome prediction with time-use exchange between two compositional components (i.e., the `comparisons = "one-vs-one"` option of the `predict_delta_comps()` function), please see
[Dumuid et al. (2017b)](https://doi.org/10.1177%2F0962280217737805).

Information on outcome prediction with time-use exchange between all compositional components (the `comparisons = "prop-realloc"` and `comparisons = "one-vs-all"` options of the `predict_delta_comps()` function), please see ...

For a starting point to learn about compositional data analysis please see [Aitchison (1982)](https://doi.org/10.1111/j.2517-6161.1982.tb01195.x). However the aforementioned [Dumuid et al. (2017a)](https://doi.org/10.1177/0962280217710835) and [Dumuid et al. (2017b)](https://doi.org/10.1177%2F0962280217737805) may be more approachable introductions.


## Datasets in package

Two datasets are supplied with the package:

* `fat_data` and 
* `fairclough`. 

The `fairclough` dataset was kindly provided by the authors of [Fairclough et al. (2017)](https://doi.org/10.1186/s12966-017-0521-z). `fat_data` is a randomly generated test dataset that might roughly mimic a real dataset.

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

![](https://github.com/tystan/deltacomp/blob/master/inst/img/delta_comps.png)
