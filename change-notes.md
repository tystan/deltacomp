
# Change notes

## To-dos

* function (that calls `predict_delta_comps()`) that does the minutes in the day specifically by default (also deltas in minutes rather than proportions in output)
* additional commenting
* additional unit tests


## Version 0.2.2 (2022-03-09)

Improved output: 

* The output to console is now better spaced with dividers (`---`) for easier reading
* The ilrs (`ilr1`, `ilr2`, ...) are defined in terms of the input compositional variables in `predict_delta_comps()` output (printed to console)
* `predict_delta_comps()` now prints to the console a statistical test for the ilrs being collectively significant in the model (i.e., do they improve the model statistically?)

## Version 0.2.1 (2020-09-04)

* fixed error in `append_ilr_coords()` that would create errors when a single row data.frame was passed (failing unit tests and examples). Seemed to be a new way the `compositions` package handled data potentially, or the move to R 4.0


## Version 0.2.0 (2020-06-20)

* the main function `get_plus_minus_changes()` has been more sensibly renamed to `predict_delta_comps()`
* refactor and modularisation of `predict_delta_comps()`
* the two types of reallocation (`prop-realloc` and `one-v-one`) are working correctly, unit tested
* includes more checks to throw errors for obvious malfunctions
* the mean composition now correctly uses the geometric mean (on the simplex) and not the naive arithmetic mean of the compositions
* added plotting function `plot_delta_comp()` (see '5. Output and plotting results' below)
* `predict_delta_comps()` now removes rows with `NA` values in input datasets (and warns the user)
* the mean composition and the resulting predicted outcome with confidence interval is now produced by `predict_delta_comps()` (see '5.1 Prediction for the mean composition' below)
* `predict_delta_comps()` now checks for compositional components of 0 [or non-sensical negative values] (geometric mean incompatable)
* completed general testing and unit tests