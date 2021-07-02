
# The `deltacomp` package

The functions in the `deltacomp` package produce predictions (with confidence intervals) for relative increases and decreases in the compositional parts. 

The development of the package was initiated by Ty Stanford and Dorothea Dumuid in 2018 and is still under development. Changes and corrections are expected to be made during 2021.


## Installing `deltacomp`

Run the following code to install and load the `deltacomp` package

```R
library(devtools) # see https://www.r-project.org/nosvn/pandoc/devtools.html
devtools::install_github('tystan/deltacomp')
library(deltacomp)

```

The following code are run to see help files:

```R
?predict_delta_comps
```

## How to use `deltacomp`?

Please see the package vignette for examples of what the `deltacomp` package can do. To view, run the following:

```R
vignette("deltacomp vignette")
```

## Release notes

See [/change-notes.md](https://github.com/tystan/deltacomp/blob/master/change-notes.md).

