
<!-- README.md is generated from README.Rmd. Please edit that file -->

# roosdorp

<!-- badges: start -->
<!-- badges: end -->

The purpose of the package roosdorp is to compile a set of useful
functions that do not get everyday use. The main example is to provide a
function to make it easy to create hundreds or thousands of Rmd-reports
together with a progress bar. And which also utilizes parallelization
(if possible, otherwise falls back on serialization) using the parallel
package.

## Installation

You can install the development version of roosdorp from
[GitHub](https://github.com/thoroo/roosdorp) with:

``` r
# install.packages("devtools")
devtools::install_github("thoroo/roosdorp")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(roosdorp)

# generate_report(mtcars, car, template)
# generate_example()
```

## Note

Rendering markdown files on a network drive will cause issues, as per:
<https://github.com/rstudio/rmarkdown/issues/1268> A solution to this is
keeping the project and output folders on a hard drive.
