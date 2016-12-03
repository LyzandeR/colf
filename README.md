# colf

#### Released Version

[![CRAN version](http://www.r-pkg.org/badges/version/colf)](https://cran.r-project.org/package=colf)

#### Build Status

[![Travis-CI Build Status](https://travis-ci.org/LyzandeR/colf.svg?branch=master)](https://travis-ci.org/LyzandeR/colf)

#### Description

A package to perform a least squares constrained optimization on a linear objective function. It provides a very easy way to run a constrained linear regression using  the `lm` formula syntax that 
most R users are familiar with.

## Installation

To install the latest released version from CRAN you just need to run on your console:

```r
install.packages('colf')
```

To install the development version you need to have the `devtools` package installed. To install devtools type in your console: `install.packages('devtools')`.

Then to install colf run the following on your console:

```R
devtools::install_github('lyzander/colf')
```

## Usage

By typing on your console:

```R
library(colf)
colf_nls(mpg ~ ., mtcars, upper = rep(2, 11), lower = rep(-0.5, 11))
```

you can see a first example of how to run a constrained optimization on a linear objective function!

## Links - Cran / Tutorial / Examples

To read the tutorial and documentation for colf please see the [vignette](https://cran.r-project.org/web/packages/colf/vignettes/colf.html).

To see the released version you can visit [CRAN](https://cran.r-project.org/package=colf).