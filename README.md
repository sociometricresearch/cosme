
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cosme

<!-- badges: start -->

[![R build
status](https://github.com/sociometricresearch/cosme/workflows/R-CMD-check/badge.svg)](https://github.com/sociometricresearch/cosme/actions)
[![Codecov test
coverage](https://codecov.io/gh/sociometricresearch/cosme/branch/master/graph/badge.svg)](https://codecov.io/gh/sociometricresearch/cosme?branch=master)
[![DOI](https://zenodo.org/badge/198222115.svg)](https://zenodo.org/badge/latestdoi/198222115)
[![CRAN
status](https://www.r-pkg.org/badges/version/cosme)](https://cran.r-project.org/package=cosme)
<!-- badges: end -->

The `cosme` package allows you to calculate several estimations of the
quality of your survey questions and also adjust your estimations for
measurement error.

Assuming you have a data frame with the `reliability`, `validity` and
`quality` of your question, you can correct for the quality and common
method variance easily.

Load the package as:

``` r
# devtools::install_github("sociometricresearch/cosme")
library(cosme)
```

## A simple example

`cosme` introduces the concept of a measurement error design, such as
the `survey` package has a survey design object. You can define this
measurement error design with three objects: your model design, your
measurement error data and the data of the analysis. For a simple case,
let’s use the data from the European Social Survey already loaded with
the package. The variables `trstplt`, `trstprl` and `trstprt` were asked
with the same type of the question. In other words, they share a common
method (for example, a likert type scale question). Sharing a common
method allows us to correct for their common method variance. We could
define the measurement error design object as this:

``` r
# Data
data(ess7es)
ess_subset <- ess7es[1:3]

# This is the model definition
model_definition <- "
  # Correct for measurement error
  ~~ trstplt + trstprl + trstprt

  # Correct for common method variance
  ~ trstplt + trstprl + trstprt
"

# The measurement error data
me_data <-
  data.frame(
    question = c("trstprl", "trstplt", "trstprt"),
    reliability = c(0.812, 0.852, 0.858),
    validity = c(0.959, 0.965, 0.956),
    quality = c(0.779, 0.822, 0.821)
  )

# Define your measurement error design
me_obj <- medesign(model_definition, ess_subset, me_data)
#> Correcting for measurement error in trstplt, trstprl, trstprt. If you want to correct other variables, make sure they are both in `me_data` and `.data` and you specify their names in the model syntax (`~~`).
me_obj
#> <Measurement error design>
#> Parsed model:
#>    ~~ trstprl + trstplt + trstprt
#>    ~ trstplt + trstprl + trstprt
```

This object describes your measurement error design. With this, we
simply pass it to `me_cmv_cor` to adjust the correlation of `trstplt`,
`trstprl` and `trstprt` for common method variance as well as their
measurement error:

``` r
me_cmv_cor(me_obj)
#> # A tibble: 3 x 4
#>   rowname trstprl trstplt trstprt
#>   <chr>     <dbl>   <dbl>   <dbl>
#> 1 trstprl   1       0.771   0.704
#> 2 trstplt   0.771   1       0.984
#> 3 trstprt   0.704   0.984   1
```

## Another simple example

The previous example corrected only variables `trstplt`, `trstprl` and
`trstprt`. What if we have more groups of variables that share a common
method? Just keep adding them to the model syntax\! For example,
`stfedu` and `stfhlth` also share a common method, let’s add them to the
model syntax and make sure we have data on `me_data` for them:

``` r
# Data
data(ess7es)
ess_subset <- ess7es[1:5]

# This is the model definition
model_definition <- "
   # Correct for measurement error on all variables (.)
   ~~ .;

   # Correct for common method variance on these group of variables
   ~ trstplt + trstprl + trstprt;
   ~ stfedu + stfhlth
"

# The measurement error data
me_data <-
  data.frame(
    question = c("trstprl", "trstplt", "trstprt", "stfedu", "stfhlth"),
    reliability = c(0.812, 0.852, 0.858, 0.870, 0.871),
    validity = c(0.959, 0.965, 0.956, 0.915, 0.893),
    quality = c(0.779, 0.822, 0.821, 0.796, 0.779)
  )

# Define your measurement error design
me_obj <- medesign(model_definition, ess_subset, me_data)
#> Correcting for measurement error in trstprl, trstplt, trstprt, stfedu, stfhlth. If you want to correct other variables, make sure they are both in `me_data` and `.data` and you specify their names in the model syntax (`~~`).
me_obj
#> <Measurement error design>
#> Parsed model:
#>    ~~ trstprl + trstplt + trstprt + stfedu + stfhlth
#>    ~ trstplt + trstprl + trstprt
#>    ~ stfedu + stfhlth
```

Once you have your measurement error design, we simply pass it to
`me_cmv_cor` to adjust the correlation for common method variance as
well as their quality:

``` r
me_cmv_cor(me_obj)
#> # A tibble: 5 x 6
#>   rowname trstprl trstplt trstprt stfedu stfhlth
#>   <chr>     <dbl>   <dbl>   <dbl>  <dbl>   <dbl>
#> 1 trstprl   1       0.774   0.704  0.442   0.430
#> 2 trstplt   0.774   1       0.981  0.436   0.462
#> 3 trstprt   0.704   0.981   1      0.390   0.371
#> 4 stfedu    0.442   0.436   0.390  1       0.704
#> 5 stfhlth   0.430   0.462   0.371  0.704   1
```

Alternatively, you can use `me_cmv_cov` to adjust a covariance matrix
for common method variance as well as quality.
