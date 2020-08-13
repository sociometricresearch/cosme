
<!-- README.md is generated from README.Rmd. Please edit that file -->

# measurementfree

<!-- badges: start -->

[![R build
status](https://github.com/sociometricresearch/measurementfree/workflows/R-CMD-check/badge.svg)](https://github.com/sociometricresearch/measurementfree/actions)
[![Codecov test
coverage](https://codecov.io/gh/sociometricresearch/measurementfree/branch/master/graph/badge.svg)](https://codecov.io/gh/sociometricresearch/measurementfree?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/measurementfree)](https://cran.r-project.org/package=measurementfree)
<!-- badges: end -->

The `measurementfree` package allows you to calculate several
estimations of the quality of your survey questions and also adjust your
estimations for measurement error.

Assuming you have a data frame with the `reliability`, `validity` and
`quality` of your question, you can correct for the quality and common
method variance easily.

Load the package as:

``` r
# devtools::install_github("sociometricresearch/measurementfree")
library(measurementfree)
```

## A simple example

`measurementfree` introduces the concept of a measurement error design,
such as the `survey` package has a survey design object. You can define
this measurement error design with three objects: your model design,
your measurement error data and the data of the analysis. For a simple
case, let’s assume that the columns `mpg`, `cyl` and `disp` for `mtcars`
were measured with the same method (e.g. likert scale), then we could
define the measurement error design object as this:

``` r
# This is the model definition
model_definition <- "~ mpg + cyl + disp"

# The measurement error data
me_data <- data.frame(stringsAsFactors = FALSE,
                        question = c("mpg", "cyl", "disp"),
                        reliability = c(0.729, 0.815, 0.68),
                        validity = c(0.951, 0.944, 0.79),
                        quality = c(0.693, 0.77, 0.89)
                      )

# Define your measurement error design
me_obj <- medesign(model_definition, mtcars, me_data)
me_obj
#> <Measurement error design>
#> Parsed model:
#>    ~ mpg + cyl + disp
```

Once you have that object, we simply pass it to `me_cmv_cor` to adjust
the correlation of `mpg`, `cyl`, and `cyl` for common method variance as
well as their quality:

``` r
me_cmv_cor(me_obj)
#> # A tibble: 11 x 12
#>    rowname    mpg    cyl   disp     hp    drat     wt    qsec     vs      am
#>    <chr>    <dbl>  <dbl>  <dbl>  <dbl>   <dbl>  <dbl>   <dbl>  <dbl>   <dbl>
#>  1 mpg      1     -1.04  -1.04  -0.851  0.747  -0.951  0.459   0.728  0.657 
#>  2 cyl     -1.04   1      0.903  0.889 -0.747   0.835 -0.631  -0.866 -0.558 
#>  3 disp    -1.04   0.903  1      0.814 -0.731   0.914 -0.447  -0.731 -0.609 
#>  4 hp      -0.851  0.889  0.814  1     -0.449   0.659 -0.708  -0.723 -0.243 
#>  5 drat     0.747 -0.747 -0.731 -0.449  1      -0.712  0.0912  0.440  0.713 
#>  6 wt      -0.951  0.835  0.914  0.659 -0.712   1     -0.175  -0.555 -0.692 
#>  7 qsec     0.459 -0.631 -0.447 -0.708  0.0912 -0.175  1       0.745 -0.230 
#>  8 vs       0.728 -0.866 -0.731 -0.723  0.440  -0.555  0.745   1      0.168 
#>  9 am       0.657 -0.558 -0.609 -0.243  0.713  -0.692 -0.230   0.168  1     
#> 10 gear     0.526 -0.526 -0.572 -0.126  0.700  -0.583 -0.213   0.206  0.794 
#> 11 carb    -0.604  0.563  0.407  0.750 -0.0908  0.428 -0.656  -0.570  0.0575
#> # … with 2 more variables: gear <dbl>, carb <dbl>
```

## Another simple example

For this example let’s assume that `mpg` and `cyl` were measured with
the same method and `disp` and `drat` were measured with another method,
then we can simply supply a new line to the model above and make sure
that the variable has data on `me_data`:

``` r
# This is the model definition
model_definition <-
  "~ mpg + cyl
   ~ disp + drat"

# The (fake) measurement error data
me_data <- data.frame(stringsAsFactors = FALSE,
                        question = c("mpg", "cyl", "disp", "drat"),
                        reliability = c(0.729, 0.815, 0.68, 0.69),
                        validity = c(0.951, 0.944, 0.79, 0.89),
                        quality = c(0.693, 0.77, 0.89, 0.93)
                      )

# Define your measurement error design
me_obj <- medesign(model_definition, mtcars, me_data)

me_obj
#> <Measurement error design>
#> Parsed model:
#>    ~ mpg + cyl
#>    ~ disp + drat
```

Once you have that object, we simply pass it to `me_cmv_cor` to adjust
the correlation of `mpg`, `cyl`, and `cyl` for common method variance as
well as their quality:

``` r
me_cmv_cor(me_obj)
#> # A tibble: 11 x 12
#>    rowname    mpg    cyl   disp     hp    drat     wt    qsec     vs      am
#>    <chr>    <dbl>  <dbl>  <dbl>  <dbl>   <dbl>  <dbl>   <dbl>  <dbl>   <dbl>
#>  1 mpg      1     -1.04  -0.956 -0.851  0.760  -0.951  0.459   0.728  0.657 
#>  2 cyl     -1.04   1      0.991  0.889 -0.761   0.835 -0.631  -0.866 -0.558 
#>  3 disp    -0.956  0.991  1      0.814 -0.849   0.914 -0.447  -0.731 -0.609 
#>  4 hp      -0.851  0.889  0.814  1     -0.457   0.659 -0.708  -0.723 -0.243 
#>  5 drat     0.760 -0.761 -0.849 -0.457  1      -0.725  0.0929  0.448  0.726 
#>  6 wt      -0.951  0.835  0.914  0.659 -0.725   1     -0.175  -0.555 -0.692 
#>  7 qsec     0.459 -0.631 -0.447 -0.708  0.0929 -0.175  1       0.745 -0.230 
#>  8 vs       0.728 -0.866 -0.731 -0.723  0.448  -0.555  0.745   1      0.168 
#>  9 am       0.657 -0.558 -0.609 -0.243  0.726  -0.692 -0.230   0.168  1     
#> 10 gear     0.526 -0.526 -0.572 -0.126  0.712  -0.583 -0.213   0.206  0.794 
#> 11 carb    -0.604  0.563  0.407  0.750 -0.0925  0.428 -0.656  -0.570  0.0575
#> # … with 2 more variables: gear <dbl>, carb <dbl>
```

Alternatively, you can use `me_cmv_cov` to adjust a covariance matrix
for common method variance as well as quality.
