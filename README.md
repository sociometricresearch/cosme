
<!-- README.md is generated from README.Rmd. Please edit that file -->

# measurementfree

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/measurementfree)](https://cran.r-project.org/package=measurementfree)
[![Travis build
status](https://travis-ci.org/sociometricresearch/measurementfree.svg?branch=master)](https://travis-ci.org/sociometricresearch/measurementfree)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/sociometricresearch/measurementfree?branch=master&svg=true)](https://ci.appveyor.com/project/sociometricresearch/measurementfree)
<!-- [![Codecov test coverage](https://codecov.io/gh/sociometricresearch/measurementfree/branch/master/graph/badge.svg)](https://codecov.io/gh/sociometricresearch/measurementfree?branch=master) -->
<!-- badges: end -->

The `measurementfree` package allows you to calculate several
estimations of the quality of your survey questions and also adjust your
estimations for measurement error.

Assuming you have a data frame with the `reliability`, `validity` and
`quality` of your question, you can correct for the quality and common
method variance easily.

# A simple example

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
#>    rowname    mpg    cyl   disp     hp    drat     wt    qsec     vs
#>    <chr>    <dbl>  <dbl>  <dbl>  <dbl>   <dbl>  <dbl>   <dbl>  <dbl>
#>  1 mpg      1     -1.19  -1.10  -0.932  0.818  -1.04   0.503   0.798
#>  2 cyl     -1.19   1      1.07   0.949 -0.798   0.892 -0.674  -0.924
#>  3 disp    -1.10   1.07   1      0.838 -0.753   0.941 -0.460  -0.753
#>  4 hp      -0.932  0.949  0.838  1     -0.449   0.659 -0.708  -0.723
#>  5 drat     0.818 -0.798 -0.753 -0.449  1      -0.712  0.0912  0.440
#>  6 wt      -1.04   0.892  0.941  0.659 -0.712   1     -0.175  -0.555
#>  7 qsec     0.503 -0.674 -0.460 -0.708  0.0912 -0.175  1       0.745
#>  8 vs       0.798 -0.924 -0.753 -0.723  0.440  -0.555  0.745   1    
#>  9 am       0.721 -0.596 -0.627 -0.243  0.713  -0.692 -0.230   0.168
#> 10 gear     0.577 -0.561 -0.589 -0.126  0.700  -0.583 -0.213   0.206
#> 11 carb    -0.662  0.601  0.419  0.750 -0.0908  0.428 -0.656  -0.570
#> # … with 3 more variables: am <dbl>, gear <dbl>, carb <dbl>
```

# Another simple example

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
#>    rowname    mpg    cyl   disp     hp    drat     wt    qsec     vs
#>    <chr>    <dbl>  <dbl>  <dbl>  <dbl>   <dbl>  <dbl>   <dbl>  <dbl>
#>  1 mpg      1     -1.22  -1.08  -0.932  0.848  -1.04   0.503   0.798
#>  2 cyl     -1.22   1      1.09   0.949 -0.827   0.892 -0.674  -0.924
#>  3 disp    -1.08   1.09   1      0.838 -0.885   0.941 -0.460  -0.753
#>  4 hp      -0.932  0.949  0.838  1     -0.465   0.659 -0.708  -0.723
#>  5 drat     0.848 -0.827 -0.885 -0.465  1      -0.739  0.0946  0.457
#>  6 wt      -1.04   0.892  0.941  0.659 -0.739   1     -0.175  -0.555
#>  7 qsec     0.503 -0.674 -0.460 -0.708  0.0946 -0.175  1       0.745
#>  8 vs       0.798 -0.924 -0.753 -0.723  0.457  -0.555  0.745   1    
#>  9 am       0.721 -0.596 -0.627 -0.243  0.739  -0.692 -0.230   0.168
#> 10 gear     0.577 -0.561 -0.589 -0.126  0.725  -0.583 -0.213   0.206
#> 11 carb    -0.662  0.601  0.419  0.750 -0.0941  0.428 -0.656  -0.570
#> # … with 3 more variables: am <dbl>, gear <dbl>, carb <dbl>
```

Alternatively, you can use `me_cmv_cov` to adjust a covariance matrix
for common method variance as well as quality.
