
<!-- README.md is generated from README.Rmd. Please edit that file -->

# measurementfree

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/measurementfree)](https://cran.r-project.org/package=measurementfree)
[![Travis build
status](https://travis-ci.org/sociometricresearch/measurementfree.svg?branch=master)](https://travis-ci.org/sociometricresearch/measurementfree)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/sociometricresearch/measurementfree?branch=master&svg=true)](https://ci.appveyor.com/project/sociometricresearch/measurementfree)
[![Codecov test
coverage](https://codecov.io/gh/sociometricresearch/measurementfree/branch/master/graph/badge.svg)](https://codecov.io/gh/sociometricresearch/measurementfree?branch=master)
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
#> $parsed_model
#>   lhs op  rhs   std mod.idx     type
#> 1  X1  ~  mpg FALSE       0 observed
#> 2  X1  ~  cyl FALSE       0 observed
#> 3  X1  ~ disp FALSE       0 observed
#> 
#> $.data
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
#> 
#> $me_data
#>   question reliability validity quality
#> 1      mpg       0.729    0.951   0.693
#> 2      cyl       0.815    0.944   0.770
#> 3     disp       0.680    0.790   0.890
#> 
#> $corr
#> # A tibble: 11 x 12
#>    rowname    mpg    cyl   disp     hp    drat     wt    qsec     vs
#>    <chr>    <dbl>  <dbl>  <dbl>  <dbl>   <dbl>  <dbl>   <dbl>  <dbl>
#>  1 mpg      0.693 -0.852 -0.848 -0.776  0.681  -0.868  0.419   0.664
#>  2 cyl     -0.852  0.77   0.902  0.832 -0.700   0.782 -0.591  -0.811
#>  3 disp    -0.848  0.902  0.89   0.791 -0.710   0.888 -0.434  -0.710
#>  4 hp      -0.776  0.832  0.791  1     -0.449   0.659 -0.708  -0.723
#>  5 drat     0.681 -0.700 -0.710 -0.449  1      -0.712  0.0912  0.440
#>  6 wt      -0.868  0.782  0.888  0.659 -0.712   1     -0.175  -0.555
#>  7 qsec     0.419 -0.591 -0.434 -0.708  0.0912 -0.175  1       0.745
#>  8 vs       0.664 -0.811 -0.710 -0.723  0.440  -0.555  0.745   1    
#>  9 am       0.600 -0.523 -0.591 -0.243  0.713  -0.692 -0.230   0.168
#> 10 gear     0.480 -0.493 -0.556 -0.126  0.700  -0.583 -0.213   0.206
#> 11 carb    -0.551  0.527  0.395  0.750 -0.0908  0.428 -0.656  -0.570
#> # … with 3 more variables: am <dbl>, gear <dbl>, carb <dbl>
#> 
#> $covv
#> # A tibble: 11 x 12
#>    rowname      mpg     cyl    disp      hp     drat      wt     qsec
#>    <chr>      <dbl>   <dbl>   <dbl>   <dbl>    <dbl>   <dbl>    <dbl>
#>  1 mpg        0.693  -9.17  -633.   -321.     2.20    -5.12    4.51  
#>  2 cyl       -9.17    0.77   200.    102.    -0.668    1.37   -1.89  
#>  3 disp    -633.    200.       0.89 6721.   -47.1    108.    -96.1   
#>  4 hp      -321.    102.    6721.   4701.   -16.5     44.2   -86.8   
#>  5 drat       2.20   -0.668  -47.1   -16.5    0.286   -0.373   0.0871
#>  6 wt        -5.12    1.37   108.     44.2   -0.373    0.957  -0.305 
#>  7 qsec       4.51   -1.89   -96.1   -86.8    0.0871  -0.305   3.19  
#>  8 vs         2.02   -0.730  -44.4   -25.0    0.119   -0.274   0.671 
#>  9 am         1.80   -0.466  -36.6    -8.32   0.190   -0.338  -0.205 
#> 10 gear       2.14   -0.649  -50.8    -6.36   0.276   -0.421  -0.280 
#> 11 carb      -5.36    1.52    79.1    83.0   -0.0784   0.676  -1.89  
#> # … with 4 more variables: vs <dbl>, am <dbl>, gear <dbl>, carb <dbl>
#> 
#> attr(,"class")
#> [1] "medesign"
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
#> $parsed_model
#>   lhs op  rhs   std mod.idx     type
#> 1  X1  ~  mpg FALSE       0 observed
#> 2  X1  ~  cyl FALSE       0 observed
#> 3  X2  ~ disp FALSE       0 observed
#> 4  X2  ~ drat FALSE       0 observed
#> 
#> $.data
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
#> 
#> $me_data
#>   question reliability validity quality
#> 1      mpg       0.729    0.951   0.693
#> 2      cyl       0.815    0.944   0.770
#> 3     disp       0.680    0.790   0.890
#> 4     drat       0.690    0.890   0.930
#> 
#> $corr
#> # A tibble: 11 x 12
#>    rowname    mpg    cyl   disp     hp    drat     wt    qsec     vs
#>    <chr>    <dbl>  <dbl>  <dbl>  <dbl>   <dbl>  <dbl>   <dbl>  <dbl>
#>  1 mpg      0.693 -0.852 -0.848 -0.776  0.681  -0.868  0.419   0.664
#>  2 cyl     -0.852  0.77   0.902  0.832 -0.700   0.782 -0.591  -0.811
#>  3 disp    -0.848  0.902  0.89   0.791 -0.710   0.888 -0.434  -0.710
#>  4 hp      -0.776  0.832  0.791  1     -0.449   0.659 -0.708  -0.723
#>  5 drat     0.681 -0.700 -0.710 -0.449  0.93   -0.712  0.0912  0.440
#>  6 wt      -0.868  0.782  0.888  0.659 -0.712   1     -0.175  -0.555
#>  7 qsec     0.419 -0.591 -0.434 -0.708  0.0912 -0.175  1       0.745
#>  8 vs       0.664 -0.811 -0.710 -0.723  0.440  -0.555  0.745   1    
#>  9 am       0.600 -0.523 -0.591 -0.243  0.713  -0.692 -0.230   0.168
#> 10 gear     0.480 -0.493 -0.556 -0.126  0.700  -0.583 -0.213   0.206
#> 11 carb    -0.551  0.527  0.395  0.750 -0.0908  0.428 -0.656  -0.570
#> # … with 3 more variables: am <dbl>, gear <dbl>, carb <dbl>
#> 
#> $covv
#> # A tibble: 11 x 12
#>    rowname      mpg     cyl    disp      hp     drat      wt     qsec
#>    <chr>      <dbl>   <dbl>   <dbl>   <dbl>    <dbl>   <dbl>    <dbl>
#>  1 mpg        0.693  -9.17  -633.   -321.     2.20    -5.12    4.51  
#>  2 cyl       -9.17    0.77   200.    102.    -0.668    1.37   -1.89  
#>  3 disp    -633.    200.       0.89 6721.   -47.1    108.    -96.1   
#>  4 hp      -321.    102.    6721.   4701.   -16.5     44.2   -86.8   
#>  5 drat       2.20   -0.668  -47.1   -16.5    0.93    -0.373   0.0871
#>  6 wt        -5.12    1.37   108.     44.2   -0.373    0.957  -0.305 
#>  7 qsec       4.51   -1.89   -96.1   -86.8    0.0871  -0.305   3.19  
#>  8 vs         2.02   -0.730  -44.4   -25.0    0.119   -0.274   0.671 
#>  9 am         1.80   -0.466  -36.6    -8.32   0.190   -0.338  -0.205 
#> 10 gear       2.14   -0.649  -50.8    -6.36   0.276   -0.421  -0.280 
#> 11 carb      -5.36    1.52    79.1    83.0   -0.0784   0.676  -1.89  
#> # … with 4 more variables: vs <dbl>, am <dbl>, gear <dbl>, carb <dbl>
#> 
#> attr(,"class")
#> [1] "medesign"
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
