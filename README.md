
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Normalization of Numeric Data <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/loelschlaeger/normalize/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/loelschlaeger/normalize/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/normalize)](https://CRAN.R-project.org/package=normalize)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/normalize)](https://CRAN.R-project.org/package=normalize)
[![Codecov test
coverage](https://codecov.io/gh/loelschlaeger/normalize/branch/master/graph/badge.svg)](https://app.codecov.io/gh/loelschlaeger/normalize?branch=master)
<!-- badges: end -->

The `{normalize}` `R` package offers convenient tools to normalize
(centering to zero mean and scaling to unit variance) numeric data:

1.  works for `vector`, `matrix`, `data.frame`, and `list` objects

2.  can normalize by row or by column

3.  can ignore rows or columns when normalizing

4.  allows for joint normalizing of rows or columns

5.  provides the normalizing constants as attributes

6.  preserves object attributes

## Installation

You can install the released version from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("normalize")
```

## Example

Can normalize a `vector`:

``` r
x <- 1:10
normalize(x)
#>  [1] -1.4863011 -1.1560120 -0.8257228 -0.4954337 -0.1651446  0.1651446
#>  [7]  0.4954337  0.8257228  1.1560120  1.4863011
#> attr(,"center")
#> [1] 5.5
#> attr(,"scale")
#> [1] 3.02765
normalize(x, center = FALSE)
#>  [1] 0.3302891 0.6605783 0.9908674 1.3211565 1.6514456 1.9817348 2.3120239
#>  [8] 2.6423130 2.9726022 3.3028913
#> attr(,"scale")
#> [1] 3.02765
normalize(x, scale = FALSE)
#>  [1] -4.5 -3.5 -2.5 -1.5 -0.5  0.5  1.5  2.5  3.5  4.5
#> attr(,"center")
#> [1] 5.5
```

Can normalize a `matrix`:

``` r
normalize(
  matrix(1:12, nrow = 3, ncol = 4),
  jointly = list(1:2, 3:4) # joint normalization of columns 1, 2 and 3, 4
)
#>      [,1] [,2] [,3] [,4]
#> [1,] -2.5  0.5 -2.5  0.5
#> [2,] -1.5  1.5 -1.5  1.5
#> [3,] -0.5  2.5 -0.5  2.5
#> attr(,"center")
#> [1] 3.5 3.5 9.5 9.5
#> attr(,"scale")
#> [1] 1 1 1 1
```

Can normalize a `data.frame`:

``` r
normalize(
  data.frame(a = 1:3, b = c("A", "B", "C"), c = 7:9, d = 10:12),
  ignore = 2 # ignore character column 2 for normalization
)
#>    a b  c  d
#> 1 -1 A -1 -1
#> 2  0 B  0  0
#> 3  1 C  1  1
```

Can work on a `list`:

``` r
normalize(list(1:5, diag(3), data.frame(1:3, 2:4)))
#> [[1]]
#> [1] -1.2649111 -0.6324555  0.0000000  0.6324555  1.2649111
#> attr(,"center")
#> [1] 3
#> attr(,"scale")
#> [1] 1.581139
#> 
#> [[2]]
#>            [,1]       [,2]       [,3]
#> [1,]  1.1547005 -0.5773503 -0.5773503
#> [2,] -0.5773503  1.1547005 -0.5773503
#> [3,] -0.5773503 -0.5773503  1.1547005
#> attr(,"center")
#> [1] 0.3333333 0.3333333 0.3333333
#> attr(,"scale")
#> [1] 0.5773503 0.5773503 0.5773503
#> 
#> [[3]]
#>   X1.3 X2.4
#> 1   -1   -1
#> 2    0    0
#> 3    1    1
```
