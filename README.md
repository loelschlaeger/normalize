
<!-- README.md is generated from README.Rmd. Please edit that file -->

# normalize

<!-- badges: start -->
<!-- badges: end -->

`{normalize}` is a small R package that allows for centering and scaling
of numeric data. The goal is to extend the base R `scale()` function
with some additional features:

- normalizing by row or by column
- ignoring rows or columns when normalizing
- normalizing certain rows or columns jointly
- normalizing over list elements

## Installation

You can install the development version of normalize like so:

``` r
# install.packages("devtools")
devtools::install.github("loelschlaeger/normalize")
```

## Example

We can work on a vector:

``` r
normalize(1:5)
#> [1] -1.2649111 -0.6324555  0.0000000  0.6324555  1.2649111
#> attr(,"center")
#> [1] 3
#> attr(,"scale")
#> [1] 1.581139
```

We can work on a matrix:

``` r
normalize(diag(3))
#>            [,1]       [,2]       [,3]
#> [1,]  1.1547005 -0.5773503 -0.5773503
#> [2,] -0.5773503  1.1547005 -0.5773503
#> [3,] -0.5773503 -0.5773503  1.1547005
```

We can work on a data frame:

``` r
normalize(data.frame(1:3, 2:4))
#>      X1.3 X2.4
#> [1,]   -1   -1
#> [2,]    0    0
#> [3,]    1    1
```

We can work on a list:

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
#> 
#> [[3]]
#>      X1.3 X2.4
#> [1,]   -1   -1
#> [2,]    0    0
#> [3,]    1    1
```
