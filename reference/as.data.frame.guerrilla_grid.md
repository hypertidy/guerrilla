# Turn a grid into a data frame

One row per cell, with the cell centre coordinates and the value.

## Usage

``` r
# S3 method for class 'guerrilla_grid'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  a `guerrilla_grid`

- row.names, optional:

  ignored, for consistency with the generic

- ...:

  ignored

## Value

A data frame of `x`, `y` and `value`.

## Examples

``` r
g <- grid_spec(dimension = c(3, 2), extent = c(0, 3, 0, 2))
g$values <- 1:6
head(as.data.frame(g))
#>     x   y value
#> 1 0.5 1.5     1
#> 2 1.5 1.5     2
#> 3 2.5 1.5     3
#> 4 0.5 0.5     4
#> 5 1.5 0.5     5
#> 6 2.5 0.5     6
```
