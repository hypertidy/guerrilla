# Turn a grid into a matrix

The values as a matrix, one row per grid row, with the top row of the
grid first. This is the arrangement
[`image()`](https://rdrr.io/r/graphics/image.html) and `raster` both
think in, and it is where most orientation bugs come from, so it gets
its own function.

## Usage

``` r
# S3 method for class 'guerrilla_grid'
as.matrix(x, ...)
```

## Arguments

- x:

  a `guerrilla_grid`

- ...:

  ignored

## Value

A matrix with `nrow` rows and `ncol` columns.

## Examples

``` r
g <- grid_spec(dimension = c(3, 2), extent = c(0, 3, 0, 2))
g$values <- 1:6
as.matrix(g)
#>      [,1] [,2] [,3]
#> [1,]    1    2    3
#> [2,]    4    5    6
```
